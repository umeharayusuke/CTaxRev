# Fig. 5 a-c energy mix with panel d replaced by the Fig. 6 CDR mix.
# 2050 Def/Aid comparison for the current 15th provider/recipient regions.
# Run from the project root or from Rprog/Ctax_paper_260811.

library_paths <- c(Sys.getenv("R_LIBS_USER"), "C:/ENVI5809/R_library")
library_paths <- library_paths[nzchar(library_paths) & dir.exists(library_paths)]
.libPaths(unique(c(library_paths, .libPaths())))
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  library(gdxrrw)
})

root <- if (file.exists("data/AR6database/global_17_IAMC.gdx")) "." else "../.."
gdx_file <- file.path(root, "data/AR6database/global_17_IAMC.gdx")
if (!file.exists(gdx_file)) stop("Missing GDX: ", gdx_file)
output_dir <- file.path(root, "output/Figure")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
gams_dirs <- c(Sys.getenv("GAMS_SYSDIR"), "C:/GAMS/34", "C:/GAMS/win64/26.1")
gams_dirs <- gams_dirs[nzchar(gams_dirs) & dir.exists(gams_dirs)]
if (length(gams_dirs)) igdx(gams_dirs[1])

scenarios <- c(Def = "SSP2_400C_2030CP_NoCC_No",
               Aid = "SSP2_400C_2030CP_15th_NoCC_No")
regions <- c(Rprovider15th = "Provider", Rrecipient15th = "Recipient")
year <- 2050L

# Use mutually exclusive top-level energy categories, not a mix of totals and
# subcategories. The current GDX calls hydro "Hyp" (not "Hyd" in the old code).
spec <- tibble::tribble(
  ~panel, ~variable,                         ~component,
  "a",    "Prm_Ene_Fos",                     "Fossil fuels",
  "a",    "Prm_Ene_Bio",                     "Biomass",
  "a",    "Prm_Ene_Hyp",                     "Hydro",
  "a",    "Prm_Ene_Nuc",                     "Nuclear",
  "a",    "Prm_Ene_Solar",                   "Solar",
  "a",    "Prm_Ene_Win",                     "Wind",
  "b",    "Sec_Ene_Ele_Fos",                 "Fossil fuels",
  "b",    "Sec_Ene_Ele_Bio",                 "Biomass",
  "b",    "Sec_Ene_Ele_Hyp",                 "Hydro",
  "b",    "Sec_Ene_Ele_Nuc",                 "Nuclear",
  "b",    "Sec_Ene_Ele_Solar",               "Solar",
  "b",    "Sec_Ene_Ele_Win",                 "Wind",
  "c",    "Fin_Ene_SolidsCoa",               "Coal",
  "c",    "Fin_Ene_Liq_Oil",                 "Oil",
  "c",    "Fin_Ene_Gas",                     "Gas",
  "c",    "Fin_Ene_SolidsBio",               "Solid biomass",
  "c",    "Fin_Ene_Liq_Bio",                 "Biofuel",
  "c",    "Fin_Ene_Ele",                     "Electricity",
  "c",    "Fin_Ene_Heat",                    "Heat",
  "c",    "Fin_Ene_Hyd",                     "Hydrogen",
  "d",    "Car_Rem_Bio_wit_CCS",             "BECCS",
  "d",    "Car_Rem_Bio",                     "Biochar",
  "d",    "Car_Rem_Dir_Air_Cap_wit_CCS",     "DACCS",
  "d",    "Car_Rem_Enh_Wea",                 "Enhanced weathering",
  "d",    "Car_Rem_Frs",                     "Afforestation",
  "d",    "Car_Rem_Soi_Car_Seq",             "Soil carbon"
)

raw <- rgdx.param(gdx_file, "IAMC_template")
required <- c("SCENARIO", "REMF", "VEMF", "YEMF", "IAMC_Template")
if (!all(required %in% names(raw))) stop("Unexpected IAMC_template columns.")
selected <- raw %>%
  filter(as.character(SCENARIO) %in% unname(scenarios),
         as.character(REMF) %in% names(regions),
         as.character(VEMF) %in% spec$variable,
         as.character(YEMF) == as.character(year)) %>%
  transmute(scenario = names(scenarios)[match(as.character(SCENARIO), scenarios)],
            region = as.character(REMF), variable = as.character(VEMF),
            value = as.numeric(IAMC_Template))
rm(raw)
if (anyDuplicated(selected[c("scenario", "region", "variable")])) {
  stop("Duplicate scenario-region-variable records.")
}
if (any(!is.finite(selected$value))) stop("Non-finite GDX values.")
expected <- expand_grid(scenario = names(scenarios), region = names(regions),
                        variable = spec$variable)
missing <- anti_join(expected, selected,
                     by = c("scenario", "region", "variable"))
if (nrow(missing)) {
  print(missing)
  stop("Missing records; check the current GDX before plotting.")
}

units <- rgdx.set(gdx_file, "VUMAP") %>%
  transmute(variable = as.character(VEMF), unit = as.character(UEMF))
unit_check <- spec %>% left_join(units, by = "variable")
expected_units <- if_else(unit_check$panel == "d", "Mt CO2/yr", "EJ/yr")
if (any(is.na(unit_check$unit) | unit_check$unit != expected_units)) {
  print(unit_check)
  stop("Unexpected or missing GDX units.")
}

plot_data <- selected %>%
  inner_join(spec, by = "variable", relationship = "many-to-one") %>%
  mutate(region_name = factor(unname(regions[region]), levels = unname(regions)),
         scenario = factor(scenario, levels = names(scenarios)),
         value = if_else(panel == "d", value / 1000, value)) %>%
  group_by(panel, scenario, region, region_name, component) %>%
  summarise(value = sum(value), .groups = "drop")
if (any(plot_data$value < -1e-8)) stop("Negative category values cannot be stacked as planned.")

# The first factor level stacks on top in ggplot2. List lower-carbon sources
# first, while retaining the source figure's original category colours.
primary_colors <- c("Solar" = "lightsalmon", "Wind" = "lightskyblue3",
                    "Hydro" = "lightsteelblue", "Nuclear" = "moccasin",
                    "Biomass" = "darkolivegreen2", "Fossil fuels" = "gray60")
power_colors <- primary_colors
power_colors["Fossil fuels"] <- "grey50"
final_colors <- c("Hydrogen" = "thistle2", "Electricity" = "lightsteelblue",
                  "Biofuel" = "#DBFF70", "Solid biomass" = "#A9D65D",
                  "Heat" = "salmon", "Gas" = "moccasin",
                  "Oil" = "sandybrown", "Coal" = "grey70")
cdr_colors <- c("BECCS" = "#4DAF4A", "Biochar" = "#E69F00",
                "DACCS" = "#984EA3", "Enhanced weathering" = "#377EB8",
                "Afforestation" = "#1B7837", "Soil carbon" = "#A65628")

make_panel <- function(panel_id, title, colors, y_label) {
  dat <- plot_data %>% filter(panel == panel_id) %>%
    mutate(component = factor(component, levels = names(colors)))
  totals <- dat %>% group_by(region_name, scenario) %>%
    summarise(total = sum(value), .groups = "drop")
  ggplot(dat, aes(scenario, value, fill = component)) +
    geom_col(width = 0.62) +
    geom_text(data = totals, aes(x = scenario, y = total,
                                 label = sprintf("%.1f", total)),
              inherit.aes = FALSE, vjust = -0.45,
              size = 3.1, colour = "#263B4B") +
    facet_grid(. ~ region_name) +
    scale_fill_manual(values = colors, breaks = names(colors), drop = FALSE,
                      guide = guide_legend(nrow = 2, byrow = TRUE)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.13))) +
    labs(title = title, x = NULL, y = y_label, fill = NULL) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(colour = "#E5EAED", linewidth = 0.35),
          axis.text = element_text(colour = "#263B4B"),
          axis.title = element_text(colour = "#263B4B"),
          plot.title = element_text(face = "bold", colour = "#173747", size = 12),
          strip.text = element_text(face = "bold", colour = "#173747"),
          strip.background = element_rect(fill = "#F1F5F6", colour = NA),
          legend.position = "bottom", legend.text = element_text(size = 8.5),
          legend.key.width = grid::unit(0.8, "lines"),
          plot.margin = margin(8, 12, 6, 8))
}

p_a <- make_panel("a", "a  Primary energy", primary_colors, "EJ/yr")
p_b <- make_panel("b", "b  Electricity generation", power_colors, "EJ/yr")
p_c <- make_panel("c", "c  Final energy", final_colors, "EJ/yr")
p_d <- make_panel("d", "d  Carbon dioxide removal", cdr_colors, "GtCO2/yr")
figure <- (p_a | p_b) / (p_c | p_d) + plot_layout(heights = c(1, 1))

png_file <- file.path(output_dir, "Fig5_energy_CDR_provider_recipient_2050_clean_top.png")
svg_file <- file.path(output_dir, "Fig5_energy_CDR_provider_recipient_2050_clean_top.svg")
ggsave(png_file, figure, width = 18, height = 13.5, dpi = 300, bg = "white")
grDevices::svg(svg_file, width = 18, height = 13.5, bg = "white")
print(figure)
grDevices::dev.off()
write.csv(plot_data %>% mutate(scenario = as.character(scenario),
                               region_name = as.character(region_name)),
          file.path(output_dir, "Fig5_energy_CDR_provider_recipient_2050.csv"),
          row.names = FALSE)
message("Saved: ", normalizePath(png_file, winslash = "/"))
message("Saved: ", normalizePath(svg_file, winslash = "/"))
