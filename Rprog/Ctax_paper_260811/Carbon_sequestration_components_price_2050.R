# Carbon sequestration and carbon prices in 2050: Def versus Aid.
# Panels b/c adapt Car_Seq_components_2100.R (Equity/Interim_M2) to the
# current CTax GDX, scenarios, year and AIM17 order. Panel d adds Prc_Car.
# Only PNG and SVG are exported; the older source and its figures are untouched.

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
providers <- c("XE25", "JPN", "USA", "XER", "XOC", "CAN", "TUR", "XLM", "CHN", "CIS")
recipients <- c("XME", "XNF", "BRA", "XAF", "XSE", "IND", "XSA")
regions <- c(providers, recipients)
region_names <- c(
  XE25 = "EU25", JPN = "Japan", USA = "United States", XER = "Rest of Europe",
  XOC = "Oceania", CAN = "Canada", TUR = "Turkiye", XLM = "Latin America",
  CHN = "China", CIS = "Former Soviet Union", XME = "Middle East",
  XNF = "North Africa", BRA = "Brazil", XAF = "Rest of Africa",
  XSE = "Southeast Asia", IND = "India", XSA = "Rest of Asia"
)
component_codes <- c("Car_Seq_CCS_Fos", "Car_Seq_CCS_Ind_Pro", "Car_Seq_CCS_Bio",
                     "Car_Seq_Dir_Air_Cap", "Car_Seq_Enh_Wea", "Car_Seq_Lan_Use")
component_names <- c("Fossil CCS", "Industrial-process CCS", "BECCS",
                     "DACCS", "EW", "Land use")
names(component_names) <- component_codes
component_colors <- c("Fossil CCS" = "#4D4D4D",
                      "Industrial-process CCS" = "#D55E00",
                      "BECCS" = "#009E73", "DACCS" = "#0072B2",
                      "EW" = "#CC79A7", "Land use" = "#8C6D31")
scenario_colors <- c(Def = "#657787", Aid = "#087D8A")
offsets <- c(Def = -0.18, Aid = 0.18)
year <- 2050L

raw <- rgdx.param(gdx_file, "IAMC_template")
required <- c("SCENARIO", "REMF", "VEMF", "YEMF", "IAMC_Template")
if (!all(required %in% names(raw))) stop("Unexpected IAMC_template columns.")
selected <- raw %>%
  filter(as.character(SCENARIO) %in% unname(scenarios),
         as.character(REMF) %in% c("World", regions),
         as.character(VEMF) %in% c(component_codes, "Prc_Car"),
         as.character(YEMF) == as.character(year)) %>%
  transmute(scenario = names(scenarios)[match(as.character(SCENARIO), scenarios)],
            region = as.character(REMF), variable = as.character(VEMF),
            value = as.numeric(IAMC_Template))
rm(raw)
if (anyDuplicated(selected[c("scenario", "region", "variable")])) {
  stop("Duplicate scenario-region-variable records.")
}
if (any(!is.finite(selected$value))) stop("Non-finite input values.")

units <- rgdx.set(gdx_file, "VUMAP") %>%
  transmute(variable = as.character(VEMF), unit = as.character(UEMF))
expected_units <- c(rep("Mt CO2/yr", length(component_codes)), "USD_2010/t CO2")
actual_units <- units$unit[match(c(component_codes, "Prc_Car"), units$variable)]
if (!identical(actual_units, expected_units)) {
  stop("Unexpected GDX units: ", paste(actual_units, collapse = ", "))
}

# The IAMC template is sparse for unused sequestration technologies.
sequestration <- expand_grid(scenario = names(scenarios),
                             region = c("World", regions), variable = component_codes) %>%
  left_join(selected %>% filter(variable %in% component_codes),
            by = c("scenario", "region", "variable")) %>%
  mutate(value = replace_na(value, 0) / 1000,
         component = factor(unname(component_names[variable]),
                            levels = unname(component_names)),
         scenario = factor(scenario, levels = names(scenarios)))
if (any(sequestration$value < -1e-9)) stop("Negative sequestration component values.")

price <- selected %>% filter(variable == "Prc_Car")
missing_price <- anti_join(expand_grid(scenario = names(scenarios),
                                      region = c("World", regions)),
                           price, by = c("scenario", "region"))
if (nrow(missing_price)) {
  print(missing_price)
  stop("Missing carbon prices.")
}
price <- price %>% mutate(scenario = factor(scenario, levels = names(scenarios)))

theme_carbon <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E5EAED", linewidth = 0.35),
        plot.title = element_text(face = "bold", colour = "#1C3445", size = 12),
        axis.title = element_text(colour = "#1C3445"),
        axis.text = element_text(colour = "#1C3445"),
        legend.position = "bottom", legend.title = element_blank(),
        plot.margin = margin(8, 12, 8, 8))

# b: two World bars, with the original six-component colour mapping.
world <- sequestration %>% filter(region == "World") %>%
  mutate(scenario_y = factor(scenario, levels = rev(names(scenarios))))
world_totals <- world %>% group_by(scenario_y) %>%
  summarise(total = sum(value), .groups = "drop")
p_world <- ggplot(world, aes(value, scenario_y, fill = component)) +
  geom_col(width = 0.62, position = position_stack(reverse = TRUE)) +
  geom_text(data = world_totals, aes(x = total, y = scenario_y,
                                    label = sprintf("%.1f", total)),
            inherit.aes = FALSE, hjust = -0.15, size = 3.5, colour = "#1C3445") +
  scale_fill_manual(values = component_colors, breaks = names(component_colors),
                    guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(title = "b  World carbon sequestration", x = "GtCO2/yr", y = NULL) +
  theme_carbon + theme(panel.grid.major.y = element_blank())

# c: 17 AIM regions in the provider-to-recipient order used elsewhere here.
regional <- sequestration %>% filter(region != "World") %>%
  mutate(region_index = match(region, regions),
         x = region_index + unname(offsets[as.character(scenario)]))
regional_totals <- regional %>% group_by(region, scenario, x) %>%
  summarise(total = sum(value), .groups = "drop")
first_region_labels <- regional_totals %>% filter(region == regions[[1]]) %>%
  mutate(label_y = total + 0.035 * max(regional_totals$total))
p_regions <- ggplot(regional, aes(x, value, fill = component)) +
  geom_col(width = 0.31, position = position_stack(reverse = TRUE)) +
  geom_text(data = first_region_labels,
            aes(x = x, y = label_y, label = scenario), inherit.aes = FALSE,
            angle = 90, hjust = 0, size = 3.1, colour = "#1C3445") +
  geom_vline(xintercept = length(providers) + 0.5, colour = "#AEBBC4",
             linetype = "dashed", linewidth = 0.45) +
  scale_fill_manual(values = component_colors, breaks = names(component_colors),
                    guide = guide_legend(nrow = 1, byrow = TRUE)) +
  scale_x_continuous(breaks = seq_along(regions),
                     labels = unname(region_names[regions]),
                     limits = c(0.45, length(regions) + 0.55),
                     expand = expansion(mult = 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.07))) +
  labs(title = "c  AIM17 carbon sequestration", x = NULL, y = "GtCO2/yr") +
  theme_carbon +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8.5),
        legend.text = element_text(size = 9), panel.grid.major.x = element_blank())

# d: carbon-price dumbbells on the same regional axis; no secondary axis.
price_regional <- price %>% filter(region != "World") %>%
  mutate(region_index = match(region, regions),
         x = region_index + unname(offsets[as.character(scenario)]))
price_pairs <- price_regional %>% select(region, region_index, scenario, value) %>%
  pivot_wider(names_from = scenario, values_from = value)
p_price <- ggplot() +
  geom_vline(xintercept = length(providers) + 0.5, colour = "#AEBBC4",
             linetype = "dashed", linewidth = 0.45) +
  geom_segment(data = price_pairs,
               aes(x = region_index + offsets[["Def"]],
                   xend = region_index + offsets[["Aid"]],
                   y = Def, yend = Aid),
               colour = "#AAB7C0", linewidth = 0.7) +
  geom_point(data = price_regional, aes(x, value, colour = scenario), size = 2.6) +
  scale_colour_manual(values = scenario_colors, name = NULL) +
  scale_x_continuous(breaks = seq_along(regions),
                     labels = unname(region_names[regions]),
                     limits = c(0.45, length(regions) + 0.55),
                     expand = expansion(mult = 0)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.12))) +
  labs(title = "d  Carbon price", x = NULL, y = "US$2010/tCO2") +
  theme_carbon +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8.5),
        legend.position = "bottom")

figure <- p_world / p_regions / p_price +
  plot_layout(heights = c(1.15, 3.2, 2.15))
png_file <- file.path(output_dir, "Carbon_sequestration_components_price_2050_labeled.png")
svg_file <- file.path(output_dir, "Carbon_sequestration_components_price_2050_labeled.svg")
ggsave(png_file, figure, width = 20, height = 15, dpi = 300, bg = "white")
grDevices::svg(svg_file, width = 20, height = 15, bg = "white")
print(figure)
grDevices::dev.off()
message("Saved: ", normalizePath(png_file, winslash = "/"))
message("Saved: ", normalizePath(svg_file, winslash = "/"))
