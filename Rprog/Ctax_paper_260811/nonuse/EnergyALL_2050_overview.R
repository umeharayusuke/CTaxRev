# Def-to-Aid changes in 2050, ordered along the electricity-system flow.
# Run from the project root or from Rprog/Ctax_paper_260811.

library_paths <- c(Sys.getenv("R_LIBS_USER"), "C:/ENVI5809/R_library")
library_paths <- library_paths[nzchar(library_paths) & dir.exists(library_paths)]
.libPaths(unique(c(library_paths, .libPaths())))
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(gdxrrw)
})

root <- if (dir.exists("data/AR6database")) "." else "../.."
gdx_file <- file.path(root, "data/AR6database/global_17_IAMC.gdx")
if (!file.exists(gdx_file)) stop("Missing GDX: ", gdx_file)
output_dir <- file.path(root, "output/Figure")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
gams_dirs <- c(Sys.getenv("GAMS_SYSDIR"), "C:/GAMS/34", "C:/GAMS/win64/26.1")
gams_dirs <- gams_dirs[nzchar(gams_dirs) & dir.exists(gams_dirs)]
if (length(gams_dirs)) igdx(gams_dirs[1])

scenarios <- c(Def = "SSP2_400C_2030CP_NoCC_No",
               Aid = "SSP2_400C_2030CP_15th_NoCC_No")
advanced <- c("XE25", "XER", "TUR", "XOC", "JPN", "CAN", "USA")
developing <- c("CHN", "IND", "XSE", "XSA", "BRA", "XLM", "CIS", "XME", "XNF", "XAF")
groups <- c("Advanced (R2OECD)", "Developing (R2NonOECD)")

# The four rows follow the structure of the proposed Sankey: inputs,
# electricity generation, electricity use/trade, then wider model outcomes.
spec <- tibble::tribble(
  ~code,                      ~stage,                   ~indicator,             ~scale,
  "Prm_Ene_Coa_Ele",         "1  Fuel input to power", "Coal",                1,
  "Prm_Ene_Gas_Ele",         "1  Fuel input to power", "Gas",                 1,
  "Prm_Ene_Oil_Ele",         "1  Fuel input to power", "Oil",                 1,
  "Prm_Ene_Bio_Ele",         "1  Fuel input to power", "Biomass",             1,
  "Sec_Ene_Ele_Coa",         "2  Electricity supply", "Coal",                1,
  "Sec_Ene_Ele_Gas",         "2  Electricity supply", "Gas",                 1,
  "Sec_Ene_Ele_Oil",         "2  Electricity supply", "Oil",                 1,
  "Sec_Ene_Ele_Bio",         "2  Electricity supply", "Biomass",             1,
  "Sec_Ene_Ele_NonBioRen",   "2  Electricity supply", "Non-bio renewables",  1,
  "Sec_Ene_Ele_Nuc",         "2  Electricity supply", "Nuclear",             1,
  "Sec_Ene_Ele",             "2  Electricity supply", "Total generation",    1,
  "Fin_Ene_Ind_Ele",         "3  Electricity use",    "Industry",            1,
  "Fin_Ene_Res_and_Com_Ele", "3  Electricity use",    "Buildings",           1,
  "Fin_Ene_Tra_Ele",         "3  Electricity use",    "Transport",           1,
  "Fin_Ene_Ele",             "3  Electricity use",    "Total final use",     1,
  "Trd_Sec_Ene_Ele_Vol",     "3  Electricity use",    "Net exports",         1,
  "Prm_Ene",                 "4  Wider outcomes",     "Primary energy",      1,
  "Fin_Ene",                 "4  Wider outcomes",     "Final energy",        1,
  "CNS",                     "4  Wider outcomes",     "Consumption",         1,
  "Val_Add_Ind",             "4  Wider outcomes",     "Industry value added", 1,
  "Trd_Goo_Val",             "4  Wider outcomes",     "Goods trade balance", 1,
  "Car_Seq_CDR",             "4  Wider outcomes",     "CDR",                 1 / 1000
)

raw <- rgdx.param(gdx_file, "IAMC_template")
required <- c("SCENARIO", "REMF", "VEMF", "YEMF", "IAMC_Template")
if (!all(required %in% names(raw))) stop("Unexpected IAMC_template columns.")
selected <- raw %>%
  filter(as.character(SCENARIO) %in% unname(scenarios),
         as.character(REMF) %in% c(advanced, developing),
         as.character(VEMF) %in% spec$code,
         as.character(YEMF) == "2050") %>%
  transmute(scenario = names(scenarios)[match(as.character(SCENARIO), scenarios)],
            region = as.character(REMF), code = as.character(VEMF),
            value = as.numeric(IAMC_Template))
rm(raw)
if (anyDuplicated(selected[c("scenario", "region", "code")])) {
  stop("Duplicate scenario-region-variable records in GDX.")
}
if (any(!is.finite(selected$value))) stop("Non-finite values in selected GDX records.")

grid <- expand_grid(scenario = names(scenarios), region = c(advanced, developing),
                    code = spec$code)
missing <- grid %>% anti_join(selected, by = c("scenario", "region", "code"))
# GDX may omit exact zeroes, especially for oil-fired generation and net trade.
# Do not silently treat a missing major aggregate as zero.
core <- c("Prm_Ene", "Fin_Ene", "Sec_Ene_Ele", "Fin_Ene_Ele", "CNS", "Val_Add_Ind")
missing_core <- missing %>% filter(code %in% core)
if (nrow(missing_core)) {
  print(missing_core)
  stop("Required aggregate records are missing.")
}
if (any(!names(scenarios) %in% selected$scenario[selected$code == "Car_Seq_CDR"])) {
  stop("CDR is absent for at least one scenario.")
}

values <- grid %>%
  left_join(selected, by = c("scenario", "region", "code")) %>%
  mutate(value = replace_na(value, 0),
         group = if_else(region %in% advanced, groups[1], groups[2])) %>%
  group_by(scenario, group, code) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  left_join(spec, by = "code") %>%
  mutate(value = value * scale)

comparison <- values %>%
  select(group, stage, indicator, code, scenario, value) %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(change_pct = if_else(abs(Def) > 1e-9, 100 * (Aid - Def) / abs(Def), NA_real_),
         label = if_else(is.na(change_pct), "n/a", sprintf("%+.1f%%", change_pct)),
         group = factor(group, levels = rev(groups)),
         stage = factor(stage, levels = unique(spec$stage)),
         item = factor(code, levels = spec$code, labels = spec$indicator))

# Same percent scale in every stage. Colors saturate at +/-50% while printed
# cell values retain the uncapped estimate, so small-baseline outliers remain visible.
color_cap <- 50
p <- ggplot(comparison, aes(x = item, y = group, fill = change_pct)) +
  geom_tile(colour = "white", linewidth = 0.9, height = 0.88) +
  geom_text(aes(label = label), size = 3.2, colour = "#17283C") +
  facet_wrap(~ stage, ncol = 1, scales = "free_x", strip.position = "top") +
  scale_fill_gradient2(low = "#376F9E", mid = "#F3F5F6", high = "#D88737",
                       midpoint = 0, limits = c(-color_cap, color_cap),
                       oob = scales::squish, na.value = "#D8DDE1",
                       name = "Aid vs Def (%)") +
  labs(title = "2050: how Aid changes the energy system and economy",
       subtitle = "Flow-ordered overview: fuel input -> electricity supply -> electricity use -> wider outcomes",
       caption = "(Aid - Def) / |Def| x 100. Positive = higher under Aid. Colors cap at +/-50%; labels do not. Net exports cross zero in both groups.",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(), strip.text = element_text(face = "bold", size = 11),
        axis.text.x = element_text(size = 9, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 10.5),
        legend.position = "bottom", legend.key.width = grid::unit(2.1, "cm"))

png_file <- file.path(output_dir, "EnergyALL_2050_overview_percent.png")
svg_file <- file.path(output_dir, "EnergyALL_2050_overview_percent.svg")
ggsave(png_file, p, width = 15, height = 10, dpi = 300, bg = "white")
grDevices::svg(svg_file, width = 15, height = 10, bg = "white")
print(p)
grDevices::dev.off()
write.csv(comparison %>% mutate(group = as.character(group), stage = as.character(stage)) %>%
            select(group, stage, indicator, code, Def, Aid, change_pct),
          file.path(output_dir, "EnergyALL_2050_overview_percent.csv"), row.names = FALSE)
message("Saved: ", normalizePath(png_file, winslash = "/"))
message("Saved: ", normalizePath(svg_file, winslash = "/"))
