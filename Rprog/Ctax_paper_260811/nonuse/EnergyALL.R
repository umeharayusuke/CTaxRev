library_paths <- c(Sys.getenv("R_LIBS_USER"), "C:/ENVI5809/R_library")
library_paths <- library_paths[nzchar(library_paths) & dir.exists(library_paths)]
.libPaths(unique(c(library_paths, .libPaths())))
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(gdxrrw)
})

# プロジェクト直下、またはこのスクリプトのフォルダから実行できる。
gdx_candidates <- c("data/AR6database/global_17_IAMC.gdx",
                    "../../data/AR6database/global_17_IAMC.gdx")
gdx_file <- gdx_candidates[file.exists(gdx_candidates)][1]
if (is.na(gdx_file)) stop("data/AR6database/global_17_IAMC.gdx が見つかりません。")
output_dir <- if (dir.exists("output")) "output/Figure" else "../../output/Figure"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

gams_candidates <- c(Sys.getenv("GAMS_SYSDIR"), "C:/GAMS/34", "C:/GAMS/win64/26.1")
gams_candidates <- gams_candidates[nzchar(gams_candidates) & dir.exists(gams_candidates)]
if (length(gams_candidates)) igdx(gams_candidates[1])

# 2050年の政策シナリオを比較する。
scenario_names <- c(
  "SSP2_400C_2030CP_NoCC_No" = "Def",
  "SSP2_400C_2030CP_15th_NoCC_No" = "Aid"
)
year <- 2050L

# mergeIAMC.gms の IAMC_template は SCENARIO, REMF, VEMF, YEMF の4次元。
raw <- rgdx.param(gdx_file, "IAMC_template")
required_columns <- c("SCENARIO", "REMF", "VEMF", "YEMF", "IAMC_Template")
if (!all(required_columns %in% names(raw))) {
  stop("IAMC_template の列が想定と異なります: ", paste(names(raw), collapse = ", "))
}
missing_scenarios <- setdiff(names(scenario_names), unique(as.character(raw[["SCENARIO"]])))
if (length(missing_scenarios)) stop("指定シナリオがありません: ", paste(missing_scenarios, collapse = ", "))

# GDXに収録された15thの地域集計を直接使う。
region_names <- c(Rprovider15th = "Provider", Rrecipient15th = "Recipient")
regions <- names(region_names)

spec <- tibble::tribble(
  ~variable,       ~indicator,               ~scale,
  "Prm_Ene",       "Primary energy",         1,
  "Fin_Ene",       "Final energy",           1,
  "Sec_Ene_Ele",   "Electricity generation", 1,
  "Fin_Ene_Ele",   "Final electricity use",  1,
  "CNS",           "Consumption",            1,
  "Val_Add_Ind",   "Industrial value added", 1,
  "Trd_Goo_Val",   "Goods trade balance",    1,
  "Car_Seq_CDR",   "CDR",                    1 / 1000
)

# GDXはゼロを省略する場合がある。貿易・CDRの欠損だけゼロとして補う。
keep <- as.character(raw[["SCENARIO"]]) %in% names(scenario_names) &
  as.character(raw[["REMF"]]) %in% regions &
  as.character(raw[["VEMF"]]) %in% spec$variable &
  as.character(raw[["YEMF"]]) == as.character(year)
selected <- raw[keep, , drop = FALSE] %>%
  transmute(scenario = unname(scenario_names[as.character(SCENARIO)]),
            region = as.character(REMF), variable = as.character(VEMF),
            value = as.numeric(IAMC_Template))
rm(raw)
if (anyDuplicated(selected[c("scenario", "region", "variable")])) {
  stop("対象データにシナリオ・地域・変数の重複があります。")
}

grid <- tidyr::expand_grid(
  scenario = unname(scenario_names),
  region = regions,
  variable = spec$variable
)

dat <- grid %>%
  left_join(selected, by = c("scenario", "region", "variable"))

missing_core <- dat %>%
  filter(is.na(value),
         !variable %in% c("Trd_Goo_Val", "Car_Seq_CDR"))
if (nrow(missing_core) > 0L) {
  print(missing_core)
  stop("主要指標に欠損があります。変数・地域・対象年を確認してください。")
}

# CDRがシナリオごとに全く記録されていなければ、ゼロと断定しない。
if (any(!unname(scenario_names) %in% selected$scenario[selected$variable == "Car_Seq_CDR"])) {
  stop("Def/Aidの一方でCDRの記録がありません。")
}

dat <- dat %>%
  mutate(
    value = replace_na(value, 0),
    group = unname(region_names[region])
  ) %>%
  group_by(scenario, group, variable) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  left_join(spec, by = "variable") %>%
  mutate(value = value * scale,
         scenario = factor(scenario, levels = c("Def", "Aid")),
         group = factor(group, levels = c("Provider", "Recipient")),
         indicator = factor(indicator, levels = spec$indicator))

change <- dat %>%
  select(group, indicator, scenario, value) %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(change_pct = 100 * (Aid - Def) / abs(Def))
if (any(!is.finite(change$change_pct))) {
  stop("Defがゼロ、または有効でない値のため変化率を計算できません。")
}

print(change)

change <- change %>%
  mutate(label_pct = if_else(abs(change_pct) < 0.1,
                             sprintf("%+.2f%%", change_pct),
                             sprintf("%+.1f%%", change_pct)))
p_pct <- ggplot(change, aes(x = change_pct, y = group, fill = group)) +
  geom_vline(xintercept = 0, colour = "#596579", linewidth = 0.5) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_text(aes(label = label_pct,
                hjust = if_else(change_pct < 0, 1.1, -0.1)), size = 3.6) +
  facet_wrap(~ indicator, ncol = 2, scales = "free_x") +
  scale_fill_manual(values = c("Provider" = "#315D88",
                               "Recipient" = "#D88937")) +
  scale_x_continuous(labels = scales::label_number(suffix = "%"),
                     expand = expansion(mult = 0.23)) +
  labs(title = "Aid vs Def in 2050: relative change",
       subtitle = "(Aid - Def) / |Def| x 100. Positive = higher under Aid; negative = lower. Scales differ by panel.",
       caption = "Regions: Rprovider15th and Rrecipient15th. Electricity generation is total secondary electricity; final electricity use is a subset of final energy.",
       x = "Change relative to |Def| (%)", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

save_plot <- function(plot, stem) {
  png_file <- file.path(output_dir, paste0(stem, ".png"))
  svg_file <- file.path(output_dir, paste0(stem, ".svg"))
  ggsave(png_file, plot, width = 12, height = 10, dpi = 300, bg = "white")
  grDevices::svg(svg_file, width = 12, height = 10, bg = "white")
  print(plot)
  grDevices::dev.off()
  message("Saved: ", normalizePath(png_file, winslash = "/"))
  message("Saved: ", normalizePath(svg_file, winslash = "/"))
}
save_plot(p_pct, "EnergyALL_Def_Aid_2050_percent_change_provider_recipient")
write.csv(change, file.path(output_dir, "EnergyALL_Def_Aid_2050_percent_change_provider_recipient.csv"), row.names = FALSE)
