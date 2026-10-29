library_candidates <- c(
  Sys.getenv("R_LIBS_USER"),
  file.path(getwd(), "Rlib"),
  "C:/ENVI5809/R_library",
  "C:/Users/umeha/AppData/Local/R/win-library/4.5"
)
library_candidates <- unique(
  library_candidates[nzchar(library_candidates) & dir.exists(library_candidates)]
)
.libPaths(unique(c(library_candidates, .libPaths())))

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(gdxrrw)
})

gams_dirs <- c("C:/GAMS/34", "C:/GAMS/win64/26.1")
for (gams_dir in gams_dirs) {
  if (dir.exists(gams_dir)) {
    igdx(gams_dir)
    break
  }
}

comparison_year <- 2050
population_threshold <- "pop_8.30"
ssp_levels <- paste0("SSP", 1:3)
policy_levels <- c("BaU", "Default", "10th", "15th", "20th")

scenario_grid <- crossing(
  SSP = factor(ssp_levels, levels = ssp_levels),
  Policy = factor(policy_levels, levels = policy_levels)
) %>%
  mutate(
    Scenario_id = case_when(
      Policy == "BaU" ~ paste0(SSP, "_BaU_NoCC_No"),
      Policy == "Default" ~ paste0(SSP, "_400C_2030CP_NoCC_No"),
      TRUE ~ paste0(SSP, "_400C_2030CP_", Policy, "_NoCC_No")
    )
  ) %>%
  filter(!(SSP == "SSP3" & Policy %in% c("10th", "15th")))

gdx_file <- "data/AR6database/AnalysisExpenditure.gdx"
if (!file.exists(gdx_file)) {
  stop("GDX file not found: ", gdx_file)
}

budget <- rgdx.param(gdx_file, "BudgetInitial") %>%
  transmute(
    Region_code = as.character(R),
    Segment = as.character(Seg),
    Expenditure = as.numeric(BudgetInitial)
  ) %>%
  filter(is.finite(Expenditure), Expenditure >= 0)

frequency <- rgdx.param(gdx_file, "FreqSegall_exp") %>%
  transmute(
    Region_code = as.character(R),
    Scenario_id = as.character(Ref),
    Year = as.integer(as.character(Y)),
    Segment = as.character(Seg),
    Frequency = as.numeric(FreqSegall_exp)
  ) %>%
  filter(
    Scenario_id %in% scenario_grid$Scenario_id,
    Year == comparison_year,
    is.finite(Frequency),
    Frequency >= 0
  )

missing_scenarios <- setdiff(scenario_grid$Scenario_id, unique(frequency$Scenario_id))
if (length(missing_scenarios) > 0) {
  stop("Missing scenarios in FreqSegall_exp: ", paste(missing_scenarios, collapse = ", "))
}

budget_bins <- budget %>% count(Region_code, name = "Budget_bins")
frequency_cells <- frequency %>%
  count(Region_code, Scenario_id, Year, name = "Frequency_bins") %>%
  left_join(budget_bins, by = "Region_code") %>%
  filter(Frequency_bins == Budget_bins)

balanced_regions <- frequency_cells %>%
  count(Region_code, name = "Complete_cells") %>%
  filter(Complete_cells == nrow(scenario_grid)) %>%
  pull(Region_code)

if (length(balanced_regions) == 0) {
  stop("No balanced country panel was found.")
}

poverty_count <- rgdx.param(gdx_file, "PoVExp") %>%
  transmute(
    Region_code = as.character(R),
    Scenario_id = as.character(Ref),
    Year = as.integer(as.character(Y)),
    Poverty_line = as.character(TH),
    People = as.numeric(PoVExp)
  ) %>%
  filter(
    Scenario_id %in% scenario_grid$Scenario_id,
    Region_code %in% balanced_regions,
    Year == comparison_year,
    Poverty_line == population_threshold,
    is.finite(People)
  )

poverty_rate <- rgdx.param(gdx_file, "RoPExp") %>%
  transmute(
    Region_code = as.character(R),
    Scenario_id = as.character(Ref),
    Year = as.integer(as.character(Y)),
    Poverty_line = as.character(TH),
    Poverty_rate = as.numeric(RoPExp)
  ) %>%
  filter(
    Scenario_id %in% scenario_grid$Scenario_id,
    Region_code %in% balanced_regions,
    Year == comparison_year,
    Poverty_line == population_threshold,
    is.finite(Poverty_rate),
    Poverty_rate > 0
  )

country_population <- poverty_count %>%
  inner_join(
    poverty_rate,
    by = c("Region_code", "Scenario_id", "Year", "Poverty_line")
  ) %>%
  transmute(
    Region_code,
    Scenario_id,
    Year,
    Population = People / Poverty_rate
  ) %>%
  filter(is.finite(Population), Population > 0)

expected_population_cells <- nrow(scenario_grid) * length(balanced_regions)
if (nrow(country_population) != expected_population_cells) {
  stop(
    "Incomplete country population panel: ", nrow(country_population),
    " cells found; expected ", expected_population_cells, "."
  )
}

frequency_sums <- frequency %>%
  filter(Region_code %in% balanced_regions) %>%
  group_by(Region_code, Scenario_id, Year) %>%
  summarise(Frequency_sum = sum(Frequency), .groups = "drop")

if (any(!is.finite(frequency_sums$Frequency_sum) | frequency_sums$Frequency_sum <= 0)) {
  stop("Non-positive frequency sum found.")
}

detailed_distribution <- frequency %>%
  filter(Region_code %in% balanced_regions) %>%
  left_join(frequency_sums, by = c("Region_code", "Scenario_id", "Year")) %>%
  mutate(Population_share = Frequency / Frequency_sum) %>%
  inner_join(budget, by = c("Region_code", "Segment")) %>%
  inner_join(country_population, by = c("Region_code", "Scenario_id", "Year")) %>%
  mutate(Population_weight = Population_share * Population) %>%
  filter(
    is.finite(Expenditure), Expenditure >= 0,
    is.finite(Population_weight), Population_weight > 0
  )

lorenz_cells <- detailed_distribution %>%
  inner_join(scenario_grid, by = "Scenario_id") %>%
  group_by(SSP, Policy, Scenario_id, Year, Expenditure) %>%
  summarise(Population_weight = sum(Population_weight), .groups = "drop") %>%
  arrange(SSP, Policy, Expenditure) %>%
  group_by(SSP, Policy, Scenario_id, Year) %>%
  mutate(
    Expenditure_weight = Expenditure * Population_weight,
    Cumulative_population_pct = 100 * cumsum(Population_weight) / sum(Population_weight),
    Cumulative_expenditure_pct = 100 * cumsum(Expenditure_weight) / sum(Expenditure_weight)
  ) %>%
  ungroup() %>%
  select(SSP, Policy, Scenario_id, Year, Cumulative_population_pct, Cumulative_expenditure_pct)

origin_rows <- scenario_grid %>%
  transmute(
    SSP,
    Policy,
    Scenario_id,
    Year = comparison_year,
    Cumulative_population_pct = 0,
    Cumulative_expenditure_pct = 0
  )

lorenz_data <- bind_rows(origin_rows, lorenz_cells) %>%
  arrange(SSP, Policy, Cumulative_population_pct)

curve_checks <- lorenz_data %>%
  group_by(SSP, Policy, Scenario_id, Year) %>%
  summarise(
    start_population = first(Cumulative_population_pct),
    start_expenditure = first(Cumulative_expenditure_pct),
    end_population = last(Cumulative_population_pct),
    end_expenditure = last(Cumulative_expenditure_pct),
    monotone_population = all(diff(Cumulative_population_pct) >= -1e-10),
    monotone_expenditure = all(diff(Cumulative_expenditure_pct) >= -1e-10),
    .groups = "drop"
  )

if (
  any(abs(curve_checks$start_population) > 1e-8) ||
  any(abs(curve_checks$start_expenditure) > 1e-8) ||
  any(abs(curve_checks$end_population - 100) > 1e-6) ||
  any(abs(curve_checks$end_expenditure - 100) > 1e-6) ||
  any(!curve_checks$monotone_population) ||
  any(!curve_checks$monotone_expenditure)
) {
  stop("Lorenz curve validation failed.")
}

gini_check <- lorenz_data %>%
  group_by(SSP, Policy, Scenario_id) %>%
  summarise(
    Gini = 1 - sum(
      diff(Cumulative_population_pct / 100) *
        (head(Cumulative_expenditure_pct / 100, -1) +
           tail(Cumulative_expenditure_pct / 100, -1))
    ),
    .groups = "drop"
  )

policy_colours <- c(
  "BaU" = "#4D4D4D",
  "Default" = "#8C6BB1",
  "10th" = "#0072B2",
  "15th" = "#D55E00",
  "20th" = "#009E73"
)
policy_labels <- c(
  "BaU" = "BaU",
  "Default" = "Default",
  "10th" = "10k USD",
  "15th" = "15k USD",
  "20th" = "20k USD"
)
policy_linetypes <- c(
  "BaU" = "22",
  "Default" = "longdash",
  "10th" = "solid",
  "15th" = "solid",
  "20th" = "solid"
)

plot_lorenz <- ggplot(
  lorenz_data,
  aes(
    x = Cumulative_population_pct,
    y = Cumulative_expenditure_pct,
    colour = Policy,
    linetype = Policy,
    group = Policy
  )
) +
  geom_abline(
    intercept = 0,
    slope = 1,
    colour = "#8A8A8A",
    linewidth = 0.28,
    linetype = "dotted"
  ) +
  geom_line(linewidth = 0.45, lineend = "round") +
  facet_wrap(~SSP, nrow = 1) +
  scale_colour_manual(
    values = policy_colours,
    breaks = policy_levels,
    labels = policy_labels,
    drop = FALSE
  ) +
  scale_linetype_manual(
    values = policy_linetypes,
    breaks = policy_levels,
    labels = policy_labels,
    drop = FALSE
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, 20),
    labels = label_number(accuracy = 1),
    expand = expansion(mult = 0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    labels = label_number(accuracy = 1),
    expand = expansion(mult = 0)
  ) +
  coord_equal(xlim = c(0, 100), ylim = c(0, 100), clip = "off") +
  labs(
    x = "Cumulative population share (%)",
    y = "Cumulative expenditure share (%)",
    colour = NULL,
    linetype = NULL
  ) +
  guides(
    colour = guide_legend(nrow = 1, byrow = TRUE),
    linetype = guide_legend(nrow = 1, byrow = TRUE)
  ) +
  theme_bw(base_family = "Arial", base_size = 8) +
  theme(
    panel.grid.major = element_line(colour = "#E8E8E8", linewidth = 0.25),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", linewidth = 0.45),
    strip.background = element_blank(),
    strip.text = element_text(size = 9, face = "bold", margin = margin(b = 3)),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 7.5, colour = "black"),
    axis.ticks = element_line(linewidth = 0.35),
    legend.position = "bottom",
    legend.justification = "center",
    legend.key.width = unit(12, "mm"),
    legend.key.height = unit(3, "mm"),
    legend.text = element_text(size = 8),
    legend.margin = margin(t = 1, r = 0, b = 0, l = 0),
    plot.margin = margin(5, 5, 3, 5)
  )

output_dir <- "output/Figure/analysis"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

png_file <- file.path(output_dir, "14_sensitivity_lorenz_curves_2050_SSP1_SSP3.png")
csv_file <- file.path(output_dir, "14_sensitivity_lorenz_curves_2050_SSP1_SSP3_data.csv")
gini_file <- file.path(output_dir, "14_sensitivity_lorenz_curves_2050_SSP1_SSP3_gini_check.csv")

ggsave(
  filename = png_file,
  plot = plot_lorenz,
  width = 180,
  height = 83,
  units = "mm",
  dpi = 400,
  bg = "white"
)

write.csv(lorenz_data, csv_file, row.names = FALSE, fileEncoding = "UTF-8")
write.csv(gini_check, gini_file, row.names = FALSE, fileEncoding = "UTF-8")

message("Balanced regions: ", length(balanced_regions))
message("Saved: ", png_file)
message("Saved: ", csv_file)
message("Saved: ", gini_file)
print(gini_check)
