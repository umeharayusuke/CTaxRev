# Relative importance of SSP and provider/recipient classification -----------
#
# The 10k, 15k, and 20k cases are treated as three categorical region
# partitions, not as observations on a continuous GDP-per-capita scale.
# Sensitivity is the mean absolute pairwise change produced by each driver,
# divided by the full observed range of the metric. SSP differences are
# computed within each available partition and then averaged with equal weight
# across partitions. Partition differences are computed within SSP1 and SSP2
# and then averaged with equal weight across these SSPs. SSP3 10k and 15k are
# excluded because those runs are invalid.

find_project_root <- function(start) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    marker <- file.path(current, "data", "AR6database", "global_17_IAMC.gdx")
    if (file.exists(marker)) return(current)
    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }
  NA_character_
}

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg[[1]]), winslash = "/"))
} else {
  getwd()
}

project_root <- find_project_root(getwd())
if (is.na(project_root)) project_root <- find_project_root(script_dir)
if (is.na(project_root)) stop("Could not locate the project root.")

library_candidates <- c(
  Sys.getenv("R_LIBS_USER"),
  file.path(project_root, "Rlib"),
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
  library(patchwork)
  library(gdxrrw)
})

gams_candidates <- c(
  Sys.getenv("GAMS_SYSDIR", unset = ""),
  Sys.getenv("GAMSDIR", unset = ""),
  "C:/GAMS/34",
  "C:/GAMS/win64/26.1"
)
gams_candidates <- unique(
  gams_candidates[nzchar(gams_candidates) & dir.exists(gams_candidates)]
)
if (!length(gams_candidates)) stop("A GAMS system directory was not found.")
igdx(gams_candidates[[1]])

comparison_year <- 2050
ssp_levels <- paste0("SSP", 1:3)
threshold_levels <- c("10th", "15th", "20th")

scenario_grid <- crossing(
  SSP = ssp_levels,
  Threshold = threshold_levels
) %>%
  mutate(
    Scenario_id = paste0(SSP, "_400C_2030CP_", Threshold, "_NoCC_No"),
    Provider_region = paste0("Rprovider", Threshold)
  ) %>%
  filter(!(SSP == "SSP3" & Threshold %in% c("10th", "15th")))

if (nrow(scenario_grid) != 7L) {
  stop("The valid sensitivity grid must contain seven Aid scenarios.")
}

# IAMC outcomes: provider carbon-tax revenue and World cumulative loss -------

iamc_file <- file.path(
  project_root, "data", "AR6database", "global_17_IAMC.gdx"
)
iamc_raw <- rgdx.param(iamc_file, "IAMC_template") %>%
  transmute(
    Scenario_id = as.character(SCENARIO),
    Region_code = as.character(REMF),
    Variable = as.character(VEMF),
    Year = as.numeric(as.character(YEMF)),
    Value = as.numeric(IAMC_Template)
  )

revenue <- iamc_raw %>%
  inner_join(
    scenario_grid,
    by = c("Scenario_id", "Region_code" = "Provider_region")
  ) %>%
  filter(
    Variable == "Rev_gov_Tax_Car_Tax",
    Year == comparison_year
  ) %>%
  transmute(
    SSP,
    Threshold,
    Scenario_id,
    Metric = "Carbon revenue",
    Value = Value / 1e6,
    Unit = "trillion US$2010 yr-1"
  )

consumption_loss <- iamc_raw %>%
  inner_join(
    scenario_grid %>% select(SSP, Threshold, Scenario_id),
    by = "Scenario_id"
  ) %>%
  filter(
    Region_code == "World",
    Variable == "Pol_Cos_Cns_Los_rat_NPV_5pc",
    Year == comparison_year
  ) %>%
  transmute(
    SSP,
    Threshold,
    Scenario_id,
    Metric = "Consumption loss",
    Value,
    Unit = "% (NPV 5%, through 2050)"
  )

# Additional World poverty population relative to each SSP's BaU ------------

expenditure_file <- file.path(
  project_root, "data", "AR6database", "AnalysisExpenditure.gdx"
)
bau_grid <- tibble(
  SSP = ssp_levels,
  BaU_scenario = paste0(ssp_levels, "_BaU_NoCC_No")
)

poverty_raw <- rgdx.param(expenditure_file, "PoVExp") %>%
  transmute(
    Scenario_id = as.character(Ref),
    Region_code = as.character(R),
    Year = as.numeric(as.character(Y)),
    Poverty_line = as.character(TH),
    People = as.numeric(PoVExp)
  ) %>%
  filter(
    Scenario_id %in% c(scenario_grid$Scenario_id, bau_grid$BaU_scenario),
    Region_code == "WLD",
    Year == comparison_year,
    Poverty_line == "pop_3.00"
  )

bau_poverty <- poverty_raw %>%
  inner_join(bau_grid, by = c("Scenario_id" = "BaU_scenario")) %>%
  select(SSP, BaU_people = People)

poverty <- poverty_raw %>%
  inner_join(
    scenario_grid %>% select(SSP, Threshold, Scenario_id),
    by = "Scenario_id"
  ) %>%
  left_join(bau_poverty, by = "SSP") %>%
  transmute(
    SSP,
    Threshold,
    Scenario_id,
    Metric = "Additional poverty",
    Value = (People - BaU_people) / 1e6,
    Unit = "million people vs SSP-specific BaU"
  )

# Welfare index from the established detailed-expenditure sensitivity output -

wi_file <- file.path(
  project_root, "output", "Figure", "analysis", "csv",
  "09_sensitivity_inequality_welfare_2050_SSP1_SSP3_data.csv"
)
if (!file.exists(wi_file)) {
  stop(
    "Required WI sensitivity data were not found. Run ",
    "Sensitivity_inequality_welfare_2050_SSP1_SSP3.R first."
  )
}

distribution_source <- read.csv(wi_file, stringsAsFactors = FALSE) %>%
  mutate(
    SSP = as.character(SSP),
    Threshold = as.character(Policy),
    Scenario_id = as.character(Scenario_id)
  )

distribution_bau <- distribution_source %>%
  filter(Threshold == "BaU") %>%
  transmute(
    SSP,
    BaU_mean_expenditure = as.numeric(Mean_expenditure),
    BaU_gini_percent = as.numeric(Gini_percent)
  )

distribution_metrics <- distribution_source %>%
  filter(
    Threshold %in% threshold_levels,
    !(SSP == "SSP3" & Threshold %in% c("10th", "15th"))
  ) %>%
  left_join(distribution_bau, by = "SSP")

mean_consumption <- distribution_metrics %>%
  transmute(
    SSP,
    Threshold,
    Scenario_id,
    Metric = "Mean consumption change",
    Value = 100 * (
      as.numeric(Mean_expenditure) / BaU_mean_expenditure - 1
    ),
    Unit = "% vs SSP-specific BaU"
  )

global_gini <- distribution_metrics %>%
  transmute(
    SSP,
    Threshold,
    Scenario_id,
    Metric = "Gini change",
    Value = as.numeric(Gini_percent) - BaU_gini_percent,
    Unit = "percentage points vs SSP-specific BaU"
  )

wi <- distribution_metrics %>%
  transmute(
    SSP,
    Threshold,
    Scenario_id,
    Metric = "Welfare index",
    Value = as.numeric(Welfare_index),
    Unit = "BaU = 100"
  )

metric_levels <- c(
  "Carbon revenue",
  "Consumption loss",
  "Additional poverty",
  "Mean consumption change",
  "Gini change",
  "Welfare index"
)

metric_labels <- c(
  "Carbon revenue" = "Provider carbon-tax revenue\n(trillion US$2010 yr-1)",
  "Consumption loss" = "World cumulative consumption loss\n(%, NPV 5%, through 2050)",
  "Additional poverty" = "World additional poverty population\n(million people, US$3/day, vs BaU)",
  "Mean consumption change" = "World mean consumption change\n(%, vs SSP-specific BaU)",
  "Gini change" = "Global expenditure Gini change\n(percentage points, vs SSP-specific BaU)",
  "Welfare index" = "World welfare index\n(BaU = 100)"
)

outcome_data <- bind_rows(
  revenue,
  consumption_loss,
  poverty,
  mean_consumption,
  global_gini,
  wi
) %>%
  mutate(
    SSP = factor(SSP, levels = ssp_levels),
    Threshold = factor(Threshold, levels = threshold_levels),
    Metric = factor(Metric, levels = metric_levels)
  ) %>%
  arrange(Metric, Threshold, SSP)

reference_data <- tibble(
  Metric = factor(metric_levels, levels = metric_levels),
  Reference = c(0, 0, 0, 0, 0, 100)
)

cell_check <- outcome_data %>% count(Metric, name = "Cells")
if (
  nrow(cell_check) != length(metric_levels) ||
  any(cell_check$Cells != nrow(scenario_grid)) ||
  any(!is.finite(outcome_data$Value))
) {
  stop("At least one metric does not contain the seven valid scenario cells.")
}

mean_pairwise_difference <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2L) return(NA_real_)
  differences <- abs(outer(x, x, "-"))
  mean(differences[upper.tri(differences)])
}

# SSP effect: pairwise SSP changes at a fixed classification, equally averaged
# across the 10k, 15k, and 20k classifications.
ssp_components <- outcome_data %>%
  group_by(Metric, Threshold) %>%
  summarise(
    Levels_compared = n(),
    Pairwise_change = mean_pairwise_difference(Value),
    .groups = "drop"
  ) %>%
  filter(Levels_compared >= 2L)

ssp_sensitivity <- ssp_components %>%
  group_by(Metric) %>%
  summarise(
    Raw_sensitivity = mean(Pairwise_change),
    Comparison_basis = paste0(
      "Mean across 10k/15k/20k partitions; SSP levels per partition = ",
      paste(Levels_compared, collapse = "/")
    ),
    .groups = "drop"
  ) %>%
  mutate(Driver = "SSP")

# Classification effect: pairwise changes among the three discrete partitions
# within SSP1 and SSP2. SSP3 is excluded because its 10k/15k runs are invalid.
classification_components <- outcome_data %>%
  group_by(Metric, SSP) %>%
  summarise(
    Levels_compared = n(),
    Pairwise_change = mean_pairwise_difference(Value),
    .groups = "drop"
  ) %>%
  filter(Levels_compared == length(threshold_levels))

classification_sensitivity <- classification_components %>%
  group_by(Metric) %>%
  summarise(
    Raw_sensitivity = mean(Pairwise_change),
    Comparison_basis = "Mean across SSP1 and SSP2; 10k/15k/20k treated categorically",
    .groups = "drop"
  ) %>%
  mutate(Driver = "Provider/recipient classification")

metric_ranges <- outcome_data %>%
  group_by(Metric) %>%
  summarise(
    Observed_min = min(Value),
    Observed_max = max(Value),
    Observed_range = Observed_max - Observed_min,
    .groups = "drop"
  )

sensitivity_summary <- bind_rows(
  ssp_sensitivity,
  classification_sensitivity
) %>%
  left_join(metric_ranges, by = "Metric") %>%
  mutate(
    Sensitivity_pct_range = 100 * Raw_sensitivity / Observed_range,
    Metric = factor(Metric, levels = rev(metric_levels)),
    Driver = factor(
      Driver,
      levels = c("SSP", "Provider/recipient classification")
    )
  )

if (
  any(!is.finite(sensitivity_summary$Sensitivity_pct_range)) ||
  any(sensitivity_summary$Sensitivity_pct_range < 0)
) {
  stop("Sensitivity indices are invalid.")
}

dominance_summary <- sensitivity_summary %>%
  select(Metric, Driver, Sensitivity_pct_range) %>%
  pivot_wider(names_from = Driver, values_from = Sensitivity_pct_range) %>%
  mutate(
    Dominant_driver = if_else(
      SSP >= `Provider/recipient classification`,
      "SSP",
      "Provider/recipient classification"
    ),
    Dominance_ratio = pmax(SSP, `Provider/recipient classification`) /
      pmin(SSP, `Provider/recipient classification`)
  )

# Plot -----------------------------------------------------------------------

ssp_colours <- c(
  "SSP1" = "#0072B2",
  "SSP2" = "#D55E00",
  "SSP3" = "#009E73"
)
ssp_shapes <- c("SSP1" = 16, "SSP2" = 17, "SSP3" = 15)
driver_colours <- c(
  "SSP" = "#4E79A7",
  "Provider/recipient classification" = "#E28E2C"
)
driver_shapes <- c(
  "SSP" = 16,
  "Provider/recipient classification" = 17
)
ink <- "#252525"

theme_nature <- function() {
  theme_bw(base_family = "Arial", base_size = 8) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "#E5E5E5", linewidth = 0.25),
      panel.border = element_rect(colour = ink, linewidth = 0.42),
      strip.background = element_blank(),
      strip.text = element_text(size = 7.8, face = "bold", margin = margin(b = 2)),
      axis.title = element_text(size = 8.3),
      axis.text = element_text(size = 7.2, colour = ink),
      axis.ticks = element_line(linewidth = 0.32),
      legend.title = element_blank(),
      legend.text = element_text(size = 7.5),
      legend.key.height = unit(3.5, "mm"),
      legend.key.width = unit(6, "mm"),
      plot.margin = margin(4, 5, 4, 5)
    )
}

p_outcomes <- ggplot(
  outcome_data,
  aes(x = Threshold, y = Value, colour = SSP, shape = SSP, group = SSP)
) +
  geom_hline(
    data = reference_data,
    aes(yintercept = Reference),
    inherit.aes = FALSE,
    colour = "#777777",
    linewidth = 0.32,
    linetype = "dotted"
  ) +
  geom_line(linewidth = 0.48, na.rm = TRUE) +
  geom_point(size = 2.15, stroke = 0.45) +
  facet_wrap(
    ~Metric,
    ncol = 2,
    scales = "free_y",
    labeller = as_labeller(metric_labels)
  ) +
  scale_colour_manual(values = ssp_colours, drop = FALSE) +
  scale_shape_manual(values = ssp_shapes, drop = FALSE) +
  scale_x_discrete(
    labels = c(
      "10th" = "10k USD",
      "15th" = "15k USD",
      "20th" = "20k USD"
    ),
    drop = FALSE
  ) +
  scale_y_continuous(
    labels = label_number(accuracy = 0.1),
    expand = expansion(mult = c(0.08, 0.12))
  ) +
  labs(
    x = "Provider/recipient classification based on 2025 GDP per capita",
    y = NULL
  ) +
  guides(
    colour = guide_legend(nrow = 1, byrow = TRUE),
    shape = guide_legend(nrow = 1, byrow = TRUE)
  ) +
  theme_nature() +
  theme(
    legend.position = "top",
    legend.justification = "left",
    panel.spacing = unit(7, "mm")
  )

segment_data <- sensitivity_summary %>%
  select(Metric, Driver, Sensitivity_pct_range) %>%
  pivot_wider(names_from = Driver, values_from = Sensitivity_pct_range)

label_nudge <- max(sensitivity_summary$Sensitivity_pct_range) * 0.025

p_sensitivity <- ggplot(
  sensitivity_summary,
  aes(
    x = Sensitivity_pct_range,
    y = Metric,
    colour = Driver,
    shape = Driver
  )
) +
  geom_segment(
    data = segment_data,
    aes(
      x = SSP,
      xend = `Provider/recipient classification`,
      y = Metric,
      yend = Metric
    ),
    inherit.aes = FALSE,
    colour = "#A7A7A7",
    linewidth = 0.42
  ) +
  geom_point(size = 2.8, stroke = 0.55) +
  geom_text(
    aes(label = sprintf("%.1f", Sensitivity_pct_range)),
    hjust = -0.18,
    size = 2.25,
    family = "Arial",
    show.legend = FALSE
  ) +
  scale_colour_manual(values = driver_colours, drop = FALSE) +
  scale_shape_manual(values = driver_shapes, drop = FALSE) +
  scale_x_continuous(
    labels = label_number(accuracy = 1, suffix = "%"),
    limits = c(0, max(sensitivity_summary$Sensitivity_pct_range) + 5 * label_nudge),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "Sensitivity index (mean pairwise change / observed range)",
    y = NULL,
    caption = paste0(
      "10k, 15k and 20k are treated as discrete region partitions; ",
      "SSP3 10k/15k are excluded.\n",
      "Mean consumption and Gini are changes from each SSP-specific BaU.\n",
      "For provider revenue, the classification effect includes changes ",
      "in the membership of the provider aggregate."
    )
  ) +
  guides(
    colour = guide_legend(nrow = 1, byrow = TRUE),
    shape = guide_legend(nrow = 1, byrow = TRUE)
  ) +
  theme_nature() +
  theme(
    legend.position = "top",
    legend.justification = "left",
    axis.text.y = element_text(face = "bold"),
    plot.caption = element_text(size = 6.6, colour = "#4D4D4D", hjust = 0),
    panel.grid.major.y = element_blank()
  )

figure <- p_outcomes / p_sensitivity +
  plot_layout(heights = c(2.05, 1.25)) +
  plot_annotation(tag_levels = "a") &
  theme(
    plot.tag = element_text(
      family = "Arial", face = "bold", size = 10, colour = ink
    )
  )

# Output ---------------------------------------------------------------------

figure_dir <- file.path(project_root, "output", "Figure", "analysis", "tem")
csv_dir <- file.path(project_root, "output", "Figure", "analysis", "csv")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)

output_file <- file.path(
  figure_dir, "15_sensitivity_driver_comparison_2050.png"
)
outcome_file <- file.path(
  csv_dir, "15_sensitivity_driver_comparison_2050_outcomes.csv"
)
sensitivity_file <- file.path(
  csv_dir, "15_sensitivity_driver_comparison_2050_indices.csv"
)
dominance_file <- file.path(
  csv_dir, "15_sensitivity_driver_comparison_2050_dominance.csv"
)

ggsave(
  filename = output_file,
  plot = figure,
  device = "png",
  type = "cairo",
  width = 180,
  height = 250,
  units = "mm",
  dpi = 400,
  bg = "white"
)

write.csv(outcome_data, outcome_file, row.names = FALSE, fileEncoding = "UTF-8")
write.csv(
  sensitivity_summary,
  sensitivity_file,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
write.csv(
  dominance_summary,
  dominance_file,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

message("Created: ", output_file)
message("Created: ", outcome_file)
message("Created: ", sensitivity_file)
message("Created: ", dominance_file)
print(sensitivity_summary)
print(dominance_summary)
