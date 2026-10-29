# Sensitivity of global inequality, mean expenditure, and welfare in 2050 ----
#
# This figure is the 2050 sensitivity counterpart to panels c-f of
# 04b_poverty_inequality_welfare_detailed_expenditure_2020_2050.png.
# It uses the same detailed country-by-expenditure-segment distribution,
# population recovery, global weighted-Gini calculation, welfare index, and
# exact two-factor Shapley decomposition. SSP3 10k and 15k are excluded.

find_project_root <- function(start) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    marker <- file.path(
      current, "data", "AR6database", "AnalysisExpenditure.gdx"
    )
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
  "C:/GAMS/34",
  "C:/GAMS/win64/26.1"
)
gams_candidates <- unique(
  gams_candidates[nzchar(gams_candidates) & dir.exists(gams_candidates)]
)
if (!length(gams_candidates)) stop("A GAMS system directory was not found.")
igdx(gams_candidates[[1]])

gdx_file <- file.path(
  project_root, "data", "AR6database", "AnalysisExpenditure.gdx"
)
output_dir <- file.path(project_root, "output", "Figure", "analysis")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_stem <- file.path(
  output_dir, "09_sensitivity_inequality_welfare_2050_SSP1_SSP3"
)

# Settings ------------------------------------------------------------------

comparison_year <- 2050
population_threshold <- "pop_8.30"
ssp_levels <- paste0("SSP", 1:3)
policy_levels <- c("BaU", "Default", "10th", "15th", "20th")
policy_non_bau <- policy_levels[-1]

scenario_grid <- crossing(
  SSP = ssp_levels,
  Policy = policy_levels
) %>%
  mutate(
    Scenario_id = case_when(
      Policy == "BaU" ~ paste0(SSP, "_BaU_NoCC_No"),
      Policy == "Default" ~ paste0(SSP, "_400C_2030CP_NoCC_No"),
      TRUE ~ paste0(SSP, "_400C_2030CP_", Policy, "_NoCC_No")
    )
  ) %>%
  filter(!(SSP == "SSP3" & Policy %in% c("10th", "15th")))

policy_labels <- c(
  "BaU" = "BaU", "Default" = "Default", "10th" = "10k",
  "15th" = "15k", "20th" = "20k"
)
ssp_colours <- c("SSP1" = "#0072B2", "SSP2" = "#D55E00", "SSP3" = "#009E73")
ssp_shapes <- c("SSP1" = 16, "SSP2" = 17, "SSP3" = 15)
effect_colours <- c(
  "Mean consumption expenditure" = "#6B5B95",
  "Interpersonal distribution" = "#2A9D8F"
)
ink <- "#252525"

check_grid <- function(data, expected, keys, context) {
  if (anyDuplicated(as.data.frame(data[keys]))) {
    stop("Duplicate records in ", context, ".")
  }
  missing <- anti_join(expected, data, by = keys)
  if (nrow(missing)) {
    stop(
      "Missing ", nrow(missing), " records in ", context, "; first: ",
      paste(unlist(missing[1, ]), collapse = " / ")
    )
  }
}

weighted_gini <- function(expenditure, population) {
  valid <- is.finite(expenditure) & expenditure >= 0 &
    is.finite(population) & population > 0
  expenditure <- expenditure[valid]
  population <- population[valid]
  if (!length(expenditure) || sum(expenditure * population) <= 0) return(NA_real_)
  ordering <- order(expenditure)
  expenditure <- expenditure[ordering]
  population <- population[ordering]
  population_share <- population / sum(population)
  expenditure_share <-
    population * expenditure / sum(population * expenditure)
  cumulative_population <- c(0, cumsum(population_share))
  cumulative_expenditure <- c(0, cumsum(expenditure_share))
  1 - sum(
    diff(cumulative_population) *
      (head(cumulative_expenditure, -1) + tail(cumulative_expenditure, -1))
  )
}

# Detailed expenditure distribution -----------------------------------------

message("Reading detailed expenditure distributions from: ", gdx_file)

budget <- rgdx.param(gdx_file, "BudgetInitial") %>%
  transmute(
    Region_code = as.character(R),
    Segment = as.character(Seg),
    Expenditure = as.numeric(BudgetInitial)
  )

frequency <- rgdx.param(gdx_file, "FreqSegall_exp") %>%
  transmute(
    Region_code = as.character(R),
    Scenario_id = as.character(Ref),
    Year = as.numeric(as.character(Y)),
    Segment = as.character(Seg),
    Frequency = as.numeric(FreqSegall_exp)
  ) %>%
  filter(
    Scenario_id %in% scenario_grid$Scenario_id,
    Year == comparison_year
  )

available_scenarios <- sort(unique(frequency$Scenario_id))
missing_scenarios <- setdiff(scenario_grid$Scenario_id, available_scenarios)
if (length(missing_scenarios)) {
  stop(
    "FreqSegall_exp is missing scenario(s): ",
    paste(missing_scenarios, collapse = ", ")
  )
}
if (
  any(!is.finite(budget$Expenditure) | budget$Expenditure <= 0) ||
    any(!is.finite(frequency$Frequency) | frequency$Frequency < 0)
) {
  stop("Detailed expenditure and frequency inputs must be finite and valid.")
}

# Use one balanced country panel for every SSP-policy comparison. A complete
# cell contains all expenditure bins available for that country.
budget_bins <- budget %>% count(Region_code, name = "Budget_bins")
frequency_cells <- frequency %>%
  count(Region_code, Scenario_id, Year, name = "Frequency_bins") %>%
  left_join(budget_bins, by = "Region_code") %>%
  filter(Frequency_bins == Budget_bins)
balanced_regions <- frequency_cells %>%
  count(Region_code, name = "Complete_cells") %>%
  filter(Complete_cells == nrow(scenario_grid)) %>%
  pull(Region_code)
if (!length(balanced_regions)) stop("No balanced country panel was found.")

poverty_count <- rgdx.param(gdx_file, "PoVExp") %>%
  transmute(
    Scenario_id = as.character(Ref),
    Region_code = as.character(R),
    Year = as.numeric(as.character(Y)),
    Threshold = as.character(TH),
    People = as.numeric(PoVExp)
  ) %>%
  filter(
    Scenario_id %in% scenario_grid$Scenario_id,
    Region_code %in% balanced_regions,
    Year == comparison_year,
    Threshold == population_threshold
  )

poverty_rate <- rgdx.param(gdx_file, "RoPExp") %>%
  transmute(
    Scenario_id = as.character(Ref),
    Region_code = as.character(R),
    Year = as.numeric(as.character(Y)),
    Threshold = as.character(TH),
    Poverty_rate = as.numeric(RoPExp)
  ) %>%
  filter(
    Scenario_id %in% scenario_grid$Scenario_id,
    Region_code %in% balanced_regions,
    Year == comparison_year,
    Threshold == population_threshold
  )

country_population <- poverty_count %>%
  inner_join(
    poverty_rate,
    by = c("Scenario_id", "Region_code", "Year", "Threshold")
  ) %>%
  transmute(
    Scenario_id,
    Region_code,
    Year,
    Population = People / Poverty_rate
  )

check_grid(
  country_population,
  crossing(
    Scenario_id = scenario_grid$Scenario_id,
    Region_code = balanced_regions,
    Year = comparison_year
  ),
  c("Scenario_id", "Region_code", "Year"),
  "country population used for detailed expenditure weights"
)
if (any(!is.finite(country_population$Population) |
          country_population$Population <= 0)) {
  stop("Recovered country populations must be finite and positive.")
}

frequency_sums <- frequency %>%
  filter(Region_code %in% balanced_regions) %>%
  group_by(Scenario_id, Region_code, Year) %>%
  summarise(Frequency_sum = sum(Frequency), .groups = "drop")
if (any(!is.finite(frequency_sums$Frequency_sum) |
          frequency_sums$Frequency_sum <= 0)) {
  stop("Each country/scenario/year must have a positive frequency sum.")
}

detailed_distribution <- frequency %>%
  filter(Region_code %in% balanced_regions) %>%
  left_join(
    frequency_sums,
    by = c("Scenario_id", "Region_code", "Year")
  ) %>%
  mutate(Population_share = Frequency / Frequency_sum) %>%
  inner_join(budget, by = c("Region_code", "Segment")) %>%
  inner_join(
    country_population,
    by = c("Scenario_id", "Region_code", "Year")
  ) %>%
  mutate(Population_weight = Population_share * Population)

metrics <- detailed_distribution %>%
  group_by(Scenario_id, Year) %>%
  summarise(
    Mean_expenditure = weighted.mean(Expenditure, Population_weight),
    Gini = weighted_gini(Expenditure, Population_weight),
    Population = sum(Population_weight),
    .groups = "drop"
  ) %>%
  inner_join(scenario_grid, by = "Scenario_id") %>%
  mutate(
    SSP = factor(SSP, levels = ssp_levels),
    Policy = factor(Policy, levels = policy_levels),
    Mean_expenditure_thousand = Mean_expenditure / 1000,
    Gini_percent = 100 * Gini,
    Gini_adjusted_welfare = Mean_expenditure * (1 - Gini)
  ) %>%
  group_by(SSP) %>%
  mutate(
    BaU_welfare = Gini_adjusted_welfare[Policy == "BaU"],
    Welfare_index = 100 * Gini_adjusted_welfare / BaU_welfare
  ) %>%
  ungroup() %>%
  arrange(SSP, Policy)

check_grid(
  metrics,
  scenario_grid %>%
    transmute(
      SSP = factor(SSP, levels = ssp_levels),
      Policy = factor(Policy, levels = policy_levels)
    ),
  c("SSP", "Policy"),
  "2050 detailed expenditure metrics"
)
if (
  any(!is.finite(metrics$Mean_expenditure) | metrics$Mean_expenditure <= 0) ||
    any(!is.finite(metrics$Gini) | metrics$Gini < 0 | metrics$Gini > 1) ||
    any(!is.finite(metrics$Welfare_index))
) {
  stop("Detailed expenditure metrics are outside their valid ranges.")
}

# Exact two-factor Shapley decomposition -------------------------------------
# WI/100 = M * D, where M is the mean-expenditure ratio and D is the equality
# ratio (1-G)/(1-G_BaU). Contributions sum exactly to WI - 100.

bau_metrics <- metrics %>%
  filter(Policy == "BaU") %>%
  select(
    SSP,
    BaU_mean_expenditure = Mean_expenditure,
    BaU_gini = Gini
  )

decomposition <- metrics %>%
  filter(Policy != "BaU") %>%
  left_join(bau_metrics, by = "SSP") %>%
  mutate(
    Mean_ratio = Mean_expenditure / BaU_mean_expenditure,
    Equality_ratio = (1 - Gini) / (1 - BaU_gini),
    `Mean consumption expenditure` =
      100 * (Mean_ratio - 1) * (1 + Equality_ratio) / 2,
    `Interpersonal distribution` =
      100 * (Equality_ratio - 1) * (1 + Mean_ratio) / 2,
    Net = `Mean consumption expenditure` + `Interpersonal distribution`,
    Reported_net = Welfare_index - 100
  )

decomposition_error <- max(abs(decomposition$Net - decomposition$Reported_net))
if (!is.finite(decomposition_error) || decomposition_error > 1e-8) {
  stop("The exact decomposition does not reproduce the welfare index.")
}

decomposition_long <- decomposition %>%
  select(
    SSP, Policy,
    `Mean consumption expenditure`, `Interpersonal distribution`
  ) %>%
  pivot_longer(
    c(`Mean consumption expenditure`, `Interpersonal distribution`),
    names_to = "Effect",
    values_to = "Contribution"
  ) %>%
  mutate(
    SSP = factor(SSP, levels = ssp_levels),
    Policy = factor(Policy, levels = policy_non_bau),
    Effect = factor(Effect, levels = names(effect_colours))
  )

net_data <- decomposition %>%
  transmute(
    SSP = factor(SSP, levels = ssp_levels),
    Policy = factor(Policy, levels = policy_non_bau),
    Net,
    Label = sprintf("%+.1f", Net)
  )

# Complete the x-axis grid so excluded SSP3 10k and 15k remain visibly blank.
metric_plot_data <- crossing(
  SSP = factor(ssp_levels, levels = ssp_levels),
  Policy = factor(policy_levels, levels = policy_levels)
) %>%
  left_join(
    metrics %>%
      select(SSP, Policy, Gini_percent, Mean_expenditure_thousand, Welfare_index),
    by = c("SSP", "Policy")
  )

# Export plotted source data for checking and reuse.
metric_source <- metrics %>%
  select(
    SSP, Policy, Scenario_id, Year, Population, Mean_expenditure,
    Mean_expenditure_thousand, Gini_percent, Welfare_index
  ) %>%
  mutate(
    Mean_contribution = NA_real_, Distribution_contribution = NA_real_,
    Net_WI_change = Welfare_index - 100
  )
decomposition_source <- decomposition %>%
  select(
    SSP, Policy, Scenario_id, Year, Population, Mean_expenditure,
    Mean_expenditure_thousand, Gini_percent, Welfare_index,
    `Mean consumption expenditure`, `Interpersonal distribution`, Net
  ) %>%
  rename(
    Mean_contribution = `Mean consumption expenditure`,
    Distribution_contribution = `Interpersonal distribution`,
    Net_WI_change = Net
  )
write.csv(
  metric_source %>%
    select(-Mean_contribution, -Distribution_contribution, -Net_WI_change) %>%
    left_join(
      decomposition_source %>%
        select(
          SSP, Policy, Mean_contribution, Distribution_contribution,
          Net_WI_change
        ),
      by = c("SSP", "Policy")
    ),
  paste0(output_stem, "_data.csv"),
  row.names = FALSE
)

# Figure style ---------------------------------------------------------------

theme_nature <- function() {
  theme_classic(base_family = "Arial", base_size = 8) +
    theme(
      text = element_text(colour = ink),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 7.0, colour = ink),
      axis.ticks = element_line(colour = ink, linewidth = 0.24),
      axis.ticks.length = grid::unit(1.1, "mm"),
      axis.line = element_blank(),
      panel.border = element_rect(fill = NA, colour = ink, linewidth = 0.32),
      panel.grid.major.y = element_line(colour = "#E2E2E2", linewidth = 0.22),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption = element_blank(),
      plot.tag = element_text(face = "bold", size = 8.5, colour = ink),
      plot.tag.position = "topleft",
      legend.title = element_text(size = 7.2, face = "plain"),
      legend.text = element_text(size = 6.9),
      legend.key.height = grid::unit(3.3, "mm"),
      legend.key.width = grid::unit(5.5, "mm"),
      legend.spacing.x = grid::unit(0.8, "mm"),
      legend.margin = margin(1, 0, 0, 0, unit = "mm"),
      plot.margin = margin(4, 4, 3, 4, unit = "mm")
    )
}

metric_panel <- function(data, y, y_label, reference = NULL, accuracy = 0.1) {
  panel <- ggplot(
    data,
    aes(
      x = Policy,
      y = .data[[y]],
      colour = SSP,
      shape = SSP,
      group = SSP
    )
  )
  if (!is.null(reference)) {
    panel <- panel + geom_hline(
      yintercept = reference,
      linetype = "22",
      colour = "#8A8A8A",
      linewidth = 0.28
    )
  }
  panel +
    geom_line(linewidth = 0.60, na.rm = TRUE) +
    geom_point(size = 2.05, stroke = 0.55, na.rm = TRUE) +
    scale_colour_manual(
      values = ssp_colours,
      breaks = ssp_levels,
      drop = FALSE,
      guide = guide_legend(title = NULL, nrow = 1)
    ) +
    scale_shape_manual(
      values = ssp_shapes,
      breaks = ssp_levels,
      drop = FALSE,
      guide = guide_legend(title = NULL, nrow = 1)
    ) +
    scale_x_discrete(labels = policy_labels, drop = FALSE) +
    scale_y_continuous(
      labels = label_number(accuracy = accuracy),
      expand = expansion(mult = c(0.10, 0.12))
    ) +
    labs(x = NULL, y = y_label) +
    theme_nature() +
    theme(
      legend.position = "top",
      legend.justification = "center",
      axis.text.x = element_text(angle = 32, hjust = 1, vjust = 1)
    )
}

# Panels ---------------------------------------------------------------------

p_a <- metric_panel(
  metric_plot_data,
  "Gini_percent",
  "Global interpersonal\nexpenditure Gini (%)",
  accuracy = 0.1
)

p_b <- metric_panel(
  metric_plot_data,
  "Mean_expenditure_thousand",
  expression(atop(
    "Mean annual consumption expenditure",
    paste("(10"^3, " US$ person"^{-1}, " yr"^{-1}, ")")
  )),
  accuracy = 0.1
)

p_c <- metric_panel(
  metric_plot_data,
  "Welfare_index",
  "Welfare index\n(BaU = 100)",
  reference = 100,
  accuracy = 0.1
)

p_d <- ggplot(
  decomposition_long,
  aes(x = Policy, y = Contribution, fill = Effect)
) +
  geom_hline(yintercept = 0, colour = ink, linewidth = 0.32) +
  geom_col(width = 0.62, colour = "white", linewidth = 0.16) +
  geom_point(
    data = net_data,
    aes(x = Policy, y = Net, shape = "Net change"),
    inherit.aes = FALSE,
    size = 2.30,
    stroke = 0.62,
    fill = "white",
    colour = ink
  ) +
  geom_text(
    data = net_data,
    aes(
      x = Policy,
      y = Net,
      label = Label,
      vjust = ifelse(Net >= 0, -0.75, 1.45)
    ),
    inherit.aes = FALSE,
    family = "Arial",
    size = 2.05,
    colour = ink
  ) +
  facet_wrap(~SSP, nrow = 1, drop = FALSE) +
  scale_x_discrete(labels = policy_labels[policy_non_bau], drop = FALSE) +
  scale_fill_manual(
    values = effect_colours,
    breaks = names(effect_colours),
    drop = FALSE
  ) +
  scale_shape_manual(values = c("Net change" = 23)) +
  scale_y_continuous(
    labels = label_number(accuracy = 1),
    expand = expansion(mult = c(0.15, 0.20))
  ) +
  labs(
    x = NULL,
    y = "Contribution to WI change relative to BaU\n(percentage points)",
    fill = NULL,
    shape = NULL
  ) +
  guides(
    fill = guide_legend(order = 1, nrow = 1),
    shape = guide_legend(order = 2, nrow = 1)
  ) +
  theme_nature() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    strip.background = element_blank(),
    strip.text = element_text(size = 8.0, face = "bold", margin = margin(b = 1.5))
  )

top_row <- (
  (p_a | p_b | p_c) + plot_layout(guides = "collect")
) &
  theme(
    legend.position = "top",
    legend.justification = "center",
    legend.margin = margin(0, 0, 1.5, 0, unit = "mm")
  )

figure <- (
  top_row / p_d +
    plot_layout(heights = c(0.92, 1.08)) +
    plot_annotation(tag_levels = "a")
) &
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    plot.tag = element_text(
      family = "Arial", face = "bold", size = 8.5, colour = ink
    ),
    plot.tag.position = "topleft"
  )

ggsave(
  filename = paste0(output_stem, ".png"),
  plot = figure,
  device = "png",
  type = "cairo",
  width = 180,
  height = 155,
  units = "mm",
  dpi = 400,
  bg = "white",
  limitsize = FALSE
)

message("Created: ", paste0(output_stem, ".png"))
message("Created: ", paste0(output_stem, "_data.csv"))
message("Balanced detailed-expenditure countries: ", length(balanced_regions))
message(
  "Within-country frequency-sum range before normalisation: ",
  sprintf(
    "%.6f to %.6f",
    min(frequency_sums$Frequency_sum),
    max(frequency_sums$Frequency_sum)
  )
)
message(
  "Maximum exact welfare-decomposition error: ",
  format(decomposition_error, scientific = TRUE)
)
print(
  metrics %>%
    select(SSP, Policy, Mean_expenditure, Gini_percent, Welfare_index)
)
