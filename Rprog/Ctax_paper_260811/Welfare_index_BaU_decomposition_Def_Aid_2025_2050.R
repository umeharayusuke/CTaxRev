# Exact decomposition of Def/Aid welfare-index changes relative to BaU
#
# EDE = mean consumption * (1 - Atkinson index). Its change relative to BaU
# is decomposed into a mean-consumption effect and a between-region
# distribution effect using the exact, order-invariant two-factor Shapley
# decomposition.
#
# Input:
#   output/Figure/csv/
#     Between_region_inequality_welfare_BaU_Def_Aid_2025_2050.csv
# Outputs:
#   output/Figure/Welfare_index_BaU_decomposition_Def_Aid_2025_2050.png
#   output/Figure/Welfare_index_BaU_decomposition_Def_Aid_2025_2050.svg
#   output/Figure/csv/
#     Welfare_index_BaU_decomposition_Def_Aid_2025_2050.csv

project_library_candidates <- c(
  file.path(getwd(), ".r-lib"),
  "C:/ENVI5809/R_library",
  "C:/Users/umeha/AppData/Local/R/win-library/4.5"
)
project_library_candidates <- project_library_candidates[
  dir.exists(project_library_candidates)
]
if (length(project_library_candidates) > 0) {
  .libPaths(unique(c(project_library_candidates, .libPaths())))
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
})

# Paths -----------------------------------------------------------------------

find_project_dir <- function(start_dir) {
  candidate <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(candidate, "CTaxRev.Rproj"))) return(candidate)
    parent <- dirname(candidate)
    if (identical(parent, candidate)) stop("Could not find the CTaxRev project root.")
    candidate <- parent
  }
}

command_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", command_args, value = TRUE)
start_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(
    sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE
  ))
} else {
  getwd()
}

project_dir <- find_project_dir(start_dir)
output_dir <- file.path(project_dir, "output", "Figure")
csv_dir <- file.path(output_dir, "csv")
input_name <- "Between_region_inequality_welfare_BaU_Def_Aid_2025_2050.csv"
input_file <- file.path(csv_dir, input_name)
output_name <- "Welfare_index_BaU_decomposition_Def_Aid_2025_2050"
output_stem <- file.path(output_dir, output_name)
csv_output <- file.path(csv_dir, paste0(output_name, ".csv"))

if (!file.exists(input_file)) {
  stop(
    "Missing input: ", input_file, "\n",
    "Run Between_region_inequality_welfare_BaU_Def_Aid_2025_2050.R first."
  )
}

# Exact two-factor decomposition ---------------------------------------------

scenario_levels <- c("Def", "Aid")
target_years <- c(2025, 2050)

metric_data <- read.csv(input_file, check.names = FALSE) %>%
  as_tibble() %>%
  filter(Year %in% target_years)

required_columns <- c(
  "Scenario", "Year", "mean_consumption_thousand_USD2010_per_capita",
  "EDE_consumption_thousand_USD2010_per_capita",
  "Atkinson_index_percent", "Welfare_index_BaU_equals_100"
)
missing_columns <- setdiff(required_columns, names(metric_data))
if (length(missing_columns) > 0) {
  stop("Input CSV is missing column(s): ", paste(missing_columns, collapse = ", "))
}

bau_data <- metric_data %>%
  filter(Scenario == "BaU") %>%
  select(
    Year,
    BaU_mean_consumption = mean_consumption_thousand_USD2010_per_capita,
    BaU_Atkinson_percent = Atkinson_index_percent,
    BaU_EDE_consumption = EDE_consumption_thousand_USD2010_per_capita
  )
if (nrow(bau_data) != length(target_years) || anyDuplicated(bau_data$Year)) {
  stop("A unique BaU reference record is required for each target year.")
}

# Let M be the mean-consumption ratio and D the distribution multiplier.
# WI / 100 = M * D. Exact Shapley attribution:
#   mean effect         = 100 * (M - 1) * (1 + D) / 2
#   distribution effect = 100 * (D - 1) * (1 + M) / 2
decomposition <- metric_data %>%
  filter(Scenario %in% scenario_levels) %>%
  left_join(bau_data, by = "Year") %>%
  mutate(
    mean_consumption_ratio =
      mean_consumption_thousand_USD2010_per_capita / BaU_mean_consumption,
    distribution_multiplier =
      (1 - Atkinson_index_percent / 100) /
      (1 - BaU_Atkinson_percent / 100),
    mean_consumption_effect_pp =
      100 * (mean_consumption_ratio - 1) *
      (1 + distribution_multiplier) / 2,
    distribution_effect_pp =
      100 * (distribution_multiplier - 1) *
      (1 + mean_consumption_ratio) / 2,
    net_welfare_change_pp =
      mean_consumption_effect_pp + distribution_effect_pp,
    reported_net_change_pp = Welfare_index_BaU_equals_100 - 100,
    decomposition_error = net_welfare_change_pp - reported_net_change_pp,
    Scenario = factor(Scenario, levels = scenario_levels),
    Year_plot = factor(Year, levels = target_years)
  ) %>%
  arrange(Year, Scenario)

if (nrow(decomposition) != length(scenario_levels) * length(target_years)) {
  stop("A unique Def and Aid record is required for each target year.")
}
if (any(!is.finite(decomposition$decomposition_error))) {
  stop("Non-finite values were found in the welfare decomposition.")
}
if (max(abs(decomposition$decomposition_error)) > 1e-8) {
  stop("The decomposition does not reproduce the reported welfare index.")
}

write.csv(
  decomposition %>%
    transmute(
      Year,
      Scenario = as.character(Scenario),
      mean_consumption_ratio_to_BaU = mean_consumption_ratio,
      distribution_multiplier_to_BaU = distribution_multiplier,
      mean_consumption_effect_percentage_points = mean_consumption_effect_pp,
      distribution_effect_percentage_points = distribution_effect_pp,
      net_welfare_change_percentage_points = net_welfare_change_pp,
      Welfare_index_BaU_equals_100
    ),
  csv_output,
  row.names = FALSE
)

plot_data <- decomposition %>%
  select(
    Year_plot, Scenario, mean_consumption_effect_pp,
    distribution_effect_pp
  ) %>%
  pivot_longer(
    cols = c(mean_consumption_effect_pp, distribution_effect_pp),
    names_to = "Effect", values_to = "Contribution"
  ) %>%
  mutate(
    Effect = factor(
      Effect,
      levels = c("mean_consumption_effect_pp", "distribution_effect_pp"),
      labels = c(
        "Absolute consumption-level effect",
        "Between-region distribution effect"
      )
    )
  )

plot_span <- max(abs(c(
  plot_data$Contribution,
  decomposition$net_welfare_change_pp
)), na.rm = TRUE)
label_offset <- max(0.08, 0.08 * plot_span)
net_data <- decomposition %>%
  mutate(
    label_y = net_welfare_change_pp + if_else(
      net_welfare_change_pp >= 0, label_offset, -label_offset
    ),
    net_label = number(
      net_welfare_change_pp, accuracy = 0.01, style_positive = "plus"
    )
  )

# Figure ----------------------------------------------------------------------

effect_colors <- c(
  "Absolute consumption-level effect" = "#D55E00",
  "Between-region distribution effect" = "#0072B2"
)

decomposition_plot <- ggplot(
  plot_data,
  aes(x = Scenario, y = Contribution, fill = Effect)
) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.40) +
  geom_col(width = 0.58, colour = "white", linewidth = 0.20) +
  geom_point(
    data = net_data,
    aes(x = Scenario, y = net_welfare_change_pp, shape = "Net welfare change"),
    inherit.aes = FALSE, size = 2.4, stroke = 0.65,
    fill = "white", colour = "black"
  ) +
  geom_text(
    data = net_data,
    aes(x = Scenario, y = label_y, label = net_label),
    inherit.aes = FALSE, size = 2.4, colour = "black", family = "sans"
  ) +
  facet_wrap(~Year_plot, nrow = 1) +
  scale_fill_manual(values = effect_colors, breaks = names(effect_colors), name = NULL) +
  scale_shape_manual(values = c("Net welfare change" = 23), name = NULL) +
  scale_x_discrete(drop = FALSE, expand = expansion(add = c(0.45, 0.45))) +
  scale_y_continuous(
    labels = label_number(accuracy = 0.1),
    expand = expansion(mult = c(0.16, 0.20))
  ) +
  guides(
    fill = guide_legend(order = 1, nrow = 1, override.aes = list(colour = NA)),
    shape = guide_legend(
      order = 2,
      override.aes = list(fill = "white", colour = "black", size = 2.3)
    )
  ) +
  labs(
    x = NULL,
    y = "Contribution to welfare-index change\n(relative to BaU; percentage points)"
  ) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(
    panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.25),
    panel.grid.minor = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_line(colour = "black", linewidth = 0.35),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_line(colour = "black", linewidth = 0.30),
    axis.ticks.length = grid::unit(1.2, "mm"),
    axis.text.x = element_text(size = 7.5, colour = "black", margin = margin(t = 3)),
    axis.text.y = element_text(size = 7, colour = "black"),
    axis.title.y = element_text(size = 8, margin = margin(r = 5)),
    strip.background = element_rect(fill = "#EFEFEF", colour = NA),
    strip.text = element_text(
      face = "bold", size = 8, margin = margin(2.5, 0, 2.5, 0)
    ),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.text = element_text(size = 7),
    legend.key.width = grid::unit(5.0, "mm"),
    legend.key.height = grid::unit(3.6, "mm"),
    panel.spacing.x = grid::unit(3.5, "mm"),
    plot.margin = margin(4, 4, 2, 4, unit = "mm")
  )

png_output <- paste0(output_stem, ".png")
ggsave(
  png_output, decomposition_plot, width = 180, height = 105, units = "mm",
  dpi = 600, bg = "white", limitsize = FALSE,
  device = grDevices::png, type = "cairo-png"
)
message("Saved: ", png_output)

svg_output <- paste0(output_stem, ".svg")
ggsave(
  svg_output, decomposition_plot, width = 180, height = 105, units = "mm",
  bg = "white", limitsize = FALSE,
  device = function(filename, ...) {
    grDevices::svg(filename = filename, family = "sans", onefile = TRUE, ...)
  }
)
message("Saved: ", svg_output)
message("Saved: ", csv_output)

