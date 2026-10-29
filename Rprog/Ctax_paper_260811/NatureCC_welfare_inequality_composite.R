# Nature Climate Change-style composite figure:
# between-region inequality, mean consumption, welfare, and decomposition
#
# Input:
#   output/Figure/csv/
#     Between_region_inequality_welfare_BaU_Def_Aid_2025_2050.csv
# Outputs:
#   output/Figure/NatureCC_welfare_inequality_composite.png
#   output/Figure/NatureCC_welfare_inequality_composite.pdf
#   output/Figure/NatureCC_welfare_inequality_composite.svg
#   output/Figure/csv/NatureCC_welfare_inequality_composite_data.csv

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
  library(patchwork)
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
input_file <- file.path(
  csv_dir,
  "Between_region_inequality_welfare_BaU_Def_Aid_2025_2050.csv"
)
output_stem <- file.path(output_dir, "NatureCC_welfare_inequality_composite")
figure_data_file <- file.path(
  csv_dir, "NatureCC_welfare_inequality_composite_data.csv"
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_file)) {
  stop(
    "Missing input: ", input_file, "\n",
    "Run Between_region_inequality_welfare_BaU_Def_Aid_2025_2050.R first."
  )
}

# Data and exact decomposition ------------------------------------------------

scenario_levels <- c("BaU", "Def", "Aid")
scenario_colors <- c(BaU = "#737373", Def = "#D55E00", Aid = "#0072B2")
scenario_shapes <- c(BaU = 16, Def = 17, Aid = 15)
target_years <- c(2025, 2050)

metrics <- read.csv(input_file, check.names = FALSE) %>%
  as_tibble() %>%
  filter(Year %in% target_years, Scenario %in% scenario_levels) %>%
  mutate(
    Scenario = factor(Scenario, levels = scenario_levels),
    Year = as.numeric(Year)
  ) %>%
  arrange(Year, Scenario)

required_columns <- c(
  "Scenario", "Year",
  "mean_consumption_thousand_USD2010_per_capita",
  "Atkinson_index_percent", "Welfare_index_BaU_equals_100"
)
missing_columns <- setdiff(required_columns, names(metrics))
if (length(missing_columns) > 0) {
  stop("Input CSV is missing column(s): ", paste(missing_columns, collapse = ", "))
}
if (nrow(metrics) != length(scenario_levels) * length(target_years) ||
    anyDuplicated(metrics[c("Scenario", "Year")])) {
  stop("A unique BaU, Def, and Aid record is required for 2025 and 2050.")
}

bau_2050 <- metrics %>%
  filter(Year == 2050, Scenario == "BaU") %>%
  transmute(
    BaU_mean = mean_consumption_thousand_USD2010_per_capita,
    BaU_atkinson = Atkinson_index_percent
  )
if (nrow(bau_2050) != 1) stop("A unique 2050 BaU record is required.")

# WI / 100 = M * D, where M is the mean-consumption ratio and D is the
# distribution multiplier. The Shapley terms sum exactly to WI - 100.
decomposition <- metrics %>%
  filter(Year == 2050, Scenario %in% c("Def", "Aid")) %>%
  mutate(
    mean_ratio =
      mean_consumption_thousand_USD2010_per_capita / bau_2050$BaU_mean,
    distribution_multiplier =
      (1 - Atkinson_index_percent / 100) /
      (1 - bau_2050$BaU_atkinson / 100),
    Mean_consumption =
      100 * (mean_ratio - 1) * (1 + distribution_multiplier) / 2,
    Between_region_distribution =
      100 * (distribution_multiplier - 1) * (1 + mean_ratio) / 2,
    Net = Mean_consumption + Between_region_distribution,
    Reported_net = Welfare_index_BaU_equals_100 - 100
  )

decomposition_error <- max(abs(decomposition$Net - decomposition$Reported_net))
if (!is.finite(decomposition_error) || decomposition_error > 1e-8) {
  stop("The exact decomposition does not reproduce the 2050 welfare index.")
}

decomposition_long <- decomposition %>%
  select(Scenario, Mean_consumption, Between_region_distribution) %>%
  pivot_longer(
    cols = c(Mean_consumption, Between_region_distribution),
    names_to = "Effect", values_to = "Contribution"
  ) %>%
  mutate(
    Scenario = factor(as.character(Scenario), levels = c("Aid", "Def")),
    Effect = factor(
      Effect,
      levels = c("Mean_consumption", "Between_region_distribution"),
      labels = c("Mean consumption", "Between-region distribution")
    )
  )

net_data <- decomposition %>%
  transmute(
    Scenario = factor(as.character(Scenario), levels = c("Aid", "Def")),
    Net,
    Net_label = paste0("Net ", number(Net, accuracy = 0.01))
  )

# Export all plotted values in a tidy source-data table.
metric_source <- metrics %>%
  select(
    Year, Scenario,
    Atkinson_index_percent,
    mean_consumption_thousand_USD2010_per_capita,
    Welfare_index_BaU_equals_100
  ) %>%
  pivot_longer(
    cols = c(
      Atkinson_index_percent,
      mean_consumption_thousand_USD2010_per_capita,
      Welfare_index_BaU_equals_100
    ),
    names_to = "Measure", values_to = "Value"
  ) %>%
  mutate(
    Panel = recode(
      Measure,
      Atkinson_index_percent = "a",
      mean_consumption_thousand_USD2010_per_capita = "b",
      Welfare_index_BaU_equals_100 = "c"
    ),
    Component = NA_character_
  ) %>%
  select(Panel, Year, Scenario, Measure, Component, Value)

decomposition_source <- bind_rows(
  decomposition_long %>%
    transmute(
      Panel = "d", Year = 2050, Scenario,
      Measure = "WI_change_from_BaU_percentage_points",
      Component = as.character(Effect), Value = Contribution
    ),
  net_data %>%
    transmute(
      Panel = "d", Year = 2050, Scenario,
      Measure = "WI_change_from_BaU_percentage_points",
      Component = "Net", Value = Net
    )
)

write.csv(
  bind_rows(metric_source, decomposition_source) %>%
    mutate(Scenario = as.character(Scenario)),
  figure_data_file,
  row.names = FALSE
)

# Figure style ----------------------------------------------------------------

# Designed at final double-column size. Nature guidance recommends a 180 mm
# width, a maximum height of 170 mm, sans-serif lettering, and 5--7 pt text.
theme_nature <- function() {
  theme_classic(base_size = 7, base_family = "sans") +
    theme(
      text = element_text(colour = "#222222"),
      plot.title = element_text(
        face = "bold", size = 7.5, hjust = 0, margin = margin(b = 3)
      ),
      plot.tag = element_text(face = "bold", size = 8.5, colour = "black"),
      plot.tag.position = c(0, 1),
      axis.title = element_text(size = 7),
      axis.text = element_text(size = 6.5, colour = "#222222"),
      axis.line = element_line(colour = "#333333", linewidth = 0.30),
      axis.ticks = element_line(colour = "#333333", linewidth = 0.25),
      axis.ticks.length = grid::unit(1.2, "mm"),
      panel.grid.major.y = element_line(colour = "#E7E7E7", linewidth = 0.25),
      panel.grid.minor = element_blank(),
      legend.title = element_blank(),
      legend.text = element_text(size = 6.5),
      legend.key.height = grid::unit(3.0, "mm"),
      legend.key.width = grid::unit(4.5, "mm"),
      legend.spacing.x = grid::unit(1.0, "mm"),
      plot.margin = margin(3, 4, 3, 3, unit = "mm")
    )
}

make_trajectory_panel <- function(
    data, value_column, title, y_label, y_scale, tag, reference = NULL,
    show_legend = FALSE) {
  panel <- ggplot(
    data,
    aes(
      x = Year, y = .data[[value_column]], colour = Scenario,
      shape = Scenario, group = Scenario
    )
  )

  if (!is.null(reference)) {
    panel <- panel + geom_hline(
      yintercept = reference, linetype = "22", colour = "#8A8A8A",
      linewidth = 0.30
    )
  }

  panel +
    geom_line(linewidth = 0.48, show.legend = FALSE) +
    geom_point(size = 2.15, stroke = 0.55) +
    scale_colour_manual(values = scenario_colors, drop = FALSE) +
    scale_shape_manual(values = scenario_shapes, drop = FALSE) +
    scale_x_continuous(
      breaks = target_years, limits = c(2022, 2053),
      expand = expansion(mult = c(0, 0))
    ) +
    y_scale +
    labs(tag = tag, title = title, x = NULL, y = y_label) +
    guides(
      colour = guide_legend(order = 1, nrow = 1),
      shape = guide_legend(order = 1, nrow = 1)
    ) +
    theme_nature() +
    theme(
      legend.position = if (show_legend) "top" else "none",
      legend.justification = "left",
      legend.margin = margin(0, 0, 2, 0),
      plot.tag.position = c(-0.04, 1.05)
    )
}

p_atkinson <- make_trajectory_panel(
  metrics,
  "Atkinson_index_percent",
  "Between-region inequality",
  "Atkinson index (%)",
  scale_y_continuous(
    breaks = seq(55, 65, 2.5),
    labels = label_number(accuracy = 0.1),
    expand = expansion(mult = c(0.08, 0.10))
  ),
  "a",
  show_legend = TRUE
)

p_mean <- make_trajectory_panel(
  metrics,
  "mean_consumption_thousand_USD2010_per_capita",
  "Mean consumption level",
  expression(atop(
    "Mean consumption",
    paste("(10"^3, " US$2010 person"^{-1}, " yr"^{-1}, ")")
  )),
  scale_y_continuous(
    breaks = seq(8, 13, 1),
    labels = label_number(accuracy = 0.1),
    expand = expansion(mult = c(0.08, 0.10))
  ),
  "b"
)

p_welfare <- make_trajectory_panel(
  metrics,
  "Welfare_index_BaU_equals_100",
  "Inequality-adjusted welfare",
  "Welfare index (BaU = 100)",
  scale_y_continuous(
    breaks = seq(85, 100, 5),
    labels = label_number(accuracy = 1),
    expand = expansion(mult = c(0.08, 0.10))
  ),
  "c",
  reference = 100
)

effect_colors <- c(
  "Mean consumption" = "#BDBDBD",
  "Between-region distribution" = "#756BB1"
)

p_decomposition <- ggplot(
  decomposition_long,
  aes(y = Scenario, x = Contribution, fill = Effect)
) +
  geom_vline(xintercept = 0, colour = "#222222", linewidth = 0.38) +
  geom_col(width = 0.56, colour = "white", linewidth = 0.20) +
  geom_point(
    data = net_data,
    aes(y = Scenario, x = Net, shape = "Net welfare change"),
    inherit.aes = FALSE,
    size = 2.55, stroke = 0.70, fill = "white", colour = "black"
  ) +
  geom_text(
    data = net_data,
    aes(y = Scenario, x = Net - 0.45, label = Net_label),
    inherit.aes = FALSE,
    hjust = 1, size = 2.25, family = "sans", colour = "#222222"
  ) +
  scale_fill_manual(values = effect_colors, breaks = names(effect_colors)) +
  scale_shape_manual(values = c("Net welfare change" = 23)) +
  scale_x_continuous(
    breaks = seq(-15, 5, 5),
    labels = label_number(accuracy = 1),
    limits = c(-18, 7),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_discrete(drop = FALSE) +
  guides(
    fill = guide_legend(
      order = 1, nrow = 1, override.aes = list(colour = NA)
    ),
    shape = guide_legend(
      order = 2,
      override.aes = list(fill = "white", colour = "black", size = 2.4)
    )
  ) +
  labs(
    tag = "d",
    title = "Drivers of the 2050 welfare change",
    x = "Contribution to WI change relative to BaU (percentage points)",
    y = NULL
  ) +
  theme_nature() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "#E7E7E7", linewidth = 0.25),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(face = "bold", size = 7),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.margin = margin(2, 0, 0, 0),
    plot.tag.position = c(-0.01, 1.04),
    plot.margin = margin(4, 4, 1, 3, unit = "mm")
  )

# Three comparable indicator panels above one full-width decomposition panel.
# The scenario key is collected once above the complete upper row.
top_row <- (
  (p_atkinson | p_mean | p_welfare) + plot_layout(guides = "collect")
) &
  theme(
    legend.position = "top",
    legend.justification = "center",
    legend.margin = margin(0, 0, 2, 0)
  )

figure <- top_row / p_decomposition +
  plot_layout(heights = c(1.08, 0.92)) &
  theme(plot.background = element_rect(fill = "white", colour = NA))

# Export at final double-column size (180 x 145 mm).
png_output <- paste0(output_stem, ".png")
ggsave(
  png_output, figure, width = 180, height = 145, units = "mm",
  dpi = 300, bg = "white", limitsize = FALSE,
  device = grDevices::png, type = "cairo-png"
)
message("Saved: ", png_output)

pdf_output <- paste0(output_stem, ".pdf")
ggsave(
  pdf_output, figure, width = 180, height = 145, units = "mm",
  bg = "white", limitsize = FALSE, device = grDevices::cairo_pdf
)
message("Saved: ", pdf_output)

svg_output <- paste0(output_stem, ".svg")
ggsave(
  svg_output, figure, width = 180, height = 145, units = "mm",
  bg = "white", limitsize = FALSE,
  device = function(filename, ...) {
    grDevices::svg(filename = filename, family = "sans", onefile = TRUE, ...)
  }
)
message("Saved: ", svg_output)
message("Saved: ", figure_data_file)
message(
  "Maximum exact-decomposition error: ",
  format(decomposition_error, scientific = TRUE)
)
