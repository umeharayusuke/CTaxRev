# Between-region inequality and welfare for BaU, Def, and Aid
#
# The upper panel reports the population-weighted Atkinson index. The lower
# panel reports equally distributed equivalent (EDE) consumption indexed to
# BaU = 100 within each year. Both panels use eta = 1.5.
#
# Input:
#   data/AR6database/global_17_IAMC.gdx (IAMC_template)
# Outputs:
#   output/Figure/Between_region_inequality_welfare_BaU_Def_Aid_2025_2050.png
#   output/Figure/Between_region_inequality_welfare_BaU_Def_Aid_2025_2050.svg
#   output/Figure/csv/Between_region_inequality_welfare_BaU_Def_Aid_2025_2050.csv

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
  library(gdxrrw)
})

# Paths -----------------------------------------------------------------------

find_project_dir <- function(start_dir) {
  candidate <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)
  repeat {
    input_candidate <- file.path(
      candidate, "data", "AR6database", "global_17_IAMC.gdx"
    )
    if (file.exists(input_candidate)) return(candidate)
    parent <- dirname(candidate)
    if (identical(parent, candidate)) {
      stop("Could not find the CTaxRev project root.")
    }
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
gdx_file <- file.path(
  project_dir, "data", "AR6database", "global_17_IAMC.gdx"
)
output_dir <- file.path(project_dir, "output", "Figure")
csv_dir <- file.path(output_dir, "csv")
output_name <- "Between_region_inequality_welfare_BaU_Def_Aid_2025_2050"
output_stem <- file.path(output_dir, output_name)
csv_output <- file.path(csv_dir, paste0(output_name, ".csv"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)

gams_candidates <- unique(c(
  Sys.getenv("GAMS_SYSDIR", unset = ""),
  "C:/GAMS/34",
  "C:/GAMS/win64/26.1"
))
gams_candidates <- gams_candidates[nzchar(gams_candidates)]
gams_dir <- gams_candidates[dir.exists(gams_candidates)][1]
if (is.na(gams_dir)) {
  stop("GAMS system directory was not found; gdxrrw cannot read the GDX.")
}
igdx(gams_dir)

# Analysis settings -----------------------------------------------------------

scenario_key <- c(
  "SSP2_BaU_NoCC_No" = "BaU",
  "SSP2_400C_2030CP_NoCC_No" = "Def",
  "SSP2_400C_2030CP_15th_NoCC_No" = "Aid"
)
scenario_ids <- names(scenario_key)
scenario_levels <- unname(scenario_key)
aim17 <- c(
  "CHN", "XE25", "IND", "JPN", "TUR", "XSE", "CIS", "USA", "XLM",
  "BRA", "CAN", "XOC", "XER", "XSA", "XME", "XNF", "XAF"
)
target_years <- c(2025, 2050)
eta <- 1.5
consumption_variable <- "CNS"
population_variable <- "Pop"
scenario_colors <- c(BaU = "#777777", Def = "#D55E00", Aid = "#0072B2")
scenario_shapes <- c(BaU = 16, Def = 17, Aid = 15)

# Population-weighted equally distributed equivalent consumption under CRRA.
ede_consumption <- function(cons_pc, population, inequality_aversion) {
  ok <- is.finite(cons_pc) & cons_pc > 0 &
    is.finite(population) & population > 0
  cons_pc <- cons_pc[ok]
  population <- population[ok]
  population_share <- population / sum(population)

  if (abs(inequality_aversion - 1) < 1e-12) {
    exp(sum(population_share * log(cons_pc)))
  } else {
    sum(
      population_share * cons_pc^(1 - inequality_aversion)
    )^(1 / (1 - inequality_aversion))
  }
}

# Read and validate -----------------------------------------------------------

message("Reading IAMC_template from ", gdx_file)
raw <- rgdx.param(gdx_file, "IAMC_template")
year_column <- intersect(c("YEMF", "Y", "Year"), names(raw))[1]
if (is.na(year_column)) stop("Cannot identify the year column in IAMC_template.")

regional_long <- raw %>%
  transmute(
    Scenario_id = as.character(SCENARIO),
    Region = as.character(REMF),
    variable = as.character(VEMF),
    Year = as.numeric(as.character(.data[[year_column]])),
    value = as.numeric(IAMC_Template)
  ) %>%
  filter(
    Scenario_id %in% scenario_ids,
    Region %in% aim17,
    variable %in% c(consumption_variable, population_variable),
    Year %in% target_years
  )
rm(raw)

duplicate_rows <- regional_long %>%
  count(Scenario_id, Region, Year, variable) %>%
  filter(n != 1)
if (nrow(duplicate_rows) > 0) {
  stop("Duplicate CNS/Pop records found for target combinations.")
}

expected_grid <- expand_grid(
  Scenario_id = scenario_ids,
  Region = aim17,
  Year = target_years,
  variable = c(consumption_variable, population_variable)
)
missing_rows <- expected_grid %>%
  anti_join(regional_long, by = c("Scenario_id", "Region", "Year", "variable"))
if (nrow(missing_rows) > 0) {
  stop("Missing CNS/Pop records for ", nrow(missing_rows), " target combinations.")
}

regional_data <- regional_long %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  rename(
    consumption = all_of(consumption_variable),
    population = all_of(population_variable)
  ) %>%
  mutate(cons_pc = consumption / population)

if (any(!is.finite(regional_data$consumption) | regional_data$consumption <= 0)) {
  stop("CNS contains missing, non-finite, or non-positive values.")
}
if (any(!is.finite(regional_data$population) | regional_data$population <= 0)) {
  stop("Pop contains missing, non-finite, or non-positive values.")
}
if (any(!is.finite(regional_data$cons_pc) | regional_data$cons_pc <= 0)) {
  stop("Per-capita consumption contains invalid values.")
}

# Indicators -----------------------------------------------------------------

scenario_metrics <- regional_data %>%
  group_by(Scenario_id, Year) %>%
  summarise(
    population_million = sum(population),
    mean_consumption_pc = sum(consumption) / sum(population),
    ede_consumption_pc = ede_consumption(cons_pc, population, eta),
    .groups = "drop"
  ) %>%
  mutate(
    Atkinson_percent = 100 * (1 - ede_consumption_pc / mean_consumption_pc),
    Scenario = unname(scenario_key[Scenario_id])
  )

bau_welfare <- scenario_metrics %>%
  filter(Scenario == "BaU") %>%
  select(Year, BaU_ede_consumption_pc = ede_consumption_pc)

scenario_metrics <- scenario_metrics %>%
  left_join(bau_welfare, by = "Year") %>%
  mutate(
    Welfare_index_BaU = 100 * ede_consumption_pc / BaU_ede_consumption_pc,
    CE_change_from_BaU_percent = Welfare_index_BaU - 100,
    Scenario = factor(Scenario, levels = scenario_levels),
    Year_plot = factor(Year, levels = target_years)
  ) %>%
  arrange(Year, Scenario)

if (any(!is.finite(scenario_metrics$Atkinson_percent) |
        !is.finite(scenario_metrics$Welfare_index_BaU))) {
  stop("Calculated inequality/welfare metrics contain invalid values.")
}

write.csv(
  scenario_metrics %>%
    transmute(
      Scenario_id,
      Scenario = as.character(Scenario),
      Year,
      eta,
      population_million,
      mean_consumption_thousand_USD2010_per_capita = mean_consumption_pc,
      EDE_consumption_thousand_USD2010_per_capita = ede_consumption_pc,
      Atkinson_index_percent = Atkinson_percent,
      Welfare_index_BaU_equals_100 = Welfare_index_BaU,
      CE_change_from_BaU_percent
    ),
  csv_output,
  row.names = FALSE
)

# Nature Climate Change-style figure -----------------------------------------

make_panel <- function(data, value_column, y_label, y_labels, reference = NULL) {
  panel_plot <- ggplot(
    data,
    aes(x = Scenario, y = .data[[value_column]], group = 1)
  )

  if (!is.null(reference)) {
    panel_plot <- panel_plot +
      geom_hline(
        yintercept = reference, colour = "grey55", linewidth = 0.35,
        linetype = "dashed"
      )
  }

  panel_plot +
    geom_line(colour = "grey65", linewidth = 0.50) +
    geom_point(
      aes(colour = Scenario, shape = Scenario), size = 2.3, stroke = 0.6
    ) +
    facet_wrap(~Year_plot, nrow = 1) +
    scale_colour_manual(values = scenario_colors, drop = FALSE, name = NULL) +
    scale_shape_manual(values = scenario_shapes, drop = FALSE, name = NULL) +
    scale_y_continuous(
      labels = y_labels,
      expand = expansion(mult = c(0.10, 0.12))
    ) +
    guides(
      colour = guide_legend(order = 1, nrow = 1),
      shape = guide_legend(order = 1, nrow = 1)
    ) +
    labs(x = NULL, y = y_label) +
    theme_classic(base_size = 8, base_family = "sans") +
    theme(
      panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.28),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black", linewidth = 0.35),
      axis.ticks = element_line(colour = "black", linewidth = 0.35),
      axis.ticks.length = grid::unit(1.4, "mm"),
      axis.text = element_text(colour = "black", size = 7),
      axis.title.y = element_text(size = 8, margin = margin(r = 4)),
      strip.background = element_rect(fill = "grey94", colour = NA),
      strip.text = element_text(
        face = "bold", size = 7.5, margin = margin(2.5, 0, 2.5, 0)
      ),
      panel.spacing.x = grid::unit(3.0, "mm"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.text = element_text(size = 7),
      legend.key.width = grid::unit(4.3, "mm"),
      legend.key.height = grid::unit(3.4, "mm"),
      plot.margin = margin(2, 2, 2, 2, unit = "mm")
    )
}

inequality_plot <- make_panel(
  scenario_metrics,
  "Atkinson_percent",
  "Atkinson index (%)",
  label_number(accuracy = 0.1, suffix = "%")
)

welfare_plot <- make_panel(
  scenario_metrics,
  "Welfare_index_BaU",
  "Welfare index (BaU = 100)",
  label_number(accuracy = 0.1),
  reference = 100
)

combined_plot <- inequality_plot / welfare_plot +
  plot_layout(guides = "collect", heights = c(1, 1)) +
  plot_annotation(
    tag_levels = "a", tag_prefix = "(", tag_suffix = ")",
    theme = theme(
      plot.tag = element_text(family = "sans", face = "bold", size = 8.5),
      plot.tag.position = c(0, 1)
    )
  ) &
  theme(legend.position = "bottom")

png_output <- paste0(output_stem, ".png")
ggsave(
  png_output, combined_plot, width = 180, height = 135, units = "mm",
  dpi = 600, bg = "white", limitsize = FALSE,
  device = grDevices::png, type = "cairo-png"
)
message("Saved: ", png_output)

svg_output <- paste0(output_stem, ".svg")
ggsave(
  svg_output, combined_plot, width = 180, height = 135, units = "mm",
  bg = "white", limitsize = FALSE,
  device = function(filename, ...) {
    grDevices::svg(filename = filename, family = "sans", onefile = TRUE, ...)
  }
)
message("Saved: ", svg_output)
message("Saved: ", csv_output)

