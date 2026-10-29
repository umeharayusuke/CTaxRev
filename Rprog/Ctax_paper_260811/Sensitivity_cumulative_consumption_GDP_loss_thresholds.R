# Sensitivity of cumulative consumption and GDP losses to aid thresholds -----
#
# Panels show the model's directly reported 2050 cumulative loss rates using a
# 5% discount rate. Columns are the 10k, 15k, and 20k USD GDP-per-capita
# thresholds; the x axis shows World, the matching Rprovider* aggregate, and
# the matching Rrecipient* aggregate. Every threshold panel includes both the
# no-aid Default scenario and its corresponding Aid scenario.

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
if (is.na(project_root)) {
  stop("Could not locate data/AR6database/global_17_IAMC.gdx.")
}

local_library <- file.path(project_root, "Rlib")
if (dir.exists(local_library)) .libPaths(c(local_library, .libPaths()))
shared_libraries <- c(Sys.getenv("R_LIBS_USER"), "C:/ENVI5809/R_library")
shared_libraries <- shared_libraries[
  nzchar(shared_libraries) & dir.exists(shared_libraries)
]
.libPaths(unique(c(shared_libraries, .libPaths())))

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(patchwork)
})

if (!requireNamespace("gdxrrw", quietly = TRUE)) {
  stop("The gdxrrw package is required to read IAMC_template.")
}

gams_candidates <- c(
  Sys.getenv("GAMSDIR", unset = ""),
  "C:/GAMS/34",
  "C:/GAMS/win64/26.1"
)
gams_candidates <- unique(gams_candidates[nzchar(gams_candidates)])
gams_dir <- gams_candidates[dir.exists(gams_candidates)][1]
if (is.na(gams_dir)) stop("Could not locate a GAMS installation.")
gdxrrw::igdx(gams_dir)

metric_codes <- c(
  Consumption = "Pol_Cos_Cns_Los_rat_NPV_5pc",
  GDP = "Pol_Cos_GDP_Los_rat_NPV_5pc"
)
ssp_levels <- paste0("SSP", 1:3)
threshold_levels <- c("10th", "15th", "20th")
policy_levels <- c("Default", "Aid")
region_levels <- c("World", "Developed", "Developing")

plot_grid <- crossing(
  SSP = ssp_levels,
  Threshold = threshold_levels,
  Policy = policy_levels,
  Metric = names(metric_codes),
  Region_group = region_levels
) %>%
  mutate(
    SCENARIO = if_else(
      Policy == "Default",
      paste0(SSP, "_400C_2030CP_NoCC_No"),
      paste0(SSP, "_400C_2030CP_", Threshold, "_NoCC_No")
    ),
    REMF = case_when(
      Region_group == "World" ~ "World",
      Region_group == "Developed" ~ paste0("Rprovider", Threshold),
      Region_group == "Developing" ~ paste0("Rrecipient", Threshold)
    ),
    VEMF = unname(metric_codes[Metric]),
    Year = 2050
  )

gdx_file <- file.path(
  project_root, "data", "AR6database", "global_17_IAMC.gdx"
)
message("Reading IAMC_template from: ", gdx_file)
raw <- gdxrrw::rgdx.param(gdx_file, "IAMC_template")

required_columns <- c("SCENARIO", "REMF", "VEMF", "YEMF", "IAMC_Template")
if (!all(required_columns %in% names(raw))) {
  stop("IAMC_template does not have the expected columns.")
}

iamc <- raw %>%
  transmute(
    SCENARIO = as.character(SCENARIO),
    REMF = as.character(REMF),
    VEMF = as.character(VEMF),
    Year = as.numeric(as.character(YEMF)),
    Loss_pct = as.numeric(IAMC_Template)
  ) %>%
  filter(
    SCENARIO %in% unique(plot_grid$SCENARIO),
    REMF %in% unique(plot_grid$REMF),
    VEMF %in% metric_codes,
    Year == 2050
  )

plot_data <- plot_grid %>%
  left_join(iamc, by = c("SCENARIO", "REMF", "VEMF", "Year"))

if (nrow(plot_data) != nrow(plot_grid)) {
  stop("Joining IAMC_template changed the expected plotting-grid size.")
}
if (anyDuplicated(plot_data[c(
  "SSP", "Threshold", "Policy", "Metric", "Region_group"
)])) {
  stop("Duplicate cumulative-loss plotting observations were found.")
}
if (any(!is.finite(plot_data$Loss_pct))) {
  stop("At least one required cumulative-loss value is missing or non-finite.")
}

# SSP3 10th and 15th Aid runs are invalid for the full horizon. Their values
# are retained as explicit NA rows so the intended empty positions are stable.
plot_data <- plot_data %>%
  mutate(
    Loss_pct = if_else(
      SSP == "SSP3" & Threshold %in% c("10th", "15th") & Policy == "Aid",
      NA_real_,
      Loss_pct
    ),
    SSP = factor(SSP, levels = ssp_levels),
    Threshold = factor(Threshold, levels = threshold_levels),
    Policy = factor(Policy, levels = policy_levels),
    Metric = factor(Metric, levels = c("Consumption", "GDP")),
    Region_group = factor(Region_group, levels = region_levels),
    Region_index = as.numeric(Region_group),
    SSP_offset = case_when(
      SSP == "SSP1" ~ -0.18,
      SSP == "SSP2" ~ 0,
      SSP == "SSP3" ~ 0.18
    ),
    X_position = Region_index + SSP_offset
  )

excluded <- plot_data %>%
  filter(SSP == "SSP3", Threshold %in% c("10th", "15th"), Policy == "Aid")
if (nrow(excluded) != 12L || any(!is.na(excluded$Loss_pct))) {
  stop("The SSP3 10th/15th Aid exclusion was not applied correctly.")
}

default_rows <- plot_data %>% filter(Policy == "Default")
if (nrow(default_rows) != 54L || any(!is.finite(default_rows$Loss_pct))) {
  stop("Default values are not complete across all threshold panels.")
}

ssp_colours <- c(
  "SSP1" = "#0072B2",
  "SSP2" = "#D55E00",
  "SSP3" = "#009E73"
)
policy_shapes <- c("Default" = 1, "Aid" = 16)
threshold_labels <- c(
  "10th" = "10k USD threshold",
  "15th" = "15k USD threshold",
  "20th" = "20k USD threshold"
)
ink <- "#252525"

theme_sensitivity <- function() {
  theme_classic(base_family = "Arial", base_size = 8) +
    theme(
      text = element_text(colour = ink),
      axis.line = element_blank(),
      axis.ticks = element_line(linewidth = 0.25, colour = ink),
      axis.ticks.length = grid::unit(1.2, "mm"),
      axis.title = element_text(size = 8.2),
      axis.text = element_text(size = 7.4, colour = ink),
      panel.border = element_rect(fill = NA, colour = ink, linewidth = 0.30),
      panel.grid.major.y = element_line(colour = "#E2E2E2", linewidth = 0.22),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = 8.2),
      panel.spacing.x = grid::unit(3, "mm"),
      legend.position = "bottom",
      legend.justification = "center",
      legend.box = "horizontal",
      legend.box.just = "center",
      legend.spacing.x = grid::unit(3, "mm"),
      legend.title = element_text(size = 7.4),
      legend.text = element_text(size = 7.4),
      legend.key.width = grid::unit(6, "mm"),
      plot.margin = margin(3, 6, 3, 4)
    )
}

make_loss_plot <- function(data, y_title, show_legend) {
  ggplot(
    data,
    aes(
      x = X_position,
      y = Loss_pct,
      colour = SSP,
      shape = Policy
    )
  ) +
    geom_hline(yintercept = 0, linewidth = 0.35, colour = ink) +
    geom_line(
      aes(group = interaction(SSP, Region_group)),
      linewidth = 0.48,
      linetype = "22",
      na.rm = TRUE
    ) +
    geom_point(
      size = 2.35,
      stroke = 0.70,
      na.rm = TRUE
    ) +
    facet_wrap(
      ~Threshold,
      nrow = 1,
      labeller = as_labeller(threshold_labels)
    ) +
    scale_colour_manual(
      values = ssp_colours,
      breaks = ssp_levels,
      name = "SSP"
    ) +
    scale_shape_manual(
      values = policy_shapes,
      breaks = policy_levels,
      labels = c("Default" = "Default (no aid)", "Aid" = "Aid"),
      name = "Policy"
    ) +
    scale_x_continuous(
      breaks = seq_along(region_levels),
      labels = region_levels,
      limits = c(0.58, 3.42),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(
      name = y_title,
      labels = label_number(accuracy = 0.1),
      expand = expansion(mult = c(0.10, 0.12))
    ) +
    labs(x = NULL) +
    guides(
      colour = guide_legend(order = 1, nrow = 1),
      shape = guide_legend(order = 2, nrow = 1)
    ) +
    theme_sensitivity() +
    theme(legend.position = if (show_legend) "bottom" else "none")
}

p_consumption <- make_loss_plot(
  plot_data %>% filter(Metric == "Consumption"),
  "Cumulative consumption loss in 2050 (%)",
  TRUE
)

p_gdp <- make_loss_plot(
  plot_data %>% filter(Metric == "GDP"),
  "Cumulative GDP loss in 2050 (%)",
  FALSE
)

p <- (
  p_consumption / p_gdp / guide_area() +
    plot_layout(guides = "collect", heights = c(1, 1, 0.13)) +
    plot_annotation(tag_levels = "a")
) &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.box.just = "center",
    legend.spacing.x = grid::unit(3, "mm"),
    legend.margin = margin(t = 3, b = 2, unit = "mm"),
    plot.tag = element_text(
      family = "Arial", face = "bold", size = 9.0, colour = ink
    ),
    plot.tag.position = "topleft"
  )

output_dir <- file.path(project_root, "output", "Figure", "analysis")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_file <- file.path(
  output_dir, "07_sensitivity_cumulative_consumption_GDP_loss_thresholds.png"
)

ggsave(
  filename = output_file,
  plot = p,
  device = "png",
  type = "cairo",
  width = 180,
  height = 142,
  units = "mm",
  dpi = 400,
  bg = "white"
)

message("Created: ", output_file)
message("Cumulative-loss plotting data (NA values are intentionally blank):")
print(
  plot_data %>%
    select(SSP, Threshold, Policy, Metric, Region_group, Loss_pct) %>%
    arrange(Metric, Threshold, Region_group, SSP, Policy),
  row.names = FALSE,
  digits = 5
)
