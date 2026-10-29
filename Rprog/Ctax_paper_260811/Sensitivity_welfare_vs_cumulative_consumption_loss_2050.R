# Welfare versus global cumulative consumption loss in 2050 -----------------
#
# The x axis is the global detailed-expenditure welfare index (each SSP's BaU
# equals 100). The y axis is the model's directly reported World cumulative
# consumption-loss rate in 2050 (5% discount rate). Only mitigation policies
# are plotted: no-aid Default and the 10k/15k/20k aid thresholds. SSP3 10k and
# 15k are excluded. Lines span the policies within each SSP and make the
# near-horizontal welfare gains from aid visible.

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
  library(ggplot2)
  library(scales)
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
  project_root, "data", "AR6database", "global_17_IAMC.gdx"
)
analysis_dir <- file.path(project_root, "output", "Figure", "analysis")
wi_file <- file.path(
  analysis_dir,
  "09_sensitivity_inequality_welfare_2050_SSP1_SSP3_data.csv"
)
output_stem <- file.path(
  analysis_dir,
  "10_welfare_vs_cumulative_consumption_loss_2050"
)
dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(wi_file)) {
  stop(
    "Missing welfare-index data: ", wi_file, "\n",
    "Run Sensitivity_inequality_welfare_2050_SSP1_SSP3.R first."
  )
}

# Settings ------------------------------------------------------------------

comparison_year <- 2050
loss_variable <- "Pol_Cos_Cns_Los_rat_NPV_5pc"
ssp_levels <- paste0("SSP", 1:3)
policy_levels <- c("Default", "10th", "15th", "20th")
policy_labels <- c(
  "Default" = "Default (no aid)",
  "10th" = "10k USD",
  "15th" = "15k USD",
  "20th" = "20k USD"
)
ssp_colours <- c(
  "SSP1" = "#0072B2",
  "SSP2" = "#D55E00",
  "SSP3" = "#009E73"
)
policy_shapes <- c("Default" = 1, "10th" = 16, "15th" = 17, "20th" = 15)
ink <- "#252525"

scenario_grid <- tidyr::crossing(
  SSP = ssp_levels,
  Policy = policy_levels
) %>%
  mutate(
    Scenario_id = if_else(
      Policy == "Default",
      paste0(SSP, "_400C_2030CP_NoCC_No"),
      paste0(SSP, "_400C_2030CP_", Policy, "_NoCC_No")
    )
  ) %>%
  filter(!(SSP == "SSP3" & Policy %in% c("10th", "15th")))

# Welfare index --------------------------------------------------------------

wi_data <- read.csv(wi_file, check.names = FALSE) %>%
  as_tibble() %>%
  filter(Year == comparison_year, Policy %in% policy_levels) %>%
  transmute(
    SSP = as.character(SSP),
    Policy = as.character(Policy),
    Scenario_id = as.character(Scenario_id),
    Welfare_index = as.numeric(Welfare_index)
  )

if (nrow(wi_data) != nrow(scenario_grid) ||
    anyDuplicated(wi_data[c("SSP", "Policy")]) ||
    any(!is.finite(wi_data$Welfare_index))) {
  stop("The 2050 welfare-index input is incomplete or invalid.")
}
if (nrow(anti_join(scenario_grid, wi_data, by = c("SSP", "Policy", "Scenario_id")))) {
  stop("The welfare-index scenarios do not match the expected scenario grid.")
}

# Global cumulative consumption loss ----------------------------------------

message("Reading IAMC_template from: ", gdx_file)
raw <- rgdx.param(gdx_file, "IAMC_template")
required_columns <- c("SCENARIO", "REMF", "VEMF", "YEMF", "IAMC_Template")
if (!all(required_columns %in% names(raw))) {
  stop("IAMC_template does not have the expected columns.")
}

loss_data <- raw %>%
  transmute(
    Scenario_id = as.character(SCENARIO),
    Region = as.character(REMF),
    Variable = as.character(VEMF),
    Year = as.numeric(as.character(YEMF)),
    Cumulative_consumption_loss_pct = as.numeric(IAMC_Template)
  ) %>%
  filter(
    Scenario_id %in% scenario_grid$Scenario_id,
    Region == "World",
    Variable == loss_variable,
    Year == comparison_year
  )

if (nrow(loss_data) != nrow(scenario_grid) ||
    anyDuplicated(loss_data$Scenario_id) ||
    any(!is.finite(loss_data$Cumulative_consumption_loss_pct))) {
  stop("The World cumulative-consumption-loss input is incomplete or invalid.")
}

plot_data <- scenario_grid %>%
  left_join(
    wi_data %>% select(SSP, Policy, Scenario_id, Welfare_index),
    by = c("SSP", "Policy", "Scenario_id")
  ) %>%
  left_join(
    loss_data %>% select(Scenario_id, Cumulative_consumption_loss_pct),
    by = "Scenario_id"
  ) %>%
  mutate(
    SSP = factor(SSP, levels = ssp_levels),
    Policy = factor(Policy, levels = policy_levels)
  )

if (nrow(plot_data) != nrow(scenario_grid) ||
    any(!is.finite(plot_data$Welfare_index)) ||
    any(!is.finite(plot_data$Cumulative_consumption_loss_pct))) {
  stop("The combined plotting data are incomplete or invalid.")
}

# Quantify how little global loss changes when aid is introduced relative to
# the no-aid Default case within the same SSP.
loss_diagnostics <- plot_data %>%
  group_by(SSP) %>%
  mutate(
    Default_loss = Cumulative_consumption_loss_pct[Policy == "Default"],
    Loss_difference_from_default =
      Cumulative_consumption_loss_pct - Default_loss
  ) %>%
  ungroup()
maximum_loss_difference <- max(
  abs(loss_diagnostics$Loss_difference_from_default), na.rm = TRUE
)

write.csv(
  loss_diagnostics %>%
    mutate(
      SSP = as.character(SSP),
      Policy = as.character(Policy)
    ),
  paste0(output_stem, "_data.csv"),
  row.names = FALSE
)

# Figure ---------------------------------------------------------------------

theme_nature <- function() {
  theme_classic(base_family = "Arial", base_size = 8) +
    theme(
      text = element_text(colour = ink),
      axis.title = element_text(size = 8.4),
      axis.text = element_text(size = 7.5, colour = ink),
      axis.ticks = element_line(colour = ink, linewidth = 0.25),
      axis.ticks.length = grid::unit(1.2, "mm"),
      axis.line = element_blank(),
      panel.border = element_rect(fill = NA, colour = ink, linewidth = 0.32),
      panel.grid.major = element_line(colour = "#E2E2E2", linewidth = 0.22),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      legend.position = "bottom",
      legend.justification = "center",
      legend.box = "vertical",
      legend.title = element_text(size = 7.3, face = "plain"),
      legend.text = element_text(size = 7.1),
      legend.key.height = grid::unit(3.5, "mm"),
      legend.key.width = grid::unit(6.0, "mm"),
      legend.spacing.x = grid::unit(1.2, "mm"),
      plot.margin = margin(4, 5, 3, 4, unit = "mm")
    )
}

p <- ggplot(
  plot_data,
  aes(
    x = Welfare_index,
    y = Cumulative_consumption_loss_pct,
    colour = SSP,
    shape = Policy
  )
) +
  geom_vline(
    xintercept = 100,
    linetype = "22",
    colour = "#8A8A8A",
    linewidth = 0.30
  ) +
  geom_line(
    aes(group = SSP),
    linewidth = 0.64,
    alpha = 0.86
  ) +
  geom_point(size = 2.75, stroke = 0.72) +
  scale_colour_manual(
    values = ssp_colours,
    breaks = ssp_levels,
    drop = FALSE,
    name = "SSP"
  ) +
  scale_shape_manual(
    values = policy_shapes,
    breaks = policy_levels,
    labels = policy_labels,
    drop = FALSE,
    name = "Policy"
  ) +
  scale_x_continuous(
    labels = label_number(accuracy = 1),
    expand = expansion(mult = c(0.06, 0.08))
  ) +
  scale_y_continuous(
    labels = label_number(accuracy = 0.1),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    x = "Welfare index in 2050 (BaU = 100)",
    y = "Global cumulative consumption loss in 2050 (%)"
  ) +
  guides(
    colour = guide_legend(order = 1, nrow = 1),
    shape = guide_legend(order = 2, nrow = 1)
  ) +
  theme_nature()

ggsave(
  filename = paste0(output_stem, ".png"),
  plot = p,
  device = "png",
  type = "cairo",
  width = 150,
  height = 112,
  units = "mm",
  dpi = 400,
  bg = "white",
  limitsize = FALSE
)

message("Created: ", paste0(output_stem, ".png"))
message("Created: ", paste0(output_stem, "_data.csv"))
message(
  "Maximum absolute loss difference from Default within an SSP (percentage points): ",
  sprintf("%.8f", maximum_loss_difference)
)
print(
  loss_diagnostics %>%
    select(
      SSP, Policy, Welfare_index, Cumulative_consumption_loss_pct,
      Loss_difference_from_default
    ) %>%
    arrange(SSP, Policy),
  n = Inf
)
