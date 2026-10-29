# Four-panel transfer, recovery, zero-loss and GDP figure -------------------
#
# This script creates a separate rearranged version of
# 03_transfer_timing_consumption_gdp_recovery_zero_loss.png. The original
# six-panel figure and its script are not modified. The new panel order is:
#   a = original a (transfer timing)
#   b = original e (annual transfer and consumption recovery)
#   c = original f (cumulative transfer and zero-loss funding)
#   d = original d (2050 GDP decomposition)

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
  library(ggplot2)
  library(patchwork)
})

# Evaluate only the data and panel-building section of the six-panel script.
# Stopping before its layout block prevents the original output from being
# regenerated or overwritten while retaining exactly the same panel objects.
source_script <- file.path(
  project_root, "Rprog", "Ctax_paper_260811",
  "Transfer_timing_consumption_gdp_six_panel.R"
)
if (!file.exists(source_script)) {
  stop("Missing six-panel source script: ", source_script)
}

source_lines <- readLines(source_script, warn = FALSE, encoding = "UTF-8")
layout_marker <- grep(
  "^# Six-panel layout",
  source_lines
)
if (length(layout_marker) != 1L || layout_marker <= 1L) {
  stop("Could not identify the six-panel layout boundary in: ", source_script)
}

panel_env <- new.env(parent = globalenv())
eval(
  parse(text = paste(source_lines[seq_len(layout_marker - 1L)], collapse = "\n")),
  envir = panel_env
)

required_objects <- c("base_env", "p_recovery", "p_zero_loss", "ink")
missing_objects <- required_objects[
  !vapply(required_objects, exists, logical(1), envir = panel_env)
]
if (length(missing_objects)) {
  stop(
    "Missing panel object(s): ",
    paste(missing_objects, collapse = ", ")
  )
}

base_env <- panel_env$base_env
ink <- panel_env$ink

required_base_objects <- c("p_timing", "p_gdp_decomposition")
missing_base_objects <- required_base_objects[
  !vapply(required_base_objects, exists, logical(1), envir = base_env)
]
if (length(missing_base_objects)) {
  stop(
    "Missing base panel object(s): ",
    paste(missing_base_objects, collapse = ", ")
  )
}

# Add the model-reported World aggregate to panel a. The original panel uses
# the same annual consumption-loss-rate variable for the developed and
# developing aggregates, so the World series is directly comparable.
timing_world_data <- base_env$iamc %>%
  dplyr::filter(
    VEMF == "Pol_Cos_Cns_Los_rat",
    SCENARIO %in% base_env$scenarios_needed,
    REMF == "World",
    Year %in% base_env$model_years
  ) %>%
  dplyr::mutate(
    Region = factor(
      "World",
      levels = c("World", "Developed regions", "Developing regions")
    ),
    Policy = factor(
      ifelse(
        SCENARIO == base_env$scenario_def,
        "No transfer (Def)",
        "Climate finance (Aid)"
      ),
      levels = c("No transfer (Def)", "Climate finance (Aid)")
    )
  ) %>%
  dplyr::rename(Consumption_loss_pct = Value)

if (nrow(timing_world_data) != length(base_env$model_years) * 2L ||
    anyDuplicated(timing_world_data[c("SCENARIO", "Year")])) {
  stop("The World timing data are incomplete or duplicated.")
}

timing_data_with_world <- dplyr::bind_rows(
  base_env$loss_data %>%
    dplyr::mutate(
      Region = factor(
        as.character(Region),
        levels = c("World", "Developed regions", "Developing regions")
      )
    ),
  timing_world_data
)

p_timing_world <- base_env$p_timing
p_timing_world$data <- timing_data_with_world
p_timing_world <- p_timing_world +
  scale_colour_manual(
    values = c(
      "World" = ink,
      "Developed regions" = base_env$colour_developed,
      "Developing regions" = base_env$colour_developing
    ),
    breaks = c("World", "Developed regions", "Developing regions"),
    labels = c(
      "World" = "World",
      "Developed regions" = "Developed",
      "Developing regions" = "Developing"
    ),
    guide = guide_legend(order = 1, title = NULL, nrow = 1)
  )

# Give the zero-loss panel more horizontal space and place its guides below
# the plotting area, consistent with the other panels in the figure. Rebuild
# the secondary-axis labels from the cumulative-axis tick positions so the
# upper and lower ticks remain exactly aligned.
zero_loss_x_breaks <- panel_env$x_breaks
period_years <- panel_env$period_years

# Remove only the grey segment and its funding-gap label (14.5) between the
# two zero-loss benchmarks. The benchmark points and their own values remain.
p_zero_loss_clean <- panel_env$p_zero_loss
if (length(p_zero_loss_clean$layers) < 8L) {
  stop("Unexpected zero-loss panel structure; cannot remove gap annotation.")
}
p_zero_loss_clean$layers <- p_zero_loss_clean$layers[-c(5L, 6L)]

# Add the World aggregate to panel c at the same Def and Aid transfer amounts.
# World is intentionally omitted from the line layer, leaving its two scenario
# values as unconnected points while the developed/developing lines remain.
world_zero_loss_data <- base_env$iamc %>%
  dplyr::filter(
    VEMF == "Pol_Cos_Cns_Los_rat_NPV_5pc",
    SCENARIO %in% base_env$scenarios_needed,
    REMF == "World",
    Year == 2050
  ) %>%
  dplyr::transmute(
    Group = factor(
      "World",
      levels = c("World", "Provider", "Recipient")
    ),
    Scenario = factor(
      ifelse(SCENARIO == base_env$scenario_def, "Def", "Aid"),
      levels = c("Def", "Aid")
    ),
    Region = REMF,
    ScenarioCode = SCENARIO,
    CumulativeAid_trillion_USD2010 = ifelse(
      Scenario == "Def", 0, panel_env$actual_aid
    ),
    AverageAnnualAid_trillion_USD2010_per_year =
      CumulativeAid_trillion_USD2010 / period_years,
    CumulativeConsumptionLossRate_pct = Value
  )

if (nrow(world_zero_loss_data) != 2L ||
    anyDuplicated(world_zero_loss_data["Scenario"])) {
  stop("The World cumulative-loss data are incomplete or duplicated.")
}

zero_loss_data_with_world <- dplyr::bind_rows(
  panel_env$zero_loss_data %>%
    dplyr::mutate(
      Group = factor(
        as.character(Group),
        levels = c("World", "Provider", "Recipient")
      )
    ),
  world_zero_loss_data
)

p_zero_loss_clean$data <- zero_loss_data_with_world
p_zero_loss_clean$layers[[3L]]$data <- zero_loss_data_with_world %>%
  dplyr::filter(Group != "World")

zero_loss_y_values <- zero_loss_data_with_world$CumulativeConsumptionLossRate_pct
zero_loss_y_span <- diff(range(c(zero_loss_y_values, 0)))
if (!is.finite(zero_loss_y_span) || zero_loss_y_span == 0) zero_loss_y_span <- 1
zero_loss_y_min <- min(zero_loss_y_values, 0) - 0.13 * zero_loss_y_span
zero_loss_y_max <- max(zero_loss_y_values, 0) + 0.18 * zero_loss_y_span

p_zero_loss_bottom <- p_zero_loss_clean +
  scale_colour_manual(
    values = c(
      World = ink,
      Provider = base_env$colour_developed,
      Recipient = base_env$colour_developing
    ),
    breaks = c("World", "Provider", "Recipient"),
    labels = c(
      World = "World", Provider = "Developed", Recipient = "Developing"
    ),
    name = NULL
  ) +
  scale_fill_manual(
    values = c(
      "World.Def" = "white",
      "World.Aid" = ink,
      "Provider.Def" = "white",
      "Provider.Aid" = base_env$colour_developed,
      "Recipient.Def" = "white",
      "Recipient.Aid" = base_env$colour_developing
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = zero_loss_x_breaks,
    labels = scales::label_number(accuracy = 1),
    limits = c(0, panel_env$x_max),
    expand = expansion(mult = c(0, 0)),
    sec.axis = dup_axis(
      breaks = zero_loss_x_breaks,
      labels = scales::label_number(
        accuracy = 0.1,
        scale = 1 / period_years
      ),
      name = "Average annual amount, 2030-2050\n(trillion US$2010/yr)"
    )
  ) +
  scale_y_continuous(
    labels = scales::label_number(accuracy = 1, suffix = "%"),
    limits = c(zero_loss_y_min, zero_loss_y_max),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    y = "Cumulative consumption loss rate in 2050 (%)"
  ) +
  guides(
    colour = guide_legend(
      order = 1,
      nrow = 1,
      override.aes = list(linetype = 0, size = 2.1)
    ),
    shape = guide_legend(
      order = 2,
      nrow = 1,
      override.aes = list(colour = ink)
    )
  ) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom",
    legend.justification = "center",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.box.just = "center",
    legend.background = element_blank(),
    legend.margin = margin(t = 1, r = 0, b = 0, l = 0),
    legend.spacing.y = grid::unit(0.4, "mm"),
    legend.key.height = grid::unit(2.8, "mm"),
    legend.key.width = grid::unit(4.0, "mm")
  )

# Panel b already states the unit in both axis titles, so repeating a percent
# sign at every tick is unnecessary.
p_recovery_plain_axes <- panel_env$p_recovery +
  scale_x_continuous(
    breaks = panel_env$axis_breaks,
    labels = scales::label_number(accuracy = 1),
    expand = expansion(mult = 0)
  ) +
  scale_y_continuous(
    breaks = panel_env$axis_breaks,
    labels = scales::label_number(accuracy = 1),
    expand = expansion(mult = 0)
  ) +
  scale_shape_manual(
    values = c(Provider = 16, Recipient = 17),
    breaks = c("Provider", "Recipient"),
    labels = c(Provider = "Developed", Recipient = "Developing"),
    name = NULL
  ) +
  labs(
    x = "Annual transfer / GDP (%)",
    y = "Consumption change under\nAid relative to Def (%)"
  ) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.tag.position = c(0.14, 0.99)
  )

# The narrower panel d needs its shape guide below the two-row fill guide so
# the Net GDP key and label remain fully inside the figure.
p_gdp_decomposition_compact <- base_env$p_gdp_decomposition +
  scale_y_continuous(
    name = "Contribution to GDP change\nrelative to BaU in 2050 (%)",
    labels = scales::label_number(accuracy = 0.1),
    expand = expansion(mult = c(0.14, 0.17))
  ) +
  scale_shape_manual(
    values = c("Net GDP change" = 23),
    labels = c("Net GDP change" = "Net GDP change"),
    name = NULL
  ) +
  guides(
    fill = guide_legend(order = 1, nrow = 2, byrow = TRUE),
    shape = guide_legend(order = 2, nrow = 1)
  ) +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.box.just = "center",
    legend.box = "vertical",
    legend.text = element_text(size = 6.4),
    legend.key.width = grid::unit(4.5, "mm"),
    legend.key.height = grid::unit(2.8, "mm"),
    legend.margin = margin(t = 1, r = 0, b = 0, l = 0),
    strip.text = element_text(face = "plain", size = 7.0)
  )

# Requested physical arrangement:
# original a | original e
# original f (wider) | original d
p_four_panel <- (
  patchwork::wrap_plots(
    A = p_timing_world,
    B = p_recovery_plain_axes,
    C = patchwork::free(p_zero_loss_bottom, side = "t"),
    D = p_gdp_decomposition_compact,
    design = "AAAAABBBBB\nCCCCCCDDDD",
    heights = c(1, 1),
    guides = "keep"
  ) +
    patchwork::plot_annotation(tag_levels = "a")
) &
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    plot.tag = element_text(
      family = "Arial",
      face = "bold",
      size = 8.5,
      colour = ink
    )
  )

output_dir <- file.path(project_root, "output", "Figure")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

output_file <- file.path(
  output_dir,
  "03_transfer_timing_recovery_zero_loss_gdp_four_panel.png"
)
svg_file <- file.path(
  output_dir,
  "03_transfer_timing_recovery_zero_loss_gdp_four_panel.svg"
)

ggplot2::ggsave(
  filename = output_file,
  plot = p_four_panel,
  device = "png",
  type = "cairo",
  width = 180,
  height = 174,
  units = "mm",
  dpi = 400,
  bg = "white",
  limitsize = FALSE
)
if (requireNamespace("svglite", quietly = TRUE)) {
  ggplot2::ggsave(
    filename = svg_file,
    plot = p_four_panel,
    device = svglite::svglite,
    width = 180 / 25.4,
    height = 174 / 25.4,
    bg = "white",
    limitsize = FALSE
  )
}

message("Saved rearranged four-panel figure: ", output_file)
