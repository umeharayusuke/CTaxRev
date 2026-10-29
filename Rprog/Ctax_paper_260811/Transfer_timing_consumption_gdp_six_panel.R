# Six-panel transfer, consumption and GDP figure -----------------------------
#
# This script creates a separate six-panel extension of
# 03_transfer_timing_consumption_gdp.png. The existing figure and its source
# file are not modified. Panels a-c reuse the original definitions, panel d is
# the original panel e (2050 GDP decomposition), panel e is the all-year
# transfer-recovery scatter using BaU GDP, and panel f is the cumulative
# transfer zero-loss comparison. All panels use the same Nature-family theme.

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
  library(ggplot2)
  library(scales)
  library(patchwork)
})

output_dir <- file.path(project_root, "output", "Figure")
csv_dir <- file.path(output_dir, "csv")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Rebuild the original plot objects in an isolated environment. File-writing
# functions are masked so sourcing the original script cannot overwrite or
# delete any existing figure. Only the in-memory plot objects are reused.
base_script <- file.path(
  project_root, "Rprog", "Ctax_paper_260811",
  "Transfer_emissions_efficiency_timing.R"
)
if (!file.exists(base_script)) stop("Missing base figure script: ", base_script)

base_env <- new.env(parent = globalenv())
base_env$ggsave <- function(...) invisible(NULL)
base_env$unlink <- function(...) invisible(0L)
sys.source(base_script, envir = base_env)

required_objects <- c(
  "p_timing", "p_cumulative_loss", "p_cumulative_gdp",
  "p_gdp_decomposition", "theme_nature", "ink",
  "colour_developed", "colour_developing"
)
missing_objects <- required_objects[
  !vapply(required_objects, exists, logical(1), envir = base_env)
]
if (length(missing_objects)) {
  stop("Missing plot object(s) from base script: ",
       paste(missing_objects, collapse = ", "))
}

theme_nature <- base_env$theme_nature
ink <- base_env$ink
colour_developed <- base_env$colour_developed
colour_developing <- base_env$colour_developing

# Panel e: annual transfer relative to BaU GDP and consumption recovery -------
recovery_csv <- file.path(
  csv_dir, "transfer_recovery_scatter_5year_source_data.csv"
)
if (!file.exists(recovery_csv)) {
  stop(
    "Missing transfer-recovery source data: ", recovery_csv,
    ". Run transfer-benefit.R first."
  )
}

plot_years <- seq(2030, 2050, by = 5)
recovery_data <- read.csv(recovery_csv, stringsAsFactors = FALSE) %>%
  filter(
    GDPBasis == "BaU",
    Year %in% plot_years,
    is.finite(NetTransferPct),
    is.finite(ConsumptionChangePct)
  ) %>%
  mutate(
    Year = factor(Year, levels = plot_years),
    Type = factor(Type, levels = c("Provider", "Recipient"))
  )

expected_recovery_rows <- length(plot_years) * 17L
if (nrow(recovery_data) != expected_recovery_rows ||
    anyDuplicated(recovery_data[c("Year", "REMF")])) {
  stop("The BaU-GDP transfer-recovery data are incomplete or duplicated.")
}

regression_lines <- recovery_data %>%
  group_by(Year) %>%
  group_modify(~ {
    model <- lm(ConsumptionChangePct ~ NetTransferPct, data = .x)
    tibble::tibble(
      Intercept = unname(coef(model)[1]),
      Slope = unname(coef(model)[2])
    )
  }) %>%
  ungroup() %>%
  filter(as.character(Year) != "2030")

axis_limits <- c(-50, 50)
axis_breaks <- seq(-50, 50, by = 10)
year_colours <- c(
  "2030" = "#4E79A7",
  "2035" = "#59A14F",
  "2040" = "#F28E2B",
  "2045" = "#E15759",
  "2050" = "#B07AA1"
)

p_recovery <- ggplot(
  recovery_data,
  aes(NetTransferPct, ConsumptionChangePct)
) +
  geom_hline(yintercept = 0, colour = ink, linewidth = 0.35) +
  geom_vline(xintercept = 0, colour = ink, linewidth = 0.35) +
  geom_abline(
    intercept = 0,
    slope = 1,
    colour = ink,
    linetype = "dotted",
    linewidth = 0.45
  ) +
  geom_abline(
    data = regression_lines,
    aes(intercept = Intercept, slope = Slope, colour = Year),
    inherit.aes = FALSE,
    linewidth = 0.60,
    alpha = 0.58,
    show.legend = FALSE
  ) +
  geom_point(
    aes(colour = Year, shape = Type),
    size = 1.80,
    alpha = 0.84,
    stroke = 0.55
  ) +
  scale_colour_manual(
    values = year_colours,
    limits = as.character(plot_years),
    drop = FALSE,
    name = "Year"
  ) +
  scale_shape_manual(
    values = c(Provider = 16, Recipient = 17),
    name = NULL
  ) +
  scale_x_continuous(
    breaks = axis_breaks,
    labels = label_number(accuracy = 1, suffix = "%"),
    expand = expansion(mult = 0)
  ) +
  scale_y_continuous(
    breaks = axis_breaks,
    labels = label_number(accuracy = 1, suffix = "%"),
    expand = expansion(mult = 0)
  ) +
  coord_fixed(
    ratio = 1,
    xlim = axis_limits,
    ylim = axis_limits,
    expand = FALSE
  ) +
  labs(
    x = "Annual transfer / BaU GDP (%)",
    y = "Consumption change under Aid relative to Def (%)"
  ) +
  guides(
    colour = guide_legend(
      order = 1,
      nrow = 1,
      override.aes = list(
        size = 1.8, alpha = 0.84, shape = 16,
        linetype = 0, linewidth = 0
      )
    ),
    shape = guide_legend(order = 2, nrow = 1)
  ) +
  theme_nature() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(fill = NA, colour = ink, linewidth = 0.35),
    axis.line = element_blank(),
    legend.position = "bottom",
    legend.justification = "center",
    legend.box.just = "center",
    legend.box = "vertical",
    legend.direction = "horizontal",
    legend.title = element_text(size = 6.5),
    legend.text = element_text(size = 6.3),
    legend.key.width = grid::unit(4.0, "mm"),
    legend.key.height = grid::unit(2.5, "mm")
  )

# Panel f: cumulative transfer and zero-loss funding -------------------------
zero_loss_csv <- file.path(
  csv_dir, "Cumulative_transfer_zero_loss_2050_source_data.csv"
)
benchmark_csv <- file.path(
  csv_dir, "Cumulative_transfer_zero_loss_2050_benchmarks.csv"
)
if (!file.exists(zero_loss_csv) || !file.exists(benchmark_csv)) {
  stop(
    "Missing cumulative zero-loss source data. Run ",
    "Cumulative_transfer_zero_loss_2050.R first."
  )
}

zero_loss_data <- read.csv(zero_loss_csv, stringsAsFactors = FALSE) %>%
  mutate(
    Group = factor(Group, levels = c("Provider", "Recipient")),
    Scenario = factor(Scenario, levels = c("Def", "Aid"))
  )
benchmarks <- read.csv(benchmark_csv, stringsAsFactors = FALSE)

if (nrow(zero_loss_data) != 4L ||
    anyDuplicated(zero_loss_data[c("Group", "Scenario")])) {
  stop("The cumulative zero-loss point data are incomplete or duplicated.")
}

benchmark_value <- function(metric) {
  value <- benchmarks$Value[benchmarks$Metric == metric]
  if (length(value) != 1L || !is.finite(value)) {
    stop("Missing benchmark value: ", metric)
  }
  value
}

actual_aid <- benchmark_value("Actual cumulative Aid transfer")
cge_zero <- benchmark_value("CGE-implied recipient zero-loss funding")
def_benchmark <- benchmark_value("Def-loss accounting benchmark")
funding_gap <- benchmark_value("Accounting benchmark minus CGE")
period_years <- 20

x_max <- max(actual_aid, cge_zero, def_benchmark) * 1.12
y_values <- zero_loss_data$CumulativeConsumptionLossRate_pct
y_span <- diff(range(c(y_values, 0)))
if (!is.finite(y_span) || y_span == 0) y_span <- 1
y_min <- min(y_values, 0) - 0.13 * y_span
y_max <- max(y_values, 0) + 0.18 * y_span
x_breaks <- pretty(c(0, x_max), n = 6)
x_breaks <- x_breaks[x_breaks >= 0 & x_breaks <= x_max]

benchmark_points <- tibble::tibble(
  Benchmark = factor(
    c("CGE-implied", "Def-loss benchmark"),
    levels = c("CGE-implied", "Def-loss benchmark")
  ),
  X = c(cge_zero, def_benchmark),
  Y = 0,
  Label = number(c(cge_zero, def_benchmark), accuracy = 0.1),
  LabelY = c(0.075 * y_span, -0.10 * y_span)
)

p_zero_loss <- ggplot(
  zero_loss_data,
  aes(
    x = CumulativeAid_trillion_USD2010,
    y = CumulativeConsumptionLossRate_pct,
    colour = Group,
    group = Group
  )
) +
  geom_hline(yintercept = 0, colour = ink, linewidth = 0.40) +
  geom_vline(xintercept = 0, colour = ink, linewidth = 0.40) +
  geom_line(linetype = "dotted", linewidth = 0.75) +
  geom_point(
    aes(shape = Scenario, fill = interaction(Group, Scenario)),
    size = 2.75,
    stroke = 0.75
  ) +
  geom_segment(
    data = tibble::tibble(
      x = min(cge_zero, def_benchmark),
      xend = max(cge_zero, def_benchmark),
      y = 0,
      yend = 0
    ),
    aes(x = x, xend = xend, y = y, yend = yend),
    inherit.aes = FALSE,
    colour = "#999999",
    linewidth = 2.6,
    alpha = 0.35,
    lineend = "round"
  ) +
  annotate(
    "label",
    x = mean(c(cge_zero, def_benchmark)),
    y = -0.035 * y_span,
    label = number(funding_gap, accuracy = 0.1),
    size = 2.15,
    linewidth = 0,
    fill = alpha("white", 0.92),
    colour = "#666666"
  ) +
  geom_point(
    data = benchmark_points,
    aes(x = X, y = Y, shape = Benchmark),
    inherit.aes = FALSE,
    size = 2.75,
    stroke = 0.75,
    colour = ink,
    fill = "white"
  ) +
  geom_label(
    data = benchmark_points,
    aes(x = X, y = LabelY, label = Label),
    inherit.aes = FALSE,
    size = 2.25,
    lineheight = 0.95,
    linewidth = 0,
    fill = alpha("white", 0.92),
    colour = ink
  ) +
  scale_colour_manual(
    values = c(
      Provider = colour_developed,
      Recipient = colour_developing
    ),
    labels = c(Provider = "Developed", Recipient = "Developing"),
    name = NULL
  ) +
  scale_fill_manual(
    values = c(
      "Provider.Def" = "white",
      "Provider.Aid" = colour_developed,
      "Recipient.Def" = "white",
      "Recipient.Aid" = colour_developing
    ),
    guide = "none"
  ) +
  scale_shape_manual(
    values = c(
      Def = 1,
      Aid = 16,
      `CGE-implied` = 23,
      `Def-loss benchmark` = 24
    ),
    breaks = c("Def", "Aid", "CGE-implied", "Def-loss benchmark"),
    labels = c(
      "Def", "Aid", "CGE zero loss", "Def-loss benchmark"
    ),
    name = NULL
  ) +
  scale_x_continuous(
    breaks = x_breaks,
    labels = label_number(accuracy = 1),
    limits = c(0, x_max),
    expand = expansion(mult = c(0, 0)),
    sec.axis = sec_axis(
      ~ . / period_years,
      breaks = x_breaks / period_years,
      labels = label_number(accuracy = 0.1),
      name = "Average annual amount, 2030-2050\n(trillion US$2010/yr)"
    )
  ) +
  scale_y_continuous(
    labels = label_number(accuracy = 1, suffix = "%"),
    limits = c(y_min, y_max),
    expand = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Cumulative transfer, 2030-2050 (trillion US$2010)",
    y = "Cumulative consumption loss rate in 2050"
  ) +
  guides(
    colour = guide_legend(
      order = 1,
      override.aes = list(linetype = 0, size = 2.1)
    ),
    shape = guide_legend(order = 2, override.aes = list(colour = ink))
  ) +
  theme_nature() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(fill = NA, colour = ink, linewidth = 0.35),
    axis.line = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.82, 0.76),
    legend.justification = c(0.5, 0.5),
    legend.direction = "vertical",
    legend.background = element_rect(fill = "white", colour = NA),
    legend.text = element_text(size = 6.4),
    legend.key.height = grid::unit(2.8, "mm"),
    legend.key.width = grid::unit(4.0, "mm"),
    axis.title.x.top = element_text(size = 7.5)
  )

# Six-panel layout ------------------------------------------------------------
p_six_panel <- (
  wrap_plots(
    A = base_env$p_timing,
    B = base_env$p_cumulative_loss,
    C = base_env$p_cumulative_gdp,
    D = base_env$p_gdp_decomposition,
    E = p_recovery,
    F = p_zero_loss,
    design = "AAAABBCC\nDDDDEEEE\nFFFFFFFF",
    heights = c(0.86, 1.05, 0.92),
    guides = "keep"
  ) +
    plot_annotation(tag_levels = "a")
) &
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    plot.tag = element_text(
      family = "Arial",
      face = "bold",
      size = 8.5,
      colour = ink
    ),
    plot.tag.position = "topleft"
  )

output_file <- file.path(
  output_dir,
  "03_transfer_timing_consumption_gdp_recovery_zero_loss.png"
)
svg_file <- file.path(
  output_dir,
  "03_transfer_timing_consumption_gdp_recovery_zero_loss.svg"
)

ggplot2::ggsave(
  filename = output_file,
  plot = p_six_panel,
  device = "png",
  type = "cairo",
  width = 180,
  height = 282,
  units = "mm",
  dpi = 400,
  bg = "white",
  limitsize = FALSE
)
if (requireNamespace("svglite", quietly = TRUE)) {
  ggplot2::ggsave(
    filename = svg_file,
    plot = p_six_panel,
    device = svglite::svglite,
    width = 180 / 25.4,
    height = 282 / 25.4,
    bg = "white",
    limitsize = FALSE
  )
}

message("Saved six-panel figure: ", output_file)
