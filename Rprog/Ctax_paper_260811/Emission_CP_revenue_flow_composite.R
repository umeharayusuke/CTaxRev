# Emissions, carbon price and carbon-revenue flow composite -----------------
#
# Creates a new five-panel figure without modifying the two source figures:
#   a: Emission_CP_sectoral panel a (global CO2 emissions)
#   b: Emission_CP_sectoral panel b (global carbon price)
#   c: C_revFlow_and_annual_distribution panel a (cumulative flow map)
#   d: C_revFlow_and_annual_distribution panel b (annual distribution)
#   e: Emission_CP_sectoral panel c (sectoral cumulative-emission change)

find_project_root <- function(start) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(current, "CTaxRev.Rproj"))) return(current)
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
if (is.na(project_root)) stop("Could not locate the CTaxRev project root.")

library_candidates <- c(
  file.path(project_root, "Rlib"),
  Sys.getenv("R_LIBS_USER"),
  "C:/ENVI5809/R_library",
  "C:/Users/umeha/AppData/Local/R/win-library/4.5"
)
library_candidates <- library_candidates[
  nzchar(library_candidates) & dir.exists(library_candidates)
]
.libPaths(unique(c(library_candidates, .libPaths())))

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})

eval_script_before <- function(path, marker, env, working_dir = NULL) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  marker_line <- grep(marker, lines)
  if (length(marker_line) != 1L || marker_line <= 1L) {
    stop("Could not find a unique boundary in ", path, ": ", marker)
  }

  old_dir <- getwd()
  on.exit(setwd(old_dir), add = TRUE)
  if (!is.null(working_dir)) setwd(working_dir)

  eval(
    parse(text = paste(lines[seq_len(marker_line - 1L)], collapse = "\n")),
    envir = env
  )
  invisible(env)
}

script_root <- file.path(project_root, "Rprog", "Ctax_paper_260811")
data_dir <- file.path(project_root, "data", "AR6database")
output_dir <- file.path(project_root, "output", "Figure")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Build the emission/price/sector panels only. Mask source-data writing so the
# existing source figure and its companion files retain their timestamps.
emission_env <- new.env(parent = globalenv())
emission_env$write.csv <- function(...) invisible(NULL)
eval_script_before(
  file.path(script_root, "Emission_CP_sectoral.R"),
  "^# Individual outputs",
  emission_env,
  project_root
)

# Build both revenue-flow panels while suppressing the source script's two
# standalone ggsave calls. Its legacy relative paths require the data folder.
revenue_env <- new.env(parent = globalenv())
revenue_env$ggsave <- function(...) invisible(NULL)
eval_script_before(
  file.path(script_root, "Carbon-revenue flows_annual distribution.R"),
  "^# Combine the unchanged panel a",
  revenue_env,
  data_dir
)

required_emission <- c("p_global", "p_price", "p_sector_change")
missing_emission <- required_emission[
  !vapply(required_emission, exists, logical(1), envir = emission_env)
]
if (length(missing_emission)) {
  stop("Missing emission panel(s): ", paste(missing_emission, collapse = ", "))
}

required_revenue <- c("figure2b", "figure_annual", "ink")
missing_revenue <- required_revenue[
  !vapply(required_revenue, exists, logical(1), envir = revenue_env)
]
if (length(missing_revenue)) {
  stop("Missing revenue panel(s): ", paste(missing_revenue, collapse = ", "))
}

ink <- "#252525"
tag_theme <- theme(
  plot.tag = element_text(
    family = "Arial", face = "bold", size = 8.2, colour = ink
  ),
  plot.tag.position = c(0.005, 0.995),
  plot.background = element_rect(fill = "white", colour = NA)
)

# Top row: retain the original asymmetric emphasis of the emission figure.
p_a <- emission_env$p_global +
  labs(tag = "a") +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.justification = "center"
  ) +
  tag_theme

p_b <- emission_env$p_price +
  labs(tag = "b") +
  tag_theme

# Middle row: the map spans the complete figure width. Smaller legend text and
# margins retain legibility at a 180-mm journal figure width.
p_c <- revenue_env$figure2b +
  labs(tag = "c") +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.justification = "center",
    legend.title = element_text(size = 6.2),
    legend.text = element_text(size = 5.8),
    legend.key.height = grid::unit(2.8, "mm"),
    plot.margin = margin(0.5, 1.5, 1, 1.5, unit = "mm")
  ) +
  tag_theme

# The map source was designed for an 18-inch-wide standalone output and uses
# fixed annotation sizes. Rescale those layers for the 180-mm composite.
if (length(p_c$layers) < 15L) {
  stop("Unexpected revenue-map layer structure; cannot rescale annotations.")
}
p_c$layers[[7L]]$aes_params$size <- 1.55
p_c$layers[[8L]]$aes_params$size <- 1.55
p_c$layers[[9L]]$aes_params$size <- 1.75
p_c$layers[[10L]]$aes_params$size <- 1.75
p_c$layers[[11L]]$aes_params$size <- 30
p_c$layers[[12L]]$aes_params$size <- 3.15
p_c$layers[[13L]]$aes_params$size <- 2.65
p_c$layers[[14L]]$aes_params$size <- 2.35
p_c$layers[[15L]]$aes_params$size <- 2.35

# Use the model-reported undiscounted cumulative revenue for the central pool
# total. The 2050 Rprovider15th value is 67.6176 trillion US$2010, displayed as
# 68 after rounding upward as requested.
cumulative_pool_rows <- revenue_env$iamc |>
  dplyr::filter(
    VEMF == "Cum_Rev_gov_Tax_Car_Tax",
    SCENARIO == revenue_env$scenario_aid,
    REMF == "Rprovider15th",
    Year == revenue_env$end_year
  ) |>
  dplyr::pull(IAMC_Template)
if (length(cumulative_pool_rows) != 1L ||
    !is.finite(cumulative_pool_rows[[1L]]) ||
    cumulative_pool_rows[[1L]] <= 0) {
  stop("Missing the 2050 Rprovider15th cumulative carbon-tax revenue.")
}
cumulative_pool_trillion <- cumulative_pool_rows[[1L]] / 1e6
pool_total_label <- paste0(
  ceiling(cumulative_pool_trillion),
  " trillion US$2010"
)
pool_label_replaced <- FALSE
for (layer_index in seq_along(p_c$layers)) {
  layer_data <- p_c$layers[[layer_index]]$data
  if (is.data.frame(layer_data) && "label" %in% names(layer_data)) {
    replace_rows <- as.character(layer_data$label) == "61 trillion US$2010"
    if (any(replace_rows, na.rm = TRUE)) {
      p_c$layers[[layer_index]]$data$label[replace_rows] <- pool_total_label
      pool_label_replaced <- TRUE
    }
  }
  fixed_label <- p_c$layers[[layer_index]]$aes_params$label
  if (!is.null(fixed_label) &&
      any(as.character(fixed_label) == "61 trillion US$2010")) {
    p_c$layers[[layer_index]]$aes_params$label <- pool_total_label
    pool_label_replaced <- TRUE
  }
}
if (!pool_label_replaced) {
  stop("Could not locate the fixed cumulative-pool label in panel c.")
}

# Separate labels in the two dense regional clusters while leaving their map
# points and revenue-flow anchors unchanged.
apply_label_offsets <- function(data, offsets, map_width) {
  matched <- match(as.character(data$REMF), offsets$REMF)
  move <- !is.na(matched)
  data$x[move] <- data$x[move] + offsets$dx[matched[move]] * map_width
  data$y[move] <- data$y[move] + offsets$dy[matched[move]] * map_width
  data
}

provider_label_offsets <- data.frame(
  REMF = c("XE25", "XER", "TUR", "JPN"),
  dx = c(-0.030, -0.006, 0.026, 0.014),
  dy = c(0.020, -0.016, 0.006, 0.006)
)
recipient_label_offsets <- data.frame(
  REMF = c("XNF", "XME", "XSA", "IND", "XSE"),
  dx = c(-0.018, -0.008, 0.004, 0.016, 0.020),
  dy = c(0.012, -0.008, 0.016, -0.010, 0.006)
)

p_c$layers[[9L]]$data <- apply_label_offsets(
  p_c$layers[[9L]]$data,
  provider_label_offsets,
  revenue_env$map_width
)
p_c$layers[[10L]]$data <- apply_label_offsets(
  p_c$layers[[10L]]$data,
  recipient_label_offsets,
  revenue_env$map_width
)

# Bottom-left: rescale the original 18-inch annual-flow panel typography for
# its smaller slot. The data, stacks, colours and legend ordering are unchanged.
p_d <- revenue_env$figure_annual +
  labs(
    tag = "d",
    y = "Annual carbon-tax revenue flow\n(trillion US$2010 yr-1)"
  ) +
  theme_minimal(base_family = "Arial", base_size = 7.2) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#D9D9D9", linewidth = 0.25),
    axis.text = element_text(size = 6.0, colour = ink),
    axis.title = element_text(size = 6.7, colour = ink),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.box.just = "center",
    legend.justification = "center",
    legend.text = element_text(size = 5.3),
    legend.key.height = grid::unit(2.3, "mm"),
    legend.key.width = grid::unit(2.5, "mm"),
    legend.spacing.x = grid::unit(0.5, "mm"),
    legend.margin = margin(1, 0, 0, 0, unit = "mm"),
    plot.margin = margin(3, 3, 2, 3, unit = "mm")
  ) +
  tag_theme

# The two in-panel group annotations were sized for an 18-inch standalone
# figure. Scale them down for the composite without altering their content.
for (layer_index in seq_along(p_d$layers)) {
  if (inherits(p_d$layers[[layer_index]]$geom, "GeomText")) {
    p_d$layers[[layer_index]]$aes_params$size <- 2.25
  }
}

p_e <- emission_env$p_sector_change +
  labs(tag = "e") +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center"
  ) +
  tag_theme

top_row <- patchwork::wrap_plots(
  p_a, p_b,
  nrow = 1,
  widths = c(1.8, 1),
  guides = "keep"
)

bottom_row <- patchwork::wrap_plots(
  p_d, p_e,
  nrow = 1,
  widths = c(1.35, 1),
  guides = "keep"
)

figure_composite <- patchwork::wrap_plots(
  top_row,
  p_c,
  bottom_row,
  ncol = 1,
  heights = c(1.00, 0.84, 1.15),
  guides = "keep"
) &
  theme(plot.background = element_rect(fill = "white", colour = NA))

png_file <- file.path(
  output_dir, "Emission_CP_revenue_flow_composite.png"
)
pdf_file <- file.path(
  output_dir, "Emission_CP_revenue_flow_composite.pdf"
)
svg_file <- file.path(
  output_dir, "Emission_CP_revenue_flow_composite.svg"
)

ggplot2::ggsave(
  filename = png_file,
  plot = figure_composite,
  device = grDevices::png,
  type = "cairo-png",
  width = 195,
  height = 232,
  units = "mm",
  dpi = 400,
  bg = "white",
  limitsize = FALSE
)
ggplot2::ggsave(
  filename = pdf_file,
  plot = figure_composite,
  device = grDevices::cairo_pdf,
  width = 195,
  height = 232,
  units = "mm",
  bg = "white",
  limitsize = FALSE
)
ggplot2::ggsave(
  filename = svg_file,
  plot = figure_composite,
  device = function(filename, ...) {
    grDevices::svg(filename = filename, family = "Arial", onefile = TRUE, ...)
  },
  width = 195,
  height = 232,
  units = "mm",
  bg = "white",
  limitsize = FALSE
)

message("Saved integrated five-panel figure: ", png_file)
