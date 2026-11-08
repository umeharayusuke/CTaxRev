required_packages <- c(
  "dplyr",
  "tidyr",
  "ggplot2",
  "scales",
  "patchwork",
  "readr",
  "readxl",
  "tidyverse",
  "sf",
  "rnaturalearth",
  "rnaturalearthdata",
  "ggnewscale",
  "svglite",
  "gdxrrw"
)

theme_ncc <- function(
    base_size = 8,
    base_family = "Arial",
    legend_position = "bottom",
    margin_mm = c(top = 2, right = 3, bottom = 2, left = 2),
    panel_spacing_mm = 3) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("theme_ncc() requires the ggplot2 package.")
  }
  if (length(margin_mm) != 4L) {
    stop("margin_mm must contain top, right, bottom, and left values.")
  }

  ggplot2::theme_classic(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      text = ggplot2::element_text(
        family = base_family, face = "plain", colour = "black"
      ),
      axis.title = ggplot2::element_text(
        size = base_size, face = "plain", colour = "black"
      ),
      axis.text = ggplot2::element_text(
        size = base_size * 0.875, face = "plain", colour = "black"
      ),
      axis.line = ggplot2::element_line(linewidth = 0.35, colour = "black"),
      axis.ticks = ggplot2::element_line(linewidth = 0.35, colour = "black"),
      axis.ticks.length = grid::unit(1.5, "mm"),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.grid = ggplot2::element_blank(),
      panel.spacing = grid::unit(panel_spacing_mm, "mm"),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        size = base_size, face = "bold", colour = "black"
      ),
      legend.position = legend_position,
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(
        size = base_size * 0.8125, face = "plain", colour = "black"
      ),
      legend.key.height = grid::unit(3.2, "mm"),
      legend.key.width = grid::unit(5.2, "mm"),
      legend.spacing.x = grid::unit(1.2, "mm"),
      plot.title = ggplot2::element_text(
        size = base_size * 1.15, face = "bold", colour = "black"
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size, face = "plain", colour = "black"
      ),
      plot.caption = ggplot2::element_text(
        size = base_size * 0.75, face = "plain", colour = "black"
      ),
      plot.tag = ggplot2::element_text(
        size = base_size * 1.125, face = "bold", colour = "black"
      ),
      plot.tag.position = c(0, 1),
      plot.margin = ggplot2::margin(
        margin_mm[[1]], margin_mm[[2]], margin_mm[[3]], margin_mm[[4]],
        unit = "mm"
      )
    )
}

apply_ncc_theme <- function(plot, ...) {
  ncc_theme <- theme_ncc(...)
  if (inherits(plot, "patchwork")) {
    plot & ncc_theme
  } else {
    plot + ncc_theme
  }
}

check_required_packages <- function() {
  missing_packages <- required_packages[
    !vapply(
      required_packages,
      function(package) length(find.package(package, quiet = TRUE)) > 0L,
      logical(1)
    )
  ]
  if (length(missing_packages)) {
    stop(
      "Missing required R packages: ",
      paste(missing_packages, collapse = ", "),
      ". Run Rprog/All/install_packages.R and install gdxrrw from the ",
      "GAMS distribution if necessary."
    )
  }
  invisible(required_packages)
}

find_project_root <- function(start) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(current, "CTaxRev.Rproj"))) return(current)
    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }
  stop("Could not locate the CTaxRev project root from: ", start)
}

figure_output_dirs <- function(project_root) {
  output_root <- file.path(project_root, "output", "All")
  dirs <- list(
    root = output_root,
    png = file.path(output_root, "png"),
    svg = file.path(output_root, "svg")
  )
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
  dirs
}

save_figure_pair <- function(
    plot,
    filename_stem,
    width_mm,
    height_mm,
    output_dirs,
    dpi = 400) {
  png_file <- file.path(output_dirs$png, paste0(filename_stem, ".png"))
  svg_file <- file.path(output_dirs$svg, paste0(filename_stem, ".svg"))

  ggplot2::ggsave(
    filename = png_file,
    plot = plot,
    device = "png",
    width = width_mm,
    height = height_mm,
    units = "mm",
    dpi = dpi,
    bg = "white",
    limitsize = FALSE
  )
  ggplot2::ggsave(
    filename = svg_file,
    plot = plot,
    device = svglite::svglite,
    width = width_mm,
    height = height_mm,
    units = "mm",
    bg = "white",
    limitsize = FALSE
  )

  message("Created: ", png_file)
  message("Created: ", svg_file)
  invisible(c(png = png_file, svg = svg_file))
}
