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

check_required_packages <- function() {
  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
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

evaluate_script_prefix <- function(
    project_root,
    script_name,
    stop_pattern,
    occurrence = 1L,
    working_dir = project_root) {
  script_path <- file.path(
    project_root, "Rprog", "Ctax_paper_260811", script_name
  )
  if (!file.exists(script_path)) stop("Missing source script: ", script_path)

  lines <- readLines(script_path, warn = FALSE, encoding = "UTF-8")
  boundaries <- grep(stop_pattern, lines)
  if (length(boundaries) < occurrence) {
    stop(
      "Could not find boundary ", stop_pattern, " (occurrence ", occurrence,
      ") in ", script_path
    )
  }
  boundary <- boundaries[[occurrence]]
  if (boundary <= 1L) stop("Invalid source boundary in: ", script_path)

  env <- new.env(parent = globalenv())
  env$ggsave <- function(...) invisible(NULL)
  env$unlink <- function(...) invisible(0L)

  old_dir <- getwd()
  on.exit(setwd(old_dir), add = TRUE)
  setwd(working_dir)
  eval(
    parse(text = paste(lines[seq_len(boundary - 1L)], collapse = "\n")),
    envir = env
  )
  env
}

get_plot_object <- function(env, object_name, script_name) {
  if (!exists(object_name, envir = env, inherits = FALSE)) {
    stop("Plot object '", object_name, "' was not created by ", script_name)
  }
  get(object_name, envir = env, inherits = FALSE)
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

build_figure <- function(
    project_root,
    output_dirs,
    script_name,
    stop_pattern,
    plot_object,
    filename_stem,
    width_mm,
    height_mm,
    occurrence = 1L,
    working_dir = project_root) {
  message("Building ", filename_stem, " ...")
  env <- evaluate_script_prefix(
    project_root = project_root,
    script_name = script_name,
    stop_pattern = stop_pattern,
    occurrence = occurrence,
    working_dir = working_dir
  )
  plot <- get_plot_object(env, plot_object, script_name)
  save_figure_pair(
    plot = plot,
    filename_stem = filename_stem,
    width_mm = width_mm,
    height_mm = if (is.function(height_mm)) height_mm(env) else height_mm,
    output_dirs = output_dirs
  )
}
