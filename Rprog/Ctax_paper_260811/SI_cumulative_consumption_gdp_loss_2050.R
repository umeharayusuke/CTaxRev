# Supplementary figure: cumulative consumption and GDP losses in 2050 -------
#
# Standalone SI version of panels b and c in
# 03_transfer_timing_consumption_gdp.png. The original figure is not modified.

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
  library(scales)
  library(patchwork)
})

scenario_def <- "SSP2_400C_2030CP_NoCC_No"
scenario_aid <- "SSP2_400C_2030CP_15th_NoCC_No"
scenarios_needed <- c(scenario_def, scenario_aid)
variables_needed <- c(
  "Pol_Cos_Cns_Los_rat_NPV_5pc",
  "Pol_Cos_GDP_Los_rat_NPV_5pc"
)
region_codes <- c("World", "Rprovider15th", "Rrecipient15th")
region_labels <- c("World", "Developed regions", "Developing regions")

read_iamc_gdxrrw <- function(path) {
  if (!requireNamespace("gdxrrw", quietly = TRUE)) return(NULL)
  candidates <- unique(c(
    Sys.getenv("GAMSDIR", unset = ""),
    "C:/GAMS/34",
    "C:/GAMS/win64/26.1"
  ))
  candidates <- candidates[nzchar(candidates) & dir.exists(candidates)]
  for (candidate in candidates) {
    loaded <- suppressWarnings(try(gdxrrw::igdx(candidate), silent = TRUE))
    if (inherits(loaded, "try-error")) next
    raw <- try(gdxrrw::rgdx.param(path, "IAMC_template"), silent = TRUE)
    if (inherits(raw, "try-error")) next
    return(data.frame(
      SCENARIO = as.character(raw$SCENARIO),
      REMF = as.character(raw$REMF),
      VEMF = as.character(raw$VEMF),
      Year = as.numeric(as.character(raw$YEMF)),
      Value = as.numeric(raw$IAMC_Template),
      stringsAsFactors = FALSE
    ))
  }
  NULL
}

find_gdxdump <- function() {
  from_path <- unname(Sys.which(c("gdxdump", "gdxdump.exe")))
  candidates <- unique(c(
    from_path[nzchar(from_path)],
    "C:/GAMS/34/gdxdump.exe",
    "C:/GAMS/win64/26.1/gdxdump.exe"
  ))
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) return(NA_character_)
  normalizePath(hit[[1]], winslash = "/")
}

read_iamc_gdxdump <- function(path) {
  gdxdump <- find_gdxdump()
  if (is.na(gdxdump)) {
    stop("Neither a working gdxrrw package nor gdxdump was found.")
  }
  dump_file <- tempfile("IAMC_template_", fileext = ".csv")
  on.exit(unlink(dump_file), add = TRUE)
  result <- system2(
    gdxdump,
    args = c(
      shQuote(normalizePath(path, winslash = "/")),
      "symb=IAMC_template",
      "format=csv",
      paste0("output=", shQuote(dump_file))
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(result, "status")
  if (is.null(status)) status <- 0L
  if (status != 0L || !file.exists(dump_file)) {
    stop("gdxdump failed: ", paste(result, collapse = "\n"))
  }

  con <- file(dump_file, open = "r", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  invisible(readLines(con, n = 1L, warn = FALSE))
  pieces <- list()
  piece_id <- 0L
  repeat {
    lines <- readLines(con, n = 200000L, warn = FALSE)
    if (!length(lines)) break
    scenario_hit <- Reduce(
      `|`,
      lapply(scenarios_needed, function(x) startsWith(lines, paste0('"', x, '",')))
    )
    variable_hit <- Reduce(
      `|`,
      lapply(variables_needed, function(x) {
        grepl(paste0(',"', x, '",'), lines, fixed = TRUE)
      })
    )
    selected <- lines[scenario_hit & variable_hit]
    if (length(selected)) {
      piece_id <- piece_id + 1L
      pieces[[piece_id]] <- read.csv(
        text = paste(selected, collapse = "\n"),
        header = FALSE,
        col.names = c("SCENARIO", "REMF", "VEMF", "Year", "Value"),
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(pieces)) stop("No required records were read from IAMC_template.")
  do.call(rbind, pieces)
}

gdx_file <- file.path(project_root, "data", "AR6database", "global_17_IAMC.gdx")
message("Reading IAMC_template from: ", gdx_file)
iamc <- read_iamc_gdxrrw(gdx_file)
if (is.null(iamc)) {
  message("A working gdxrrw installation was not found; using gdxdump fallback.")
  iamc <- read_iamc_gdxdump(gdx_file)
}

plot_data <- iamc[
  iamc$SCENARIO %in% scenarios_needed &
    iamc$VEMF %in% variables_needed &
    iamc$REMF %in% region_codes &
    iamc$Year == 2050 &
    is.finite(iamc$Value),
  c("SCENARIO", "REMF", "VEMF", "Year", "Value")
]
expected_rows <- length(scenarios_needed) * length(region_codes) *
  length(variables_needed)
if (nrow(plot_data) != expected_rows ||
    anyDuplicated(plot_data[c("SCENARIO", "REMF", "VEMF", "Year")])) {
  stop("Expected one observation for every scenario-region-indicator combination.")
}

plot_data$Region <- factor(
  region_labels[match(plot_data$REMF, region_codes)],
  levels = region_labels
)
plot_data$Policy <- factor(
  ifelse(plot_data$SCENARIO == scenario_def, "Def", "Aid"),
  levels = c("Def", "Aid")
)

colour_developed <- "#3F6C8E"
colour_developing <- "#B6573F"
ink <- "#252525"

theme_si <- function() {
  theme_classic(base_family = "Arial", base_size = 8) +
    theme(
      text = element_text(colour = ink),
      axis.line = element_blank(),
      axis.ticks = element_line(linewidth = 0.25, colour = ink),
      axis.ticks.length = grid::unit(1.2, "mm"),
      axis.title = element_text(size = 8.2),
      axis.text = element_text(size = 7.4, colour = ink),
      axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1),
      panel.grid = element_blank(),
      panel.border = element_rect(fill = NA, colour = ink, linewidth = 0.30),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption = element_blank(),
      legend.position = "bottom",
      legend.justification = "center",
      legend.box.just = "center",
      legend.title = element_blank(),
      legend.text = element_text(size = 7.4),
      legend.key.height = grid::unit(3.5, "mm"),
      legend.key.width = grid::unit(7, "mm"),
      legend.margin = margin(t = 2, unit = "mm"),
      plot.margin = margin(4, 6, 3, 4)
    )
}

make_panel <- function(variable, y_title) {
  ggplot(
    plot_data[plot_data$VEMF == variable, ],
    aes(x = Region, y = Value, colour = Region, shape = Policy, group = Policy)
  ) +
    geom_hline(yintercept = 0, linewidth = 0.35, colour = ink) +
    geom_point(
      position = position_dodge(width = 0.30),
      size = 2.35,
      stroke = 0.60
    ) +
    scale_colour_manual(
      values = c(
        "World" = ink,
        "Developed regions" = colour_developed,
        "Developing regions" = colour_developing
      ),
      limits = region_labels,
      drop = FALSE,
      guide = "none"
    ) +
    scale_shape_manual(
      values = c("Def" = 1, "Aid" = 16),
      breaks = c("Def", "Aid"),
      guide = guide_legend(
        order = 1,
        nrow = 1,
        override.aes = list(colour = ink, size = 2.35, stroke = 0.60)
      )
    ) +
    scale_x_discrete(
      labels = c(
        "World" = "World",
        "Developed regions" = "Developed",
        "Developing regions" = "Developing"
      )
    ) +
    scale_y_continuous(
      name = y_title,
      labels = label_number(accuracy = 1),
      expand = expansion(mult = c(0.15, 0.18))
    ) +
    labs(x = NULL) +
    theme_si()
}

p_consumption <- make_panel(
  "Pol_Cos_Cns_Los_rat_NPV_5pc",
  "Cumulative consumption loss in 2050 (%)"
)
p_gdp <- make_panel(
  "Pol_Cos_GDP_Los_rat_NPV_5pc",
  "Cumulative GDP loss in 2050 (%)"
)

p_si <- (p_consumption | p_gdp) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(
    legend.position = "bottom",
    plot.tag = element_text(
      family = "Arial", face = "bold", size = 8.5, colour = ink
    ),
    plot.tag.position = "topleft"
  )

output_base <- file.path(
  project_root,
  "output",
  "Figure",
  "SI_cumulative_consumption_gdp_loss_2050"
)
dir.create(dirname(output_base), recursive = TRUE, showWarnings = FALSE)

ggsave(
  paste0(output_base, ".png"),
  p_si,
  device = "png",
  type = "cairo",
  width = 180,
  height = 92,
  units = "mm",
  dpi = 400,
  bg = "white",
  limitsize = FALSE
)
ggsave(
  paste0(output_base, ".pdf"),
  p_si,
  device = cairo_pdf,
  width = 180 / 25.4,
  height = 92 / 25.4,
  units = "in",
  bg = "white",
  limitsize = FALSE
)

message("Created SI figure: ", paste0(output_base, ".png"))
message("Created SI figure: ", paste0(output_base, ".pdf"))
