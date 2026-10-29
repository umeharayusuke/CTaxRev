# World population and GDP trajectories under SSP1-SSP3 ---------------------
#
# The socioeconomic trajectories are read from each SSP's BaU scenario because
# Pop and GDP_MER define the underlying SSP pathway and do not require an aid
# or mitigation-policy variant. Raw population is million people; raw GDP_MER
# is billion USD_2010/yr. Both are converted to compact plotting units below.

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

scenario_grid <- data.frame(
  SSP = paste0("SSP", 1:3),
  SCENARIO = paste0("SSP", 1:3, "_BaU_NoCC_No"),
  stringsAsFactors = FALSE
)
model_years <- seq(2020, 2050, by = 5)
variables <- c("Pop", "GDP_MER")

gdx_file <- file.path(
  project_root, "data", "AR6database", "global_17_IAMC.gdx"
)
message("Reading IAMC_template from: ", gdx_file)
raw <- gdxrrw::rgdx.param(gdx_file, "IAMC_template")

required_columns <- c("SCENARIO", "REMF", "VEMF", "YEMF", "IAMC_Template")
if (!all(required_columns %in% names(raw))) {
  stop("IAMC_template does not have the expected columns.")
}

plot_data <- raw %>%
  transmute(
    SCENARIO = as.character(SCENARIO),
    REMF = as.character(REMF),
    VEMF = as.character(VEMF),
    Year = as.numeric(as.character(YEMF)),
    Value = as.numeric(IAMC_Template)
  ) %>%
  inner_join(scenario_grid, by = "SCENARIO") %>%
  filter(
    REMF == "World",
    VEMF %in% variables,
    Year %in% model_years
  ) %>%
  select(SSP, SCENARIO, VEMF, Year, Value) %>%
  arrange(VEMF, SSP, Year)

expected_rows <- nrow(scenario_grid) * length(variables) * length(model_years)
if (nrow(plot_data) != expected_rows) {
  stop(
    "Expected ", expected_rows, " World Pop/GDP observations; found ",
    nrow(plot_data), "."
  )
}
if (anyDuplicated(plot_data[c("SCENARIO", "VEMF", "Year")])) {
  stop("Duplicate World Pop/GDP observations were found.")
}
if (any(!is.finite(plot_data$Value)) || any(plot_data$Value <= 0)) {
  stop("World Pop/GDP observations must be finite and positive.")
}

plot_data <- plot_data %>%
  mutate(
    SSP = factor(SSP, levels = paste0("SSP", 1:3)),
    Plot_value = case_when(
      VEMF == "Pop" ~ Value / 1000,
      VEMF == "GDP_MER" ~ Value / 1000,
      TRUE ~ NA_real_
    )
  )

population_data <- plot_data %>% filter(VEMF == "Pop")
gdp_data <- plot_data %>% filter(VEMF == "GDP_MER")

ssp_colours <- c(
  "SSP1" = "#0072B2",
  "SSP2" = "#D55E00",
  "SSP3" = "#009E73"
)
ssp_shapes <- c("SSP1" = 16, "SSP2" = 17, "SSP3" = 15)
ink <- "#252525"

common_scales <- list(
  scale_colour_manual(values = ssp_colours, name = NULL),
  scale_shape_manual(values = ssp_shapes, name = NULL),
  scale_x_continuous(
    breaks = c(2020, 2030, 2040, 2050),
    limits = range(model_years),
    expand = expansion(mult = c(0.02, 0.02))
  )
)

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
      legend.position = "bottom",
      legend.justification = "center",
      legend.text = element_text(size = 7.4),
      legend.key.width = grid::unit(7, "mm"),
      plot.margin = margin(4, 6, 3, 4)
    )
}

p_population <- ggplot(
  population_data,
  aes(x = Year, y = Plot_value, colour = SSP, shape = SSP, group = SSP)
) +
  geom_line(linewidth = 0.78) +
  geom_point(size = 2.05, stroke = 0.55) +
  common_scales +
  scale_y_continuous(
    name = "Global population (billion people)",
    labels = label_number(accuracy = 0.1),
    expand = expansion(mult = c(0.08, 0.10))
  ) +
  labs(x = NULL) +
  theme_sensitivity()

p_gdp <- ggplot(
  gdp_data,
  aes(x = Year, y = Plot_value, colour = SSP, shape = SSP, group = SSP)
) +
  geom_line(linewidth = 0.78) +
  geom_point(size = 2.05, stroke = 0.55) +
  common_scales +
  scale_y_continuous(
    name = expression(paste("Global GDP, MER (trillion US$"[2010], "/yr)")),
    labels = label_number(accuracy = 1),
    expand = expansion(mult = c(0.08, 0.10))
  ) +
  labs(x = NULL) +
  theme_sensitivity()

p <- (
  p_population | p_gdp
) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.tag = element_text(
      family = "Arial", face = "bold", size = 9.0, colour = ink
    ),
    plot.tag.position = "topleft"
  )

output_dir <- file.path(project_root, "output", "Figure", "analysis")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_file <- file.path(
  output_dir, "05_sensitivity_world_population_GDP_SSP1_SSP3.png"
)

ggsave(
  filename = output_file,
  plot = p,
  device = "png",
  type = "cairo",
  width = 180,
  height = 82,
  units = "mm",
  dpi = 400,
  bg = "white"
)

message("Created: ", output_file)
message("World population and GDP values in plotting units:")
print(
  plot_data %>% select(SSP, VEMF, Year, Plot_value),
  row.names = FALSE,
  digits = 5
)
