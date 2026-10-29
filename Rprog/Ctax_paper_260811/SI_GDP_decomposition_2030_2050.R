# SI: GDP expenditure decomposition from 2030 to 2050 ------------------------
#
# This script extends panel e of 03_transfer_timing_consumption_gdp.png across
# all available five-year model years from 2030 through 2050. The original
# composite figure is not modified. For each year, scenario and region group,
# expenditure-component changes relative to the same-year BaU are expressed as
# percentage-point contributions using the same-year BaU GDP as denominator.
# Diamonds and connecting lines show the resulting net GDP change.

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
})

if (!requireNamespace("gdxrrw", quietly = TRUE)) {
  stop("gdxrrw is required to read GDP_s from the scenario GDX files.")
}

gams_candidates <- c(
  Sys.getenv("GAMSDIR", unset = ""),
  "C:/GAMS/34",
  "C:/GAMS/win64/26.1"
)
gams_candidates <- unique(gams_candidates[nzchar(gams_candidates)])
gdx_initialised <- FALSE
for (candidate in gams_candidates) {
  if (!dir.exists(candidate)) next
  loaded <- suppressWarnings(try(gdxrrw::igdx(candidate), silent = TRUE))
  if (!inherits(loaded, "try-error")) {
    gdx_initialised <- TRUE
    break
  }
}
if (!gdx_initialised) stop("Could not initialise gdxrrw with a GAMS directory.")

model_years <- seq(2030, 2050, by = 5)
providers <- c(
  "XE25", "JPN", "TUR", "CHN", "USA",
  "XER", "XOC", "CAN", "XLM", "CIS"
)
recipients <- c("XSA", "IND", "XNF", "XAF", "XSE", "BRA", "XME")

scenario_paths <- c(
  BaU = file.path(
    project_root, "data", "AR6database", "SSP2_BaU_NoCC_No.gdx"
  ),
  Def = file.path(
    project_root, "data", "AR6database", "SSP2_400C_2030CP_NoCC_No.gdx"
  ),
  Aid = file.path(
    project_root, "data", "AR6database",
    "SSP2_400C_2030CP_15th_NoCC_No.gdx"
  )
)
missing_files <- scenario_paths[!file.exists(scenario_paths)]
if (length(missing_files)) {
  stop("Missing GDP decomposition GDX file(s): ",
       paste(missing_files, collapse = ", "))
}

region_map <- tibble::tibble(
  Region_code = c(providers, recipients),
  Region_group = c(
    rep("Developed regions", length(providers)),
    rep("Developing regions", length(recipients))
  )
)
region_levels <- c("Developed regions", "Developing regions")
category_levels <- c(
  "Consumption", "Government spending", "Investment", "Net exports"
)

read_gdp_components <- function(scenario_name, path) {
  raw <- gdxrrw::rgdx.param(path, "GDP_s")
  required <- c("Y", "R", "INS_MCR", "GDP_s")
  if (!all(required %in% names(raw))) {
    stop("Unexpected GDP_s columns in ", basename(path), ".")
  }
  raw %>%
    transmute(
      Scenario = scenario_name,
      Year = as.numeric(as.character(Y)),
      Region_code = as.character(R),
      Category_code = as.character(INS_MCR),
      Category = case_when(
        Category_code == "HURB" ~ "Consumption",
        Category_code == "GOV" ~ "Government spending",
        Category_code == "S-I" ~ "Investment",
        Category_code %in% c("ROW", "IMP") ~ "Net exports",
        TRUE ~ NA_character_
      ),
      Value = as.numeric(GDP_s)
    )
}

gdp_raw <- bind_rows(Map(
  read_gdp_components,
  names(scenario_paths),
  unname(scenario_paths)
))
if (any(is.na(gdp_raw$Year)) || any(!is.finite(gdp_raw$Value))) {
  stop("GDP_s contains a non-numeric year or non-finite value.")
}
if (any(is.na(gdp_raw$Category))) {
  stop(
    "Unrecognised GDP_s category code(s): ",
    paste(unique(gdp_raw$Category_code[is.na(gdp_raw$Category)]),
          collapse = ", ")
  )
}

gdp_grouped <- gdp_raw %>%
  filter(
    Year %in% model_years,
    Region_code %in% region_map$Region_code
  ) %>%
  left_join(region_map, by = "Region_code") %>%
  group_by(Year, Scenario, Region_group, Category) %>%
  summarise(Value = sum(Value), .groups = "drop") %>%
  complete(
    Year = model_years,
    Scenario = names(scenario_paths),
    Region_group = region_levels,
    Category = category_levels,
    fill = list(Value = 0)
  )

expected_rows <- length(model_years) * length(scenario_paths) *
  length(region_levels) * length(category_levels)
if (nrow(gdp_grouped) != expected_rows ||
    anyDuplicated(gdp_grouped[c(
      "Year", "Scenario", "Region_group", "Category"
    )])) {
  stop("The GDP year-scenario-region-category grid is incomplete or duplicated.")
}

gdp_contributions <- gdp_grouped %>%
  group_by(Year, Region_group, Category) %>%
  mutate(BaU_category_value = Value[Scenario == "BaU"][[1]]) %>%
  ungroup() %>%
  group_by(Year, Region_group, Scenario) %>%
  mutate(
    BaU_total = sum(BaU_category_value),
    Contribution_pp = 100 * (Value - BaU_category_value) / BaU_total
  ) %>%
  ungroup() %>%
  filter(Scenario %in% c("Def", "Aid")) %>%
  mutate(
    Scenario = factor(Scenario, levels = c("Def", "Aid")),
    Region_group = factor(Region_group, levels = region_levels),
    Category = factor(Category, levels = category_levels)
  ) %>%
  arrange(Region_group, Scenario, Year, Category)

gdp_totals <- gdp_contributions %>%
  group_by(Year, Region_group, Scenario) %>%
  summarise(
    Scenario_total = sum(Value),
    BaU_total = first(BaU_total),
    Total_change_pct = sum(Contribution_pp),
    .groups = "drop"
  ) %>%
  mutate(
    Direct_change_pct = 100 * (Scenario_total - BaU_total) / BaU_total,
    Label = sprintf("%+.1f", Total_change_pct)
  )

decomposition_error <- max(abs(
  gdp_totals$Total_change_pct - gdp_totals$Direct_change_pct
))
if (!is.finite(decomposition_error) || decomposition_error > 1e-10) {
  stop("GDP contributions do not reproduce the total GDP change.")
}

y_range <- range(c(
  gdp_contributions$Contribution_pp,
  gdp_totals$Total_change_pct
))
label_offset <- max(0.14, 0.018 * diff(y_range))
gdp_totals <- gdp_totals %>%
  mutate(
    Label_y = Total_change_pct +
      if_else(Total_change_pct >= 0, label_offset, -label_offset),
    Label_vjust = if_else(Total_change_pct >= 0, 0, 1)
  )

component_colours <- c(
  "Consumption" = "#0072B2",
  "Government spending" = "#CC79A7",
  "Investment" = "#E69F00",
  "Net exports" = "#009E73"
)
ink <- "#252525"

si_figure <- ggplot(
  gdp_contributions,
  aes(x = Year, y = Contribution_pp, fill = Category)
) +
  geom_hline(yintercept = 0, colour = ink, linewidth = 0.40) +
  geom_col(width = 3.2, colour = "white", linewidth = 0.20) +
  geom_line(
    data = gdp_totals,
    aes(x = Year, y = Total_change_pct, group = 1),
    inherit.aes = FALSE,
    colour = ink,
    linewidth = 0.55
  ) +
  geom_point(
    data = gdp_totals,
    aes(x = Year, y = Total_change_pct, shape = "Net GDP change"),
    inherit.aes = FALSE,
    size = 2.45,
    stroke = 0.60,
    fill = "white",
    colour = ink
  ) +
  geom_text(
    data = gdp_totals,
    aes(
      x = Year,
      y = Label_y,
      label = Label,
      vjust = Label_vjust
    ),
    inherit.aes = FALSE,
    family = "Arial",
    size = 2.25,
    colour = ink
  ) +
  facet_grid(
    Region_group ~ Scenario,
    labeller = labeller(
      Region_group = c(
        "Developed regions" = "Developed",
        "Developing regions" = "Developing"
      )
    )
  ) +
  scale_fill_manual(
    values = component_colours,
    breaks = category_levels,
    labels = c(
      "Consumption" = "Consumption",
      "Government spending" = "Government",
      "Investment" = "Investment",
      "Net exports" = "Net exports"
    ),
    name = NULL
  ) +
  scale_shape_manual(
    values = c("Net GDP change" = 23),
    labels = c("Net GDP change" = "Net GDP"),
    name = NULL
  ) +
  scale_x_continuous(
    breaks = model_years,
    expand = expansion(mult = c(0.045, 0.045))
  ) +
  scale_y_continuous(
    name = "Contribution to GDP change relative to BaU (%)",
    labels = label_number(accuracy = 0.1),
    expand = expansion(mult = c(0.12, 0.14))
  ) +
  labs(x = NULL) +
  guides(
    fill = guide_legend(order = 1, nrow = 1, byrow = TRUE),
    shape = guide_legend(order = 2)
  ) +
  coord_cartesian(clip = "off") +
  theme_classic(base_family = "Arial", base_size = 9) +
  theme(
    text = element_text(colour = ink),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.border = element_rect(fill = NA, colour = ink, linewidth = 0.35),
    axis.line = element_blank(),
    axis.ticks = element_line(linewidth = 0.25, colour = ink),
    axis.ticks.length = grid::unit(1.2, "mm"),
    axis.title = element_text(size = 9.2),
    axis.text = element_text(size = 8.1, colour = ink),
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "plain", size = 9.0, colour = ink),
    panel.spacing = grid::unit(4.0, "mm"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.justification = "center",
    legend.text = element_text(size = 8.0),
    legend.key.width = grid::unit(6.0, "mm"),
    legend.key.height = grid::unit(3.5, "mm"),
    legend.spacing.x = grid::unit(1.5, "mm"),
    plot.margin = margin(5, 8, 5, 5)
  )

output_dir <- file.path(project_root, "output", "Figure")
csv_dir <- file.path(output_dir, "csv")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)

png_file <- file.path(output_dir, "SI_GDP_decomposition_2030_2050.png")
svg_file <- file.path(output_dir, "SI_GDP_decomposition_2030_2050.svg")
csv_file <- file.path(csv_dir, "SI_GDP_decomposition_2030_2050_source_data.csv")

ggsave(
  filename = png_file,
  plot = si_figure,
  device = "png",
  type = "cairo",
  width = 180,
  height = 142,
  units = "mm",
  dpi = 400,
  bg = "white"
)
if (requireNamespace("svglite", quietly = TRUE)) {
  ggsave(
    filename = svg_file,
    plot = si_figure,
    device = svglite::svglite,
    width = 180 / 25.4,
    height = 142 / 25.4,
    bg = "white"
  )
}

source_data <- gdp_contributions %>%
  left_join(
    gdp_totals %>%
      select(
        Year, Region_group, Scenario,
        Net_GDP_change_pct = Total_change_pct
      ),
    by = c("Year", "Region_group", "Scenario")
  ) %>%
  select(
    Year, Region_group, Scenario, Category,
    Component_contribution_pp = Contribution_pp,
    Net_GDP_change_pct
  )
write.csv(source_data, csv_file, row.names = FALSE, na = "")

message("Saved SI figure: ", png_file)
message("Saved source data: ", csv_file)
message(
  "Maximum GDP decomposition error: ",
  format(decomposition_error, scientific = TRUE)
)
