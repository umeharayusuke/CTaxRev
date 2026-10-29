# SI figures: carbon dioxide removal and final-energy use
#
# Rows: World, recipient regions, provider regions
# Columns: BaU, Def, Aid
# Years: 2020-2050 at the model's five-year time steps

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
  library(tidyr)
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
output_dir <- file.path(project_root, "output", "Figure")
csv_dir <- file.path(output_dir, "csv")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)

# Shared definitions ----------------------------------------------------------

scenario_codes <- c(
  BaU = "SSP2_BaU_NoCC_No",
  Def = "SSP2_400C_2030CP_NoCC_No",
  Aid = "SSP2_400C_2030CP_15th_NoCC_No"
)
scenario_levels <- names(scenario_codes)

providers <- c(
  "XE25", "JPN", "TUR", "CHN", "USA",
  "XER", "XOC", "CAN", "XLM", "CIS"
)
recipients <- c("XSA", "IND", "XNF", "XAF", "XSE", "BRA", "XME")
aim17 <- c(providers, recipients)
region_levels <- c("World", "Recipient regions", "Provider regions")
years <- seq(2020, 2050, by = 5)
ink <- "#252525"

cdr_labels <- c(
  "Car_Seq_Lan_Use" = "Land-use removal",
  "Car_Seq_CCS_Bio" = "BECCS",
  "Car_Seq_Dir_Air_Cap" = "DACCS",
  "Car_Seq_Enh_Wea" = "Enhanced weathering"
)
cdr_levels <- unname(cdr_labels)
cdr_colours <- c(
  "Land-use removal" = "#91C44B",
  "BECCS" = "#4F8C63",
  "DACCS" = "#A987BD",
  "Enhanced weathering" = "#A66C45"
)

final_labels <- c(
  "Fin_Ene_Ind" = "Industry",
  "Fin_Ene_Res_and_Com" = "Buildings",
  "Fin_Ene_Tra_w_bun" = "Transport incl. bunkers",
  "Fin_Ene_Oth_Sec" = "Other sectors",
  "Fin_Ene_NonEneUse" = "Non-energy use",
  "Fin_Ene_Car_Man_Bio" = "Biomass CDR energy",
  "Fin_Ene_Car_Man_Dir_Air_Cap" = "DACCS energy",
  "Fin_Ene_Car_Man_Enh_Wea" = "Enhanced weathering energy"
)
final_levels <- unname(final_labels)
final_colours <- c(
  "Industry" = "#F08A67",
  "Buildings" = "#AABDD2",
  "Transport incl. bunkers" = "#91C44B",
  "Other sectors" = "#B9B3AF",
  "Non-energy use" = "#E4C85A",
  "Biomass CDR energy" = "#4F8C63",
  "DACCS energy" = "#A987BD",
  "Enhanced weathering energy" = "#A66C45"
)

all_variables <- c(names(cdr_labels), names(final_labels))

# Data -----------------------------------------------------------------------

raw <- rgdx.param(gdx_file, "IAMC_template") %>%
  transmute(
    Scenario_id = as.character(SCENARIO),
    Region_code = as.character(REMF),
    Variable = as.character(VEMF),
    Year = as.numeric(as.character(YEMF)),
    Value = as.numeric(IAMC_Template)
  ) %>%
  filter(
    Scenario_id %in% unname(scenario_codes),
    Region_code %in% c("World", aim17),
    Variable %in% all_variables,
    Year %in% years
  )

if (!nrow(raw)) stop("No matching CDR or final-energy records were found.")
if (any(!is.finite(raw$Value))) {
  stop("CDR or final-energy records contain non-finite values.")
}
if (any(raw$Value < 0)) {
  stop("Negative CDR or final-energy values were found; stacked areas require review.")
}

prepare_panel_data <- function(raw_data, variable_labels, scale_factor = 1) {
  variable_levels <- names(variable_labels)

  panel_data <- raw_data %>%
    filter(Variable %in% variable_levels) %>%
    mutate(
      Region_group = case_when(
        Region_code == "World" ~ "World",
        Region_code %in% recipients ~ "Recipient regions",
        Region_code %in% providers ~ "Provider regions",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(Region_group)) %>%
    group_by(Region_group, Scenario_id, Year, Variable) %>%
    summarise(Value = sum(Value) * scale_factor, .groups = "drop") %>%
    complete(
      Region_group = region_levels,
      Scenario_id = unname(scenario_codes),
      Year = years,
      Variable = variable_levels,
      fill = list(Value = 0)
    ) %>%
    mutate(
      Region_group = factor(Region_group, levels = region_levels),
      Scenario = factor(
        names(scenario_codes)[match(Scenario_id, scenario_codes)],
        levels = scenario_levels
      ),
      Component = factor(
        unname(variable_labels[Variable]),
        levels = unname(variable_labels)
      )
    )

  expected_rows <- length(region_levels) * length(scenario_codes) *
    length(years) * length(variable_levels)
  if (nrow(panel_data) != expected_rows || anyDuplicated(panel_data[c(
    "Region_group", "Scenario_id", "Year", "Variable"
  )])) {
    stop("A completed figure grid is not unique and complete.")
  }

  panel_data
}

validate_regional_sum <- function(panel_data, panel_name) {
  check <- panel_data %>%
    mutate(Region_group = as.character(Region_group)) %>%
    group_by(Scenario_id, Year, Variable) %>%
    summarise(
      World = Value[Region_group == "World"],
      AIM17 = sum(Value[Region_group != "World"]),
      Gap = AIM17 - World,
      .groups = "drop"
    )

  max_gap <- max(abs(check$Gap))
  max_scale <- max(abs(check$World))
  tolerance <- max(1e-8, 2e-5 * max_scale)
  if (!is.finite(max_gap) || max_gap > tolerance) {
    stop(
      panel_name, ": provider plus recipient values do not reproduce the ",
      "world total; maximum absolute gap = ", format(max_gap, digits = 6), "."
    )
  }
  max_gap
}

# CDR is reported in MtCO2/yr in the GDX file and converted to GtCO2/yr.
cdr_data <- prepare_panel_data(raw, cdr_labels, scale_factor = 0.001)
final_data <- prepare_panel_data(raw, final_labels, scale_factor = 1)

cdr_gap <- validate_regional_sum(cdr_data, "CDR")
final_gap <- validate_regional_sum(final_data, "Final energy")

write.csv(
  cdr_data %>%
    transmute(
      Region = as.character(Region_group),
      Scenario = as.character(Scenario),
      Year,
      Technology = as.character(Component),
      CDR_GtCO2_per_year = Value
    ),
  file.path(csv_dir, "SI_carbon_dioxide_removal_2020_2050.csv"),
  row.names = FALSE
)

write.csv(
  final_data %>%
    transmute(
      Region = as.character(Region_group),
      Scenario = as.character(Scenario),
      Year,
      Sector_or_activity = as.character(Component),
      Final_energy_EJ_per_year = Value
    ),
  file.path(csv_dir, "SI_final_energy_use_2020_2050.csv"),
  row.names = FALSE
)

# Figure ---------------------------------------------------------------------

theme_si <- function() {
  theme_classic(base_family = "Arial", base_size = 8) +
    theme(
      text = element_text(colour = ink),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 7, colour = ink),
      axis.ticks = element_line(colour = ink, linewidth = 0.24),
      axis.ticks.length = grid::unit(1.0, "mm"),
      axis.line = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.border = element_rect(fill = NA, colour = ink, linewidth = 0.32),
      plot.background = element_rect(fill = "white", colour = NA),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption = element_blank(),
      strip.background = element_blank(),
      strip.text.x = element_text(face = "bold", size = 8),
      strip.text.y = element_text(face = "bold", size = 7.5),
      strip.placement = "outside",
      panel.spacing.x = grid::unit(5.5, "mm"),
      panel.spacing.y = grid::unit(3.0, "mm"),
      legend.position = "bottom",
      legend.title = element_text(size = 7.2, face = "plain"),
      legend.text = element_text(size = 6.6),
      legend.key.height = grid::unit(3.2, "mm"),
      legend.key.width = grid::unit(5.0, "mm"),
      legend.spacing.x = grid::unit(0.7, "mm"),
      legend.margin = margin(t = 2, unit = "mm"),
      plot.margin = margin(4, 5, 3, 4, unit = "mm")
    )
}

make_stacked_figure <- function(
    data,
    component_levels,
    colours,
    legend_title,
    y_label,
    y_accuracy,
    legend_rows) {
  totals <- data %>%
    group_by(Region_group, Scenario, Year) %>%
    summarise(Total = sum(Value), .groups = "drop")

  ggplot(data, aes(x = Year, y = Value, fill = Component, group = Component)) +
    geom_area(position = "stack", colour = NA, linewidth = 0, alpha = 1) +
    geom_hline(yintercept = 0, colour = ink, linewidth = 0.28) +
    geom_line(
      data = totals,
      aes(x = Year, y = Total, group = 1),
      inherit.aes = FALSE,
      colour = ink,
      linewidth = 0.38
    ) +
    facet_grid(
      rows = vars(Region_group),
      cols = vars(Scenario),
      scales = "free_y",
      switch = "y"
    ) +
    scale_fill_manual(
      values = colours,
      breaks = component_levels,
      drop = FALSE,
      name = legend_title,
      guide = guide_legend(nrow = legend_rows, byrow = TRUE)
    ) +
    scale_x_continuous(
      breaks = c(2020, 2030, 2040, 2050),
      limits = c(2019, 2051),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(
      labels = label_number(accuracy = y_accuracy),
      expand = expansion(mult = c(0.04, 0.06))
    ) +
    labs(x = NULL, y = y_label) +
    theme_si() +
    theme(axis.text.x = element_text(size = 6.5, angle = 0, hjust = 0.5))
}

cdr_figure <- make_stacked_figure(
  data = cdr_data,
  component_levels = cdr_levels,
  colours = cdr_colours,
  legend_title = "CDR technology",
  y_label = expression("Carbon dioxide removal (GtCO"[2]~"yr"^{-1}*")"),
  y_accuracy = 0.1,
  legend_rows = 1
)

final_figure <- make_stacked_figure(
  data = final_data,
  component_levels = final_levels,
  colours = final_colours,
  legend_title = "Final-energy use",
  y_label = expression("Final energy use (EJ yr"^{-1}*")"),
  y_accuracy = 1,
  legend_rows = 2
)

save_figure <- function(figure, output_stem) {
  ggsave(
    paste0(output_stem, ".png"),
    figure,
    device = "png",
    type = "cairo",
    width = 180,
    height = 180,
    units = "mm",
    dpi = 400,
    bg = "white",
    limitsize = FALSE
  )
  ggsave(
    paste0(output_stem, ".pdf"),
    figure,
    device = cairo_pdf,
    width = 180,
    height = 180,
    units = "mm",
    bg = "white",
    limitsize = FALSE
  )
  ggsave(
    paste0(output_stem, ".svg"),
    figure,
    device = function(filename, ...) {
      grDevices::svg(filename = filename, family = "Arial", onefile = TRUE, ...)
    },
    width = 180,
    height = 180,
    units = "mm",
    bg = "white",
    limitsize = FALSE
  )

  message("Created: ", paste0(output_stem, ".png"))
  message("Created: ", paste0(output_stem, ".pdf"))
  message("Created: ", paste0(output_stem, ".svg"))
}

cdr_stem <- file.path(output_dir, "SI_carbon_dioxide_removal_2020_2050")
final_stem <- file.path(output_dir, "SI_final_energy_use_2020_2050")
save_figure(cdr_figure, cdr_stem)
save_figure(final_figure, final_stem)

message(
  "Maximum World - (recipient + provider) gap: CDR = ",
  format(cdr_gap, scientific = TRUE), " GtCO2/yr; final energy = ",
  format(final_gap, scientific = TRUE), " EJ/yr."
)

summarise_2050 <- function(data, label) {
  data %>%
    group_by(Region_group, Scenario, Year) %>%
    summarise(Total = sum(Value), .groups = "drop") %>%
    filter(Year == 2050) %>%
    mutate(Measure = label, .before = 1)
}

print(bind_rows(
  summarise_2050(cdr_data, "CDR (GtCO2/yr)"),
  summarise_2050(final_data, "Final energy (EJ/yr)")
))
