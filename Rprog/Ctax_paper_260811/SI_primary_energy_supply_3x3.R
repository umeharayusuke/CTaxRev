# SI: primary energy supply by source, scenario, and transfer-side region
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
output_stem <- file.path(output_dir, "SI_primary_energy_supply_2020_2050")
source_data_file <- file.path(
  csv_dir, "SI_primary_energy_supply_2020_2050.csv"
)

# Definitions ----------------------------------------------------------------

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

variable_labels <- c(
  "Prm_Ene_Coa" = "Coal",
  "Prm_Ene_Oil" = "Oil",
  "Prm_Ene_Gas" = "Gas",
  "Prm_Ene_Bio" = "Bioenergy",
  "Prm_Ene_Nuc" = "Nuclear",
  "Prm_Ene_Hyp" = "Hydropower",
  "Prm_Ene_Geo" = "Geothermal",
  "Prm_Ene_Solar" = "Solar",
  "Prm_Ene_Win" = "Wind",
  "Prm_Ene_Oth" = "Other",
  "Prm_Ene_Sec_Ene_Trd" = "Secondary-energy trade"
)
# Legend order also controls the top-to-bottom stacking order in ggplot2.
# Place low-carbon sources at the top and fossil fuels at the bottom.
source_levels <- c(
  "Solar",
  "Wind",
  "Hydropower",
  "Geothermal",
  "Bioenergy",
  "Nuclear",
  "Secondary-energy trade",
  "Other",
  "Gas",
  "Oil",
  "Coal"
)

# Muted palette adapted to the energy-system figures in the reference report.
source_colours <- c(
  "Solar" = "#F08A67",
  "Wind" = "#83ACC5",
  "Hydropower" = "#AABDD2",
  "Geothermal" = "#A66C45",
  "Bioenergy" = "#91C44B",
  "Nuclear" = "#E4C85A",
  "Secondary-energy trade" = "#A987BD",
  "Other" = "#B9B3AF",
  "Gas" = "#F3D79A",
  "Oil" = "#F0A05C",
  "Coal" = "#666666"
)
ink <- "#252525"

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
    Variable %in% names(variable_labels),
    Year %in% years
  )

if (!nrow(raw)) stop("No matching primary-energy records were found.")
if (any(!is.finite(raw$Value))) {
  stop("Primary-energy records contain non-finite values.")
}

# Aggregate the same 17 AIM regions for every scenario.  This is necessary
# because the provider aggregate is not directly reported for BaU.
primary_energy <- raw %>%
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
  summarise(Value = sum(Value), .groups = "drop") %>%
  complete(
    Region_group = region_levels,
    Scenario_id = unname(scenario_codes),
    Year = years,
    Variable = names(variable_labels),
    fill = list(Value = 0)
  ) %>%
  mutate(
    Region_group = factor(Region_group, levels = region_levels),
    Scenario = factor(
      names(scenario_codes)[match(Scenario_id, scenario_codes)],
      levels = scenario_levels
    ),
    Source = factor(
      unname(variable_labels[Variable]),
      levels = source_levels
    )
  )

expected_rows <- length(region_levels) * length(scenario_codes) *
  length(years) * length(variable_labels)
if (nrow(primary_energy) != expected_rows || anyDuplicated(primary_energy[c(
  "Region_group", "Scenario_id", "Year", "Variable"
)])) {
  stop("The completed primary-energy grid is not unique and complete.")
}

# Validate that the provider and recipient sums reproduce the reported world
# total for every scenario, year, and source.
regional_check <- primary_energy %>%
  mutate(Region_group = as.character(Region_group)) %>%
  group_by(Scenario_id, Year, Variable) %>%
  summarise(
    World = Value[Region_group == "World"],
    AIM17 = sum(Value[Region_group != "World"]),
    Gap = AIM17 - World,
    .groups = "drop"
  )
max_world_gap <- max(abs(regional_check$Gap))
max_world_scale <- max(abs(regional_check$World))
if (!is.finite(max_world_gap) || max_world_gap > max(1e-6, 2e-5 * max_world_scale)) {
  stop(
    "Provider plus recipient primary energy does not reproduce the world ",
    "total; maximum absolute gap = ", format(max_world_gap, digits = 6), "."
  )
}

totals <- primary_energy %>%
  group_by(Region_group, Scenario, Year) %>%
  summarise(Total = sum(Value), .groups = "drop")

write.csv(
  primary_energy %>%
    transmute(
      Region = as.character(Region_group),
      Scenario = as.character(Scenario),
      Year,
      Source = as.character(Source),
      Primary_energy_EJ_per_year = Value
    ),
  source_data_file,
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

figure <- ggplot(
  primary_energy,
  aes(x = Year, y = Value, fill = Source, group = Source)
) +
  geom_area(
    position = "stack",
    colour = NA,
    linewidth = 0,
    alpha = 1
  ) +
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
    values = source_colours,
    breaks = source_levels,
    drop = FALSE,
    name = "Primary-energy source",
    guide = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  scale_x_continuous(
    breaks = c(2020, 2030, 2040, 2050),
    limits = c(2019, 2051),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    labels = label_number(accuracy = 1),
    expand = expansion(mult = c(0.04, 0.06))
  ) +
  labs(
    x = NULL,
    y = expression("Primary energy supply (EJ yr"^{-1}*")")
  ) +
  theme_si() +
  theme(
    strip.placement = "outside",
    axis.text.x = element_text(size = 6.5, angle = 0, hjust = 0.5)
  )

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
message("Created: ", source_data_file)
message(
  "Maximum absolute World - (recipient + provider) gap (EJ/yr): ",
  format(max_world_gap, scientific = TRUE)
)
print(
  totals %>%
    filter(Year == 2050) %>%
    arrange(Region_group, Scenario)
)
