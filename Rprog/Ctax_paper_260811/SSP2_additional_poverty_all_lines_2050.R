# SSP2 additional poverty across all poverty lines in 2050 ------------------
#
# This extends panel b of
# 04b_poverty_inequality_welfare_detailed_expenditure_2020_2050.png to every
# poverty line stored in PoVExp and to SSP2 BaU, Default, 10k, 15k, and 20k.
# Each stacked bar is the change from the matching SSP2 BaU headcount, split
# across the same five R5 regions and using the same regional colours.

find_project_root <- function(start) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    marker <- file.path(
      current, "data", "AR6database", "AnalysisExpenditure.gdx"
    )
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
  project_root, "data", "AR6database", "AnalysisExpenditure.gdx"
)
output_dir <- file.path(project_root, "output", "Figure", "analysis")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_stem <- file.path(
  output_dir, "13_SSP2_additional_poverty_all_lines_2050"
)

# Settings ------------------------------------------------------------------

comparison_year <- 2050
policy_levels <- c("BaU", "Default", "10th", "15th", "20th")
scenario_codes <- c(
  "BaU" = "SSP2_BaU_NoCC_No",
  "Default" = "SSP2_400C_2030CP_NoCC_No",
  "10th" = "SSP2_400C_2030CP_10th_NoCC_No",
  "15th" = "SSP2_400C_2030CP_15th_NoCC_No",
  "20th" = "SSP2_400C_2030CP_20th_NoCC_No"
)
policy_labels <- c(
  "BaU" = "BaU",
  "Default" = "Default",
  "10th" = "10k",
  "15th" = "15k",
  "20th" = "20k"
)

r5_codes <- c("R5OECD90+EU", "R5REF", "R5ASIA", "R5MAF", "R5LAM")
r5_labels <- c(
  "R5OECD90+EU" = "OECD90 + EU",
  "R5REF" = "Reforming economies",
  "R5ASIA" = "Asia",
  "R5MAF" = "Middle East & Africa",
  "R5LAM" = "Latin America"
)

# Exact palette used in panel b of the 04b source figure.
r5_colours <- c(
  "OECD90 + EU" = "#4E79A7",
  "Reforming economies" = "#8E6C8A",
  "Asia" = "#E5A03E",
  "Middle East & Africa" = "#59A14F",
  "Latin America" = "#D3725B"
)
ink <- "#252525"

check_grid <- function(data, expected, keys, context) {
  if (anyDuplicated(as.data.frame(data[keys]))) {
    stop("Duplicate records in ", context, ".")
  }
  missing <- anti_join(expected, data, by = keys)
  if (nrow(missing)) {
    stop(
      "Missing ", nrow(missing), " records in ", context, "; first: ",
      paste(unlist(missing[1, ]), collapse = " / ")
    )
  }
}

# Poverty headcounts ---------------------------------------------------------

message("Reading PoVExp from: ", gdx_file)
poverty_raw <- rgdx.param(gdx_file, "PoVExp") %>%
  transmute(
    Scenario_id = as.character(Ref),
    Region_code = as.character(R),
    Year = as.numeric(as.character(Y)),
    Threshold = as.character(TH),
    People = as.numeric(PoVExp)
  ) %>%
  filter(
    Scenario_id %in% unname(scenario_codes),
    Region_code %in% c("WLD", r5_codes),
    Year == comparison_year
  )

# Retain every PoVExp threshold common to the five SSP2 scenarios and order
# them by the numeric daily-income cutoff embedded in names such as pop_3.00.
threshold_coverage <- poverty_raw %>%
  distinct(Scenario_id, Threshold) %>%
  count(Threshold, name = "Scenario_count") %>%
  filter(Scenario_count == length(scenario_codes)) %>%
  mutate(
    Threshold_value = suppressWarnings(as.numeric(sub("^pop_", "", Threshold)))
  )
if (!nrow(threshold_coverage)) {
  stop("No poverty line is common to all selected SSP2 scenarios.")
}
if (any(!is.finite(threshold_coverage$Threshold_value))) {
  stop(
    "Could not parse numeric poverty thresholds: ",
    paste(
      threshold_coverage$Threshold[!is.finite(threshold_coverage$Threshold_value)],
      collapse = ", "
    )
  )
}
threshold_coverage <- threshold_coverage %>% arrange(Threshold_value)
threshold_levels <- threshold_coverage$Threshold
threshold_labels <- setNames(
  paste0("US$", sprintf("%.2f", threshold_coverage$Threshold_value), "/day"),
  threshold_levels
)

poverty <- poverty_raw %>%
  filter(Threshold %in% threshold_levels) %>%
  mutate(
    Policy = names(scenario_codes)[match(Scenario_id, scenario_codes)]
  )

expected <- crossing(
  Scenario_id = unname(scenario_codes),
  Region_code = c("WLD", r5_codes),
  Threshold = threshold_levels,
  Year = comparison_year
)
check_grid(
  poverty,
  expected,
  c("Scenario_id", "Region_code", "Threshold", "Year"),
  "2050 SSP2 poverty headcounts"
)
if (nrow(poverty) != nrow(expected) ||
    any(!is.finite(poverty$People) | poverty$People < 0)) {
  stop("Poverty headcounts are incomplete, non-finite, or negative.")
}

# Confirm that R5 totals reproduce the reported World headcount for every
# scenario and poverty line.
r5_world_check <- poverty %>%
  filter(Region_code %in% r5_codes) %>%
  group_by(Scenario_id, Threshold, Year) %>%
  summarise(R5_total = sum(People), .groups = "drop") %>%
  left_join(
    poverty %>%
      filter(Region_code == "WLD") %>%
      select(Scenario_id, Threshold, Year, World_total = People),
    by = c("Scenario_id", "Threshold", "Year")
  )
maximum_r5_gap <- max(abs(r5_world_check$R5_total - r5_world_check$World_total))
if (!is.finite(maximum_r5_gap) || maximum_r5_gap > 1) {
  stop("R5 totals do not reproduce the World poverty headcount.")
}

r5_baseline <- poverty %>%
  filter(Region_code %in% r5_codes, Policy == "BaU") %>%
  select(Region_code, Threshold, BaU_people = People)

plot_data <- poverty %>%
  filter(Region_code %in% r5_codes) %>%
  left_join(r5_baseline, by = c("Region_code", "Threshold")) %>%
  mutate(
    Additional_million = (People - BaU_people) / 1e6,
    Policy = factor(Policy, levels = policy_levels),
    Threshold = factor(Threshold, levels = threshold_levels),
    R5_region = factor(
      unname(r5_labels[Region_code]),
      levels = unname(r5_labels[r5_codes])
    )
  )

if (any(!is.finite(plot_data$Additional_million))) {
  stop("Additional-poverty values are non-finite.")
}

# Verify the stacked R5 differences against direct World differences.
additional_world_check <- plot_data %>%
  group_by(Policy, Threshold) %>%
  summarise(R5_additional_million = sum(Additional_million), .groups = "drop") %>%
  left_join(
    poverty %>%
      filter(Region_code == "WLD") %>%
      select(Policy, Threshold, People) %>%
      left_join(
        poverty %>%
          filter(Region_code == "WLD", Policy == "BaU") %>%
          select(Threshold, BaU_world = People),
        by = "Threshold"
      ) %>%
      transmute(
        Policy = factor(Policy, levels = policy_levels),
        Threshold = factor(Threshold, levels = threshold_levels),
        World_additional_million = (People - BaU_world) / 1e6
      ),
    by = c("Policy", "Threshold")
  )
maximum_additional_gap <- max(abs(
  additional_world_check$R5_additional_million -
    additional_world_check$World_additional_million
))
if (!is.finite(maximum_additional_gap) || maximum_additional_gap > 1e-6) {
  stop("R5 stacked differences do not reproduce the World difference.")
}

write.csv(
  plot_data %>%
    mutate(
      Policy = as.character(Policy),
      Threshold = as.character(Threshold),
      R5_region = as.character(R5_region)
    ) %>%
    select(
      Scenario_id, Policy, Threshold, Region_code, R5_region,
      People, BaU_people, Additional_million
    ),
  paste0(output_stem, "_data.csv"),
  row.names = FALSE
)

# Figure ---------------------------------------------------------------------

theme_nature <- function() {
  theme_classic(base_family = "Arial", base_size = 8) +
    theme(
      text = element_text(colour = ink),
      axis.title = element_text(size = 8.2),
      axis.text = element_text(size = 7.2, colour = ink),
      axis.ticks = element_line(colour = ink, linewidth = 0.24),
      axis.ticks.length = grid::unit(1.1, "mm"),
      axis.line = element_blank(),
      panel.border = element_rect(fill = NA, colour = ink, linewidth = 0.32),
      panel.grid.major.y = element_line(colour = "#E2E2E2", linewidth = 0.22),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      strip.background = element_blank(),
      strip.text = element_text(size = 8.1, face = "bold", margin = margin(b = 1.5)),
      panel.spacing.x = grid::unit(3.0, "mm"),
      panel.spacing.y = grid::unit(4.0, "mm"),
      legend.position = "bottom",
      legend.justification = "center",
      legend.title = element_text(size = 7.2, face = "plain"),
      legend.text = element_text(size = 6.9),
      legend.key.height = grid::unit(3.4, "mm"),
      legend.key.width = grid::unit(5.5, "mm"),
      plot.margin = margin(4, 5, 3, 4, unit = "mm")
    )
}

p <- ggplot(
  plot_data,
  aes(x = Policy, y = Additional_million, fill = R5_region)
) +
  geom_hline(yintercept = 0, colour = ink, linewidth = 0.32) +
  geom_col(width = 0.68, colour = "white", linewidth = 0.16) +
  facet_wrap(
    ~Threshold,
    ncol = min(3L, length(threshold_levels)),
    labeller = as_labeller(threshold_labels),
    drop = FALSE
  ) +
  scale_x_discrete(labels = policy_labels, drop = FALSE) +
  scale_fill_manual(
    values = r5_colours,
    breaks = unname(r5_labels[r5_codes]),
    drop = FALSE,
    guide = guide_legend(title = "R5 region", nrow = 2, byrow = TRUE)
  ) +
  scale_y_continuous(
    labels = label_number(accuracy = 1),
    expand = expansion(mult = c(0.10, 0.14))
  ) +
  labs(
    x = NULL,
    y = "Additional population relative to BaU in 2050 (million)"
  ) +
  theme_nature() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),
    legend.box = "horizontal"
  )

number_of_rows <- ceiling(length(threshold_levels) / 3)
figure_height <- if (number_of_rows <= 1) 92 else 74 + 48 * number_of_rows

ggsave(
  filename = paste0(output_stem, ".png"),
  plot = p,
  device = "png",
  type = "cairo",
  width = 180,
  height = figure_height,
  units = "mm",
  dpi = 400,
  bg = "white",
  limitsize = FALSE
)

message("Created: ", paste0(output_stem, ".png"))
message("Created: ", paste0(output_stem, "_data.csv"))
message("Poverty lines: ", paste(threshold_labels, collapse = ", "))
message("Maximum R5-World poverty gap (people): ", maximum_r5_gap)
message(
  "Maximum R5-World additional-poverty gap (million people): ",
  maximum_additional_gap
)
