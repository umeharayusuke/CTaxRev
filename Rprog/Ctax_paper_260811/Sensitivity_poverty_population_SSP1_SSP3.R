# Sensitivity of poverty headcounts under SSP1-SSP3 -------------------------
#
# Panel a shows the World population below US$3.00/day from 2020 to 2050 for
# BaU, no-aid Default, and the 10k/15k/20k aid thresholds. Panel b decomposes
# the additional 2050 poverty headcount relative to each SSP's BaU across the
# five R5 regions. SSP3 10k and 15k aid cases are excluded throughout.

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
  library(patchwork)
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

ssp_levels <- paste0("SSP", 1:3)
policy_levels <- c("BaU", "Default", "10th", "15th", "20th")
years <- c(2020, 2030, 2040, 2050)
comparison_year <- 2050

scenario_grid <- crossing(
  SSP = ssp_levels,
  Policy = policy_levels
) %>%
  mutate(
    Scenario_id = case_when(
      Policy == "BaU" ~ paste0(SSP, "_BaU_NoCC_No"),
      Policy == "Default" ~ paste0(SSP, "_400C_2030CP_NoCC_No"),
      TRUE ~ paste0(SSP, "_400C_2030CP_", Policy, "_NoCC_No")
    )
  ) %>%
  filter(!(SSP == "SSP3" & Policy %in% c("10th", "15th")))

r5_codes <- c("R5OECD90+EU", "R5REF", "R5ASIA", "R5MAF", "R5LAM")
r5_labels <- c(
  "R5OECD90+EU" = "OECD90 + EU",
  "R5REF" = "Reforming economies",
  "R5ASIA" = "Asia",
  "R5MAF" = "Middle East & Africa",
  "R5LAM" = "Latin America"
)

# Exact five-region palette used in
# 04b_poverty_inequality_welfare_detailed_expenditure_2020_2050.png.
r5_colours <- c(
  "OECD90 + EU" = "#4E79A7",
  "Reforming economies" = "#8E6C8A",
  "Asia" = "#E5A03E",
  "Middle East & Africa" = "#59A14F",
  "Latin America" = "#D3725B"
)

policy_colours <- c(
  "BaU" = "#4D4D4D",
  "Default" = "#8C6BB1",
  "10th" = "#0072B2",
  "15th" = "#D55E00",
  "20th" = "#009E73"
)
policy_shapes <- c("BaU" = 1, "Default" = 18, "10th" = 16, "15th" = 17, "20th" = 15)
policy_linetypes <- c(
  "BaU" = "22", "Default" = "longdash", "10th" = "solid",
  "15th" = "solid", "20th" = "solid"
)
policy_labels <- c(
  "BaU" = "BaU", "Default" = "Default", "10th" = "10k USD",
  "15th" = "15k USD", "20th" = "20k USD"
)
ink <- "#252525"

message("Reading PoVExp from: ", gdx_file)
poverty <- rgdx.param(gdx_file, "PoVExp") %>%
  transmute(
    Scenario_id = as.character(Ref),
    Region_code = as.character(R),
    Year = as.numeric(as.character(Y)),
    Threshold = as.character(TH),
    People = as.numeric(PoVExp)
  ) %>%
  filter(
    Scenario_id %in% scenario_grid$Scenario_id,
    Region_code %in% c("WLD", r5_codes),
    Year %in% years,
    Threshold == "pop_3.00"
  ) %>%
  inner_join(scenario_grid, by = "Scenario_id")

expected_rows <- nrow(scenario_grid) * (length(r5_codes) + 1L) * length(years)
if (nrow(poverty) != expected_rows) {
  stop(
    "Expected ", expected_rows, " poverty observations; found ",
    nrow(poverty), "."
  )
}
if (anyDuplicated(poverty[c("Scenario_id", "Region_code", "Year")])) {
  stop("Duplicate poverty observations were found.")
}
if (any(!is.finite(poverty$People)) || any(poverty$People < 0)) {
  stop("Poverty headcounts must be finite and non-negative.")
}

r5_world_check <- poverty %>%
  filter(Region_code %in% r5_codes) %>%
  group_by(Scenario_id, Year) %>%
  summarise(R5_total = sum(People), .groups = "drop") %>%
  left_join(
    poverty %>%
      filter(Region_code == "WLD") %>%
      select(Scenario_id, Year, World_total = People),
    by = c("Scenario_id", "Year")
  )
maximum_r5_gap <- max(abs(r5_world_check$R5_total - r5_world_check$World_total))
if (!is.finite(maximum_r5_gap) || maximum_r5_gap > 1) {
  stop("R5 poverty totals do not reproduce the World poverty headcount.")
}

world_poverty <- poverty %>%
  filter(Region_code == "WLD") %>%
  transmute(
    SSP = factor(SSP, levels = ssp_levels),
    Policy = factor(Policy, levels = policy_levels),
    Year,
    Population_million = People / 1e6
  )

baseline_2050 <- poverty %>%
  filter(
    Region_code %in% r5_codes,
    Policy == "BaU",
    Year == comparison_year
  ) %>%
  select(SSP, Region_code, BaU_people = People)

r5_additional_2050 <- poverty %>%
  filter(
    Region_code %in% r5_codes,
    Policy != "BaU",
    Year == comparison_year
  ) %>%
  left_join(baseline_2050, by = c("SSP", "Region_code")) %>%
  mutate(
    Additional_million = (People - BaU_people) / 1e6,
    SSP = factor(SSP, levels = ssp_levels),
    Policy = factor(Policy, levels = policy_levels[-1]),
    R5_region = factor(
      unname(r5_labels[Region_code]),
      levels = unname(r5_labels[r5_codes])
    )
  )

expected_additional_rows <- (nrow(scenario_grid) - length(ssp_levels)) *
  length(r5_codes)
if (nrow(r5_additional_2050) != expected_additional_rows ||
    any(!is.finite(r5_additional_2050$Additional_million))) {
  stop("The 2050 R5 additional-poverty decomposition is incomplete.")
}

additional_world_check <- r5_additional_2050 %>%
  group_by(SSP, Policy) %>%
  summarise(R5_additional = sum(Additional_million), .groups = "drop") %>%
  left_join(
    world_poverty %>%
      filter(Policy != "BaU", Year == comparison_year) %>%
      left_join(
        world_poverty %>%
          filter(Policy == "BaU", Year == comparison_year) %>%
          select(SSP, BaU_world = Population_million),
        by = "SSP"
      ) %>%
      transmute(
        SSP,
        Policy,
        World_additional = Population_million - BaU_world
      ),
    by = c("SSP", "Policy")
  )
maximum_additional_gap <- max(abs(
  additional_world_check$R5_additional -
    additional_world_check$World_additional
))
if (!is.finite(maximum_additional_gap) || maximum_additional_gap > 1e-6) {
  stop("R5 additional-poverty totals do not reproduce the World difference.")
}

theme_nature <- function() {
  theme_classic(base_family = "Arial", base_size = 8) +
    theme(
      text = element_text(colour = ink),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 7.2, colour = ink),
      axis.ticks = element_line(colour = ink, linewidth = 0.24),
      axis.ticks.length = grid::unit(1.1, "mm"),
      axis.line = element_blank(),
      panel.border = element_rect(fill = NA, colour = ink, linewidth = 0.32),
      panel.grid.major.y = element_line(colour = "#E2E2E2", linewidth = 0.22),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption = element_blank(),
      plot.tag = element_text(face = "bold", size = 8.5, colour = ink),
      plot.tag.position = "topleft",
      legend.title = element_text(size = 7.2, face = "plain"),
      legend.text = element_text(size = 6.9),
      legend.key.height = grid::unit(3.3, "mm"),
      legend.key.width = grid::unit(6.2, "mm"),
      legend.position = "bottom",
      legend.justification = "center",
      plot.margin = margin(3, 4, 3, 4)
    )
}

p_a <- ggplot(
  world_poverty,
  aes(
    x = Year,
    y = Population_million,
    colour = Policy,
    shape = Policy,
    linetype = Policy,
    group = Policy
  )
) +
  geom_line(linewidth = 0.70) +
  geom_point(size = 1.90, stroke = 0.55) +
  facet_wrap(~SSP, nrow = 1, drop = FALSE) +
  scale_colour_manual(
    values = policy_colours,
    breaks = policy_levels,
    labels = policy_labels,
    name = NULL,
    drop = FALSE
  ) +
  scale_shape_manual(
    values = policy_shapes,
    breaks = policy_levels,
    labels = policy_labels,
    name = NULL,
    drop = FALSE
  ) +
  scale_linetype_manual(
    values = policy_linetypes,
    breaks = policy_levels,
    labels = policy_labels,
    name = NULL,
    drop = FALSE
  ) +
  scale_x_continuous(
    breaks = years,
    limits = range(years),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    labels = label_number(accuracy = 1),
    expand = expansion(mult = c(0.08, 0.10))
  ) +
  labs(x = NULL, y = "Population below US$3.00/day (million)") +
  guides(
    colour = guide_legend(nrow = 1),
    shape = guide_legend(nrow = 1),
    linetype = guide_legend(nrow = 1)
  ) +
  theme_nature() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 8.2, face = "bold")
  )

p_b <- ggplot(
  r5_additional_2050,
  aes(x = Policy, y = Additional_million, fill = R5_region)
) +
  geom_hline(yintercept = 0, colour = ink, linewidth = 0.32) +
  geom_col(width = 0.68, colour = "white", linewidth = 0.16) +
  facet_wrap(~SSP, nrow = 1, drop = FALSE) +
  scale_x_discrete(
    limits = policy_levels[-1],
    labels = c(
      "Default" = "Default",
      "10th" = "10k",
      "15th" = "15k",
      "20th" = "20k"
    ),
    drop = FALSE
  ) +
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
    y = "Additional population below US$3.00/day\nrelative to BaU in 2050 (million)"
  ) +
  theme_nature() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 8.2, face = "bold"),
    legend.box = "horizontal"
  )

p <- (
  p_a / p_b +
    plot_layout(heights = c(0.92, 1.08)) +
    plot_annotation(tag_levels = "a")
) &
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    plot.tag = element_text(
      family = "Arial", face = "bold", size = 8.5, colour = ink
    ),
    plot.tag.position = "topleft"
  )

output_dir <- file.path(project_root, "output", "Figure", "analysis")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_file <- file.path(
  output_dir, "08_sensitivity_poverty_population_SSP1_SSP3.png"
)

ggsave(
  filename = output_file,
  plot = p,
  device = "png",
  type = "cairo",
  width = 180,
  height = 158,
  units = "mm",
  dpi = 400,
  bg = "white",
  limitsize = FALSE
)

message("Created: ", output_file)
message("Maximum R5-World poverty gap (people): ", maximum_r5_gap)
message(
  "Maximum R5-World additional-poverty gap (million people): ",
  maximum_additional_gap
)
