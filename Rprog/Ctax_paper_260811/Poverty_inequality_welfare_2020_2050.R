# Poverty, between-region inequality, and welfare: BaU, Def, and Aid ---------
#
# Creates one six-panel, Nature Climate Change-style PNG covering 2020-2050:
#   a  World population below US$3.00 per person per day
#   b  Additional poverty relative to BaU, decomposed by R5 region
#   c  Population-weighted Gini of AIM17 mean per-capita consumption
#   d  Population-weighted mean consumption
#   e  Gini-adjusted welfare index (BaU = 100 in each year)
#   f  Exact Shapley decomposition of the 2050 welfare-index change
#
# Only a PNG is written to output/Figure/analysis.

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

iamc_gdx <- file.path(project_root, "data", "AR6database", "global_17_IAMC.gdx")
poverty_gdx <- file.path(
  project_root, "data", "AR6database", "AnalysisExpenditure.gdx"
)
output_dir <- file.path(project_root, "output", "Figure", "analysis")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_file <- file.path(
  output_dir, "04_poverty_inequality_welfare_2020_2050.png"
)

# Settings -------------------------------------------------------------------

scenario_codes <- c(
  BaU = "SSP2_BaU_NoCC_No",
  Def = "SSP2_400C_2030CP_NoCC_No",
  Aid = "SSP2_400C_2030CP_15th_NoCC_No"
)
scenario_levels <- names(scenario_codes)
years <- c(2020, 2030, 2040, 2050)
policy_years <- c(2030, 2040, 2050)
decomposition_year <- 2050

aim17 <- c(
  "CHN", "XE25", "IND", "JPN", "TUR", "XSE", "CIS", "USA", "XLM",
  "BRA", "CAN", "XOC", "XER", "XSA", "XME", "XNF", "XAF"
)
r5_codes <- c("R5OECD90+EU", "R5REF", "R5ASIA", "R5MAF", "R5LAM")
r5_labels <- c(
  "R5OECD90+EU" = "OECD90 + EU",
  "R5REF" = "Reforming economies",
  "R5ASIA" = "Asia",
  "R5MAF" = "Middle East & Africa",
  "R5LAM" = "Latin America"
)

scenario_colours <- c(BaU = "#252525", Def = "#B6573F", Aid = "#3F6C8E")
scenario_shapes <- c(BaU = 16, Def = 1, Aid = 15)
scenario_linetypes <- c(BaU = "solid", Def = "22", Aid = "solid")
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

weighted_gini <- function(consumption_pc, population) {
  valid <- is.finite(consumption_pc) & consumption_pc >= 0 &
    is.finite(population) & population > 0
  consumption_pc <- consumption_pc[valid]
  population <- population[valid]
  weights <- population / sum(population)
  ordering <- order(consumption_pc)
  consumption_pc <- consumption_pc[ordering]
  weights <- weights[ordering]
  cumulative_population <- c(0, cumsum(weights))
  consumption_share <-
    weights * consumption_pc / sum(weights * consumption_pc)
  cumulative_consumption <- c(0, cumsum(consumption_share))
  1 - sum(
    diff(cumulative_population) *
      (head(cumulative_consumption, -1) + tail(cumulative_consumption, -1))
  )
}

# Poverty data ---------------------------------------------------------------

poverty_raw <- rgdx.param(poverty_gdx, "PoVExp")
poverty <- poverty_raw %>%
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
    Year %in% years,
    Threshold == "pop_3.00"
  ) %>%
  mutate(
    Scenario = names(scenario_codes)[match(Scenario_id, scenario_codes)]
  )
rm(poverty_raw)

expected_poverty <- expand_grid(
  Scenario_id = unname(scenario_codes),
  Region_code = c("WLD", r5_codes),
  Year = years
)
check_grid(
  poverty,
  expected_poverty,
  c("Scenario_id", "Region_code", "Year"),
  "US$3.00 poverty data"
)
if (any(!is.finite(poverty$People) | poverty$People < 0)) {
  stop("Poverty headcounts must be finite and non-negative.")
}

world_poverty <- poverty %>%
  filter(Region_code == "WLD") %>%
  transmute(
    Scenario = factor(Scenario, levels = scenario_levels),
    Year,
    Population_million = People / 1e6
  )

r5_poverty <- poverty %>%
  filter(Region_code %in% r5_codes) %>%
  mutate(
    R5_region = factor(
      unname(r5_labels[Region_code]),
      levels = unname(r5_labels[r5_codes])
    )
  )

# Confirm that the R5 decomposition reproduces the world total.
r5_world_check <- r5_poverty %>%
  group_by(Scenario_id, Year) %>%
  summarise(R5_total = sum(People), .groups = "drop") %>%
  left_join(
    poverty %>%
      filter(Region_code == "WLD") %>%
      select(Scenario_id, Year, World_total = People),
    by = c("Scenario_id", "Year")
  )
if (max(abs(r5_world_check$R5_total - r5_world_check$World_total)) > 1) {
  stop("R5 poverty totals do not reproduce the world poverty headcount.")
}

r5_baseline <- r5_poverty %>%
  filter(Scenario == "BaU") %>%
  select(Region_code, Year, BaU_people = People)
r5_additional <- r5_poverty %>%
  left_join(r5_baseline, by = c("Region_code", "Year")) %>%
  mutate(
    Additional_million = (People - BaU_people) / 1e6,
    Scenario = factor(Scenario, levels = c("Def", "Aid")),
    Year_factor = factor(Year, levels = policy_years)
  ) %>%
  filter(!is.na(Scenario), Year %in% policy_years)

# AIM17 consumption, inequality, and welfare ---------------------------------

iamc_raw <- rgdx.param(iamc_gdx, "IAMC_template")
regional_long <- iamc_raw %>%
  transmute(
    Scenario_id = as.character(SCENARIO),
    Region = as.character(REMF),
    Variable = as.character(VEMF),
    Year = as.numeric(as.character(YEMF)),
    Value = as.numeric(IAMC_Template)
  ) %>%
  filter(
    Scenario_id %in% unname(scenario_codes),
    Region %in% aim17,
    Variable %in% c("CNS", "Pop"),
    Year %in% years
  ) %>%
  group_by(Scenario_id, Region, Year, Variable) %>%
  summarise(Value = sum(Value), .groups = "drop")
rm(iamc_raw)

expected_regional <- expand_grid(
  Scenario_id = unname(scenario_codes),
  Region = aim17,
  Year = years,
  Variable = c("CNS", "Pop")
)
check_grid(
  regional_long,
  expected_regional,
  c("Scenario_id", "Region", "Year", "Variable"),
  "AIM17 consumption and population data"
)

regional_data <- regional_long %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  transmute(
    Scenario = names(scenario_codes)[match(Scenario_id, scenario_codes)],
    Region,
    Year,
    Consumption = CNS,
    Population = Pop,
    Consumption_pc = CNS / Pop
  )
if (
  any(!is.finite(regional_data$Consumption) | regional_data$Consumption <= 0) ||
    any(!is.finite(regional_data$Population) | regional_data$Population <= 0) ||
    any(!is.finite(regional_data$Consumption_pc) |
          regional_data$Consumption_pc <= 0)
) {
  stop("Consumption and population inputs must be finite and positive.")
}

metrics <- regional_data %>%
  group_by(Scenario, Year) %>%
  summarise(
    Mean_consumption_pc = sum(Consumption) / sum(Population),
    Gini = weighted_gini(Consumption_pc, Population),
    .groups = "drop"
  ) %>%
  mutate(
    Gini_percent = 100 * Gini,
    Gini_adjusted_welfare = Mean_consumption_pc * (1 - Gini),
    Scenario = factor(Scenario, levels = scenario_levels)
  ) %>%
  group_by(Year) %>%
  mutate(
    BaU_welfare = Gini_adjusted_welfare[Scenario == "BaU"],
    Welfare_index = 100 * Gini_adjusted_welfare / BaU_welfare
  ) %>%
  ungroup()

check_grid(
  metrics,
  expand_grid(Scenario = factor(scenario_levels, levels = scenario_levels), Year = years),
  c("Scenario", "Year"),
  "Gini-welfare metrics"
)

# Exact two-factor Shapley decomposition. WI/100 = M * D, where M is the
# mean-consumption ratio and D is the equality ratio (1-G)/(1-G_BaU).
bau_2050 <- metrics %>%
  filter(Year == decomposition_year, Scenario == "BaU")
if (nrow(bau_2050) != 1L) stop("A unique 2050 BaU record is required.")

decomposition <- metrics %>%
  filter(Year == decomposition_year, Scenario %in% c("Def", "Aid")) %>%
  mutate(
    Mean_ratio = Mean_consumption_pc / bau_2050$Mean_consumption_pc,
    Equality_ratio = (1 - Gini) / (1 - bau_2050$Gini),
    `Mean consumption` =
      100 * (Mean_ratio - 1) * (1 + Equality_ratio) / 2,
    `Between-region distribution` =
      100 * (Equality_ratio - 1) * (1 + Mean_ratio) / 2,
    Net = `Mean consumption` + `Between-region distribution`,
    Reported_net = Welfare_index - 100
  )
decomposition_error <- max(abs(decomposition$Net - decomposition$Reported_net))
if (!is.finite(decomposition_error) || decomposition_error > 1e-8) {
  stop("The exact decomposition does not reproduce the welfare index.")
}

decomposition_long <- decomposition %>%
  select(Scenario, `Mean consumption`, `Between-region distribution`) %>%
  pivot_longer(
    c(`Mean consumption`, `Between-region distribution`),
    names_to = "Effect",
    values_to = "Contribution"
  ) %>%
  mutate(
    Scenario = factor(as.character(Scenario), levels = c("Aid", "Def")),
    Effect = factor(
      Effect,
      levels = c("Mean consumption", "Between-region distribution")
    )
  )
net_data <- decomposition %>%
  transmute(
    Scenario = factor(as.character(Scenario), levels = c("Aid", "Def")),
    Net,
    Label = sprintf("%+.1f", Net),
    Hjust = ifelse(Net >= 0, -0.18, 1.18)
  )

# Figure style ----------------------------------------------------------------

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
      panel.grid = element_blank(),
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
      legend.key.width = grid::unit(5.5, "mm"),
      legend.spacing.x = grid::unit(0.8, "mm"),
      legend.margin = margin(1, 0, 0, 0, unit = "mm"),
      plot.margin = margin(4, 5, 3, 4, unit = "mm")
    )
}

scenario_scales <- list(
  scale_colour_manual(
    values = scenario_colours,
    breaks = scenario_levels,
    drop = FALSE,
    guide = guide_legend(title = NULL, nrow = 1, order = 1)
  ),
  scale_shape_manual(
    values = scenario_shapes,
    breaks = scenario_levels,
    drop = FALSE,
    guide = guide_legend(title = NULL, nrow = 1, order = 1)
  ),
  scale_linetype_manual(
    values = scenario_linetypes,
    breaks = scenario_levels,
    drop = FALSE,
    guide = guide_legend(title = NULL, nrow = 1, order = 1)
  )
)

trajectory_panel <- function(
    data,
    y,
    y_name,
    reference = NULL,
    legend = FALSE,
    y_accuracy = 0.1) {
  plot <- ggplot(
    data,
    aes(
      x = Year,
      y = .data[[y]],
      colour = Scenario,
      shape = Scenario,
      linetype = Scenario,
      group = Scenario
    )
  )
  if (!is.null(reference)) {
    plot <- plot + geom_hline(
      yintercept = reference,
      linetype = "22",
      colour = "#8A8A8A",
      linewidth = 0.28
    )
  }
  plot +
    geom_line(linewidth = 0.62) +
    geom_point(size = 1.85, stroke = 0.52) +
    scenario_scales +
    scale_x_continuous(
      breaks = years,
      limits = c(2019, 2051),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(
      labels = label_number(accuracy = y_accuracy),
      expand = expansion(mult = c(0.10, 0.12))
    ) +
    labs(x = NULL, y = y_name) +
    theme_nature() +
    theme(legend.position = if (legend) "bottom" else "none")
}

# Panels ----------------------------------------------------------------------

p_a <- trajectory_panel(
  world_poverty,
  "Population_million",
  "Population below US$3.00 per day (million)",
  legend = TRUE,
  y_accuracy = 1
)

p_b <- ggplot(
  r5_additional,
  aes(x = Year_factor, y = Additional_million, fill = R5_region)
) +
  geom_hline(yintercept = 0, colour = ink, linewidth = 0.32) +
  geom_col(width = 0.70, colour = "white", linewidth = 0.16) +
  facet_wrap(~Scenario, nrow = 1, drop = FALSE) +
  scale_fill_manual(
    values = r5_colours,
    drop = FALSE,
    guide = guide_legend(title = "R5 region", nrow = 2, byrow = TRUE)
  ) +
  scale_y_continuous(
    labels = label_number(accuracy = 1),
    expand = expansion(mult = c(0.08, 0.12))
  ) +
  labs(x = NULL, y = "Additional population relative to BaU (million)") +
  theme_nature() +
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    strip.text = element_text(size = 7.4, face = "bold", margin = margin(b = 1.5))
  )

p_c <- trajectory_panel(
  metrics,
  "Gini_percent",
  "Population-weighted Gini (%)"
)

p_d <- trajectory_panel(
  metrics,
  "Mean_consumption_pc",
  expression(atop(
    "Mean consumption",
    paste("(10"^3, " US$2010 person"^{-1}, " yr"^{-1}, ")")
  ))
)

p_e <- trajectory_panel(
  metrics,
  "Welfare_index",
  "Welfare Index (BaU=100)",
  reference = 100
)

effect_colours <- c(
  "Mean consumption" = "#6B5B95",
  "Between-region distribution" = "#2A9D8F"
)
p_f <- ggplot(
  decomposition_long,
  aes(y = Scenario, x = Contribution, fill = Effect)
) +
  geom_vline(xintercept = 0, colour = ink, linewidth = 0.32) +
  geom_col(width = 0.56, colour = "white", linewidth = 0.16) +
  geom_point(
    data = net_data,
    aes(y = Scenario, x = Net, shape = "Net change"),
    inherit.aes = FALSE,
    size = 2.25,
    stroke = 0.60,
    fill = "white",
    colour = ink
  ) +
  geom_text(
    data = net_data,
    aes(y = Scenario, x = Net, label = Label, hjust = Hjust),
    inherit.aes = FALSE,
    family = "Arial",
    size = 2.10,
    colour = ink
  ) +
  scale_fill_manual(values = effect_colours, drop = FALSE) +
  scale_shape_manual(values = c("Net change" = 23)) +
  scale_x_continuous(
    labels = label_number(accuracy = 1),
    expand = expansion(mult = c(0.10, 0.14))
  ) +
  scale_y_discrete(drop = FALSE) +
  labs(
    x = "Decomposition of WI change in 2050 relative to BaU (%)",
    y = NULL,
    fill = NULL,
    shape = NULL
  ) +
  guides(
    fill = guide_legend(order = 1, nrow = 1),
    shape = guide_legend(order = 2, nrow = 1)
  ) +
  theme_nature() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    axis.text.y = element_text(face = "bold")
  )

layout_design <- "
AABBBB
CCDDEE
FFFFFF
"

figure <- (
  wrap_plots(
    A = p_a,
    B = p_b,
    C = p_c,
    D = p_d,
    E = p_e,
    F = p_f,
    design = layout_design,
    heights = c(1.05, 0.92, 0.78)
  ) +
    plot_annotation(tag_levels = "a")
) &
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    plot.tag = element_text(
      family = "Arial", face = "bold", size = 8.5, colour = ink
    ),
    plot.tag.position = "topleft"
  )

ggsave(
  filename = output_file,
  plot = figure,
  device = "png",
  type = "cairo",
  width = 180,
  height = 202,
  units = "mm",
  dpi = 400,
  bg = "white",
  limitsize = FALSE
)

message("Created: ", output_file)
message(
  "Maximum R5-world poverty gap (people): ",
  format(max(abs(r5_world_check$R5_total - r5_world_check$World_total)), digits = 4)
)
message(
  "Maximum welfare decomposition error: ",
  format(decomposition_error, scientific = TRUE)
)
print(
  metrics %>%
    filter(Year == 2050) %>%
    select(Scenario, Mean_consumption_pc, Gini_percent, Welfare_index)
)
