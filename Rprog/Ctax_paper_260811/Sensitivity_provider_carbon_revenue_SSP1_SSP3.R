# Sensitivity of revenue, emissions, and carbon price to the income threshold -
#
# The 10th, 15th, and 20th cases classify provider and recipient regions using
# 2025 GDP per capita thresholds of 10,000, 15,000, and 20,000 USD,
# respectively. Rev_gov_Tax_Car_Tax for the matching Rprovider* aggregate is
# the carbon-tax revenue transferred from provider regions.
#
# SSP3 10th and 15th are treated as unavailable for the entire time horizon.
# Their values in IAMC_template are deliberately replaced with NA before any
# of the three panels are plotted.

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

scenario_grid <- expand.grid(
  SSP = paste0("SSP", 1:3),
  Policy = c("10th", "15th", "20th"),
  stringsAsFactors = FALSE
) %>%
  mutate(
    SCENARIO = paste0(SSP, "_400C_2030CP_", Policy, "_NoCC_No"),
    REMF = paste0("Rprovider", Policy)
  )

bau_grid <- data.frame(
  SSP = paste0("SSP", 1:3),
  Policy = "BaU",
  SCENARIO = paste0("SSP", 1:3, "_BaU_NoCC_No"),
  stringsAsFactors = FALSE
)

default_grid <- data.frame(
  SSP = paste0("SSP", 1:3),
  Policy = "Default",
  SCENARIO = paste0("SSP", 1:3, "_400C_2030CP_NoCC_No"),
  stringsAsFactors = FALSE
)

policy_grid <- bind_rows(
  scenario_grid %>% select(SSP, Policy, SCENARIO),
  bau_grid,
  default_grid
)

gdx_file <- file.path(
  project_root, "data", "AR6database", "global_17_IAMC.gdx"
)
message("Reading IAMC_template from: ", gdx_file)
raw <- gdxrrw::rgdx.param(gdx_file, "IAMC_template")

required_columns <- c("SCENARIO", "REMF", "VEMF", "YEMF", "IAMC_Template")
if (!all(required_columns %in% names(raw))) {
  stop("IAMC_template does not have the expected columns.")
}

model_years <- seq(2030, 2050, by = 5)
iamc <- raw %>%
  transmute(
    SCENARIO = as.character(SCENARIO),
    REMF = as.character(REMF),
    VEMF = as.character(VEMF),
    Year = as.numeric(as.character(YEMF)),
    Value = as.numeric(IAMC_Template)
  )

revenue_data <- iamc %>%
  inner_join(scenario_grid, by = c("SCENARIO", "REMF")) %>%
  filter(
    VEMF == "Rev_gov_Tax_Car_Tax",
    Year %in% model_years
  ) %>%
  select(SSP, Policy, SCENARIO, REMF, Year, Value) %>%
  arrange(SSP, Policy, Year)

expected_rows <- nrow(scenario_grid) * length(model_years)
if (nrow(revenue_data) != expected_rows) {
  stop(
    "Expected ", expected_rows, " provider-revenue observations; found ",
    nrow(revenue_data), "."
  )
}
if (anyDuplicated(revenue_data[c("SCENARIO", "REMF", "Year")])) {
  stop("Duplicate provider-revenue observations were found.")
}
if (any(!is.finite(revenue_data$Value))) {
  stop("Provider-revenue observations contain non-finite values.")
}

# Existing project scripts treat Rev_gov_Tax_Car_Tax as million USD_2010/yr.
# Dividing by 1e6 expresses the series in trillion USD_2010/yr.
revenue_data <- revenue_data %>%
  mutate(
    Revenue_trillion_USD2010 = Value / 1e6,
    Revenue_trillion_USD2010 = if_else(
      SSP == "SSP3" & Policy %in% c("10th", "15th"),
      NA_real_,
      Revenue_trillion_USD2010
    )
  )

excluded_rows <- revenue_data %>%
  filter(SSP == "SSP3", Policy %in% c("10th", "15th"))
if (nrow(excluded_rows) != 10L ||
    any(!is.na(excluded_rows$Revenue_trillion_USD2010))) {
  stop("The full SSP3 10th/15th revenue exclusion was not applied correctly.")
}

# World emissions and carbon price for BaU, no-aid Default, and all aid cases.
# Carbon prices are not stored for BaU, for which the policy price is zero by
# definition, so explicit zero-valued BaU rows are added below.
climate_data <- iamc %>%
  inner_join(policy_grid, by = "SCENARIO") %>%
  filter(
    REMF == "World",
    VEMF %in% c("Emi_CO2", "Prc_Car"),
    Year %in% model_years
  ) %>%
  select(SSP, Policy, SCENARIO, VEMF, Year, Value)

emissions_data <- climate_data %>%
  filter(VEMF == "Emi_CO2")
expected_climate_rows <- nrow(policy_grid) * length(model_years)
if (nrow(emissions_data) != expected_climate_rows ||
    anyDuplicated(emissions_data[c("SCENARIO", "Year")])) {
  stop("World CO2 emissions are not complete and unique for all cases.")
}

price_mitigation <- climate_data %>%
  filter(VEMF == "Prc_Car", Policy != "BaU")
expected_price_rows <- (nrow(policy_grid) - nrow(bau_grid)) *
  length(model_years)
if (nrow(price_mitigation) != expected_price_rows ||
    anyDuplicated(price_mitigation[c("SCENARIO", "Year")])) {
  stop("World carbon prices are not complete and unique for mitigation cases.")
}

price_bau <- expand.grid(
  SSP = paste0("SSP", 1:3),
  Year = model_years,
  stringsAsFactors = FALSE
) %>%
  mutate(
    Policy = "BaU",
    SCENARIO = paste0(SSP, "_BaU_NoCC_No"),
    VEMF = "Prc_Car",
    Value = 0
  ) %>%
  select(SSP, Policy, SCENARIO, VEMF, Year, Value)

price_data <- bind_rows(price_mitigation, price_bau)
if (nrow(price_data) != expected_climate_rows ||
    anyDuplicated(price_data[c("SCENARIO", "Year")])) {
  stop("World carbon-price data are not complete and unique after adding BaU.")
}

emissions_data <- emissions_data %>%
  mutate(
    Emissions_GtCO2 = Value / 1000,
    Emissions_GtCO2 = if_else(
      SSP == "SSP3" & Policy %in% c("10th", "15th"),
      NA_real_,
      Emissions_GtCO2
    )
  )

price_data <- price_data %>%
  mutate(
    Carbon_price_USD2010 = if_else(
      SSP == "SSP3" & Policy %in% c("10th", "15th"),
      NA_real_,
      Value
    )
  )

for (data_name in c("revenue_data", "emissions_data", "price_data")) {
  data_object <- get(data_name)
  data_object <- data_object %>%
    mutate(
      SSP = factor(SSP, levels = paste0("SSP", 1:3)),
      Policy = factor(
        Policy,
        levels = c("BaU", "Default", "10th", "15th", "20th")
      )
    )
  assign(data_name, data_object)
}

for (value_column in c(
  "Revenue_trillion_USD2010", "Emissions_GtCO2", "Carbon_price_USD2010"
)) {
  data_object <- switch(
    value_column,
    Revenue_trillion_USD2010 = revenue_data,
    Emissions_GtCO2 = emissions_data,
    Carbon_price_USD2010 = price_data
  )
  excluded <- data_object %>%
    filter(SSP == "SSP3", Policy %in% c("10th", "15th"))
  if (nrow(excluded) != 10L || any(!is.na(excluded[[value_column]]))) {
    stop("The SSP3 10th/15th exclusion failed for ", value_column, ".")
  }
}

policy_colours <- c(
  "BaU" = "#4D4D4D",
  "Default" = "#8C6BB1",
  "10th" = "#0072B2",
  "15th" = "#D55E00",
  "20th" = "#009E73"
)
policy_labels <- c(
  "BaU" = "BaU",
  "Default" = "Default",
  "10th" = "10k USD",
  "15th" = "15k USD",
  "20th" = "20k USD"
)
policy_shapes <- c(
  "BaU" = 1, "Default" = 18, "10th" = 16, "15th" = 17, "20th" = 15
)
policy_linetypes <- c(
  "BaU" = "22", "Default" = "longdash", "10th" = "solid",
  "15th" = "solid", "20th" = "solid"
)
ink <- "#252525"

common_scales <- list(
  scale_colour_manual(
    values = policy_colours,
    breaks = names(policy_colours),
    labels = policy_labels,
    drop = FALSE,
    name = "Scenario"
  ),
  scale_shape_manual(
    values = policy_shapes,
    breaks = names(policy_shapes),
    labels = policy_labels,
    drop = FALSE,
    name = "Scenario"
  ),
  scale_linetype_manual(
    values = policy_linetypes,
    breaks = names(policy_linetypes),
    labels = policy_labels,
    drop = FALSE,
    name = "Scenario"
  ),
  scale_x_continuous(
    breaks = c(2030, 2040, 2050),
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
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = 8.2),
      panel.spacing.x = grid::unit(3, "mm"),
      legend.position = "bottom",
      legend.justification = "center",
      legend.title = element_text(size = 7.4),
      legend.text = element_text(size = 7.4),
      legend.key.width = grid::unit(7, "mm"),
      plot.margin = margin(3, 6, 3, 4)
    )
}

p_revenue <- ggplot(
  revenue_data,
  aes(
    x = Year,
    y = Revenue_trillion_USD2010,
    colour = Policy,
    shape = Policy,
    linetype = Policy,
    group = Policy
  )
) +
  geom_line(linewidth = 0.72, na.rm = TRUE) +
  geom_point(size = 2.0, stroke = 0.55, na.rm = TRUE) +
  facet_wrap(~SSP, nrow = 1) +
  common_scales +
  scale_y_continuous(
    name = expression(
      paste("Climate-finance transfer (trillion US$"[2010], "/yr)")
    ),
    labels = label_number(accuracy = 0.1),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(x = NULL) +
  theme_sensitivity() +
  theme(legend.position = "none")

p_emissions <- ggplot(
  emissions_data,
  aes(
    x = Year,
    y = Emissions_GtCO2,
    colour = Policy,
    shape = Policy,
    linetype = Policy,
    group = Policy
  )
) +
  geom_line(linewidth = 0.72, na.rm = TRUE) +
  geom_point(size = 2.0, stroke = 0.55, na.rm = TRUE) +
  facet_wrap(~SSP, nrow = 1) +
  common_scales +
  scale_y_continuous(
    name = expression(paste("Global CO"[2], " emissions (Gt CO"[2], "/yr)")),
    labels = label_number(accuracy = 1),
    expand = expansion(mult = c(0.06, 0.08))
  ) +
  labs(x = NULL) +
  theme_sensitivity()

p_price <- ggplot(
  price_data,
  aes(
    x = Year,
    y = Carbon_price_USD2010,
    colour = Policy,
    shape = Policy,
    linetype = Policy,
    group = Policy
  )
) +
  geom_line(linewidth = 0.72, na.rm = TRUE) +
  geom_point(size = 2.0, stroke = 0.55, na.rm = TRUE) +
  facet_wrap(~SSP, nrow = 1) +
  common_scales +
  scale_y_continuous(
    name = expression(paste("Global carbon price (US$"[2010], "/t CO"[2], ")")),
    labels = label_number(accuracy = 1),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(x = NULL) +
  theme_sensitivity() +
  theme(legend.position = "none")

p <- (
  p_emissions / p_price / p_revenue / guide_area() +
    plot_layout(guides = "collect", heights = c(1, 1, 1, 0.12)) +
    plot_annotation(tag_levels = "a")
) &
  theme(
    plot.tag = element_text(
      family = "Arial", face = "bold", size = 9.0, colour = ink
    ),
    plot.tag.position = "topleft"
  )

output_dir <- file.path(project_root, "output", "Figure", "analysis")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_file <- file.path(
  output_dir, "04_sensitivity_provider_carbon_revenue_SSP1_SSP3.png"
)

ggsave(
  filename = output_file,
  plot = p,
  device = "png",
  type = "cairo",
  width = 180,
  height = 190,
  units = "mm",
  dpi = 400,
  bg = "white"
)

message("Created: ", output_file)
message("Revenue values plotted (trillion USD_2010/yr; NA values are blank):")
print(
  revenue_data %>%
    select(SSP, Policy, Year, Revenue_trillion_USD2010),
  row.names = FALSE,
  digits = 4
)
