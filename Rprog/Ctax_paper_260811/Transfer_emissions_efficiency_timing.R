# Climate-finance transfers: emissions, efficiency, and timing ----------------
#
# This script creates three publication-ready PNG figures in
# output/Figure/analysis. It intentionally writes no CSV, PDF, or SVG files.
#
# Definitions
#   Def: SSP2 climate policy without international transfers
#   Aid: SSP2 climate policy with the 15th-percentile transfer rule
#   Transfer pool: carbon-tax revenue collected in provider regions under Aid
#   Transfer-equivalent quantity: transfer pool / Aid world carbon price
#   Transfer efficiency: developing-region consumption gain / transfer received

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
  library(patchwork)
})

scenario_def <- "SSP2_400C_2030CP_NoCC_No"
scenario_aid <- "SSP2_400C_2030CP_15th_NoCC_No"
scenarios_needed <- c(scenario_def, scenario_aid)

providers <- c("XE25", "JPN", "TUR", "CHN", "USA",
               "XER", "XOC", "CAN", "XLM", "CIS")
recipients <- c("XSA", "IND", "XNF", "XAF", "XSE", "BRA", "XME")
provider_aggregate <- "Rprovider15th"
recipient_aggregate <- "Rrecipient15th"

variables_needed <- c(
  "Emi_CO2", "CNS", "GDP_MER", "Prc_Car", "Pol_Cos_Cns_Los",
  "Pol_Cos_Cns_Los_rat", "Pol_Cos_Cns_Los_rat_NPV_5pc",
  "Pol_Cos_GDP_Los_rat_NPV_5pc",
  "Rev_gov_Tax_Car_Tax"
)

gdx_file <- file.path(project_root, "data", "AR6database", "global_17_IAMC.gdx")
output_dir <- file.path(project_root, "output", "Figure", "analysis")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
legacy_efficiency_png <- file.path(output_dir, "02_transfer_efficiency_2050.png")
if (file.exists(legacy_efficiency_png)) unlink(legacy_efficiency_png)
legacy_emissions_png <- file.path(output_dir, "01_transfer_emissions_equivalent_2050.png")
if (file.exists(legacy_emissions_png)) unlink(legacy_emissions_png)

# gdxrrw is preferred because it is also used elsewhere in this project. The
# gdxdump fallback keeps the script runnable with current R versions for which
# an older binary build of gdxrrw may not load.
read_iamc_gdxrrw <- function(path) {
  if (!requireNamespace("gdxrrw", quietly = TRUE)) return(NULL)

  gams_candidates <- c(
    Sys.getenv("GAMSDIR", unset = ""),
    "C:/GAMS/34",
    "C:/GAMS/win64/26.1"
  )
  gams_candidates <- unique(gams_candidates[nzchar(gams_candidates)])
  for (candidate in gams_candidates) {
    if (dir.exists(candidate)) {
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
  }
  NULL
}

find_gdxdump <- function() {
  from_path <- unname(Sys.which(c("gdxdump", "gdxdump.exe")))
  from_path <- from_path[nzchar(from_path)]
  candidates <- c(
    from_path,
    "C:/GAMS/34/gdxdump.exe",
    "C:/GAMS/win64/26.1/gdxdump.exe"
  )
  candidates <- unique(candidates)
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) return(NA_character_)
  normalizePath(hit[[1]], winslash = "/")
}

read_iamc_gdxdump <- function(path, scenarios, variables) {
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
  invisible(readLines(con, n = 1L, warn = FALSE)) # CSV header

  pieces <- list()
  piece_id <- 0L
  repeat {
    lines <- readLines(con, n = 200000L, warn = FALSE)
    if (!length(lines)) break

    scenario_hit <- rep(FALSE, length(lines))
    for (scenario in scenarios) {
      scenario_hit <- scenario_hit |
        startsWith(lines, paste0('"', scenario, '",'))
    }

    variable_hit <- rep(FALSE, length(lines))
    for (variable in variables) {
      variable_hit <- variable_hit |
        grepl(paste0(',"', variable, '",'), lines, fixed = TRUE)
    }

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

message("Reading IAMC_template from: ", gdx_file)
iamc <- read_iamc_gdxrrw(gdx_file)
if (is.null(iamc)) {
  message("A working gdxrrw installation was not found; using gdxdump fallback.")
  iamc <- read_iamc_gdxdump(gdx_file, scenarios_needed, variables_needed)
}

iamc <- iamc[
  iamc$SCENARIO %in% scenarios_needed &
    iamc$VEMF %in% variables_needed &
    is.finite(iamc$Year) & is.finite(iamc$Value),
  c("SCENARIO", "REMF", "VEMF", "Year", "Value")
]

one_value <- function(variable, scenario, region, year) {
  values <- iamc$Value[
    iamc$VEMF == variable &
      iamc$SCENARIO == scenario &
      iamc$REMF == region &
      iamc$Year == year
  ]
  if (length(values) != 1L || !is.finite(values)) {
    stop(
      "Expected one finite value for ", variable, ", ", scenario,
      ", ", region, ", ", year, "; found ", length(values), "."
    )
  }
  values
}

sum_values <- function(variable, scenario, regions, year) {
  rows <- iamc[
    iamc$VEMF == variable &
      iamc$SCENARIO == scenario &
      iamc$REMF %in% regions &
      iamc$Year == year,
  ]
  if (nrow(rows) != length(regions) || any(!is.finite(rows$Value))) {
    stop(
      "Expected one finite ", variable, " value for each of ",
      length(regions), " regions in ", year, "."
    )
  }
  sum(rows$Value)
}

# Shared quantities -----------------------------------------------------------
transfer_rows <- iamc[
  iamc$VEMF == "Rev_gov_Tax_Car_Tax" &
    iamc$SCENARIO == scenario_aid &
    iamc$REMF %in% providers &
    iamc$Year >= 2030 & iamc$Year <= 2050,
]
if (!nrow(transfer_rows)) stop("No provider carbon-tax revenue records found.")

transfer_annual <- aggregate(Value ~ Year, data = transfer_rows, FUN = sum)
names(transfer_annual)[names(transfer_annual) == "Value"] <- "Transfer_million_USD2010"
transfer_annual <- transfer_annual[order(transfer_annual$Year),]

model_years <- seq(2030, 2050, by = 5)
if (!all(model_years %in% transfer_annual$Year)) {
  stop("The transfer series does not contain every five-year model point, 2030-2050.")
}

transfer_2050_musd <- transfer_annual$Transfer_million_USD2010[
  transfer_annual$Year == 2050
]
if (length(transfer_2050_musd) != 1L || transfer_2050_musd <= 0) {
  stop("The 2050 transfer pool must be a single positive value.")
}

world_carbon_price_2050 <- one_value(
  "Prc_Car", scenario_aid, "World", 2050
)
if (world_carbon_price_2050 <= 0) stop("The 2050 world carbon price must be positive.")

# The generic VUMAP label for Rev_gov_Tax_Car_Tax is ambiguous ("billion
# USD_2010/yr OR local currency"). In the AIM output used by this project, the
# raw values are treated as million USD_2010/yr throughout the existing scripts
# (e.g., division by 1e6 to obtain trillion USD). The GDP ratio below provides
# an additional scale check: treating the raw values as billion USD would imply
# an economically impossible revenue total.
provider_gdp_2050_busd <- sum(iamc$Value[
  iamc$VEMF == "GDP_MER" &
    iamc$SCENARIO == scenario_aid &
    iamc$REMF %in% providers &
    iamc$Year == 2050
])
transfer_share_provider_gdp <-
  100 * (transfer_2050_musd / 1000) / provider_gdp_2050_busd
if (!is.finite(transfer_share_provider_gdp) || transfer_share_provider_gdp > 100) {
  stop("The assumed carbon-revenue unit is inconsistent with provider GDP.")
}

# Order-of-magnitude unit check using the requested physical calculation.
# Mt CO2/yr * USD_2010/t CO2 = million USD_2010/yr. This is only a proxy for
# revenue because Emi_CO2 is net CO2, whereas the tax base can include gross
# emissions, additional gases, exemptions, and differentiated regional rates.
provider_emissions_check <- iamc[
  iamc$VEMF == "Emi_CO2" &
    iamc$SCENARIO == scenario_aid &
    iamc$REMF %in% providers &
    iamc$Year == 2050,
  c("REMF", "Value")
]
provider_price_check <- iamc[
  iamc$VEMF == "Prc_Car" &
    iamc$SCENARIO == scenario_aid &
    iamc$REMF %in% providers &
    iamc$Year == 2050,
  c("REMF", "Value")
]
names(provider_emissions_check)[2] <- "Emissions_MtCO2"
names(provider_price_check)[2] <- "Price_USD_per_tCO2"
tax_base_check <- merge(provider_emissions_check, provider_price_check, by = "REMF")
if (nrow(tax_base_check) != length(providers)) {
  stop("Could not construct the 2050 emissions-times-price unit check.")
}
emissions_price_proxy_musd <- sum(
  tax_base_check$Emissions_MtCO2 * tax_base_check$Price_USD_per_tCO2
)
revenue_to_emissions_price_proxy <-
  transfer_2050_musd / emissions_price_proxy_musd
billion_interpretation_mismatch <-
  (transfer_2050_musd * 1000) / emissions_price_proxy_musd

# Nature-family figure styling: Arial, restrained colour, thin axes, and sizes
# appropriate for a 180-mm double-column figure.
colour_developed <- "#3F6C8E"
colour_developing <- "#B6573F"
colour_transfer <- "#8E8E8E"
ink <- "#252525"
grid_colour <- "#E2E2E2"

theme_nature <- function() {
  theme_classic(base_family = "Arial", base_size = 8) +
    theme(
      text = element_text(colour = ink),
      axis.line = element_line(linewidth = 0.30, colour = ink),
      axis.line.x = element_blank(),
      axis.ticks = element_line(linewidth = 0.25, colour = ink),
      axis.ticks.length = grid::unit(1.2, "mm"),
      axis.title = element_text(size = 8.2),
      axis.text = element_text(size = 7.4, colour = ink),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption = element_blank(),
      panel.grid.major.y = element_line(colour = grid_colour, linewidth = 0.22),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.justification = "left",
      legend.box.just = "left",
      legend.title = element_text(size = 7.4, face = "plain"),
      legend.text = element_text(size = 7.4),
      legend.key.height = grid::unit(3.5, "mm"),
      legend.key.width = grid::unit(7, "mm"),
      legend.spacing.y = grid::unit(0.3, "mm"),
      legend.margin = margin(t = 2, unit = "mm"),
      plot.margin = margin(4, 6, 3, 4)
    )
}

save_png <- function(filename, plot, height_mm) {
  ggsave(
    filename = file.path(output_dir, filename),
    plot = plot,
    device = "png",
    type = "cairo",
    width = 180,
    height = height_mm,
    units = "mm",
    dpi = 400,
    bg = "white"
  )
}

# 1. Actual emissions response and transfer-equivalent quantity over time -----
# The two measures share physical units but differ by more than an order of
# magnitude in some years. Separate panels with independent y scales preserve
# the actual emissions signal without visually compressing it.
annual_emissions_data <- do.call(rbind, lapply(model_years, function(year) {
  transfer_musd <- transfer_annual$Transfer_million_USD2010[
    transfer_annual$Year == year
  ]
  world_price <- one_value("Prc_Car", scenario_aid, "World", year)
  if (length(transfer_musd) != 1L || world_price <= 0) {
    stop("Missing transfer or non-positive world carbon price in ", year, ".")
  }

  developed_actual <- (
    one_value("Emi_CO2", scenario_aid, provider_aggregate, year) -
      one_value("Emi_CO2", scenario_def, provider_aggregate, year)
  ) / 1000
  developing_actual <- (
    one_value("Emi_CO2", scenario_aid, recipient_aggregate, year) -
      one_value("Emi_CO2", scenario_def, recipient_aggregate, year)
  ) / 1000

  # million US$ / (US$ per t CO2) = million t CO2; /1,000 = Gt CO2.
  transfer_equivalent <- transfer_musd / world_price / 1000

  data.frame(
    Year = rep(year, 4),
    Region = rep(c("Developed regions", "Developing regions"), 2),
    Measure = rep(
      c("Actual emissions response (Aid - Def)",
        "Transfer-equivalent quantity (funds / carbon price)"),
      each = 2
    ),
    Change_GtCO2 = c(
      developed_actual,
      developing_actual,
      -transfer_equivalent,
      transfer_equivalent
    ),
    World_carbon_price = world_price
  )
}))

annual_emissions_data$Region <- factor(
  annual_emissions_data$Region,
  levels = c("Developed regions", "Developing regions")
)
annual_emissions_data$Measure <- factor(
  annual_emissions_data$Measure,
  levels = c(
    "Actual emissions response (Aid - Def)",
    "Transfer-equivalent quantity (funds / carbon price)"
  )
)

p_emissions <- ggplot(
  annual_emissions_data,
  aes(x = Year, y = Change_GtCO2, colour = Region, group = Region)
) +
  geom_hline(yintercept = 0, linewidth = 0.35, colour = ink) +
  geom_line(linewidth = 0.68) +
  geom_point(shape = 21, fill = "white", stroke = 0.50, size = 1.85) +
  facet_wrap(vars(Measure), ncol = 1, scales = "free_y", strip.position = "top") +
  scale_colour_manual(
    values = c(
      "Developed regions" = colour_developed,
      "Developing regions" = colour_developing
    ),
    guide = guide_legend(title = "Region", nrow = 1)
  ) +
  scale_x_continuous(
    breaks = model_years,
    limits = c(2029.2, 2050.8),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    name = expression("Change in CO"[2] * " emissions (Gt CO"[2] * " " * yr^{-1} * ")"),
    labels = label_number(accuracy = 0.1),
    expand = expansion(mult = c(0.13, 0.13))
  ) +
  labs(x = NULL) +
  theme_nature() +
  theme(
    panel.spacing.y = grid::unit(4, "mm"),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.x = element_text(
      face = "plain",
      size = 7.8,
      hjust = 0,
      margin = margin(1, 0, 2, 0, unit = "mm")
    )
  )

save_png("01_transfer_emissions_equivalent_2030_2050.png", p_emissions, 102)

# Alternative one-panel version on one common physical scale. This deliberately
# leaves the actual response close to zero so that its small magnitude relative
# to the transfer-equivalent quantity is the principal visual result.
equivalent_max <- max(abs(annual_emissions_data$Change_GtCO2[
  annual_emissions_data$Measure ==
    "Transfer-equivalent quantity (funds / carbon price)"
]))
combined_emissions_data <- annual_emissions_data
actual_label_data <- combined_emissions_data[
  combined_emissions_data$Measure == "Actual emissions response (Aid - Def)",
]
actual_label_data$Label_y <- ifelse(
  actual_label_data$Change_GtCO2 >= 0,
  equivalent_max * 0.065,
  -equivalent_max * 0.065
)

p_emissions_combined <- ggplot(
  combined_emissions_data,
  aes(
    x = Year,
    y = Change_GtCO2,
    colour = Region,
    linetype = Measure,
    shape = Measure,
    group = interaction(Region, Measure)
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.35, colour = ink) +
  geom_line(linewidth = 0.68) +
  geom_point(fill = "white", stroke = 0.50, size = 1.85) +
  geom_segment(
    data = actual_label_data,
    aes(xend = Year, yend = Label_y),
    linewidth = 0.22,
    show.legend = FALSE
  ) +
  geom_text(
    data = actual_label_data,
    aes(
      y = Label_y,
      label = sprintf("%+.2f", Change_GtCO2)
    ),
    family = "Arial",
    fontface = "plain",
    size = 2.05,
    vjust = ifelse(actual_label_data$Label_y > 0, -0.35, 1.25),
    show.legend = FALSE
  ) +
  scale_colour_manual(
    values = c(
      "Developed regions" = colour_developed,
      "Developing regions" = colour_developing
    ),
    guide = guide_legend(order = 1, title = "Region", nrow = 1)
  ) +
  scale_linetype_manual(
    values = c(
      "Actual emissions response (Aid - Def)" = "solid",
      "Transfer-equivalent quantity (funds / carbon price)" = "22"
    ),
    guide = guide_legend(order = 2, title = "Measure", nrow = 1)
  ) +
  scale_shape_manual(
    values = c(
      "Actual emissions response (Aid - Def)" = 21,
      "Transfer-equivalent quantity (funds / carbon price)" = 24
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = model_years,
    limits = c(2029.2, 2050.8),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    name = expression("Emission change or equivalent quantity (Gt CO"[2] * " " * yr^{-1} * ")"),
    labels = label_number(accuracy = 1),
    expand = expansion(mult = c(0.10, 0.10))
  ) +
  labs(x = NULL) +
  theme_nature() +
  theme(
    legend.box = "vertical"
  )

save_png(
  "01b_transfer_emissions_equivalent_combined_2030_2050.png",
  p_emissions_combined,
  88
)

# Retain 2050 values for the console diagnostics below.
developed_emissions_change <- annual_emissions_data$Change_GtCO2[
  annual_emissions_data$Year == 2050 &
    annual_emissions_data$Region == "Developed regions" &
    annual_emissions_data$Measure == "Actual emissions response (Aid - Def)"
]
developing_emissions_change <- annual_emissions_data$Change_GtCO2[
  annual_emissions_data$Year == 2050 &
    annual_emissions_data$Region == "Developing regions" &
    annual_emissions_data$Measure == "Actual emissions response (Aid - Def)"
]
allowance_equivalent_gt <- abs(annual_emissions_data$Change_GtCO2[
  annual_emissions_data$Year == 2050 &
    annual_emissions_data$Region == "Developing regions" &
    annual_emissions_data$Measure ==
      "Transfer-equivalent quantity (funds / carbon price)"
])

# 2. Annual transfer efficiency, recipients and providers --------------------
# Pol_Cos_Cns_Los is expressed in billion USD_2010/yr. For recipients, the
# benefit is the reduction in consumption loss (Def - Aid). For providers, the
# burden is the increase in consumption loss (Aid - Def). Both are divided by
# the same annual transfer pool after converting it from million to billion USD.
annual_efficiency <- do.call(rbind, lapply(model_years, function(year) {
  transfer_busd <- transfer_annual$Transfer_million_USD2010[
    transfer_annual$Year == year
  ] / 1000

  recipient_benefit_busd <-
    sum_values("Pol_Cos_Cns_Los", scenario_def, recipients, year) -
    sum_values("Pol_Cos_Cns_Los", scenario_aid, recipients, year)
  provider_burden_busd <-
    sum_values("Pol_Cos_Cns_Los", scenario_aid, providers, year) -
    sum_values("Pol_Cos_Cns_Los", scenario_def, providers, year)

  recipient_cns_gain_busd <-
    sum_values("CNS", scenario_aid, recipients, year) -
    sum_values("CNS", scenario_def, recipients, year)
  provider_cns_loss_busd <-
    sum_values("CNS", scenario_def, providers, year) -
    sum_values("CNS", scenario_aid, providers, year)

  data.frame(
    Year = year,
    Transfer_billion_USD2010 = transfer_busd,
    Recipient_benefit_billion_USD2010 = recipient_benefit_busd,
    Provider_burden_billion_USD2010 = provider_burden_busd,
    Recipient_efficiency = recipient_benefit_busd / transfer_busd,
    Provider_burden_ratio = provider_burden_busd / transfer_busd,
    Recipient_identity_gap = recipient_benefit_busd - recipient_cns_gain_busd,
    Provider_identity_gap = provider_burden_busd - provider_cns_loss_busd
  )
}))

if (any(abs(annual_efficiency$Recipient_identity_gap) > 1e-6) ||
    any(abs(annual_efficiency$Provider_identity_gap) > 1e-6)) {
  stop("Pol_Cos_Cns_Los and CNS imply inconsistent annual consumption effects.")
}
if (any(annual_efficiency$Transfer_billion_USD2010 <= 0)) {
  stop("Annual transfer amounts must be positive to calculate efficiency.")
}

money_plot_data <- rbind(
  data.frame(
    Year = annual_efficiency$Year,
    PlotYear = annual_efficiency$Year - 0.68,
    Amount_trillion_USD2010 =
      annual_efficiency$Recipient_benefit_billion_USD2010 / 1000,
    Series = "Recipient consumption gain"
  ),
  data.frame(
    Year = annual_efficiency$Year,
    PlotYear = annual_efficiency$Year - 0.68,
    Amount_trillion_USD2010 =
      -annual_efficiency$Provider_burden_billion_USD2010 / 1000,
    Series = "Provider consumption loss"
  ),
  data.frame(
    Year = annual_efficiency$Year,
    PlotYear = annual_efficiency$Year + 0.68,
    Amount_trillion_USD2010 =
      annual_efficiency$Transfer_billion_USD2010 / 1000,
    Series = "Transfer received/provided"
  ),
  data.frame(
    Year = annual_efficiency$Year,
    PlotYear = annual_efficiency$Year + 0.68,
    Amount_trillion_USD2010 =
      -annual_efficiency$Transfer_billion_USD2010 / 1000,
    Series = "Transfer received/provided"
  )
)
money_plot_data$Series <- factor(
  money_plot_data$Series,
  levels = c(
    "Recipient consumption gain",
    "Provider consumption loss",
    "Transfer received/provided"
  )
)

ratio_plot_data <- rbind(
  data.frame(
    Year = annual_efficiency$Year,
    Signed_ratio = annual_efficiency$Recipient_efficiency,
    Ratio = "Recipient: gain / transfer"
  ),
  data.frame(
    Year = annual_efficiency$Year,
    Signed_ratio = -annual_efficiency$Provider_burden_ratio,
    Ratio = "Provider: loss / transfer"
  )
)
ratio_plot_data$Ratio <- factor(
  ratio_plot_data$Ratio,
  levels = c("Recipient: gain / transfer", "Provider: loss / transfer")
)

money_max <- max(abs(money_plot_data$Amount_trillion_USD2010))
ratio_max <- max(abs(ratio_plot_data$Signed_ratio))
efficiency_axis_scale <- 0.82 * money_max / ratio_max
ratio_plot_data$Scaled_ratio <-
  ratio_plot_data$Signed_ratio * efficiency_axis_scale
ratio_plot_display <- ratio_plot_data[ratio_plot_data$Year >= 2035, ]

p_efficiency <- ggplot() +
  geom_col(
    data = money_plot_data,
    aes(x = PlotYear, y = Amount_trillion_USD2010, fill = Series),
    width = 1.08,
    colour = NA,
    alpha = 0.88
  ) +
  geom_hline(yintercept = 0, colour = ink, linewidth = 0.35) +
  geom_line(
    data = ratio_plot_display,
    aes(x = Year, y = Scaled_ratio, colour = Ratio, group = Ratio),
    linewidth = 0.68
  ) +
  geom_point(
    data = ratio_plot_display,
    aes(x = Year, y = Scaled_ratio, colour = Ratio),
    shape = 18,
    size = 2.10
  ) +
  geom_text(
    data = ratio_plot_display,
    aes(
      x = Year,
      y = Scaled_ratio,
      label = sprintf("%.1f", abs(Signed_ratio)),
      colour = Ratio,
      vjust = ifelse(Signed_ratio >= 0, -0.95, 1.45)
    ),
    family = "Arial",
    fontface = "plain",
    size = 2.05,
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = c(
      "Recipient consumption gain" = colour_developing,
      "Provider consumption loss" = colour_developed,
      "Transfer received/provided" = colour_transfer
    ),
    breaks = "Transfer received/provided",
    labels = "Transferred money",
    guide = guide_legend(order = 3, title = NULL, nrow = 1)
  ) +
  scale_colour_manual(
    values = c(
      "Recipient: gain / transfer" = colour_developing,
      "Provider: loss / transfer" = colour_developed
    ),
    labels = c(
      "Recipient: gain / transfer" = "Developing",
      "Provider: loss / transfer" = "Developed"
    ),
    guide = guide_legend(order = 1, title = NULL, nrow = 1)
  ) +
  scale_x_continuous(
    breaks = model_years,
    limits = c(2028.2, 2051.8),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    name = "Annual consumption change (trillion US$2010 yr-1)",
    labels = function(x) label_number(accuracy = 0.1)(abs(x)),
    sec.axis = sec_axis(
      ~ . / efficiency_axis_scale,
      name = "Consumption/Transfer",
      labels = function(x) label_number(accuracy = 0.1)(abs(x))
    ),
    expand = expansion(mult = c(0.10, 0.10))
  ) +
  labs(x = NULL) +
  coord_cartesian(clip = "off") +
  theme_nature() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_rect(fill = NA, colour = ink, linewidth = 0.30),
    axis.line = element_blank(),
    legend.box = "vertical",
    axis.title.y.right = element_text(colour = "#4B4B4B"),
    axis.text.y.right = element_text(colour = "#4B4B4B")
  )

save_png("02_transfer_efficiency_2030_2050.png", p_efficiency, 94)

# Retain key 2050 values and aggregate-row sensitivity checks in diagnostics.
developing_loss_reduction_busd <- annual_efficiency$Recipient_benefit_billion_USD2010[
  annual_efficiency$Year == 2050
]
provider_burden_2050_busd <- annual_efficiency$Provider_burden_billion_USD2010[
  annual_efficiency$Year == 2050
]
transfer_2050_busd <- transfer_2050_musd / 1000
transfer_efficiency <- annual_efficiency$Recipient_efficiency[
  annual_efficiency$Year == 2050
]
provider_burden_ratio_2050 <- annual_efficiency$Provider_burden_ratio[
  annual_efficiency$Year == 2050
]
identity_gap_busd <- max(
  abs(annual_efficiency$Recipient_identity_gap),
  abs(annual_efficiency$Provider_identity_gap)
)

aggregate_loss_reduction_busd <-
  one_value("Pol_Cos_Cns_Los", scenario_def, recipient_aggregate, 2050) -
  one_value("Pol_Cos_Cns_Los", scenario_aid, recipient_aggregate, 2050)
aggregate_row_efficiency <- aggregate_loss_reduction_busd / transfer_2050_busd
aggregate_provider_transfer_busd <-
  one_value("Rev_gov_Tax_Car_Tax", scenario_aid, provider_aggregate, 2050) / 1000
aggregate_both_efficiency <-
  aggregate_loss_reduction_busd / aggregate_provider_transfer_busd

# 3. Timing of consumption losses and transfers -------------------------------
loss_data <- iamc[
  iamc$VEMF == "Pol_Cos_Cns_Los_rat" &
    iamc$SCENARIO %in% scenarios_needed &
    iamc$REMF %in% c(provider_aggregate, recipient_aggregate) &
    iamc$Year %in% model_years,
]
if (nrow(loss_data) != length(model_years) * 2L * 2L) {
  stop("Expected 20 consumption-loss observations for 2030-2050.")
}

loss_data$Region <- factor(
  ifelse(loss_data$REMF == provider_aggregate,
         "Developed regions", "Developing regions"),
  levels = c("Developed regions", "Developing regions")
)
loss_data$Policy <- factor(
  ifelse(loss_data$SCENARIO == scenario_def,
         "No transfer (Def)", "Climate finance (Aid)"),
  levels = c("No transfer (Def)", "Climate finance (Aid)")
)
names(loss_data)[names(loss_data) == "Value"] <- "Consumption_loss_pct"
loss_data <- loss_data[order(loss_data$Region, loss_data$Policy, loss_data$Year),]

p_timing <- ggplot(
  loss_data,
  aes(
    x = Year,
    y = Consumption_loss_pct,
    colour = Region,
    linetype = Policy,
    shape = Policy,
    group = interaction(Region, Policy)
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.35, colour = ink) +
  geom_line(linewidth = 0.68) +
  geom_point(stroke = 0.55, size = 1.90) +
  scale_colour_manual(
    values = c(
      "Developed regions" = colour_developed,
      "Developing regions" = colour_developing
    ),
    labels = c(
      "Developed regions" = "Developed",
      "Developing regions" = "Developing"
    ),
    guide = guide_legend(order = 1, title = NULL, nrow = 1)
  ) +
  scale_linetype_manual(
    values = c("No transfer (Def)" = "22", "Climate finance (Aid)" = "solid"),
    labels = c("No transfer (Def)" = "Def", "Climate finance (Aid)" = "Aid"),
    guide = guide_legend(order = 2, title = NULL, nrow = 1)
  ) +
  scale_shape_manual(
    values = c("No transfer (Def)" = 1, "Climate finance (Aid)" = 16),
    labels = c("No transfer (Def)" = "Def", "Climate finance (Aid)" = "Aid"),
    guide = guide_legend(order = 2, title = NULL, nrow = 1)
  ) +
  scale_x_continuous(
    breaks = model_years,
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    name = "Consumption loss rate (%)",
    labels = label_number(accuracy = 1),
    expand = expansion(mult = c(0.08, 0.09))
  ) +
  labs(x = NULL) +
  coord_cartesian(xlim = c(2028.3, 2051.7), clip = "off") +
  theme_nature() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_rect(fill = NA, colour = ink, linewidth = 0.30),
    axis.line = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA)
  )

# 2050 cumulative consumption-loss rates use the model's 5% NPV indicator.
cumulative_region_codes <- c(
  "World", provider_aggregate, recipient_aggregate
)
cumulative_region_labels <- c(
  "World", "Developed regions", "Developing regions"
)
cumulative_loss_data <- iamc[
  iamc$VEMF == "Pol_Cos_Cns_Los_rat_NPV_5pc" &
    iamc$SCENARIO %in% scenarios_needed &
    iamc$REMF %in% cumulative_region_codes &
    iamc$Year == 2050,
]
if (nrow(cumulative_loss_data) != 6L) {
  stop("Expected six 2050 cumulative consumption-loss observations.")
}
if (anyDuplicated(cumulative_loss_data[c("SCENARIO", "REMF", "Year")])) {
  stop("Duplicate 2050 cumulative consumption-loss observations.")
}
cumulative_loss_data$Region <- factor(
  cumulative_region_labels[
    match(cumulative_loss_data$REMF, cumulative_region_codes)
  ],
  levels = cumulative_region_labels
)
cumulative_loss_data$Policy <- factor(
  ifelse(
    cumulative_loss_data$SCENARIO == scenario_def,
    "No transfer (Def)",
    "Climate finance (Aid)"
  ),
  levels = c("No transfer (Def)", "Climate finance (Aid)")
)
names(cumulative_loss_data)[names(cumulative_loss_data) == "Value"] <-
  "Cumulative_loss_pct"

p_cumulative_loss <- ggplot(
  cumulative_loss_data,
  aes(
    x = Region,
    y = Cumulative_loss_pct,
    colour = Region,
    shape = Policy,
    group = Policy
  )
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
    limits = cumulative_region_labels,
    drop = FALSE,
    guide = "none"
  ) +
  scale_shape_manual(
    values = c("No transfer (Def)" = 1, "Climate finance (Aid)" = 16),
    guide = "none"
  ) +
  scale_x_discrete(
    labels = c(
      "World" = "World",
      "Developed regions" = "Developed",
      "Developing regions" = "Developing"
    )
  ) +
  scale_y_continuous(
    name = "Cumulative consumption loss in 2050 (%)",
    labels = label_number(accuracy = 1),
    expand = expansion(mult = c(0.15, 0.18))
  ) +
  labs(x = NULL) +
  theme_nature() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_rect(fill = NA, colour = ink, linewidth = 0.30),
    axis.line = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1),
    legend.position = "none"
  )

# The GDP counterpart uses the model's directly reported 5%-discounted NPV
# loss rate, exactly parallel to the cumulative consumption indicator above.
cumulative_gdp_data <- iamc[
  iamc$VEMF == "Pol_Cos_GDP_Los_rat_NPV_5pc" &
    iamc$SCENARIO %in% scenarios_needed &
    iamc$REMF %in% cumulative_region_codes &
    iamc$Year == 2050,
]
if (nrow(cumulative_gdp_data) != 6L) {
  stop("Expected six 2050 cumulative GDP-loss observations.")
}
if (anyDuplicated(cumulative_gdp_data[c("SCENARIO", "REMF", "Year")])) {
  stop("Duplicate 2050 cumulative GDP-loss observations.")
}
cumulative_gdp_data$Region <- factor(
  cumulative_region_labels[
    match(cumulative_gdp_data$REMF, cumulative_region_codes)
  ],
  levels = cumulative_region_labels
)
cumulative_gdp_data$Policy <- factor(
  ifelse(
    cumulative_gdp_data$SCENARIO == scenario_def,
    "No transfer (Def)",
    "Climate finance (Aid)"
  ),
  levels = c("No transfer (Def)", "Climate finance (Aid)")
)
names(cumulative_gdp_data)[names(cumulative_gdp_data) == "Value"] <-
  "Cumulative_GDP_loss_pct"

p_cumulative_gdp <- ggplot(
  cumulative_gdp_data,
  aes(
    x = Region,
    y = Cumulative_GDP_loss_pct,
    colour = Region,
    shape = Policy,
    group = Policy
  )
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
    limits = cumulative_region_labels,
    drop = FALSE,
    guide = "none"
  ) +
  scale_shape_manual(
    values = c("No transfer (Def)" = 1, "Climate finance (Aid)" = 16),
    guide = "none"
  ) +
  scale_x_discrete(
    labels = c(
      "World" = "World",
      "Developed regions" = "Developed",
      "Developing regions" = "Developing"
    )
  ) +
  scale_y_continuous(
    name = "Cumulative GDP loss in 2050 (%)",
    labels = label_number(accuracy = 1),
    expand = expansion(mult = c(0.15, 0.18))
  ) +
  labs(x = NULL) +
  theme_nature() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_rect(fill = NA, colour = ink, linewidth = 0.30),
    axis.line = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1),
    legend.position = "none"
  )

# 2050 GDP expenditure decomposition ----------------------------------------
# This compact panel follows NatureCC_GDP_decomposition_Def_Aid_2050.png:
# expenditure-component contributions are stacked and the diamond reports the
# net GDP change relative to BaU.
gdp_scenario_paths <- c(
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
missing_gdp_files <- gdp_scenario_paths[!file.exists(gdp_scenario_paths)]
if (length(missing_gdp_files)) {
  stop("Missing GDP decomposition GDX file(s): ",
       paste(missing_gdp_files, collapse = ", "))
}
if (!requireNamespace("gdxrrw", quietly = TRUE)) {
  stop("gdxrrw is required to read GDP_s for the new decomposition panel.")
}

gdp_group_map <- tibble(
  Region_code = c(providers, recipients),
  Region_group = c(
    rep("Developed regions", length(providers)),
    rep("Developing regions", length(recipients))
  )
)
gdp_group_levels <- c("Developed regions", "Developing regions")
gdp_category_levels <- c(
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
  names(gdp_scenario_paths),
  unname(gdp_scenario_paths)
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
  filter(Year == 2050, Region_code %in% gdp_group_map$Region_code) %>%
  left_join(gdp_group_map, by = "Region_code") %>%
  group_by(Scenario, Region_group, Category) %>%
  summarise(Value = sum(Value), .groups = "drop") %>%
  complete(
    Scenario = names(gdp_scenario_paths),
    Region_group = gdp_group_levels,
    Category = gdp_category_levels,
    fill = list(Value = 0)
  )

expected_gdp_rows <- length(gdp_scenario_paths) * length(gdp_group_levels) *
  length(gdp_category_levels)
if (nrow(gdp_grouped) != expected_gdp_rows || anyDuplicated(gdp_grouped[c(
  "Scenario", "Region_group", "Category"
)])) {
  stop("The GDP scenario-region-category grid is not complete and unique.")
}

gdp_contributions <- gdp_grouped %>%
  group_by(Region_group, Category) %>%
  mutate(BaU_category_value = Value[Scenario == "BaU"][[1]]) %>%
  ungroup() %>%
  group_by(Region_group, Scenario) %>%
  mutate(
    BaU_total = sum(BaU_category_value),
    Contribution_pp = 100 * (Value - BaU_category_value) / BaU_total
  ) %>%
  ungroup() %>%
  filter(Scenario %in% c("Def", "Aid")) %>%
  mutate(
    Scenario = factor(Scenario, levels = c("Def", "Aid")),
    Region_group = factor(Region_group, levels = gdp_group_levels),
    Category = factor(Category, levels = gdp_category_levels)
  )

gdp_totals <- gdp_contributions %>%
  group_by(Region_group, Scenario) %>%
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
gdp_decomposition_error <- max(abs(
  gdp_totals$Total_change_pct - gdp_totals$Direct_change_pct
))
if (!is.finite(gdp_decomposition_error) || gdp_decomposition_error > 1e-10) {
  stop("GDP contributions do not reproduce the total GDP change.")
}

gdp_range <- range(c(
  gdp_contributions$Contribution_pp,
  gdp_totals$Total_change_pct
))
gdp_label_offset <- max(0.20, 0.035 * diff(gdp_range))
gdp_totals <- gdp_totals %>%
  mutate(
    Label_y = Total_change_pct +
      if_else(Total_change_pct >= 0, gdp_label_offset, -gdp_label_offset),
    Label_vjust = if_else(Total_change_pct >= 0, 0, 1)
  )

gdp_component_colours <- c(
  "Consumption" = "#0072B2",
  "Government spending" = "#CC79A7",
  "Investment" = "#E69F00",
  "Net exports" = "#009E73"
)

p_gdp_decomposition <- ggplot(
  gdp_contributions,
  aes(x = Scenario, y = Contribution_pp, fill = Category)
) +
  geom_hline(yintercept = 0, colour = ink, linewidth = 0.35) +
  geom_col(width = 0.58, colour = "white", linewidth = 0.18) +
  geom_point(
    data = gdp_totals,
    aes(x = Scenario, y = Total_change_pct, shape = "Net GDP change"),
    inherit.aes = FALSE,
    size = 2.20,
    stroke = 0.60,
    fill = "white",
    colour = ink
  ) +
  geom_text(
    data = gdp_totals,
    aes(
      x = Scenario,
      y = Label_y,
      label = Label,
      vjust = Label_vjust
    ),
    inherit.aes = FALSE,
    family = "Arial",
    size = 2.0,
    colour = ink
  ) +
  facet_grid(. ~ Region_group) +
  scale_fill_manual(
    values = gdp_component_colours,
    breaks = gdp_category_levels,
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
  scale_y_continuous(
    name = "Contribution to GDP change relative to BaU (%)",
    labels = label_number(accuracy = 0.1),
    expand = expansion(mult = c(0.14, 0.17))
  ) +
  labs(x = NULL) +
  guides(
    fill = guide_legend(order = 1, nrow = 2, byrow = TRUE),
    shape = guide_legend(order = 2)
  ) +
  theme_nature() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(fill = NA, colour = ink, linewidth = 0.30),
    axis.line = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 7.0),
    panel.spacing.x = grid::unit(3, "mm")
  )

p_efficiency_compact <- p_efficiency +
  guides(colour = "none")

p_timing_composite <- (
  wrap_plots(
    A = p_timing,
    B = p_cumulative_loss,
    C = p_efficiency_compact,
    design = "AAB\nCCC",
    heights = c(1, 1.08),
    guides = "collect"
  ) +
    plot_annotation(tag_levels = "a")
) &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.spacing.x = grid::unit(2, "mm"),
    panel.border = element_rect(fill = NA, colour = ink, linewidth = 0.35),
    axis.line = element_blank(),
    plot.tag = element_text(
      family = "Arial", face = "bold", size = 8.5, colour = ink
    ),
    plot.tag.position = "topleft"
  )

save_png("03_transfer_timing_consumption_loss.png", p_timing_composite, 158)

# Five-panel extension: consumption timing, cumulative consumption and GDP
# loss, annual transfer efficiency, and the 2050 GDP decomposition.
p_timing_consumption_gdp <- (
  wrap_plots(
    A = p_timing,
    B = p_cumulative_loss,
    C = p_cumulative_gdp,
    D = p_efficiency_compact,
    E = p_gdp_decomposition,
    design = "AAAABBCC\nDDDDEEEE",
    heights = c(0.90, 1.10),
    guides = "collect"
  ) +
    plot_annotation(tag_levels = "a")
) &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.justification = "center",
    legend.text = element_text(size = 6.5),
    legend.key.width = grid::unit(4.5, "mm"),
    legend.key.height = grid::unit(3.0, "mm"),
    legend.spacing.x = grid::unit(1.0, "mm"),
    panel.border = element_rect(fill = NA, colour = ink, linewidth = 0.35),
    axis.line = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA),
    plot.tag = element_text(
      family = "Arial", face = "bold", size = 8.5, colour = ink
    ),
    plot.tag.position = "topleft"
  )

extended_output_file <- file.path(
  project_root,
  "output",
  "Figure",
  "03_transfer_timing_consumption_gdp.png"
)
ggsave(
  filename = extended_output_file,
  plot = p_timing_consumption_gdp,
  device = "png",
  type = "cairo",
  width = 180,
  height = 176,
  units = "mm",
  dpi = 400,
  bg = "white",
  limitsize = FALSE
)

message("Created PNG files in: ", output_dir)
message("Created extended composite: ", extended_output_file)
message(
  "Maximum GDP decomposition error: ",
  format(gdp_decomposition_error, scientific = TRUE)
)
message("Annual transfer-efficiency diagnostics (monetary values in billion US$2010/yr):")
print(
  annual_efficiency[, c(
    "Year", "Transfer_billion_USD2010",
    "Recipient_benefit_billion_USD2010", "Recipient_efficiency",
    "Provider_burden_billion_USD2010", "Provider_burden_ratio"
  )],
  row.names = FALSE,
  digits = 4
)
message(sprintf(
  paste0(
    "2050 diagnostics: transfer = %.3f trillion US$2010/yr; world carbon price = ",
    "%.3f US$2010/tCO2; transfer equivalent = %.3f GtCO2/yr; ",
    "recipient benefit = %.3f billion US$2010/yr; recipient efficiency = %.3f; ",
    "provider burden = %.3f billion US$2010/yr; provider burden ratio = %.3f; ",
    "aggregate-row sensitivity = %.3f (aggregate numerator only) / %.3f ",
    "(aggregate numerator and denominator); revenue/provider GDP = %.3f%%; ",
    "Emi_CO2*Prc_Car proxy = %.3f trillion US$2010/yr; revenue/proxy = %.3f; ",
    "if raw revenue were billion, revenue/proxy would be %.0f; ",
    "maximum CNS identity gap = %.3g billion."
  ),
  transfer_2050_musd / 1e6,
  world_carbon_price_2050,
  allowance_equivalent_gt,
  developing_loss_reduction_busd,
  transfer_efficiency,
  provider_burden_2050_busd,
  provider_burden_ratio_2050,
  aggregate_row_efficiency,
  aggregate_both_efficiency,
  transfer_share_provider_gdp,
  emissions_price_proxy_musd / 1e6,
  revenue_to_emissions_price_proxy,
  billion_interpretation_mismatch,
  identity_gap_busd
))
