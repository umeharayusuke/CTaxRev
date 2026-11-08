# Supplementary figures: complete bundled source code -----------------------
#
# This file contains every analysis and plotting script required for
# Supplementary Figs. 1-13. No code outside Rprog/All is required.

supplementary_script_registry <- list(
  "Sensitivity_GDP_per_capita_threshold_classification_2025.R" = r"------------------------(
# 2025 regional GDP per capita and aid-threshold classification -------------
#
# GDP_per_cap is stored in kUSD_2010/person. The 10k, 15k, and 20k thresholds
# classify a region as Recipient below the threshold and Provider at or above
# it. The classification is checked against all SSP1-SSP3 BaU values; SSP2 is
# then used as the representative value series in the figure.

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

regions <- c(
  "XE25", "JPN", "IND", "TUR", "CHN", "USA", "XER", "XOC", "XSE",
  "XSA", "CAN", "BRA", "XLM", "CIS", "XME", "XNF", "XAF"
)
scenarios <- paste0("SSP", 1:3, "_BaU_NoCC_No")
thresholds <- data.frame(
  Threshold_kUSD = c(10, 15, 20),
  Threshold = factor(c("10k", "15k", "20k"), levels = c("10k", "15k", "20k"))
)

expected_recipients <- list(
  `10` = c("IND", "XSA", "XNF", "XAF"),
  `15` = c("IND", "BRA", "XSA", "XSE", "XME", "XNF", "XAF"),
  `20` = c(
    "CHN", "IND", "BRA", "XSA", "XSE", "XME", "XLM", "XNF", "XAF", "CIS"
  )
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

gdp_all <- raw %>%
  transmute(
    SCENARIO = as.character(SCENARIO),
    Region = as.character(REMF),
    VEMF = as.character(VEMF),
    Year = as.numeric(as.character(YEMF)),
    GDPpc_kUSD = as.numeric(IAMC_Template)
  ) %>%
  filter(
    SCENARIO %in% scenarios,
    Region %in% regions,
    VEMF == "GDP_per_cap",
    Year == 2025
  ) %>%
  select(SCENARIO, Region, GDPpc_kUSD)

expected_rows <- length(scenarios) * length(regions)
if (nrow(gdp_all) != expected_rows) {
  stop(
    "Expected ", expected_rows, " regional 2025 GDP-per-capita values; found ",
    nrow(gdp_all), "."
  )
}
if (anyDuplicated(gdp_all[c("SCENARIO", "Region")])) {
  stop("Duplicate regional GDP-per-capita values were found.")
}
if (any(!is.finite(gdp_all$GDPpc_kUSD)) || any(gdp_all$GDPpc_kUSD <= 0)) {
  stop("Regional GDP-per-capita values must be finite and positive.")
}

# Verify that every SSP yields exactly the region memberships supplied for the
# three thresholds, even though unrounded GDP values differ slightly by SSP.
classification_all <- crossing(gdp_all, thresholds) %>%
  mutate(
    Classification = if_else(
      GDPpc_kUSD < Threshold_kUSD, "Recipient", "Provider"
    )
  )

for (scenario in scenarios) {
  for (threshold in thresholds$Threshold_kUSD) {
    observed <- classification_all %>%
      filter(
        SCENARIO == scenario,
        Threshold_kUSD == threshold,
        Classification == "Recipient"
      ) %>%
      pull(Region)
    expected <- expected_recipients[[as.character(threshold)]]
    if (!setequal(observed, expected)) {
      stop(
        "Recipient classification differs from the supplied mapping for ",
        scenario, " at ", threshold, "k."
      )
    }
  }
}

reference_scenario <- "SSP2_BaU_NoCC_No"
reference_data <- gdp_all %>%
  filter(SCENARIO == reference_scenario) %>%
  arrange(GDPpc_kUSD)
region_levels <- reference_data$Region

reference_data <- reference_data %>%
  mutate(
    Region = factor(Region, levels = region_levels),
    Value_label = sprintf("%.1f", GDPpc_kUSD)
  )

classification_data <- crossing(
  reference_data %>% select(Region, GDPpc_kUSD),
  thresholds
) %>%
  mutate(
    Classification = if_else(
      GDPpc_kUSD < Threshold_kUSD, "Recipient", "Provider"
    ),
    Classification = factor(
      Classification, levels = c("Recipient", "Provider")
    ),
    Cell_label = if_else(Classification == "Provider", "P", "R")
  )

band_data <- data.frame(
  xmin = c(0, 10, 15, 20),
  xmax = c(10, 15, 20, 68),
  Fill = c("#F7ECE8", "#F7F3E8", "#EEF4EE", "#EAF1F5")
)
threshold_lines <- data.frame(
  Threshold_kUSD = c(10, 15, 20),
  Threshold = factor(c("10k", "15k", "20k"), levels = c("10k", "15k", "20k"))
)
threshold_colours <- c("10k" = "#0072B2", "15k" = "#D55E00", "20k" = "#009E73")
classification_colours <- c("Recipient" = "#B6573F", "Provider" = "#3F6C8E")
ink <- "#252525"

p_values <- ggplot(reference_data, aes(y = Region)) +
  geom_rect(
    data = band_data,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = Fill),
    inherit.aes = FALSE,
    alpha = 0.72
  ) +
  scale_fill_identity() +
  geom_vline(
    data = threshold_lines,
    aes(xintercept = Threshold_kUSD, colour = Threshold),
    linewidth = 0.65,
    linetype = "22",
    show.legend = FALSE
  ) +
  geom_segment(
    aes(x = 0, xend = GDPpc_kUSD, yend = Region),
    linewidth = 0.35,
    colour = "#A9A9A9"
  ) +
  geom_point(aes(x = GDPpc_kUSD), size = 2.25, colour = ink) +
  geom_text(
    aes(x = GDPpc_kUSD, label = Value_label),
    nudge_x = 0.75,
    hjust = 0,
    family = "Arial",
    size = 2.35,
    colour = ink
  ) +
  scale_colour_manual(values = threshold_colours) +
  scale_x_continuous(
    name = expression(paste("2025 GDP per capita (kUS$"[2010], "/person)")),
    breaks = c(0, 10, 15, 20, 40, 60),
    limits = c(0, 68),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_discrete(limits = region_levels, drop = FALSE, expand = expansion(add = 0.55)) +
  coord_cartesian(clip = "off") +
  theme_classic(base_family = "Arial", base_size = 8) +
  theme(
    text = element_text(colour = ink),
    axis.line = element_blank(),
    axis.ticks = element_line(linewidth = 0.25, colour = ink),
    axis.title.x = element_text(size = 8.2),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 7.4, colour = ink),
    panel.border = element_rect(fill = NA, colour = ink, linewidth = 0.30),
    panel.grid = element_blank(),
    plot.margin = margin(4, 7, 4, 4)
  )

p_classification <- ggplot(
  classification_data,
  aes(x = Threshold, y = Region, fill = Classification)
) +
  geom_tile(width = 0.90, height = 0.90, colour = "white", linewidth = 0.45) +
  geom_text(
    aes(label = Cell_label),
    family = "Arial",
    fontface = "bold",
    size = 2.6,
    colour = "white"
  ) +
  scale_fill_manual(
    values = classification_colours,
    breaks = c("Recipient", "Provider"),
    labels = c(
      "Recipient" = "Developing / Recipient",
      "Provider" = "Developed / Provider"
    ),
    name = NULL
  ) +
  scale_x_discrete(position = "bottom", drop = FALSE) +
  scale_y_discrete(limits = region_levels, drop = FALSE, expand = expansion(add = 0.55)) +
  labs(x = "GDP per capita threshold", y = NULL) +
  guides(fill = guide_legend(nrow = 1, title.position = "top")) +
  theme_classic(base_family = "Arial", base_size = 8) +
  theme(
    text = element_text(colour = ink),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(size = 8.2, margin = margin(t = 2)),
    axis.text.x = element_text(size = 7.4, face = "bold", colour = ink),
    axis.text.y = element_blank(),
    panel.border = element_rect(fill = NA, colour = ink, linewidth = 0.30),
    legend.position = "bottom",
    legend.justification = "center",
    legend.text = element_text(size = 7.4),
    legend.key.size = grid::unit(3.7, "mm"),
    plot.margin = margin(4, 4, 4, 1)
  )

p <- wrap_plots(
  A = p_values,
  B = p_classification,
  C = guide_area(),
  design = "AB\nCC",
  widths = c(2.35, 1),
  heights = c(1, 0.10),
  guides = "collect"
) +
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
  output_dir, "06_GDP_per_capita_threshold_classification_2025.png"
)

ggsave(
  filename = output_file,
  plot = p,
  device = "png",
  type = "cairo",
  width = 180,
  height = 125,
  units = "mm",
  dpi = 400,
  bg = "white"
)

message("Created: ", output_file)
message("2025 GDP per capita used in the figure (kUSD_2010/person):")
print(
  reference_data %>% select(Region, GDPpc_kUSD),
  row.names = FALSE,
  digits = 6
)

)------------------------",
  "Transfer_emissions_efficiency_timing.R" = r"------------------------(
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

)------------------------",
  "SI_primary_energy_supply_3x3.R" = r"------------------------(
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

)------------------------",
  "SI_cumulative_consumption_gdp_loss_2050.R" = r"------------------------(
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

)------------------------",
  "SSP2_additional_poverty_all_lines_2050.R" = r"------------------------(
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

)------------------------",
  "Sensitivity_world_population_GDP_SSP1_SSP3.R" = r"------------------------(
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

)------------------------",
  "Sensitivity_provider_carbon_revenue_SSP1_SSP3.R" = r"------------------------(
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

)------------------------",
  "Sensitivity_cumulative_consumption_GDP_loss_thresholds.R" = r"------------------------(
# Sensitivity of cumulative consumption and GDP losses to aid thresholds -----
#
# Panels show the model's directly reported 2050 cumulative loss rates using a
# 5% discount rate. Columns are the 10k, 15k, and 20k USD GDP-per-capita
# thresholds; the x axis shows World, the matching Rprovider* aggregate, and
# the matching Rrecipient* aggregate. Every threshold panel includes both the
# no-aid Default scenario and its corresponding Aid scenario.

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

metric_codes <- c(
  Consumption = "Pol_Cos_Cns_Los_rat_NPV_5pc",
  GDP = "Pol_Cos_GDP_Los_rat_NPV_5pc"
)
ssp_levels <- paste0("SSP", 1:3)
threshold_levels <- c("10th", "15th", "20th")
policy_levels <- c("Default", "Aid")
region_levels <- c("World", "Developed", "Developing")

plot_grid <- crossing(
  SSP = ssp_levels,
  Threshold = threshold_levels,
  Policy = policy_levels,
  Metric = names(metric_codes),
  Region_group = region_levels
) %>%
  mutate(
    SCENARIO = if_else(
      Policy == "Default",
      paste0(SSP, "_400C_2030CP_NoCC_No"),
      paste0(SSP, "_400C_2030CP_", Threshold, "_NoCC_No")
    ),
    REMF = case_when(
      Region_group == "World" ~ "World",
      Region_group == "Developed" ~ paste0("Rprovider", Threshold),
      Region_group == "Developing" ~ paste0("Rrecipient", Threshold)
    ),
    VEMF = unname(metric_codes[Metric]),
    Year = 2050
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

iamc <- raw %>%
  transmute(
    SCENARIO = as.character(SCENARIO),
    REMF = as.character(REMF),
    VEMF = as.character(VEMF),
    Year = as.numeric(as.character(YEMF)),
    Loss_pct = as.numeric(IAMC_Template)
  ) %>%
  filter(
    SCENARIO %in% unique(plot_grid$SCENARIO),
    REMF %in% unique(plot_grid$REMF),
    VEMF %in% metric_codes,
    Year == 2050
  )

plot_data <- plot_grid %>%
  left_join(iamc, by = c("SCENARIO", "REMF", "VEMF", "Year"))

if (nrow(plot_data) != nrow(plot_grid)) {
  stop("Joining IAMC_template changed the expected plotting-grid size.")
}
if (anyDuplicated(plot_data[c(
  "SSP", "Threshold", "Policy", "Metric", "Region_group"
)])) {
  stop("Duplicate cumulative-loss plotting observations were found.")
}
if (any(!is.finite(plot_data$Loss_pct))) {
  stop("At least one required cumulative-loss value is missing or non-finite.")
}

# SSP3 10th and 15th Aid runs are invalid for the full horizon. Their values
# are retained as explicit NA rows so the intended empty positions are stable.
plot_data <- plot_data %>%
  mutate(
    Loss_pct = if_else(
      SSP == "SSP3" & Threshold %in% c("10th", "15th") & Policy == "Aid",
      NA_real_,
      Loss_pct
    ),
    SSP = factor(SSP, levels = ssp_levels),
    Threshold = factor(Threshold, levels = threshold_levels),
    Policy = factor(Policy, levels = policy_levels),
    Metric = factor(Metric, levels = c("Consumption", "GDP")),
    Region_group = factor(Region_group, levels = region_levels),
    Region_index = as.numeric(Region_group),
    SSP_offset = case_when(
      SSP == "SSP1" ~ -0.18,
      SSP == "SSP2" ~ 0,
      SSP == "SSP3" ~ 0.18
    ),
    X_position = Region_index + SSP_offset
  )

excluded <- plot_data %>%
  filter(SSP == "SSP3", Threshold %in% c("10th", "15th"), Policy == "Aid")
if (nrow(excluded) != 12L || any(!is.na(excluded$Loss_pct))) {
  stop("The SSP3 10th/15th Aid exclusion was not applied correctly.")
}

default_rows <- plot_data %>% filter(Policy == "Default")
if (nrow(default_rows) != 54L || any(!is.finite(default_rows$Loss_pct))) {
  stop("Default values are not complete across all threshold panels.")
}

ssp_colours <- c(
  "SSP1" = "#0072B2",
  "SSP2" = "#D55E00",
  "SSP3" = "#009E73"
)
policy_shapes <- c("Default" = 1, "Aid" = 16)
threshold_labels <- c(
  "10th" = "10k USD threshold",
  "15th" = "15k USD threshold",
  "20th" = "20k USD threshold"
)
ink <- "#252525"

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
      legend.box = "horizontal",
      legend.box.just = "center",
      legend.spacing.x = grid::unit(3, "mm"),
      legend.title = element_text(size = 7.4),
      legend.text = element_text(size = 7.4),
      legend.key.width = grid::unit(6, "mm"),
      plot.margin = margin(3, 6, 3, 4)
    )
}

make_loss_plot <- function(data, y_title, show_legend) {
  ggplot(
    data,
    aes(
      x = X_position,
      y = Loss_pct,
      colour = SSP,
      shape = Policy
    )
  ) +
    geom_hline(yintercept = 0, linewidth = 0.35, colour = ink) +
    geom_line(
      aes(group = interaction(SSP, Region_group)),
      linewidth = 0.48,
      linetype = "22",
      na.rm = TRUE
    ) +
    geom_point(
      size = 2.35,
      stroke = 0.70,
      na.rm = TRUE
    ) +
    facet_wrap(
      ~Threshold,
      nrow = 1,
      labeller = as_labeller(threshold_labels)
    ) +
    scale_colour_manual(
      values = ssp_colours,
      breaks = ssp_levels,
      name = "SSP"
    ) +
    scale_shape_manual(
      values = policy_shapes,
      breaks = policy_levels,
      labels = c("Default" = "Default (no aid)", "Aid" = "Aid"),
      name = "Policy"
    ) +
    scale_x_continuous(
      breaks = seq_along(region_levels),
      labels = region_levels,
      limits = c(0.58, 3.42),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(
      name = y_title,
      labels = label_number(accuracy = 0.1),
      expand = expansion(mult = c(0.10, 0.12))
    ) +
    labs(x = NULL) +
    guides(
      colour = guide_legend(order = 1, nrow = 1),
      shape = guide_legend(order = 2, nrow = 1)
    ) +
    theme_sensitivity() +
    theme(legend.position = if (show_legend) "bottom" else "none")
}

p_consumption <- make_loss_plot(
  plot_data %>% filter(Metric == "Consumption"),
  "Cumulative consumption loss in 2050 (%)",
  TRUE
)

p_gdp <- make_loss_plot(
  plot_data %>% filter(Metric == "GDP"),
  "Cumulative GDP loss in 2050 (%)",
  FALSE
)

p <- (
  p_consumption / p_gdp / guide_area() +
    plot_layout(guides = "collect", heights = c(1, 1, 0.13)) +
    plot_annotation(tag_levels = "a")
) &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.box.just = "center",
    legend.spacing.x = grid::unit(3, "mm"),
    legend.margin = margin(t = 3, b = 2, unit = "mm"),
    plot.tag = element_text(
      family = "Arial", face = "bold", size = 9.0, colour = ink
    ),
    plot.tag.position = "topleft"
  )

output_dir <- file.path(project_root, "output", "Figure", "analysis")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_file <- file.path(
  output_dir, "07_sensitivity_cumulative_consumption_GDP_loss_thresholds.png"
)

ggsave(
  filename = output_file,
  plot = p,
  device = "png",
  type = "cairo",
  width = 180,
  height = 142,
  units = "mm",
  dpi = 400,
  bg = "white"
)

message("Created: ", output_file)
message("Cumulative-loss plotting data (NA values are intentionally blank):")
print(
  plot_data %>%
    select(SSP, Threshold, Policy, Metric, Region_group, Loss_pct) %>%
    arrange(Metric, Threshold, Region_group, SSP, Policy),
  row.names = FALSE,
  digits = 5
)

)------------------------",
  "Sensitivity_poverty_population_SSP1_SSP3.R" = r"------------------------(
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

)------------------------",
  "Sensitivity_lorenz_curves_2050_SSP1_SSP3.R" = r"------------------------(
library_candidates <- c(
  Sys.getenv("R_LIBS_USER"),
  file.path(getwd(), "Rlib"),
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

gams_dirs <- c("C:/GAMS/34", "C:/GAMS/win64/26.1")
for (gams_dir in gams_dirs) {
  if (dir.exists(gams_dir)) {
    igdx(gams_dir)
    break
  }
}

comparison_year <- 2050
population_threshold <- "pop_8.30"
ssp_levels <- paste0("SSP", 1:3)
policy_levels <- c("BaU", "Default", "10th", "15th", "20th")

scenario_grid <- crossing(
  SSP = factor(ssp_levels, levels = ssp_levels),
  Policy = factor(policy_levels, levels = policy_levels)
) %>%
  mutate(
    Scenario_id = case_when(
      Policy == "BaU" ~ paste0(SSP, "_BaU_NoCC_No"),
      Policy == "Default" ~ paste0(SSP, "_400C_2030CP_NoCC_No"),
      TRUE ~ paste0(SSP, "_400C_2030CP_", Policy, "_NoCC_No")
    )
  ) %>%
  filter(!(SSP == "SSP3" & Policy %in% c("10th", "15th")))

gdx_file <- "data/AR6database/AnalysisExpenditure.gdx"
if (!file.exists(gdx_file)) {
  stop("GDX file not found: ", gdx_file)
}

budget <- rgdx.param(gdx_file, "BudgetInitial") %>%
  transmute(
    Region_code = as.character(R),
    Segment = as.character(Seg),
    Expenditure = as.numeric(BudgetInitial)
  ) %>%
  filter(is.finite(Expenditure), Expenditure >= 0)

frequency <- rgdx.param(gdx_file, "FreqSegall_exp") %>%
  transmute(
    Region_code = as.character(R),
    Scenario_id = as.character(Ref),
    Year = as.integer(as.character(Y)),
    Segment = as.character(Seg),
    Frequency = as.numeric(FreqSegall_exp)
  ) %>%
  filter(
    Scenario_id %in% scenario_grid$Scenario_id,
    Year == comparison_year,
    is.finite(Frequency),
    Frequency >= 0
  )

missing_scenarios <- setdiff(scenario_grid$Scenario_id, unique(frequency$Scenario_id))
if (length(missing_scenarios) > 0) {
  stop("Missing scenarios in FreqSegall_exp: ", paste(missing_scenarios, collapse = ", "))
}

budget_bins <- budget %>% count(Region_code, name = "Budget_bins")
frequency_cells <- frequency %>%
  count(Region_code, Scenario_id, Year, name = "Frequency_bins") %>%
  left_join(budget_bins, by = "Region_code") %>%
  filter(Frequency_bins == Budget_bins)

balanced_regions <- frequency_cells %>%
  count(Region_code, name = "Complete_cells") %>%
  filter(Complete_cells == nrow(scenario_grid)) %>%
  pull(Region_code)

if (length(balanced_regions) == 0) {
  stop("No balanced country panel was found.")
}

poverty_count <- rgdx.param(gdx_file, "PoVExp") %>%
  transmute(
    Region_code = as.character(R),
    Scenario_id = as.character(Ref),
    Year = as.integer(as.character(Y)),
    Poverty_line = as.character(TH),
    People = as.numeric(PoVExp)
  ) %>%
  filter(
    Scenario_id %in% scenario_grid$Scenario_id,
    Region_code %in% balanced_regions,
    Year == comparison_year,
    Poverty_line == population_threshold,
    is.finite(People)
  )

poverty_rate <- rgdx.param(gdx_file, "RoPExp") %>%
  transmute(
    Region_code = as.character(R),
    Scenario_id = as.character(Ref),
    Year = as.integer(as.character(Y)),
    Poverty_line = as.character(TH),
    Poverty_rate = as.numeric(RoPExp)
  ) %>%
  filter(
    Scenario_id %in% scenario_grid$Scenario_id,
    Region_code %in% balanced_regions,
    Year == comparison_year,
    Poverty_line == population_threshold,
    is.finite(Poverty_rate),
    Poverty_rate > 0
  )

country_population <- poverty_count %>%
  inner_join(
    poverty_rate,
    by = c("Region_code", "Scenario_id", "Year", "Poverty_line")
  ) %>%
  transmute(
    Region_code,
    Scenario_id,
    Year,
    Population = People / Poverty_rate
  ) %>%
  filter(is.finite(Population), Population > 0)

expected_population_cells <- nrow(scenario_grid) * length(balanced_regions)
if (nrow(country_population) != expected_population_cells) {
  stop(
    "Incomplete country population panel: ", nrow(country_population),
    " cells found; expected ", expected_population_cells, "."
  )
}

frequency_sums <- frequency %>%
  filter(Region_code %in% balanced_regions) %>%
  group_by(Region_code, Scenario_id, Year) %>%
  summarise(Frequency_sum = sum(Frequency), .groups = "drop")

if (any(!is.finite(frequency_sums$Frequency_sum) | frequency_sums$Frequency_sum <= 0)) {
  stop("Non-positive frequency sum found.")
}

detailed_distribution <- frequency %>%
  filter(Region_code %in% balanced_regions) %>%
  left_join(frequency_sums, by = c("Region_code", "Scenario_id", "Year")) %>%
  mutate(Population_share = Frequency / Frequency_sum) %>%
  inner_join(budget, by = c("Region_code", "Segment")) %>%
  inner_join(country_population, by = c("Region_code", "Scenario_id", "Year")) %>%
  mutate(Population_weight = Population_share * Population) %>%
  filter(
    is.finite(Expenditure), Expenditure >= 0,
    is.finite(Population_weight), Population_weight > 0
  )

lorenz_cells <- detailed_distribution %>%
  inner_join(scenario_grid, by = "Scenario_id") %>%
  group_by(SSP, Policy, Scenario_id, Year, Expenditure) %>%
  summarise(Population_weight = sum(Population_weight), .groups = "drop") %>%
  arrange(SSP, Policy, Expenditure) %>%
  group_by(SSP, Policy, Scenario_id, Year) %>%
  mutate(
    Expenditure_weight = Expenditure * Population_weight,
    Cumulative_population_pct = 100 * cumsum(Population_weight) / sum(Population_weight),
    Cumulative_expenditure_pct = 100 * cumsum(Expenditure_weight) / sum(Expenditure_weight)
  ) %>%
  ungroup() %>%
  select(SSP, Policy, Scenario_id, Year, Cumulative_population_pct, Cumulative_expenditure_pct)

origin_rows <- scenario_grid %>%
  transmute(
    SSP,
    Policy,
    Scenario_id,
    Year = comparison_year,
    Cumulative_population_pct = 0,
    Cumulative_expenditure_pct = 0
  )

lorenz_data <- bind_rows(origin_rows, lorenz_cells) %>%
  arrange(SSP, Policy, Cumulative_population_pct)

curve_checks <- lorenz_data %>%
  group_by(SSP, Policy, Scenario_id, Year) %>%
  summarise(
    start_population = first(Cumulative_population_pct),
    start_expenditure = first(Cumulative_expenditure_pct),
    end_population = last(Cumulative_population_pct),
    end_expenditure = last(Cumulative_expenditure_pct),
    monotone_population = all(diff(Cumulative_population_pct) >= -1e-10),
    monotone_expenditure = all(diff(Cumulative_expenditure_pct) >= -1e-10),
    .groups = "drop"
  )

if (
  any(abs(curve_checks$start_population) > 1e-8) ||
  any(abs(curve_checks$start_expenditure) > 1e-8) ||
  any(abs(curve_checks$end_population - 100) > 1e-6) ||
  any(abs(curve_checks$end_expenditure - 100) > 1e-6) ||
  any(!curve_checks$monotone_population) ||
  any(!curve_checks$monotone_expenditure)
) {
  stop("Lorenz curve validation failed.")
}

gini_check <- lorenz_data %>%
  group_by(SSP, Policy, Scenario_id) %>%
  summarise(
    Gini = 1 - sum(
      diff(Cumulative_population_pct / 100) *
        (head(Cumulative_expenditure_pct / 100, -1) +
           tail(Cumulative_expenditure_pct / 100, -1))
    ),
    .groups = "drop"
  )

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
policy_linetypes <- c(
  "BaU" = "22",
  "Default" = "longdash",
  "10th" = "solid",
  "15th" = "solid",
  "20th" = "solid"
)

plot_lorenz <- ggplot(
  lorenz_data,
  aes(
    x = Cumulative_population_pct,
    y = Cumulative_expenditure_pct,
    colour = Policy,
    linetype = Policy,
    group = Policy
  )
) +
  geom_abline(
    intercept = 0,
    slope = 1,
    colour = "#8A8A8A",
    linewidth = 0.28,
    linetype = "dotted"
  ) +
  geom_line(linewidth = 0.45, lineend = "round") +
  facet_wrap(~SSP, nrow = 1) +
  scale_colour_manual(
    values = policy_colours,
    breaks = policy_levels,
    labels = policy_labels,
    drop = FALSE
  ) +
  scale_linetype_manual(
    values = policy_linetypes,
    breaks = policy_levels,
    labels = policy_labels,
    drop = FALSE
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, 20),
    labels = label_number(accuracy = 1),
    expand = expansion(mult = 0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    labels = label_number(accuracy = 1),
    expand = expansion(mult = 0)
  ) +
  coord_equal(xlim = c(0, 100), ylim = c(0, 100), clip = "off") +
  labs(
    x = "Cumulative population share (%)",
    y = "Cumulative expenditure share (%)",
    colour = NULL,
    linetype = NULL
  ) +
  guides(
    colour = guide_legend(nrow = 1, byrow = TRUE),
    linetype = guide_legend(nrow = 1, byrow = TRUE)
  ) +
  theme_bw(base_family = "Arial", base_size = 8) +
  theme(
    panel.grid.major = element_line(colour = "#E8E8E8", linewidth = 0.25),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", linewidth = 0.45),
    strip.background = element_blank(),
    strip.text = element_text(size = 9, face = "bold", margin = margin(b = 3)),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 7.5, colour = "black"),
    axis.ticks = element_line(linewidth = 0.35),
    legend.position = "bottom",
    legend.justification = "center",
    legend.key.width = unit(12, "mm"),
    legend.key.height = unit(3, "mm"),
    legend.text = element_text(size = 8),
    legend.margin = margin(t = 1, r = 0, b = 0, l = 0),
    plot.margin = margin(5, 5, 3, 5)
  )

output_dir <- "output/Figure/analysis"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

png_file <- file.path(output_dir, "14_sensitivity_lorenz_curves_2050_SSP1_SSP3.png")
csv_file <- file.path(output_dir, "14_sensitivity_lorenz_curves_2050_SSP1_SSP3_data.csv")
gini_file <- file.path(output_dir, "14_sensitivity_lorenz_curves_2050_SSP1_SSP3_gini_check.csv")

ggsave(
  filename = png_file,
  plot = plot_lorenz,
  width = 180,
  height = 83,
  units = "mm",
  dpi = 400,
  bg = "white"
)

write.csv(lorenz_data, csv_file, row.names = FALSE, fileEncoding = "UTF-8")
write.csv(gini_check, gini_file, row.names = FALSE, fileEncoding = "UTF-8")

message("Balanced regions: ", length(balanced_regions))
message("Saved: ", png_file)
message("Saved: ", csv_file)
message("Saved: ", gini_file)
print(gini_check)

)------------------------",
  "Sensitivity_inequality_welfare_2050_SSP1_SSP3.R" = r"------------------------(
# Sensitivity of global inequality, mean expenditure, and welfare in 2050 ----
#
# This figure is the 2050 sensitivity counterpart to panels c-f of
# 04b_poverty_inequality_welfare_detailed_expenditure_2020_2050.png.
# It uses the same detailed country-by-expenditure-segment distribution,
# population recovery, global weighted-Gini calculation, welfare index, and
# exact two-factor Shapley decomposition. SSP3 10k and 15k are excluded.

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
output_dir <- file.path(project_root, "output", "Figure", "analysis")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_stem <- file.path(
  output_dir, "09_sensitivity_inequality_welfare_2050_SSP1_SSP3"
)

# Settings ------------------------------------------------------------------

comparison_year <- 2050
population_threshold <- "pop_8.30"
ssp_levels <- paste0("SSP", 1:3)
policy_levels <- c("BaU", "Default", "10th", "15th", "20th")
policy_non_bau <- policy_levels[-1]

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

policy_labels <- c(
  "BaU" = "BaU", "Default" = "Default", "10th" = "10k",
  "15th" = "15k", "20th" = "20k"
)
ssp_colours <- c("SSP1" = "#0072B2", "SSP2" = "#D55E00", "SSP3" = "#009E73")
ssp_shapes <- c("SSP1" = 16, "SSP2" = 17, "SSP3" = 15)
effect_colours <- c(
  "Mean consumption expenditure" = "#6B5B95",
  "Interpersonal distribution" = "#2A9D8F"
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

weighted_gini <- function(expenditure, population) {
  valid <- is.finite(expenditure) & expenditure >= 0 &
    is.finite(population) & population > 0
  expenditure <- expenditure[valid]
  population <- population[valid]
  if (!length(expenditure) || sum(expenditure * population) <= 0) return(NA_real_)
  ordering <- order(expenditure)
  expenditure <- expenditure[ordering]
  population <- population[ordering]
  population_share <- population / sum(population)
  expenditure_share <-
    population * expenditure / sum(population * expenditure)
  cumulative_population <- c(0, cumsum(population_share))
  cumulative_expenditure <- c(0, cumsum(expenditure_share))
  1 - sum(
    diff(cumulative_population) *
      (head(cumulative_expenditure, -1) + tail(cumulative_expenditure, -1))
  )
}

# Detailed expenditure distribution -----------------------------------------

message("Reading detailed expenditure distributions from: ", gdx_file)

budget <- rgdx.param(gdx_file, "BudgetInitial") %>%
  transmute(
    Region_code = as.character(R),
    Segment = as.character(Seg),
    Expenditure = as.numeric(BudgetInitial)
  )

frequency <- rgdx.param(gdx_file, "FreqSegall_exp") %>%
  transmute(
    Region_code = as.character(R),
    Scenario_id = as.character(Ref),
    Year = as.numeric(as.character(Y)),
    Segment = as.character(Seg),
    Frequency = as.numeric(FreqSegall_exp)
  ) %>%
  filter(
    Scenario_id %in% scenario_grid$Scenario_id,
    Year == comparison_year
  )

available_scenarios <- sort(unique(frequency$Scenario_id))
missing_scenarios <- setdiff(scenario_grid$Scenario_id, available_scenarios)
if (length(missing_scenarios)) {
  stop(
    "FreqSegall_exp is missing scenario(s): ",
    paste(missing_scenarios, collapse = ", ")
  )
}
if (
  any(!is.finite(budget$Expenditure) | budget$Expenditure <= 0) ||
    any(!is.finite(frequency$Frequency) | frequency$Frequency < 0)
) {
  stop("Detailed expenditure and frequency inputs must be finite and valid.")
}

# Use one balanced country panel for every SSP-policy comparison. A complete
# cell contains all expenditure bins available for that country.
budget_bins <- budget %>% count(Region_code, name = "Budget_bins")
frequency_cells <- frequency %>%
  count(Region_code, Scenario_id, Year, name = "Frequency_bins") %>%
  left_join(budget_bins, by = "Region_code") %>%
  filter(Frequency_bins == Budget_bins)
balanced_regions <- frequency_cells %>%
  count(Region_code, name = "Complete_cells") %>%
  filter(Complete_cells == nrow(scenario_grid)) %>%
  pull(Region_code)
if (!length(balanced_regions)) stop("No balanced country panel was found.")

poverty_count <- rgdx.param(gdx_file, "PoVExp") %>%
  transmute(
    Scenario_id = as.character(Ref),
    Region_code = as.character(R),
    Year = as.numeric(as.character(Y)),
    Threshold = as.character(TH),
    People = as.numeric(PoVExp)
  ) %>%
  filter(
    Scenario_id %in% scenario_grid$Scenario_id,
    Region_code %in% balanced_regions,
    Year == comparison_year,
    Threshold == population_threshold
  )

poverty_rate <- rgdx.param(gdx_file, "RoPExp") %>%
  transmute(
    Scenario_id = as.character(Ref),
    Region_code = as.character(R),
    Year = as.numeric(as.character(Y)),
    Threshold = as.character(TH),
    Poverty_rate = as.numeric(RoPExp)
  ) %>%
  filter(
    Scenario_id %in% scenario_grid$Scenario_id,
    Region_code %in% balanced_regions,
    Year == comparison_year,
    Threshold == population_threshold
  )

country_population <- poverty_count %>%
  inner_join(
    poverty_rate,
    by = c("Scenario_id", "Region_code", "Year", "Threshold")
  ) %>%
  transmute(
    Scenario_id,
    Region_code,
    Year,
    Population = People / Poverty_rate
  )

check_grid(
  country_population,
  crossing(
    Scenario_id = scenario_grid$Scenario_id,
    Region_code = balanced_regions,
    Year = comparison_year
  ),
  c("Scenario_id", "Region_code", "Year"),
  "country population used for detailed expenditure weights"
)
if (any(!is.finite(country_population$Population) |
          country_population$Population <= 0)) {
  stop("Recovered country populations must be finite and positive.")
}

frequency_sums <- frequency %>%
  filter(Region_code %in% balanced_regions) %>%
  group_by(Scenario_id, Region_code, Year) %>%
  summarise(Frequency_sum = sum(Frequency), .groups = "drop")
if (any(!is.finite(frequency_sums$Frequency_sum) |
          frequency_sums$Frequency_sum <= 0)) {
  stop("Each country/scenario/year must have a positive frequency sum.")
}

detailed_distribution <- frequency %>%
  filter(Region_code %in% balanced_regions) %>%
  left_join(
    frequency_sums,
    by = c("Scenario_id", "Region_code", "Year")
  ) %>%
  mutate(Population_share = Frequency / Frequency_sum) %>%
  inner_join(budget, by = c("Region_code", "Segment")) %>%
  inner_join(
    country_population,
    by = c("Scenario_id", "Region_code", "Year")
  ) %>%
  mutate(Population_weight = Population_share * Population)

metrics <- detailed_distribution %>%
  group_by(Scenario_id, Year) %>%
  summarise(
    Mean_expenditure = weighted.mean(Expenditure, Population_weight),
    Gini = weighted_gini(Expenditure, Population_weight),
    Population = sum(Population_weight),
    .groups = "drop"
  ) %>%
  inner_join(scenario_grid, by = "Scenario_id") %>%
  mutate(
    SSP = factor(SSP, levels = ssp_levels),
    Policy = factor(Policy, levels = policy_levels),
    Mean_expenditure_thousand = Mean_expenditure / 1000,
    Gini_percent = 100 * Gini,
    Gini_adjusted_welfare = Mean_expenditure * (1 - Gini)
  ) %>%
  group_by(SSP) %>%
  mutate(
    BaU_welfare = Gini_adjusted_welfare[Policy == "BaU"],
    Welfare_index = 100 * Gini_adjusted_welfare / BaU_welfare
  ) %>%
  ungroup() %>%
  arrange(SSP, Policy)

check_grid(
  metrics,
  scenario_grid %>%
    transmute(
      SSP = factor(SSP, levels = ssp_levels),
      Policy = factor(Policy, levels = policy_levels)
    ),
  c("SSP", "Policy"),
  "2050 detailed expenditure metrics"
)
if (
  any(!is.finite(metrics$Mean_expenditure) | metrics$Mean_expenditure <= 0) ||
    any(!is.finite(metrics$Gini) | metrics$Gini < 0 | metrics$Gini > 1) ||
    any(!is.finite(metrics$Welfare_index))
) {
  stop("Detailed expenditure metrics are outside their valid ranges.")
}

# Exact two-factor Shapley decomposition -------------------------------------
# WI/100 = M * D, where M is the mean-expenditure ratio and D is the equality
# ratio (1-G)/(1-G_BaU). Contributions sum exactly to WI - 100.

bau_metrics <- metrics %>%
  filter(Policy == "BaU") %>%
  select(
    SSP,
    BaU_mean_expenditure = Mean_expenditure,
    BaU_gini = Gini
  )

decomposition <- metrics %>%
  filter(Policy != "BaU") %>%
  left_join(bau_metrics, by = "SSP") %>%
  mutate(
    Mean_ratio = Mean_expenditure / BaU_mean_expenditure,
    Equality_ratio = (1 - Gini) / (1 - BaU_gini),
    `Mean consumption expenditure` =
      100 * (Mean_ratio - 1) * (1 + Equality_ratio) / 2,
    `Interpersonal distribution` =
      100 * (Equality_ratio - 1) * (1 + Mean_ratio) / 2,
    Net = `Mean consumption expenditure` + `Interpersonal distribution`,
    Reported_net = Welfare_index - 100
  )

decomposition_error <- max(abs(decomposition$Net - decomposition$Reported_net))
if (!is.finite(decomposition_error) || decomposition_error > 1e-8) {
  stop("The exact decomposition does not reproduce the welfare index.")
}

decomposition_long <- decomposition %>%
  select(
    SSP, Policy,
    `Mean consumption expenditure`, `Interpersonal distribution`
  ) %>%
  pivot_longer(
    c(`Mean consumption expenditure`, `Interpersonal distribution`),
    names_to = "Effect",
    values_to = "Contribution"
  ) %>%
  mutate(
    SSP = factor(SSP, levels = ssp_levels),
    Policy = factor(Policy, levels = policy_non_bau),
    Effect = factor(Effect, levels = names(effect_colours))
  )

net_data <- decomposition %>%
  transmute(
    SSP = factor(SSP, levels = ssp_levels),
    Policy = factor(Policy, levels = policy_non_bau),
    Net,
    Label = sprintf("%+.1f", Net)
  )

# Complete the x-axis grid so excluded SSP3 10k and 15k remain visibly blank.
metric_plot_data <- crossing(
  SSP = factor(ssp_levels, levels = ssp_levels),
  Policy = factor(policy_levels, levels = policy_levels)
) %>%
  left_join(
    metrics %>%
      select(SSP, Policy, Gini_percent, Mean_expenditure_thousand, Welfare_index),
    by = c("SSP", "Policy")
  )

# Export plotted source data for checking and reuse.
metric_source <- metrics %>%
  select(
    SSP, Policy, Scenario_id, Year, Population, Mean_expenditure,
    Mean_expenditure_thousand, Gini_percent, Welfare_index
  ) %>%
  mutate(
    Mean_contribution = NA_real_, Distribution_contribution = NA_real_,
    Net_WI_change = Welfare_index - 100
  )
decomposition_source <- decomposition %>%
  select(
    SSP, Policy, Scenario_id, Year, Population, Mean_expenditure,
    Mean_expenditure_thousand, Gini_percent, Welfare_index,
    `Mean consumption expenditure`, `Interpersonal distribution`, Net
  ) %>%
  rename(
    Mean_contribution = `Mean consumption expenditure`,
    Distribution_contribution = `Interpersonal distribution`,
    Net_WI_change = Net
  )
write.csv(
  metric_source %>%
    select(-Mean_contribution, -Distribution_contribution, -Net_WI_change) %>%
    left_join(
      decomposition_source %>%
        select(
          SSP, Policy, Mean_contribution, Distribution_contribution,
          Net_WI_change
        ),
      by = c("SSP", "Policy")
    ),
  paste0(output_stem, "_data.csv"),
  row.names = FALSE
)

# Figure style ---------------------------------------------------------------

theme_nature <- function() {
  theme_classic(base_family = "Arial", base_size = 8) +
    theme(
      text = element_text(colour = ink),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 7.0, colour = ink),
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
      legend.key.width = grid::unit(5.5, "mm"),
      legend.spacing.x = grid::unit(0.8, "mm"),
      legend.margin = margin(1, 0, 0, 0, unit = "mm"),
      plot.margin = margin(4, 4, 3, 4, unit = "mm")
    )
}

metric_panel <- function(data, y, y_label, reference = NULL, accuracy = 0.1) {
  panel <- ggplot(
    data,
    aes(
      x = Policy,
      y = .data[[y]],
      colour = SSP,
      shape = SSP,
      group = SSP
    )
  )
  if (!is.null(reference)) {
    panel <- panel + geom_hline(
      yintercept = reference,
      linetype = "22",
      colour = "#8A8A8A",
      linewidth = 0.28
    )
  }
  panel +
    geom_line(linewidth = 0.60, na.rm = TRUE) +
    geom_point(size = 2.05, stroke = 0.55, na.rm = TRUE) +
    scale_colour_manual(
      values = ssp_colours,
      breaks = ssp_levels,
      drop = FALSE,
      guide = guide_legend(title = NULL, nrow = 1)
    ) +
    scale_shape_manual(
      values = ssp_shapes,
      breaks = ssp_levels,
      drop = FALSE,
      guide = guide_legend(title = NULL, nrow = 1)
    ) +
    scale_x_discrete(labels = policy_labels, drop = FALSE) +
    scale_y_continuous(
      labels = label_number(accuracy = accuracy),
      expand = expansion(mult = c(0.10, 0.12))
    ) +
    labs(x = NULL, y = y_label) +
    theme_nature() +
    theme(
      legend.position = "top",
      legend.justification = "center",
      axis.text.x = element_text(angle = 32, hjust = 1, vjust = 1)
    )
}

# Panels ---------------------------------------------------------------------

p_a <- metric_panel(
  metric_plot_data,
  "Gini_percent",
  "Global interpersonal\nexpenditure Gini (%)",
  accuracy = 0.1
)

p_b <- metric_panel(
  metric_plot_data,
  "Mean_expenditure_thousand",
  expression(atop(
    "Mean annual consumption expenditure",
    paste("(10"^3, " US$ person"^{-1}, " yr"^{-1}, ")")
  )),
  accuracy = 0.1
)

p_c <- metric_panel(
  metric_plot_data,
  "Welfare_index",
  "Welfare index\n(BaU = 100)",
  reference = 100,
  accuracy = 0.1
)

p_d <- ggplot(
  decomposition_long,
  aes(x = Policy, y = Contribution, fill = Effect)
) +
  geom_hline(yintercept = 0, colour = ink, linewidth = 0.32) +
  geom_col(width = 0.62, colour = "white", linewidth = 0.16) +
  geom_point(
    data = net_data,
    aes(x = Policy, y = Net, shape = "Net change"),
    inherit.aes = FALSE,
    size = 2.30,
    stroke = 0.62,
    fill = "white",
    colour = ink
  ) +
  geom_text(
    data = net_data,
    aes(
      x = Policy,
      y = Net,
      label = Label,
      vjust = ifelse(Net >= 0, -0.75, 1.45)
    ),
    inherit.aes = FALSE,
    family = "Arial",
    size = 2.05,
    colour = ink
  ) +
  facet_wrap(~SSP, nrow = 1, drop = FALSE) +
  scale_x_discrete(labels = policy_labels[policy_non_bau], drop = FALSE) +
  scale_fill_manual(
    values = effect_colours,
    breaks = names(effect_colours),
    drop = FALSE
  ) +
  scale_shape_manual(values = c("Net change" = 23)) +
  scale_y_continuous(
    labels = label_number(accuracy = 1),
    expand = expansion(mult = c(0.15, 0.20))
  ) +
  labs(
    x = NULL,
    y = "Contribution to WI change relative to BaU\n(percentage points)",
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
    strip.background = element_blank(),
    strip.text = element_text(size = 8.0, face = "bold", margin = margin(b = 1.5))
  )

top_row <- (
  (p_a | p_b | p_c) + plot_layout(guides = "collect")
) &
  theme(
    legend.position = "top",
    legend.justification = "center",
    legend.margin = margin(0, 0, 1.5, 0, unit = "mm")
  )

figure <- (
  top_row / p_d +
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

ggsave(
  filename = paste0(output_stem, ".png"),
  plot = figure,
  device = "png",
  type = "cairo",
  width = 180,
  height = 155,
  units = "mm",
  dpi = 400,
  bg = "white",
  limitsize = FALSE
)

message("Created: ", paste0(output_stem, ".png"))
message("Created: ", paste0(output_stem, "_data.csv"))
message("Balanced detailed-expenditure countries: ", length(balanced_regions))
message(
  "Within-country frequency-sum range before normalisation: ",
  sprintf(
    "%.6f to %.6f",
    min(frequency_sums$Frequency_sum),
    max(frequency_sums$Frequency_sum)
  )
)
message(
  "Maximum exact welfare-decomposition error: ",
  format(decomposition_error, scientific = TRUE)
)
print(
  metrics %>%
    select(SSP, Policy, Mean_expenditure, Gini_percent, Welfare_index)
)

)------------------------",
  "Sensitivity_welfare_vs_cumulative_consumption_loss_2050.R" = r"------------------------(
# Welfare versus global cumulative consumption loss in 2050 -----------------
#
# The x axis is the global detailed-expenditure welfare index (each SSP's BaU
# equals 100). The y axis is the model's directly reported World cumulative
# consumption-loss rate in 2050 (5% discount rate). Only mitigation policies
# are plotted: no-aid Default and the 10k/15k/20k aid thresholds. SSP3 10k and
# 15k are excluded. Lines span the policies within each SSP and make the
# near-horizontal welfare gains from aid visible.

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
analysis_dir <- file.path(project_root, "output", "Figure", "analysis")
wi_file <- file.path(
  analysis_dir,
  "09_sensitivity_inequality_welfare_2050_SSP1_SSP3_data.csv"
)
output_stem <- file.path(
  analysis_dir,
  "10_welfare_vs_cumulative_consumption_loss_2050"
)
dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(wi_file)) {
  stop(
    "Missing welfare-index data: ", wi_file, "\n",
    "Run Sensitivity_inequality_welfare_2050_SSP1_SSP3.R first."
  )
}

# Settings ------------------------------------------------------------------

comparison_year <- 2050
loss_variable <- "Pol_Cos_Cns_Los_rat_NPV_5pc"
ssp_levels <- paste0("SSP", 1:3)
policy_levels <- c("Default", "10th", "15th", "20th")
policy_labels <- c(
  "Default" = "Default (no aid)",
  "10th" = "10k USD",
  "15th" = "15k USD",
  "20th" = "20k USD"
)
ssp_colours <- c(
  "SSP1" = "#0072B2",
  "SSP2" = "#D55E00",
  "SSP3" = "#009E73"
)
policy_shapes <- c("Default" = 1, "10th" = 16, "15th" = 17, "20th" = 15)
ink <- "#252525"

scenario_grid <- tidyr::crossing(
  SSP = ssp_levels,
  Policy = policy_levels
) %>%
  mutate(
    Scenario_id = if_else(
      Policy == "Default",
      paste0(SSP, "_400C_2030CP_NoCC_No"),
      paste0(SSP, "_400C_2030CP_", Policy, "_NoCC_No")
    )
  ) %>%
  filter(!(SSP == "SSP3" & Policy %in% c("10th", "15th")))

# Welfare index --------------------------------------------------------------

wi_data <- read.csv(wi_file, check.names = FALSE) %>%
  as_tibble() %>%
  filter(Year == comparison_year, Policy %in% policy_levels) %>%
  transmute(
    SSP = as.character(SSP),
    Policy = as.character(Policy),
    Scenario_id = as.character(Scenario_id),
    Welfare_index = as.numeric(Welfare_index)
  )

if (nrow(wi_data) != nrow(scenario_grid) ||
    anyDuplicated(wi_data[c("SSP", "Policy")]) ||
    any(!is.finite(wi_data$Welfare_index))) {
  stop("The 2050 welfare-index input is incomplete or invalid.")
}
if (nrow(anti_join(scenario_grid, wi_data, by = c("SSP", "Policy", "Scenario_id")))) {
  stop("The welfare-index scenarios do not match the expected scenario grid.")
}

# Global cumulative consumption loss ----------------------------------------

message("Reading IAMC_template from: ", gdx_file)
raw <- rgdx.param(gdx_file, "IAMC_template")
required_columns <- c("SCENARIO", "REMF", "VEMF", "YEMF", "IAMC_Template")
if (!all(required_columns %in% names(raw))) {
  stop("IAMC_template does not have the expected columns.")
}

loss_data <- raw %>%
  transmute(
    Scenario_id = as.character(SCENARIO),
    Region = as.character(REMF),
    Variable = as.character(VEMF),
    Year = as.numeric(as.character(YEMF)),
    Cumulative_consumption_loss_pct = as.numeric(IAMC_Template)
  ) %>%
  filter(
    Scenario_id %in% scenario_grid$Scenario_id,
    Region == "World",
    Variable == loss_variable,
    Year == comparison_year
  )

if (nrow(loss_data) != nrow(scenario_grid) ||
    anyDuplicated(loss_data$Scenario_id) ||
    any(!is.finite(loss_data$Cumulative_consumption_loss_pct))) {
  stop("The World cumulative-consumption-loss input is incomplete or invalid.")
}

plot_data <- scenario_grid %>%
  left_join(
    wi_data %>% select(SSP, Policy, Scenario_id, Welfare_index),
    by = c("SSP", "Policy", "Scenario_id")
  ) %>%
  left_join(
    loss_data %>% select(Scenario_id, Cumulative_consumption_loss_pct),
    by = "Scenario_id"
  ) %>%
  mutate(
    SSP = factor(SSP, levels = ssp_levels),
    Policy = factor(Policy, levels = policy_levels)
  )

if (nrow(plot_data) != nrow(scenario_grid) ||
    any(!is.finite(plot_data$Welfare_index)) ||
    any(!is.finite(plot_data$Cumulative_consumption_loss_pct))) {
  stop("The combined plotting data are incomplete or invalid.")
}

# Quantify how little global loss changes when aid is introduced relative to
# the no-aid Default case within the same SSP.
loss_diagnostics <- plot_data %>%
  group_by(SSP) %>%
  mutate(
    Default_loss = Cumulative_consumption_loss_pct[Policy == "Default"],
    Loss_difference_from_default =
      Cumulative_consumption_loss_pct - Default_loss
  ) %>%
  ungroup()
maximum_loss_difference <- max(
  abs(loss_diagnostics$Loss_difference_from_default), na.rm = TRUE
)

write.csv(
  loss_diagnostics %>%
    mutate(
      SSP = as.character(SSP),
      Policy = as.character(Policy)
    ),
  paste0(output_stem, "_data.csv"),
  row.names = FALSE
)

# Figure ---------------------------------------------------------------------

theme_nature <- function() {
  theme_classic(base_family = "Arial", base_size = 8) +
    theme(
      text = element_text(colour = ink),
      axis.title = element_text(size = 8.4),
      axis.text = element_text(size = 7.5, colour = ink),
      axis.ticks = element_line(colour = ink, linewidth = 0.25),
      axis.ticks.length = grid::unit(1.2, "mm"),
      axis.line = element_blank(),
      panel.border = element_rect(fill = NA, colour = ink, linewidth = 0.32),
      panel.grid.major = element_line(colour = "#E2E2E2", linewidth = 0.22),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      legend.position = "bottom",
      legend.justification = "center",
      legend.box = "vertical",
      legend.title = element_text(size = 7.3, face = "plain"),
      legend.text = element_text(size = 7.1),
      legend.key.height = grid::unit(3.5, "mm"),
      legend.key.width = grid::unit(6.0, "mm"),
      legend.spacing.x = grid::unit(1.2, "mm"),
      plot.margin = margin(4, 5, 3, 4, unit = "mm")
    )
}

p <- ggplot(
  plot_data,
  aes(
    x = Welfare_index,
    y = Cumulative_consumption_loss_pct,
    colour = SSP,
    shape = Policy
  )
) +
  geom_vline(
    xintercept = 100,
    linetype = "22",
    colour = "#8A8A8A",
    linewidth = 0.30
  ) +
  geom_line(
    aes(group = SSP),
    linewidth = 0.64,
    alpha = 0.86
  ) +
  geom_point(size = 2.75, stroke = 0.72) +
  scale_colour_manual(
    values = ssp_colours,
    breaks = ssp_levels,
    drop = FALSE,
    name = "SSP"
  ) +
  scale_shape_manual(
    values = policy_shapes,
    breaks = policy_levels,
    labels = policy_labels,
    drop = FALSE,
    name = "Policy"
  ) +
  scale_x_continuous(
    labels = label_number(accuracy = 1),
    expand = expansion(mult = c(0.06, 0.08))
  ) +
  scale_y_continuous(
    labels = label_number(accuracy = 0.1),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    x = "Welfare index in 2050 (BaU = 100)",
    y = "Global cumulative consumption loss in 2050 (%)"
  ) +
  guides(
    colour = guide_legend(order = 1, nrow = 1),
    shape = guide_legend(order = 2, nrow = 1)
  ) +
  theme_nature()

ggsave(
  filename = paste0(output_stem, ".png"),
  plot = p,
  device = "png",
  type = "cairo",
  width = 150,
  height = 112,
  units = "mm",
  dpi = 400,
  bg = "white",
  limitsize = FALSE
)

message("Created: ", paste0(output_stem, ".png"))
message("Created: ", paste0(output_stem, "_data.csv"))
message(
  "Maximum absolute loss difference from Default within an SSP (percentage points): ",
  sprintf("%.8f", maximum_loss_difference)
)
print(
  loss_diagnostics %>%
    select(
      SSP, Policy, Welfare_index, Cumulative_consumption_loss_pct,
      Loss_difference_from_default
    ) %>%
    arrange(SSP, Policy),
  n = Inf
)

)------------------------",
  "SSP2_Gini_exp_change_boxplots_R5_2050.R" = r"------------------------(
# SSP2 country expenditure-Gini changes from BaU in 2050 --------------------
#
# Country-level Gini_exp changes are shown as boxplots for the World and the
# five R5 aggregates. The policy cases are SSP2 Default (no aid) and SSP2 Aid
# under the 10k, 15k, and 20k USD thresholds. Gini_exp is reported on a 0-1
# scale in the GDX and converted here to changes on a 0-100 index-point scale.

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
region_map_file <- file.path(
  project_root, "data", "AR6database", "RegionmapRagg.map"
)
output_dir <- file.path(project_root, "output", "Figure", "analysis")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_stem <- file.path(
  output_dir, "12_SSP2_Gini_exp_change_boxplots_R5_2050"
)

if (!file.exists(region_map_file)) {
  stop("Missing country-to-R5 mapping: ", region_map_file)
}

# Settings ------------------------------------------------------------------

comparison_year <- 2050
scenario_codes <- c(
  BaU = "SSP2_BaU_NoCC_No",
  Def = "SSP2_400C_2030CP_NoCC_No",
  `10th` = "SSP2_400C_2030CP_10th_NoCC_No",
  `15th` = "SSP2_400C_2030CP_15th_NoCC_No",
  `20th` = "SSP2_400C_2030CP_20th_NoCC_No"
)
policy_levels <- c("Def", "10th", "15th", "20th")
policy_labels <- c(
  "Def" = "Default (no aid)",
  "10th" = "Aid (10k USD)",
  "15th" = "Aid (15k USD)",
  "20th" = "Aid (20k USD)"
)
policy_colours <- c(
  "Def" = "#8C6BB1",
  "10th" = "#0072B2",
  "15th" = "#D55E00",
  "20th" = "#009E73"
)

r5_codes <- c("R5OECD90+EU", "R5REF", "R5ASIA", "R5MAF", "R5LAM")
r5_labels <- c(
  "R5OECD90+EU" = "OECD90 + EU",
  "R5REF" = "Reforming economies",
  "R5ASIA" = "Asia",
  "R5MAF" = "Middle East & Africa",
  "R5LAM" = "Latin America"
)
aggregate_levels <- c("World", unname(r5_labels[r5_codes]))
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

# Country-to-R5 mapping ------------------------------------------------------

region_map_raw <- read.table(
  region_map_file,
  header = FALSE,
  quote = "\"",
  stringsAsFactors = FALSE,
  fill = TRUE,
  comment.char = ""
)
if (ncol(region_map_raw) < 3L) {
  stop("RegionmapRagg.map does not have the expected three columns.")
}

r5_map <- region_map_raw %>%
  transmute(
    ISO3 = as.character(V1),
    R5_code = as.character(V3)
  ) %>%
  filter(R5_code %in% r5_codes) %>%
  distinct(ISO3, R5_code)

if (anyDuplicated(r5_map$ISO3)) {
  duplicates <- unique(r5_map$ISO3[duplicated(r5_map$ISO3)])
  stop("Countries assigned to multiple R5 groups: ", paste(duplicates, collapse = ", "))
}

# Country Gini changes -------------------------------------------------------

message("Reading Gini_exp from: ", gdx_file)
gini <- rgdx.param(gdx_file, "Gini_exp") %>%
  transmute(
    Scenario_id = as.character(Ref),
    ISO3 = as.character(R),
    Year = as.numeric(as.character(Y)),
    Gini = as.numeric(Gini_exp)
  ) %>%
  filter(
    Scenario_id %in% unname(scenario_codes),
    Year == comparison_year
  ) %>%
  mutate(
    Scenario = names(scenario_codes)[match(Scenario_id, scenario_codes)]
  )

country_codes <- sort(unique(gini$ISO3))
check_grid(
  gini,
  crossing(
    Scenario_id = unname(scenario_codes),
    ISO3 = country_codes,
    Year = comparison_year
  ),
  c("Scenario_id", "ISO3", "Year"),
  "2050 country Gini_exp"
)
if (any(!is.finite(gini$Gini) | gini$Gini < 0 | gini$Gini > 1)) {
  stop("Gini_exp must be finite and on the 0-1 scale.")
}

unmapped_countries <- setdiff(country_codes, r5_map$ISO3)
if (length(unmapped_countries)) {
  stop(
    "Gini_exp countries missing from the R5 mapping: ",
    paste(unmapped_countries, collapse = ", ")
  )
}

country_changes <- gini %>%
  select(ISO3, Scenario, Gini) %>%
  pivot_wider(names_from = Scenario, values_from = Gini) %>%
  transmute(
    ISO3,
    Def = 100 * (Def - BaU),
    `10th` = 100 * (`10th` - BaU),
    `15th` = 100 * (`15th` - BaU),
    `20th` = 100 * (`20th` - BaU)
  ) %>%
  pivot_longer(
    all_of(policy_levels),
    names_to = "Policy",
    values_to = "Gini_change"
  ) %>%
  left_join(r5_map, by = "ISO3") %>%
  mutate(
    Policy = factor(Policy, levels = policy_levels),
    R5_region = unname(r5_labels[R5_code])
  )

if (any(!is.finite(country_changes$Gini_change)) ||
    any(is.na(country_changes$R5_region))) {
  stop("Country Gini changes or R5 classifications are incomplete.")
}

plot_data <- bind_rows(
  country_changes %>% mutate(Aggregate = "World"),
  country_changes %>% mutate(Aggregate = R5_region)
) %>%
  mutate(Aggregate = factor(Aggregate, levels = aggregate_levels))

expected_plot_rows <- length(country_codes) * length(policy_levels) * 2L
if (nrow(plot_data) != expected_plot_rows) {
  stop("The World-plus-R5 plotting data have an unexpected number of rows.")
}

write.csv(
  plot_data %>%
    mutate(
      Policy = as.character(Policy),
      Aggregate = as.character(Aggregate)
    ) %>%
    select(ISO3, R5_code, R5_region, Aggregate, Policy, Gini_change),
  paste0(output_stem, "_data.csv"),
  row.names = FALSE
)

summary_data <- plot_data %>%
  group_by(Aggregate, Policy) %>%
  summarise(
    Countries = n(),
    Median = median(Gini_change),
    Q1 = quantile(Gini_change, 0.25),
    Q3 = quantile(Gini_change, 0.75),
    Minimum = min(Gini_change),
    Maximum = max(Gini_change),
    .groups = "drop"
  )
write.csv(
  summary_data,
  paste0(output_stem, "_summary.csv"),
  row.names = FALSE
)

# Figure ---------------------------------------------------------------------

theme_nature <- function() {
  theme_classic(base_family = "Arial", base_size = 8) +
    theme(
      text = element_text(colour = ink),
      axis.title = element_text(size = 8.3),
      axis.text = element_text(size = 7.3, colour = ink),
      axis.ticks = element_line(colour = ink, linewidth = 0.25),
      axis.ticks.length = grid::unit(1.1, "mm"),
      axis.line = element_blank(),
      panel.border = element_rect(fill = NA, colour = ink, linewidth = 0.32),
      panel.grid.major.y = element_line(colour = "#E2E2E2", linewidth = 0.22),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = 8.0, margin = margin(b = 1.5)),
      panel.spacing.x = grid::unit(3.0, "mm"),
      panel.spacing.y = grid::unit(4.0, "mm"),
      legend.position = "bottom",
      legend.justification = "center",
      legend.title = element_blank(),
      legend.text = element_text(size = 7.2),
      legend.key.height = grid::unit(3.5, "mm"),
      legend.key.width = grid::unit(6.0, "mm"),
      plot.margin = margin(4, 5, 3, 4, unit = "mm")
    )
}

p <- ggplot(
  plot_data,
  aes(x = Policy, y = Gini_change, fill = Policy)
) +
  geom_hline(
    yintercept = 0,
    linetype = "12",
    colour = ink,
    linewidth = 0.30
  ) +
  geom_boxplot(
    width = 0.62,
    linewidth = 0.36,
    colour = ink,
    outlier.shape = 16,
    outlier.size = 1.15,
    outlier.stroke = 0,
    outlier.colour = ink
  ) +
  facet_wrap(~Aggregate, ncol = 3, drop = FALSE) +
  scale_fill_manual(
    values = policy_colours,
    breaks = policy_levels,
    labels = policy_labels,
    drop = FALSE
  ) +
  scale_x_discrete(
    labels = c(
      "Def" = "Default",
      "10th" = "10k",
      "15th" = "15k",
      "20th" = "20k"
    ),
    drop = FALSE
  ) +
  scale_y_continuous(
    labels = label_number(accuracy = 0.1),
    expand = expansion(mult = c(0.08, 0.12))
  ) +
  labs(
    x = NULL,
    y = "Change in expenditure Gini from BaU\n(0-100 index points)"
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  theme_nature()

ggsave(
  filename = paste0(output_stem, ".png"),
  plot = p,
  device = "png",
  type = "cairo",
  width = 180,
  height = 125,
  units = "mm",
  dpi = 400,
  bg = "white",
  limitsize = FALSE
)

message("Created: ", paste0(output_stem, ".png"))
message("Created: ", paste0(output_stem, "_data.csv"))
message("Created: ", paste0(output_stem, "_summary.csv"))
message("Countries included: ", length(country_codes))
print(summary_data, n = Inf)

)------------------------"
)

generate_supplementary_figures <- function(project_root, output_dirs) {
  build_figure(
    project_root,
    output_dirs,
    script_name = "Sensitivity_GDP_per_capita_threshold_classification_2025.R",
    stop_pattern = "^output_dir <-",
    plot_object = "p",
    filename_stem =
      "SupplementaryFig1_GDP_per_capita_threshold_classification_2025",
    width_mm = 180,
    height_mm = 125,
    script_registry = supplementary_script_registry
  )

  build_figure(
    project_root,
    output_dirs,
    script_name = "Transfer_emissions_efficiency_timing.R",
    stop_pattern = "^save_png\\($",
    plot_object = "p_emissions_combined",
    filename_stem =
      "SupplementaryFig2_transfer_emissions_equivalent_combined_2030_2050",
    width_mm = 180,
    height_mm = 88,
    script_registry = supplementary_script_registry
  )

  build_figure(
    project_root,
    output_dirs,
    script_name = "SI_primary_energy_supply_3x3.R",
    stop_pattern = "^ggsave\\(",
    plot_object = "figure",
    filename_stem = "SupplementaryFig3_primary_energy_supply_2020_2050",
    width_mm = 180,
    height_mm = 180,
    script_registry = supplementary_script_registry
  )

  build_figure(
    project_root,
    output_dirs,
    script_name = "SI_cumulative_consumption_gdp_loss_2050.R",
    stop_pattern = "^output_base <-",
    plot_object = "p_si",
    filename_stem =
      "SupplementaryFig4_cumulative_consumption_gdp_loss_2050",
    width_mm = 180,
    height_mm = 92,
    script_registry = supplementary_script_registry
  )

  build_figure(
    project_root,
    output_dirs,
    script_name = "SSP2_additional_poverty_all_lines_2050.R",
    stop_pattern = "^ggsave\\(",
    plot_object = "p",
    filename_stem =
      "SupplementaryFig5_SSP2_additional_poverty_all_lines_2050",
    width_mm = 180,
    height_mm = function(env) env$figure_height,
    script_registry = supplementary_script_registry
  )

  build_figure(
    project_root,
    output_dirs,
    script_name = "Sensitivity_world_population_GDP_SSP1_SSP3.R",
    stop_pattern = "^output_dir <-",
    plot_object = "p",
    filename_stem =
      "SupplementaryFig6_sensitivity_world_population_GDP_SSP1_SSP3",
    width_mm = 180,
    height_mm = 82,
    script_registry = supplementary_script_registry
  )

  build_figure(
    project_root,
    output_dirs,
    script_name = "Sensitivity_provider_carbon_revenue_SSP1_SSP3.R",
    stop_pattern = "^output_dir <-",
    plot_object = "p",
    filename_stem =
      "SupplementaryFig7_sensitivity_provider_carbon_revenue_SSP1_SSP3",
    width_mm = 180,
    height_mm = 190,
    script_registry = supplementary_script_registry
  )

  build_figure(
    project_root,
    output_dirs,
    script_name = "Sensitivity_cumulative_consumption_GDP_loss_thresholds.R",
    stop_pattern = "^output_dir <-",
    plot_object = "p",
    filename_stem = paste0(
      "SupplementaryFig8_",
      "sensitivity_cumulative_consumption_GDP_loss_thresholds"
    ),
    width_mm = 180,
    height_mm = 142,
    script_registry = supplementary_script_registry
  )

  build_figure(
    project_root,
    output_dirs,
    script_name = "Sensitivity_poverty_population_SSP1_SSP3.R",
    stop_pattern = "^output_dir <-",
    plot_object = "p",
    filename_stem =
      "SupplementaryFig9_sensitivity_poverty_population_SSP1_SSP3",
    width_mm = 180,
    height_mm = 158,
    script_registry = supplementary_script_registry
  )

  build_figure(
    project_root,
    output_dirs,
    script_name = "Sensitivity_lorenz_curves_2050_SSP1_SSP3.R",
    stop_pattern = "^output_dir <-",
    plot_object = "plot_lorenz",
    filename_stem =
      "SupplementaryFig10_sensitivity_lorenz_curves_2050_SSP1_SSP3",
    width_mm = 180,
    height_mm = 83,
    script_registry = supplementary_script_registry
  )

  build_figure(
    project_root,
    output_dirs,
    script_name = "Sensitivity_inequality_welfare_2050_SSP1_SSP3.R",
    stop_pattern = "^ggsave\\(",
    plot_object = "figure",
    filename_stem =
      "SupplementaryFig11_sensitivity_inequality_welfare_2050_SSP1_SSP3",
    width_mm = 180,
    height_mm = 155,
    script_registry = supplementary_script_registry
  )

  build_figure(
    project_root,
    output_dirs,
    script_name = "Sensitivity_welfare_vs_cumulative_consumption_loss_2050.R",
    stop_pattern = "^ggsave\\(",
    plot_object = "p",
    filename_stem = paste0(
      "SupplementaryFig12_",
      "welfare_vs_cumulative_consumption_loss_2050"
    ),
    width_mm = 150,
    height_mm = 112,
    script_registry = supplementary_script_registry
  )

  build_figure(
    project_root,
    output_dirs,
    script_name = "SSP2_Gini_exp_change_boxplots_R5_2050.R",
    stop_pattern = "^ggsave\\(",
    plot_object = "p",
    filename_stem =
      "SupplementaryFig13_SSP2_Gini_exp_change_boxplots_R5_2050",
    width_mm = 180,
    height_mm = 125,
    script_registry = supplementary_script_registry
  )

  invisible(NULL)
}
