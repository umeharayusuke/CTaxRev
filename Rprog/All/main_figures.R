build_emission_panels <- function(project_root) {
  old_dir <- getwd()
  on.exit(setwd(old_dir), add = TRUE)
  setwd(project_root)
  write.csv <- function(...) invisible(NULL)
  project_library_candidates <- c(file.path(getwd(), "Rlib"), file.path(getwd(), "..", "Rlib"), file.path(getwd(), "..", "..",
      "Rlib"), "C:/ENVI5809/R_library", "C:/Users/umeha/AppData/Local/R/win-library/4.5")
  project_library_candidates <- project_library_candidates[dir.exists(project_library_candidates)]
  if (length(project_library_candidates) > 0) {
      .libPaths(unique(c(project_library_candidates, .libPaths())))
  }
  suppressPackageStartupMessages({
      library(dplyr)
      library(tidyr)
      library(readr)
      library(readxl)
      library(ggplot2)
      library(gdxrrw)
      library(patchwork)
      library(scales)
  })
  find_project_dir <- function(start_dir) {
      candidate <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)
      repeat {
          if (file.exists(file.path(candidate, "CTaxRev.Rproj")))
              return(candidate)
          parent <- dirname(candidate)
          if (identical(parent, candidate))
              stop("Could not find the CTaxRev project root.")
          candidate <- parent
      }
  }
  command_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", command_args, value = TRUE)
  start_dir <- if (length(file_arg) > 0) {
      dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE))
  } else {
      getwd()
  }
  project_dir <- find_project_dir(start_dir)
  data_dir <- file.path(project_dir, "data", "All")
  gdx_file <- file.path(data_dir, "global_17_IAMC.gdx")
  ar6_meta_file <- file.path(data_dir, "AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx")
  ar6_world_file <- file.path(data_dir, "AR6_Scenarios_Database_World_v1.1.csv")
  output_dir <- file.path(project_dir, "output", "Figure")
  csv_dir <- file.path(output_dir, "csv")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(csv_dir, showWarnings = FALSE, recursive = TRUE)
  gams_candidates <- unique(c(Sys.getenv("GAMS_SYSDIR", unset = ""), "C:/GAMS/34", "C:/GAMS/win64/26.1"))
  gams_candidates <- gams_candidates[nzchar(gams_candidates)]
  gams_dir <- gams_candidates[dir.exists(gams_candidates)][1]
  if (is.na(gams_dir)) {
      stop("GAMS system directory was not found; gdxrrw cannot read the GDX.")
  }
  igdx(gams_dir)
  scenario_names <- c(SSP2_BaU_NoCC_No = "BaU", SSP2_400C_2030CP_NoCC_No = "Def", SSP2_400C_2030CP_15th_NoCC_No = "Aid")
  scenario_colors <- c(BaU = "#252525", Def = "#B6573F", Aid = "#3F6C8E")
  scenario_shapes <- c(BaU = 16, Def = 1, Aid = 15)
  scenario_linetypes <- c(BaU = "solid", Def = "22", Aid = "solid")
  ar6_colors <- c(C1 = "#8AB9D1", C2 = "#E7B65A")
  figure_font <- "Arial"
  year_breaks <- seq(2020, 2050, 5)
  vec_emissions <- c("Emi_CO2_AFO", "Emi_CO2_Ene_Sup", "Emi_CO2_Ene_Dem", "Emi_CO2_Ind_Pro", "Emi_CO2_Pro_Use", "Emi_CO2_Cap_and_Rem")
  CO2Order <- c("Energy supply", "Energy demand", "Industrial processes", "Product use", "AFOLU", "Capture and removal")
  col_emissions <- c(`Energy supply` = "#70C2A5", `Energy demand` = "#90A1CC", `Industrial processes` = "#954FA4", `Product use` = "#FDFE22",
      AFOLU = "#F58C60", `Capture and removal` = "#437FB9")
  CLP <- names(scenario_names)
  Region <- c("Rprovider15th", "Rrecipient15th")
  required_files <- c(gdx_file, ar6_meta_file, ar6_world_file)
  if (any(!file.exists(required_files))) stop("Missing file(s): ", paste(required_files[!file.exists(required_files)], collapse = ", "))
  ar6_category <- read_xlsx(ar6_meta_file, sheet = "meta_Ch3vetted_withclimate") %>% transmute(Case = paste0(Model, "_", Scenario),
      Category) %>% filter(Category %in% c("C1", "C2")) %>% distinct(Case, .keep_all = TRUE)
  ar6_long <- read_csv(ar6_world_file, show_col_types = FALSE) %>% mutate(Case = paste0(Model, "_", Scenario)) %>% inner_join(ar6_category,
      by = "Case", relationship = "many-to-one") %>% filter(Region == "World", Variable %in% c("Emissions|CO2", "Price|Carbon")) %>%
      pivot_longer(cols = matches("^[0-9]{4}$"), names_to = "Year", values_to = "value", names_transform = list(Year = as.numeric)) %>%
      filter(!is.na(value), Year %in% year_breaks) %>% mutate(value = if_else(Variable == "Emissions|CO2", value/1000, value))
  ar6_box <- ar6_long %>% group_by(Variable, Category, Year) %>% summarise(p05 = quantile(value, 0.05, na.rm = TRUE), p25 = quantile(value,
      0.25, na.rm = TRUE), p50 = median(value, na.rm = TRUE), p75 = quantile(value, 0.75, na.rm = TRUE), p95 = quantile(value,
      0.95, na.rm = TRUE), n = sum(is.finite(value)), .groups = "drop") %>% mutate(PlotYear = Year + if_else(Category == "C1",
      -0.7, 0.7))
  iamc <- rgdx.param(gdx_file, "IAMC_template") %>% mutate(Year = as.numeric(as.character(YEMF)), Value = as.numeric(IAMC_Template),
      Scenario = recode(SCENARIO, !!!scenario_names), Scenario = factor(Scenario, levels = c("BaU", "Def", "Aid")))
  aim_emissions <- iamc %>% filter(VEMF == "Emi_CO2", SCENARIO %in% names(scenario_names), REMF %in% c("World", "Rprovider15th",
      "Rrecipient15th"), Year >= 2020, Year <= 2050) %>% transmute(Year, REMF, Scenario, value = Value/1000)
  aim_price <- iamc %>% filter(VEMF == "Prc_Car", SCENARIO %in% names(scenario_names), REMF == "World", Year >= 2020, Year <=
      2050) %>% transmute(Year, Scenario, value = Value)
  df_emissions <- iamc %>% filter(VEMF %in% vec_emissions, SCENARIO %in% CLP, REMF %in% Region) %>% transmute(Year = as.numeric(as.character(YEMF)),
      REMF = recode(REMF, Rprovider15th = "Provider", Rrecipient15th = "Recipient"), SCENARIO = recode(SCENARIO, SSP2_BaU_NoCC_No = "BaU",
          SSP2_400C_2030CP_NoCC_No = "Def", SSP2_400C_2030CP_15th_NoCC_No = "Aid"), VEMF = recode(VEMF, Emi_CO2_AFO = "AFOLU",
          Emi_CO2_Ene_Sup = "Energy supply", Emi_CO2_Ene_Dem = "Energy demand", Emi_CO2_Ind_Pro = "Industrial processes", Emi_CO2_Pro_Use = "Product use",
          Emi_CO2_Cap_and_Rem = "Capture and removal"), value = as.numeric(IAMC_Template)/1000) %>% filter(Year >= 2020, Year <=
      2050) %>% group_by(REMF, SCENARIO, Year, VEMF) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(REMF = factor(REMF, levels = c("Provider", "Recipient")), SCENARIO = factor(SCENARIO, levels = c("BaU", "Def",
          "Aid")), VEMF = factor(VEMF, levels = CO2Order)) %>% complete(REMF, SCENARIO, Year, VEMF, fill = list(value = 0))
  sector_cumulative_change <- df_emissions %>% arrange(REMF, SCENARIO, VEMF, Year) %>% group_by(REMF, SCENARIO, VEMF) %>% summarise(Cumulative = sum((value +
      lead(value))/2 * (lead(Year) - Year), na.rm = TRUE), .groups = "drop") %>% left_join(df_emissions %>% filter(SCENARIO ==
      "BaU") %>% arrange(REMF, VEMF, Year) %>% group_by(REMF, VEMF) %>% summarise(BaU = sum((value + lead(value))/2 * (lead(Year) -
      Year), na.rm = TRUE), .groups = "drop"), by = c("REMF", "VEMF")) %>% filter(SCENARIO != "BaU") %>% mutate(Change = Cumulative -
      BaU, SCENARIO = factor(SCENARIO, levels = c("Aid", "Def")))
  cumulative_emissions <- iamc %>% filter(VEMF == "Emi_CO2_Cum", SCENARIO %in% CLP, REMF %in% Region, Year %in% c(2020, 2050)) %>%
      transmute(REMF = recode(REMF, Rprovider15th = "Provider", Rrecipient15th = "Recipient"), Scenario = recode(SCENARIO,
          !!!scenario_names), Year, value = Value/1000) %>% pivot_wider(names_from = Year, values_from = value, names_prefix = "y") %>%
      mutate(Cumulative = y2050 - y2020, REMF = factor(REMF, levels = c("Provider", "Recipient")), Scenario = factor(Scenario,
          levels = c("BaU", "Def", "Aid"))) %>% group_by(REMF) %>% mutate(BaU = Cumulative[Scenario == "BaU"], Change = Cumulative -
      BaU, Reduction = 100 * (BaU - Cumulative)/BaU) %>% ungroup()
  direct_cumulative_change <- cumulative_emissions %>% filter(Scenario != "BaU") %>% transmute(REMF, SCENARIO = factor(as.character(Scenario),
      levels = c("Aid", "Def")), DirectChange = Change)
  sector_cumulative_change <- sector_cumulative_change %>% group_by(REMF, SCENARIO) %>% mutate(RawNet = sum(Change)) %>% ungroup() %>%
      left_join(direct_cumulative_change, by = c("REMF", "SCENARIO")) %>% mutate(ReconciliationFactor = if_else(abs(RawNet) >
      .Machine$double.eps, DirectChange/RawNet, 1), Change = Change * ReconciliationFactor)
  ar6_source <- ar6_box %>% select(Variable, Category, Year, p05, p25, p50, p75, p95, n) %>% pivot_longer(cols = c(p05, p25,
      p50, p75, p95, n), names_to = "Statistic", values_to = "Value") %>% transmute(Panel = if_else(Variable == "Emissions|CO2",
      "a", "b"), Dataset = "AR6", Region = "World", Scenario = Category, Sector = NA, Year, Statistic, Value, Unit = if_else(Variable ==
      "Emissions|CO2", "Gt CO2 yr-1", "USD t CO2-1"))
  aim_source <- bind_rows(aim_emissions %>% filter(REMF == "World") %>% transmute(Panel = "a", Dataset = "AIM", Region = "World",
      Scenario = as.character(Scenario), Sector = NA, Year, Statistic = "value", Value = value, Unit = "Gt CO2 yr-1"), aim_price %>%
      transmute(Panel = "b", Dataset = "AIM", Region = "World", Scenario = as.character(Scenario), Sector = NA, Year, Statistic = "value",
          Value = value, Unit = "USD t CO2-1"))
  sector_source <- sector_cumulative_change %>% transmute(Panel = "c", Dataset = "AIM", Region = as.character(REMF), Scenario = as.character(SCENARIO),
      Sector = as.character(VEMF), Year = 2050, Statistic = "cumulative_change_from_BaU", Value = Change, Unit = "Gt CO2 (2020-2050)")
  cumulative_source <- cumulative_emissions %>% transmute(Panel = "d", Dataset = "AIM", Region = as.character(REMF), Scenario = as.character(Scenario),
      Sector = NA, Year = 2050, Statistic = "cumulative_2020_2050", Value = Cumulative, Unit = "Gt CO2")
  source_data_file <- file.path(csv_dir, "Emission_CP_sectoral_source_data.csv")
  write.csv(bind_rows(ar6_source, aim_source, sector_source, cumulative_source), source_data_file, row.names = FALSE)
  scenario_scale <- function() {
      scale_color_manual(name = NULL, values = scenario_colors, limits = c("BaU", "Def", "Aid"), drop = FALSE)
  }
  scenario_shape_scale <- function() {
      scale_shape_manual(name = NULL, values = scenario_shapes, limits = c("BaU", "Def", "Aid"), drop = FALSE)
  }
  scenario_linetype_scale <- function() {
      scale_linetype_manual(name = NULL, values = scenario_linetypes, limits = c("BaU", "Def", "Aid"), drop = FALSE)
  }
  ar6_scale <- function() {
      scale_fill_manual(name = "AR6 category", values = ar6_colors, limits = c("C1", "C2"), drop = FALSE)
  }
  x_scale <- function(start_year = 2020) {
      scale_x_continuous(limits = c(start_year, 2050), breaks = seq(start_year, 2050, 5), labels = seq(start_year, 2050, 5),
          expand = expansion(mult = c(0, 0.015)))
  }
  theme_panel <- function() theme_classic(base_size = 7.2, base_family = figure_font) + theme(text = element_text(colour = "#202020"),
      panel.grid = element_blank(), panel.background = element_rect(fill = "white", colour = NA), plot.background = element_rect(fill = "white",
          colour = NA), axis.text = element_text(size = 6.2, colour = "#303030"), axis.title = element_text(size = 6.8), axis.line.x.bottom = element_line(colour = "#202020",
          linewidth = 0.32, lineend = "square"), axis.line.y.left = element_line(colour = "#202020", linewidth = 0.32, lineend = "square"),
      axis.ticks = element_line(colour = "#606060", linewidth = 0.22), axis.ticks.length = grid::unit(1, "mm"), plot.title = element_blank(),
      plot.subtitle = element_blank(), plot.tag = element_text(face = "bold", size = 8.2, colour = "black"), plot.tag.position = c(0,
          1), strip.background = element_blank(), legend.title = element_text(face = "plain", size = 6.4), legend.text = element_text(size = 6),
      legend.key.height = grid::unit(2.8, "mm"), legend.key.width = grid::unit(4.2, "mm"), legend.spacing.x = grid::unit(0.7,
          "mm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(5, 4, 4, 5, unit = "mm"))
  make_ar6_panel <- function(variable, aim_data, y_label, tag, start_year = 2020) {
      range_data <- ar6_box %>% filter(Variable == variable, Year >= start_year)
      aim_plot_data <- aim_data %>% filter(Year >= start_year)
      ggplot() + geom_hline(yintercept = 0, linewidth = 0.28, colour = "#8A8A8A") + geom_ribbon(data = range_data, aes(Year,
          ymin = p05, ymax = p95, fill = Category, group = Category), alpha = 0.14, colour = NA) + geom_ribbon(data = range_data,
          aes(Year, ymin = p25, ymax = p75, fill = Category, group = Category), alpha = 0.34, colour = NA) + geom_line(data = filter(range_data,
          Category == "C1"), aes(Year, p50), colour = ar6_colors[["C1"]], linewidth = 0.38, linetype = "13") + geom_line(data = filter(range_data,
          Category == "C2"), aes(Year, p50), colour = ar6_colors[["C2"]], linewidth = 0.38, linetype = "13") + geom_line(data = aim_plot_data,
          aes(Year, value, colour = Scenario, linetype = Scenario, group = Scenario), linewidth = 0.65) + geom_point(data = filter(aim_plot_data,
          Year %in% year_breaks), aes(Year, value, colour = Scenario, shape = Scenario), size = 1.45, stroke = 0.45) + ar6_scale() +
          scenario_scale() + scenario_shape_scale() + scenario_linetype_scale() + x_scale(start_year) + labs(tag = tag, x = NULL,
          y = y_label) + guides(fill = guide_legend(order = 1, nrow = 1, override.aes = list(alpha = 0.3)), colour = guide_legend(order = 2,
          nrow = 1, override.aes = list(linewidth = 0.65, size = 1.5)), shape = guide_legend(order = 2, nrow = 1), linetype = guide_legend(order = 2,
          nrow = 1)) + theme_panel()
  }
  p_sector_change <- ggplot(sector_cumulative_change, aes(y = SCENARIO, x = Change, fill = VEMF)) + geom_vline(xintercept = 0,
      linewidth = 0.32, colour = "#505050") + geom_col(width = 0.58, position = position_stack(reverse = TRUE), colour = "white",
      linewidth = 0.18) + facet_wrap(vars(REMF), ncol = 1, strip.position = "top") + scale_fill_manual(name = NULL, values = col_emissions,
      limits = CO2Order, breaks = CO2Order, drop = FALSE) + scale_x_continuous(labels = label_number(accuracy = 1)) + labs(tag = "c",
      x = expression("Change in cumulative emissions (Gt CO"[2] * ")"), y = NULL) + guides(fill = guide_legend(order = 3, nrow = 2,
      byrow = TRUE)) + theme_panel() + theme(panel.grid.major.y = element_blank(), strip.text = element_text(face = "bold",
      size = 6.5, hjust = 0), axis.text.y = element_text(face = "bold"), plot.margin = margin(5, 4, 4, 5, unit = "mm"))
  cumulative_plot_data <- cumulative_emissions %>% mutate(RegionY = if_else(REMF == "Provider", 2, 1), PlotY = RegionY + recode(as.character(Scenario),
      BaU = 0, Def = 0.15, Aid = -0.15))
  cumulative_segments <- cumulative_plot_data %>% filter(Scenario != "BaU") %>% transmute(REMF, Scenario, x = BaU, xend = Cumulative,
      y = RegionY, yend = PlotY)
  p_cumulative <- ggplot() + geom_segment(data = cumulative_segments, aes(x = x, xend = xend, y = y, yend = yend, colour = Scenario),
      linewidth = 0.65, alpha = 0.62) + geom_point(data = cumulative_plot_data, aes(Cumulative, PlotY, colour = Scenario, shape = Scenario),
      size = 2.15, stroke = 0.55) + geom_text(data = filter(cumulative_plot_data, Scenario != "BaU"), aes(Cumulative, PlotY,
      label = paste0("-", sprintf("%.0f", Reduction), "%")), nudge_x = -12, hjust = 1, size = 2, colour = "#303030") + scenario_scale() +
      scenario_shape_scale() + scale_x_continuous(limits = c(100, 1000), breaks = seq(200, 1000, 200), labels = label_number(accuracy = 1),
      expand = c(0, 0)) + scale_y_continuous(breaks = c(1, 2), labels = c("Recipient", "Provider"), limits = c(0.55, 2.45),
      expand = c(0, 0)) + labs(tag = "d", x = expression("Cumulative emissions (Gt CO"[2] * ")"), y = NULL) + guides(colour = "none",
      shape = "none") + theme_panel() + theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(face = "bold"),
      plot.margin = margin(5, 5, 4, 4, unit = "mm"))
  p_global <- make_ar6_panel("Emissions|CO2", filter(aim_emissions, REMF == "World"), expression("CO"[2] ~ "emissions (Gt CO"[2] ~
      "yr"^{
          -1
      } * ")"), "a")
  p_price <- make_ar6_panel("Price|Carbon", aim_price, expression("Carbon price (US$ t CO"[2]^{
      -1
  } * ")"), "b", 2030) + guides(fill = "none", colour = "none", shape = "none", linetype = "none")
  environment()
}

build_revenue_panels <- function(project_root) {
  old_dir <- getwd()
  on.exit(setwd(old_dir), add = TRUE)
  setwd(project_root)
  setwd(file.path(project_root, "data", "All"))
  ggsave <- function(...) invisible(NULL)
  library(tidyverse)
  library(ggplot2)
  library(gdxrrw)
  library(scales)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(ggnewscale)
  library(svglite)
  gdx_file <- "global_17_IAMC.gdx"
  region_map_file <- "RegionmapRagg.map"
  output_dir <- "../../output/Figure"
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  scenario_aid <- "SSP2_400C_2030CP_15th_NoCC_No"
  start_year <- 2030
  end_year <- 2050
  providers <- c("XE25", "JPN", "TUR", "CHN", "USA", "XER", "XOC", "CAN", "XLM", "CIS")
  recipients <- c("XSA", "IND", "XNF", "XAF", "XSE", "BRA", "XME")
  all_regions <- c(providers, recipients)
  provider_colors <- c(XE25 = "#0072B2", JPN = "#009E73", TUR = "#56B4E9", CHN = "#6A3D9A", USA = "#1B9E77", XER = "#377EB8",
      XOC = "#984EA3", CAN = "#4DAF4A", XLM = "#00A6D6", CIS = "#2F4858")
  recipient_colors <- c(XSA = "#D55E00", IND = "#E69F00", XNF = "#F0C541", XAF = "#CC79A7", XSE = "#E41A1C", BRA = "#A65628",
      XME = "#F781BF")
  provider_blue <- "#2E6F9E"
  recipient_orange <- "#D27755"
  pool_gold <- "#E6B85C"
  ink <- "#252525"
  trapz_sum <- function(year, value) {
      ok <- is.finite(year) & is.finite(value)
      year <- year[ok]
      value <- value[ok]
      if (length(year) < 2)
          return(NA)
      o <- order(year)
      year <- year[o]
      value <- value[o]
      sum(diff(year) * (head(value, -1) + tail(value, -1))/2)
  }
  if (!file.exists(gdx_file)) stop("Place global_17_IAMC.gdx in the working directory.")
  if (!file.exists(region_map_file)) stop("Place RegionmapRagg.map in the working directory.")
  iamc <- rgdx.param(gdx_file, "IAMC_template") %>% mutate(Year = as.numeric(as.character(YEMF)))
  provider_raw <- iamc %>% filter(VEMF == "Rev_gov_Tax_Car_Tax", SCENARIO == scenario_aid, REMF %in% providers, Year >= start_year,
      Year <= end_year) %>% transmute(Year, REMF, Transfer = as.numeric(IAMC_Template))
  model_years <- sort(unique(provider_raw$Year))
  provider_annual <- provider_raw %>% group_by(Year, REMF) %>% summarise(Transfer = sum(Transfer, na.rm = TRUE), .groups = "drop") %>%
      complete(Year = model_years, REMF = providers, fill = list(Transfer = 0))
  total_annual <- provider_annual %>% group_by(Year) %>% summarise(TotalTransfer = sum(Transfer, na.rm = TRUE), .groups = "drop")
  recipient_gdp <- iamc %>% filter(VEMF == "GDP_PPP", SCENARIO == scenario_aid, REMF %in% recipients, Year >= start_year, Year <=
      end_year) %>% transmute(Year, REMF, GDP_PPP = as.numeric(IAMC_Template)) %>% group_by(Year, REMF) %>% summarise(GDP_PPP = sum(GDP_PPP,
      na.rm = TRUE), .groups = "drop") %>% complete(Year = model_years, REMF = recipients, fill = list(GDP_PPP = 0)) %>% group_by(Year) %>%
      mutate(TotalGDP_PPP = sum(GDP_PPP, na.rm = TRUE)) %>% ungroup()
  if (any(recipient_gdp$TotalGDP_PPP <= 0)) {
      stop("GDP_PPP is missing for all recipient regions in at least one model year.")
  }
  recipient_annual <- recipient_gdp %>% mutate(GDPShare = GDP_PPP/TotalGDP_PPP) %>% left_join(total_annual, by = "Year") %>%
      transmute(Year, REMF, Transfer = TotalTransfer * GDPShare)
  provider_cum <- provider_annual %>% group_by(REMF) %>% summarise(CumTransfer = trapz_sum(Year, Transfer), .groups = "drop") %>%
      mutate(CumTrillion = CumTransfer/1e+06, Type = "Provider")
  recipient_cum <- recipient_annual %>% group_by(REMF) %>% summarise(CumTransfer = trapz_sum(Year, Transfer), .groups = "drop") %>%
      mutate(CumTrillion = CumTransfer/1e+06, Type = "Recipient")
  pool_total <- sum(provider_cum$CumTrillion, na.rm = TRUE)
  recipient_total <- sum(recipient_cum$CumTrillion, na.rm = TRUE)
  relative_gap <- abs(pool_total - recipient_total)/max(abs(pool_total), 1e-12)
  if (relative_gap > 1e-08) {
      warning(paste0("Provider and recipient cumulative totals differ by ", percent(relative_gap, accuracy = 0.001), "."))
  }
  region_map <- read.table(region_map_file, header = FALSE, stringsAsFactors = FALSE) %>% as_tibble() %>% select(iso3c = 1,
      REMF = 3) %>% mutate(iso3c = str_remove_all(iso3c, "\""), REMF = str_remove_all(REMF, "\""), REMF = str_remove(REMF,
      "^R17")) %>% filter(REMF %in% all_regions)
  world_raw <- ne_countries(scale = "medium", returnclass = "sf")
  world_raw$iso3c <- world_raw$iso_a3
  world_raw$iso3c[world_raw$iso3c == "-99" | is.na(world_raw$iso3c)] <- world_raw$adm0_a3[world_raw$iso3c == "-99" | is.na(world_raw$iso3c)]
  world_raw$iso3c[world_raw$admin == "France"] <- "FRA"
  world_raw$iso3c[world_raw$admin == "Norway"] <- "NOR"
  world <- world_raw %>% select(iso3c, geometry) %>% filter(iso3c != "ATA") %>% left_join(region_map, by = "iso3c") %>% st_transform("+proj=robin")
  bbox <- st_bbox(world)
  map_width <- as.numeric(bbox["xmax"] - bbox["xmin"])
  shift_x <- map_width * 1.28
  fund_x <- shift_x/2
  fund_y <- 0
  map_crs <- st_crs(world)
  world_right <- world
  st_geometry(world_right) <- st_geometry(world_right) + c(shift_x, 0)
  st_crs(world_right) <- map_crs
  provider_map <- world %>% left_join(provider_cum %>% select(REMF, CumTrillion), by = "REMF") %>% filter(!is.na(CumTrillion))
  recipient_map <- world_right %>% left_join(recipient_cum %>% select(REMF, CumTrillion), by = "REMF") %>% filter(!is.na(CumTrillion))
  provider_regions_sf <- provider_map %>% select(REMF) %>% group_by(REMF) %>% summarise(do_union = TRUE, .groups = "drop")
  recipient_regions_sf <- recipient_map %>% select(REMF) %>% group_by(REMF) %>% summarise(do_union = TRUE, .groups = "drop")
  provider_centers_sf <- suppressWarnings(st_point_on_surface(provider_regions_sf)) %>% left_join(provider_cum %>% select(REMF,
      CumTrillion), by = "REMF")
  recipient_centers_sf <- suppressWarnings(st_point_on_surface(recipient_regions_sf)) %>% left_join(recipient_cum %>% select(REMF,
      CumTrillion), by = "REMF")
  anchor_override_ll <- tribble(~REMF, ~lon, ~lat, "XLM", -70, -12, "XSE", 105, 12)
  anchor_override_sf <- st_as_sf(anchor_override_ll, coords = c("lon", "lat"), crs = 4326) %>% st_transform(map_crs)
  anchor_override_xy <- bind_cols(st_drop_geometry(anchor_override_sf), as_tibble(st_coordinates(anchor_override_sf), .name_repair = "minimal")) %>%
      transmute(REMF, x_override = X, y_override = Y)
  provider_centers <- bind_cols(st_drop_geometry(provider_centers_sf), as_tibble(st_coordinates(provider_centers_sf), .name_repair = "minimal")) %>%
      rename(x = X, y = Y) %>% left_join(anchor_override_xy %>% filter(REMF %in% providers), by = "REMF") %>% mutate(x = coalesce(x_override,
      x), y = coalesce(y_override, y), xend = fund_x, yend = fund_y) %>% select(-x_override, -y_override)
  recipient_centers <- bind_cols(st_drop_geometry(recipient_centers_sf), as_tibble(st_coordinates(recipient_centers_sf), .name_repair = "minimal")) %>%
      rename(x = X, y = Y) %>% left_join(anchor_override_xy %>% filter(REMF %in% recipients) %>% mutate(x_override = x_override +
      shift_x), by = "REMF") %>% mutate(x = coalesce(x_override, x), y = coalesce(y_override, y), xstart = fund_x, ystart = fund_y) %>%
      select(-x_override, -y_override)
  figure2b <- ggplot() + geom_sf(data = world, fill = "#F3F3F3", color = "white", linewidth = 0.08) + geom_sf(data = provider_map,
      aes(fill = CumTrillion), color = "white", linewidth = 0.08) + scale_fill_gradient(low = "#CFE1EE", high = "#174F78",
      name = "Provider contribution\n(trillion US$2010)", guide = guide_colorbar(order = 1, title.position = "top")) + ggnewscale::new_scale_fill() +
      geom_sf(data = world_right, fill = "#F3F3F3", color = "white", linewidth = 0.08) + geom_sf(data = recipient_map, aes(fill = CumTrillion),
      color = "white", linewidth = 0.08) + scale_fill_gradient(low = "#F6D8C9", high = "#A83D2C", name = "Recipient receipt\n(trillion US$2010)",
      guide = guide_colorbar(order = 2, title.position = "top")) + geom_curve(data = provider_centers, aes(x = x, y = y, xend = xend,
      yend = yend, linewidth = CumTrillion), curvature = 0.12, color = provider_blue, alpha = 0.48, lineend = "round", inherit.aes = FALSE) +
      geom_curve(data = recipient_centers, aes(x = xstart, y = ystart, xend = x, yend = y, linewidth = CumTrillion), curvature = -0.12,
          color = recipient_orange, alpha = 0.48, lineend = "round", inherit.aes = FALSE) + scale_linewidth_continuous(range = c(0.5,
      4.5), guide = "none") + geom_point(data = provider_centers, aes(x, y), color = provider_blue, fill = "white", shape = 21,
      size = 2.5, stroke = 0.8, inherit.aes = FALSE) + geom_point(data = recipient_centers, aes(x, y), color = recipient_orange,
      fill = "white", shape = 21, size = 2.5, stroke = 0.8, inherit.aes = FALSE) + geom_text(data = provider_centers, aes(x,
      y, label = REMF), nudge_y = map_width * 0.012, size = 2.7, color = ink, inherit.aes = FALSE) + geom_text(data = recipient_centers,
      aes(x, y, label = REMF), nudge_y = map_width * 0.012, size = 2.7, color = ink, inherit.aes = FALSE) + annotate("point",
      x = fund_x, y = fund_y, shape = 21, size = 70, fill = pool_gold, color = "white", stroke = 1.2) + annotate("text", x = fund_x,
      y = fund_y + map_width * 0.01, label = "Carbon tax revenue\nin developed regions", fontface = "bold", size = 8, color = ink) +
      annotate("text", x = fund_x, y = fund_y - map_width * 0.08, label = "61 trillion US$2010", size = 6, color = ink) + annotate("text",
      x = 0, y = as.numeric(bbox["ymax"]) * 0.92, label = "Provider regions", fontface = "bold", color = provider_blue, size = 4) +
      annotate("text", x = shift_x, y = as.numeric(bbox["ymax"]) * 0.92, label = "Recipient regions", fontface = "bold", color = recipient_orange,
          size = 4) + coord_sf(xlim = c(as.numeric(bbox["xmin"]), as.numeric(bbox["xmax"]) + shift_x), ylim = c(as.numeric(bbox["ymin"]),
      as.numeric(bbox["ymax"])), expand = FALSE, datum = NA) + theme_void(base_size = 16) + theme(legend.position = "bottom",
      legend.box = "horizontal", plot.title = element_text(face = "bold", size = 15, hjust = 0.5), plot.subtitle = element_text(color = "#555555"),
      plot.caption = element_text(color = "#666666", hjust = 0.5), plot.margin = margin(10, 15, 10, 15))
  ggsave(file.path(output_dir, "C_revFlow.png"), figure2b, width = 18, height = 7.5, dpi = 600, bg = "white")
  plot_years <- seq(from = ceiling(start_year/5) * 5, to = floor(end_year/5) * 5, by = 5)
  provider_plot <- provider_annual %>% filter(Year %in% plot_years) %>% transmute(Year, REMF = factor(REMF, levels = providers),
      Flow = Transfer/1e+06, Side = "Provider contributions (+)")
  recipient_plot <- recipient_annual %>% filter(Year %in% plot_years) %>% transmute(Year, REMF = factor(REMF, levels = recipients),
      Flow = -Transfer/1e+06, Side = "Recipient allocations by GDP share (-)")
  annual_flow_plot <- bind_rows(provider_plot, recipient_plot) %>% mutate(REMF = factor(as.character(REMF), levels = c(providers,
      recipients)), Side = factor(Side, levels = c("Provider contributions (+)", "Recipient allocations by GDP share (-)")))
  flow_balance <- annual_flow_plot %>% group_by(Year, Side) %>% summarise(Total = sum(Flow), .groups = "drop") %>% mutate(Total = abs(Total)) %>%
      pivot_wider(names_from = Side, values_from = Total)
  balance_gap <- with(flow_balance, abs(`Provider contributions (+)` - `Recipient allocations by GDP share (-)`)/pmax(`Provider contributions (+)`,
      1e-12))
  if (any(balance_gap > 1e-08)) {
      warning("Provider and recipient annual totals do not balance for at least one plotted year.")
  }
  bar_half_width <- min(diff(plot_years)) * 0.36
  total_limit <- annual_flow_plot %>% group_by(Year, Side) %>% summarise(Total = abs(sum(Flow)), .groups = "drop") %>% summarise(Max = max(Total,
      na.rm = TRUE)) %>% pull(Max)
  figure_annual <- ggplot() + geom_col(data = provider_plot, aes(x = Year, y = Flow, fill = REMF), width = bar_half_width *
      2, color = "white", linewidth = 0.18, position = position_stack(reverse = TRUE)) + scale_fill_manual(values = provider_colors,
      breaks = providers, limits = providers, name = NULL, guide = guide_legend(nrow = 1, byrow = TRUE, order = 1)) + ggnewscale::new_scale_fill() +
      geom_col(data = recipient_plot, aes(x = Year, y = Flow, fill = REMF), width = bar_half_width * 2, color = "white", linewidth = 0.18,
          position = position_stack(reverse = TRUE)) + scale_fill_manual(values = recipient_colors, breaks = recipients, limits = recipients,
      name = NULL, guide = guide_legend(nrow = 1, byrow = TRUE, order = 2)) + geom_hline(yintercept = 0, color = ink, linewidth = 0.65) +
      annotate("text", x = min(plot_years), y = total_limit * 1.08, label = "Developed-region carbon-tax revenue", hjust = 0,
          color = provider_blue, fontface = "bold", size = 4.2) + annotate("text", x = min(plot_years), y = -total_limit *
      1.08, label = "Developing-region receipts allocated by GDP share", hjust = 0, vjust = 1, color = recipient_orange, fontface = "bold",
      size = 4.2) + scale_x_continuous(breaks = plot_years, limits = c(min(plot_years) - bar_half_width * 1.4, max(plot_years) +
      bar_half_width * 1.4), expand = expansion(mult = 0)) + scale_y_continuous(labels = label_number(accuracy = 0.1), limits = c(-total_limit *
      1.2, total_limit * 1.2), expand = expansion(mult = 0)) + labs(x = "Year", y = "Annual carbon-tax revenue flow \n (trillion US$2010 yr\342\201\273\302\271)") +
      theme_minimal(base_size = 16) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color = "#D9D9D9",
      linewidth = 0.4), axis.text = element_text(color = ink), axis.title = element_text(color = ink), legend.position = "bottom",
      legend.box = "vertical", legend.box.just = "center", legend.justification = c(0.5, 0.5), legend.title = element_blank(),
      legend.key.size = unit(0.8, "lines"), plot.margin = margin(10, 45, 10, 45))
  ggsave(file.path(output_dir, "C_revFlow_annual_distribution.png"), figure_annual, width = 18, height = 7, dpi = 600, bg = "white")
  environment()
}

build_main_figure_2 <- function(project_root) {
  old_dir <- getwd()
  on.exit(setwd(old_dir), add = TRUE)
  setwd(project_root)
  emission_env <- build_emission_panels(project_root)
  revenue_env <- build_revenue_panels(project_root)
  required_emission <- c("p_global", "p_price", "p_sector_change")
  missing_emission <- required_emission[!vapply(required_emission, exists, logical(1), envir = emission_env)]
  if (length(missing_emission)) {
      stop("Missing emission panel(s): ", paste(missing_emission, collapse = ", "))
  }
  required_revenue <- c("figure2b", "figure_annual", "ink")
  missing_revenue <- required_revenue[!vapply(required_revenue, exists, logical(1), envir = revenue_env)]
  if (length(missing_revenue)) {
      stop("Missing revenue panel(s): ", paste(missing_revenue, collapse = ", "))
  }
  ink <- "#252525"
  tag_theme <- theme(plot.tag = element_text(family = "Arial", face = "bold", size = 8.2, colour = ink), plot.tag.position = c(0.005,
      0.995), plot.background = element_rect(fill = "white", colour = NA))
  p_a <- emission_env$p_global + labs(tag = "a") + theme(legend.position = "bottom", legend.direction = "horizontal", legend.box = "vertical",
      legend.justification = "center") + tag_theme
  p_b <- emission_env$p_price + labs(tag = "b") + tag_theme
  p_c <- revenue_env$figure2b + labs(tag = "c") + theme(legend.position = "bottom", legend.box = "horizontal", legend.justification = "center",
      legend.title = element_text(size = 6.2), legend.text = element_text(size = 5.8), legend.key.height = grid::unit(2.8,
          "mm"), plot.margin = margin(0.5, 1.5, 1, 1.5, unit = "mm")) + tag_theme
  if (length(p_c$layers) < 15L) {
      stop("Unexpected revenue-map layer structure; cannot rescale annotations.")
  }
  p_c$layers[[7L]]$aes_params$size <- 1.55
  p_c$layers[[8L]]$aes_params$size <- 1.55
  p_c$layers[[9L]]$aes_params$size <- 1.75
  p_c$layers[[10L]]$aes_params$size <- 1.75
  p_c$layers[[11L]]$aes_params$size <- 30
  p_c$layers[[12L]]$aes_params$size <- 3.15
  p_c$layers[[13L]]$aes_params$size <- 2.65
  p_c$layers[[14L]]$aes_params$size <- 2.35
  p_c$layers[[15L]]$aes_params$size <- 2.35
  cumulative_pool_rows <- dplyr::pull(dplyr::filter(revenue_env$iamc, VEMF == "Cum_Rev_gov_Tax_Car_Tax", SCENARIO == revenue_env$scenario_aid,
      REMF == "Rprovider15th", Year == revenue_env$end_year), IAMC_Template)
  if (length(cumulative_pool_rows) != 1L || !is.finite(cumulative_pool_rows[[1L]]) || cumulative_pool_rows[[1L]] <= 0) {
      stop("Missing the 2050 Rprovider15th cumulative carbon-tax revenue.")
  }
  cumulative_pool_trillion <- cumulative_pool_rows[[1L]]/1e+06
  pool_total_label <- paste0(ceiling(cumulative_pool_trillion), " trillion US$2010")
  pool_label_replaced <- FALSE
  for (layer_index in seq_along(p_c$layers)) {
      layer_data <- p_c$layers[[layer_index]]$data
      if (is.data.frame(layer_data) && "label" %in% names(layer_data)) {
          replace_rows <- as.character(layer_data$label) == "61 trillion US$2010"
          if (any(replace_rows, na.rm = TRUE)) {
              p_c$layers[[layer_index]]$data$label[replace_rows] <- pool_total_label
              pool_label_replaced <- TRUE
          }
      }
      fixed_label <- p_c$layers[[layer_index]]$aes_params$label
      if (!is.null(fixed_label) && any(as.character(fixed_label) == "61 trillion US$2010")) {
          p_c$layers[[layer_index]]$aes_params$label <- pool_total_label
          pool_label_replaced <- TRUE
      }
  }
  if (!pool_label_replaced) {
      stop("Could not locate the fixed cumulative-pool label in panel c.")
  }
  apply_label_offsets <- function(data, offsets, map_width) {
      matched <- match(as.character(data$REMF), offsets$REMF)
      move <- !is.na(matched)
      data$x[move] <- data$x[move] + offsets$dx[matched[move]] * map_width
      data$y[move] <- data$y[move] + offsets$dy[matched[move]] * map_width
      data
  }
  provider_label_offsets <- data.frame(REMF = c("XE25", "XER", "TUR", "JPN"), dx = c(-0.03, -0.006, 0.026, 0.014), dy = c(0.02,
      -0.016, 0.006, 0.006))
  recipient_label_offsets <- data.frame(REMF = c("XNF", "XME", "XSA", "IND", "XSE"), dx = c(-0.018, -0.008, 0.004, 0.016, 0.02),
      dy = c(0.012, -0.008, 0.016, -0.01, 0.006))
  p_c$layers[[9L]]$data <- apply_label_offsets(p_c$layers[[9L]]$data, provider_label_offsets, revenue_env$map_width)
  p_c$layers[[10L]]$data <- apply_label_offsets(p_c$layers[[10L]]$data, recipient_label_offsets, revenue_env$map_width)
  p_d <- revenue_env$figure_annual + labs(tag = "d", y = "Annual carbon-tax revenue flow\n(trillion US$2010 yr-1)") + theme_minimal(base_family = "Arial",
      base_size = 7.2) + theme(panel.background = element_rect(fill = "white", colour = NA), plot.background = element_rect(fill = "white",
      colour = NA), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(colour = "#D9D9D9",
      linewidth = 0.25), axis.text = element_text(size = 6, colour = ink), axis.title = element_text(size = 6.7, colour = ink),
      legend.position = "bottom", legend.box = "vertical", legend.box.just = "center", legend.justification = "center", legend.text = element_text(size = 5.3),
      legend.key.height = grid::unit(2.3, "mm"), legend.key.width = grid::unit(2.5, "mm"), legend.spacing.x = grid::unit(0.5,
          "mm"), legend.margin = margin(1, 0, 0, 0, unit = "mm"), plot.margin = margin(3, 3, 2, 3, unit = "mm")) + tag_theme
  for (layer_index in seq_along(p_d$layers)) {
      if (inherits(p_d$layers[[layer_index]]$geom, "GeomText")) {
          p_d$layers[[layer_index]]$aes_params$size <- 2.25
      }
  }
  p_e <- emission_env$p_sector_change + labs(tag = "e") + theme(legend.position = "bottom", legend.direction = "horizontal",
      legend.justification = "center") + tag_theme
  top_row <- patchwork::wrap_plots(p_a, p_b, nrow = 1, widths = c(1.8, 1), guides = "keep")
  bottom_row <- patchwork::wrap_plots(p_d, p_e, nrow = 1, widths = c(1.35, 1), guides = "keep")
  figure_composite <- patchwork::wrap_plots(top_row, p_c, bottom_row, ncol = 1, heights = c(1, 0.84, 1.15), guides = "keep") &
      theme(plot.background = element_rect(fill = "white", colour = NA))
  figure_composite
}

prepare_transfer_benefit <- function(project_root) {
  old_dir <- getwd()
  on.exit(setwd(old_dir), add = TRUE)
  setwd(project_root)

  project_library_candidates <- c(file.path(getwd(), "Rlib"), file.path(getwd(), "..", "Rlib"), file.path(getwd(), "..", "..",
      "Rlib"), "C:/ENVI5809/R_library", "C:/Users/umeha/AppData/Local/R/win-library/4.5")
  project_library_candidates <- project_library_candidates[dir.exists(project_library_candidates)]
  if (length(project_library_candidates) > 0) {
      .libPaths(unique(c(project_library_candidates, .libPaths())))
  }
  suppressPackageStartupMessages({
      library(dplyr)
      library(tidyr)
      library(ggplot2)
      library(gdxrrw)
      library(scales)
  })
  find_project_dir <- function(start_dir) {
      candidate <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)
      repeat {
          if (file.exists(file.path(candidate, "CTaxRev.Rproj")))
              return(candidate)
          parent <- dirname(candidate)
          if (identical(parent, candidate)) {
              stop("Could not find the CTaxRev project root.")
          }
          candidate <- parent
      }
  }
  command_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", command_args, value = TRUE)
  start_dir <- if (length(file_arg) > 0) {
      dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE))
  } else {
      getwd()
  }
  project_dir <- find_project_dir(start_dir)
  gdx_file <- file.path(project_dir, "data", "All", "global_17_IAMC.gdx")
  output_dir <- file.path(project_dir, "output", "Figure")
  csv_dir <- file.path(output_dir, "csv")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(csv_dir, showWarnings = FALSE, recursive = TRUE)
  if (!file.exists(gdx_file)) {
      stop("GDX input was not found: ", gdx_file)
  }
  gams_candidates <- unique(c(Sys.getenv("GAMS_SYSDIR", unset = ""), "C:/GAMS/34", "C:/GAMS/win64/26.1"))
  gams_candidates <- gams_candidates[nzchar(gams_candidates)]
  gams_dir <- gams_candidates[dir.exists(gams_candidates)][1]
  if (is.na(gams_dir)) {
      stop("GAMS system directory was not found; gdxrrw cannot read the GDX.")
  }
  igdx(gams_dir)
  scenario_def <- "SSP2_400C_2030CP_NoCC_No"
  scenario_aid <- "SSP2_400C_2030CP_15th_NoCC_No"
  scenario_bau <- "SSP2_BaU_NoCC_No"
  start_year <- 2030
  end_year <- 2050
  providers <- c("XE25", "JPN", "TUR", "CHN", "USA", "XER", "XOC", "CAN", "XLM", "CIS")
  recipients <- c("XSA", "IND", "XNF", "XAF", "XSE", "BRA", "XME")
  all_regions <- c(providers, recipients)
  provider_color <- "#356C9B"
  recipient_color <- "#D27755"
  theme_paper <- theme_minimal(base_size = 16) + theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(color = "#E5E5E5",
      linewidth = 0.3), axis.title = element_text(face = "plain"), legend.position = "bottom", plot.caption = element_text(color = "#666666",
      hjust = 0))
  trapz_sum <- function(year, value) {
      ok <- is.finite(year) & is.finite(value)
      year <- year[ok]
      value <- value[ok]
      if (length(year) < 2)
          return(NA)
      o <- order(year)
      year <- year[o]
      value <- value[o]
      sum(diff(year) * (head(value, -1) + tail(value, -1))/2)
  }
  iamc <- rgdx.param(gdx_file, "IAMC_template") %>% mutate(Year = as.numeric(as.character(YEMF)), IAMC_Template = as.numeric(IAMC_Template))
  provider_annual <- iamc %>% filter(VEMF == "Rev_gov_Tax_Car_Tax", SCENARIO == scenario_aid, REMF %in% providers, Year >=
      start_year, Year <= end_year) %>% transmute(Year, REMF, Transfer = IAMC_Template) %>% group_by(Year, REMF) %>% summarise(Transfer = sum(Transfer,
      na.rm = TRUE), .groups = "drop") %>% complete(Year = seq(start_year, end_year, 5), REMF = providers, fill = list(Transfer = 0))
  total_annual <- provider_annual %>% group_by(Year) %>% summarise(TotalTransfer = sum(Transfer, na.rm = TRUE), .groups = "drop")
  recipient_annual <- iamc %>% filter(VEMF == "GDP_PPP", SCENARIO == scenario_aid, REMF %in% recipients, Year >= start_year,
      Year <= end_year) %>% group_by(Year) %>% mutate(GDPShare = IAMC_Template/sum(IAMC_Template, na.rm = TRUE)) %>% ungroup() %>%
      left_join(total_annual, by = "Year") %>% transmute(Year, REMF, Transfer = TotalTransfer * GDPShare)
  transfer_cumulative <- bind_rows(provider_annual %>% group_by(REMF) %>% summarise(CumTransfer = trapz_sum(Year, Transfer),
      .groups = "drop") %>% mutate(Type = "Provider"), recipient_annual %>% group_by(REMF) %>% summarise(CumTransfer = trapz_sum(Year,
      Transfer), .groups = "drop") %>% mutate(Type = "Recipient"))
  gdp_cumulative <- iamc %>% filter(VEMF == "GDP_MER", SCENARIO %in% c(scenario_aid, scenario_bau), REMF %in% all_regions,
      Year >= start_year, Year <= end_year) %>% mutate(GDPBasis = if_else(SCENARIO == scenario_aid, "Aid", "BaU")) %>% group_by(GDPBasis,
      REMF) %>% summarise(CumGDP = trapz_sum(Year, IAMC_Template), .groups = "drop")
  transfer_summary <- transfer_cumulative %>% left_join(gdp_cumulative, by = "REMF") %>% mutate(CumTrillion = CumTransfer/1e+06,
      NetTransferPct = if_else(Type == "Provider", -100 * CumTransfer/(CumGDP * 1000), 100 * CumTransfer/(CumGDP * 1000)))
  loss <- iamc %>% filter(VEMF == "Pol_Cos_Cns_Los_rat_NPV_5pc", SCENARIO %in% c(scenario_def, scenario_aid), REMF %in% all_regions,
      Year == end_year) %>% mutate(Scenario = case_when(SCENARIO == scenario_def ~ "Def", SCENARIO == scenario_aid ~ "Aid")) %>%
      group_by(REMF, Scenario) %>% summarise(Value = mean(IAMC_Template, na.rm = TRUE), .groups = "drop") %>% pivot_wider(names_from = Scenario,
      values_from = Value) %>% filter(is.finite(Def), is.finite(Aid)) %>% mutate(Recovery = Def - Aid)
  scatter_data <- transfer_summary %>% select(GDPBasis, REMF, Type, NetTransferPct, CumTrillion) %>% left_join(loss %>% select(REMF,
      Def, Aid, Recovery), by = "REMF") %>% mutate(Type = factor(Type, levels = c("Provider", "Recipient"))) %>% filter(is.finite(NetTransferPct),
      is.finite(Recovery), is.finite(CumTrillion))
  plot_years <- seq(start_year, end_year, 5)
  annual_transfer <- bind_rows(provider_annual %>% filter(Year %in% plot_years) %>% mutate(Type = "Provider"), recipient_annual %>%
      filter(Year %in% plot_years) %>% mutate(Type = "Recipient"))
  gdp_annual <- iamc %>% filter(VEMF == "GDP_MER", SCENARIO %in% c(scenario_aid, scenario_bau), REMF %in% all_regions, Year %in%
      plot_years) %>% transmute(Year, REMF, GDPBasis = if_else(SCENARIO == scenario_aid, "Aid", "BaU"), GDP = IAMC_Template)
  annual_consumption <- iamc %>% filter(VEMF == "CNS", SCENARIO %in% c(scenario_def, scenario_aid), REMF %in% all_regions,
      Year %in% plot_years) %>% mutate(Scenario = case_when(SCENARIO == scenario_def ~ "Def", SCENARIO == scenario_aid ~ "Aid")) %>%
      group_by(Year, REMF, Scenario) %>% summarise(Value = mean(IAMC_Template, na.rm = TRUE), .groups = "drop") %>% pivot_wider(names_from = Scenario,
      values_from = Value) %>% filter(is.finite(Def), is.finite(Aid), Def > 0) %>% transmute(Year, REMF, CNS_Def = Def, CNS_Aid = Aid,
      ConsumptionChangePct = 100 * (CNS_Aid - CNS_Def)/CNS_Def)
  annual_scatter_data <- annual_transfer %>% left_join(gdp_annual, by = c("Year", "REMF")) %>% mutate(Type = factor(Type, levels = c("Provider",
      "Recipient")), AnnualTrillion = Transfer/1e+06, NetTransferPct = if_else(Type == "Provider", -100 * Transfer/(GDP * 1000),
      100 * Transfer/(GDP * 1000))) %>% left_join(annual_consumption, by = c("Year", "REMF")) %>% filter(is.finite(NetTransferPct),
      is.finite(ConsumptionChangePct), is.finite(AnnualTrillion))
  write.csv(annual_scatter_data %>% arrange(GDPBasis, Year, Type, REMF) %>% select(GDPBasis, Year, REMF, Type, Transfer, GDP,
      NetTransferPct, AnnualTrillion, CNS_Def, CNS_Aid, ConsumptionChangePct), file.path(csv_dir, "transfer_recovery_scatter_5year_source_data.csv"),
      row.names = FALSE)
  invisible(NULL)
}

prepare_zero_loss <- function(project_root) {
  old_dir <- getwd()
  on.exit(setwd(old_dir), add = TRUE)
  setwd(project_root)

  find_project_root <- function(start) {
      current <- normalizePath(start, winslash = "/", mustWork = TRUE)
      repeat {
          marker <- file.path(current, "data", "All", "global_17_IAMC.gdx")
          if (file.exists(marker))
              return(current)
          parent <- dirname(current)
          if (identical(parent, current))
              break
          current <- parent
      }
      NA
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
      stop("Could not locate data/All/global_17_IAMC.gdx.")
  }
  local_library <- file.path(project_root, "Rlib")
  if (dir.exists(local_library)) .libPaths(c(local_library, .libPaths()))
  shared_libraries <- c(Sys.getenv("R_LIBS_USER"), "C:/ENVI5809/R_library")
  shared_libraries <- shared_libraries[nzchar(shared_libraries) & dir.exists(shared_libraries)]
  .libPaths(unique(c(shared_libraries, .libPaths())))
  suppressPackageStartupMessages({
      library(dplyr)
      library(ggplot2)
      library(scales)
  })
  scenario_def <- "SSP2_400C_2030CP_NoCC_No"
  scenario_aid <- "SSP2_400C_2030CP_15th_NoCC_No"
  provider_region <- "Rprovider15th"
  recipient_region <- "Rrecipient15th"
  sensitivity_region_membership <- bind_rows(tibble::tibble(Threshold = "10th", Region = c("IND", "XSA", "XNF", "XAF"), Group = "Recipient"),
      tibble::tibble(Threshold = "10th", Region = c("XE25", "XER", "TUR", "XOC", "CHN", "JPN", "XSE", "CAN", "USA", "BRA",
          "XLM", "CIS", "XME"), Group = "Provider"), tibble::tibble(Threshold = "15th", Region = c("IND", "BRA", "XSA", "XSE",
          "XME", "XNF", "XAF"), Group = "Recipient"), tibble::tibble(Threshold = "15th", Region = c("XE25", "XER", "TUR", "XOC",
          "CHN", "JPN", "CAN", "USA", "XLM", "CIS"), Group = "Provider"), tibble::tibble(Threshold = "20th", Region = c("CHN",
          "IND", "BRA", "XSA", "XSE", "XME", "XLM", "XNF", "XAF", "CIS"), Group = "Recipient"), tibble::tibble(Threshold = "20th",
          Region = c("XE25", "XER", "TUR", "XOC", "JPN", "CAN", "USA"), Group = "Provider"))
  membership_check <- sensitivity_region_membership %>% count(Threshold, Region) %>% filter(n != 1L)
  if (nrow(membership_check) || any((sensitivity_region_membership %>% count(Threshold))$n != 17L)) {
      stop("Each GDP threshold must classify all 17 AIM regions exactly once.")
  }
  sensitivity_cases <- expand.grid(SSP = paste0("SSP", 1:3), Threshold = c("10th", "15th", "20th"), stringsAsFactors = FALSE) %>%
      mutate(DefScenario = paste0(SSP, "_400C_2030CP_NoCC_No"), AidScenario = paste0(SSP, "_400C_2030CP_", Threshold, "_NoCC_No")) %>%
      filter(!(SSP == "SSP3" & Threshold %in% c("10th", "15th")))
  start_year <- 2030
  end_year <- 2050
  model_years <- seq(start_year, end_year, by = 5)
  period_years <- end_year - start_year
  loss_rate_variable <- "Pol_Cos_Cns_Los_rat_NPV_5pc"
  loss_amount_variable <- "Pol_Cos_Cns_Los"
  revenue_variable <- "Rev_gov_Tax_Car_Tax"
  cumulative_revenue_variable <- "Cum_Rev_gov_Tax_Car_Tax"
  variables_needed <- c(loss_rate_variable, loss_amount_variable, revenue_variable, cumulative_revenue_variable)
  scenarios_needed <- unique(c(scenario_def, scenario_aid, sensitivity_cases$DefScenario, sensitivity_cases$AidScenario))
  gdx_file <- file.path(project_root, "data", "All", "global_17_IAMC.gdx")
  output_dir <- file.path(project_root, "output", "Figure")
  csv_dir <- file.path(output_dir, "csv")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
  find_gdxdump <- function() {
      from_path <- unname(Sys.which(c("gdxdump", "gdxdump.exe")))
      from_path <- from_path[nzchar(from_path)]
      candidates <- unique(c(from_path, "C:/GAMS/34/gdxdump.exe", "C:/GAMS/win64/26.1/gdxdump.exe"))
      hit <- candidates[file.exists(candidates)]
      if (!length(hit))
          return(NA)
      normalizePath(hit[[1]], winslash = "/")
  }
  read_iamc_subset <- function(path, scenarios, variables) {
      gdxdump <- find_gdxdump()
      if (is.na(gdxdump))
          stop("Could not locate gdxdump.exe.")
      dump_file <- tempfile("IAMC_template_", fileext = ".csv")
      on.exit(unlink(dump_file), add = TRUE)
      result <- system2(gdxdump, args = c(shQuote(normalizePath(path, winslash = "/")), "symb=IAMC_template", "format=csv",
          paste0("output=", shQuote(dump_file))), stdout = TRUE, stderr = TRUE)
      status <- attr(result, "status")
      if (is.null(status))
          status <- 0L
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
          if (!length(lines))
              break
          scenario_hit <- rep(FALSE, length(lines))
          for (scenario in scenarios) {
              scenario_hit <- scenario_hit | startsWith(lines, paste0("\"", scenario, "\","))
          }
          variable_hit <- rep(FALSE, length(lines))
          for (variable in variables) {
              variable_hit <- variable_hit | grepl(paste0(",\"", variable, "\","), lines, fixed = TRUE)
          }
          selected <- lines[scenario_hit & variable_hit]
          if (length(selected)) {
              piece_id <- piece_id + 1L
              pieces[[piece_id]] <- read.csv(text = paste(selected, collapse = "\n"), header = FALSE, col.names = c("SCENARIO",
                  "REMF", "VEMF", "Year", "Value"), stringsAsFactors = FALSE)
          }
      }
      if (!length(pieces))
          stop("No required IAMC_template records were found.")
      bind_rows(pieces) %>% mutate(Year = as.numeric(Year), Value = as.numeric(Value))
  }
  message("Reading selected IAMC_template records from: ", gdx_file)
  iamc <- read_iamc_subset(gdx_file, scenarios_needed, variables_needed) %>% filter(SCENARIO %in% scenarios_needed, VEMF %in%
      variables_needed, is.finite(Year), is.finite(Value))
  one_value <- function(variable, scenario, region, year) {
      values <- iamc$Value[iamc$VEMF == variable & iamc$SCENARIO == scenario & iamc$REMF == region & iamc$Year == year]
      if (length(values) != 1L || !is.finite(values)) {
          stop("Expected one finite value for ", variable, ", ", scenario, ", ", region, ", ", year, "; found ", length(values),
              ".")
      }
      values
  }
  five_year_series <- function(variable, scenario, region) {
      rows <- iamc %>% filter(VEMF == variable, SCENARIO == scenario, REMF == region, Year %in% model_years) %>% select(Year,
          Value) %>% arrange(Year)
      if (nrow(rows) != length(model_years) || anyDuplicated(rows$Year) || !identical(rows$Year, as.numeric(model_years)) ||
          any(!is.finite(rows$Value))) {
          stop("Expected one finite ", variable, " observation for every model year ", start_year, "-", end_year, " in ", scenario,
              ", ", region, ".")
      }
      rows
  }
  trapezoid_integral <- function(year, annual_flow) {
      if (length(year) < 2L || length(year) != length(annual_flow)) {
          stop("A valid annual-flow series is required for integration.")
      }
      sum(diff(year) * (head(annual_flow, -1L) + tail(annual_flow, -1L))/2)
  }
  revenue_annual <- five_year_series(revenue_variable, scenario_aid, provider_region) %>% mutate(Flow = "Provider carbon-tax revenue",
      Unit = "million USD2010/yr")
  revenue_cumulative <- five_year_series(cumulative_revenue_variable, scenario_aid, provider_region) %>% mutate(Flow = "Provider cumulative carbon-tax revenue",
      Unit = "million USD2010")
  loss_amount_annual <- five_year_series(loss_amount_variable, scenario_def, recipient_region) %>% mutate(Flow = "Recipient Def consumption loss",
      Unit = "billion USD2010/yr")
  actual_cumulative_aid <- one_value(cumulative_revenue_variable, scenario_aid, provider_region, end_year)/1e+06
  accounting_zero_loss_funding <- trapezoid_integral(loss_amount_annual$Year, loss_amount_annual$Value)/1000
  if (!is.finite(actual_cumulative_aid) || actual_cumulative_aid <= 0) {
      stop("The cumulative provider transfer must be positive.")
  }
  if (!is.finite(accounting_zero_loss_funding) || accounting_zero_loss_funding <= 0) {
      stop("The cumulative Def recipient consumption loss must be positive.")
  }
  plot_data <- tibble::tribble(~Group, ~Scenario, ~Region, ~ScenarioCode, "Provider", "Def", provider_region, scenario_def,
      "Provider", "Aid", provider_region, scenario_aid, "Recipient", "Def", recipient_region, scenario_def, "Recipient", "Aid",
      recipient_region, scenario_aid) %>% rowwise() %>% mutate(CumulativeAid_trillion_USD2010 = if_else(Scenario == "Def",
      0, actual_cumulative_aid), AverageAnnualAid_trillion_USD2010_per_year = CumulativeAid_trillion_USD2010/period_years,
      CumulativeConsumptionLossRate_pct = one_value(loss_rate_variable, ScenarioCode, Region, end_year)) %>% ungroup() %>%
      mutate(Group = factor(Group, levels = c("Provider", "Recipient")), Scenario = factor(Scenario, levels = c("Def", "Aid")))
  recipient_points <- plot_data %>% filter(Group == "Recipient") %>% arrange(Scenario)
  recipient_def_loss_rate <- recipient_points$CumulativeConsumptionLossRate_pct[recipient_points$Scenario == "Def"]
  recipient_aid_loss_rate <- recipient_points$CumulativeConsumptionLossRate_pct[recipient_points$Scenario == "Aid"]
  denominator <- recipient_def_loss_rate - recipient_aid_loss_rate
  if (!is.finite(denominator) || denominator <= 0) {
      stop("The recipient Def-Aid line does not imply a positive zero-loss crossing.")
  }
  cge_zero_loss_funding <- actual_cumulative_aid * recipient_def_loss_rate/denominator
  if (cge_zero_loss_funding < 0 || cge_zero_loss_funding > actual_cumulative_aid) {
      stop("The CGE-implied zero-loss crossing is outside the Def-Aid interval.")
  }
  funding_gap <- accounting_zero_loss_funding - cge_zero_loss_funding
  funding_ratio <- accounting_zero_loss_funding/cge_zero_loss_funding
  benchmarks <- tibble::tibble(Metric = c("Actual cumulative Aid transfer", "CGE-implied recipient zero-loss funding", "Def-loss accounting benchmark",
      "Accounting benchmark minus CGE", "Accounting benchmark / CGE"), Value = c(actual_cumulative_aid, cge_zero_loss_funding,
      accounting_zero_loss_funding, funding_gap, funding_ratio), Unit = c(rep("trillion USD2010, undiscounted", 4), "ratio"))
  provider_colour <- "#3F6C8E"
  recipient_colour <- "#B6573F"
  ink <- "#222222"
  x_max <- max(actual_cumulative_aid, cge_zero_loss_funding, accounting_zero_loss_funding) * 1.12
  y_values <- plot_data$CumulativeConsumptionLossRate_pct
  y_span <- diff(range(c(y_values, 0)))
  if (!is.finite(y_span) || y_span == 0) y_span <- 1
  y_min <- min(y_values, 0) - 0.13 * y_span
  y_max <- max(y_values, 0) + 0.18 * y_span
  benchmark_points <- tibble::tibble(Benchmark = c("CGE-implied", "Def-loss benchmark"), X = c(cge_zero_loss_funding, accounting_zero_loss_funding),
      Y = 0, Label = c(number(cge_zero_loss_funding, accuracy = 0.1), number(accounting_zero_loss_funding, accuracy = 0.1)),
      LabelY = c(0.075 * y_span, -0.1 * y_span))
  x_breaks <- pretty(c(0, x_max), n = 6)
  x_breaks <- x_breaks[x_breaks >= 0 & x_breaks <= x_max]
  figure <- ggplot(plot_data, aes(x = CumulativeAid_trillion_USD2010, y = CumulativeConsumptionLossRate_pct, color = Group,
      group = Group)) + geom_hline(yintercept = 0, color = ink, linewidth = 0.55) + geom_vline(xintercept = 0, color = ink,
      linewidth = 0.55) + geom_line(linetype = "dotted", linewidth = 1) + geom_point(aes(shape = Scenario, fill = interaction(Group,
      Scenario)), size = 4.2, stroke = 1.05) + geom_segment(data = tibble::tibble(x = min(accounting_zero_loss_funding, cge_zero_loss_funding),
      xend = max(accounting_zero_loss_funding, cge_zero_loss_funding), y = 0, yend = 0), aes(x = x, xend = xend, y = y, yend = yend),
      inherit.aes = FALSE, color = "#999999", linewidth = 3.2, alpha = 0.35, lineend = "round") + annotate("label", x = mean(c(cge_zero_loss_funding,
      accounting_zero_loss_funding)), y = -0.035 * y_span, label = number(funding_gap, accuracy = 0.1), size = 3, linewidth = 0,
      fill = alpha("white", 0.9), color = "#666666") + geom_point(data = benchmark_points, aes(x = X, y = Y, shape = Benchmark),
      inherit.aes = FALSE, size = 4, stroke = 1, color = ink, fill = "white") + geom_label(data = benchmark_points, aes(x = X,
      y = LabelY, label = Label), inherit.aes = FALSE, size = 3.25, lineheight = 0.95, linewidth = 0, fill = alpha("white",
      0.9), color = ink) + scale_color_manual(values = c(Provider = provider_colour, Recipient = recipient_colour), name = NULL) +
      scale_fill_manual(values = c(Provider.Def = "white", Provider.Aid = provider_colour, Recipient.Def = "white", Recipient.Aid = recipient_colour),
          guide = "none") + scale_shape_manual(values = c(Def = 1, Aid = 16, `CGE-implied` = 23, `Def-loss benchmark` = 24),
      breaks = c("Def", "Aid", "CGE-implied", "Def-loss benchmark"), labels = c("Def", "Aid", "CGE-implied zero loss", "Def-loss benchmark"),
      name = NULL) + scale_x_continuous(breaks = x_breaks, labels = label_number(accuracy = 1), limits = c(0, x_max), expand = expansion(mult = c(0,
      0)), sec.axis = sec_axis(~./period_years, breaks = x_breaks/period_years, labels = label_number(accuracy = 0.1), name = "Average annual amount, 2030-2050 (trillion US$2010/yr)")) +
      scale_y_continuous(labels = label_number(accuracy = 1, suffix = "%"), limits = c(y_min, y_max), expand = expansion(mult = c(0,
          0))) + coord_cartesian(clip = "off") + labs(x = "Cumulative transfer from 2030 to 2050 (trillion US$2010)", y = "Cumulative consumption loss rate in 2050") +
      guides(color = guide_legend(order = 1, override.aes = list(linetype = 0, size = 3)), shape = guide_legend(order = 2,
          override.aes = list(color = ink))) + theme_classic(base_size = 11, base_family = "sans") + theme(panel.background = element_rect(fill = "white",
      color = NA), plot.background = element_rect(fill = "white", color = NA), axis.line = element_line(color = ink, linewidth = 0.55),
      axis.ticks = element_line(color = ink, linewidth = 0.45), axis.title = element_text(color = ink, size = 10.5), axis.text = element_text(color = ink,
          size = 9.5), legend.position = "inside", legend.position.inside = c(0.8, 0.76), legend.direction = "vertical", legend.background = element_rect(fill = "white",
          color = NA), legend.key = element_blank(), plot.margin = margin(10, 12, 10, 10))
  png_file <- file.path(output_dir, "Cumulative_transfer_zero_loss_2050.png")
  svg_file <- file.path(output_dir, "Cumulative_transfer_zero_loss_2050.svg")
  source_csv <- file.path(csv_dir, "Cumulative_transfer_zero_loss_2050_source_data.csv")
  benchmark_csv <- file.path(csv_dir, "Cumulative_transfer_zero_loss_2050_benchmarks.csv")
  annual_csv <- file.path(csv_dir, "Cumulative_transfer_zero_loss_2050_annual_flows.csv")
  ggsave(png_file, figure, width = 7.4, height = 6.2, dpi = 600, bg = "white")
  if (requireNamespace("svglite", quietly = TRUE)) {
      ggsave(svg_file, figure, width = 7.4, height = 6.2, device = svglite::svglite, bg = "white")
  }
  write.csv(plot_data, source_csv, row.names = FALSE, na = "")
  write.csv(benchmarks, benchmark_csv, row.names = FALSE, na = "")
  write.csv(bind_rows(revenue_annual %>% mutate(ConvertedValue = Value/1e+06, ConvertedUnit = "trillion USD2010/yr"), revenue_cumulative %>%
      mutate(ConvertedValue = Value/1e+06, ConvertedUnit = "trillion USD2010"), loss_amount_annual %>% mutate(ConvertedValue = Value/1000,
      ConvertedUnit = "trillion USD2010/yr")) %>% select(Flow, Year, Value, Unit, ConvertedValue, ConvertedUnit), annual_csv,
      row.names = FALSE, na = "")
  invisible(NULL)
}

build_transfer_base <- function(project_root) {
  old_dir <- getwd()
  on.exit(setwd(old_dir), add = TRUE)
  setwd(project_root)
  ggsave <- function(...) invisible(NULL)
  unlink <- function(...) invisible(0L)
  find_project_root <- function(start) {
      current <- normalizePath(start, winslash = "/", mustWork = TRUE)
      repeat {
          marker <- file.path(current, "data", "All", "global_17_IAMC.gdx")
          if (file.exists(marker))
              return(current)
          parent <- dirname(current)
          if (identical(parent, current))
              break
          current <- parent
      }
      NA
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
      stop("Could not locate data/All/global_17_IAMC.gdx.")
  }
  local_library <- file.path(project_root, "Rlib")
  if (dir.exists(local_library)) .libPaths(c(local_library, .libPaths()))
  shared_libraries <- c(Sys.getenv("R_LIBS_USER"), "C:/ENVI5809/R_library")
  shared_libraries <- shared_libraries[nzchar(shared_libraries) & dir.exists(shared_libraries)]
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
  providers <- c("XE25", "JPN", "TUR", "CHN", "USA", "XER", "XOC", "CAN", "XLM", "CIS")
  recipients <- c("XSA", "IND", "XNF", "XAF", "XSE", "BRA", "XME")
  provider_aggregate <- "Rprovider15th"
  recipient_aggregate <- "Rrecipient15th"
  variables_needed <- c("Emi_CO2", "CNS", "GDP_MER", "Prc_Car", "Pol_Cos_Cns_Los", "Pol_Cos_Cns_Los_rat", "Pol_Cos_Cns_Los_rat_NPV_5pc",
      "Pol_Cos_GDP_Los_rat_NPV_5pc", "Rev_gov_Tax_Car_Tax")
  gdx_file <- file.path(project_root, "data", "All", "global_17_IAMC.gdx")
  output_dir <- file.path(project_root, "output", "Figure", "analysis")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  legacy_efficiency_png <- file.path(output_dir, "02_transfer_efficiency_2050.png")
  if (file.exists(legacy_efficiency_png)) unlink(legacy_efficiency_png)
  legacy_emissions_png <- file.path(output_dir, "01_transfer_emissions_equivalent_2050.png")
  if (file.exists(legacy_emissions_png)) unlink(legacy_emissions_png)
  read_iamc_gdxrrw <- function(path) {
      if (!requireNamespace("gdxrrw", quietly = TRUE))
          return(NULL)
      gams_candidates <- c(Sys.getenv("GAMSDIR", unset = ""), "C:/GAMS/34", "C:/GAMS/win64/26.1")
      gams_candidates <- unique(gams_candidates[nzchar(gams_candidates)])
      for (candidate in gams_candidates) {
          if (dir.exists(candidate)) {
              loaded <- suppressWarnings(try(gdxrrw::igdx(candidate), silent = TRUE))
              if (inherits(loaded, "try-error"))
                  next
              raw <- try(gdxrrw::rgdx.param(path, "IAMC_template"), silent = TRUE)
              if (inherits(raw, "try-error"))
                  next
              return(data.frame(SCENARIO = as.character(raw$SCENARIO), REMF = as.character(raw$REMF), VEMF = as.character(raw$VEMF),
                  Year = as.numeric(as.character(raw$YEMF)), Value = as.numeric(raw$IAMC_Template), stringsAsFactors = FALSE))
          }
      }
      NULL
  }
  find_gdxdump <- function() {
      from_path <- unname(Sys.which(c("gdxdump", "gdxdump.exe")))
      from_path <- from_path[nzchar(from_path)]
      candidates <- c(from_path, "C:/GAMS/34/gdxdump.exe", "C:/GAMS/win64/26.1/gdxdump.exe")
      candidates <- unique(candidates)
      hit <- candidates[file.exists(candidates)]
      if (!length(hit))
          return(NA)
      normalizePath(hit[[1]], winslash = "/")
  }
  read_iamc_gdxdump <- function(path, scenarios, variables) {
      gdxdump <- find_gdxdump()
      if (is.na(gdxdump)) {
          stop("Neither a working gdxrrw package nor gdxdump was found.")
      }
      dump_file <- tempfile("IAMC_template_", fileext = ".csv")
      on.exit(unlink(dump_file), add = TRUE)
      result <- system2(gdxdump, args = c(shQuote(normalizePath(path, winslash = "/")), "symb=IAMC_template", "format=csv",
          paste0("output=", shQuote(dump_file))), stdout = TRUE, stderr = TRUE)
      status <- attr(result, "status")
      if (is.null(status))
          status <- 0L
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
          if (!length(lines))
              break
          scenario_hit <- rep(FALSE, length(lines))
          for (scenario in scenarios) {
              scenario_hit <- scenario_hit | startsWith(lines, paste0("\"", scenario, "\","))
          }
          variable_hit <- rep(FALSE, length(lines))
          for (variable in variables) {
              variable_hit <- variable_hit | grepl(paste0(",\"", variable, "\","), lines, fixed = TRUE)
          }
          selected <- lines[scenario_hit & variable_hit]
          if (length(selected)) {
              piece_id <- piece_id + 1L
              pieces[[piece_id]] <- read.csv(text = paste(selected, collapse = "\n"), header = FALSE, col.names = c("SCENARIO",
                  "REMF", "VEMF", "Year", "Value"), stringsAsFactors = FALSE)
          }
      }
      if (!length(pieces))
          stop("No required records were read from IAMC_template.")
      do.call(rbind, pieces)
  }
  message("Reading IAMC_template from: ", gdx_file)
  iamc <- read_iamc_gdxrrw(gdx_file)
  if (is.null(iamc)) {
      message("A working gdxrrw installation was not found; using gdxdump fallback.")
      iamc <- read_iamc_gdxdump(gdx_file, scenarios_needed, variables_needed)
  }
  iamc <- iamc[iamc$SCENARIO %in% scenarios_needed & iamc$VEMF %in% variables_needed & is.finite(iamc$Year) & is.finite(iamc$Value),
      c("SCENARIO", "REMF", "VEMF", "Year", "Value")]
  one_value <- function(variable, scenario, region, year) {
      values <- iamc$Value[iamc$VEMF == variable & iamc$SCENARIO == scenario & iamc$REMF == region & iamc$Year == year]
      if (length(values) != 1L || !is.finite(values)) {
          stop("Expected one finite value for ", variable, ", ", scenario, ", ", region, ", ", year, "; found ", length(values),
              ".")
      }
      values
  }
  sum_values <- function(variable, scenario, regions, year) {
      rows <- iamc[iamc$VEMF == variable & iamc$SCENARIO == scenario & iamc$REMF %in% regions & iamc$Year == year, ]
      if (nrow(rows) != length(regions) || any(!is.finite(rows$Value))) {
          stop("Expected one finite ", variable, " value for each of ", length(regions), " regions in ", year, ".")
      }
      sum(rows$Value)
  }
  transfer_rows <- iamc[iamc$VEMF == "Rev_gov_Tax_Car_Tax" & iamc$SCENARIO == scenario_aid & iamc$REMF %in% providers & iamc$Year >=
      2030 & iamc$Year <= 2050, ]
  if (!nrow(transfer_rows)) stop("No provider carbon-tax revenue records found.")
  transfer_annual <- aggregate(Value ~ Year, data = transfer_rows, FUN = sum)
  names(transfer_annual)[names(transfer_annual) == "Value"] <- "Transfer_million_USD2010"
  transfer_annual <- transfer_annual[order(transfer_annual$Year), ]
  model_years <- seq(2030, 2050, by = 5)
  if (!all(model_years %in% transfer_annual$Year)) {
      stop("The transfer series does not contain every five-year model point, 2030-2050.")
  }
  transfer_2050_musd <- transfer_annual$Transfer_million_USD2010[transfer_annual$Year == 2050]
  if (length(transfer_2050_musd) != 1L || transfer_2050_musd <= 0) {
      stop("The 2050 transfer pool must be a single positive value.")
  }
  world_carbon_price_2050 <- one_value("Prc_Car", scenario_aid, "World", 2050)
  if (world_carbon_price_2050 <= 0) stop("The 2050 world carbon price must be positive.")
  provider_gdp_2050_busd <- sum(iamc$Value[iamc$VEMF == "GDP_MER" & iamc$SCENARIO == scenario_aid & iamc$REMF %in% providers &
      iamc$Year == 2050])
  transfer_share_provider_gdp <- 100 * (transfer_2050_musd/1000)/provider_gdp_2050_busd
  if (!is.finite(transfer_share_provider_gdp) || transfer_share_provider_gdp > 100) {
      stop("The assumed carbon-revenue unit is inconsistent with provider GDP.")
  }
  provider_emissions_check <- iamc[iamc$VEMF == "Emi_CO2" & iamc$SCENARIO == scenario_aid & iamc$REMF %in% providers & iamc$Year ==
      2050, c("REMF", "Value")]
  provider_price_check <- iamc[iamc$VEMF == "Prc_Car" & iamc$SCENARIO == scenario_aid & iamc$REMF %in% providers & iamc$Year ==
      2050, c("REMF", "Value")]
  names(provider_emissions_check)[2] <- "Emissions_MtCO2"
  names(provider_price_check)[2] <- "Price_USD_per_tCO2"
  tax_base_check <- merge(provider_emissions_check, provider_price_check, by = "REMF")
  if (nrow(tax_base_check) != length(providers)) {
      stop("Could not construct the 2050 emissions-times-price unit check.")
  }
  emissions_price_proxy_musd <- sum(tax_base_check$Emissions_MtCO2 * tax_base_check$Price_USD_per_tCO2)
  revenue_to_emissions_price_proxy <- transfer_2050_musd/emissions_price_proxy_musd
  billion_interpretation_mismatch <- (transfer_2050_musd * 1000)/emissions_price_proxy_musd
  colour_developed <- "#3F6C8E"
  colour_developing <- "#B6573F"
  colour_transfer <- "#8E8E8E"
  ink <- "#252525"
  grid_colour <- "#E2E2E2"
  theme_nature <- function() {
      theme_classic(base_family = "Arial", base_size = 8) + theme(text = element_text(colour = ink), axis.line = element_line(linewidth = 0.3,
          colour = ink), axis.line.x = element_blank(), axis.ticks = element_line(linewidth = 0.25, colour = ink), axis.ticks.length = grid::unit(1.2,
          "mm"), axis.title = element_text(size = 8.2), axis.text = element_text(size = 7.4, colour = ink), plot.title = element_blank(),
          plot.subtitle = element_blank(), plot.caption = element_blank(), panel.grid.major.y = element_line(colour = grid_colour,
              linewidth = 0.22), panel.grid.minor = element_blank(), legend.position = "bottom", legend.justification = "left",
          legend.box.just = "left", legend.title = element_text(size = 7.4, face = "plain"), legend.text = element_text(size = 7.4),
          legend.key.height = grid::unit(3.5, "mm"), legend.key.width = grid::unit(7, "mm"), legend.spacing.y = grid::unit(0.3,
              "mm"), legend.margin = margin(t = 2, unit = "mm"), plot.margin = margin(4, 6, 3, 4))
  }
  save_png <- function(filename, plot, height_mm) {
      ggsave(filename = file.path(output_dir, filename), plot = plot, device = "png", type = "cairo", width = 180, height = height_mm,
          units = "mm", dpi = 400, bg = "white")
  }
  annual_emissions_data <- do.call(rbind, lapply(model_years, function(year) {
      transfer_musd <- transfer_annual$Transfer_million_USD2010[transfer_annual$Year == year]
      world_price <- one_value("Prc_Car", scenario_aid, "World", year)
      if (length(transfer_musd) != 1L || world_price <= 0) {
          stop("Missing transfer or non-positive world carbon price in ", year, ".")
      }
      developed_actual <- (one_value("Emi_CO2", scenario_aid, provider_aggregate, year) - one_value("Emi_CO2", scenario_def,
          provider_aggregate, year))/1000
      developing_actual <- (one_value("Emi_CO2", scenario_aid, recipient_aggregate, year) - one_value("Emi_CO2", scenario_def,
          recipient_aggregate, year))/1000
      transfer_equivalent <- transfer_musd/world_price/1000
      data.frame(Year = rep(year, 4), Region = rep(c("Developed regions", "Developing regions"), 2), Measure = rep(c("Actual emissions response (Aid - Def)",
          "Transfer-equivalent quantity (funds / carbon price)"), each = 2), Change_GtCO2 = c(developed_actual, developing_actual,
          -transfer_equivalent, transfer_equivalent), World_carbon_price = world_price)
  }))
  annual_emissions_data$Region <- factor(annual_emissions_data$Region, levels = c("Developed regions", "Developing regions"))
  annual_emissions_data$Measure <- factor(annual_emissions_data$Measure, levels = c("Actual emissions response (Aid - Def)",
      "Transfer-equivalent quantity (funds / carbon price)"))
  p_emissions <- ggplot(annual_emissions_data, aes(x = Year, y = Change_GtCO2, colour = Region, group = Region)) + geom_hline(yintercept = 0,
      linewidth = 0.35, colour = ink) + geom_line(linewidth = 0.68) + geom_point(shape = 21, fill = "white", stroke = 0.5,
      size = 1.85) + facet_wrap(vars(Measure), ncol = 1, scales = "free_y", strip.position = "top") + scale_colour_manual(values = c(`Developed regions` = colour_developed,
      `Developing regions` = colour_developing), guide = guide_legend(title = "Region", nrow = 1)) + scale_x_continuous(breaks = model_years,
      limits = c(2029.2, 2050.8), expand = expansion(mult = c(0, 0))) + scale_y_continuous(name = expression("Change in CO"[2] *
      " emissions (Gt CO"[2] * " " * yr^{
      -1
  } * ")"), labels = label_number(accuracy = 0.1), expand = expansion(mult = c(0.13, 0.13))) + labs(x = NULL) + theme_nature() +
      theme(panel.spacing.y = grid::unit(4, "mm"), strip.placement = "outside", strip.background = element_blank(), strip.text.x = element_text(face = "plain",
          size = 7.8, hjust = 0, margin = margin(1, 0, 2, 0, unit = "mm")))
  save_png("01_transfer_emissions_equivalent_2030_2050.png", p_emissions, 102)
  equivalent_max <- max(abs(annual_emissions_data$Change_GtCO2[annual_emissions_data$Measure == "Transfer-equivalent quantity (funds / carbon price)"]))
  combined_emissions_data <- annual_emissions_data
  actual_label_data <- combined_emissions_data[combined_emissions_data$Measure == "Actual emissions response (Aid - Def)",
      ]
  actual_label_data$Label_y <- ifelse(actual_label_data$Change_GtCO2 >= 0, equivalent_max * 0.065, -equivalent_max * 0.065)
  p_emissions_combined <- ggplot(combined_emissions_data, aes(x = Year, y = Change_GtCO2, colour = Region, linetype = Measure,
      shape = Measure, group = interaction(Region, Measure))) + geom_hline(yintercept = 0, linewidth = 0.35, colour = ink) +
      geom_line(linewidth = 0.68) + geom_point(fill = "white", stroke = 0.5, size = 1.85) + geom_segment(data = actual_label_data,
      aes(xend = Year, yend = Label_y), linewidth = 0.22, show.legend = FALSE) + geom_text(data = actual_label_data, aes(y = Label_y,
      label = sprintf("%+.2f", Change_GtCO2)), family = "Arial", fontface = "plain", size = 2.05, vjust = ifelse(actual_label_data$Label_y >
      0, -0.35, 1.25), show.legend = FALSE) + scale_colour_manual(values = c(`Developed regions` = colour_developed, `Developing regions` = colour_developing),
      guide = guide_legend(order = 1, title = "Region", nrow = 1)) + scale_linetype_manual(values = c(`Actual emissions response (Aid - Def)` = "solid",
      `Transfer-equivalent quantity (funds / carbon price)` = "22"), guide = guide_legend(order = 2, title = "Measure", nrow = 1)) +
      scale_shape_manual(values = c(`Actual emissions response (Aid - Def)` = 21, `Transfer-equivalent quantity (funds / carbon price)` = 24),
          guide = "none") + scale_x_continuous(breaks = model_years, limits = c(2029.2, 2050.8), expand = expansion(mult = c(0,
      0))) + scale_y_continuous(name = expression("Emission change or equivalent quantity (Gt CO"[2] * " " * yr^{
      -1
  } * ")"), labels = label_number(accuracy = 1), expand = expansion(mult = c(0.1, 0.1))) + labs(x = NULL) + theme_nature() +
      theme(legend.box = "vertical")
  save_png("01b_transfer_emissions_equivalent_combined_2030_2050.png", p_emissions_combined, 88)
  developed_emissions_change <- annual_emissions_data$Change_GtCO2[annual_emissions_data$Year == 2050 & annual_emissions_data$Region ==
      "Developed regions" & annual_emissions_data$Measure == "Actual emissions response (Aid - Def)"]
  developing_emissions_change <- annual_emissions_data$Change_GtCO2[annual_emissions_data$Year == 2050 & annual_emissions_data$Region ==
      "Developing regions" & annual_emissions_data$Measure == "Actual emissions response (Aid - Def)"]
  allowance_equivalent_gt <- abs(annual_emissions_data$Change_GtCO2[annual_emissions_data$Year == 2050 & annual_emissions_data$Region ==
      "Developing regions" & annual_emissions_data$Measure == "Transfer-equivalent quantity (funds / carbon price)"])
  annual_efficiency <- do.call(rbind, lapply(model_years, function(year) {
      transfer_busd <- transfer_annual$Transfer_million_USD2010[transfer_annual$Year == year]/1000
      recipient_benefit_busd <- sum_values("Pol_Cos_Cns_Los", scenario_def, recipients, year) - sum_values("Pol_Cos_Cns_Los",
          scenario_aid, recipients, year)
      provider_burden_busd <- sum_values("Pol_Cos_Cns_Los", scenario_aid, providers, year) - sum_values("Pol_Cos_Cns_Los",
          scenario_def, providers, year)
      recipient_cns_gain_busd <- sum_values("CNS", scenario_aid, recipients, year) - sum_values("CNS", scenario_def, recipients,
          year)
      provider_cns_loss_busd <- sum_values("CNS", scenario_def, providers, year) - sum_values("CNS", scenario_aid, providers,
          year)
      data.frame(Year = year, Transfer_billion_USD2010 = transfer_busd, Recipient_benefit_billion_USD2010 = recipient_benefit_busd,
          Provider_burden_billion_USD2010 = provider_burden_busd, Recipient_efficiency = recipient_benefit_busd/transfer_busd,
          Provider_burden_ratio = provider_burden_busd/transfer_busd, Recipient_identity_gap = recipient_benefit_busd - recipient_cns_gain_busd,
          Provider_identity_gap = provider_burden_busd - provider_cns_loss_busd)
  }))
  if (any(abs(annual_efficiency$Recipient_identity_gap) > 1e-06) || any(abs(annual_efficiency$Provider_identity_gap) > 1e-06)) {
      stop("Pol_Cos_Cns_Los and CNS imply inconsistent annual consumption effects.")
  }
  if (any(annual_efficiency$Transfer_billion_USD2010 <= 0)) {
      stop("Annual transfer amounts must be positive to calculate efficiency.")
  }
  money_plot_data <- rbind(data.frame(Year = annual_efficiency$Year, PlotYear = annual_efficiency$Year - 0.68, Amount_trillion_USD2010 = annual_efficiency$Recipient_benefit_billion_USD2010/1000,
      Series = "Recipient consumption gain"), data.frame(Year = annual_efficiency$Year, PlotYear = annual_efficiency$Year -
      0.68, Amount_trillion_USD2010 = -annual_efficiency$Provider_burden_billion_USD2010/1000, Series = "Provider consumption loss"),
      data.frame(Year = annual_efficiency$Year, PlotYear = annual_efficiency$Year + 0.68, Amount_trillion_USD2010 = annual_efficiency$Transfer_billion_USD2010/1000,
          Series = "Transfer received/provided"), data.frame(Year = annual_efficiency$Year, PlotYear = annual_efficiency$Year +
          0.68, Amount_trillion_USD2010 = -annual_efficiency$Transfer_billion_USD2010/1000, Series = "Transfer received/provided"))
  money_plot_data$Series <- factor(money_plot_data$Series, levels = c("Recipient consumption gain", "Provider consumption loss",
      "Transfer received/provided"))
  ratio_plot_data <- rbind(data.frame(Year = annual_efficiency$Year, Signed_ratio = annual_efficiency$Recipient_efficiency,
      Ratio = "Recipient: gain / transfer"), data.frame(Year = annual_efficiency$Year, Signed_ratio = -annual_efficiency$Provider_burden_ratio,
      Ratio = "Provider: loss / transfer"))
  ratio_plot_data$Ratio <- factor(ratio_plot_data$Ratio, levels = c("Recipient: gain / transfer", "Provider: loss / transfer"))
  money_max <- max(abs(money_plot_data$Amount_trillion_USD2010))
  ratio_max <- max(abs(ratio_plot_data$Signed_ratio))
  efficiency_axis_scale <- 0.82 * money_max/ratio_max
  ratio_plot_data$Scaled_ratio <- ratio_plot_data$Signed_ratio * efficiency_axis_scale
  ratio_plot_display <- ratio_plot_data[ratio_plot_data$Year >= 2035, ]
  p_efficiency <- ggplot() + geom_col(data = money_plot_data, aes(x = PlotYear, y = Amount_trillion_USD2010, fill = Series),
      width = 1.08, colour = NA, alpha = 0.88) + geom_hline(yintercept = 0, colour = ink, linewidth = 0.35) + geom_line(data = ratio_plot_display,
      aes(x = Year, y = Scaled_ratio, colour = Ratio, group = Ratio), linewidth = 0.68) + geom_point(data = ratio_plot_display,
      aes(x = Year, y = Scaled_ratio, colour = Ratio), shape = 18, size = 2.1) + geom_text(data = ratio_plot_display, aes(x = Year,
      y = Scaled_ratio, label = sprintf("%.1f", abs(Signed_ratio)), colour = Ratio, vjust = ifelse(Signed_ratio >= 0, -0.95,
          1.45)), family = "Arial", fontface = "plain", size = 2.05, show.legend = FALSE) + scale_fill_manual(values = c(`Recipient consumption gain` = colour_developing,
      `Provider consumption loss` = colour_developed, `Transfer received/provided` = colour_transfer), breaks = "Transfer received/provided",
      labels = "Transferred money", guide = guide_legend(order = 3, title = NULL, nrow = 1)) + scale_colour_manual(values = c(`Recipient: gain / transfer` = colour_developing,
      `Provider: loss / transfer` = colour_developed), labels = c(`Recipient: gain / transfer` = "Developing", `Provider: loss / transfer` = "Developed"),
      guide = guide_legend(order = 1, title = NULL, nrow = 1)) + scale_x_continuous(breaks = model_years, limits = c(2028.2,
      2051.8), expand = expansion(mult = c(0, 0))) + scale_y_continuous(name = "Annual consumption change (trillion US$2010 yr-1)",
      labels = function(x) label_number(accuracy = 0.1)(abs(x)), sec.axis = sec_axis(~./efficiency_axis_scale, name = "Consumption/Transfer",
          labels = function(x) label_number(accuracy = 0.1)(abs(x))), expand = expansion(mult = c(0.1, 0.1))) + labs(x = NULL) +
      coord_cartesian(clip = "off") + theme_nature() + theme(panel.grid = element_blank(), panel.grid.major.y = element_blank(),
      panel.border = element_rect(fill = NA, colour = ink, linewidth = 0.3), axis.line = element_blank(), legend.box = "vertical",
      axis.title.y.right = element_text(colour = "#4B4B4B"), axis.text.y.right = element_text(colour = "#4B4B4B"))
  save_png("02_transfer_efficiency_2030_2050.png", p_efficiency, 94)
  developing_loss_reduction_busd <- annual_efficiency$Recipient_benefit_billion_USD2010[annual_efficiency$Year == 2050]
  provider_burden_2050_busd <- annual_efficiency$Provider_burden_billion_USD2010[annual_efficiency$Year == 2050]
  transfer_2050_busd <- transfer_2050_musd/1000
  transfer_efficiency <- annual_efficiency$Recipient_efficiency[annual_efficiency$Year == 2050]
  provider_burden_ratio_2050 <- annual_efficiency$Provider_burden_ratio[annual_efficiency$Year == 2050]
  identity_gap_busd <- max(abs(annual_efficiency$Recipient_identity_gap), abs(annual_efficiency$Provider_identity_gap))
  aggregate_loss_reduction_busd <- one_value("Pol_Cos_Cns_Los", scenario_def, recipient_aggregate, 2050) - one_value("Pol_Cos_Cns_Los",
      scenario_aid, recipient_aggregate, 2050)
  aggregate_row_efficiency <- aggregate_loss_reduction_busd/transfer_2050_busd
  aggregate_provider_transfer_busd <- one_value("Rev_gov_Tax_Car_Tax", scenario_aid, provider_aggregate, 2050)/1000
  aggregate_both_efficiency <- aggregate_loss_reduction_busd/aggregate_provider_transfer_busd
  loss_data <- iamc[iamc$VEMF == "Pol_Cos_Cns_Los_rat" & iamc$SCENARIO %in% scenarios_needed & iamc$REMF %in% c(provider_aggregate,
      recipient_aggregate) & iamc$Year %in% model_years, ]
  if (nrow(loss_data) != length(model_years) * 2L * 2L) {
      stop("Expected 20 consumption-loss observations for 2030-2050.")
  }
  loss_data$Region <- factor(ifelse(loss_data$REMF == provider_aggregate, "Developed regions", "Developing regions"), levels = c("Developed regions",
      "Developing regions"))
  loss_data$Policy <- factor(ifelse(loss_data$SCENARIO == scenario_def, "No transfer (Def)", "Climate finance (Aid)"), levels = c("No transfer (Def)",
      "Climate finance (Aid)"))
  names(loss_data)[names(loss_data) == "Value"] <- "Consumption_loss_pct"
  loss_data <- loss_data[order(loss_data$Region, loss_data$Policy, loss_data$Year), ]
  p_timing <- ggplot(loss_data, aes(x = Year, y = Consumption_loss_pct, colour = Region, linetype = Policy, shape = Policy,
      group = interaction(Region, Policy))) + geom_hline(yintercept = 0, linewidth = 0.35, colour = ink) + geom_line(linewidth = 0.68) +
      geom_point(stroke = 0.55, size = 1.9) + scale_colour_manual(values = c(`Developed regions` = colour_developed, `Developing regions` = colour_developing),
      labels = c(`Developed regions` = "Developed", `Developing regions` = "Developing"), guide = guide_legend(order = 1, title = NULL,
          nrow = 1)) + scale_linetype_manual(values = c(`No transfer (Def)` = "22", `Climate finance (Aid)` = "solid"), labels = c(`No transfer (Def)` = "Def",
      `Climate finance (Aid)` = "Aid"), guide = guide_legend(order = 2, title = NULL, nrow = 1)) + scale_shape_manual(values = c(`No transfer (Def)` = 1,
      `Climate finance (Aid)` = 16), labels = c(`No transfer (Def)` = "Def", `Climate finance (Aid)` = "Aid"), guide = guide_legend(order = 2,
      title = NULL, nrow = 1)) + scale_x_continuous(breaks = model_years, expand = expansion(mult = c(0, 0))) + scale_y_continuous(name = "Consumption loss rate (%)",
      labels = label_number(accuracy = 1), expand = expansion(mult = c(0.08, 0.09))) + labs(x = NULL) + coord_cartesian(xlim = c(2028.3,
      2051.7), clip = "off") + theme_nature() + theme(panel.grid = element_blank(), panel.grid.major.y = element_blank(), panel.border = element_rect(fill = NA,
      colour = ink, linewidth = 0.3), axis.line = element_blank(), panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA))
  cumulative_region_codes <- c("World", provider_aggregate, recipient_aggregate)
  cumulative_region_labels <- c("World", "Developed regions", "Developing regions")
  cumulative_loss_data <- iamc[iamc$VEMF == "Pol_Cos_Cns_Los_rat_NPV_5pc" & iamc$SCENARIO %in% scenarios_needed & iamc$REMF %in%
      cumulative_region_codes & iamc$Year == 2050, ]
  if (nrow(cumulative_loss_data) != 6L) {
      stop("Expected six 2050 cumulative consumption-loss observations.")
  }
  if (anyDuplicated(cumulative_loss_data[c("SCENARIO", "REMF", "Year")])) {
      stop("Duplicate 2050 cumulative consumption-loss observations.")
  }
  cumulative_loss_data$Region <- factor(cumulative_region_labels[match(cumulative_loss_data$REMF, cumulative_region_codes)],
      levels = cumulative_region_labels)
  cumulative_loss_data$Policy <- factor(ifelse(cumulative_loss_data$SCENARIO == scenario_def, "No transfer (Def)", "Climate finance (Aid)"),
      levels = c("No transfer (Def)", "Climate finance (Aid)"))
  names(cumulative_loss_data)[names(cumulative_loss_data) == "Value"] <- "Cumulative_loss_pct"
  p_cumulative_loss <- ggplot(cumulative_loss_data, aes(x = Region, y = Cumulative_loss_pct, colour = Region, shape = Policy,
      group = Policy)) + geom_hline(yintercept = 0, linewidth = 0.35, colour = ink) + geom_point(position = position_dodge(width = 0.3),
      size = 2.35, stroke = 0.6) + scale_colour_manual(values = c(World = ink, `Developed regions` = colour_developed, `Developing regions` = colour_developing),
      limits = cumulative_region_labels, drop = FALSE, guide = "none") + scale_shape_manual(values = c(`No transfer (Def)` = 1,
      `Climate finance (Aid)` = 16), guide = "none") + scale_x_discrete(labels = c(World = "World", `Developed regions` = "Developed",
      `Developing regions` = "Developing")) + scale_y_continuous(name = "Cumulative consumption loss in 2050 (%)", labels = label_number(accuracy = 1),
      expand = expansion(mult = c(0.15, 0.18))) + labs(x = NULL) + theme_nature() + theme(panel.grid = element_blank(), panel.grid.major.y = element_blank(),
      panel.border = element_rect(fill = NA, colour = ink, linewidth = 0.3), axis.line = element_blank(), panel.background = element_rect(fill = "white",
          colour = NA), plot.background = element_rect(fill = "white", colour = NA), axis.text.x = element_text(angle = 35,
          hjust = 1, vjust = 1), legend.position = "none")
  cumulative_gdp_data <- iamc[iamc$VEMF == "Pol_Cos_GDP_Los_rat_NPV_5pc" & iamc$SCENARIO %in% scenarios_needed & iamc$REMF %in%
      cumulative_region_codes & iamc$Year == 2050, ]
  if (nrow(cumulative_gdp_data) != 6L) {
      stop("Expected six 2050 cumulative GDP-loss observations.")
  }
  if (anyDuplicated(cumulative_gdp_data[c("SCENARIO", "REMF", "Year")])) {
      stop("Duplicate 2050 cumulative GDP-loss observations.")
  }
  cumulative_gdp_data$Region <- factor(cumulative_region_labels[match(cumulative_gdp_data$REMF, cumulative_region_codes)],
      levels = cumulative_region_labels)
  cumulative_gdp_data$Policy <- factor(ifelse(cumulative_gdp_data$SCENARIO == scenario_def, "No transfer (Def)", "Climate finance (Aid)"),
      levels = c("No transfer (Def)", "Climate finance (Aid)"))
  names(cumulative_gdp_data)[names(cumulative_gdp_data) == "Value"] <- "Cumulative_GDP_loss_pct"
  p_cumulative_gdp <- ggplot(cumulative_gdp_data, aes(x = Region, y = Cumulative_GDP_loss_pct, colour = Region, shape = Policy,
      group = Policy)) + geom_hline(yintercept = 0, linewidth = 0.35, colour = ink) + geom_point(position = position_dodge(width = 0.3),
      size = 2.35, stroke = 0.6) + scale_colour_manual(values = c(World = ink, `Developed regions` = colour_developed, `Developing regions` = colour_developing),
      limits = cumulative_region_labels, drop = FALSE, guide = "none") + scale_shape_manual(values = c(`No transfer (Def)` = 1,
      `Climate finance (Aid)` = 16), guide = "none") + scale_x_discrete(labels = c(World = "World", `Developed regions` = "Developed",
      `Developing regions` = "Developing")) + scale_y_continuous(name = "Cumulative GDP loss in 2050 (%)", labels = label_number(accuracy = 1),
      expand = expansion(mult = c(0.15, 0.18))) + labs(x = NULL) + theme_nature() + theme(panel.grid = element_blank(), panel.grid.major.y = element_blank(),
      panel.border = element_rect(fill = NA, colour = ink, linewidth = 0.3), axis.line = element_blank(), panel.background = element_rect(fill = "white",
          colour = NA), plot.background = element_rect(fill = "white", colour = NA), axis.text.x = element_text(angle = 35,
          hjust = 1, vjust = 1), legend.position = "none")
  gdp_scenario_paths <- c(BaU = file.path(project_root, "data", "All", "SSP2_BaU_NoCC_No.gdx"), Def = file.path(project_root,
      "data", "All", "SSP2_400C_2030CP_NoCC_No.gdx"), Aid = file.path(project_root, "data", "All", "SSP2_400C_2030CP_15th_NoCC_No.gdx"))
  missing_gdp_files <- gdp_scenario_paths[!file.exists(gdp_scenario_paths)]
  if (length(missing_gdp_files)) {
      stop("Missing GDP decomposition GDX file(s): ", paste(missing_gdp_files, collapse = ", "))
  }
  if (!requireNamespace("gdxrrw", quietly = TRUE)) {
      stop("gdxrrw is required to read GDP_s for the new decomposition panel.")
  }
  gdp_group_map <- tibble(Region_code = c(providers, recipients), Region_group = c(rep("Developed regions", length(providers)),
      rep("Developing regions", length(recipients))))
  gdp_group_levels <- c("Developed regions", "Developing regions")
  gdp_category_levels <- c("Consumption", "Government spending", "Investment", "Net exports")
  read_gdp_components <- function(scenario_name, path) {
      raw <- gdxrrw::rgdx.param(path, "GDP_s")
      required <- c("Y", "R", "INS_MCR", "GDP_s")
      if (!all(required %in% names(raw))) {
          stop("Unexpected GDP_s columns in ", basename(path), ".")
      }
      raw %>% transmute(Scenario = scenario_name, Year = as.numeric(as.character(Y)), Region_code = as.character(R), Category_code = as.character(INS_MCR),
          Category = case_when(Category_code == "HURB" ~ "Consumption", Category_code == "GOV" ~ "Government spending", Category_code ==
              "S-I" ~ "Investment", Category_code %in% c("ROW", "IMP") ~ "Net exports", TRUE ~ NA), Value = as.numeric(GDP_s))
  }
  gdp_raw <- bind_rows(Map(read_gdp_components, names(gdp_scenario_paths), unname(gdp_scenario_paths)))
  if (any(is.na(gdp_raw$Year)) || any(!is.finite(gdp_raw$Value))) {
      stop("GDP_s contains a non-numeric year or non-finite value.")
  }
  if (any(is.na(gdp_raw$Category))) {
      stop("Unrecognised GDP_s category code(s): ", paste(unique(gdp_raw$Category_code[is.na(gdp_raw$Category)]), collapse = ", "))
  }
  gdp_grouped <- gdp_raw %>% filter(Year == 2050, Region_code %in% gdp_group_map$Region_code) %>% left_join(gdp_group_map,
      by = "Region_code") %>% group_by(Scenario, Region_group, Category) %>% summarise(Value = sum(Value), .groups = "drop") %>%
      complete(Scenario = names(gdp_scenario_paths), Region_group = gdp_group_levels, Category = gdp_category_levels, fill = list(Value = 0))
  expected_gdp_rows <- length(gdp_scenario_paths) * length(gdp_group_levels) * length(gdp_category_levels)
  if (nrow(gdp_grouped) != expected_gdp_rows || anyDuplicated(gdp_grouped[c("Scenario", "Region_group", "Category")])) {
      stop("The GDP scenario-region-category grid is not complete and unique.")
  }
  gdp_contributions <- gdp_grouped %>% group_by(Region_group, Category) %>% mutate(BaU_category_value = Value[Scenario == "BaU"][[1]]) %>%
      ungroup() %>% group_by(Region_group, Scenario) %>% mutate(BaU_total = sum(BaU_category_value), Contribution_pp = 100 *
      (Value - BaU_category_value)/BaU_total) %>% ungroup() %>% filter(Scenario %in% c("Def", "Aid")) %>% mutate(Scenario = factor(Scenario,
      levels = c("Def", "Aid")), Region_group = factor(Region_group, levels = gdp_group_levels), Category = factor(Category,
      levels = gdp_category_levels))
  gdp_totals <- gdp_contributions %>% group_by(Region_group, Scenario) %>% summarise(Scenario_total = sum(Value), BaU_total = first(BaU_total),
      Total_change_pct = sum(Contribution_pp), .groups = "drop") %>% mutate(Direct_change_pct = 100 * (Scenario_total - BaU_total)/BaU_total,
      Label = sprintf("%+.1f", Total_change_pct))
  gdp_decomposition_error <- max(abs(gdp_totals$Total_change_pct - gdp_totals$Direct_change_pct))
  if (!is.finite(gdp_decomposition_error) || gdp_decomposition_error > 1e-10) {
      stop("GDP contributions do not reproduce the total GDP change.")
  }
  gdp_range <- range(c(gdp_contributions$Contribution_pp, gdp_totals$Total_change_pct))
  gdp_label_offset <- max(0.2, 0.035 * diff(gdp_range))
  gdp_totals <- gdp_totals %>% mutate(Label_y = Total_change_pct + if_else(Total_change_pct >= 0, gdp_label_offset, -gdp_label_offset),
      Label_vjust = if_else(Total_change_pct >= 0, 0, 1))
  gdp_component_colours <- c(Consumption = "#0072B2", `Government spending` = "#CC79A7", Investment = "#E69F00", `Net exports` = "#009E73")
  p_gdp_decomposition <- ggplot(gdp_contributions, aes(x = Scenario, y = Contribution_pp, fill = Category)) + geom_hline(yintercept = 0,
      colour = ink, linewidth = 0.35) + geom_col(width = 0.58, colour = "white", linewidth = 0.18) + geom_point(data = gdp_totals,
      aes(x = Scenario, y = Total_change_pct, shape = "Net GDP change"), inherit.aes = FALSE, size = 2.2, stroke = 0.6, fill = "white",
      colour = ink) + geom_text(data = gdp_totals, aes(x = Scenario, y = Label_y, label = Label, vjust = Label_vjust), inherit.aes = FALSE,
      family = "Arial", size = 2, colour = ink) + facet_grid(. ~ Region_group) + scale_fill_manual(values = gdp_component_colours,
      breaks = gdp_category_levels, labels = c(Consumption = "Consumption", `Government spending` = "Government", Investment = "Investment",
          `Net exports` = "Net exports"), name = NULL) + scale_shape_manual(values = c(`Net GDP change` = 23), labels = c(`Net GDP change` = "Net GDP"),
      name = NULL) + scale_y_continuous(name = "Contribution to GDP change relative to BaU (%)", labels = label_number(accuracy = 0.1),
      expand = expansion(mult = c(0.14, 0.17))) + labs(x = NULL) + guides(fill = guide_legend(order = 1, nrow = 2, byrow = TRUE),
      shape = guide_legend(order = 2)) + theme_nature() + theme(panel.grid = element_blank(), panel.border = element_rect(fill = NA,
      colour = ink, linewidth = 0.3), axis.line = element_blank(), strip.background = element_blank(), strip.text = element_text(face = "bold",
      size = 7), panel.spacing.x = grid::unit(3, "mm"))
  p_efficiency_compact <- p_efficiency + guides(colour = "none")
  p_timing_composite <- (wrap_plots(A = p_timing, B = p_cumulative_loss, C = p_efficiency_compact, design = "AAB\nCCC", heights = c(1,
      1.08), guides = "collect") + plot_annotation(tag_levels = "a")) & theme(legend.position = "bottom", legend.box = "horizontal",
      legend.spacing.x = grid::unit(2, "mm"), panel.border = element_rect(fill = NA, colour = ink, linewidth = 0.35), axis.line = element_blank(),
      plot.tag = element_text(family = "Arial", face = "bold", size = 8.5, colour = ink), plot.tag.position = "topleft")
  save_png("03_transfer_timing_consumption_loss.png", p_timing_composite, 158)
  p_timing_consumption_gdp <- (wrap_plots(A = p_timing, B = p_cumulative_loss, C = p_cumulative_gdp, D = p_efficiency_compact,
      E = p_gdp_decomposition, design = "AAAABBCC\nDDDDEEEE", heights = c(0.9, 1.1), guides = "collect") + plot_annotation(tag_levels = "a")) &
      theme(legend.position = "bottom", legend.box = "horizontal", legend.justification = "center", legend.text = element_text(size = 6.5),
          legend.key.width = grid::unit(4.5, "mm"), legend.key.height = grid::unit(3, "mm"), legend.spacing.x = grid::unit(1,
              "mm"), panel.border = element_rect(fill = NA, colour = ink, linewidth = 0.35), axis.line = element_blank(), plot.background = element_rect(fill = "white",
              colour = NA), plot.tag = element_text(family = "Arial", face = "bold", size = 8.5, colour = ink), plot.tag.position = "topleft")
  environment()
}

build_six_panel_parts <- function(project_root) {
  old_dir <- getwd()
  on.exit(setwd(old_dir), add = TRUE)
  setwd(project_root)
  output_dir <- file.path(project_root, "output", "Figure")
  csv_dir <- file.path(output_dir, "csv")
  base_env <- build_transfer_base(project_root)
  required_objects <- c("p_timing", "p_cumulative_loss", "p_cumulative_gdp", "p_gdp_decomposition", "theme_nature", "ink",
      "colour_developed", "colour_developing")
  missing_objects <- required_objects[!vapply(required_objects, exists, logical(1), envir = base_env)]
  if (length(missing_objects)) {
      stop("Missing plot object(s) from base script: ", paste(missing_objects, collapse = ", "))
  }
  theme_nature <- base_env$theme_nature
  ink <- base_env$ink
  colour_developed <- base_env$colour_developed
  colour_developing <- base_env$colour_developing
  recovery_csv <- file.path(csv_dir, "transfer_recovery_scatter_5year_source_data.csv")
  if (!file.exists(recovery_csv)) {
      stop("Missing transfer-recovery source data: ", recovery_csv, ". Run transfer-benefit.R first.")
  }
  plot_years <- seq(2030, 2050, by = 5)
  recovery_data <- read.csv(recovery_csv, stringsAsFactors = FALSE) %>% filter(GDPBasis == "BaU", Year %in% plot_years, is.finite(NetTransferPct),
      is.finite(ConsumptionChangePct)) %>% mutate(Year = factor(Year, levels = plot_years), Type = factor(Type, levels = c("Provider",
      "Recipient")))
  expected_recovery_rows <- length(plot_years) * 17L
  if (nrow(recovery_data) != expected_recovery_rows || anyDuplicated(recovery_data[c("Year", "REMF")])) {
      stop("The BaU-GDP transfer-recovery data are incomplete or duplicated.")
  }
  regression_lines <- recovery_data %>% group_by(Year) %>% group_modify(~{
      model <- lm(ConsumptionChangePct ~ NetTransferPct, data = .x)
      tibble::tibble(Intercept = unname(coef(model)[1]), Slope = unname(coef(model)[2]))
  }) %>% ungroup() %>% filter(as.character(Year) != "2030")
  axis_limits <- c(-50, 50)
  axis_breaks <- seq(-50, 50, by = 10)
  year_colours <- c(`2030` = "#4E79A7", `2035` = "#59A14F", `2040` = "#F28E2B", `2045` = "#E15759", `2050` = "#B07AA1")
  p_recovery <- ggplot(recovery_data, aes(NetTransferPct, ConsumptionChangePct)) + geom_hline(yintercept = 0, colour = ink,
      linewidth = 0.35) + geom_vline(xintercept = 0, colour = ink, linewidth = 0.35) + geom_abline(intercept = 0, slope = 1,
      colour = ink, linetype = "dotted", linewidth = 0.45) + geom_abline(data = regression_lines, aes(intercept = Intercept,
      slope = Slope, colour = Year), inherit.aes = FALSE, linewidth = 0.6, alpha = 0.58, show.legend = FALSE) + geom_point(aes(colour = Year,
      shape = Type), size = 1.8, alpha = 0.84, stroke = 0.55) + scale_colour_manual(values = year_colours, limits = as.character(plot_years),
      drop = FALSE, name = "Year") + scale_shape_manual(values = c(Provider = 16, Recipient = 17), name = NULL) + scale_x_continuous(breaks = axis_breaks,
      labels = label_number(accuracy = 1, suffix = "%"), expand = expansion(mult = 0)) + scale_y_continuous(breaks = axis_breaks,
      labels = label_number(accuracy = 1, suffix = "%"), expand = expansion(mult = 0)) + coord_fixed(ratio = 1, xlim = axis_limits,
      ylim = axis_limits, expand = FALSE) + labs(x = "Annual transfer / BaU GDP (%)", y = "Consumption change under Aid relative to Def (%)") +
      guides(colour = guide_legend(order = 1, nrow = 1, override.aes = list(size = 1.8, alpha = 0.84, shape = 16, linetype = 0,
          linewidth = 0)), shape = guide_legend(order = 2, nrow = 1)) + theme_nature() + theme(panel.grid = element_blank(),
      panel.border = element_rect(fill = NA, colour = ink, linewidth = 0.35), axis.line = element_blank(), legend.position = "bottom",
      legend.justification = "center", legend.box.just = "center", legend.box = "vertical", legend.direction = "horizontal",
      legend.title = element_text(size = 6.5), legend.text = element_text(size = 6.3), legend.key.width = grid::unit(4, "mm"),
      legend.key.height = grid::unit(2.5, "mm"))
  zero_loss_csv <- file.path(csv_dir, "Cumulative_transfer_zero_loss_2050_source_data.csv")
  benchmark_csv <- file.path(csv_dir, "Cumulative_transfer_zero_loss_2050_benchmarks.csv")
  if (!file.exists(zero_loss_csv) || !file.exists(benchmark_csv)) {
      stop("Missing cumulative zero-loss source data. Run ", "Cumulative_transfer_zero_loss_2050.R first.")
  }
  zero_loss_data <- read.csv(zero_loss_csv, stringsAsFactors = FALSE) %>% mutate(Group = factor(Group, levels = c("Provider",
      "Recipient")), Scenario = factor(Scenario, levels = c("Def", "Aid")))
  benchmarks <- read.csv(benchmark_csv, stringsAsFactors = FALSE)
  if (nrow(zero_loss_data) != 4L || anyDuplicated(zero_loss_data[c("Group", "Scenario")])) {
      stop("The cumulative zero-loss point data are incomplete or duplicated.")
  }
  benchmark_value <- function(metric) {
      value <- benchmarks$Value[benchmarks$Metric == metric]
      if (length(value) != 1L || !is.finite(value)) {
          stop("Missing benchmark value: ", metric)
      }
      value
  }
  actual_aid <- benchmark_value("Actual cumulative Aid transfer")
  cge_zero <- benchmark_value("CGE-implied recipient zero-loss funding")
  def_benchmark <- benchmark_value("Def-loss accounting benchmark")
  funding_gap <- benchmark_value("Accounting benchmark minus CGE")
  period_years <- 20
  x_max <- max(actual_aid, cge_zero, def_benchmark) * 1.12
  y_values <- zero_loss_data$CumulativeConsumptionLossRate_pct
  y_span <- diff(range(c(y_values, 0)))
  if (!is.finite(y_span) || y_span == 0) y_span <- 1
  y_min <- min(y_values, 0) - 0.13 * y_span
  y_max <- max(y_values, 0) + 0.18 * y_span
  x_breaks <- pretty(c(0, x_max), n = 6)
  x_breaks <- x_breaks[x_breaks >= 0 & x_breaks <= x_max]
  benchmark_points <- tibble::tibble(Benchmark = factor(c("CGE-implied", "Def-loss benchmark"), levels = c("CGE-implied", "Def-loss benchmark")),
      X = c(cge_zero, def_benchmark), Y = 0, Label = number(c(cge_zero, def_benchmark), accuracy = 0.1), LabelY = c(0.075 *
          y_span, -0.1 * y_span))
  p_zero_loss <- ggplot(zero_loss_data, aes(x = CumulativeAid_trillion_USD2010, y = CumulativeConsumptionLossRate_pct, colour = Group,
      group = Group)) + geom_hline(yintercept = 0, colour = ink, linewidth = 0.4) + geom_vline(xintercept = 0, colour = ink,
      linewidth = 0.4) + geom_line(linetype = "dotted", linewidth = 0.75) + geom_point(aes(shape = Scenario, fill = interaction(Group,
      Scenario)), size = 2.75, stroke = 0.75) + geom_segment(data = tibble::tibble(x = min(cge_zero, def_benchmark), xend = max(cge_zero,
      def_benchmark), y = 0, yend = 0), aes(x = x, xend = xend, y = y, yend = yend), inherit.aes = FALSE, colour = "#999999",
      linewidth = 2.6, alpha = 0.35, lineend = "round") + annotate("label", x = mean(c(cge_zero, def_benchmark)), y = -0.035 *
      y_span, label = number(funding_gap, accuracy = 0.1), size = 2.15, linewidth = 0, fill = alpha("white", 0.92), colour = "#666666") +
      geom_point(data = benchmark_points, aes(x = X, y = Y, shape = Benchmark), inherit.aes = FALSE, size = 2.75, stroke = 0.75,
          colour = ink, fill = "white") + geom_label(data = benchmark_points, aes(x = X, y = LabelY, label = Label), inherit.aes = FALSE,
      size = 2.25, lineheight = 0.95, linewidth = 0, fill = alpha("white", 0.92), colour = ink) + scale_colour_manual(values = c(Provider = colour_developed,
      Recipient = colour_developing), labels = c(Provider = "Developed", Recipient = "Developing"), name = NULL) + scale_fill_manual(values = c(Provider.Def = "white",
      Provider.Aid = colour_developed, Recipient.Def = "white", Recipient.Aid = colour_developing), guide = "none") + scale_shape_manual(values = c(Def = 1,
      Aid = 16, `CGE-implied` = 23, `Def-loss benchmark` = 24), breaks = c("Def", "Aid", "CGE-implied", "Def-loss benchmark"),
      labels = c("Def", "Aid", "CGE zero loss", "Def-loss benchmark"), name = NULL) + scale_x_continuous(breaks = x_breaks,
      labels = label_number(accuracy = 1), limits = c(0, x_max), expand = expansion(mult = c(0, 0)), sec.axis = sec_axis(~./period_years,
          breaks = x_breaks/period_years, labels = label_number(accuracy = 0.1), name = "Average annual amount, 2030-2050\n(trillion US$2010/yr)")) +
      scale_y_continuous(labels = label_number(accuracy = 1, suffix = "%"), limits = c(y_min, y_max), expand = expansion(mult = c(0,
          0))) + coord_cartesian(clip = "off") + labs(x = "Cumulative transfer, 2030-2050 (trillion US$2010)", y = "Cumulative consumption loss rate in 2050") +
      guides(colour = guide_legend(order = 1, override.aes = list(linetype = 0, size = 2.1)), shape = guide_legend(order = 2,
          override.aes = list(colour = ink))) + theme_nature() + theme(panel.grid = element_blank(), panel.border = element_rect(fill = NA,
      colour = ink, linewidth = 0.35), axis.line = element_blank(), legend.position = "inside", legend.position.inside = c(0.82,
      0.76), legend.justification = c(0.5, 0.5), legend.direction = "vertical", legend.background = element_rect(fill = "white",
      colour = NA), legend.text = element_text(size = 6.4), legend.key.height = grid::unit(2.8, "mm"), legend.key.width = grid::unit(4,
      "mm"), axis.title.x.top = element_text(size = 7.5))
  environment()
}

build_main_figure_3 <- function(project_root) {
  old_dir <- getwd()
  on.exit(setwd(old_dir), add = TRUE)
  setwd(project_root)
  panel_env <- build_six_panel_parts(project_root)
  required_objects <- c("base_env", "p_recovery", "p_zero_loss", "ink")
  missing_objects <- required_objects[!vapply(required_objects, exists, logical(1), envir = panel_env)]
  if (length(missing_objects)) {
      stop("Missing panel object(s): ", paste(missing_objects, collapse = ", "))
  }
  base_env <- panel_env$base_env
  ink <- panel_env$ink
  required_base_objects <- c("p_timing", "p_gdp_decomposition")
  missing_base_objects <- required_base_objects[!vapply(required_base_objects, exists, logical(1), envir = base_env)]
  if (length(missing_base_objects)) {
      stop("Missing base panel object(s): ", paste(missing_base_objects, collapse = ", "))
  }
  timing_world_data <- base_env$iamc %>% dplyr::filter(VEMF == "Pol_Cos_Cns_Los_rat", SCENARIO %in% base_env$scenarios_needed,
      REMF == "World", Year %in% base_env$model_years) %>% dplyr::mutate(Region = factor("World", levels = c("World", "Developed regions",
      "Developing regions")), Policy = factor(ifelse(SCENARIO == base_env$scenario_def, "No transfer (Def)", "Climate finance (Aid)"),
      levels = c("No transfer (Def)", "Climate finance (Aid)"))) %>% dplyr::rename(Consumption_loss_pct = Value)
  if (nrow(timing_world_data) != length(base_env$model_years) * 2L || anyDuplicated(timing_world_data[c("SCENARIO", "Year")])) {
      stop("The World timing data are incomplete or duplicated.")
  }
  timing_data_with_world <- dplyr::bind_rows(base_env$loss_data %>% dplyr::mutate(Region = factor(as.character(Region), levels = c("World",
      "Developed regions", "Developing regions"))), timing_world_data)
  p_timing_world <- base_env$p_timing
  p_timing_world$data <- timing_data_with_world
  p_timing_world <- p_timing_world + scale_colour_manual(values = c(World = ink, `Developed regions` = base_env$colour_developed,
      `Developing regions` = base_env$colour_developing), breaks = c("World", "Developed regions", "Developing regions"), labels = c(World = "World",
      `Developed regions` = "Developed", `Developing regions` = "Developing"), guide = guide_legend(order = 1, title = NULL,
      nrow = 1))
  zero_loss_x_breaks <- panel_env$x_breaks
  period_years <- panel_env$period_years
  p_zero_loss_clean <- panel_env$p_zero_loss
  if (length(p_zero_loss_clean$layers) < 8L) {
      stop("Unexpected zero-loss panel structure; cannot remove gap annotation.")
  }
  p_zero_loss_clean$layers <- p_zero_loss_clean$layers[-c(5L, 6L)]
  world_zero_loss_data <- base_env$iamc %>% dplyr::filter(VEMF == "Pol_Cos_Cns_Los_rat_NPV_5pc", SCENARIO %in% base_env$scenarios_needed,
      REMF == "World", Year == 2050) %>% dplyr::transmute(Group = factor("World", levels = c("World", "Provider", "Recipient")),
      Scenario = factor(ifelse(SCENARIO == base_env$scenario_def, "Def", "Aid"), levels = c("Def", "Aid")), Region = REMF,
      ScenarioCode = SCENARIO, CumulativeAid_trillion_USD2010 = ifelse(Scenario == "Def", 0, panel_env$actual_aid), AverageAnnualAid_trillion_USD2010_per_year = CumulativeAid_trillion_USD2010/period_years,
      CumulativeConsumptionLossRate_pct = Value)
  if (nrow(world_zero_loss_data) != 2L || anyDuplicated(world_zero_loss_data["Scenario"])) {
      stop("The World cumulative-loss data are incomplete or duplicated.")
  }
  zero_loss_data_with_world <- dplyr::bind_rows(panel_env$zero_loss_data %>% dplyr::mutate(Group = factor(as.character(Group),
      levels = c("World", "Provider", "Recipient"))), world_zero_loss_data)
  p_zero_loss_clean$data <- zero_loss_data_with_world
  p_zero_loss_clean$layers[[3L]]$data <- zero_loss_data_with_world %>% dplyr::filter(Group != "World")
  zero_loss_y_values <- zero_loss_data_with_world$CumulativeConsumptionLossRate_pct
  zero_loss_y_span <- diff(range(c(zero_loss_y_values, 0)))
  if (!is.finite(zero_loss_y_span) || zero_loss_y_span == 0) zero_loss_y_span <- 1
  zero_loss_y_min <- min(zero_loss_y_values, 0) - 0.13 * zero_loss_y_span
  zero_loss_y_max <- max(zero_loss_y_values, 0) + 0.18 * zero_loss_y_span
  p_zero_loss_bottom <- p_zero_loss_clean + scale_colour_manual(values = c(World = ink, Provider = base_env$colour_developed,
      Recipient = base_env$colour_developing), breaks = c("World", "Provider", "Recipient"), labels = c(World = "World", Provider = "Developed",
      Recipient = "Developing"), name = NULL) + scale_fill_manual(values = c(World.Def = "white", World.Aid = ink, Provider.Def = "white",
      Provider.Aid = base_env$colour_developed, Recipient.Def = "white", Recipient.Aid = base_env$colour_developing), guide = "none") +
      scale_x_continuous(breaks = zero_loss_x_breaks, labels = scales::label_number(accuracy = 1), limits = c(0, panel_env$x_max),
          expand = expansion(mult = c(0, 0)), sec.axis = dup_axis(breaks = zero_loss_x_breaks, labels = scales::label_number(accuracy = 0.1,
              scale = 1/period_years), name = "Average annual amount, 2030-2050\n(trillion US$2010/yr)")) + scale_y_continuous(labels = scales::label_number(accuracy = 1,
      suffix = "%"), limits = c(zero_loss_y_min, zero_loss_y_max), expand = expansion(mult = c(0, 0))) + labs(y = "Cumulative consumption loss rate in 2050 (%)") +
      guides(colour = guide_legend(order = 1, nrow = 1, override.aes = list(linetype = 0, size = 2.1)), shape = guide_legend(order = 2,
          nrow = 1, override.aes = list(colour = ink))) + theme(panel.background = element_rect(fill = "white", colour = NA),
      panel.grid = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(), legend.position = "bottom", legend.justification = "center", legend.direction = "horizontal",
      legend.box = "vertical", legend.box.just = "center", legend.background = element_blank(), legend.margin = margin(t = 1,
          r = 0, b = 0, l = 0), legend.spacing.y = grid::unit(0.4, "mm"), legend.key.height = grid::unit(2.8, "mm"), legend.key.width = grid::unit(4,
          "mm"))
  p_recovery_plain_axes <- panel_env$p_recovery + scale_x_continuous(breaks = panel_env$axis_breaks, labels = scales::label_number(accuracy = 1),
      expand = expansion(mult = 0)) + scale_y_continuous(breaks = panel_env$axis_breaks, labels = scales::label_number(accuracy = 1),
      expand = expansion(mult = 0)) + scale_shape_manual(values = c(Provider = 16, Recipient = 17), breaks = c("Provider",
      "Recipient"), labels = c(Provider = "Developed", Recipient = "Developing"), name = NULL) + labs(x = "Annual transfer / GDP (%)",
      y = "Consumption change under\nAid relative to Def (%)") + theme(panel.background = element_rect(fill = "white", colour = NA),
      panel.grid = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(), plot.tag.position = c(0.14, 0.99))
  p_gdp_decomposition_compact <- base_env$p_gdp_decomposition + scale_y_continuous(name = "Contribution to GDP change\nrelative to BaU in 2050 (%)",
      labels = scales::label_number(accuracy = 0.1), expand = expansion(mult = c(0.14, 0.17))) + scale_shape_manual(values = c(`Net GDP change` = 23),
      labels = c(`Net GDP change` = "Net GDP change"), name = NULL) + guides(fill = guide_legend(order = 1, nrow = 2, byrow = TRUE),
      shape = guide_legend(order = 2, nrow = 1)) + theme(legend.position = "bottom", legend.justification = "center", legend.box.just = "center",
      legend.box = "vertical", legend.text = element_text(size = 6.4), legend.key.width = grid::unit(4.5, "mm"), legend.key.height = grid::unit(2.8,
          "mm"), legend.margin = margin(t = 1, r = 0, b = 0, l = 0), strip.text = element_text(face = "plain", size = 7))
  p_four_panel <- (patchwork::wrap_plots(A = p_timing_world, B = p_recovery_plain_axes, C = patchwork::free(p_zero_loss_bottom,
      side = "t"), D = p_gdp_decomposition_compact, design = "AAAAABBBBB\nCCCCCCDDDD", heights = c(1, 1), guides = "keep") +
      patchwork::plot_annotation(tag_levels = "a")) & theme(plot.background = element_rect(fill = "white", colour = NA), plot.tag = element_text(family = "Arial",
      face = "bold", size = 8.5, colour = ink))
  p_four_panel
}

build_main_figure_4 <- function(project_root) {
  old_dir <- getwd()
  on.exit(setwd(old_dir), add = TRUE)
  setwd(project_root)

  find_project_root <- function(start) {
      current <- normalizePath(start, winslash = "/", mustWork = TRUE)
      repeat {
          marker <- file.path(current, "data", "All", "AnalysisExpenditure.gdx")
          if (file.exists(marker))
              return(current)
          parent <- dirname(current)
          if (identical(parent, current))
              break
          current <- parent
      }
      NA
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
  library_candidates <- c(Sys.getenv("R_LIBS_USER"), file.path(project_root, "Rlib"), "C:/ENVI5809/R_library", "C:/Users/umeha/AppData/Local/R/win-library/4.5")
  library_candidates <- unique(library_candidates[nzchar(library_candidates) & dir.exists(library_candidates)])
  .libPaths(unique(c(library_candidates, .libPaths())))
  suppressPackageStartupMessages({
      library(dplyr)
      library(tidyr)
      library(ggplot2)
      library(scales)
      library(patchwork)
      library(gdxrrw)
  })
  gams_candidates <- c(Sys.getenv("GAMS_SYSDIR", unset = ""), "C:/GAMS/34", "C:/GAMS/win64/26.1")
  gams_candidates <- unique(gams_candidates[nzchar(gams_candidates) & dir.exists(gams_candidates)])
  if (!length(gams_candidates)) stop("A GAMS system directory was not found.")
  igdx(gams_candidates[[1]])
  gdx_file <- file.path(project_root, "data", "All", "AnalysisExpenditure.gdx")
  output_dir <- file.path(project_root, "output", "Figure", "analysis")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_file <- file.path(output_dir, "04b_poverty_inequality_welfare_detailed_expenditure_2020_2050.png")
  scenario_codes <- c(BaU = "SSP2_BaU_NoCC_No", Def = "SSP2_400C_2030CP_NoCC_No", Aid = "SSP2_400C_2030CP_15th_NoCC_No")
  scenario_levels <- names(scenario_codes)
  years <- c(2020, 2030, 2040, 2050)
  policy_years <- c(2030, 2040, 2050)
  decomposition_year <- 2050
  population_threshold <- "pop_8.30"
  providers <- c("XE25", "JPN", "TUR", "CHN", "USA", "XER", "XOC", "CAN", "XLM", "CIS")
  recipients <- c("XSA", "IND", "XNF", "XAF", "XSE", "BRA", "XME")
  r17_group_lookup <- bind_rows(tibble(Region_code = paste0("R17", providers), Region_group = "Developed / Provider"), tibble(Region_code = paste0("R17",
      recipients), Region_group = "Developing / Recipient"))
  region_group_levels <- c("Developed / Provider", "Developing / Recipient")
  scenario_colours <- c(BaU = "#252525", Def = "#B6573F", Aid = "#3F6C8E")
  scenario_shapes <- c(BaU = 16, Def = 1, Aid = 15)
  scenario_linetypes <- c(BaU = "solid", Def = "22", Aid = "solid")
  region_group_colours <- c(`Developed / Provider` = "#6B7D2A", `Developing / Recipient` = "#D6A72C")
  ink <- "#252525"
  check_grid <- function(data, expected, keys, context) {
      if (anyDuplicated(as.data.frame(data[keys]))) {
          stop("Duplicate records in ", context, ".")
      }
      missing <- anti_join(expected, data, by = keys)
      if (nrow(missing)) {
          stop("Missing ", nrow(missing), " records in ", context, "; first: ", paste(unlist(missing[1, ]), collapse = " / "))
      }
  }
  weighted_gini <- function(expenditure, population) {
      valid <- is.finite(expenditure) & expenditure >= 0 & is.finite(population) & population > 0
      expenditure <- expenditure[valid]
      population <- population[valid]
      if (!length(expenditure) || sum(expenditure * population) <= 0)
          return(NA)
      ordering <- order(expenditure)
      expenditure <- expenditure[ordering]
      population <- population[ordering]
      population_share <- population/sum(population)
      expenditure_share <- population * expenditure/sum(population * expenditure)
      cumulative_population <- c(0, cumsum(population_share))
      cumulative_expenditure <- c(0, cumsum(expenditure_share))
      1 - sum(diff(cumulative_population) * (head(cumulative_expenditure, -1) + tail(cumulative_expenditure, -1)))
  }
  poverty_all <- rgdx.param(gdx_file, "PoVExp") %>% transmute(Scenario_id = as.character(Ref), Region_code = as.character(R),
      Year = as.numeric(as.character(Y)), Threshold = as.character(TH), People = as.numeric(PoVExp)) %>% filter(Scenario_id %in%
      unname(scenario_codes), Year %in% years)
  poverty <- poverty_all %>% filter(Region_code %in% c("WLD", r17_group_lookup$Region_code), Threshold == "pop_3.00") %>% mutate(Scenario = names(scenario_codes)[match(Scenario_id,
      scenario_codes)])
  expected_poverty <- expand_grid(Scenario_id = unname(scenario_codes), Region_code = c("WLD", r17_group_lookup$Region_code),
      Year = years)
  check_grid(poverty, expected_poverty, c("Scenario_id", "Region_code", "Year"), "US$3.00 poverty data")
  if (any(!is.finite(poverty$People) | poverty$People < 0)) {
      stop("Poverty headcounts must be finite and non-negative.")
  }
  world_poverty <- poverty %>% filter(Region_code == "WLD") %>% transmute(Scenario = factor(Scenario, levels = scenario_levels),
      Year, Population_million = People/1e+06)
  r17_poverty <- poverty %>% filter(Region_code %in% r17_group_lookup$Region_code) %>% left_join(r17_group_lookup, by = "Region_code") %>%
      mutate(Region_group = factor(Region_group, levels = region_group_levels))
  r17_world_check <- r17_poverty %>% group_by(Scenario_id, Year) %>% summarise(R17_total = sum(People), .groups = "drop") %>%
      left_join(poverty %>% filter(Region_code == "WLD") %>% select(Scenario_id, Year, World_total = People), by = c("Scenario_id",
          "Year"))
  if (max(abs(r17_world_check$R17_total - r17_world_check$World_total)) > 1) {
      stop("R17 poverty totals do not reproduce the world poverty headcount.")
  }
  r17_baseline <- r17_poverty %>% filter(Scenario == "BaU") %>% select(Region_code, Year, BaU_people = People)
  r17_group_additional <- r17_poverty %>% left_join(r17_baseline, by = c("Region_code", "Year")) %>% mutate(Scenario = factor(Scenario,
      levels = c("Def", "Aid")), Year_factor = factor(Year, levels = policy_years)) %>% filter(!is.na(Scenario), Year %in%
      policy_years) %>% group_by(Scenario, Year, Year_factor, Region_group) %>% summarise(Additional_million = sum(People -
      BaU_people)/1e+06, .groups = "drop")
  budget <- rgdx.param(gdx_file, "BudgetInitial") %>% transmute(Region_code = as.character(R), Segment = as.character(Seg),
      Expenditure = as.numeric(BudgetInitial))
  frequency <- rgdx.param(gdx_file, "FreqSegall_exp") %>% transmute(Region_code = as.character(R), Scenario_id = as.character(Ref),
      Year = as.numeric(as.character(Y)), Segment = as.character(Seg), Frequency = as.numeric(FreqSegall_exp)) %>% filter(Scenario_id %in%
      unname(scenario_codes), Year %in% years)
  if (any(!is.finite(budget$Expenditure) | budget$Expenditure <= 0) || any(!is.finite(frequency$Frequency) | frequency$Frequency <
      0)) {
      stop("Detailed expenditure and frequency inputs must be finite and valid.")
  }
  budget_bins <- budget %>% count(Region_code, name = "Budget_bins")
  frequency_cells <- frequency %>% count(Region_code, Scenario_id, Year, name = "Frequency_bins") %>% left_join(budget_bins,
      by = "Region_code") %>% filter(Frequency_bins == Budget_bins)
  balanced_regions <- frequency_cells %>% count(Region_code, name = "Complete_cells") %>% filter(Complete_cells == length(scenario_codes) *
      length(years)) %>% pull(Region_code)
  if (!length(balanced_regions)) stop("No balanced country panel was found.")
  poverty_rate <- rgdx.param(gdx_file, "RoPExp") %>% transmute(Scenario_id = as.character(Ref), Region_code = as.character(R),
      Year = as.numeric(as.character(Y)), Threshold = as.character(TH), Poverty_rate = as.numeric(RoPExp)) %>% filter(Scenario_id %in%
      unname(scenario_codes), Region_code %in% balanced_regions, Year %in% years, Threshold == population_threshold)
  country_population <- poverty_all %>% filter(Region_code %in% balanced_regions, Threshold == population_threshold) %>% inner_join(poverty_rate,
      by = c("Scenario_id", "Region_code", "Year", "Threshold")) %>% transmute(Scenario_id, Region_code, Year, Population = People/Poverty_rate)
  check_grid(country_population, expand_grid(Scenario_id = unname(scenario_codes), Region_code = balanced_regions, Year = years),
      c("Scenario_id", "Region_code", "Year"), "country population used for detailed expenditure weights")
  if (any(!is.finite(country_population$Population) | country_population$Population <= 0)) {
      stop("Recovered country populations must be finite and positive.")
  }
  frequency_sums <- frequency %>% filter(Region_code %in% balanced_regions) %>% group_by(Scenario_id, Region_code, Year) %>%
      summarise(Frequency_sum = sum(Frequency), .groups = "drop")
  if (any(!is.finite(frequency_sums$Frequency_sum) | frequency_sums$Frequency_sum <= 0)) {
      stop("Each country/scenario/year must have a positive frequency sum.")
  }
  detailed_distribution <- frequency %>% filter(Region_code %in% balanced_regions) %>% left_join(frequency_sums, by = c("Scenario_id",
      "Region_code", "Year")) %>% mutate(Population_share = Frequency/Frequency_sum) %>% inner_join(budget, by = c("Region_code",
      "Segment")) %>% inner_join(country_population, by = c("Scenario_id", "Region_code", "Year")) %>% mutate(Population_weight = Population_share *
      Population)
  metrics <- detailed_distribution %>% group_by(Scenario_id, Year) %>% summarise(Mean_expenditure = weighted.mean(Expenditure,
      Population_weight), Gini = weighted_gini(Expenditure, Population_weight), Population = sum(Population_weight), .groups = "drop") %>%
      transmute(Scenario = factor(names(scenario_codes)[match(Scenario_id, scenario_codes)], levels = scenario_levels), Year,
          Population, Mean_expenditure, Mean_expenditure_thousand = Mean_expenditure/1000, Gini, Gini_percent = 100 * Gini,
          Gini_adjusted_welfare = Mean_expenditure * (1 - Gini)) %>% group_by(Year) %>% mutate(BaU_welfare = Gini_adjusted_welfare[Scenario ==
      "BaU"], Welfare_index = 100 * Gini_adjusted_welfare/BaU_welfare) %>% ungroup()
  check_grid(metrics, expand_grid(Scenario = factor(scenario_levels, levels = scenario_levels), Year = years), c("Scenario",
      "Year"), "detailed expenditure metrics")
  if (any(!is.finite(metrics$Mean_expenditure) | metrics$Mean_expenditure <= 0) || any(!is.finite(metrics$Gini) | metrics$Gini <
      0 | metrics$Gini > 1)) {
      stop("Detailed expenditure metrics are outside their valid ranges.")
  }
  bau_2050 <- metrics %>% filter(Year == decomposition_year, Scenario == "BaU")
  if (nrow(bau_2050) != 1L) stop("A unique 2050 BaU record is required.")
  decomposition <- metrics %>% filter(Year == decomposition_year, Scenario %in% c("Def", "Aid")) %>% mutate(Mean_ratio = Mean_expenditure/bau_2050$Mean_expenditure,
      Equality_ratio = (1 - Gini)/(1 - bau_2050$Gini), `Mean expenditure` = 100 * (Mean_ratio - 1) * (1 + Equality_ratio)/2,
      `Interpersonal distribution` = 100 * (Equality_ratio - 1) * (1 + Mean_ratio)/2, Net = `Mean expenditure` + `Interpersonal distribution`,
      Reported_net = Welfare_index - 100)
  decomposition_error <- max(abs(decomposition$Net - decomposition$Reported_net))
  if (!is.finite(decomposition_error) || decomposition_error > 1e-08) {
      stop("The exact decomposition does not reproduce the welfare index.")
  }
  decomposition_long <- decomposition %>% select(Scenario, `Mean expenditure`, `Interpersonal distribution`) %>% pivot_longer(c(`Mean expenditure`,
      `Interpersonal distribution`), names_to = "Effect", values_to = "Contribution") %>% mutate(Scenario = factor(as.character(Scenario),
      levels = c("Aid", "Def")), Effect = factor(Effect, levels = c("Mean expenditure", "Interpersonal distribution")))
  net_data <- decomposition %>% transmute(Scenario = factor(as.character(Scenario), levels = c("Aid", "Def")), Net, Label = sprintf("%+.1f",
      Net), Hjust = ifelse(Net >= 0, -0.18, 1.18))
  theme_nature <- function() {
      theme_classic(base_family = "Arial", base_size = 8) + theme(text = element_text(colour = ink), axis.title = element_text(size = 8),
          axis.text = element_text(size = 7.2, colour = ink), axis.ticks = element_line(colour = ink, linewidth = 0.24), axis.ticks.length = grid::unit(1.1,
              "mm"), axis.line = element_blank(), panel.border = element_rect(fill = NA, colour = ink, linewidth = 0.32), panel.grid = element_blank(),
          panel.background = element_rect(fill = "white", colour = NA), plot.background = element_rect(fill = "white", colour = NA),
          plot.title = element_blank(), plot.subtitle = element_blank(), plot.caption = element_blank(), plot.tag = element_text(face = "bold",
              size = 8.5, colour = ink), plot.tag.position = "topleft", legend.title = element_text(size = 7.2, face = "plain"),
          legend.text = element_text(size = 6.9), legend.key.height = grid::unit(3.3, "mm"), legend.key.width = grid::unit(5.5,
              "mm"), legend.spacing.x = grid::unit(0.8, "mm"), legend.margin = margin(1, 0, 0, 0, unit = "mm"), plot.margin = margin(4,
              5, 3, 4, unit = "mm"))
  }
  scenario_scales <- list(scale_colour_manual(values = scenario_colours, breaks = scenario_levels, drop = FALSE, guide = guide_legend(title = NULL,
      nrow = 1, order = 1)), scale_shape_manual(values = scenario_shapes, breaks = scenario_levels, drop = FALSE, guide = guide_legend(title = NULL,
      nrow = 1, order = 1)), scale_linetype_manual(values = scenario_linetypes, breaks = scenario_levels, drop = FALSE, guide = guide_legend(title = NULL,
      nrow = 1, order = 1)))
  trajectory_panel <- function(data, y, y_name, reference = NULL, legend = FALSE, y_accuracy = 0.1) {
      plot <- ggplot(data, aes(x = Year, y = .data[[y]], colour = Scenario, shape = Scenario, linetype = Scenario, group = Scenario))
      if (!is.null(reference)) {
          plot <- plot + geom_hline(yintercept = reference, linetype = "22", colour = "#8A8A8A", linewidth = 0.28)
      }
      plot + geom_line(linewidth = 0.62) + geom_point(size = 1.85, stroke = 0.52) + scenario_scales + scale_x_continuous(breaks = years,
          limits = c(2019, 2051), expand = expansion(mult = c(0, 0))) + scale_y_continuous(labels = label_number(accuracy = y_accuracy),
          expand = expansion(mult = c(0.1, 0.12))) + labs(x = NULL, y = y_name) + theme_nature() + theme(legend.position = if (legend)
          "bottom"
      else "none")
  }
  p_a <- trajectory_panel(world_poverty, "Population_million", "Population below US$3.00 per day (million)", legend = TRUE,
      y_accuracy = 1)
  p_b <- ggplot(r17_group_additional, aes(x = Year_factor, y = Additional_million, fill = Region_group)) + geom_hline(yintercept = 0,
      colour = ink, linewidth = 0.32) + geom_col(width = 0.7, colour = "white", linewidth = 0.16) + facet_wrap(~Scenario, nrow = 1,
      drop = FALSE) + scale_fill_manual(values = region_group_colours, breaks = region_group_levels, drop = FALSE, guide = guide_legend(title = NULL,
      nrow = 1, byrow = TRUE)) + scale_y_continuous(labels = label_number(accuracy = 1), expand = expansion(mult = c(0.08,
      0.12))) + labs(x = NULL, y = "Additional population relative to BaU (million)") + theme_nature() + theme(legend.position = "bottom",
      strip.background = element_blank(), strip.text = element_text(size = 7.4, face = "bold", margin = margin(b = 1.5)))
  p_c <- trajectory_panel(metrics, "Gini_percent", "Global interpersonal expenditure Gini (%)")
  p_d <- trajectory_panel(metrics, "Mean_expenditure_thousand", expression(atop("Mean annual expenditure", paste("(10"^3, " US$ person"^{
      -1
  }, " yr"^{
      -1
  }, ")"))))
  p_e <- trajectory_panel(metrics, "Welfare_index", "Welfare Index (BaU=100)", reference = 100)
  effect_colours <- c(`Mean expenditure` = "#6B5B95", `Interpersonal distribution` = "#2A9D8F")
  p_f <- ggplot(decomposition_long, aes(y = Scenario, x = Contribution, fill = Effect)) + geom_vline(xintercept = 0, colour = ink,
      linewidth = 0.32) + geom_col(width = 0.56, colour = "white", linewidth = 0.16) + geom_point(data = net_data, aes(y = Scenario,
      x = Net, shape = "Net change"), inherit.aes = FALSE, size = 2.25, stroke = 0.6, fill = "white", colour = ink) + geom_text(data = net_data,
      aes(y = Scenario, x = Net, label = Label, hjust = Hjust), inherit.aes = FALSE, family = "Arial", size = 2.1, colour = ink) +
      scale_fill_manual(values = effect_colours, drop = FALSE) + scale_shape_manual(values = c(`Net change` = 23)) + scale_x_continuous(labels = label_number(accuracy = 1),
      expand = expansion(mult = c(0.1, 0.14))) + scale_y_discrete(drop = FALSE) + labs(x = "Decomposition of WI change in 2050 relative to BaU (%)",
      y = NULL, fill = NULL, shape = NULL) + guides(fill = guide_legend(order = 1, nrow = 1), shape = guide_legend(order = 2,
      nrow = 1)) + theme_nature() + theme(legend.position = "bottom", legend.box = "horizontal", axis.text.y = element_text(face = "bold"))
  layout_design <- "\nAABBBB\nCCDDEE\nFFFFFF\n"
  figure <- (wrap_plots(A = p_a, B = p_b, C = p_c, D = p_d, E = p_e, F = p_f, design = layout_design, heights = c(1.05, 0.92,
      0.78)) + plot_annotation(tag_levels = "a")) & theme(plot.background = element_rect(fill = "white", colour = NA), plot.tag = element_text(family = "Arial",
      face = "bold", size = 8.5, colour = ink), plot.tag.position = "topleft")
  figure
}

generate_main_figures <- function(project_root, output_dirs) {
  save_figure_pair(build_main_figure_2(project_root), "Fig2_Emission_CP_revenue_flow_composite", 195, 232, output_dirs)
  prepare_transfer_benefit(project_root)
  prepare_zero_loss(project_root)
  save_figure_pair(build_main_figure_3(project_root), "Fig3_transfer_timing_recovery_zero_loss_gdp_four_panel", 180, 174, output_dirs)
  save_figure_pair(build_main_figure_4(project_root), "Fig4_poverty_inequality_welfare_detailed_expenditure_2020_2050", 180, 202, output_dirs)
  invisible(NULL)
}
