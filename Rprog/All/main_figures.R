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
  output_dir <- tempdir()
  csv_dir <- tempdir()
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
  annual_gdx_file <- "SSP2_400C_2030CP_15th_NoCC_No.gdx"
  region_map_file <- "RegionmapRagg.map"
  output_dir <- tempdir()
  scenario_aid <- "SSP2_400C_2030CP_15th_NoCC_No"
  start_year <- 2026
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
  if (!file.exists(gdx_file)) stop("Place global_17_IAMC.gdx in the working directory.")
  if (!file.exists(annual_gdx_file)) stop("Place SSP2_400C_2030CP_15th_NoCC_No.gdx in the working directory.")
  if (!file.exists(region_map_file)) stop("Place RegionmapRagg.map in the working directory.")
  iamc <- rgdx.param(gdx_file, "IAMC_template") %>% mutate(Year = as.numeric(as.character(YEMF)))
  annual_iamc <- rgdx.param(annual_gdx_file, "IAMC_template") %>% mutate(Year = as.numeric(as.character(Y)), Value = as.numeric(IAMC_template))
  provider_raw <- annual_iamc %>% filter(VEMF == "Rev_gov_Tax_Car_Tax", REMF %in% providers,
      Year >= start_year, Year <= end_year) %>% transmute(Year, REMF, Transfer = Value)
  model_years <- sort(unique(provider_raw$Year))
  provider_annual <- provider_raw %>% group_by(Year, REMF) %>% summarise(Transfer = sum(Transfer, na.rm = TRUE), .groups = "drop") %>%
      complete(Year = model_years, REMF = providers, fill = list(Transfer = 0))
  total_annual <- provider_annual %>% group_by(Year) %>% summarise(TotalTransfer = sum(Transfer, na.rm = TRUE), .groups = "drop")
  recipient_gdp <- annual_iamc %>% filter(VEMF == "GDP_PPP", REMF %in% recipients, Year >= start_year, Year <=
      end_year) %>% transmute(Year, REMF, GDP_PPP = Value) %>% group_by(Year, REMF) %>% summarise(GDP_PPP = sum(GDP_PPP,
      na.rm = TRUE), .groups = "drop") %>% complete(Year = model_years, REMF = recipients, fill = list(GDP_PPP = 0)) %>% group_by(Year) %>%
      mutate(TotalGDP_PPP = sum(GDP_PPP, na.rm = TRUE)) %>% ungroup()
  if (any(recipient_gdp$TotalGDP_PPP <= 0)) {
      stop("GDP_PPP is missing for all recipient regions in at least one model year.")
  }
  recipient_annual <- recipient_gdp %>% mutate(GDPShare = GDP_PPP/TotalGDP_PPP) %>% left_join(total_annual, by = "Year") %>%
      transmute(Year, REMF, Transfer = TotalTransfer * GDPShare)
  provider_cum <- provider_annual %>% group_by(REMF) %>% summarise(CumTransfer = sum(Transfer), .groups = "drop") %>%
      mutate(CumTrillion = CumTransfer/1e+06, Type = "Provider")
  recipient_cum <- recipient_annual %>% group_by(REMF) %>% summarise(CumTransfer = sum(Transfer), .groups = "drop") %>%
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
      annotate("text", x = fund_x, y = fund_y - map_width * 0.08, label = sprintf("%.1f trillion US$2010", pool_total), size = 6, color = ink) + annotate("text",
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


build_main_figure_3 <- function(project_root, artifact_dir = NULL) {
  region_lookup <- tibble::tibble(
    REMF = c(
      "XE25", "XER", "TUR", "XOC", "CHN", "JPN", "CAN", "USA", "XLM", "CIS",
      "IND", "BRA", "XSA", "XSE", "XME", "XNF", "XAF"
    ),
    Region = c(rep("Developed", 10), rep("Developing", 7))
  )
  model_regions <- region_lookup$REMF
  provider_regions <- region_lookup$REMF[region_lookup$Region == "Developed"]
  plot_years <- seq(2030, 2050, by = 5)
  ink <- "#252525"

  def_file <- file.path(project_root, "data", "All", "SSP2_400C_2030CP_NoCC_No.gdx")
  aid_file <- file.path(project_root, "data", "All", "SSP2_400C_2030CP_15th_NoCC_No.gdx")
  annual_years <- 2026:2050

  read_annual_gdx <- function(path, scenario) {
    message("Reading annual IAMC_template from: ", path)
    gdxrrw::rgdx.param(path, "IAMC_template") |>
      dplyr::transmute(
        REMF = as.character(REMF), Variable = as.character(VEMF),
        Year = as.integer(as.character(Y)), Value = as.numeric(IAMC_template),
        Scenario = scenario
      ) |>
      dplyr::filter(
        REMF %in% model_regions,
        Variable %in% c("CNS", "Rev_gov_Tax_Car_Tax"),
        Year %in% annual_years
      )
  }

  annual_raw <- dplyr::bind_rows(
    read_annual_gdx(def_file, "Def"),
    read_annual_gdx(aid_file, "Aid")
  )

  annual_grid_check <- annual_raw |>
    dplyr::count(Scenario, Variable, REMF, Year) |>
    dplyr::filter(n != 1L)
  if (nrow(annual_grid_check)) {
    stop("The annual SSP2 GDX data contain missing or duplicated region-variable-year records.")
  }

  annual_revenue_exact <- annual_raw |>
    dplyr::filter(
      Scenario == "Aid", Variable == "Rev_gov_Tax_Car_Tax", REMF %in% provider_regions
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(Revenue_million = sum(Value), .groups = "drop")

  annual_consumption_exact <- annual_raw |>
    dplyr::filter(Variable == "CNS") |>
    dplyr::left_join(region_lookup, by = "REMF") |>
    dplyr::group_by(Year, Region, Scenario) |>
    dplyr::summarise(Consumption_billion = sum(Value), .groups = "drop") |>
    tidyr::pivot_wider(names_from = Scenario, values_from = Consumption_billion) |>
    dplyr::transmute(
      Year, Region,
      Consumption_change_billion = Aid - Def
    )

  cumulative_exact <- tidyr::crossing(
    Year = annual_years, Region = c("Developed", "Developing")
  ) |>
    dplyr::left_join(annual_revenue_exact, by = "Year") |>
    dplyr::left_join(annual_consumption_exact, by = c("Year", "Region")) |>
    dplyr::mutate(
      Annual_revenue_trillion = Revenue_million / 1e6,
      Annual_revenue_trillion = dplyr::if_else(
        Region == "Developed", -Annual_revenue_trillion, Annual_revenue_trillion
      ),
      Annual_consumption_change_trillion = Consumption_change_billion / 1000
    ) |>
    dplyr::arrange(Region, Year) |>
    dplyr::group_by(Region) |>
    dplyr::mutate(
      Cumulative_carbon_revenue_trillion = cumsum(Annual_revenue_trillion),
      Cumulative_consumption_change_trillion = cumsum(Annual_consumption_change_trillion)
    ) |>
    dplyr::ungroup()

  if (nrow(cumulative_exact) != length(annual_years) * 2L ||
      any(!is.finite(cumulative_exact$Cumulative_carbon_revenue_trillion)) ||
      any(!is.finite(cumulative_exact$Cumulative_consumption_change_trillion))) {
    stop("The exact annual cumulative data are incomplete.")
  }

  cumulative_plot_wide <- cumulative_exact |>
    dplyr::filter(Year %in% plot_years) |>
    dplyr::mutate(Region = factor(Region, levels = c("Developed", "Developing"))) |>
    dplyr::select(
      Year, Region,
      `Carbon-tax revenue` = Cumulative_carbon_revenue_trillion,
      `Consumption change` = Cumulative_consumption_change_trillion
    )

  measure_colours <- c(`Carbon-tax revenue` = "#E69F00", `Consumption change` = "#0072B2")

  panel_theme <- function() {
    ggplot2::theme_classic(base_family = "Arial", base_size = 8) +
      ggplot2::theme(
        text = ggplot2::element_text(colour = ink),
        panel.background = ggplot2::element_rect(fill = "white", colour = NA),
        plot.background = ggplot2::element_rect(fill = "white", colour = NA),
        panel.grid = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(colour = ink, linewidth = 0.3),
        axis.ticks = ggplot2::element_line(colour = ink, linewidth = 0.25),
        axis.title = ggplot2::element_text(size = 8.2),
        axis.text = ggplot2::element_text(size = 7.4, colour = ink),
        strip.background = ggplot2::element_blank(),
        strip.text = ggplot2::element_text(size = 7, face = "plain"),
        legend.position = "bottom", legend.justification = "center",
        legend.text = ggplot2::element_text(size = 7),
        plot.margin = ggplot2::margin(4, 5, 3, 4)
      )
  }

  # Rebuild panels a, b and d by aggregating the mutually exclusive R17 regions.
  scenario_codes <- c(
    BaU = "SSP2_BaU_NoCC_No",
    Def = "SSP2_400C_2030CP_NoCC_No",
    Aid = "SSP2_400C_2030CP_15th_NoCC_No"
  )
  global_file <- file.path(project_root, "data", "All", "global_17_IAMC.gdx")
  message("Reading five-year panel data from: ", global_file)
  global_raw <- gdxrrw::rgdx.param(global_file, "IAMC_template") |>
    dplyr::transmute(
      Scenario_code = as.character(SCENARIO), REMF = as.character(REMF),
      Variable = as.character(VEMF), Year = as.numeric(as.character(YEMF)),
      Value = as.numeric(IAMC_Template)
    ) |>
    dplyr::filter(
      Scenario_code %in% scenario_codes,
      REMF %in% model_regions,
      Variable %in% c("CNS", "Trd_Goo_Val", "Rev_gov_Tax_Car_Tax"),
      Year %in% plot_years
    )

  consumption_regions <- global_raw |>
    dplyr::filter(Variable == "CNS") |>
    dplyr::left_join(region_lookup, by = "REMF") |>
    dplyr::group_by(Scenario_code, Region, Year) |>
    dplyr::summarise(Consumption_trillion = sum(Value) / 1000, .groups = "drop")

  bau_consumption <- consumption_regions |>
    dplyr::filter(Scenario_code == scenario_codes[["BaU"]]) |>
    dplyr::select(Region, Year, BaU_consumption_trillion = Consumption_trillion)

  consumption_change_data <- consumption_regions |>
    dplyr::filter(Scenario_code %in% scenario_codes[c("Def", "Aid")]) |>
    dplyr::left_join(bau_consumption, by = c("Region", "Year")) |>
    dplyr::transmute(
      Year = factor(Year, levels = plot_years),
      Region = factor(Region, levels = c("Developed", "Developing")),
      Policy = factor(names(scenario_codes)[match(Scenario_code, scenario_codes)], levels = c("Def", "Aid")),
      Consumption_change_trillion = Consumption_trillion - BaU_consumption_trillion
    )

  world_consumption_change <- consumption_change_data |>
    dplyr::group_by(Year, Policy) |>
    dplyr::summarise(World_net_change_trillion = sum(Consumption_change_trillion), .groups = "drop")
  region_colours <- c(Developed = "#3F6C8E", Developing = "#B6573F")

  p_a <- ggplot2::ggplot(consumption_change_data, ggplot2::aes(Year, Consumption_change_trillion, fill = Region)) +
    ggplot2::geom_hline(yintercept = 0, colour = ink, linewidth = 0.35) +
    ggplot2::geom_col(width = 0.68) +
    ggplot2::geom_line(
      data = world_consumption_change,
      ggplot2::aes(Year, World_net_change_trillion, group = 1, linetype = "World"),
      inherit.aes = FALSE, colour = ink, linewidth = 0.62
    ) +
    ggplot2::geom_point(
      data = world_consumption_change,
      ggplot2::aes(Year, World_net_change_trillion, shape = "World"),
      inherit.aes = FALSE, colour = ink, fill = "white", size = 2, stroke = 0.65
    ) +
    ggplot2::facet_grid(Policy ~ .) +
    ggplot2::scale_fill_manual(values = region_colours, name = NULL) +
    ggplot2::scale_linetype_manual(values = c(World = "solid"), name = NULL) +
    ggplot2::scale_shape_manual(values = c(World = 21), name = NULL) +
    ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 1), expand = ggplot2::expansion(mult = c(0.08, 0.1))) +
    ggplot2::labs(x = NULL, y = "Annual consumption change relative to BaU\n(trillion US$2010/yr)") +
    ggplot2::guides(
      fill = ggplot2::guide_legend(order = 1, nrow = 1),
      linetype = ggplot2::guide_legend(order = 2, nrow = 1),
      shape = ggplot2::guide_legend(order = 2, nrow = 1)
    ) + panel_theme() +
    ggplot2::theme(
      legend.box = "vertical", legend.direction = "horizontal",
      legend.text = ggplot2::element_text(size = 6.2),
      legend.title = ggplot2::element_text(size = 6.2),
      legend.key.width = grid::unit(3.5, "mm"),
      legend.key.height = grid::unit(2.7, "mm"),
      legend.spacing.y = grid::unit(0.3, "mm")
    )

  annual_transfer <- global_raw |>
    dplyr::filter(
      Variable == "Rev_gov_Tax_Car_Tax", Scenario_code == scenario_codes[["Aid"]],
      REMF %in% provider_regions
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(Annual_transfer_trillion = sum(Value) / 1e6, .groups = "drop")

  annual_consumption_change <- global_raw |>
    dplyr::filter(Variable == "CNS", Scenario_code %in% scenario_codes[c("Def", "Aid")]) |>
    dplyr::left_join(region_lookup, by = "REMF") |>
    dplyr::group_by(Year, Region, Scenario_code) |>
    dplyr::summarise(Consumption_billion = sum(Value), .groups = "drop") |>
    tidyr::pivot_wider(names_from = Scenario_code, values_from = Consumption_billion) |>
    dplyr::transmute(
      Year, Region,
      Consumption_change_trillion =
        (.data[[scenario_codes[["Aid"]]]] - .data[[scenario_codes[["Def"]]]]) / 1000
    )

  annual_two_region_data <- tidyr::crossing(Year = plot_years, Region = c("Developed", "Developing")) |>
    dplyr::left_join(annual_transfer, by = "Year") |>
    dplyr::left_join(annual_consumption_change, by = c("Year", "Region")) |>
    dplyr::mutate(
      Annual_transfer_trillion = dplyr::if_else(
        Region == "Developed", -Annual_transfer_trillion, Annual_transfer_trillion
      ),
      Year = factor(Year, levels = plot_years),
      Region = factor(Region, levels = c("Developed", "Developing"))
    )

  annual_lines <- annual_two_region_data |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      Slope = diff(Consumption_change_trillion) / diff(Annual_transfer_trillion),
      Intercept = dplyr::first(Consumption_change_trillion) - Slope * dplyr::first(Annual_transfer_trillion),
      .groups = "drop"
    ) |>
    dplyr::filter(as.character(Year) != "2030")

  annual_axis_step <- 2
  annual_axis_limit <- annual_axis_step * ceiling(max(abs(c(
    annual_two_region_data$Annual_transfer_trillion,
    annual_two_region_data$Consumption_change_trillion
  ))) / annual_axis_step)
  annual_axis_breaks <- seq(-annual_axis_limit, annual_axis_limit, by = annual_axis_step)
  year_colours <- c(`2030` = "#4E79A7", `2035` = "#59A14F", `2040` = "#F28E2B", `2045` = "#E15759", `2050` = "#B07AA1")

  p_b <- ggplot2::ggplot(annual_two_region_data, ggplot2::aes(Annual_transfer_trillion, Consumption_change_trillion)) +
    ggplot2::geom_hline(yintercept = 0, colour = ink, linewidth = 0.35) +
    ggplot2::geom_vline(xintercept = 0, colour = ink, linewidth = 0.35) +
    ggplot2::geom_abline(intercept = 0, slope = 1, colour = ink, linetype = "dotted", linewidth = 0.45) +
    ggplot2::geom_abline(
      data = annual_lines, ggplot2::aes(intercept = Intercept, slope = Slope, colour = Year),
      linewidth = 0.58, alpha = 0.42, show.legend = FALSE
    ) +
    ggplot2::geom_point(ggplot2::aes(colour = Year, shape = Region), size = 2.8, alpha = 0.92, stroke = 0.7) +
    ggplot2::scale_colour_manual(values = year_colours, limits = as.character(plot_years), name = "Year") +
    ggplot2::scale_shape_manual(values = c(Developed = 16, Developing = 17), name = NULL) +
    ggplot2::scale_x_continuous(breaks = annual_axis_breaks, labels = scales::label_number(accuracy = 1), expand = ggplot2::expansion(mult = 0)) +
    ggplot2::scale_y_continuous(breaks = annual_axis_breaks, labels = scales::label_number(accuracy = 1), expand = ggplot2::expansion(mult = 0)) +
    ggplot2::coord_fixed(
      ratio = 1, xlim = c(-annual_axis_limit, annual_axis_limit),
      ylim = c(-annual_axis_limit, annual_axis_limit), expand = FALSE
    ) +
    ggplot2::labs(
      x = "Annual transfer (trillion US$2010/yr)",
      y = "Consumption expenditure change under\nAid relative to Def (trillion US$2010/yr)"
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(order = 1, nrow = 1, override.aes = list(size = 2.3, shape = 16, linetype = 0)),
      shape = ggplot2::guide_legend(order = 2, nrow = 1)
    ) + panel_theme() +
    ggplot2::theme(
      legend.box = "vertical", legend.direction = "horizontal",
      legend.text = ggplot2::element_text(size = 6.2),
      legend.key.width = grid::unit(4, "mm"),
      legend.key.height = grid::unit(2.7, "mm"),
      legend.spacing.y = grid::unit(0.3, "mm")
    )

  economic_change_data <- global_raw |>
    dplyr::filter(
      Variable %in% c("CNS", "Trd_Goo_Val"),
      Scenario_code %in% scenario_codes[c("Def", "Aid")]
    ) |>
    dplyr::left_join(region_lookup, by = "REMF") |>
    dplyr::group_by(Year, Region, Variable, Scenario_code) |>
    dplyr::summarise(Value_billion = sum(Value), .groups = "drop") |>
    tidyr::pivot_wider(names_from = Scenario_code, values_from = Value_billion) |>
    dplyr::transmute(
      Year, Region,
      Component = dplyr::recode(Variable, CNS = "Consumption", Trd_Goo_Val = "Net exports"),
      Change_trillion = (.data[[scenario_codes[["Aid"]]]] - .data[[scenario_codes[["Def"]]]]) / 1000
    )

  transfer_change_data <- tidyr::crossing(Year = plot_years, Region = c("Developed", "Developing")) |>
    dplyr::left_join(annual_transfer, by = "Year") |>
    dplyr::transmute(
      Year, Region, Component = "Transferred funds",
      Change_trillion = dplyr::if_else(Region == "Developed", -Annual_transfer_trillion, Annual_transfer_trillion)
    )

  d_data <- dplyr::bind_rows(economic_change_data, transfer_change_data) |>
    dplyr::mutate(
      Year = factor(Year, levels = plot_years),
      Region = factor(Region, levels = c("Developed", "Developing")),
      Component = factor(Component, levels = c("Consumption", "Net exports", "Transferred funds"))
    )
  d_totals <- d_data |>
    dplyr::group_by(Year, Region) |>
    dplyr::summarise(Total_change_trillion = sum(Change_trillion), .groups = "drop")

  p_d <- ggplot2::ggplot(d_data, ggplot2::aes(Year, Change_trillion, fill = Component)) +
    ggplot2::geom_hline(yintercept = 0, colour = ink, linewidth = 0.35) +
    ggplot2::geom_col(width = 0.68, colour = "white", linewidth = 0.15) +
    ggplot2::geom_point(
      data = d_totals, ggplot2::aes(Year, Total_change_trillion, shape = "Sum of components"),
      inherit.aes = FALSE, size = 1.9, stroke = 0.55, fill = "white", colour = ink
    ) +
    ggplot2::facet_grid(
      . ~ Region,
      labeller = ggplot2::as_labeller(c(Developed = "Developed regions", Developing = "Developing regions"))
    ) +
    ggplot2::scale_fill_manual(
      values = c(Consumption = "#0072B2", `Net exports` = "#009E73", `Transferred funds` = "#E69F00"), name = NULL
    ) +
    ggplot2::scale_shape_manual(values = c(`Sum of components` = 23), name = NULL) +
    ggplot2::scale_x_discrete(breaks = c("2030", "2040", "2050")) +
    ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 1), expand = ggplot2::expansion(mult = c(0.12, 0.14))) +
    ggplot2::labs(x = NULL, y = "Annual change under Aid relative to Def\n(trillion US$2010/yr)") +
    ggplot2::guides(
      fill = ggplot2::guide_legend(order = 1, nrow = 2, byrow = TRUE),
      shape = ggplot2::guide_legend(order = 2, nrow = 1)
    ) + panel_theme() +
    ggplot2::theme(
      legend.box = "vertical", legend.direction = "horizontal",
      legend.text = ggplot2::element_text(size = 6.1),
      legend.key.width = grid::unit(3.8, "mm"),
      legend.key.height = grid::unit(2.6, "mm"),
      legend.spacing.y = grid::unit(0.25, "mm")
    )

  c_bar_data <- cumulative_plot_wide |>
    dplyr::filter(Year == 2050) |>
    tidyr::pivot_longer(
      cols = c("Carbon-tax revenue", "Consumption change"),
      names_to = "Measure", values_to = "Value_trillion"
    ) |>
    dplyr::mutate(
      Region = factor(Region, levels = c("Developed", "Developing")),
      Measure = factor(Measure, levels = c("Carbon-tax revenue", "Consumption change")),
      Label = sprintf("%+.1f", Value_trillion)
    )

  p_c_bar <- ggplot2::ggplot(c_bar_data, ggplot2::aes(Region, Value_trillion, fill = Measure)) +
    ggplot2::geom_hline(yintercept = 0, colour = ink, linewidth = 0.35) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.78), width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = Label, group = Measure),
      position = ggplot2::position_dodge(width = 0.78),
      vjust = dplyr::if_else(c_bar_data$Value_trillion >= 0, -0.45, 1.35),
      family = "Arial", size = 2.35, colour = ink
    ) +
    ggplot2::scale_fill_manual(values = measure_colours, name = NULL) +
    ggplot2::scale_x_discrete(labels = c(Developed = "Developed regions", Developing = "Developing regions")) +
    ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 1), expand = ggplot2::expansion(mult = c(0.12, 0.14))) +
    ggplot2::labs(
      x = NULL,
      y = "Cumulative amount, 2026-2050\n(trillion US$2010; undiscounted)"
    ) + panel_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(face = "bold"))

  figure_composite <- (
    patchwork::wrap_plots(
      A = p_a, B = p_b, C = p_c_bar, D = p_d,
      design = "AAAAABBBBB\nCCCCCDDDDD", heights = c(1, 1), guides = "keep"
    ) + patchwork::plot_annotation(tag_levels = "a")
  ) & ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "white", colour = NA),
    plot.tag = ggplot2::element_text(family = "Arial", face = "bold", size = 8.5, colour = ink)
  )
  figure_composite
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
  save_figure_pair(build_main_figure_3(project_root), "Fig3_annual_exact_composite", 180, 174, output_dirs)
  save_figure_pair(build_main_figure_4(project_root), "Fig4_poverty_inequality_welfare_detailed_expenditure_2020_2050", 180, 202, output_dirs)
  invisible(NULL)
}
