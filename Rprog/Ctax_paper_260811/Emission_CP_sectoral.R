# AR6 ranges and AIM emissions/price pathways: sectoral panels c-d variant
library(tidyverse)
library(readxl)
library(gdxrrw)
library(patchwork)
library(scales)

gdx_file <- "global_17_IAMC.gdx"
ar6_meta_file <- "AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx"
ar6_world_file <- "AR6_Scenarios_Database_World_v1.1.csv"
output_dir <- file.path("../..", "output/Figure")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

scenario_names <- c("SSP2_BaU_NoCC_No" = "BaU", "SSP2_400C_2030CP_NoCC_No" = "Def", "SSP2_400C_2030CP_15th_NoCC_No" = "Aid")
scenario_colors <- c("BaU" = "#6E6E6E", "Def" = "#2E6F9E", "Aid" = "#D27755")
ar6_colors <- c("C1" = "#4E79A7", "C2" = "#F28E2B")
year_breaks <- seq(2020, 2050, 5)

vec_emissions <- c("Emi_CO2_AFO", "Emi_CO2_Ene_Sup", "Emi_CO2_Ene_Dem",
                   "Emi_CO2_Ind_Pro", "Emi_CO2_Pro_Use", "Emi_CO2_Cap_and_Rem")
CO2Order <- c("Energy Supply", "Energy Demand", "Industrial Processes",
              "Product Use", "AFOLU", "Capture and removal")
col_emissions <- c("Energy Supply" = "#4E9F85", "Energy Demand" = "#4E79A7",
                   "Industrial Processes" = "#8E6C8A", "Product Use" = "#E3B448",
                   "AFOLU" = "#E58E65", "Capture and removal" = "#3E6B89")
CLP <- c("SSP2_400C_2030CP_NoCC_No", "SSP2_400C_2030CP_15th_NoCC_No")
Region <- c("Rprovider15th", "Rrecipient15th")

required_files <- c(gdx_file, ar6_meta_file, ar6_world_file)
if (any(!file.exists(required_files))) stop("Missing file(s): ", paste(required_files[!file.exists(required_files)], collapse = ", "))

# AR6 data: read and reshape only once ---------------------------------------
ar6_category <- read_xlsx(ar6_meta_file, sheet = "meta_Ch3vetted_withclimate") %>%
  transmute(Case = paste0(Model, "_", Scenario), Category) %>%
  filter(Category %in% c("C1", "C2")) %>% distinct(Case, .keep_all = TRUE)

ar6_long <- read_csv(ar6_world_file, show_col_types = FALSE) %>%
  mutate(Case = paste0(Model, "_", Scenario)) %>%
  inner_join(ar6_category, by = "Case", relationship = "many-to-one") %>%
  filter(Region == "World", Variable %in% c("Emissions|CO2", "Price|Carbon")) %>%
  pivot_longer(cols = matches("^[0-9]{4}$"), names_to = "Year", values_to = "value", names_transform = list(Year = as.numeric)) %>%
  filter(!is.na(value), Year %in% year_breaks) %>%
  mutate(value = if_else(Variable == "Emissions|CO2", value/1000, value))

ar6_box <- ar6_long %>% group_by(Variable, Category, Year) %>%
  summarise(p05 = quantile(value, 0.05, na.rm = TRUE), p25 = quantile(value, 0.25, na.rm = TRUE),
            p50 = median(value, na.rm = TRUE), p75 = quantile(value, 0.75, na.rm = TRUE),
            p95 = quantile(value, 0.95, na.rm = TRUE), .groups = "drop") %>%
  mutate(PlotYear = Year + if_else(Category == "C1", -0.7, 0.7))

# AIM data -------------------------------------------------------------------
iamc <- rgdx.param(gdx_file, "IAMC_template") %>%
  mutate(Year = as.numeric(as.character(YEMF)), Value = as.numeric(IAMC_Template),
         Scenario = recode(SCENARIO, !!!scenario_names),
         Scenario = factor(Scenario, levels = c("BaU", "Def", "Aid")))

aim_emissions <- iamc %>%
  filter(VEMF == "Emi_CO2", SCENARIO %in% names(scenario_names),
         REMF %in% c("World", "Rprovider15th", "Rrecipient15th"), Year >= 2020, Year <= 2050) %>%
  transmute(Year, REMF, Scenario, value = Value/1000)

aim_price <- iamc %>%
  filter(VEMF == "Prc_Car", SCENARIO %in% names(scenario_names), REMF == "World", Year >= 2020, Year <= 2050) %>%
  transmute(Year, Scenario, value = Value)

# Sectoral emissions for panels c and d. Original unit: MtCO2/yr;
# converted here to GtCO2/yr.
df_emissions <- rgdx.param(gdx_file, "IAMC_template") %>%
  filter(VEMF %in% vec_emissions, SCENARIO %in% CLP, REMF %in% Region) %>%
  transmute(
    Year = as.numeric(as.character(YEMF)),
    REMF = recode(REMF,
                  "Rprovider15th" = "Provider",
                  "Rrecipient15th" = "Recipient"),
    SCENARIO = recode(SCENARIO,
                      "SSP2_400C_2030CP_NoCC_No" = "Def",
                      "SSP2_400C_2030CP_15th_NoCC_No" = "Aid"),
    VEMF = recode(VEMF,
                  "Emi_CO2_AFO" = "AFOLU",
                  "Emi_CO2_Ene_Sup" = "Energy Supply",
                  "Emi_CO2_Ene_Dem" = "Energy Demand",
                  "Emi_CO2_Ind_Pro" = "Industrial Processes",
                  "Emi_CO2_Pro_Use" = "Product Use",
                  "Emi_CO2_Cap_and_Rem" = "Capture and removal"),
    value = as.numeric(IAMC_Template) / 1000
  ) %>%
  filter(Year >= 2020, Year <= 2050) %>%
  group_by(REMF, SCENARIO, Year, VEMF) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    REMF = factor(REMF, levels = c("Provider", "Recipient")),
    SCENARIO = factor(SCENARIO, levels = c("Def", "Aid")),
    VEMF = factor(VEMF, levels = CO2Order)
  ) %>%
  complete(REMF, SCENARIO, Year, VEMF, fill = list(value = 0))

scenario_scale <- function() scale_color_manual(name = "scenario", values = scenario_colors,
                                                 limits = c("BaU", "Def", "Aid"), drop = FALSE)
ar6_scale <- function() scale_fill_manual(name = "AR6 category", values = ar6_colors,
                                          limits = c("C1", "C2"), drop = FALSE)
x_scale <- function() scale_x_continuous(limits = c(2018.5, 2051.5), breaks = year_breaks, labels = year_breaks)
theme_panel <- function() theme_bw(base_size = 16) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(color = "#E4E4E4", linewidth = 0.3),
        axis.text.x = element_text(angle = 0, hjust = 0.5), axis.title = element_text(face = "plain"),
        plot.title = element_text(face = "plain", size = 16), legend.position = "right",
        legend.box = "vertical", legend.box.just = "left", legend.title = element_text(face = "plain"))

make_ar6_panel <- function(variable, aim_data, title, y_label) {
  ggplot() + geom_hline(yintercept = 0, linewidth = 0.3, color = "grey50") +
    geom_boxplot(data = filter(ar6_box, Variable == variable),
                 aes(x = PlotYear, ymin = p05, lower = p25, middle = p50, upper = p75, ymax = p95,
                     fill = Category, group = interaction(Year, Category)),
                 stat = "identity", width = 1.15, alpha = 0.58, color = "grey25", linewidth = 0.45) +
    geom_line(data = aim_data, aes(Year, value, color = Scenario, group = Scenario), linewidth = 1.15) +
    geom_point(data = aim_data, aes(Year, value, color = Scenario), size = 2.5) +
    ar6_scale() + scenario_scale() + x_scale() + labs(title = title, x = NULL, y = y_label) +
    guides(fill = guide_legend(order = 1), color = guide_legend(order = 2,
           override.aes = list(linewidth = 1.2, size = 2.5))) + theme_panel()
}

make_sector_panel <- function(region, title) {
  # Five-year paired bars make the Def-Aid difference visible without packing
  # 31 pairs of annual bars into each lower panel.
  panel_data <- df_emissions %>%
    filter(REMF == region, Year %in% year_breaks) %>%
    mutate(PlotYear = Year + if_else(SCENARIO == "Def", -0.75, 0.75))

  scenario_labels <- panel_data %>%
    group_by(Year, PlotYear, SCENARIO) %>%
    summarise(
      PositiveTotal = sum(if_else(value > 0, value, 0), na.rm = TRUE),
      NegativeTotal = sum(if_else(value < 0, value, 0), na.rm = TRUE),
      .groups = "drop"
    )

  panel_span <- with(
    scenario_labels,
    max(PositiveTotal, na.rm = TRUE) - min(NegativeTotal, na.rm = TRUE)
  )
  scenario_labels <- scenario_labels %>%
    mutate(LabelY = PositiveTotal + panel_span * 0.025)

  ggplot(panel_data,
         aes(PlotYear, value, fill = VEMF, group = VEMF)) +
    geom_col(position = position_stack(reverse = TRUE),
             width = 1.3, color = "white", linewidth = 0.18) +
    geom_hline(yintercept = 0, linewidth = 0.35, color = "grey35") +
    geom_text(data = scenario_labels,
              aes(x = PlotYear, y = LabelY, label = SCENARIO, color = SCENARIO),
              inherit.aes = FALSE, fontface = "bold", size = 3.2,
              show.legend = FALSE) +
    scale_fill_manual(name = "Emission sector", values = col_emissions,
                      limits = CO2Order, breaks = CO2Order, drop = FALSE) +
    scale_color_manual(values = scenario_colors[c("Def", "Aid")], guide = "none") +
    x_scale() +
    scale_y_continuous(expand = expansion(mult = c(0.07, 0.14))) +
    labs(title = title, x = "Year", y = expression(CO[2]~emissions~"(GtCO"[2]*"/yr)")) +
    guides(fill = guide_legend(order = 3)) +
    theme_panel()
}

# Four plots -----------------------------------------------------------------
p_global <- make_ar6_panel("Emissions|CO2", filter(aim_emissions, REMF == "World"),
                           "Global CO2 emissions", expression(CO[2]~emissions~"(GtCO"[2]*"/yr)"))
p_price <- make_ar6_panel("Price|Carbon", aim_price, "Global carbon price",
                          expression("Carbon price (USD/tCO"[2]*")"))
p_provider <- make_sector_panel("Provider", "Provider-region CO2 emissions")
p_recipient <- make_sector_panel("Recipient", "Recipient-region CO2 emissions")

# Individual outputs ---------------------------------------------------------
save_figure <- function(plot_object, name, width, height) {
  ggsave(file.path(output_dir, paste0(name, ".png")), plot_object, width = width, height = height, dpi = 600, bg = "white")
  if (requireNamespace("svglite", quietly = TRUE))
    ggsave(file.path(output_dir, paste0(name, ".svg")), plot_object, device = svglite::svglite,
           width = width, height = height, bg = "white")
}

#save_figure(p_global, "AR6_global_CO2", 9, 6)
#save_figure(p_price, "AR6_global_carbon_price", 9, 6)
#save_figure(p_provider, "Provider_CO2_pathways", 9, 6)
#save_figure(p_recipient, "Recipient_CO2_pathways", 9, 6)

# Combined 2x2 output: one set of legends at the far right ------------------
# Keep individual legends above, but use only panel a's legends in the combined figure.
p_price_combined <- p_price + guides(fill = "none", color = "none")
p_provider_combined <- p_provider
p_recipient_combined <- p_recipient + guides(fill = "none")

figure_2x2 <- ((p_global | p_price_combined) / (p_provider_combined | p_recipient_combined)) +
  plot_layout(guides = "collect") + plot_annotation(tag_levels = "a")
figure_2x2 <- figure_2x2 &
  theme(legend.position = "right", legend.box = "vertical", legend.box.just = "left",
        plot.tag = element_text(face = "plain", size = 16))

plot(figure_2x2)
save_figure(figure_2x2, "Emission_CP_sectoral", 16, 10)
