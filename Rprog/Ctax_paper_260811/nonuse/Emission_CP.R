# AR6 ranges and AIM emissions/price pathways: individual plots and 2x2 figure
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

make_region_panel <- function(region, title) {
  ggplot(filter(aim_emissions, REMF == region, Scenario %in% c("Def", "Aid")),
         aes(Year, value, color = Scenario, group = Scenario)) +
    geom_hline(yintercept = 0, linewidth = 0.3, color = "grey50") +
    geom_line(linewidth = 1.15) + geom_point(size = 2.5) +
    scale_color_manual(name = "AIM scenario", values = scenario_colors,
                       limits = c("Def", "Aid"), drop = FALSE) + x_scale() +
    labs(title = title, x = "Year", y = expression(CO[2]~emissions~"(GtCO"[2]*"/yr)")) +
    guides(color = guide_legend(order = 2, override.aes = list(linewidth = 1.2, size = 2.5))) + theme_panel()
}

# Four plots -----------------------------------------------------------------
p_global <- make_ar6_panel("Emissions|CO2", filter(aim_emissions, REMF == "World"),
                           "Global CO2 emissions", expression(CO[2]~emissions~"(GtCO"[2]*"/yr)"))
p_price <- make_ar6_panel("Price|Carbon", aim_price, "Global carbon price",
                          expression("Carbon price (USD/tCO"[2]*")"))
p_provider <- make_region_panel("Rprovider15th", "Provider-region CO2 emissions")
p_recipient <- make_region_panel("Rrecipient15th", "Recipient-region CO2 emissions")

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
p_provider_combined <- p_provider + guides(color = "none")
p_recipient_combined <- p_recipient + guides(color = "none")

figure_2x2 <- ((p_global | p_price_combined) / (p_provider_combined | p_recipient_combined)) +
  plot_layout(guides = "collect") + plot_annotation(tag_levels = "a")
figure_2x2 <- figure_2x2 &
  theme(legend.position = "right", legend.box = "vertical", legend.box.just = "left",
        plot.tag = element_text(face = "plain", size = 16))

plot(figure_2x2)
save_figure(figure_2x2, "Emission_CP", 16, 10)