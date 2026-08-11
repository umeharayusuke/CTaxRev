# Energy-system and CDR responses to international carbon-revenue transfers
# Required input: global_17_IAMC.gdx in the working directory

library(tidyverse)
library(ggplot2)
library(gdxrrw)
library(patchwork)
library(scales)

gdx_file <- "global_17_IAMC.gdx"
output_dir <- "decisive_figures_output"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

scenario_def <- "SSP2_400C_2030CP_NoCC_No"
scenario_aid <- "SSP2_400C_2030CP_15th_NoCC_No"
regions <- c("Rprovider15th", "Rrecipient15th")
plot_years <- c(2030, 2040, 2050)

if (!file.exists(gdx_file)) stop("Place global_17_IAMC.gdx in the working directory.")

iamc <- rgdx.param(gdx_file, "IAMC_template") %>%
  transmute(VEMF, SCENARIO, REMF, Year = as.numeric(as.character(YEMF)), Value = as.numeric(IAMC_Template))

region_labels <- c("Rprovider15th" = "Provider", "Rrecipient15th" = "Recipient")
scenario_labels <- c("SSP2_400C_2030CP_NoCC_No" = "Def", "SSP2_400C_2030CP_15th_NoCC_No" = "Aid")

theme_energy <- theme_bw(base_size = 11) +
  theme(panel.grid = element_blank(), strip.background = element_blank(), strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 12), axis.title.x = element_blank(),
        legend.position = "bottom", legend.title = element_blank(), legend.key.size = grid::unit(0.55, "cm"),
        plot.caption = element_text(color = "grey40", hjust = 0))

final_map <- c("Fin_Ene_SolidsCoa" = "Fossil fuels", "Fin_Ene_Gas" = "Fossil fuels",
               "Fin_Ene_Liq_Oil" = "Fossil fuels", "Fin_Ene_Ele" = "Electricity",
               "Fin_Ene_Liq_Bio" = "Bioenergy", "Fin_Ene_SolidsBio" = "Bioenergy",
               "Fin_Ene_Heat" = "Heat and hydrogen", "Fin_Ene_Hyd" = "Heat and hydrogen")

power_map <- c("Sec_Ene_Ele_Coa_wo_CCS" = "Fossil w/o CCS", "Sec_Ene_Ele_Gas_wo_CCS" = "Fossil w/o CCS",
               "Sec_Ene_Ele_Oil_wo_CCS" = "Fossil w/o CCS", "Sec_Ene_Ele_Coa_w_CCS" = "Fossil w/ CCS",
               "Sec_Ene_Ele_Gas_w_CCS" = "Fossil w/ CCS", "Sec_Ene_Ele_Oil_w_CCS" = "Fossil w/ CCS",
               "Sec_Ene_Ele_Hyd" = "Wind, solar and hydro", "Sec_Ene_Ele_Solar" = "Wind, solar and hydro",
               "Sec_Ene_Ele_Win" = "Wind, solar and hydro", "Sec_Ene_Ele_Nuc" = "Nuclear",
               "Sec_Ene_Ele_Bio_wo_CCS" = "Biomass w/o CCS", "Sec_Ene_Ele_Bio_w_CCS" = "Biomass w/ CCS")

primary_map <- c("Prm_Ene_Coa_wo_CCS" = "Fossil w/o CCS", "Prm_Ene_Gas_wo_CCS" = "Fossil w/o CCS",
                 "Prm_Ene_Oil_wo_CCS" = "Fossil w/o CCS", "Prm_Ene_Coa_w_CCS" = "Fossil w/ CCS",
                 "Prm_Ene_Gas_w_CCS" = "Fossil w/ CCS", "Prm_Ene_Oil_w_CCS" = "Fossil w/ CCS",
                 "Prm_Ene_Hyd" = "Wind, solar and hydro", "Prm_Ene_Solar" = "Wind, solar and hydro",
                 "Prm_Ene_Win" = "Wind, solar and hydro", "Prm_Ene_Nuc" = "Nuclear",
                 "Prm_Ene_Bio_wo_CCS" = "Biomass w/o CCS", "Prm_Ene_Bio_w_CCS" = "Biomass w/ CCS")

cdr_map <- c("Car_Seq_Lan_Use" = "Land-use removal", "Car_Seq_CCS_Bio" = "BECCS",
             "Car_Seq_Dir_Air_Cap" = "DACCS", "Car_Seq_Enh_Wea" = "Enhanced weathering")

final_colors <- c("Fossil fuels" = "#6C757D", "Electricity" = "#4E79A7",
                  "Bioenergy" = "#59A14F", "Heat and hydrogen" = "#F28E2B")
technology_colors <- c("Fossil w/o CCS" = "#6C757D", "Fossil w/ CCS" = "#AAB2B9",
                       "Wind, solar and hydro" = "#4E9F85", "Nuclear" = "#E3B448",
                       "Biomass w/o CCS" = "#8FBF70", "Biomass w/ CCS" = "#3E7C59")
cdr_colors <- c("Land-use removal" = "#9ACD8C", "BECCS" = "#3E7C59",
                "DACCS" = "#7E6AA2", "Enhanced weathering" = "#C49A6C")

make_relative_change <- function(component_map) {
  iamc %>%
    filter(VEMF %in% names(component_map), SCENARIO %in% c(scenario_def, scenario_aid),
           REMF %in% regions, Year %in% plot_years) %>%
    mutate(Component = recode(VEMF, !!!component_map), Scenario = recode(SCENARIO, !!!scenario_labels),
           Region = recode(REMF, !!!region_labels)) %>%
    group_by(Region, Year, Component, Scenario) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = Scenario, values_from = Value) %>%
    filter(is.finite(Def), is.finite(Aid)) %>%
    group_by(Region, Year) %>%
    mutate(DefTotal = sum(Def, na.rm = TRUE), Contribution = 100 * (Aid - Def) / DefTotal,
           TotalChange = 100 * (sum(Aid, na.rm = TRUE) - DefTotal) / DefTotal) %>%
    ungroup() %>%
    filter(is.finite(Contribution), is.finite(TotalChange)) %>%
    mutate(Component = factor(Component, levels = unique(unname(component_map))),
           Region = factor(Region, levels = c("Provider", "Recipient")),
           Year = factor(Year, levels = plot_years))
}

make_change_plot <- function(data, title, palette) {
  totals <- data %>% distinct(Region, Year, TotalChange)
  ggplot(data, aes(Year, Contribution, fill = Component)) +
    geom_hline(yintercept = 0, color = "grey35", linewidth = 0.4) +
    geom_col(width = 0.78, color = "white", linewidth = 0.2) +
    geom_point(data = totals, aes(Year, TotalChange), inherit.aes = FALSE,
               shape = 23, size = 3, fill = "black", color = "white", stroke = 0.45) +
    facet_wrap(~Region, nrow = 1) +
    scale_fill_manual(values = palette, breaks = names(palette), drop = FALSE,
                      guide = guide_legend(nrow = 2, byrow = TRUE)) +
    scale_y_continuous(labels = label_number(accuracy = 0.1, suffix = "%"),
                       expand = expansion(mult = c(0.08, 0.08))) +
    labs(title = title, y = "Contribution to change\n(% of Def total)") +
    theme_energy
}

final_change <- make_relative_change(final_map)
power_change <- make_relative_change(power_map)
primary_change <- make_relative_change(primary_map)

p_final <- make_change_plot(final_change, "a  Final energy", final_colors)
p_power <- make_change_plot(power_change, "b  Power generation", technology_colors)
p_primary <- make_change_plot(primary_change, "c  Primary energy", technology_colors)

cdr_change <- iamc %>%
  filter(VEMF %in% names(cdr_map), SCENARIO %in% c(scenario_def, scenario_aid),
         REMF %in% regions, Year %in% plot_years) %>%
  mutate(Component = recode(VEMF, !!!cdr_map), Scenario = recode(SCENARIO, !!!scenario_labels),
         Region = recode(REMF, !!!region_labels)) %>%
  group_by(Region, Year, Component, Scenario) %>%
  summarise(Value = sum(Value, na.rm = TRUE) / 1000, .groups = "drop") %>%
  pivot_wider(names_from = Scenario, values_from = Value) %>%
  filter(is.finite(Def), is.finite(Aid)) %>%
  mutate(Change = Aid - Def) %>%
  group_by(Region, Year) %>%
  mutate(TotalChange = sum(Change, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Component = factor(Component, levels = unique(unname(cdr_map))),
         Region = factor(Region, levels = c("Provider", "Recipient")),
         Year = factor(Year, levels = plot_years))

cdr_totals <- cdr_change %>% distinct(Region, Year, TotalChange)
p_cdr <- ggplot(cdr_change, aes(Year, Change, fill = Component)) +
  geom_hline(yintercept = 0, color = "grey35", linewidth = 0.4) +
  geom_col(width = 0.78, color = "white", linewidth = 0.2) +
  geom_point(data = cdr_totals, aes(Year, TotalChange), inherit.aes = FALSE,
             shape = 23, size = 3, fill = "black", color = "white", stroke = 0.45) +
  facet_wrap(~Region, nrow = 1) +
  scale_fill_manual(values = cdr_colors, breaks = names(cdr_colors), drop = FALSE,
                    guide = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_y_continuous(labels = label_number(accuracy = 0.1), expand = expansion(mult = c(0.08, 0.08))) +
  labs(title = "d  Carbon dioxide removal", y = expression(Delta~CDR~"(GtCO"[2]*"/yr)")) +
  theme_energy

figure_energy_cdr <- (p_final | p_power) / (p_primary | p_cdr) +
  plot_annotation(title = "Energy-system responses to international carbon-revenue transfers",
                  subtitle = "Difference between Aid and Def scenarios",
                  caption = "Bars show component-level changes; black diamonds show total changes. Panels a–c are normalized by the corresponding Def total; panel d reports the absolute CDR change.")

plot(figure_energy_cdr)
ggsave(file.path(output_dir, "energy_and_cdr_overview_2x2.png"), figure_energy_cdr, width = 15, height = 11, dpi = 600)
ggsave(file.path(output_dir, "energy_and_cdr_overview_2x2.pdf"), figure_energy_cdr, width = 15, height = 11)
