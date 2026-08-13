# Provider–recipient energy and emissions causal cascade
# Required input: global_17_IAMC.gdx in the working directory

library(tidyverse)
library(ggplot2)
library(gdxrrw)
library(patchwork)
library(scales)

gdx_file <- "global_17_IAMC.gdx"
output_dir <- "../../output/Figure"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

scenario_def <- "SSP2_400C_2030CP_NoCC_No"
scenario_aid <- "SSP2_400C_2030CP_15th_NoCC_No"
target_year <- 2050
region_lookup <- tribble(~REMF, ~RegionType,
                         "Rprovider15th", "Provider",
                         "Rrecipient15th", "Recipient") %>%
  mutate(RegionType = factor(RegionType, levels = c("Provider","Recipient")))
region_colors <- c(Provider = "#356C9B", Recipient = "#D27755")

if (!file.exists(gdx_file)) stop("Place global_17_IAMC.gdx in the working directory.")

spec <- tribble(
  ~Panel, ~Order, ~VEMF, ~Component, ~Unit, ~Scale,
  "Final",1,"Fin_Ene_Ind","Industry","EJ/yr",1,
  "Final",2,"Fin_Ene_Res_and_Com","Buildings","EJ/yr",1,
  "Final",3,"Fin_Ene_Tra_w_bun","Transport","EJ/yr",1,
  "Final",4,"Fin_Ene_NonEneUse","Non-energy use","EJ/yr",1,
#  "Final",5,"Fin_Ene_Oth_Sec","Other sectors","EJ/yr",1,
#  "Final",6,"Fin_Ene_Car_Man_Bio","Biomass CDR energy","EJ/yr",1,
#  "Final",7,"Fin_Ene_Car_Man_Dir_Air_Cap","DACCS energy","EJ/yr",1,
#  "Final",8,"Fin_Ene_Car_Man_Enh_Wea","Weathering energy","EJ/yr",1,
  "Secondary",1,"Sec_Ene_Ele","Electricity","EJ/yr",1,
  "Secondary",2,"Sec_Ene_Liq","Liquids","EJ/yr",1,
  "Secondary",3,"Sec_Ene_Gas","Gas","EJ/yr",1,
  "Secondary",4,"Sec_Ene_Heat","Heat","EJ/yr",1,
#  "Secondary",5,"Sec_Ene_Hyd","Hydrogen","EJ/yr",1,
#  "Secondary",6,"Sec_Ene_Solids","Solids","EJ/yr",1,
  "Power",1,"Sec_Ene_Ele_Coa","Coal","EJ/yr",1,
  "Power",2,"Sec_Ene_Ele_Gas","Gas","EJ/yr",1,
  "Power",3,"Sec_Ene_Ele_Oil","Oil","EJ/yr",1,
  "Power",4,"Sec_Ene_Ele_Solar","Solar","EJ/yr",1,
  "Power",5,"Sec_Ene_Ele_Win","Wind","EJ/yr",1,
  "Power",6,"Sec_Ene_Ele_Nuc","Nuclear","EJ/yr",1,
  "Power",7,"Sec_Ene_Ele_Bio","Bioenergy","EJ/yr",1,
#  "Power",8,"Sec_Ene_Ele_Geo","Geothermal","EJ/yr",1,
#  "Power",9,"Sec_Ene_Ele_Hyp","Hydropower","EJ/yr",1,
#  "Power",10,"Sec_Ene_Ele_Oth","Other","EJ/yr",1,
  "Primary",1,"Prm_Ene_Coa","Coal","EJ/yr",1,
  "Primary",2,"Prm_Ene_Gas","Gas","EJ/yr",1,
  "Primary",3,"Prm_Ene_Oil","Oil","EJ/yr",1,
  "Primary",4,"Prm_Ene_Solar","Solar","EJ/yr",1,
  "Primary",5,"Prm_Ene_Win","Wind","EJ/yr",1,
  "Primary",6,"Prm_Ene_Nuc","Nuclear","EJ/yr",1,
  "Primary",7,"Prm_Ene_Bio","Bioenergy","EJ/yr",1,
#  "Primary",8,"Prm_Ene_Geo","Geothermal","EJ/yr",1,
#  "Primary",9,"Prm_Ene_Hyp","Hydropower","EJ/yr",1,
#  "Primary",10,"Prm_Ene_Oth","Other","EJ/yr",1,
#  "Primary",11,"Prm_Ene_Sec_Ene_Trd","Secondary-energy trade","EJ/yr",1,
  "CDR",1,"Car_Seq_Lan_Use","Land use","GtCO2/yr",0.001,
  "CDR",2,"Car_Seq_CCS_Bio","BECCS","GtCO2/yr",0.001,
  "CDR",3,"Car_Seq_Dir_Air_Cap","DACCS","GtCO2/yr",0.001,
  "CDR",4,"Car_Seq_Enh_Wea","Enhanced weathering","GtCO2/yr",0.001,
  "Emissions",1,"Emi_CO2_AFO","AFOLU","GtCO2/yr",0.001,
  "Emissions",2,"Emi_CO2_Ene_Sup","Energy supply","GtCO2/yr",0.001,
  "Emissions",3,"Emi_CO2_Ene_Dem","Energy demand","GtCO2/yr",0.001,
  "Emissions",4,"Emi_CO2_Cap_and_Rem","Capture and removal","GtCO2/yr",0.001
#  "Emissions",5,"Emi_CO2_Ind_Pro","Industrial processes","GtCO2/yr",0.001,
#  "Emissions",6,"Emi_CO2_Pro_Use","Product use","GtCO2/yr",0.001
)

key_components <- list(
  Final = "Industry", Secondary = "Electricity",
  Power = "Solar", Primary = c("Oil","Solar","Bioenergy"),   CDR = c("BECCS", "Enhanced weathering"),
  Emissions = c("Energy supply","Capture and removal")
)

panel_titles <- c(
  Final = "1  Final energy by sector",
  Secondary = "2  Secondary energy by carrier", Power = "3  Power generation by source",
  Primary = "4  Primary energy by source", CDR = "5  Carbon dioxide removal",
  Emissions = "6  CO2 emissions by source"
)

raw <- rgdx.param(gdx_file, "IAMC_template") %>%
  filter(VEMF %in% unique(spec$VEMF), SCENARIO %in% c(scenario_def, scenario_aid),
         REMF %in% region_lookup$REMF, as.numeric(as.character(YEMF)) == target_year) %>%
  transmute(VEMF, REMF, SCENARIO, Value = as.numeric(IAMC_Template)) %>%
  group_by(VEMF, REMF, SCENARIO) %>% summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")

if (nrow(raw) == 0) stop("No matching GDX records were found. Check region names, target_year, scenario names and VEMF names.")
missing_scenarios <- setdiff(c(scenario_def, scenario_aid), unique(raw$SCENARIO))
if (length(missing_scenarios) > 0) stop("No records were found for scenario(s): ", paste(missing_scenarios, collapse = ", "))
missing_region_scenarios <- expand_grid(REMF = region_lookup$REMF, SCENARIO = c(scenario_def, scenario_aid)) %>%
  anti_join(raw %>% distinct(REMF, SCENARIO), by = c("REMF","SCENARIO"))
if (nrow(missing_region_scenarios) > 0) stop("No records were found for: ", paste0(missing_region_scenarios$REMF, " [", missing_region_scenarios$SCENARIO, "]", collapse = ", "))

missing_data <- expand_grid(VEMF = unique(spec$VEMF), REMF = region_lookup$REMF, SCENARIO = c(scenario_def, scenario_aid)) %>%
  anti_join(raw, by = c("VEMF","REMF","SCENARIO"))
if (nrow(missing_data) > 0) {
  warning(nrow(missing_data), " GDX record(s) are absent and will be treated as zero: ",
          paste0(missing_data$REMF, "/", missing_data$VEMF, " [", if_else(missing_data$SCENARIO == scenario_def, "Def", "Aid"), "]", collapse = ", "))
}

raw_wide <- expand_grid(VEMF = unique(spec$VEMF), REMF = region_lookup$REMF, SCENARIO = c(scenario_def, scenario_aid)) %>%
  left_join(raw, by = c("VEMF","REMF","SCENARIO")) %>%
  left_join(region_lookup, by = "REMF", relationship = "many-to-one") %>%
  mutate(Value = replace_na(Value, 0),
         Scenario = case_when(SCENARIO == scenario_def ~ "Def", SCENARIO == scenario_aid ~ "Aid")) %>%
  select(VEMF, RegionType, Scenario, Value) %>%
  pivot_wider(id_cols = c(VEMF, RegionType), names_from = Scenario, values_from = Value)

if (anyDuplicated(raw_wide[c("VEMF","RegionType")])) stop("Internal reshape error: raw_wide must contain one row per VEMF and region type.")

change_data <- bind_rows(
  spec %>% mutate(RegionType = factor("Provider", levels = c("Provider","Recipient"))),
  spec %>% mutate(RegionType = factor("Recipient", levels = c("Provider","Recipient")))
) %>%
  left_join(raw_wide, by = c("VEMF","RegionType"), relationship = "many-to-one") %>%
  mutate(Change = (Aid - Def) * Scale,
         Direction = case_when(Change > 1e-12 ~ "Increase", Change < -1e-12 ~ "Decrease", TRUE ~ "No change"))

signed_number <- function(x, accuracy) paste0(if_else(x > 0, "+", ""), number(x, accuracy = accuracy))

make_panel <- function(panel_id) {
  d <- change_data %>% filter(Panel == panel_id) %>% arrange(Order)
  unit <- unique(d$Unit); accuracy <- if (unit == "EJ/yr") 0.01 else 0.001
  keys <- key_components[[panel_id]]
  total_text <- d %>% group_by(RegionType) %>% summarise(Total = sum(Change, na.rm = TRUE), .groups = "drop") %>%
    arrange(RegionType) %>% transmute(Text = paste0(RegionType, ": ", signed_number(Total, accuracy), " ", unit)) %>%
    pull(Text) %>% paste(collapse = "  |  ")
  d <- d %>% mutate(Component = factor(Component, levels = rev(unique(Component))),
                    KeyAlpha = if_else(as.character(Component) %in% keys, 1, 0.42),
                    Label = if_else(abs(Change) < 1e-10, "", signed_number(Change, accuracy)),
                    LabelFace = if_else(as.character(Component) %in% keys, "bold", "plain"),
                    LabelHjust = if_else(Change >= 0, -0.12, 1.12))
  dodge <- position_dodge(width = 0.72)
  ggplot(d, aes(Change, Component, group = RegionType)) +
    geom_vline(xintercept = 0, color = "grey35", linewidth = 0.45) +
    geom_col(aes(fill = RegionType, alpha = KeyAlpha), position = dodge, width = 0.62,
             show.legend = panel_id == "Final") +
    geom_text(aes(label = Label, hjust = LabelHjust, fontface = LabelFace), position = dodge,
              size = 2.6, show.legend = FALSE) +
    scale_fill_manual(name = NULL, values = region_colors, breaks = c("Provider","Recipient")) +
    scale_alpha_identity() +
    scale_x_continuous(labels = label_number(accuracy = accuracy), expand = expansion(mult = c(0.28, 0.28))) +
    labs(title = panel_titles[[panel_id]], subtitle = total_text,
         x = paste0("Change (", unit, ")"), y = NULL) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 10) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
          plot.title = element_text(face = "bold", size = 11), plot.subtitle = element_text(size = 9, color = "grey35"),
          axis.title.x = element_text(size = 9), axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8.5),
          legend.position = "bottom", plot.margin = margin(5, 18, 5, 5))
}

p_final <- make_panel("Final")
p_secondary <- make_panel("Secondary"); p_power <- make_panel("Power")
p_primary <- make_panel("Primary"); p_cdr <- make_panel("CDR")
p_emissions <- make_panel("Emissions")

figure_cascade <- wrap_plots(
  p_final, p_secondary, p_power,
  p_primary, p_cdr, p_emissions,
  ncol = 3, guides = "collect"
) +
  plot_annotation(
    theme = theme(plot.title = element_text(face = "bold", size = 15), plot.subtitle = element_text(size = 11),
                  plot.caption = element_text(size = 9, color = "grey35"))
  ) &
  theme(legend.position = "bottom")

plot(figure_cascade)
ggsave(file.path(output_dir, "F4_Energy_summary.png"), figure_cascade, width = 18, height = 12, dpi = 600)
#ggsave(file.path(output_dir, "provider_recipient_energy_emissions_causal_cascade.pdf"), figure_cascade, width = 18, height = 12)