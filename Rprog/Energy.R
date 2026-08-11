# Fin_Ene ------------------
gdx_file <- "global_17_IAMC.gdx"
output_dir <- "decisive_figures_output"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

vec <- c("Fin_Ene_Oth_Sec","Fin_Ene_Tra_w_bun","Fin_Ene_Res_and_Com","Fin_Ene_Ind",
         "Fin_Ene_NonEneUse","Fin_Ene_Car_Man_Bio","Fin_Ene_Car_Man_Dir_Air_Cap",
         "Fin_Ene_Car_Man_Enh_Wea")

EnergyOrder <- c("Industry","Buildings","Transport incl. bunkers","Other sectors","Non-energy use",
                 "Biomass carbon management","DACCS energy use","Enhanced weathering energy use")

col <- c("Industry"="#4E79A7","Buildings"="#F2CF5B","Transport incl. bunkers"="#E15759",
         "Other sectors"="#9D9D9D","Non-energy use"="#B279A2","Biomass carbon management"="#59A14F",
         "DACCS energy use"="#7E6AA2","Enhanced weathering energy use"="#76B7B2")

CLP <- c("SSP2_400C_2030CP_NoCC_No","SSP2_400C_2030CP_15th_NoCC_No")
Region <- c("Rprovider15th","Rrecipient15th")

theme_energy <- theme_bw(base_size = 13) +
  theme(panel.grid = element_blank(), strip.background = element_blank(),
        strip.text = element_text(size = 13, face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.position = "bottom", legend.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

df_final <- rgdx.param(gdx_file, "IAMC_template") %>%
  filter(VEMF %in% vec, SCENARIO %in% CLP, REMF %in% Region) %>%
  transmute(
    Year = as.numeric(as.character(YEMF)),
    REMF = recode(REMF, "Rprovider15th"="Provider", "Rrecipient15th"="Recipient"),
    SCENARIO = recode(SCENARIO, "SSP2_400C_2030CP_NoCC_No"="Def",
                     "SSP2_400C_2030CP_15th_NoCC_No"="Aid"),
    VEMF = recode(VEMF, "Fin_Ene_Oth_Sec"="Other sectors",
                  "Fin_Ene_Tra_w_bun"="Transport incl. bunkers",
                  "Fin_Ene_Res_and_Com"="Buildings","Fin_Ene_Ind"="Industry",
                  "Fin_Ene_NonEneUse"="Non-energy use",
                  "Fin_Ene_Car_Man_Bio"="Biomass carbon management",
                  "Fin_Ene_Car_Man_Dir_Air_Cap"="DACCS energy use",
                  "Fin_Ene_Car_Man_Enh_Wea"="Enhanced weathering energy use"),
    value = as.numeric(IAMC_Template)
  ) %>%
  filter(Year >= 2020, Year <= 2050) %>%
  group_by(REMF, SCENARIO, Year, VEMF) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    REMF = factor(REMF, levels = c("Provider","Recipient")),
    SCENARIO = factor(SCENARIO, levels = c("Def","Aid")),
    VEMF = factor(VEMF, levels = EnergyOrder)
  ) %>%
  complete(REMF, SCENARIO, Year, VEMF, fill = list(value = 0))

## 1. Absolute trend and composition --------------------------------------

df_trend_total <- df_final %>%
  group_by(REMF, SCENARIO, Year) %>%
  summarise(TotalFinalEnergy = sum(value, na.rm = TRUE), .groups = "drop")

g_trend <- ggplot(df_final, aes(Year, value, fill = VEMF)) +
  geom_area(position = "stack", color = "white", linewidth = 0.15) +
  geom_line(data = df_trend_total,
            aes(Year, TotalFinalEnergy, color = "Total final energy", group = 1),
            inherit.aes = FALSE, linewidth = 1) +
  scale_fill_manual(name = "Final-energy use", values = col, breaks = EnergyOrder,
                    guide = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_color_manual(name = NULL, values = c("Total final energy"="black")) +
  scale_x_continuous(limits = c(2020,2050), breaks = c(2020,2030,2040,2050),
                     expand = expansion(mult = c(0,0))) +
  scale_y_continuous(expand = expansion(mult = c(0,0.05))) +
  facet_grid(REMF ~ SCENARIO, scales = "free_y") +
  labs(title = "Final energy use by sector and carbon-management activity",
       x = NULL, y = "Final energy (EJ/yr)") +
  theme_energy

plot(g_trend)

## 2. Aid − Def change and contribution ----------------------------------

df_change <- df_final %>%
  pivot_wider(names_from = SCENARIO, values_from = value, values_fill = 0) %>%
  group_by(REMF, Year) %>%
  mutate(
    TotalDef = sum(Def, na.rm = TRUE),
    TotalAid = sum(Aid, na.rm = TRUE),
    Contribution = if_else(TotalDef != 0, 100*(Aid-Def)/TotalDef, NA_real_),
    TotalChange = if_else(TotalDef != 0, 100*(TotalAid-TotalDef)/TotalDef, NA_real_)
  ) %>%
  ungroup() %>%
  filter(is.finite(Contribution), is.finite(TotalChange))

df_total <- df_change %>%
  distinct(REMF, Year, TotalChange)

g_change <- ggplot(df_change, aes(Year, Contribution)) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
  geom_col(aes(fill = VEMF), position = "stack", width = 4,
           color = "white", linewidth = 0.2) +
  geom_line(data = df_total,
            aes(Year, TotalChange, color = "Total final energy", group = 1),
            linewidth = 1.1) +
  geom_point(data = df_total,
             aes(Year, TotalChange, color = "Total final energy"),
             shape = 21, fill = "white", size = 3, stroke = 1) +
  scale_fill_manual(name = "Final-energy use", values = col, breaks = EnergyOrder,
                    guide = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_color_manual(name = NULL, values = c("Total final energy"="black")) +
  scale_x_continuous(limits = c(2018,2052), breaks = c(2020,2030,2040,2050)) +
  scale_y_continuous(labels = label_number(accuracy = 0.1, suffix = "%"),
                     expand = expansion(mult = c(0.08,0.08))) +
  facet_wrap(~REMF, nrow = 1, scales = "fixed") +
  labs(title = "Change in final energy use induced by revenue transfers",
       x = NULL,
       y = "Contribution to final energy change\n(Aid − Def, % of total final energy in Def)") +
  theme_energy

plot(g_change)

ggsave(file.path(output_dir, "final_energy_absolute_trend.png"),
       g_trend, width = 13, height = 8, dpi = 600)
ggsave(file.path(output_dir, "final_energy_absolute_trend.pdf"),
       g_trend, width = 13, height = 8)

ggsave(file.path(output_dir, "final_energy_change.png"),
       g_change, width = 13, height = 6.5, dpi = 600)
ggsave(file.path(output_dir, "final_energy_change.pdf"),
       g_change, width = 13, height = 6.5)

# Fin_Ene_Ind ------------------------

gdx_file <- "global_17_IAMC.gdx"
output_dir <- "decisive_figures_output"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

vec_ind <- c("Fin_Ene_Ind_Ele","Fin_Ene_Ind_Gas","Fin_Ene_Ind_Heat","Fin_Ene_Ind_Hyd",
             "Fin_Ene_Ind_Gas_Hyd_syn","Fin_Ene_Ind_Liq","Fin_Ene_Ind_Oth","Fin_Ene_Ind_Solids")

IndustryEnergyOrder <- c("Solids","Liquids","Gas","Synthetic gas","Electricity","Heat","Hydrogen","Other")

col_ind <- c("Solids"="#6C757D","Liquids"="#F28E2B","Gas"="#EDC948","Synthetic gas"="#B279A2",
             "Electricity"="#4E79A7","Heat"="#E15759","Hydrogen"="#76B7B2","Other"="#BAB0AC")

CLP <- c("SSP2_400C_2030CP_NoCC_No","SSP2_400C_2030CP_15th_NoCC_No")
Region <- c("Rprovider15th","Rrecipient15th")

theme_energy <- theme_bw(base_size = 13) +
  theme(panel.grid = element_blank(), strip.background = element_blank(),
        strip.text = element_text(size = 13, face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.position = "bottom", legend.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

df_ind <- rgdx.param(gdx_file, "IAMC_template") %>%
  filter(VEMF %in% vec_ind, SCENARIO %in% CLP, REMF %in% Region) %>%
  transmute(
    Year = as.numeric(as.character(YEMF)),
    REMF = recode(REMF, "Rprovider15th"="Provider", "Rrecipient15th"="Recipient"),
    SCENARIO = recode(SCENARIO, "SSP2_400C_2030CP_NoCC_No"="Def",
                     "SSP2_400C_2030CP_15th_NoCC_No"="Aid"),
    VEMF = recode(VEMF, "Fin_Ene_Ind_Ele"="Electricity","Fin_Ene_Ind_Gas"="Gas",
                  "Fin_Ene_Ind_Heat"="Heat","Fin_Ene_Ind_Hyd"="Hydrogen",
                  "Fin_Ene_Ind_Gas_Hyd_syn"="Synthetic gas","Fin_Ene_Ind_Liq"="Liquids",
                  "Fin_Ene_Ind_Oth"="Other","Fin_Ene_Ind_Solids"="Solids"),
    value = as.numeric(IAMC_Template)
  ) %>%
  filter(Year >= 2020, Year <= 2050) %>%
  group_by(REMF, SCENARIO, Year, VEMF) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    REMF = factor(REMF, levels = c("Provider","Recipient")),
    SCENARIO = factor(SCENARIO, levels = c("Def","Aid")),
    VEMF = factor(VEMF, levels = IndustryEnergyOrder)
  ) %>%
  complete(REMF, SCENARIO, Year, VEMF, fill = list(value = 0))

## 1. Industrial final-energy trend --------------------------------------

df_ind_total <- df_ind %>%
  group_by(REMF, SCENARIO, Year) %>%
  summarise(TotalIndustryEnergy = sum(value, na.rm = TRUE), .groups = "drop")

g_ind_trend <- ggplot(df_ind, aes(Year, value, fill = VEMF)) +
  geom_area(position = "stack", color = "white", linewidth = 0.15) +
  geom_line(data = df_ind_total,
            aes(Year, TotalIndustryEnergy, color = "Total industry energy", group = 1),
            inherit.aes = FALSE, linewidth = 1) +
  scale_fill_manual(name = "Energy carrier", values = col_ind, breaks = IndustryEnergyOrder,
                    guide = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_color_manual(name = NULL, values = c("Total industry energy"="black")) +
  scale_x_continuous(limits = c(2020,2050), breaks = c(2020,2030,2040,2050),
                     expand = expansion(mult = c(0,0))) +
  scale_y_continuous(expand = expansion(mult = c(0,0.05))) +
  facet_grid(REMF ~ SCENARIO, scales = "free_y") +
  labs(title = "Industrial final energy use by energy carrier",
       x = NULL, y = "Industrial final energy (EJ/yr)") +
  theme_energy

plot(g_ind_trend)

## 2. Aid − Def change ---------------------------------------------------

df_ind_change <- df_ind %>%
  pivot_wider(names_from = SCENARIO, values_from = value, values_fill = 0) %>%
  group_by(REMF, Year) %>%
  mutate(
    TotalDef = sum(Def, na.rm = TRUE),
    TotalAid = sum(Aid, na.rm = TRUE),
    Contribution = if_else(TotalDef != 0, 100*(Aid-Def)/TotalDef, NA_real_),
    TotalChange = if_else(TotalDef != 0, 100*(TotalAid-TotalDef)/TotalDef, NA_real_)
  ) %>%
  ungroup() %>%
  filter(is.finite(Contribution), is.finite(TotalChange))

df_ind_change_total <- df_ind_change %>%
  distinct(REMF, Year, TotalChange)

g_ind_change <- ggplot(df_ind_change, aes(Year, Contribution)) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
  geom_col(aes(fill = VEMF), position = "stack", width = 4,
           color = "white", linewidth = 0.2) +
  geom_line(data = df_ind_change_total,
            aes(Year, TotalChange, color = "Total industry energy", group = 1),
            linewidth = 1.1) +
  geom_point(data = df_ind_change_total,
             aes(Year, TotalChange, color = "Total industry energy"),
             shape = 21, fill = "white", size = 3, stroke = 1) +
  scale_fill_manual(name = "Energy carrier", values = col_ind, breaks = IndustryEnergyOrder,
                    guide = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_color_manual(name = NULL, values = c("Total industry energy"="black")) +
  scale_x_continuous(limits = c(2018,2052), breaks = c(2020,2030,2040,2050)) +
  scale_y_continuous(labels = label_number(accuracy = 0.1, suffix = "%"),
                     expand = expansion(mult = c(0.08,0.08))) +
  facet_wrap(~REMF, nrow = 1, scales = "fixed") +
  labs(title = "Change in industrial final energy use induced by revenue transfers",
       x = NULL,
       y = "Contribution to industrial energy change\n(Aid − Def, % of industrial final energy in Def)") +
  theme_energy

plot(g_ind_change)

ggsave(file.path(output_dir, "industrial_final_energy_absolute_trend.png"),
       g_ind_trend, width = 13, height = 8, dpi = 600)
ggsave(file.path(output_dir, "industrial_final_energy_absolute_trend.pdf"),
       g_ind_trend, width = 13, height = 8)

ggsave(file.path(output_dir, "industrial_final_energy_change.png"),
       g_ind_change, width = 13, height = 6.5, dpi = 600)
ggsave(file.path(output_dir, "industrial_final_energy_change.pdf"),
       g_ind_change, width = 13, height = 6.5)

# Fin_Ene_Ele -------------------------------
gdx_file <- "global_17_IAMC.gdx"
output_dir <- "decisive_figures_output"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

vec_ele <- c("Fin_Ene_Ind_Ele","Fin_Ene_Res_and_Com_Ele","Fin_Ene_Tra_w_bun_Ele",
             "Fin_Ene_Oth_Sec_Ele","Fin_Ene_Car_Man_Bio_Ele",
             "Fin_Ene_Car_Man_Dir_Air_Cap_Ele","Fin_Ene_Car_Man_Enh_Wea_Ele")

ElectricityUseOrder <- c("Industry","Buildings","Transport incl. bunkers","Other sectors",
                         "Biomass carbon management","DACCS energy use",
                         "Enhanced weathering energy use")

col_ele <- c("Industry"="#4E79A7","Buildings"="#F2CF5B","Transport incl. bunkers"="#E15759",
             "Other sectors"="#9D9D9D","Biomass carbon management"="#59A14F",
             "DACCS energy use"="#7E6AA2","Enhanced weathering energy use"="#76B7B2")

CLP <- c("SSP2_400C_2030CP_NoCC_No","SSP2_400C_2030CP_15th_NoCC_No")
Region <- c("Rprovider15th","Rrecipient15th")

theme_energy <- theme_bw(base_size = 13) +
  theme(panel.grid = element_blank(), strip.background = element_blank(),
        strip.text = element_text(size = 13, face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.position = "bottom", legend.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

df_ele <- rgdx.param(gdx_file, "IAMC_template") %>%
  filter(VEMF %in% vec_ele, SCENARIO %in% CLP, REMF %in% Region) %>%
  transmute(
    Year = as.numeric(as.character(YEMF)),
    REMF = recode(REMF, "Rprovider15th"="Provider", "Rrecipient15th"="Recipient"),
    SCENARIO = recode(SCENARIO, "SSP2_400C_2030CP_NoCC_No"="Def",
                     "SSP2_400C_2030CP_15th_NoCC_No"="Aid"),
    VEMF = recode(VEMF, "Fin_Ene_Ind_Ele"="Industry",
                  "Fin_Ene_Res_and_Com_Ele"="Buildings",
                  "Fin_Ene_Tra_w_bun_Ele"="Transport incl. bunkers",
                  "Fin_Ene_Oth_Sec_Ele"="Other sectors",
                  "Fin_Ene_Car_Man_Bio_Ele"="Biomass carbon management",
                  "Fin_Ene_Car_Man_Dir_Air_Cap_Ele"="DACCS energy use",
                  "Fin_Ene_Car_Man_Enh_Wea_Ele"="Enhanced weathering energy use"),
    value = as.numeric(IAMC_Template)
  ) %>%
  filter(Year >= 2020, Year <= 2050) %>%
  group_by(REMF, SCENARIO, Year, VEMF) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    REMF = factor(REMF, levels = c("Provider","Recipient")),
    SCENARIO = factor(SCENARIO, levels = c("Def","Aid")),
    VEMF = factor(VEMF, levels = ElectricityUseOrder)
  ) %>%
  complete(REMF, SCENARIO, Year, VEMF, fill = list(value = 0))

## 1. Absolute electricity trend -----------------------------------------

df_ele_total <- df_ele %>%
  group_by(REMF, SCENARIO, Year) %>%
  summarise(TotalElectricity = sum(value, na.rm = TRUE), .groups = "drop")

g_ele_trend <- ggplot(df_ele, aes(Year, value, fill = VEMF)) +
  geom_area(position = "stack", color = "white", linewidth = 0.15) +
  geom_line(data = df_ele_total,
            aes(Year, TotalElectricity, color = "Total final electricity", group = 1),
            inherit.aes = FALSE, linewidth = 1) +
  scale_fill_manual(name = "Electricity use", values = col_ele,
                    breaks = ElectricityUseOrder,
                    guide = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_color_manual(name = NULL, values = c("Total final electricity"="black")) +
  scale_x_continuous(limits = c(2020,2050), breaks = c(2020,2030,2040,2050),
                     expand = expansion(mult = c(0,0))) +
  scale_y_continuous(expand = expansion(mult = c(0,0.05))) +
  facet_grid(REMF ~ SCENARIO, scales = "free_y") +
  labs(title = "Final electricity use by sector and carbon-management activity",
       x = NULL, y = "Final electricity (EJ/yr)") +
  theme_energy

plot(g_ele_trend)

## 2. Aid − Def change ---------------------------------------------------

df_ele_change <- df_ele %>%
  pivot_wider(names_from = SCENARIO, values_from = value, values_fill = 0) %>%
  group_by(REMF, Year) %>%
  mutate(
    TotalDef = sum(Def, na.rm = TRUE),
    TotalAid = sum(Aid, na.rm = TRUE),
    Contribution = if_else(TotalDef != 0, 100*(Aid-Def)/TotalDef, NA_real_),
    TotalChange = if_else(TotalDef != 0, 100*(TotalAid-TotalDef)/TotalDef, NA_real_)
  ) %>%
  ungroup() %>%
  filter(is.finite(Contribution), is.finite(TotalChange))

df_ele_change_total <- df_ele_change %>%
  distinct(REMF, Year, TotalChange)

g_ele_change <- ggplot(df_ele_change, aes(Year, Contribution)) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
  geom_col(aes(fill = VEMF), position = "stack", width = 4,
           color = "white", linewidth = 0.2) +
  geom_line(data = df_ele_change_total,
            aes(Year, TotalChange, color = "Total final electricity", group = 1),
            linewidth = 1.1) +
  geom_point(data = df_ele_change_total,
             aes(Year, TotalChange, color = "Total final electricity"),
             shape = 21, fill = "white", size = 3, stroke = 1) +
  scale_fill_manual(name = "Electricity use", values = col_ele,
                    breaks = ElectricityUseOrder,
                    guide = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_color_manual(name = NULL, values = c("Total final electricity"="black")) +
  scale_x_continuous(limits = c(2018,2052), breaks = c(2020,2030,2040,2050)) +
  scale_y_continuous(labels = label_number(accuracy = 0.1, suffix = "%"),
                     expand = expansion(mult = c(0.08,0.08))) +
  facet_wrap(~REMF, nrow = 1, scales = "fixed") +
  labs(title = "Change in final electricity use induced by revenue transfers",
       x = NULL,
       y = "Contribution to final electricity change\n(Aid − Def, % of final electricity in Def)") +
  theme_energy

plot(g_ele_change)

ggsave(file.path(output_dir, "final_electricity_absolute_trend.png"),
       g_ele_trend, width = 13, height = 8, dpi = 600)
ggsave(file.path(output_dir, "final_electricity_absolute_trend.pdf"),
       g_ele_trend, width = 13, height = 8)

ggsave(file.path(output_dir, "final_electricity_change.png"),
       g_ele_change, width = 13, height = 6.5, dpi = 600)
ggsave(file.path(output_dir, "final_electricity_change.pdf"),
       g_ele_change, width = 13, height = 6.5)

# Sec_Ene -------------------------
gdx_file <- "global_17_IAMC.gdx"
output_dir <- "decisive_figures_output"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

vec_sec <- c("Sec_Ene_Ele","Sec_Ene_Liq","Sec_Ene_Gas",
             "Sec_Ene_Heat","Sec_Ene_Hyd","Sec_Ene_Solids")

SecondaryEnergyOrder <- c("Solids","Liquids","Gas","Electricity","Heat","Hydrogen")

col_sec <- c("Solids"="#6C757D","Liquids"="#F28E2B","Gas"="#EDC948",
             "Electricity"="#4E79A7","Heat"="#E15759","Hydrogen"="#76B7B2")

CLP <- c("SSP2_400C_2030CP_NoCC_No","SSP2_400C_2030CP_15th_NoCC_No")
Region <- c("Rprovider15th","Rrecipient15th")

theme_energy <- theme_bw(base_size = 13) +
  theme(panel.grid = element_blank(), strip.background = element_blank(),
        strip.text = element_text(size = 13, face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.position = "bottom", legend.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

df_sec <- rgdx.param(gdx_file, "IAMC_template") %>%
  filter(VEMF %in% vec_sec, SCENARIO %in% CLP, REMF %in% Region) %>%
  transmute(
    Year = as.numeric(as.character(YEMF)),
    REMF = recode(REMF, "Rprovider15th"="Provider", "Rrecipient15th"="Recipient"),
    SCENARIO = recode(SCENARIO, "SSP2_400C_2030CP_NoCC_No"="Def",
                     "SSP2_400C_2030CP_15th_NoCC_No"="Aid"),
    VEMF = recode(VEMF, "Sec_Ene_Ele"="Electricity","Sec_Ene_Liq"="Liquids",
                  "Sec_Ene_Gas"="Gas","Sec_Ene_Heat"="Heat",
                  "Sec_Ene_Hyd"="Hydrogen","Sec_Ene_Solids"="Solids"),
    value = as.numeric(IAMC_Template)
  ) %>%
  filter(Year >= 2020, Year <= 2050) %>%
  group_by(REMF, SCENARIO, Year, VEMF) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    REMF = factor(REMF, levels = c("Provider","Recipient")),
    SCENARIO = factor(SCENARIO, levels = c("Def","Aid")),
    VEMF = factor(VEMF, levels = SecondaryEnergyOrder)
  ) %>%
  complete(REMF, SCENARIO, Year, VEMF, fill = list(value = 0))

## 1. Absolute secondary-energy trend ------------------------------------

df_sec_total <- df_sec %>%
  group_by(REMF, SCENARIO, Year) %>%
  summarise(TotalSecondaryEnergy = sum(value, na.rm = TRUE), .groups = "drop")

g_sec_trend <- ggplot(df_sec, aes(Year, value, fill = VEMF)) +
  geom_area(position = "stack", color = "white", linewidth = 0.15) +
  geom_line(data = df_sec_total,
            aes(Year, TotalSecondaryEnergy, color = "Total secondary energy", group = 1),
            inherit.aes = FALSE, linewidth = 1) +
  scale_fill_manual(name = "Energy carrier", values = col_sec,
                    breaks = SecondaryEnergyOrder,
                    guide = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_color_manual(name = NULL, values = c("Total secondary energy"="black")) +
  scale_x_continuous(limits = c(2020,2050), breaks = c(2020,2030,2040,2050),
                     expand = expansion(mult = c(0,0))) +
  scale_y_continuous(expand = expansion(mult = c(0,0.05))) +
  facet_grid(REMF ~ SCENARIO, scales = "free_y") +
  labs(title = "Secondary energy supply by energy carrier",
       x = NULL, y = "Secondary energy (EJ/yr)") +
  theme_energy

plot(g_sec_trend)

## 2. Aid − Def change ---------------------------------------------------

df_sec_change <- df_sec %>%
  pivot_wider(names_from = SCENARIO, values_from = value, values_fill = 0) %>%
  group_by(REMF, Year) %>%
  mutate(
    TotalDef = sum(Def, na.rm = TRUE),
    TotalAid = sum(Aid, na.rm = TRUE),
    Contribution = if_else(TotalDef != 0, 100*(Aid-Def)/TotalDef, NA_real_),
    TotalChange = if_else(TotalDef != 0, 100*(TotalAid-TotalDef)/TotalDef, NA_real_)
  ) %>%
  ungroup() %>%
  filter(is.finite(Contribution), is.finite(TotalChange))

df_sec_change_total <- df_sec_change %>%
  distinct(REMF, Year, TotalChange)

g_sec_change <- ggplot(df_sec_change, aes(Year, Contribution)) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
  geom_col(aes(fill = VEMF), position = "stack", width = 4,
           color = "white", linewidth = 0.2) +
  geom_line(data = df_sec_change_total,
            aes(Year, TotalChange, color = "Total secondary energy", group = 1),
            linewidth = 1.1) +
  geom_point(data = df_sec_change_total,
             aes(Year, TotalChange, color = "Total secondary energy"),
             shape = 21, fill = "white", size = 3, stroke = 1) +
  scale_fill_manual(name = "Energy carrier", values = col_sec,
                    breaks = SecondaryEnergyOrder,
                    guide = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_color_manual(name = NULL, values = c("Total secondary energy"="black")) +
  scale_x_continuous(limits = c(2018,2052), breaks = c(2020,2030,2040,2050)) +
  scale_y_continuous(labels = label_number(accuracy = 0.1, suffix = "%"),
                     expand = expansion(mult = c(0.08,0.08))) +
  facet_wrap(~REMF, nrow = 1, scales = "fixed") +
  labs(title = "Change in secondary energy induced by revenue transfers",
       x = NULL,
       y = "Contribution to secondary energy change\n(Aid − Def, % of secondary energy in Def)") +
  theme_energy

plot(g_sec_change)

ggsave(file.path(output_dir, "secondary_energy_absolute_trend.png"),
       g_sec_trend, width = 13, height = 8, dpi = 600)
ggsave(file.path(output_dir, "secondary_energy_absolute_trend.pdf"),
       g_sec_trend, width = 13, height = 8)

ggsave(file.path(output_dir, "secondary_energy_change.png"),
       g_sec_change, width = 13, height = 6.5, dpi = 600)
ggsave(file.path(output_dir, "secondary_energy_change.pdf"),
       g_sec_change, width = 13, height = 6.5)

# Sec_Ene_Ele -------------------
vec_power <- c("Sec_Ene_Ele_Coa","Sec_Ene_Ele_Gas","Sec_Ene_Ele_Oil","Sec_Ene_Ele_Geo",
               "Sec_Ene_Ele_Solar","Sec_Ene_Ele_Win","Sec_Ene_Ele_Hyp","Sec_Ene_Ele_Oth",
               "Sec_Ene_Ele_Bio","Sec_Ene_Ele_Nuc")

PowerOrder <- c("Coal","Oil","Gas","Bioenergy","Nuclear","Hydropower",
                "Solar","Wind","Geothermal","Other")

col_power <- c("Coal"="#4D4D4D","Oil"="#B66D3D","Gas"="#E3B448",
               "Bioenergy"="#59A14F","Nuclear"="#8E6C8A","Hydropower"="#76B7B2",
               "Solar"="#E58E65","Wind"="#4E79A7","Geothermal"="#A56A43",
               "Other"="#BAB0AC")

CLP <- c("SSP2_400C_2030CP_NoCC_No","SSP2_400C_2030CP_15th_NoCC_No")
Region <- c("Rprovider15th","Rrecipient15th")

df_power <- rgdx.param(gdx_file, "IAMC_template") %>%
  filter(VEMF %in% vec_power, SCENARIO %in% CLP, REMF %in% Region) %>%
  transmute(
    Year = as.numeric(as.character(YEMF)),
    REMF = recode(REMF, "Rprovider15th"="Provider", "Rrecipient15th"="Recipient"),
    SCENARIO = recode(SCENARIO, "SSP2_400C_2030CP_NoCC_No"="Def",
                     "SSP2_400C_2030CP_15th_NoCC_No"="Aid"),
    VEMF = recode(VEMF, "Sec_Ene_Ele_Coa"="Coal","Sec_Ene_Ele_Gas"="Gas",
                  "Sec_Ene_Ele_Oil"="Oil","Sec_Ene_Ele_Geo"="Geothermal",
                  "Sec_Ene_Ele_Solar"="Solar","Sec_Ene_Ele_Win"="Wind",
                  "Sec_Ene_Ele_Hyp"="Hydropower","Sec_Ene_Ele_Oth"="Other",
                  "Sec_Ene_Ele_Bio"="Bioenergy","Sec_Ene_Ele_Nuc"="Nuclear"),
    value = as.numeric(IAMC_Template)
  ) %>%
  filter(Year >= 2020, Year <= 2050) %>%
  group_by(REMF, SCENARIO, Year, VEMF) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    REMF = factor(REMF, levels = c("Provider","Recipient")),
    SCENARIO = factor(SCENARIO, levels = c("Def","Aid")),
    VEMF = factor(VEMF, levels = PowerOrder)
  ) %>%
  complete(REMF, SCENARIO, Year, VEMF, fill = list(value = 0))

## 1. Power-generation trend ---------------------------------------------

df_power_total <- df_power %>%
  group_by(REMF, SCENARIO, Year) %>%
  summarise(TotalPower = sum(value, na.rm = TRUE), .groups = "drop")

g_power_trend <- ggplot(df_power, aes(Year, value, fill = VEMF)) +
  geom_area(position = "stack", color = "white", linewidth = 0.15) +
  geom_line(data = df_power_total,
            aes(Year, TotalPower, color = "Total power generation", group = 1),
            inherit.aes = FALSE, linewidth = 1) +
  scale_fill_manual(name = "Power source", values = col_power, breaks = PowerOrder,
                    guide = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_color_manual(name = NULL, values = c("Total power generation"="black")) +
  scale_x_continuous(limits = c(2020,2050), breaks = c(2020,2030,2040,2050),
                     expand = expansion(mult = c(0,0))) +
  scale_y_continuous(expand = expansion(mult = c(0,0.05))) +
  facet_grid(REMF ~ SCENARIO, scales = "free_y") +
  labs(title = "Power generation by energy source",
       x = NULL, y = "Power generation (EJ/yr)") +
  theme_energy

plot(g_power_trend)

## 2. Aid − Def change ---------------------------------------------------

df_power_change <- df_power %>%
  pivot_wider(names_from = SCENARIO, values_from = value, values_fill = 0) %>%
  group_by(REMF, Year) %>%
  mutate(
    TotalDef = sum(Def, na.rm = TRUE),
    TotalAid = sum(Aid, na.rm = TRUE),
    Contribution = if_else(TotalDef != 0, 100*(Aid-Def)/TotalDef, NA_real_),
    TotalChange = if_else(TotalDef != 0, 100*(TotalAid-TotalDef)/TotalDef, NA_real_)
  ) %>%
  ungroup() %>%
  filter(is.finite(Contribution), is.finite(TotalChange))

df_power_change_total <- df_power_change %>%
  distinct(REMF, Year, TotalChange)

g_power_change <- ggplot(df_power_change, aes(Year, Contribution)) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
  geom_col(aes(fill = VEMF), position = "stack", width = 4,
           color = "white", linewidth = 0.2) +
  geom_line(data = df_power_change_total,
            aes(Year, TotalChange, color = "Total power generation", group = 1),
            linewidth = 1.1) +
  geom_point(data = df_power_change_total,
             aes(Year, TotalChange, color = "Total power generation"),
             shape = 21, fill = "white", size = 3, stroke = 1) +
  scale_fill_manual(name = "Power source", values = col_power, breaks = PowerOrder,
                    guide = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_color_manual(name = NULL, values = c("Total power generation"="black")) +
  scale_x_continuous(limits = c(2018,2052), breaks = c(2020,2030,2040,2050)) +
  scale_y_continuous(labels = label_number(accuracy = 0.1, suffix = "%"),
                     expand = expansion(mult = c(0.08,0.08))) +
  facet_wrap(~REMF, nrow = 1, scales = "fixed") +
  labs(title = "Change in power generation induced by revenue transfers",
       x = NULL,
       y = "Contribution to power-generation change\n(Aid − Def, % of power generation in Def)") +
  theme_energy

plot(g_power_change)

ggsave(file.path(output_dir, "power_generation_absolute_trend.png"),
       g_power_trend, width = 13, height = 8, dpi = 600)
ggsave(file.path(output_dir, "power_generation_absolute_trend.pdf"),
       g_power_trend, width = 13, height = 8)

ggsave(file.path(output_dir, "power_generation_change.png"),
       g_power_change, width = 13, height = 6.5, dpi = 600)
ggsave(file.path(output_dir, "power_generation_change.pdf"),
       g_power_change, width = 13, height = 6.5)

# Prm_Ene ---------------
vec_primary <- c("Prm_Ene_Coa","Prm_Ene_Gas","Prm_Ene_Oil","Prm_Ene_Geo",
                 "Prm_Ene_Solar","Prm_Ene_Win","Prm_Ene_Hyp","Prm_Ene_Oth",
                 "Prm_Ene_Bio","Prm_Ene_Nuc","Prm_Ene_Sec_Ene_Trd")

PrimaryEnergyOrder <- c("Coal","Oil","Gas","Bioenergy","Nuclear","Hydropower",
                        "Solar","Wind","Geothermal","Other","Secondary energy trade")

col_primary <- c("Coal"="#4D4D4D","Oil"="#B66D3D","Gas"="#E3B448",
                 "Bioenergy"="#59A14F","Nuclear"="#8E6C8A","Hydropower"="#76B7B2",
                 "Solar"="#E58E65","Wind"="#4E79A7","Geothermal"="#A56A43",
                 "Other"="#BAB0AC","Secondary energy trade"="#7E6AA2")

CLP <- c("SSP2_400C_2030CP_NoCC_No","SSP2_400C_2030CP_15th_NoCC_No")
Region <- c("Rprovider15th","Rrecipient15th")

df_primary <- rgdx.param(gdx_file, "IAMC_template") %>%
  filter(VEMF %in% vec_primary, SCENARIO %in% CLP, REMF %in% Region) %>%
  transmute(
    Year = as.numeric(as.character(YEMF)),
    REMF = recode(REMF, "Rprovider15th"="Provider", "Rrecipient15th"="Recipient"),
    SCENARIO = recode(SCENARIO, "SSP2_400C_2030CP_NoCC_No"="Def",
                     "SSP2_400C_2030CP_15th_NoCC_No"="Aid"),
    VEMF = recode(VEMF, "Prm_Ene_Coa"="Coal","Prm_Ene_Gas"="Gas",
                  "Prm_Ene_Oil"="Oil","Prm_Ene_Geo"="Geothermal",
                  "Prm_Ene_Solar"="Solar","Prm_Ene_Win"="Wind",
                  "Prm_Ene_Hyp"="Hydropower","Prm_Ene_Oth"="Other",
                  "Prm_Ene_Bio"="Bioenergy","Prm_Ene_Nuc"="Nuclear",
                  "Prm_Ene_Sec_Ene_Trd"="Secondary energy trade"),
    value = as.numeric(IAMC_Template)
  ) %>%
  filter(Year >= 2020, Year <= 2050) %>%
  group_by(REMF, SCENARIO, Year, VEMF) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    REMF = factor(REMF, levels = c("Provider","Recipient")),
    SCENARIO = factor(SCENARIO, levels = c("Def","Aid")),
    VEMF = factor(VEMF, levels = PrimaryEnergyOrder)
  ) %>%
  complete(REMF, SCENARIO, Year, VEMF, fill = list(value = 0))

## 1. Absolute primary-energy trend --------------------------------------

df_primary_total <- df_primary %>%
  group_by(REMF, SCENARIO, Year) %>%
  summarise(TotalPrimaryEnergy = sum(value, na.rm = TRUE), .groups = "drop")

g_primary_trend <- ggplot(df_primary, aes(Year, value, fill = VEMF)) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
  geom_area(position = "stack", color = "white", linewidth = 0.15) +
  geom_line(data = df_primary_total,
            aes(Year, TotalPrimaryEnergy, color = "Total primary energy", group = 1),
            inherit.aes = FALSE, linewidth = 1) +
  scale_fill_manual(name = "Energy source", values = col_primary,
                    breaks = PrimaryEnergyOrder,
                    guide = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_color_manual(name = NULL, values = c("Total primary energy"="black")) +
  scale_x_continuous(limits = c(2020,2050), breaks = c(2020,2030,2040,2050),
                     expand = expansion(mult = c(0,0))) +
  scale_y_continuous(expand = expansion(mult = c(0.05,0.05))) +
  facet_grid(REMF ~ SCENARIO, scales = "free_y") +
  labs(title = "Primary energy supply by energy source",
       x = NULL, y = "Primary energy (EJ/yr)") +
  theme_energy

plot(g_primary_trend)

## 2. Aid − Def change ---------------------------------------------------

df_primary_change <- df_primary %>%
  pivot_wider(names_from = SCENARIO, values_from = value, values_fill = 0) %>%
  group_by(REMF, Year) %>%
  mutate(
    TotalDef = sum(Def, na.rm = TRUE),
    TotalAid = sum(Aid, na.rm = TRUE),
    Contribution = if_else(TotalDef != 0, 100*(Aid-Def)/TotalDef, NA_real_),
    TotalChange = if_else(TotalDef != 0, 100*(TotalAid-TotalDef)/TotalDef, NA_real_)
  ) %>%
  ungroup() %>%
  filter(is.finite(Contribution), is.finite(TotalChange))

df_primary_change_total <- df_primary_change %>%
  distinct(REMF, Year, TotalChange)

g_primary_change <- ggplot(df_primary_change, aes(Year, Contribution)) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
  geom_col(aes(fill = VEMF), position = "stack", width = 4,
           color = "white", linewidth = 0.2) +
  geom_line(data = df_primary_change_total,
            aes(Year, TotalChange, color = "Total primary energy", group = 1),
            linewidth = 1.1) +
  geom_point(data = df_primary_change_total,
             aes(Year, TotalChange, color = "Total primary energy"),
             shape = 21, fill = "white", size = 3, stroke = 1) +
  scale_fill_manual(name = "Energy source", values = col_primary,
                    breaks = PrimaryEnergyOrder,
                    guide = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_color_manual(name = NULL, values = c("Total primary energy"="black")) +
  scale_x_continuous(limits = c(2018,2052), breaks = c(2020,2030,2040,2050)) +
  scale_y_continuous(labels = label_number(accuracy = 0.1, suffix = "%"),
                     expand = expansion(mult = c(0.08,0.08))) +
  facet_wrap(~REMF, nrow = 1, scales = "fixed") +
  labs(title = "Change in primary energy induced by revenue transfers",
       x = NULL,
       y = "Contribution to primary-energy change\n(Aid − Def, % of primary energy in Def)") +
  theme_energy

plot(g_primary_change)

ggsave(file.path(output_dir, "primary_energy_absolute_trend.png"),
       g_primary_trend, width = 13, height = 8, dpi = 600)
ggsave(file.path(output_dir, "primary_energy_absolute_trend.pdf"),
       g_primary_trend, width = 13, height = 8)

ggsave(file.path(output_dir, "primary_energy_change.png"),
       g_primary_change, width = 13, height = 6.5, dpi = 600)
ggsave(file.path(output_dir, "primary_energy_change.pdf"),
       g_primary_change, width = 13, height = 6.5)

# Car_Seq_CDR ------------------------
vec_cdr <- c("Car_Seq_Enh_Wea","Car_Seq_Lan_Use",
             "Car_Seq_CCS_Bio","Car_Seq_Dir_Air_Cap")

CDROrder <- c("Land-use removal","BECCS","DACCS","Enhanced weathering")

col_cdr <- c("Land-use removal"="#9ACD8C","BECCS"="#3E7C59",
             "DACCS"="#7E6AA2","Enhanced weathering"="#C49A6C")

CLP <- c("SSP2_400C_2030CP_NoCC_No","SSP2_400C_2030CP_15th_NoCC_No")
Region <- c("Rprovider15th","Rrecipient15th")

df_cdr <- rgdx.param(gdx_file, "IAMC_template") %>%
  filter(VEMF %in% vec_cdr, SCENARIO %in% CLP, REMF %in% Region) %>%
  transmute(
    Year = as.numeric(as.character(YEMF)),
    REMF = recode(REMF, "Rprovider15th"="Provider", "Rrecipient15th"="Recipient"),
    SCENARIO = recode(SCENARIO, "SSP2_400C_2030CP_NoCC_No"="Def",
                     "SSP2_400C_2030CP_15th_NoCC_No"="Aid"),
    VEMF = recode(VEMF, "Car_Seq_Enh_Wea"="Enhanced weathering",
                  "Car_Seq_Lan_Use"="Land Use",
                  "Car_Seq_CCS_Bio"="BECCS",
                  "Car_Seq_Dir_Air_Cap"="DACCS"),
    # Original unit: MtCO2/yr; converted to GtCO2/yr
    value = as.numeric(IAMC_Template)/1000
  ) %>%
  filter(Year >= 2020, Year <= 2050) %>%
  group_by(REMF, SCENARIO, Year, VEMF) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    REMF = factor(REMF, levels = c("Provider","Recipient")),
    SCENARIO = factor(SCENARIO, levels = c("Def","Aid")),
    VEMF = factor(VEMF, levels = CDROrder)
  ) %>%
  complete(REMF, SCENARIO, Year, VEMF, fill = list(value = 0))

## 1. Absolute CDR trend -------------------------------------------------

df_cdr_total <- df_cdr %>%
  group_by(REMF, SCENARIO, Year) %>%
  summarise(TotalCDR = sum(value, na.rm = TRUE), .groups = "drop")

g_cdr_trend <- ggplot(df_cdr, aes(Year, value, fill = VEMF)) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
  geom_area(position = "stack", color = "white", linewidth = 0.15) +
  geom_line(data = df_cdr_total,
            aes(Year, TotalCDR, color = "Total CDR", group = 1),
            inherit.aes = FALSE, linewidth = 1) +
  scale_fill_manual(name = "CDR technology", values = col_cdr,
                    breaks = CDROrder,
                    guide = guide_legend(nrow = 1, byrow = TRUE)) +
  scale_color_manual(name = NULL, values = c("Total CDR"="black")) +
  scale_x_continuous(limits = c(2020,2050), breaks = c(2020,2030,2040,2050),
                     expand = expansion(mult = c(0,0))) +
  scale_y_continuous(labels = label_number(accuracy = 0.1),
                     expand = expansion(mult = c(0,0.05))) +
  facet_grid(REMF ~ SCENARIO, scales = "free_y") +
  labs(title = "Carbon dioxide removal by technology",
       x = NULL,
       y = expression(CDR~"(GtCO"[2]*"/yr)")) +
  theme_energy

plot(g_cdr_trend)

## 2. Aid − Def change ---------------------------------------------------

df_cdr_change <- df_cdr %>%
  pivot_wider(names_from = SCENARIO, values_from = value, values_fill = 0) %>%
  mutate(Change = Aid-Def) %>%
  group_by(REMF, Year) %>%
  mutate(TotalChange = sum(Change, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(is.finite(Change), is.finite(TotalChange))

df_cdr_change_total <- df_cdr_change %>%
  distinct(REMF, Year, TotalChange)

g_cdr_change <- ggplot(df_cdr_change, aes(Year, Change)) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
  geom_col(aes(fill = VEMF), position = "stack", width = 4,
           color = "white", linewidth = 0.2) +
  geom_line(data = df_cdr_change_total,
            aes(Year, TotalChange, color = "Total CDR change", group = 1),
            linewidth = 1.1) +
  geom_point(data = df_cdr_change_total,
             aes(Year, TotalChange, color = "Total CDR change"),
             shape = 21, fill = "white", size = 3, stroke = 1) +
  scale_fill_manual(name = "CDR technology", values = col_cdr,
                    breaks = CDROrder,
                    guide = guide_legend(nrow = 1, byrow = TRUE)) +
  scale_color_manual(name = NULL, values = c("Total CDR change"="black")) +
  scale_x_continuous(limits = c(2018,2052), breaks = c(2020,2030,2040,2050)) +
  scale_y_continuous(labels = label_number(accuracy = 0.1),
                     expand = expansion(mult = c(0.08,0.08))) +
  facet_wrap(~REMF, nrow = 1, scales = "fixed") +
  labs(title = "Change in carbon dioxide removal induced by revenue transfers",
       x = NULL,
       y = expression(Delta~CDR~"(Aid - Def, GtCO"[2]*"/yr)")) +
  theme_energy

plot(g_cdr_change)

ggsave(file.path(output_dir, "cdr_absolute_trend.png"),
       g_cdr_trend, width = 13, height = 8, dpi = 600)
ggsave(file.path(output_dir, "cdr_absolute_trend.pdf"),
       g_cdr_trend, width = 13, height = 8)

ggsave(file.path(output_dir, "cdr_change.png"),
       g_cdr_change, width = 13, height = 6.5, dpi = 600)
ggsave(file.path(output_dir, "cdr_change.pdf"),
       g_cdr_change, width = 13, height = 6.5)

# Car_Seq_CCS --------------------------
vec_ccs <- c("Car_Seq_CCS_Fos","Car_Seq_CCS_Ind_Pro",
             "Car_Seq_CCS_Bio","Car_Seq_Dir_Air_Cap")

CCSOrder <- c("Fossil CCS","Industrial process CCS","BECCS","DACCS")

col_ccs <- c("Fossil CCS"="#6C757D","Industrial process CCS"="#4E79A7",
             "BECCS"="#3E7C59","DACCS"="#7E6AA2")

CLP <- c("SSP2_400C_2030CP_NoCC_No","SSP2_400C_2030CP_15th_NoCC_No")
Region <- c("Rprovider15th","Rrecipient15th")

df_ccs <- rgdx.param(gdx_file, "IAMC_template") %>%
  filter(VEMF %in% vec_ccs, SCENARIO %in% CLP, REMF %in% Region) %>%
  transmute(
    Year = as.numeric(as.character(YEMF)),
    REMF = recode(REMF, "Rprovider15th"="Provider", "Rrecipient15th"="Recipient"),
    SCENARIO = recode(SCENARIO, "SSP2_400C_2030CP_NoCC_No"="Def",
                     "SSP2_400C_2030CP_15th_NoCC_No"="Aid"),
    VEMF = recode(VEMF, "Car_Seq_CCS_Fos"="Fossil CCS",
                  "Car_Seq_CCS_Ind_Pro"="Industrial process CCS",
                  "Car_Seq_CCS_Bio"="BECCS",
                  "Car_Seq_Dir_Air_Cap"="DACCS"),
    # Original unit: MtCO2/yr; converted to GtCO2/yr
    value = as.numeric(IAMC_Template)/1000
  ) %>%
  filter(Year >= 2020, Year <= 2050) %>%
  group_by(REMF, SCENARIO, Year, VEMF) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    REMF = factor(REMF, levels = c("Provider","Recipient")),
    SCENARIO = factor(SCENARIO, levels = c("Def","Aid")),
    VEMF = factor(VEMF, levels = CCSOrder)
  ) %>%
  complete(REMF, SCENARIO, Year, VEMF, fill = list(value = 0))

## 1. Absolute CCS trend -------------------------------------------------

df_ccs_total <- df_ccs %>%
  group_by(REMF, SCENARIO, Year) %>%
  summarise(TotalCCS = sum(value, na.rm = TRUE), .groups = "drop")

g_ccs_trend <- ggplot(df_ccs, aes(Year, value, fill = VEMF)) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
  geom_area(position = "stack", color = "white", linewidth = 0.15) +
  geom_line(data = df_ccs_total,
            aes(Year, TotalCCS, color = "Total captured and stored", group = 1),
            inherit.aes = FALSE, linewidth = 1) +
  scale_fill_manual(name = "Capture type", values = col_ccs, breaks = CCSOrder,
                    guide = guide_legend(nrow = 1, byrow = TRUE)) +
  scale_color_manual(name = NULL,
                     values = c("Total captured and stored"="black")) +
  scale_x_continuous(limits = c(2020,2050), breaks = c(2020,2030,2040,2050),
                     expand = expansion(mult = c(0,0))) +
  scale_y_continuous(labels = label_number(accuracy = 0.1),
                     expand = expansion(mult = c(0,0.05))) +
  facet_grid(REMF ~ SCENARIO, scales = "free_y") +
  labs(title = "Carbon capture and storage by capture type",
       x = NULL,
       y = "Carbon captured and stored (GtCO₂/yr)") +
  theme_energy

plot(g_ccs_trend)

## 2. Aid − Def absolute change -----------------------------------------

df_ccs_change <- df_ccs %>%
  pivot_wider(names_from = SCENARIO, values_from = value, values_fill = 0) %>%
  mutate(Change = Aid-Def) %>%
  group_by(REMF, Year) %>%
  mutate(TotalChange = sum(Change, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(is.finite(Change), is.finite(TotalChange))

df_ccs_change_total <- df_ccs_change %>%
  distinct(REMF, Year, TotalChange)

g_ccs_change <- ggplot(df_ccs_change, aes(Year, Change)) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
  geom_col(aes(fill = VEMF), position = "stack", width = 4,
           color = "white", linewidth = 0.2) +
  geom_line(data = df_ccs_change_total,
            aes(Year, TotalChange, color = "Total CCS change", group = 1),
            linewidth = 1.1) +
  geom_point(data = df_ccs_change_total,
             aes(Year, TotalChange, color = "Total CCS change"),
             shape = 21, fill = "white", size = 3, stroke = 1) +
  scale_fill_manual(name = "Capture type", values = col_ccs, breaks = CCSOrder,
                    guide = guide_legend(nrow = 1, byrow = TRUE)) +
  scale_color_manual(name = NULL, values = c("Total CCS change"="black")) +
  scale_x_continuous(limits = c(2018,2052), breaks = c(2020,2030,2040,2050)) +
  scale_y_continuous(labels = label_number(accuracy = 0.1),
                     expand = expansion(mult = c(0.08,0.08))) +
  facet_wrap(~REMF, nrow = 1, scales = "fixed") +
  labs(title = "Change in carbon capture and storage induced by revenue transfers",
       x = NULL,
       y = "Change in captured and stored CO₂\n(Aid − Def, GtCO₂/yr)") +
  theme_energy

plot(g_ccs_change)

ggsave(file.path(output_dir, "ccs_absolute_trend.png"),
       g_ccs_trend, width = 13, height = 8, dpi = 600)
ggsave(file.path(output_dir, "ccs_absolute_trend.pdf"),
       g_ccs_trend, width = 13, height = 8)

ggsave(file.path(output_dir, "ccs_change.png"),
       g_ccs_change, width = 13, height = 6.5, dpi = 600)
ggsave(file.path(output_dir, "ccs_change.pdf"),
       g_ccs_change, width = 13, height = 6.5)

# Emission -----------------------------------
vec_emissions <- c("Emi_CO2_AFO","Emi_CO2_Ene_Sup","Emi_CO2_Ene_Dem",
                   "Emi_CO2_Ind_Pro","Emi_CO2_Pro_Use","Emi_CO2_Cap_and_Rem")

CO2Order <- c("Energy Supply","Energy Demand","Industrial Processes",
              "Product Use","AFOLU","Capture and removal")

col_emissions <- c("Energy Supply"="#4E9F85","Energy Demand"="#4E79A7",
                   "Industrial Processes"="#8E6C8A","Product Use"="#E3B448",
                   "AFOLU"="#E58E65","Capture and removal"="#3E6B89")

CLP <- c("SSP2_400C_2030CP_NoCC_No","SSP2_400C_2030CP_15th_NoCC_No")
Region <- c("Rprovider15th","Rrecipient15th")

df_emissions <- rgdx.param(gdx_file, "IAMC_template") %>%
  filter(VEMF %in% vec_emissions, SCENARIO %in% CLP, REMF %in% Region) %>%
  transmute(
    Year = as.numeric(as.character(YEMF)),
    REMF = recode(REMF, "Rprovider15th"="Provider", "Rrecipient15th"="Recipient"),
    SCENARIO = recode(SCENARIO, "SSP2_400C_2030CP_NoCC_No"="Def",
                     "SSP2_400C_2030CP_15th_NoCC_No"="Aid"),
    VEMF = recode(VEMF, "Emi_CO2_AFO"="AFOLU",
                  "Emi_CO2_Ene_Sup"="Energy Supply",
                  "Emi_CO2_Ene_Dem"="Energy Demand",
                  "Emi_CO2_Ind_Pro"="Industrial Processes",
                  "Emi_CO2_Pro_Use"="Product Use",
                  "Emi_CO2_Cap_and_Rem"="Capture and removal"),
    # Original unit: MtCO2/yr; converted to GtCO2/yr
    value = as.numeric(IAMC_Template)/1000
  ) %>%
  filter(Year >= 2020, Year <= 2050) %>%
  group_by(REMF, SCENARIO, Year, VEMF) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    REMF = factor(REMF, levels = c("Provider","Recipient")),
    SCENARIO = factor(SCENARIO, levels = c("Def","Aid")),
    VEMF = factor(VEMF, levels = CO2Order)
  ) %>%
  complete(REMF, SCENARIO, Year, VEMF, fill = list(value = 0))

## 1. Absolute CO2-emissions trend ---------------------------------------

df_emissions_total <- df_emissions %>%
  group_by(REMF, SCENARIO, Year) %>%
  summarise(NetCO2 = sum(value, na.rm = TRUE), .groups = "drop")

g_emissions_trend <- ggplot(df_emissions, aes(Year, value, fill = VEMF)) +
  geom_hline(yintercept = 0, color = "grey30", linewidth = 0.45) +
  geom_area(position = "stack", color = "white", linewidth = 0.15) +
  geom_line(data = df_emissions_total,
            aes(Year, NetCO2, color = "Net CO2 emissions", group = 1),
            inherit.aes = FALSE, linewidth = 1.1) +
  scale_fill_manual(name = "Emission source", values = col_emissions,
                    breaks = CO2Order,
                    guide = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_color_manual(name = NULL, values = c("Net CO2 emissions"="black")) +
  scale_x_continuous(limits = c(2020,2050), breaks = c(2020,2030,2040,2050),
                     expand = expansion(mult = c(0,0))) +
  scale_y_continuous(labels = label_number(accuracy = 0.1),
                     expand = expansion(mult = c(0.05,0.05))) +
  facet_grid(REMF ~ SCENARIO, scales = "free_y") +
  labs(title = "CO₂ emissions by source",
       x = NULL, y = "CO₂ emissions (GtCO₂/yr)") +
  theme_energy

plot(g_emissions_trend)

## 2. Aid − Def absolute change -----------------------------------------

df_emissions_change <- df_emissions %>%
  pivot_wider(names_from = SCENARIO, values_from = value, values_fill = 0) %>%
  mutate(Change = Aid-Def) %>%
  group_by(REMF, Year) %>%
  mutate(TotalChange = sum(Change, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(is.finite(Change), is.finite(TotalChange))

df_emissions_change_total <- df_emissions_change %>%
  distinct(REMF, Year, TotalChange)

g_emissions_change <- ggplot(df_emissions_change, aes(Year, Change)) +
  geom_hline(yintercept = 0, color = "grey30", linewidth = 0.45) +
  geom_col(aes(fill = VEMF), position = "stack", width = 4,
           color = "white", linewidth = 0.2) +
  geom_line(data = df_emissions_change_total,
            aes(Year, TotalChange, color = "Net emissions change", group = 1),
            linewidth = 1.1) +
  geom_point(data = df_emissions_change_total,
             aes(Year, TotalChange, color = "Net emissions change"),
             shape = 21, fill = "white", size = 3, stroke = 1) +
  scale_fill_manual(name = "Emission source", values = col_emissions,
                    breaks = CO2Order,
                    guide = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_color_manual(name = NULL, values = c("Net emissions change"="black")) +
  scale_x_continuous(limits = c(2018,2052), breaks = c(2020,2030,2040,2050)) +
  scale_y_continuous(labels = label_number(accuracy = 0.1),
                     expand = expansion(mult = c(0.08,0.08))) +
  facet_wrap(~REMF, nrow = 1, scales = "fixed") +
  labs(title = "Change in CO₂ emissions induced by revenue transfers",
       x = NULL,
       y = "Change in CO₂ emissions\n(Aid − Def, GtCO₂/yr)") +
  theme_energy

plot(g_emissions_change)

ggsave(file.path(output_dir, "co2_emissions_absolute_trend.png"),
       g_emissions_trend, width = 13, height = 8, dpi = 600)
ggsave(file.path(output_dir, "co2_emissions_absolute_trend.pdf"),
       g_emissions_trend, width = 13, height = 8)

ggsave(file.path(output_dir, "co2_emissions_change.png"),
       g_emissions_change, width = 13, height = 6.5, dpi = 600)
ggsave(file.path(output_dir, "co2_emissions_change.pdf"),
       g_emissions_change, width = 13, height = 6.5)