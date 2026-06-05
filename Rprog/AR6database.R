# Data format -------------------------------------------------------------

ls_Category <- read_xlsx(paste0('AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx'),'meta_Ch3vetted_withclimate') %>%
  transmute(Case=paste0(Model,'_',Scenario),Model,Scenario,Category)
# ! please download AR6 Scenarios Database world v1.1 & place it in the data directory
df_load_AR6 <- read_csv(paste0('AR6_Scenarios_Database_World_v1.1.csv')) %>%
  mutate(Case=paste0(Model,'_',Scenario)) %>%
  full_join(ls_Category) %>%
  mutate(Case=paste0(Model,'-',Scenario)) %>%
  filter(Category%in%c('C1','C2')) %>%
  select(-Unit,-Model,-Scenario) %>%
  pivot_longer(cols=-c(Case,Category,Region,Variable),
               names_to='Year',values_to='value',names_transform=as.numeric)%>%
  #filter(str_detect(Variable, "Emissions|CO2"))%>%
  filter(!is.na(value))  %>%
  #filter(Year == 2100)  %>% 
  select(-Region) %>%
  filter(Variable == "Emissions|CO2")


# vetting ------------------------------------------------------------

p_vetting <- df_load_AR6 %>%
  ggplot(aes(x = Year, y = value, group = Case, color = Category)) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey50") +
  geom_line(alpha = 0.25, linewidth = 0.4) +
  stat_summary(
    aes(group = Category),
    fun = median,
    geom = "line",
    linewidth = 1.3
  ) +
  #facet_wrap(~ Category) +
  labs(
    x = "Year",
    y = expression(CO[2]~emissions),
    color = "Category",
    title = "AR6 C1 and C2 CO2 emissions pathways"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

#Emi_CO2---------------------------------------------------------
df_ar6_range <- df_load_AR6 %>%
  mutate(value = value / 1000) %>%
  group_by(Category, Year) %>%
  summarise(
    p05 = quantile(value, 0.05, na.rm = TRUE),
    p25 = quantile(value, 0.25, na.rm = TRUE),
    p50 = median(value, na.rm = TRUE),
    p75 = quantile(value, 0.75, na.rm = TRUE),
    p95 = quantile(value, 0.95, na.rm = TRUE),
    .groups = "drop"
  )%>%
  filter(Year <= 2050)


# scenarios
CLP <- c(
  "SSP2_400C_2030CP_base_NoCC_No",
  "SSP2_400C_2030CP_POP_NoCC_No"
)
Region <- c("World")
thema <- "Emi_CO2"

df_scen <- rgdx.param("global_17_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF == thema) %>%
  filter(SCENARIO %in% CLP) %>%
  filter(REMF %in% Region) %>%
  mutate(
    SCENARIO = case_when(
      SCENARIO == "SSP2_400C_2030CP_base_NoCC_No" ~ "NonAid",
      SCENARIO == "SSP2_400C_2030CP_POP_NoCC_No" ~ "Aid",
      TRUE ~ SCENARIO
    ),
    Year = as.numeric(as.character(YEMF)),
    value = as.numeric(IAMC_Template)/1000
  ) %>%
  filter(Year <= 2050)

# Plot
g <- ggplot() +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey50") +

  # AR6 range: 5–95% range
  geom_ribbon(
    data = df_ar6_range %>% filter(Year <= 2050),
    aes(x = Year, ymin = p05, ymax = p95, fill = Category),
    alpha = 0.18
  ) +

  # AR6 range: 25–75% range
  geom_ribbon(
    data = df_ar6_range %>% filter(Year <= 2050),
    aes(x = Year, ymin = p25, ymax = p75, fill = Category),
    alpha = 0.35
  ) +

  # Your scenarios
  geom_line(
    data = df_scen %>% filter(Year <= 2050),
    aes(x = Year, y = value, group = SCENARIO, linetype = SCENARIO),
    color = "black",
    linewidth = 1.2,
    show.legend = FALSE
  ) +

  scale_fill_manual(
    values = c(
      C1 = "#4E79A7",
      C2 = "#F28E2B"
    ),
    name = "AR6 category"
  ) +

  scale_color_manual(
    values = c(
      C1 = "#4E79A7",
      C2 = "#F28E2B"
    ),
    guide = "none"
  ) +

  scale_x_continuous(
    limits = c(2020, 2050),
    breaks = seq(2020, 2050, 5)
  ) +

  labs(
    x = "Year",
    y = expression(CO[2]~emissions~"(GtCO"[2]*"/yr)"),
    #title = "Comparison with AR6 C1 and C2 CO2 emissions ranges",
    fill = "AR6 category",
    color = NULL,
    linetype = NULL
  ) +

  theme_1 +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

g

name  <- "Emi_CO2.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 8,
  dpi = 600,
)

#Price|Carbon-----------------------------------------------

ls_Category <- read_xlsx(paste0('AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx'),'meta_Ch3vetted_withclimate') %>%
  transmute(Case=paste0(Model,'_',Scenario),Model,Scenario,Category)
# ! please download AR6 Scenarios Database world v1.1 & place it in the data directory
df_load_AR6 <- read_csv(paste0('AR6_Scenarios_Database_World_v1.1.csv')) %>%
  mutate(Case=paste0(Model,'_',Scenario)) %>%
  full_join(ls_Category) %>%
  mutate(Case=paste0(Model,'-',Scenario)) %>%
  filter(Category%in%c('C1','C2')) %>%
  select(-Unit,-Model,-Scenario) %>%
  pivot_longer(cols=-c(Case,Category,Region,Variable),
               names_to='Year',values_to='value',names_transform=as.numeric)%>%
  filter(!is.na(value))  %>%
  #filter(Year == 2100)  %>% 
  select(-Region) %>%
  filter(Variable == "Price|Carbon")


df_ar6_range <- df_load_AR6 %>%
  group_by(Category, Year) %>%
  summarise(
    p05 = quantile(value, 0.05, na.rm = TRUE),
    p25 = quantile(value, 0.25, na.rm = TRUE),
    p50 = median(value, na.rm = TRUE),
    p75 = quantile(value, 0.75, na.rm = TRUE),
    p95 = quantile(value, 0.95, na.rm = TRUE),
    .groups = "drop"
  )%>%
  filter(Year <= 2050)


# scenarios
CLP <- c(
  "SSP2_400C_2030CP_base_NoCC_No",
  "SSP2_400C_2030CP_POP_NoCC_No"
)
Region <- c("World")
thema <- "Prc_Car"

df_scen <- rgdx.param("global_17_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF == thema) %>%
  filter(SCENARIO %in% CLP) %>%
  filter(REMF %in% Region) %>%
  mutate(
    SCENARIO = case_when(
      SCENARIO == "SSP2_400C_2030CP_base_NoCC_No" ~ "NonAid",
      SCENARIO == "SSP2_400C_2030CP_POP_NoCC_No" ~ "Aid",
      TRUE ~ SCENARIO
    ),
    Year = as.numeric(as.character(YEMF)),
    value = as.numeric(IAMC_Template)
  ) %>%
  filter(Year <= 2050)

g <- ggplot() +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey50") +
  geom_ribbon(
    data = df_ar6_range,
    aes(x = Year, ymin = p05, ymax = p95, fill = Category),
    alpha = 0.18
  ) +
  geom_ribbon(
    data = df_ar6_range,
    aes(x = Year, ymin = p25, ymax = p75, fill = Category),
    alpha = 0.35
  ) +
  geom_line(
  data = df_scen,
  aes(
    x = Year,
    y = value,
    linetype = SCENARIO
  ),
  color = "black",
  linewidth = 1.2
  )+
  scale_fill_manual(
    values = c(
      C1 = "#4E79A7",
      C2 = "#F28E2B"
    ),
    name = "AR6 category"
  ) +
  scale_linetype_manual(
    values = c(
      NonAid = "solid",
      Aid = "dashed"
    ),
    name = "Scenario"
  ) +
  scale_x_continuous(
    breaks = seq(2020, 2050, 5)
  ) +
  coord_cartesian(xlim = c(2020, 2050)) +
  labs(
    x = "Year", y = "Carbon price (USD/tCO₂)" ) +
  guides(
  fill = guide_legend(order = 1),
  linetype = guide_legend(order = 2)
  )+
  theme_1 +
  theme(
  legend.position = "bottom",
  legend.key.width = unit(2.5, "cm"),
  panel.grid.minor = element_blank()
  )

g

name  <- "Prc_Car.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 8,
  dpi = 600,
)
