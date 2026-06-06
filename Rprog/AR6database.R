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

#Policy Cost|Consumption-----------------------------------------------
Place <- c("R5OECD90+EU", "R5ASIA", "R5LAM", "R5MAF", "R5REF")
thema <- "Pol_Cos_Cns_Los"

ls_Category <- read_xlsx(paste0('AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx'),'meta_Ch3vetted_withclimate') %>%
  transmute(Case=paste0(Model,'_',Scenario),Model,Scenario,Category)
# ! please download AR6 Scenarios Database world v1.1 & place it in the data directory
df_load_AR6 <- read_csv(paste0('AR6_Scenarios_Database_R5_regions_v1.1.csv')) %>%
  mutate(Case=paste0(Model,'_',Scenario)) %>%
  full_join(ls_Category) %>%
  mutate(Case=paste0(Model,'-',Scenario)) %>%
  filter(Category%in%c('C1','C2')) %>%
  select(-Unit,-Model,-Scenario) %>%
  pivot_longer(cols=-c(Case,Category,Region,Variable),
               names_to='Year',values_to='value',names_transform=as.numeric)%>%
  filter(!is.na(value))  %>%
  #filter(Year == 2100)  %>% 
  filter(Variable == "Policy Cost|Consumption Loss") %>%
  filter(Region %in% Place)


df_ar6_range <- df_load_AR6 %>%
  group_by(Category,Region, Year) %>%
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


df_scen <- rgdx.param("global_17_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF == thema) %>%
  filter(SCENARIO %in% CLP) %>%
  filter(REMF %in% Place) %>%
  mutate(
    SCENARIO = case_when(
      SCENARIO == "SSP2_400C_2030CP_base_NoCC_No" ~ "NonAid",
      SCENARIO == "SSP2_400C_2030CP_POP_NoCC_No" ~ "Aid",
      TRUE ~ SCENARIO
    ),
    Region = REMF,
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
    x = "Year", y = "Consumption loss (billion US$2010)" ) +
  guides(
  fill = guide_legend(order = 1),
  linetype = guide_legend(order = 2)
  )+
  facet_wrap(~Region)+
  theme_1 +
  theme(
  legend.position = "bottom",
  legend.key.width = unit(2, "cm"),
  panel.grid.minor = element_blank()
  )

plot(g)

name  <- "Pol_Cns.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 8,
  dpi = 600,
)

#Policy Cost|GDP-----------------------------------------------
Place <- c("R5OECD90+EU", "R5ASIA", "R5LAM", "R5MAF", "R5REF")
thema <- "Pol_Cos_GDP_Los"

ls_Category <- read_xlsx(paste0('AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx'),'meta_Ch3vetted_withclimate') %>%
  transmute(Case=paste0(Model,'_',Scenario),Model,Scenario,Category)
# ! please download AR6 Scenarios Database world v1.1 & place it in the data directory
df_load_AR6 <- read_csv(paste0('AR6_Scenarios_Database_R5_regions_v1.1.csv')) %>%
  mutate(Case=paste0(Model,'_',Scenario)) %>%
  full_join(ls_Category) %>%
  mutate(Case=paste0(Model,'-',Scenario)) %>%
  filter(Category%in%c('C1','C2')) %>%
  select(-Unit,-Model,-Scenario) %>%
  pivot_longer(cols=-c(Case,Category,Region,Variable),
               names_to='Year',values_to='value',names_transform=as.numeric)%>%
  filter(!is.na(value))  %>%
  #filter(Year == 2100)  %>% 
  filter(Variable == "Policy Cost|GDP Loss") %>%
  filter(Region %in% Place)


df_ar6_range <- df_load_AR6 %>%
  group_by(Category,Region, Year) %>%
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


df_scen <- rgdx.param("global_17_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF == thema) %>%
  filter(SCENARIO %in% CLP) %>%
  filter(REMF %in% Place) %>%
  mutate(
    SCENARIO = case_when(
      SCENARIO == "SSP2_400C_2030CP_base_NoCC_No" ~ "NonAid",
      SCENARIO == "SSP2_400C_2030CP_POP_NoCC_No" ~ "Aid",
      TRUE ~ SCENARIO
    ),
    Region = REMF,
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
    x = "Year", y = "GDP loss (billion US$2010)" ) +
  guides(
  fill = guide_legend(order = 1),
  linetype = guide_legend(order = 2)
  )+
  facet_wrap(~Region)+
  theme_1 +
  theme(
  legend.position = "bottom",
  legend.key.width = unit(2, "cm"),
  panel.grid.minor = element_blank()
  )

plot(g)

name  <- "Pol_GDP.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 8,
  dpi = 600,
)


#Trade of fossil energy in AR6------------------------------

Place <- c("R5OECD90+EU","R5ASIA","R5LAM","R5MAF","R5REF")
cols <- c("C1"="#2C7BB6","C2"="#D7191C")

vars_trade <- c("Trade|Primary Energy|Coal|Volume",
                "Trade|Primary Energy|Gas|Volume",
                "Trade|Primary Energy|Oil|Volume")

ls_Category <- read_xlsx("AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx",
                         "meta_Ch3vetted_withclimate") %>%
  transmute(Case=paste0(Model,"_",Scenario), Model, Scenario, Category)

df_trade_range <- read_csv("AR6_Scenarios_Database_R5_regions_v1.1.csv") %>%
  mutate(Case=paste0(Model,"_",Scenario)) %>%
  full_join(ls_Category, by=c("Case","Model","Scenario")) %>%
  mutate(Case=paste0(Model,"-",Scenario)) %>%
  filter(Category %in% c("C1","C2")) %>%
  select(-Unit,-Model,-Scenario) %>%
  pivot_longer(cols=-c(Case,Category,Region,Variable),
               names_to="Year", values_to="value", names_transform=as.numeric) %>%
  filter(!is.na(value), Variable %in% vars_trade, Region %in% Place,
         Year >= 2020, Year <= 2050) %>%
  mutate(Fuel=recode(Variable,
                     "Trade|Primary Energy|Coal|Volume"="Coal",
                     "Trade|Primary Energy|Gas|Volume"="Gas",
                     "Trade|Primary Energy|Oil|Volume"="Oil"),
         Fuel=factor(Fuel, levels=c("Coal","Gas","Oil")),
         Region=factor(Region, levels=Place)) %>%
  group_by(Category,Region,Fuel,Year) %>%
  summarise(p05=quantile(value,0.05,na.rm=TRUE),
            p50=median(value,na.rm=TRUE),
            p95=quantile(value,0.95,na.rm=TRUE),
            .groups="drop")

p <- ggplot(df_trade_range, aes(x=Year, color=Category, group=Category)) +
  geom_linerange(aes(ymin=p05, ymax=p95), linewidth=2.2, alpha=0.35,
                 position=position_dodge(width=1.5)) +
  geom_line(aes(y=p50), linewidth=0.9, position=position_dodge(width=1.5)) +
  geom_point(aes(y=p50), size=1.8, position=position_dodge(width=1.5)) +
  geom_hline(yintercept=0, linetype="dashed", colour="grey50") +
  facet_grid(Fuel~Region, scales="free_y") +
  scale_color_manual(values=cols) +
  scale_x_continuous(breaks=c(2020,2030,2040,2050)) +
  labs(x=NULL, y="Trade volume",
       title="Primary Energy Trade (EJ/yr)",
       color="Category") +
  theme_1 +
  theme(strip.background=element_blank(),
        panel.grid.minor=element_blank(),
        legend.position="bottom")

print(p)


#Fossil share of primary energy in AR6------------------------------

Place <- c("R5OECD90+EU","R5ASIA","R5LAM","R5MAF","R5REF")
cols <- c("C1"="#2C7BB6","C2"="#D7191C")

ls_Category <- read_xlsx(
  "AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx",
  "meta_Ch3vetted_withclimate"
) %>%
  transmute(Case=paste0(Model,"_",Scenario), Model, Scenario, Category)

df_share <- read_csv("AR6_Scenarios_Database_R5_regions_v1.1.csv") %>%
  mutate(Case=paste0(Model,"_",Scenario)) %>%
  full_join(ls_Category, by=c("Case","Model","Scenario")) %>%
  mutate(Case=paste0(Model,"-",Scenario)) %>%
  filter(Category %in% c("C1","C2")) %>%
  select(-Unit,-Model,-Scenario) %>%
  pivot_longer(cols=-c(Case,Category,Region,Variable),
               names_to="Year",
               values_to="value",
               names_transform=as.numeric) %>%
  filter(!is.na(value),
         Region %in% Place,
         Variable %in% c("Primary Energy",
                         "Primary Energy|Fossil"),
         Year >= 2020,
         Year <= 2050) %>%
  pivot_wider(names_from=Variable,
              values_from=value) %>%
  mutate(
    fossil_share=100*`Primary Energy|Fossil`/`Primary Energy`
  ) %>%
  filter(is.finite(fossil_share))

df_range <- df_share %>%
  group_by(Category,Region,Year) %>%
  summarise(
    p05=quantile(fossil_share,0.05,na.rm=TRUE),
    p50=median(fossil_share,na.rm=TRUE),
    p95=quantile(fossil_share,0.95,na.rm=TRUE),
    .groups="drop"
  )

p <- ggplot(df_range,
            aes(Year,p50,color=Category,fill=Category)) +
  geom_ribbon(aes(ymin=p05,ymax=p95),
              alpha=0.18,
              color=NA) +
  geom_line(linewidth=1) +
  facet_wrap(~Region,nrow=1) +
  scale_color_manual(values=cols) +
  scale_fill_manual(values=cols) +
  scale_x_continuous(breaks=c(2020,2030,2040,2050)) +
  labs(
    x=NULL,
    y="Fossil share of primary energy (%)",
    title="Primary Energy|Fossil / Primary Energy",
    subtitle="Lines show median; shading shows 90% range",
    color="Category",
    fill="Category"
  ) +
  theme_bw() +
  theme(
    strip.background=element_blank(),
    panel.grid.minor=element_blank(),
    legend.position="bottom"
  )

print(p)