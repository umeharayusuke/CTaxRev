#setting------------
library(tidyverse)
library(dplyr)
library(ggplot2)
library(gdxrrw)
library(stringr)
library(gridExtra)
library(patchwork)
library(cowplot)
library(lemon)
library(purrr)
library(rnaturalearthdata)
library(rnaturalearth)
library(ggnewscale)
library(readxl)
library(scales)

theme_1 <- theme_bw() +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, size = 16, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        legend.position = "right", 
        #legend.title = element_blank(),
        strip.background = element_blank())

Mytheme <- theme_bw()+
  theme(text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, size = 16, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        legend.position = "right", 
        legend.title = element_blank(),
            panel.grid = element_blank())
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


## vetting ------------------------------------------------------------

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
  "SSP2_BaU_NoCC_No",
  "SSP2_400C_2030CP_15th_NoCC_No"
)
Region <- c("World")
thema <- "Emi_CO2"

df_scen <- rgdx.param("global_17_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF == thema) %>%
  filter(SCENARIO %in% CLP) %>%
  filter(REMF %in% Region) %>%
  mutate(
    SCENARIO = case_when(
      SCENARIO == "SSP2_BaU_NoCC_No" ~ "BaU",
      SCENARIO == "SSP2_400C_2030CP_15th_NoCC_No" ~ "15C",
      TRUE ~ SCENARIO
    ),
    Year = as.numeric(as.character(YEMF)),
    value = as.numeric(IAMC_Template)/1000
  ) %>%
  filter(Year <= 2050)

# Plot
df_ar6_box <- df_ar6_range %>%
  filter(Year %in% seq(2020, 2050, 5)) %>%
  mutate(PlotYear = Year + if_else(Category == "C1", -0.7, 0.7))

g <- ggplot() +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey50") +
  geom_boxplot(data = df_ar6_box, aes(x = PlotYear, ymin = p05, lower = p25, middle = p50, upper = p75, ymax = p95, fill = Category, group = interaction(Year, Category)), stat = "identity", width = 1.15, alpha = 0.6, color = "grey25", linewidth = 0.45) +
  geom_line(data = df_scen %>% filter(Year <= 2050), aes(x = Year, y = value, group = SCENARIO, color = SCENARIO), linewidth = 1.2) +
  geom_point(data = df_scen %>% filter(Year <= 2050), aes(x = Year, y = value, color = SCENARIO), size = 2.8) +
  scale_fill_manual(name = "AR6 category", values = c("C1" = "#4E79A7", "C2" = "#F28E2B")) +
  scale_color_discrete(name = "AIM scenario") +
  scale_x_continuous(limits = c(2018.5, 2051.5), breaks = seq(2020, 2050, 5), labels = seq(2020, 2050, 5)) +
  labs(x = "Year", y = expression(CO[2]~emissions~"(GtCO"[2]*"/yr)")) +
  Mytheme +
  theme(legend.position = "bottom", legend.box = "vertical", legend.title = element_text(size = 12), panel.grid.minor = element_blank())

plot(g)

name  <- "Emi_CO2.png"
output_dir <- file.path("../..", "output/Figure")

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
  summarise(p05 = quantile(value, 0.05, na.rm = TRUE), p25 = quantile(value, 0.25, na.rm = TRUE), p50 = median(value, na.rm = TRUE), p75 = quantile(value, 0.75, na.rm = TRUE), p95 = quantile(value, 0.95, na.rm = TRUE), .groups = "drop") %>%
  filter(Year <= 2050)

df_ar6_box <- df_ar6_range %>%
  filter(Year %in% seq(2020, 2050, 5)) %>%
  mutate(PlotYear = Year + if_else(Category == "C1", -0.7, 0.7))

CLP <- c("SSP2_400C_2030CP_NoCC_No", "SSP2_400C_2030CP_15th_NoCC_No")
Region <- "World"
thema <- "Prc_Car"

df_scen <- rgdx.param("global_17_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF == thema, SCENARIO %in% CLP, REMF %in% Region) %>%
  mutate(SCENARIO = case_when(SCENARIO == "SSP2_400C_2030CP_NoCC_No" ~ "400C_def", SCENARIO == "SSP2_400C_2030CP_15th_NoCC_No" ~ "400C_aid", TRUE ~ SCENARIO), Year = as.numeric(as.character(YEMF)), value = as.numeric(IAMC_Template)) %>%
  filter(Year <= 2050)

g <- ggplot() +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey50") +
  geom_boxplot(data = df_ar6_box, aes(x = PlotYear, ymin = p05, lower = p25, middle = p50, upper = p75, ymax = p95, fill = Category, group = interaction(Year, Category)), stat = "identity", width = 1.15, alpha = 0.6, color = "grey25", linewidth = 0.45) +
  geom_line(data = df_scen, aes(x = Year, y = value, group = SCENARIO, color = SCENARIO), linewidth = 1.2) +
  geom_point(data = df_scen, aes(x = Year, y = value, color = SCENARIO), size = 2.8) +
  scale_fill_manual(name = "AR6 category", values = c("C1" = "#4E79A7", "C2" = "#F28E2B")) +
  scale_color_discrete(name = "Model") +
  scale_x_continuous(limits = c(2018.5, 2051.5), breaks = seq(2020, 2050, 5), labels = seq(2020, 2050, 5)) +
  labs(x = "Year", y = expression("Carbon price (USD/tCO"[2]*")")) +
  guides(fill = guide_legend(order = 1), color = guide_legend(order = 2)) +
  Mytheme +
  theme(legend.position = "bottom", legend.box = "vertical", legend.title = element_text(size = 12), panel.grid.minor = element_blank())

plot(g)

name  <- "Prc_Car.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 8,
  dpi = 600,
)

#GDPcap---------------------

CLP <- "SSP2_BaU_NoCC_No"
thema <- "GDP_per_cap"
Region <- c("XE25","JPN","IND","TUR","CHN","USA","XER","XOC","XSE","XSA","CAN","BRA","XLM","CIS","XME","XNF","XAF")
Developing <- c("XSA","IND","XNF","XAF","XSE","BRA","XME")
Developed <- setdiff(Region, Developing)

DevelopedColor <- c(
  "XE25"="#0072B2",
  "JPN" ="#009E73",
  "TUR" ="#56B4E9",
  "CHN" ="#6A3D9A",
  "USA" ="#1B9E77",
  "XER" ="#377EB8",
  "XOC" ="#984EA3",
  "CAN" ="#4DAF4A",
  "XLM" ="#00A6D6",
  "CIS" ="#2F4858"
)

DevelopingColor <- c(
  "XSA"="#D55E00",
  "IND"="#E69F00",
  "XNF"="#F0E442",
  "XAF"="#CC79A7",
  "XSE"="#E41A1C",
  "BRA"="#A65628",
  "XME"="#F781BF"
)
DevelopedColor <- c(
  "XE25" = "#3B6FB6",
  "JPN"  = "#5AA6C8",
  "TUR"  = "#3F8F83",
  "CHN"  = "#6B5B95",
  "USA"  = "#2C7FB8",
  "XER"  = "#74A89A",
  "XOC"  = "#8C78B8",
  "CAN"  = "#4F9DA6",
  "XLM"  = "#5E7A91",
  "CIS"  = "#78A65A"
)

DevelopingColor <- c(
  "XSA" = "#C44E52",
  "IND" = "#D98C3F",
  "XNF" = "#D4B43C",
  "XAF" = "#A64D79",
  "XSE" = "#E06B65",
  "BRA" = "#8F5A70",
  "XME" = "#9A6A4F"
)
df <- rgdx.param("global_17_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF == thema, SCENARIO %in% CLP, REMF %in% Region) %>%
  mutate(SCENARIO = gsub("SSP2_BaU_NoCC_No", "BaU", SCENARIO),
         Panel = if_else(REMF %in% Developing, "Developing", "Developed"),
         Panel = factor(Panel, levels = c("Developed", "Developing")))

g <- ggplot(df, aes(x = YEMF, y = IAMC_Template)) +
  geom_hline(yintercept = 15, linetype = "dashed", linewidth = 0.4, color = "red", alpha = 0.5) +
  geom_line(data = filter(df, Panel == "Developed"), aes(group = REMF, color = REMF), linewidth = 1) +
  geom_point(data = filter(df, Panel == "Developed"), aes(color = REMF), size = 2) +
  scale_color_manual(name = "Developed regions", values = DevelopedColor, breaks = Developed,
                   guide = guide_legend(order = 1, nrow = 2, byrow = TRUE,
                                        override.aes = list(linewidth = 1.5, size = 3))) +
  ggnewscale::new_scale_color() +
  geom_line(data = filter(df, Panel == "Developing"), aes(group = REMF, color = REMF), linewidth = 1) +
  geom_point(data = filter(df, Panel == "Developing"), aes(color = REMF), size = 2) +
  scale_color_manual(name = "Developing regions", values = DevelopingColor, breaks = Developing,
                   guide = guide_legend(order = 2, nrow = 2, byrow = TRUE,
                                        override.aes = list(linewidth = 1.5, size = 3))) +
  facet_wrap(~Panel, scales = "fixed", ncol = 2) +
  scale_x_discrete(breaks = c("2020","2040","2060","2080","2100")) +
  ylab("GDP per capita (kUSD)") +
  Mytheme +
  theme(legend.position = "bottom", legend.box = "vertical", legend.box.just = "left",
        legend.title = element_text(size = 13, face = "bold"),
        strip.text = element_text(size = 14, face = "bold"))

plot(g)

name  <- "GDPcap.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 8,
  dpi = 600,
)
#Revenue-------------

thema <- "Rev_gov_Tax_Car_Tax"
CLP <- "SSP2_400C_2030CP_15th_NoCC_No"
Region <- c("XE25","JPN","TUR","CHN","USA","XER","XOC","CAN","XLM","CIS")

df <- rgdx.param("global_17_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF == thema, SCENARIO %in% CLP, REMF %in% Region) %>%
  mutate(
    SCENARIO = gsub("SSP2_400C_2030CP_15th_NoCC_No", "SSP2_400C_15k", SCENARIO),
    REMF = factor(REMF, levels = Region)
  )

g <- df %>%
  ggplot(aes(x = YEMF, y = IAMC_Template/1000000, fill = REMF)) +
  geom_col(position = "stack", width = 0.9) +
  scale_x_discrete(breaks = c("2030","2040","2050")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_brewer(palette = "Paired")+
  ylab("Carbon tax revenue provision\n(trillion US$2010)") +
  Mytheme +
  theme(legend.position = "bottom")

plot(g)

#Revenue2----------
thema <- "Rev_gov_Tax_Car_Tax"
CLP <- "SSP2_400C_2030CP_15th_NoCC_No"
Provider <- c("XE25","JPN","TUR","CHN","USA","XER","XOC","CAN","XLM","CIS")
Recipient <- c("XSA","IND","XNF","XAF","XSE","BRA","XME")
AllRegion <- c(Provider, Recipient)

iamc <- rgdx.param("global_17_IAMC.gdx", "IAMC_template") %>%
  filter(as.numeric(as.character(YEMF)) >= 2030)

df_provider <- iamc %>%
  filter(VEMF == thema, SCENARIO %in% CLP, REMF %in% Provider) %>%
  transmute(YEMF, SCENARIO, REMF, Type = "Provision", Amount = IAMC_Template/1000000)

total_revenue <- df_provider %>%
  group_by(YEMF, SCENARIO) %>%
  summarise(TotalRevenue = sum(Amount, na.rm = TRUE), .groups = "drop")

df_recipient <- iamc %>%
  filter(VEMF == "GDP_PPP", SCENARIO %in% CLP, REMF %in% Recipient) %>%
  group_by(YEMF, SCENARIO) %>%
  mutate(GDPShare = IAMC_Template/sum(IAMC_Template, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(total_revenue, by = c("YEMF","SCENARIO")) %>%
  transmute(YEMF, SCENARIO, REMF, Type = "Receipt", Amount = -TotalRevenue*GDPShare)

df_plot <- bind_rows(df_provider, df_recipient) %>%
  mutate(REMF = factor(REMF, levels = AllRegion))

ProviderColor <- c(
  "XE25" = "#3B6FB6",
  "JPN"  = "#5AA6C8",
  "TUR"  = "#3F8F83",
  "CHN"  = "#6B5B95",
  "USA"  = "#2C7FB8",
  "XER"  = "#74A89A",
  "XOC"  = "#8C78B8",
  "CAN"  = "#4F9DA6",
  "XLM"  = "#5E7A91",
  "CIS"  = "#78A65A"
)

RecipientColor <- c(
  "XSA" = "#C44E52",
  "IND" = "#D98C3F",
  "XNF" = "#D4B43C",
  "XAF" = "#A64D79",
  "XSE" = "#E06B65",
  "BRA" = "#8F5A70",
  "XME" = "#9A6A4F"
)

g <- ggplot() +
  geom_col(
    data = filter(df_plot, Type == "Provision"),
    aes(x = YEMF, y = Amount, fill = REMF),
    position = "stack", width = 0.9, color = "white", linewidth = 0.25
  ) +
  scale_fill_manual(
    name = "Provider regions (upper)",
    values = ProviderColor,
    breaks = Provider,
    guide = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  ggnewscale::new_scale_fill() +
  geom_col(
    data = filter(df_plot, Type == "Receipt"),
    aes(x = YEMF, y = Amount, fill = REMF),
    position = "stack", width = 0.9, color = "white", linewidth = 0.25
  ) +
  scale_fill_manual(
    name = "Recipient regions (lower)",
    values = RecipientColor,
    breaks = Recipient,
    guide = guide_legend(nrow = 1, byrow = TRUE)
  ) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
  scale_x_discrete(breaks = c("2030","2040","2050")) +
  scale_y_continuous(
    labels = function(x) abs(x),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  ylab("Carbon tax revenue\nprovision (upper) and receipt (lower)\n(trillion US$2010)") +
  Mytheme +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.box.just = "left",
    legend.title = element_text(size = 13, face = "bold")
  )

plot(g)

name  <- "Rev.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 8,
  dpi = 600,
)
#Revenue3-----------------
thema <- "Rev_gov_Tax_Car_Tax"
CLP <- "SSP2_400C_2030CP_15th_NoCC_No"
TargetYear <- "2050"
Provider <- c("XE25","JPN","TUR","CHN","USA","XER","XOC","CAN","XLM","CIS")
Recipient <- c("XSA","IND","XNF","XAF","XSE","BRA","XME")
AllRegion <- c(Provider, Recipient)

iamc <- rgdx.param("global_17_IAMC.gdx", "IAMC_template")

gdp_mer <- iamc %>%
  filter(VEMF == "GDP_MER", SCENARIO == CLP, REMF %in% AllRegion, as.character(YEMF) == TargetYear) %>%
  transmute(YEMF, SCENARIO, REMF, GDP_MER = IAMC_Template)

provider_raw <- iamc %>%
  filter(VEMF == thema, SCENARIO == CLP, REMF %in% Provider, as.character(YEMF) == TargetYear) %>%
  transmute(YEMF, SCENARIO, REMF, Revenue = IAMC_Template)

total_provider <- provider_raw %>%
  group_by(YEMF, SCENARIO) %>%
  summarise(TotalRevenue = sum(Revenue, na.rm = TRUE), .groups = "drop")

provider_pct <- provider_raw %>%
  left_join(gdp_mer, by = c("YEMF","SCENARIO","REMF")) %>%
  transmute(REMF, Type = "Provider", Percent = 100*Revenue/GDP_MER/1000) %>%
  filter(is.finite(Percent))

recipient_pct <- iamc %>%
  filter(VEMF == "GDP_PPP", SCENARIO == CLP, REMF %in% Recipient, as.character(YEMF) == TargetYear) %>%
  group_by(YEMF, SCENARIO) %>%
  mutate(GDPShare = IAMC_Template/sum(IAMC_Template, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(total_provider, by = c("YEMF","SCENARIO")) %>%
  transmute(YEMF, SCENARIO, REMF, Receipt = TotalRevenue*GDPShare) %>%
  left_join(gdp_mer, by = c("YEMF","SCENARIO","REMF")) %>%
  transmute(REMF, Type = "Recipient", Percent = -100*Receipt/GDP_MER/1000) %>%
  filter(is.finite(Percent))

ProviderOrder <- provider_pct %>% arrange(desc(Percent)) %>% pull(REMF) %>% as.character()
RecipientOrder <- recipient_pct %>% arrange(abs(Percent)) %>% pull(REMF) %>% as.character()

df_pct <- bind_rows(provider_pct, recipient_pct) %>%
  mutate(REMF = factor(REMF, levels = c(ProviderOrder, RecipientOrder)),
         Type = factor(Type, levels = c("Provider","Recipient")))

g <- ggplot(df_pct, aes(x = REMF, y = Percent, fill = Type)) +
  geom_col(width = 0.8, color = "white", linewidth = 0.3) +
  #geom_text(aes(label = sprintf("%.2f%%", abs(Percent)), vjust = if_else(Percent >= 0, -0.4, 1.4)), size = 3.5, show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
  geom_vline(xintercept = length(ProviderOrder) + 0.5, color = "grey50", linetype = "dashed", linewidth = 0.5) +
  scale_fill_manual(values = c("Provider" = "#3B6FB6", "Recipient" = "#C44E52"), name = NULL) +
  #scale_y_continuous(labels = function(x) scales::label_number(accuracy = 0.1, suffix = "%")(abs(x)), expand = expansion(mult = c(0.12, 0.12))) +
  labs(x = NULL, y = "Transfer relative to regional GDP (%)") +
  Mytheme +
  theme(legend.position = "bottom")

plot(g)
#Consumption--------
thema <- "Pol_Cos_Cns_Los_rat"
CLP <- c("SSP2_400C_2030CP_NoCC_No","SSP2_400C_2030CP_15th_NoCC_No")
Region <- c("R5OECD90+EU","R5MAF","R5REF","R5ASIA","R5LAM")
Region <- c("XE25","JPN","IND","TUR","CHN","USA","XER","XOC","XSE","XSA","CAN","BRA","XLM","CIS","XME","XNF","XAF")
Region <- c("Rprovider15th","Rrecipient15th")

df <- rgdx.param("global_17_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF == thema, SCENARIO %in% CLP, REMF %in% Region) %>%
  filter(as.numeric(as.character(YEMF)) >= 2020)%>%
  mutate(SCENARIO = recode(SCENARIO,
                           "SSP2_400C_2030CP_NoCC_No" = "400C-Def",
                           "SSP2_400C_2030CP_15th_NoCC_No" = "400C-Aid"),
         SCENARIO = factor(SCENARIO, levels = c("400C-Def","400C-Aid")),
         REMF = recode(REMF,
        "Rprovider15th" = "Provider",
        "Rrecipient15th" = "Recipient"),
         REMF = factor(REMF, levels = c("Provider","Recipient")))

g <- df %>%
  ggplot(aes(x = YEMF, y = IAMC_Template, group = SCENARIO, color = SCENARIO)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.4) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  facet_wrap(~REMF, scales = "fixed") +
  scale_x_discrete(breaks = c("2030","2040","2050")) +
  scale_color_discrete(name = "Scenario") +
  ylab("Consumption loss rate(%)") +
  Mytheme +
  theme(legend.position = "bottom")

plot(g)

#Cumulaive consumption loss-----------

thema <- "Pol_Cos_Cns_Los_rat_NPV_5pc"
CLP <- c("SSP2_400C_2030CP_NoCC_No","SSP2_400C_2030CP_15th_NoCC_No")
Region <- c("R5OECD90+EU","R5MAF","R5REF","R5ASIA","R5LAM")
Region <- c("Rprovider15th","Rrecipient15th")

df <- rgdx.param("global_17_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF == thema, SCENARIO %in% CLP, REMF %in% Region) %>%
  filter(as.numeric(as.character(YEMF)) == 2050)%>%
  mutate(SCENARIO = recode(SCENARIO,
                           "SSP2_400C_2030CP_NoCC_No" = "400C-Def",
                           "SSP2_400C_2030CP_15th_NoCC_No" = "400C-Aid"),
         SCENARIO = factor(SCENARIO, levels = c("400C-Def","400C-Aid")),
         REMF = recode(REMF,
        "Rprovider15th" = "Provider",
        "Rrecipient15th" = "Recipient"),
         REMF = factor(REMF, levels = c("Provider","Recipient")))

g <- df %>%
  ggplot(aes(x = REMF, y = IAMC_Template, group = SCENARIO, color = SCENARIO)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.4) +
  geom_point(size = 2.5) +
  #facet_wrap(~REMF, scales = "free_y") +
  #scale_x_discrete(breaks = c("2030","2040","2050")) +
  scale_color_discrete(name = "Scenario") +
  ylab("Cumulative Consumption loss rate(%)") +
  Mytheme +
  theme(legend.position = "bottom")

plot(g)


#Policy cost map------------------------------------------------------------
Region <- c("XE25","JPN","IND","TUR","CHN","USA","XER","XOC","XSE","XSA","CAN","BRA","XLM","CIS","XME","XNF","XAF")
CLP <- c("SSP2_400C_2030CP_NoCC_No","SSP2_400C_2030CP_15th_NoCC_No")

Indicator <- c("Pol_Cos_GDP_Los_rat_NPV_5pc")
Indicator <- c("Pol_Cos_Cns_Los_rat_NPV_5pc")

RegionmapRagg.map <- read.table("RegionmapRagg.map", header=FALSE, stringsAsFactors=FALSE)

Region_map <- RegionmapRagg.map %>%
  as_tibble() %>%
  select(iso3c=1, Region=3) %>%
  mutate(Region=str_remove_all(Region,'"'),
         Region=str_remove(Region,"^R17")) %>%
  filter(Region %in% Region)


df_cost <- rgdx.param("global_17_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF %in% Indicator) %>%
  filter(SCENARIO %in% CLP, REMF %in% Region) %>%
  mutate(
    SCENARIO = case_when(
      SCENARIO == "SSP2_400C_2030CP_NoCC_No" ~ "NonAid",
      SCENARIO == "SSP2_400C_2030CP_15th_NoCC_No"  ~ "GDPAid",
      TRUE ~ SCENARIO
    ),
    Region = REMF,
    Year = as.numeric(as.character(YEMF)),
    value = as.numeric(IAMC_Template)
  ) %>%
  filter(Year == 2050) %>%
  select(SCENARIO, Region, Year, value) %>%
  filter(is.finite(value))

world17_cost <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(iso3c = iso_a3, geometry) %>%
  filter(iso3c != "ATA") %>%
  left_join(Region_map, by = "iso3c") %>%
  left_join(df_cost, by = "Region")

world17_cost_plot <- world17_cost %>%
  filter(!is.na(SCENARIO), !is.na(value))
world17_cost_plot <- world17_cost_plot %>%
  mutate(SCENARIO = factor(SCENARIO,levels = c("NonAid", "GDPAid")) )

p_map_cost <- ggplot(world17_cost_plot) +
  geom_sf(aes(fill = value), color = "grey70", linewidth = 0.1) +
  scale_fill_gradient(
    low = "#f7fbff",
    high = "#08306b",
    na.value = "grey90",
    name = "Policy cost\nGDP loss ratio\nNPV 5%"
  ) +
  facet_grid(rows = vars(SCENARIO)) +
  guides(
    fill = guide_colorbar(
      title.position = "top",
      barwidth = unit(10, "cm"),
      barheight = unit(0.4, "cm")
    )
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    strip.background = element_blank()
  )

print(p_map_cost)


#Policy cost map recovery------------
df_recovery <- df_cost %>%
  select(SCENARIO, Region, value) %>%
  pivot_wider(names_from = SCENARIO, values_from = value) %>%
  mutate(
    recovery_GDPAid =  (NonAid - GDPAid) 
  ) %>%
  pivot_longer(
    cols = starts_with("recovery_"),
    names_to = "SCENARIO",
    values_to = "recovery"
  ) %>%
  mutate(
    SCENARIO = str_remove(SCENARIO, "^recovery_")
  ) %>%
  filter(is.finite(recovery))

world17_recovery <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(iso3c = iso_a3, geometry) %>%
  filter(iso3c != "ATA") %>%
  left_join(Region_map, by = "iso3c") %>%
  left_join(df_recovery, by = "Region")

world17_recovery_plot <- world17_recovery %>%
  filter(!is.na(SCENARIO), !is.na(recovery))

lim <- max(abs(world17_recovery_plot$recovery), na.rm = TRUE)

p_map_recovery <- ggplot(world17_recovery_plot) +
  geom_sf(aes(fill = recovery), color = "grey70", linewidth = 0.1) +
  scale_fill_gradient2(
    low = "#2166ac",
    mid = "white",
    high = "#b2182b",
    midpoint = 0,
    limits = c(-lim, lim),
    na.value = "grey90",
    name = "Recovery rate from NonAid (%)"
  ) +
  #facet_wrap(~SCENARIO) +
  theme_void() +
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    plot.title = element_text(face = "bold")
  )

print(p_map_recovery)


# line graph ------------------------------------------------------------------


thema <- "Gro_Emi_CO2"
thema <- "Emi_CO2"
thema <- "Pop"
thema <- "GDP_MER"
thema <- "Gro_Rem_CO2"
thema <- "Pol_Cos_Cns_Los_rat"
thema <- "Pol_Cos_GDP_Los_rat"
thema <- "Prc_Car"
thema <- "Trd_Emi_All_Val"
thema <- "Trd_Emi_All_Vol"
thema <- "Pol_Cos_Cns_Los_NPV_5pc"

CLP <- c("SSP2_400C_2030CP_NoCC_No","SSP2_400C_2030CP_15th_NoCC_No")
Region <- c("Rprovider15th","Rrecipient15th")

df <- rgdx.param("global_17_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF == thema) %>%
  #filter(YEMF == "2100") %>% 
  filter(SCENARIO %in% CLP) %>%
  filter(REMF %in% Region)

#df$SCENARIO <- gsub("SSP2_2020NDC_NZE_CCS_NoCC_No", "NZE_CCS", df$SCENARIO)


g <- df %>% 
  ggplot(aes(x = YEMF, y = IAMC_Template, group = SCENARIO, color = SCENARIO)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +  
  facet_wrap(~REMF, scales = "free_y")+
  scale_x_discrete(breaks = c("2020","2040","2060","2080","2100"))+
  #ylab("GDP|MER (billion US$2010/yr)")+
  #ylab("Population (million)")+
  ylab("Carbon Price (US$2010/yr)")+
  ylab("Emissions|CO2 (Mt/yr)")+
  #ylab("Policy Cost|Consumption (%)")+
  #ylab("Policy Cost|GDP (%)")+
  #ylab("Electrification rate (%)")+
  ylab(thema)+
  Mytheme+
  theme(legend.position = "bottom")

plot(g)