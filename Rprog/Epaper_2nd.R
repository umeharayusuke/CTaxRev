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

theme_1 <- theme_bw() +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, size = 16, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        legend.position = "right", 
        #legend.title = element_blank(),
        strip.background = element_blank())



CLP <- c("SSP2_BaU_NoCC_No",
         "SSP2_400C_2030CP_Cont2030_NoCC_No",
         "SSP2_400C_2030CP_UniCarPrc2030_NoCC_No",
#         "SSP2_400C_2030CP_UniCarPrc2040_NoCC_No",
        "SSP2_400C_2030CP_UniCarPrc2050_NoCC_No")
Region <- c("XE25","JPN","IND","TUR","CHN")
Region <- c("USA","XER","XOC","CAN")
Region <- c("XSE","XSA","BRA","XLM","CIS","XME","XNF","XAF")
Region <- c("XE25","JPN","IND","TUR","CHN","USA","XER","XOC","XSE","XSA","CAN","BRA","XLM","CIS","XME","XNF","XAF")
Region <- c("World","R2OECD","R2NonOECD")

Mytheme <- theme_bw()+
  theme(text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, size = 16, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        legend.position = "right", 
        legend.title = element_blank(),
  )
# Emission ----------------------------------------------------------------

vec<- c("Emi_CO2_AFO",
        "Emi_CO2_Ene_Sup",
        "Emi_CO2_Ene_Dem",
        "Emi_CO2_Ind_Pro",
        "Emi_CO2_Pro_Use",
        "Emi_CO2_Cap_and_Rem")

col <- c("AFOLU" = "#FC8D62",
         "Energy Supply" = "#66C2A5",
         "Energy Demand" = "#8DA0CB",
         "Industrial Processes" = "#984EA3",
         "Product Use" = "#FFFF33",
         "CDR" = "#377EB8")

ylabel <- "CO2 emission (Mt)"

df <- rgdx.param("global_17_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF %in% vec) %>%
  #filter(YEMF == "2100") %>% 
  #filter(YEMF %in% c("2050","2100")) %>% 
  filter(SCENARIO %in% CLP) %>% 
  filter(REMF %in% Region)
#  filter(REMF == "JPN")
#df$SCENARIO <- factor(df$SCENARIO, levels = CLP)

df$VEMF <- gsub("Emi_CO2_AFO", "AFOLU", df$VEMF)
df$VEMF <- gsub("Emi_CO2_Ene_Sup", "Energy Supply", df$VEMF)
df$VEMF <- gsub("Emi_CO2_Ene_Dem", "Energy Demand", df$VEMF)
df$VEMF <- gsub("Emi_CO2_Ind_Pro", "Industrial Processes", df$VEMF)
df$VEMF <- gsub("Emi_CO2_Pro_Use", "Product Use", df$VEMF)
df$VEMF <- gsub("Emi_CO2_Cap_and_Rem", "CDR", df$VEMF)
df$SCENARIO <- gsub("SSP2_2020NDC_NZE_NoCC_No", "NZE", df$SCENARIO)
#df$SCENARIO <- gsub("SSP2_2020NDC_NZE_CCS_NoCC_No", "NZE_CCS", df$SCENARIO)
df$SCENARIO <- gsub("SSP2_2020NDC_NZE_ET_NoCC_No", "NZE_ET", df$SCENARIO)
#df$SCENARIO <- gsub("SSP2_2020NDC_NZE_CCS_ET_NoCC_No", "NZE_CCS_ET", df$SCENARIO)
df$SCENARIO <- gsub("SSP2_2020NDC_NZE_CCS_ET_pp_NoCC_No", "NZE_ET_CCS", df$SCENARIO)




g <- ggplot(data = df) +
  geom_bar(mapping = aes(x = YEMF, y = IAMC_Template, fill = VEMF), 
           stat = "identity", width = 0.9) +
  scale_x_discrete(breaks = c("2020","2040","2060","2080","2100"))+
  scale_fill_manual(values = col)+
  ylab(ylabel)+
  facet_grid(REMF~ SCENARIO, scales = "free_y")+
  Mytheme

plot(g)


name="Emi.png"
ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)


# Car_Seq_CCS -------------------------------------------------------------

vec <- c("Car_Seq_CCS_Bio",
         "Car_Seq_CCS_Fos",
         "Car_Seq_CCS_Ind_Pro",
         "Car_Seq_Dir_Air_Cap")

ylabel <- "CCS (Mt)"


df <- rgdx.param("global_17_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF %in% vec) %>%
  #filter(YEMF == "2100") %>% 
  filter(SCENARIO %in% CLP) %>% 
  filter(REMF %in% Region)

#df$VEMF <- gsub("Car_Rem_Bio_wit_CCS", "BECCS", df$VEMF)
#df$VEMF <- gsub("Car_Rem_Bio", "Biochar", df$VEMF)
#df$VEMF <- gsub("Car_Rem_Dir_Air_Cap_wit_CCS", "DACCS", df$VEMF)
#df$VEMF <- gsub("Car_Rem_Soi_Car_Seq", "Soil Carbon", df$VEMF)
df$SCENARIO <- gsub("SSP2_2020NDC_NZE_NoCC_No", "NZE", df$SCENARIO)
#df$SCENARIO <- gsub("SSP2_2020NDC_NZE_CCS_NoCC_No", "NZE_CCS", df$SCENARIO)
df$SCENARIO <- gsub("SSP2_2020NDC_NZE_ET_NoCC_No", "NZE_ET", df$SCENARIO)
#df$SCENARIO <- gsub("SSP2_2020NDC_NZE_CCS_ET_NoCC_No", "NZE_CCS_ET", df$SCENARIO)
df$SCENARIO <- gsub("SSP2_2020NDC_NZE_CCS_ET_pp_NoCC_No", "NZE_ET_CCS", df$SCENARIO)

g <- ggplot(data = df) +
  geom_bar(mapping = aes(x = YEMF, y = IAMC_Template, fill = VEMF), 
           stat = "identity", width = 0.9) +
  scale_x_discrete(breaks = c("2020","2040","2060","2080","2100"))+
  ylab(ylabel)+
  facet_grid(REMF ~ SCENARIO, scales = "free_y")+
  Mytheme

plot(g)



name="Car_Seq_CCS.png"
ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)


# Prm_Ene -----------------------------------------------------------------

vec <- c("Prm_Ene_Coa_w_CCS", 
         "Prm_Ene_Coa_wo_CCS",
         "Prm_Ene_Gas_w_CCS", 
         "Prm_Ene_Gas_wo_CCS", 
         "Prm_Ene_Oil_w_CCS", 
         "Prm_Ene_Oil_wo_CCS",
         "Prm_Ene_Hyd",
         "Prm_Ene_Solar", 
         "Prm_Ene_Win",
         "Prm_Ene_Nuc", 
         "Prm_Ene_Bio_w_CCS",
         "Prm_Ene_Bio_wo_CCS")
col <- c("Coal|w/o CCS" = "grey50", "Coal|w/ CCS" = "grey30", "Oil|w/o CCS" = "tan3",
         "Oil|w/ CCS" = "sandybrown", "Gas|w/o CCS" = "lightgoldenrod", "Gas|w/ CCS" = "lightgoldenrod3",
         "Hydro" = "lightsteelblue", "Nuclear" = "moccasin", "Solar" = "lightsalmon", "Wind" = "lightskyblue3",
         "Biomass|w/o CCS" = "darkolivegreen2", "Biomass|w/ CCS" = "darkolivegreen4", "Geothermal" = "peru")
ylabel <- "Primary energy (EJ/yr)"

df <- rgdx.param("global_17_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF %in% vec) %>%
  #filter(YEMF == "2100") %>% 
  filter(SCENARIO %in% CLP) %>% 
  filter(REMF %in% Region)

df$VEMF <- gsub("Prm_Ene_Hyd", "Hydro", df$VEMF)
df$VEMF <- gsub("Prm_Ene_Solar", "Solar", df$VEMF)
df$VEMF <- gsub("Prm_Ene_Win", "Wind", df$VEMF)
df$VEMF <- gsub("Prm_Ene_Nuc", "Nuclear", df$VEMF)
df$VEMF <- gsub("Prm_Ene_Bio_w_CCS", "Biomass|w/ CCS", df$VEMF)
df$VEMF <- gsub("Prm_Ene_Bio_wo_CCS", "Biomass|w/o CCS", df$VEMF)
df$VEMF <- gsub("Prm_Ene_Gas_w_CCS", "Gas|w/ CCS", df$VEMF)
df$VEMF <- gsub("Prm_Ene_Gas_wo_CCS", "Gas|w/o CCS", df$VEMF)
df$VEMF <- gsub("Prm_Ene_Oil_w_CCS", "Oil|w/ CCS", df$VEMF)
df$VEMF <- gsub("Prm_Ene_Oil_wo_CCS", "Oil|w/o CCS", df$VEMF)
df$VEMF <- gsub("Prm_Ene_Coa_w_CCS", "Coal|w/ CCS", df$VEMF)
df$VEMF <- gsub("Prm_Ene_Coa_wo_CCS", "Coal|w/o CCS", df$VEMF)
#df$SCENARIO <- gsub("SSP2i_BaU_NoCC_No", "BaU", df$SCENARIO)
df$SCENARIO <- gsub("SSP2_2020NDC_NZE_NoCC_No", "NZE", df$SCENARIO)
#df$SCENARIO <- gsub("SSP2_2020NDC_NZE_CCS_NoCC_No", "NZE_CCS", df$SCENARIO)
df$SCENARIO <- gsub("SSP2_2020NDC_NZE_ET_NoCC_No", "NZE_ET", df$SCENARIO)
#df$SCENARIO <- gsub("SSP2_2020NDC_NZE_CCS_ET_NoCC_No", "NZE_CCS_ET", df$SCENARIO)
df$SCENARIO <- gsub("SSP2_2020NDC_NZE_CCS_ET_pp_NoCC_No", "NZE_ET_CCS", df$SCENARIO)

g <- ggplot(df,
            aes(x = YEMF,
                y = IAMC_Template,
                fill = VEMF,
                group = VEMF)) +
  geom_area(alpha = 1,
            position = "stack") +
  scale_fill_manual(values = col) +
  ylab(ylabel) +
  scale_x_discrete(breaks = c("2020","2040","2060","2080","2100"))+
  facet_grid(REMF ~ SCENARIO, scales = "free_y") +
  Mytheme

plot(g)


name="Prm_Ene.png"
ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)


# Sec_Ene -----------------------------------------------------------------


vec <- c("Prm_Ene_Coa_w_CCS", 
         "Prm_Ene_Coa_wo_CCS",
         "Prm_Ene_Gas_w_CCS", 
         "Prm_Ene_Gas_wo_CCS", 
         "Prm_Ene_Oil_w_CCS", 
         "Prm_Ene_Oil_wo_CCS",
         "Prm_Ene_Hyd",
         "Prm_Ene_Solar", 
         "Prm_Ene_Win",
         "Prm_Ene_Nuc", 
         "Prm_Ene_Bio_w_CCS",
         "Prm_Ene_Bio_wo_CCS")

col <- c("Coal|w/o CCS" = "grey50", "Coal|w/ CCS" = "grey30", "Oil|w/o CCS" = "tan3",
         "Oil|w/ CCS" = "sandybrown", "Gas|w/o CCS" = "lightgoldenrod", "Gas|w/ CCS" = "lightgoldenrod3",
         "Hydro" = "lightsteelblue", "Nuclear" = "moccasin", "Solar" = "lightsalmon", "Wind" = "lightskyblue3",
         "Biomass|w/o CCS" = "darkolivegreen2", "Biomass|w/ CCS" = "darkolivegreen4", "Geothermal" = "peru")

vec <- gsub("Prm_Ene", "Sec_Ene_Ele", vec)
ylabel <- "Power generation (EJ/yr)"

df <- rgdx.param("global_17_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF %in% vec) %>%
  filter(SCENARIO %in% CLP) %>% 
  filter(REMF %in% Region)

df$VEMF <- gsub("Sec_Ene_Ele_Hyd", "Hydro", df$VEMF)
df$VEMF <- gsub("Sec_Ene_Ele_Solar", "Solar", df$VEMF)
df$VEMF <- gsub("Sec_Ene_Ele_Win", "Wind", df$VEMF)
df$VEMF <- gsub("Sec_Ene_Ele_Nuc", "Nuclear", df$VEMF)
#df$VEMF <- gsub("Sec_Ene_Ele_Gas_w_CCS|Sec_Ene_Ele_Gas_wo_CCS|Sec_Ene_Ele_Oil_w_CCS|Sec_Ene_Ele_Oil_wo_CCS|Sec_Ene_Ele_Coa_w_CCS|Sec_Ene_Ele_Coa_wo_CCS", "Fossil Fuels", df$VEMF)
#df$VEMF <- gsub("Sec_Ene_Ele_Bio_w_CCS|Sec_Ene_Ele_Bio_wo_CCS", "Biomass", df$VEMF)
df$VEMF <- gsub("Sec_Ene_Ele_Bio_w_CCS", "Biomass|w/ CCS", df$VEMF)
df$VEMF <- gsub("Sec_Ene_Ele_Bio_wo_CCS", "Biomass|w/o CCS", df$VEMF)
df$VEMF <- gsub("Sec_Ene_Ele_Gas_w_CCS", "Gas|w/ CCS", df$VEMF)
df$VEMF <- gsub("Sec_Ene_Ele_Gas_wo_CCS", "Gas|w/o CCS", df$VEMF)
df$VEMF <- gsub("Sec_Ene_Ele_Oil_w_CCS", "Oil|w/ CCS", df$VEMF)
df$VEMF <- gsub("Sec_Ene_Ele_Oil_wo_CCS", "Oil|w/o CCS", df$VEMF)
df$VEMF <- gsub("Sec_Ene_Ele_Coa_w_CCS", "Coal|w/ CCS", df$VEMF)
df$VEMF <- gsub("Sec_Ene_Ele_Coa_wo_CCS", "Coal|w/o CCS", df$VEMF)
df$SCENARIO <- gsub("SSP2_2020NDC_NZE_NoCC_No", "NZE", df$SCENARIO)
#df$SCENARIO <- gsub("SSP2_2020NDC_NZE_CCS_NoCC_No", "NZE_CCS", df$SCENARIO)
df$SCENARIO <- gsub("SSP2_2020NDC_NZE_ET_NoCC_No", "NZE_ET", df$SCENARIO)
#df$SCENARIO <- gsub("SSP2_2020NDC_NZE_CCS_ET_NoCC_No", "NZE_CCS_ET", df$SCENARIO)
df$SCENARIO <- gsub("SSP2_2020NDC_NZE_CCS_ET_pp_NoCC_No", "NZE_ET_CCS", df$SCENARIO)

g <- ggplot(df,
            aes(x = YEMF,
                y = IAMC_Template,
                fill = VEMF,
                group = VEMF)) +
  geom_area(alpha = 1,
            position = "stack") +
  scale_fill_manual(values = col) +
  ylab(ylabel) +
  scale_x_discrete(breaks = c("2020","2040","2060","2080","2100"))+
  facet_grid(REMF ~ SCENARIO, scales = "free_y") +
  Mytheme

plot(g)



name="Sec_Ene.png"
ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)


# Fin_Ene -----------------------------------------------------------------


vec <- c("Fin_Ene_Ele",
         "Fin_Ene_Gas",
         "Fin_Ene_Heat",
         "Fin_Ene_Hyd",
         "Fin_Ene_Liq_Oil",
         "Fin_Ene_Liq_Bio",
         "Fin_Ene_SolidsCoa",
         "Fin_Ene_SolidsBio")

col <- c( 
  "Coal"="grey70",
  "Oil"="sandybrown",
  "Gas"="moccasin",
  "Biomass"="#A9D65D",
  "Biofuel"="#DBFF70",
  "Electricity"="lightsteelblue",
  "Heat"="salmon",
  "Hydrogen"="thistle2")

ylabel <- "Final energy (EJ/yr)"

df <- rgdx.param("global_17_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF %in% vec) %>%
  filter(SCENARIO %in% CLP)%>% 
  filter(REMF %in% Region)

df$VEMF <- gsub("Fin_Ene_Ele", "Electricity", df$VEMF)
df$VEMF <- gsub("Fin_Ene_Gas", "Gas", df$VEMF)
df$VEMF <- gsub("Fin_Ene_Heat", "Heat", df$VEMF)
df$VEMF <- gsub("Fin_Ene_Hyd", "Hydrogen", df$VEMF)
df$VEMF <- gsub("Fin_Ene_Liq_Oil", "Oil", df$VEMF)
df$VEMF <- gsub("Fin_Ene_Liq_Bio", "Biofuel", df$VEMF)
df$VEMF <- gsub("Fin_Ene_SolidsCoa", "Coal", df$VEMF)
df$VEMF <- gsub("Fin_Ene_SolidsBio", "Biomass", df$VEMF)
df$VEMF <- gsub("Fin_Ene_Ind", "Industry", df$VEMF)
df$VEMF <- gsub("Fin_Ene_Res_and_Com", "Buildings", df$VEMF)
df$VEMF <- gsub("Fin_Ene_Tra", "Transport", df$VEMF)
df$SCENARIO <- gsub("SSP2_2020NDC_NZE_NoCC_No", "NZE", df$SCENARIO)
#df$SCENARIO <- gsub("SSP2_2020NDC_NZE_CCS_NoCC_No", "NZE_CCS", df$SCENARIO)
df$SCENARIO <- gsub("SSP2_2020NDC_NZE_ET_NoCC_No", "NZE_ET", df$SCENARIO)
#df$SCENARIO <- gsub("SSP2_2020NDC_NZE_CCS_ET_NoCC_No", "NZE_CCS_ET", df$SCENARIO)
df$SCENARIO <- gsub("SSP2_2020NDC_NZE_CCS_ET_pp_NoCC_No", "NZE_ET_CCS", df$SCENARIO)

g <- ggplot(df,
            aes(x = YEMF,
                y = IAMC_Template,
                fill = VEMF,
                group = VEMF)) +
  geom_area(alpha = 1,
            position = "stack") +
  scale_fill_manual(values = col) +
  ylab(ylabel) +
  scale_x_discrete(breaks = c("2020","2040","2060","2080","2100"))+
  facet_grid(REMF ~ SCENARIO, scales = "free_y") +
  Mytheme

plot(g)


name <- "Fin_Ene.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)


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



df <- rgdx.param("global_17_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF == thema) %>%
  #filter(YEMF == "2100") %>% 
  filter(SCENARIO %in% CLP) %>%
  filter(REMF %in% Region)

df$SCENARIO <- gsub("SSP2_2020NDC_NZE_NoCC_No", "NZE", df$SCENARIO)
#df$SCENARIO <- gsub("SSP2_2020NDC_NZE_CCS_NoCC_No", "NZE_CCS", df$SCENARIO)
df$SCENARIO <- gsub("SSP2_2020NDC_NZE_ET_NoCC_No", "NZE_ET", df$SCENARIO)
#df$SCENARIO <- gsub("SSP2_2020NDC_NZE_CCS_ET_NoCC_No", "NZE_CCS_ET", df$SCENARIO)
df$SCENARIO <- gsub("SSP2_400C_2030CP_UniCarPrc2050_NoCC_No", "DifCarPrc", df$SCENARIO)
df$SCENARIO <- gsub("SSP2_400C_2030CP_UniCarPrc2030_NoCC_No", "UniCarPrc", df$SCENARIO)
df$SCENARIO <- gsub("SSP2_BaU_NoCC_No", "BaU", df$SCENARIO)
df$SCENARIO <- gsub("SSP2_400C_2030CP_Cont2030_NoCC_No", "Continue", df$SCENARIO)

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

name  <- paste0(thema, ".png")
ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 10,
  height = 7,
  dpi = 400,
  bg = "white"
)
