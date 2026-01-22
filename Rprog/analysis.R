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

output_dir <- file.path("..", "output")
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Fig. 2 ------------------------------------------------------------------

thema <- "Rev_gov_Tax_Car_Tax"
region <- "R2OECD"
files <- list(
  list(file = "SSP2_400C_2030CP_trs100_GDPP_NoCC_No.gdx", scenario = "1.5C-100%", scale=1.0),
  list(file = "SSP2_400C_2030CP_trs80_NoCC_No.gdx", scenario = "1.5C-80%", scale=0.8),
  list(file = "SSP2_400C_2030CP_trs60_NoCC_No.gdx", scenario = "1.5C-60%", scale=0.6),
  list(file = "SSP2_400C_2030CP_trs40_NoCC_No.gdx", scenario = "1.5C-40%", scale=0.4),
  list(file = "SSP2_400C_2030CP_trs20_NoCC_No.gdx", scenario = "1.5C-20%", scale=0.2),
  list(file = "SSP2_400C_2030CP_NoCC_No.gdx", scenario = "1.5C-0%", scale=0)
)

df <- data.frame()

for (file_info in files) {
  gdx_data <- rgdx.param(file_info$file, "IAMC_template")
  df_temp <- gdx_data %>%
    filter(VEMF == thema) %>%
    rename(Year = "Y") %>%
    rename(CP = "IAMC_template") %>%
    mutate(
      VEMF = file_info$scenario,
      CP   = CP * file_info$scale   
    )%>% 
    filter(as.numeric(as.character(Year)) >= 2030) %>% 
    filter(REMF==region)
  
  
  df <- rbind(df, df_temp)
}


g2 <- df %>% 
  #filter(as.numeric(as.character(Year)) %% 5 == 0) %>%
  ggplot(aes(x = Year, y = CP/1000000, group = VEMF, color = VEMF)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +  
  scale_x_discrete(
    breaks = c("2010", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
  ) +
  scale_color_manual(
    name = "scenario",
    values = c(
      "1.5C-100%"  = "#08306B",
      "1.5C-80%"   = "#2171B5",
      "1.5C-60%"   = "#4292C6",
      "1.5C-40%"   = "#6BAED6",
      "1.5C-20%"   = "#9ECAE1",
      "1.5C-0%"   = "grey50"      ),
    breaks = c("1.5C-100%", "1.5C-80%", "1.5C-60%", "1.5C-40%", "1.5C-20%", "1.5C-0%"),
    labels = c("1.5C-100%", "1.5C-80%", "1.5C-60%", "1.5C-40%", "1.5C-20%", "1.5C-0%")
  ) + 
  labs(y = "Carbon Tax Revenue transferred to developing regions  \n(billion US$2010)") +
  theme_1

plot(g2)

ggsave(
  filename = file.path(output_dir, "Fig2.png"),
  plot = g2,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)


# Fig. 3 ------------------------------------------------------------------

thema <- "Emi_Kyo_Gas"
region <- c("R2NonOECD", "R5OECD90+EU", "World")
region_replacement <- c(
  "R5OECD90+EU" = "OECD",
  "R2NonOECD" = "NonOECD"
)

files <- list(
  list(file = "SSP2_400C_2030CP_trs100_GDPP_NoCC_No.gdx", scenario = "1.5C-100%"),
  list(file = "SSP2_400C_2030CP_trs80_NoCC_No.gdx", scenario = "1.5C-80%"),
  list(file = "SSP2_400C_2030CP_trs60_NoCC_No.gdx", scenario = "1.5C-60%"),
  list(file = "SSP2_400C_2030CP_trs40_NoCC_No.gdx", scenario = "1.5C-40%"),
  list(file = "SSP2_400C_2030CP_trs20_NoCC_No.gdx", scenario = "1.5C-20%"),
  list(file = "SSP2_400C_2030CP_NoCC_No.gdx", scenario = "1.5C-0%"),
  list(file = "SSP2_BaU_NoCC_No.gdx", scenario = "NCP")
)

df <- data.frame()

for (file_info in files) {
  gdx_data <- rgdx.param(file_info$file, "IAMC_template")
  df_temp <- gdx_data %>%
    filter(VEMF == thema) %>%
    rename(Year = "Y") %>%
    rename(CP = "IAMC_template") %>%
    mutate(VEMF = file_info$scenario) %>% 
    filter(REMF %in% region) 
  df <- rbind(df, df_temp)
}


df$REMF <- recode(df$REMF, !!!region_replacement)



g3_1 <- df %>% 
  filter(as.numeric(as.character(Year)) %% 5 == 0) %>%
  ggplot(aes(x = Year, y = CP/1000, group = VEMF, color = VEMF)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) + 
  scale_x_discrete(
    breaks = c("2010", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    name = expression(paste("GHG emission (", GtCO[2], "e", q, ")"))
  ) +
  scale_color_manual(
    name = "scenario",
    values = c(
      "NCP" = "black",
      "1.5C-100%"  = "#08306B",
      "1.5C-80%"   = "#2171B5",
      "1.5C-60%"   = "#4292C6",
      "1.5C-40%"   = "#6BAED6",
      "1.5C-20%"   = "#9ECAE1",
      "1.5C-0%"   = "grey50"      ),
    breaks = c("NCP", "1.5C-0%", "1.5C-20%", "1.5C-40%", "1.5C-60%", "1.5C-80%", "1.5C-100%"),
    labels = c("NCP", "1.5C-0%", "1.5C-20%", "1.5C-40%", "1.5C-60%", "1.5C-80%", "1.5C-100%")
  ) + 
  facet_wrap(~ REMF, scales = "free_y") +
  theme_1 +
  theme(
    legend.position = "right",
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold")
  )

plot(g3_1)


thema <- "Prc_Car"
region <- "World"

files <- list(
  list(file = "SSP2_400C_2030CP_trs100_GDPP_NoCC_No.gdx", scenario = "1.5C-100%"),
  list(file = "SSP2_400C_2030CP_trs80_NoCC_No.gdx", scenario = "1.5C-80%"),
  list(file = "SSP2_400C_2030CP_trs60_NoCC_No.gdx", scenario = "1.5C-60%"),
  list(file = "SSP2_400C_2030CP_trs40_NoCC_No.gdx", scenario = "1.5C-40%"),
  list(file = "SSP2_400C_2030CP_trs20_NoCC_No.gdx", scenario = "1.5C-20%"),
  list(file = "SSP2_400C_2030CP_NoCC_No.gdx", scenario = "1.5C-0%")
)

df <- data.frame()

for (file_info in files) {
  gdx_data <- rgdx.param(file_info$file, "IAMC_template")
  df_temp <- gdx_data %>%
    filter(VEMF == thema) %>%
    rename(Year = "Y") %>%
    rename(CP = "IAMC_template") %>%
    mutate(VEMF = file_info$scenario) %>% 
    filter(REMF==region)
  
  
  df <- rbind(df, df_temp)
}


g3_2 <- df %>% 
  filter(as.numeric(as.character(Year)) %% 5 == 0) %>%
  ggplot(aes(x = Year, y = CP, group = VEMF, color = VEMF)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +  
  scale_x_discrete(
    breaks = c("2010", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
  ) +
  scale_color_manual(
    name = "scenario",
    values = c(
      "NCP" = "black",
      "1.5C-100%"  = "#08306B",
      "1.5C-80%"   = "#2171B5",
      "1.5C-60%"   = "#4292C6",
      "1.5C-40%"   = "#6BAED6",
      "1.5C-20%"   = "#9ECAE1",
      "1.5C-0%"   = "grey50"      ),
    breaks = c("NCP", "1.5C-0%", "1.5C-20%", "1.5C-40%", "1.5C-60%", "1.5C-80%", "1.5C-100%"),
    labels = c("NCP", "1.5C-0%", "1.5C-20%", "1.5C-40%", "1.5C-60%", "1.5C-80%", "1.5C-100%")
  ) + 
  labs(y = "Carbon Price (US$2010/t-CO2)") +
  theme_1+
  theme(legend.position = "none")  

plot(g3_2)

g3_1_wl <- g3_1 + theme(legend.position = "right")
g3_1_l <- get_legend(g3_1_wl)

g3_2_wl <- g3_2 + theme(legend.position = "right")
g3_2_l <- get_legend(g3_2_wl)

g3 <-ggdraw() +
  draw_plot(g3_1+theme(legend.position='none'),x=0,y=0,width=0.45,height=1) +
  draw_plot(g3_2+theme(legend.position='none'),x=0.45,y=0,width=0.45,height=1) +
  draw_plot(g3_1_l,x=0.91,y=0.15,width=0.08,height=0.7) +
  draw_plot_label(
    label=c('a','b'),
    x=c(0,0.45),
    y=c(1,1),
    size=14
  )

plot(g3)


ggsave(
  filename = file.path(output_dir, "Fig3.png"),
  plot = g3,
  width = 14,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)

# Fig. 4 ------------------------------------------------------------------

thema <- "Pol_Cos_Cns_Los"
regions <- c("R5REF", "R5LAM", "R5ASIA", "R5MAF", "R5OECD90+EU")
region_colors <- c(
  "OECD90+EU" = "#1b9e77",
  "Asia" = "#d95f02",
  "Former Soviet Union" = "#7570b3",
  "Middle East and Africa" = "#e7298a",
  "Latin America" = "#66a61e"
)

scenario_file_mapping <- data.frame(
  scenario = c("SSP2_400C_2030CP_NoCC_No",
               "SSP2_400C_2030CP_trs20_NoCC_No",
               "SSP2_400C_2030CP_trs40_NoCC_No",
               "SSP2_400C_2030CP_trs60_NoCC_No",
               "SSP2_400C_2030CP_trs80_NoCC_No",
               "SSP2_400C_2030CP_trs100_GDPP_NoCC_No"),
  scenario_label = c("1.5C-0%",
                     "1.5C-20%",
                     "1.5C-40%",
                     "1.5C-60%",
                     "1.5C-80%",
                     "1.5C-100%"),
  stringsAsFactors = FALSE
)

df <- data.frame()
for (i in 1:nrow(scenario_file_mapping)) {
  gdx_file <- paste0(scenario_file_mapping$scenario[i], ".gdx")
  df_gdx <- rgdx.param(gdx_file, "IAMC_template")
  for (region in regions) {
    df_temp <- df_gdx %>%
      filter(REMF == region, VEMF == thema) %>%
      rename(Year = "Y", LossRate = "IAMC_template") %>%
      mutate(VEMF = scenario_file_mapping$scenario_label[i], Region = region) %>% 
      mutate(
        Year = as.numeric(as.character(Year)), 
        REMF = recode(REMF,
                      "R5OECD90+EU" = "OECD90+EU",
                      "R5ASIA" = "Asia",
                      "R5REF" = "Former Soviet Union",
                      "R5MAF" = "Middle East and Africa",
                      "R5LAM" = "Latin America")
      ) %>%
      filter(Year >= 2030)
    df <- rbind(df, df_temp)
  }
}


df_s <- df %>%
  group_by(REMF, VEMF) %>%
  summarise(Loss = sum(LossRate, na.rm = TRUE)) %>%
  ungroup()

df_s$VEMF <- factor(df_s$VEMF,
                          levels = c("1.5C-0%", "1.5C-20%", "1.5C-40%", "1.5C-60%", "1.5C-80%", "1.5C-100%"))
df_s$REMF <- factor(df_s$REMF,
                          levels = c("OECD90+EU","Asia", "Former Soviet Union", "Middle East and Africa" ,"Latin America"))

g4_1 <- ggplot(df_s, aes(x = VEMF, y = Loss/1000, fill = REMF)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = region_colors) + 
  labs(
    x = "",
    y = "Cumulative Consumption Loss\n (trillion US$, 2030-2050)",
    fill = "Region"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

g4_1 <- ggplot(df_s, aes(x = VEMF, y = Loss, fill = REMF)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = region_colors) +
  labs(
    x = "",
    y = "Share of Cumulative Consumption Loss",
    fill = "Region"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

plot(g4_1)


files <- list(
  list(file = "SSP2_400C_2030CP_NoCC_No.gdx", scenario = "0"),
  list(file = "SSP2_400C_2030CP_trs20_NoCC_No.gdx", scenario = "20"),
  list(file = "SSP2_400C_2030CP_trs40_NoCC_No.gdx", scenario = "40"),
  list(file = "SSP2_400C_2030CP_trs60_NoCC_No.gdx", scenario = "60"),
  list(file = "SSP2_400C_2030CP_trs80_NoCC_No.gdx", scenario = "80"),
  list(file = "SSP2_400C_2030CP_trs100_GDPP_NoCC_No.gdx", scenario = "100")
)

year <- 2050
region <- c("World", "R5OECD90+EU", "RNonOECDexCIS")  
VEMF_list <- c("Pol_Cos_Cns_Los_rat_NPV_5pc")  

region_replacement <- c(
  "R5OECD90+EU" = "OECD",
  "RNonOECDexCIS" = "NonOECD",
  "R5ASIA" = "Asia",
  "R5REF" = "Former Soviet Union",
  "R5MAF" = "Middle East and Africa",
  "R5LAM" = "Latin America"
)

plot_list <- data.frame()

for (file_info in files) {
  df_temp <- rgdx.param(file_info$file, "IAMC_template") %>% 
    filter(Y == year) %>%
    filter(REMF %in% region) %>%
    filter(VEMF %in% VEMF_list)
  df_temp$scenario <- file_info$scenario
  plot_list <- rbind(plot_list, df_temp)
}

plot_list$VEMF <- gsub("Pol_Cos_Cns_Los_rat_NPV_5pc", "5 %", plot_list$VEMF, fixed = TRUE)
plot_list$REMF <- recode(plot_list$REMF, !!!region_replacement)

plot_list$IAMC_template <- signif(plot_list$IAMC_template, digits = 3)

plot_list <- plot_list %>% arrange(desc(IAMC_template))

plot_list$scenario <- as.numeric(plot_list$scenario)

plot_list <- plot_list %>%
  filter(REMF != "World") 


g4_2 <- plot_list %>%
  ggplot(aes(x = as.numeric(scenario), y = IAMC_template, color = REMF)) +
  geom_point(size = 4) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linetype = "dashed",
    size = 1
  ) +
  scale_x_continuous(
    breaks = c(0, 20, 40, 60, 80, 100),
    name = "Transfer ratio of carbon tax revenue\nin developed regions (%)",
    sec.axis = sec_axis(
      transform = ~ . * 1206 / 100, 
      name = "Equivalent transfer amount\n(billion US$/year)",
      breaks = seq(0, 1206, by = 200)
    )
  ) +
  scale_y_continuous(
    name = "Cumulative Consumption loss\nfrom 2030–2050 (%)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.x.top = element_text(margin = margin(b = 10)),
    plot.margin = margin(b = 15, t = 20, r = 10, l = 10),
    legend.title = element_blank()
  )

fit_data <- plot_list %>%
  group_by(REMF) %>%
  do({
    mod <- lm(IAMC_template ~ as.numeric(scenario), data = .)
    data.frame(
      scenario = seq(0, 160, by = 1),   
      IAMC_template = predict(mod, newdata = data.frame(scenario = seq(0, 160, by = 1)))
    )
  })

g4_2 <- plot_list %>%
  ggplot(aes(x = as.numeric(scenario), y = IAMC_template, color = REMF)) +
  geom_point(size = 4) +
  
  geom_line(
    data = fit_data %>% filter(scenario <= 100),
    aes(x = scenario, y = IAMC_template, color = REMF),
    linewidth = 1
  ) +
  
  geom_line(
    data = fit_data %>% filter(scenario > 100),
    aes(x = scenario, y = IAMC_template, color = REMF),
    linewidth = 1,
    linetype = "dashed"
  ) +
  
  scale_x_continuous(
    limits = c(0, 160),
    breaks = c(0, 20, 40, 60, 80, 100,120,140,160),
    name = "Transfer ratio of carbon tax revenue\nin developed regions (%)",
    sec.axis = sec_axis(
      transform = ~ . * 1206 / 100,
      name = "Equivalent transfer amount\n(billion US$/year)",
      breaks = seq(0, 2000, by = 200)
    )
  ) +
  scale_y_continuous(
    name = "Cumulative Consumption loss\nfrom 2030–2050 (%)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.x.top = element_text(margin = margin(b = 10)),
    plot.margin = margin(b = 15, t = 20, r = 10, l = 10),
    legend.title = element_blank()
  )

mod_non <- lm(
  IAMC_template ~ as.numeric(scenario),
  data = plot_list %>% filter(REMF == "NonOECD")
)

coef_non <- coef(mod_non)
x_zero_non <- -coef_non[1] / coef_non[2]
x_zero_non

g4_2 <- g4_2 +
  geom_vline(
    xintercept = x_zero_non,
    linetype = "solid",
    color = "black",
    linewidth = 1 )


g4_1_wl <- g4_1 + theme(legend.position = "right")
g4_1_l <- get_legend(g4_1_wl)

g4_2_wl <- g4_2 + theme(legend.position = "right")
g4_2_l <- get_legend(g4_2_wl)

g4 <-ggdraw() +
  draw_plot(g4_1+theme(legend.position='none'),x=0,y=0,width=0.4,height=1) +
  draw_plot(g4_2+theme(legend.position='none'),x=0.4,y=0,width=0.4,height=1) +
  draw_plot(g4_1_l,x=0.85,y=0.35,width=0.1,height=0.7) +
  draw_plot(g4_2_l,x=0.84,y=0.10,width=0.1,height=0.7) +
  draw_plot_label(
    label=c('a','b'),
    x=c(0,0.4),
    y=c(1,1),
    size=14
  )

plot(g4)

ggsave(
  filename = file.path(output_dir, "Fig4.png"),
  plot = g4,
  width = 14,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)


# Fig. 5 ------------------------------------------------------------------

gdx_files <- list(
  "SSP2_BaU_NoCC_No.gdx" = "NCP",
  "SSP2_400C_2030CP_NoCC_No.gdx" = "1.5C-0%",
  "SSP2_400C_2030CP_trs100_GDPP_NoCC_No.gdx" = "1.5C-100%"
)

year <- 2050  
textsize <- 15  

region_code <- c("R5ASIA", "R5OECD90+EU", "R5REF", "R5MAF", "R5LAM", "World" ,"R2NonOECD")
OECDorNON <- "on"  
non_OECD <- if (OECDorNON == "on") c("R5ASIA","R5REF","R5MAF","R5LAM") else NULL



process_data <- function(data, region_code, year, OECDorNON, non_OECD) {
  df2 <- data %>% filter(REMF %in% region_code)
  df3 <- df2 %>% filter(Y == year)
  df4 <- df3 %>% filter(VEMF %in% c("Prm_Ene_Coa_w_CCS", "Prm_Ene_Coa_wo_CCS", 
                                    "Prm_Ene_Gas_w_CCS", "Prm_Ene_Gas_wo_CCS", 
                                    "Prm_Ene_Oil_w_CCS", "Prm_Ene_Oil_wo_CCS", 
                                    "Prm_Ene_Hyd", 
                                    "Prm_Ene_Solar", "Prm_Ene_Win", 
                                    "Prm_Ene_Nuc", 
                                    "Prm_Ene_Bio_w_CCS", "Prm_Ene_Bio_wo_CCS"))
  
  if (OECDorNON == "on") {
    df4_OECD <- df4 %>% filter(REMF == "R5OECD90+EU")
    df4_OECD$REMF <- gsub("R5OECD90+EU", "OECD", df4_OECD$REMF, fixed = TRUE)
    
    df4_nonOECD <- df4 %>% filter(REMF == "R2NonOECD")
    df4_nonOECD$REMF <- gsub("R2NonOECD", "NonOECD", df4_nonOECD$REMF, fixed = TRUE)
    
    
    df4 <- rbind(df4_OECD, df4_nonOECD)
  }
  
  df4$VEMF <- gsub("Prm_Ene_Hyd", "Hydro", df4$VEMF)
  df4$VEMF <- gsub("Prm_Ene_Solar", "Solar", df4$VEMF)
  df4$VEMF <- gsub("Prm_Ene_Win", "Wind", df4$VEMF)
  df4$VEMF <- gsub("Prm_Ene_Nuc", "Nuclear", df4$VEMF)
  df4$VEMF <- gsub("Prm_Ene_Coa_w_CCS|Prm_Ene_Coa_wo_CCS|Prm_Ene_Oil_w_CCS|Prm_Ene_Oil_wo_CCS|Prm_Ene_Gas_w_CCS|Prm_Ene_Gas_wo_CCS", "Fossil Fuels", df4$VEMF)
  df4$VEMF <- gsub("Prm_Ene_Bio_w_CCS|Prm_Ene_Bio_wo_CCS", "Biomass", df4$VEMF)
  return(df4)
}

df_list <- lapply(names(gdx_files), function(file_name) {
  data <- rgdx.param(file_name, "IAMC_template")
  df <- process_data(data, region_code, year, OECDorNON, non_OECD)
  df$scenario <- gdx_files[[file_name]]
  return(df)
})

df5 <- do.call(rbind, df_list)

df5$scenario <- factor(df5$scenario, levels = c("NCP", "1.5C-0%", "1.5C-100%"))
df5$REMF <- factor(df5$REMF, levels = c("OECD",  "NonOECD"))
df5$VEMF <- factor(df5$VEMF, levels = c("Fossil Fuels", "Hydro", "Nuclear",  "Solar", "Wind","Biomass"))

g5_1 <- ggplot(data = df5) +
  geom_bar(mapping = aes(x = scenario, y = IAMC_template, fill = VEMF), 
           stat = "identity", width = 0.7) +
  ylab("Primary energy in 2050 (EJ)") +
  facet_wrap(~REMF, ncol = 3, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "Fossil Fuels" = "gray60",  
      "Hydro" = "lightsteelblue", 
      "Nuclear" = "moccasin",  "Solar" = "lightsalmon", "Wind" = "lightskyblue3", "Biomass" = "darkolivegreen2"
    ),
    breaks = c("Fossil Fuels", "Hydro", "Nuclear",  "Solar", "Wind","Biomass")
  ) +
  theme_1

plot(g5_1)


gdx_files <- c(  "SSP2_BaU_NoCC_No.gdx",
                 "SSP2_400C_2030CP_NoCC_No.gdx" ,
                 "SSP2_400C_2030CP_trs100_GDPP_NoCC_No.gdx")
scenarios <- c("NCP", "1.5C-0%", "1.5C-100%")  

prm_vector <- c("Prm_Ene_Coa_w_CCS", "Prm_Ene_Coa_wo_CCS", "Prm_Ene_Gas_w_CCS", "Prm_Ene_Gas_wo_CCS", "Prm_Ene_Oil_w_CCS", "Prm_Ene_Oil_wo_CCS", 
                "Prm_Ene_Hyd", "Prm_Ene_Solar", "Prm_Ene_Win",  "Prm_Ene_Nuc", "Prm_Ene_Bio_w_CCS", "Prm_Ene_Bio_wo_CCS")
sec_vector <- gsub("Prm_Ene", "Sec_Ene_Ele", prm_vector)

region_code <- c("World", "USA", "XE25", "XER", "TUR", "XOC", "CHN", "IND", "JPN", "XSE", "XSA", "CAN", "BRA", "XLM", "CIS", "XME", "XNF", "XAF")
region_code <- c("R5ASIA", "R5OECD90+EU", "R5REF", "R5MAF", "R5LAM", "World")
region_code <- c("R2NonOECD", "R5OECD90+EU")



process_data <- function(gdx_file, scenario, region_code, year, sec_vector) {
  df <- rgdx.param(gdx_file, "IAMC_template") %>%
    filter(REMF %in% region_code) %>%
    filter(Y == year) %>%
    filter(VEMF %in% sec_vector)
  df$scenario <- scenario
  
  df$VEMF <- gsub("Sec_Ene_Ele_Hyd", "Hydro", df$VEMF)
  df$VEMF <- gsub("Sec_Ene_Ele_Solar", "Solar", df$VEMF)
  df$VEMF <- gsub("Sec_Ene_Ele_Win", "Wind", df$VEMF)
  df$VEMF <- gsub("Sec_Ene_Ele_Nuc", "Nuclear", df$VEMF)
  
  df$VEMF <- gsub("Sec_Ene_Ele_Gas_w_CCS|Sec_Ene_Ele_Gas_wo_CCS|Sec_Ene_Ele_Oil_w_CCS|Sec_Ene_Ele_Oil_wo_CCS|Sec_Ene_Ele_Coa_w_CCS|Sec_Ene_Ele_Coa_wo_CCS", 
                  "Fossil Fuels", df$VEMF)
  df$VEMF <- gsub("Sec_Ene_Ele_Bio_w_CCS|Sec_Ene_Ele_Bio_wo_CCS", 
                  "Biomass", df$VEMF)
  
  return(df)
}

df_all <- data.frame()  

for (i in 1:length(gdx_files)) {
  df_temp <- process_data(gdx_files[i], scenarios[i], region_code, 2050, sec_vector)
  df_all <- rbind(df_all, df_temp)
}

df_all$scenario <- factor(df_all$scenario, levels = c("NCP", "1.5C-0%", "1.5C-100%"))

df_all <- df_all %>%
  mutate(REMF = recode(REMF,
                       "R5OECD90+EU" = "OECD",
                       "R2NonOECD" = "NonOECD",
                       "R5ASIA" = "Asia",
                       "R5REF" = "Former Soviet Union",
                       "R5MAF" = "Middle East and Africa",
                       "R5LAM" = "Latin America"))

df_all$REMF <- factor(df_all$REMF, levels = c("OECD", "NonOECD"))
df_all$VEMF <- factor(df_all$VEMF, levels =c("Fossil Fuels","Geothermal", "Hydro", "Nuclear",  "Solar", "Wind","Biomass"))


g5_2 <- ggplot(data = df_all) +
  geom_bar(mapping = aes(x = scenario, y = IAMC_template, fill = VEMF), 
           stat = "identity", width = 0.7) +
  ylab("Power generation in 2050 (EJ)") +
  facet_wrap(~REMF, ncol = 3, scales = "free_y") +  
  scale_fill_manual(values = c(
    "Fossil Fuels" = "grey50", 
    "Geothermal" = "peru", "Hydro" = "lightsteelblue", 
    "Nuclear" = "moccasin", "Solar" = "lightsalmon", "Wind" = "lightskyblue3","Biomass" = "darkolivegreen2"
  ),
  breaks = c("Fossil Fuels","Geothermal", "Hydro", "Nuclear",  "Solar", "Wind","Biomass")) +
  labs(fill = "a-b") +   
  theme_1

plot(g5_2)


year <- 2050

load_and_format_data <- function(gdx_file, scenario_name, region_code, year) {
  df <- rgdx.param(gdx_file, "IAMC_template") %>%
    filter(REMF %in% region_code) %>%
    filter(Y == year) %>%
    filter(VEMF %in% c("Fin_Ene_Ele",
                       "Fin_Ene_Gas",
                       "Fin_Ene_Heat",
                       "Fin_Ene_Hyd",
                       "Fin_Ene_Liq_Oil",
                       "Fin_Ene_Liq_Bio",
                       "Fin_Ene_SolidsCoa",
                       "Fin_Ene_SolidsBio"))
  
  df$VEMF <- gsub("Fin_Ene_Ele", "Electricity", df$VEMF)
  df$VEMF <- gsub("Fin_Ene_Gas", "Gas", df$VEMF)
  df$VEMF <- gsub("Fin_Ene_Heat", "Heat", df$VEMF)
  df$VEMF <- gsub("Fin_Ene_Hyd", "Hydrogen", df$VEMF)
  df$VEMF <- gsub("Fin_Ene_Liq_Oil", "Oil", df$VEMF)
  df$VEMF <- gsub("Fin_Ene_Liq_Bio", "Biofuel", df$VEMF)
  df$VEMF <- gsub("Fin_Ene_SolidsCoa", "Coal", df$VEMF)
  df$VEMF <- gsub("Fin_Ene_SolidsBio", "Biomass", df$VEMF)
  
  df$scenario <- scenario_name
  
  return(df)
}

scenarios <- list(
  list(gdx_file = "SSP2_BaU_NoCC_No.gdx", scenario_name = "NCP"),
  list(gdx_file = "SSP2_400C_2030CP_NoCC_No.gdx", scenario_name = "1.5C-0%"),
  list(gdx_file = "SSP2_400C_2030CP_trs100_GDPP_NoCC_No.gdx", scenario_name = "1.5C-100%")
)

region_code <- c("World", "USA", "XE25", "XER", "TUR", "XOC", "CHN", "IND", "JPN", "XSE", "XSA", "CAN", "BRA", "XLM", "CIS", "XME", "XNF", "XAF")
region_code <- c("R5ASIA", "R5OECD90+EU", "R5REF", "R5MAF", "R5LAM", "World")

OECDorNON = "on"

if(OECDorNON == "on"){
  non_OECD <- c("R5ASIA","R5REF","R5MAF","R5LAM")
  region_code <- c("R2NonOECD", "R5OECD90+EU")
}

all_data <- purrr::map_dfr(scenarios, function(scenario) {
  load_and_format_data(scenario$gdx_file, scenario$scenario_name, region_code, year)
})

all_data$scenario <- factor(all_data$scenario, levels = c("NCP", "1.5C-0%","1.5C-100%"))
all_data <- all_data %>%
  mutate(REMF = recode(REMF,
                       "R5OECD90+EU" = "OECD",
                       "R2NonOECD" = "NonOECD",
                       "R5ASIA" = "Asia",
                       "R5REF" = "Former Soviet Union",
                       "R5MAF" = "Middle East and Africa",
                       "R5LAM" = "Latin America"))
all_data$REMF <- factor(all_data$REMF, levels = c("OECD", "NonOECD"))


df_all_sum <- all_data %>%
  group_by(scenario, REMF) %>%
  summarise(total_emission = sum(IAMC_template, na.rm = TRUE))

color <- c( 
  "Coal"="grey70",
  "Oil"="sandybrown",
  "Gas"="moccasin",
  "Biomass"="#A9D65D",
  "Biofuel"="#DBFF70",
  "Electricity"="lightsteelblue",
  "Heat"="salmon",
  "Hydrogen"="thistle2"
)

all_data$VEMF <- factor(all_data$VEMF, levels = c("Coal", "Oil", "Gas","Biomass", "Biofuel", "Electricity", "Heat", "Hydrogen"))


g5_3 <- ggplot(data = all_data) +
  geom_bar(aes(x = scenario, y = IAMC_template, fill = VEMF), stat = "identity", position = "stack", width = 0.7)+
  ylab("Final Energy in 2050 (EJ) ") + 
  facet_wrap(~REMF, ncol = 3, scales = "free_y") + 
  scale_fill_manual(
    values = color, 
    breaks = c("Coal", "Oil", "Gas","Biomass", "Biofuel", "Electricity", "Heat", "Hydrogen"), 
    guide = guide_legend(reverse = FALSE)
  ) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +  
  labs(fill = "c") +  
  theme_1  


print(g5_3)

year <- 2050

load_and_format_data <- function(gdx_file, scenario_name, region_code, year) {
  df <- rgdx.param(gdx_file, "IAMC_template") %>%
    filter(REMF %in% region_code) %>%
    filter(Y == year) %>%
    filter(VEMF %in% c("Emi_CO2_AFO",
                       "Emi_CO2_Ene_Sup", 
                       "Emi_CO2_Ene_Dem", 
                       "Emi_CO2_Ind_Pro",
                       "Emi_CO2_Pro_Use",
                       "Emi_CO2_Cap_and_Rem"))
  
  df$VEMF <- gsub("Emi_CO2_AFO", "AFOLU", df$VEMF)
  df$VEMF <- gsub("Emi_CO2_Ene_Sup", "Energy Supply", df$VEMF)
  df$VEMF <- gsub("Emi_CO2_Ene_Dem", "Energy Demand", df$VEMF)
  df$VEMF <- gsub("Emi_CO2_Ind_Pro", "Industrial Processes", df$VEMF)
  df$VEMF <- gsub("Emi_CO2_Pro_Use", "Product Use", df$VEMF)
  df$VEMF <- gsub("Emi_CO2_Cap_and_Rem", "CDR(NonAff and NonBECCS)", df$VEMF)
  df$scenario <- scenario_name
  return(df)
}

scenarios <- list(
  list(gdx_file = "SSP2_BaU_NoCC_No.gdx", scenario_name = "NCP"),
  list(gdx_file = "SSP2_400C_2030CP_NoCC_No.gdx", scenario_name = "1.5C-0%"),
  list(gdx_file = "SSP2_400C_2030CP_trs100_GDPP_NoCC_No.gdx", scenario_name = "1.5C-100%")
)

region_code <- c("World", "USA", "XE25", "XER", "TUR", "XOC", "CHN", "IND", "JPN", "XSE", "XSA", "CAN", "BRA", "XLM", "CIS", "XME", "XNF", "XAF")
region_code <- c("R5ASIA", "R5OECD90+EU", "R5REF", "R5MAF", "R5LAM", "World")
region_code <- c("R2NonOECD", "R5OECD90+EU")

OECDorNON = "on"


all_data <- purrr::map_dfr(scenarios, function(scenario) {
  load_and_format_data(scenario$gdx_file, scenario$scenario_name, region_code, year)
})

all_data$scenario <- factor(all_data$scenario, levels = c("NCP", "1.5C-0%","1.5C-100%"))
all_data <- all_data %>%
  mutate(REMF = recode(REMF,
                       "R5OECD90+EU" = "OECD",
                       "R2NonOECD" = "NonOECD",
                       "R5ASIA" = "Asia",
                       "R5REF" = "Former Soviet Union",
                       "R5MAF" = "Middle East and Africa",
                       "R5LAM" = "Latin America"))

if(OECDorNON == "on"){
  all_data$REMF <- factor(all_data$REMF, levels = c("OECD", "NonOECD"))
}

df_all_sum <- all_data %>%
  group_by(scenario, REMF) %>%
  summarise(total_emission = sum(IAMC_template, na.rm = TRUE))

color <- c("AFOLU" = "#FC8D62",
           "Energy Supply" = "#66C2A5",
           "Energy Demand" = "#8DA0CB",
           "Industrial Processes" = "#984EA3",
           "Product Use" = "#FFFF33",
           "CDR(NonAff and NonBECCS)" = "#377EB8")

g5_4 <- ggplot(data = all_data) +
  geom_bar(aes(x = scenario, y = IAMC_template / 1000, fill = VEMF), stat = "identity", position = "stack", width = 0.7) +
  scale_y_continuous(name = expression(paste({CO[2]}," emission in 2050 (Gt)"))) + 
  facet_wrap(~REMF, ncol = 3, scales = "free_y") +  
  scale_fill_manual(
    values = color, 
    breaks = c("Energy Demand","Energy Supply","Industrial Processes", "Product Use", "AFOLU","CDR(NonAff and NonBECCS)"), 
    guide = guide_legend(reverse = FALSE) 
  ) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +  
  theme_1  +
  labs(fill = "d") 


g5_4 <- g5_4 + geom_point(data = df_all_sum, 
                      aes(x = scenario, y = total_emission/1000), 
                      size = 1, shape = 19, color = "black") +  
  guides(color = "none")  

print(g5_4)


g5_1_with_legend <- g5_1 + theme(legend.position = "right")
g5_1_legend <- get_legend(g5_1_with_legend)

g5_2_with_legend <- g5_2 + theme(legend.position = "right")
g5_2_legend <- get_legend(g5_2_with_legend)

g5_3_with_legend <- g5_3 + theme(legend.position = "right")
g5_3_legend <- get_legend(g5_3_with_legend)

g5_4_with_legend <- g5_4 + theme(legend.position = "right")
g5_4_legend <- get_legend(g5_4_with_legend)

g <- ggdraw() +
  draw_plot(g5_1 + theme(legend.position = "none"), x = 0,   y = 0.5, width = 0.4, height = 0.5) +
  draw_plot(g5_2 + theme(legend.position = "none"), x = 0.4, y = 0.5, width = 0.4, height = 0.5) +
  draw_plot(g5_3 + theme(legend.position = "none"), x = 0,   y = 0, width = 0.4, height = 0.5) +
  draw_plot(g5_4 + theme(legend.position = "none"), x = 0.4, y = 0, width = 0.4, height = 0.5) +
  draw_plot_label(
    label = c("a", "b", "c", "d"),
    x = c(0, 0.4, 0, 0.4),
    y = c(1, 1, 0.5, 0.5),
    size = 14
  )

g5 <- ggdraw() +
  draw_plot(g, x = 0, y = 0, width = 0.8, height = 1) +
  draw_plot(g5_2_legend, x = 0.65, y = 0.65, width = 0.15, height = 0.2) +
  draw_plot(g5_3_legend, x = 0.65, y = 0.25, width = 0.15, height = 0.2) +
  draw_plot(g5_4_legend, x = 0.8, y = 0.27, width = 0.15, height = 0.2)

plot(g5)


ggsave(
  filename = file.path(output_dir, "Fig5.png"),
  plot = g5,
  width = 14,
  height = 10,
  units = "in",
  dpi = 300,
  bg = "white"
)


# Fig. 6 ------------------------------------------------------------------


years <- seq(2030, 2050, 5)

load_and_format_data <- function(gdx_file, scenario_name, region_code) {
  df <- rgdx.param(gdx_file, "IAMC_template") %>%
    filter(REMF %in% region_code) %>%
    filter(Y %in% years) %>%      
    filter(VEMF %in% c("Car_Rem_Bio",
                       "Car_Rem_Bio_wit_CCS", 
                       "Car_Rem_Dir_Air_Cap_wit_CCS", 
                       "Car_Rem_Enh_Wea", 
                       "Car_Rem_Frs",
                       "Car_Rem_Soi_Car_Seq"))
  
  df$VEMF <- recode(df$VEMF,
                    "Car_Rem_Bio_wit_CCS" = "BECCS",
                    "Car_Rem_Bio" = "Biochar",
                    "Car_Rem_Dir_Air_Cap_wit_CCS" = "DACCS",
                    "Car_Rem_Enh_Wea" = "Enhanced Weather",
                    "Car_Rem_Frs" = "Afforestation",
                    "Car_Rem_Soi_Car_Seq" = "Soil Carbon")
  
  df$scenario <- scenario_name
  return(df)
}


scenarios <- list(
  list(gdx_file = "SSP2_BaU_NoCC_No.gdx", scenario_name = "NCP"),
  list(gdx_file = "SSP2_400C_2030CP_NoCC_No.gdx", scenario_name = "1.5C-0%"),
  list(gdx_file = "SSP2_400C_2030CP_trs100_GDPP_NoCC_No.gdx", scenario_name = "1.5C-100%")
)

OECDorNON <- "on"
if(OECDorNON == "on") region_code <- c("R2NonOECD", "R5OECD90+EU")

all_data <- purrr::map_dfr(scenarios, function(scenario) {
  load_and_format_data(scenario$gdx_file, scenario$scenario_name, region_code)
})

all_data <- all_data %>%
  mutate(REMF = recode(REMF,
                       "R5OECD90+EU" = "OECD",
                       "R2NonOECD" = "NonOECD"))

all_data$scenario <- factor(all_data$scenario, levels = c("NCP", "1.5C-0%", "1.5C-100%"))
all_data$REMF <- factor(all_data$REMF, levels = c("OECD", "NonOECD"))
all_data$VEMF <- factor(all_data$VEMF, 
                        levels = c("BECCS", "Enhanced Weather", "Biochar", "Soil Carbon", "DACCS", "Afforestation"))

color <- c(
  "BECCS" = "#4DAF4A",
  "Biochar" = "#E69F00",
  "Soil Carbon" = "#A65628",
  "Afforestation" = "#1B7837",
  "Enhanced Weather" = "#377EB8",
  "DACCS" = "#984EA3"
)



g6 <- ggplot(all_data) +
  geom_bar(aes(x = factor(Y), y = IAMC_template/1000, fill = VEMF), 
           stat = "identity", position = "stack", width = 0.9) +
  scale_y_continuous(name = "Carbon removal (Gt)") +
  scale_x_discrete(name = "Year") +
  facet_grid(REMF ~ scenario, scales = "free_y") +  # ← Facet by Region × Scenario
  scale_fill_manual(values = color, guide = guide_legend(reverse = FALSE)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_1 +
  labs(fill = NULL)

print(g6)


ggsave(
  filename = file.path(output_dir, "Fig6.png"),
  plot = g6,
  width = 14,
  height = 8,
  units = "in",
  dpi = 300,
  bg = "white"
)


# Fig. 7 ------------------------------------------------------------------


df0 <- rgdx.param("AnalysisExpenditure.gdx", "PoVExp")

df1 <- df0 %>% 
  filter(R == "WLD") %>% 
  rename(Y = "Y") %>% 
  filter(TH == "pop_2.15") %>% 
  mutate(
    Ref = case_when(
      Ref == "SSP2_BaU_NoCC_No" ~ "NCP",          
      Ref == "SSP2_400C_2030CP_NoCC_No" ~ "1.5C-0%",
      Ref == "SSP2_400C_2030CP_trs20_NoCC_No" ~ "1.5C-20%",
      Ref == "SSP2_400C_2030CP_trs40_NoCC_No" ~ "1.5C-40%",
      Ref == "SSP2_400C_2030CP_trs60_NoCC_No" ~ "1.5C-60%",
      Ref == "SSP2_400C_2030CP_trs80_NoCC_No" ~ "1.5C-80%",
      Ref == "SSP2_400C_2030CP_trs100_GDPP_NoCC_No" ~ "1.5C-100%"
    )
  )

cpt <- function(data, title) {
  data0 <- data %>% mutate(Y = as.numeric(as.character(Y)))
  data1 <- data0 %>% filter(Y >= 2030, Y <= 2050, !is.na(Ref))
  
  ggplot(data1, aes(x = Y, y = PoVExp / 1e6, group = Ref, color = Ref)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    scale_x_continuous(breaks = seq(2030, 2050, by = 5)) +
    labs(
      y = "Poverty headcount (million)",
      x = "Year",
      color = "Scenario"
    ) +
    scale_color_manual(
      name = "Scenario",
      values = c(
        "NCP"     = "black",
        "1.5C-0%"   = "grey50",
        "1.5C-20%"  = "#9ECAE1",
        "1.5C-40%"  = "#6BAED6",
        "1.5C-60%"  = "#4292C6",
        "1.5C-80%"  = "#2171B5",
        "1.5C-100%" = "#08306B"
      ),
      breaks = c("NCP", "1.5C-0%", "1.5C-20%", "1.5C-40%", "1.5C-60%", "1.5C-80%", "1.5C-100%"),
      labels = c("NCP", "1.5C-0%", "1.5C-20%", "1.5C-40%", "1.5C-60%", "1.5C-80%", "1.5C-100%")
    ) +
    theme_1
}

g7_1 <- cpt(df1, "2.15 $/day/capita")



region_colors <- c(
  "OECD90+EU" = "#1b9e77",
  "Asia" = "#d95f02",
  "Former Soviet Union" = "#7570b3",
  "Middle East and Africa" = "#e7298a",
  "Latin America" = "#66a61e"
)

df_PoVExp <- rgdx.param("AnalysisExpenditure.gdx", "PoVExp")

scenario_mapping <- data.frame(
  scenario = c("SSP2_BaU_NoCC_No", 
               "SSP2_400C_2030CP_NoCC_No",
               "SSP2_400C_2030CP_trs100_GDPP_NoCC_No", 
               "SSP2_400C_2030CP_trs60_NoCC_No", 
               "SSP2_400C_2030CP_trs40_NoCC_No", 
               "SSP2_400C_2030CP_trs20_NoCC_No", 
               "SSP2_400C_2030CP_trs80_NoCC_No"),
  label = c("BaU", "1.5C-0%", "1.5C-100%", "1.5C-60%", "1.5C-40%", "1.5C-20%", "1.5C-80%"),
  stringsAsFactors = FALSE
)

region_labels <- c(
  "R5OECD90+EU" = "OECD90+EU",
  "R5ASIA" = "Asia",
  "R5LAM" = "Latin America",
  "R5REF" = "Former Soviet Union",
  "R5MAF" = "Middle East and Africa",
  "WLD" = "World"
)

df_2_15_2050 <- df_PoVExp %>%
  rename(Year = Y) %>%
  filter(TH == "pop_2.15", Year == 2050) %>%
  mutate(
    Ref = scenario_mapping$label[match(Ref, scenario_mapping$scenario)],
    Region = recode(R, !!!region_labels)
  ) %>%
  filter(!is.na(Ref), Region %in% region_labels) %>%
  mutate(PoVExp_Million = PoVExp / 1e6)


df_BaU <- df_2_15_2050 %>% filter(Ref == "BaU") %>% select(Region, PoVExp_Million)

target_refs <- c("1.5C-0%", "1.5C-20%", "1.5C-40%", "1.5C-60%", "1.5C-80%", "1.5C-100%")

df_diff <- map_dfr(target_refs, function(ref_label) {
  df_2_15_2050 %>%
    filter(Ref == ref_label) %>%
    left_join(df_BaU, by = "Region", suffix = c("", "_BaU")) %>%
    mutate(
      Ref = ref_label,
      value = PoVExp_Million - PoVExp_Million_BaU
    ) %>%
    select(Region, Ref, value) %>%
    filter(Region != "World")
})

df_diff$Ref <- factor(df_diff$Ref, levels = target_refs)


g7_2 <- ggplot(df_diff, aes(x = Ref, y = value, fill = Region)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = region_colors) +
  labs(x = "Reference", y = "Additional Poverty (million)") +
  guides(fill = guide_legend(ncol = 1)) +
  theme_1 



g7_1_with_legend <- g7_1 + theme(legend.position = "right")
g7_1_legend <- get_legend(g7_1_with_legend)
plot(g7_1_legend)

g7_2_with_legend <- g7_2 + theme(legend.position = "right")
g7_2_legend <- get_legend(g7_2_with_legend)
plot(g7_2_legend)

g7 <-ggdraw() +
  draw_plot(g7_1+theme(legend.position='none'),x=0,y=0,width=0.4,height=1) +
  draw_plot(g7_2+theme(legend.position='none'),x=0.4,y=0,width=0.4,height=1) +
  draw_plot(g7_1_legend,x=0.81,y=0.35,width=0.1,height=0.7) +
  draw_plot(g7_2_legend,x=0.84,y=0.05,width=0.1,height=0.7) +
  draw_plot_label(
    label=c('a','b'),
    x=c(0,0.4),
    y=c(1,1),
    size=14
  )

plot(g7)


ggsave(
  filename = file.path(output_dir, "Fig7.png"),
  plot = g7,
  width = 16,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)


# S_Fig. 1-3 --------------------------------------------------------------

R17_map <-  tribble(~short,~normal,~long,~JPN,~color,
                    "JPN","Japan","Japan (JPN)","日本","palegreen3",
                    "USA","United States","United States (USA)","アメリカ","darkturquoise",
                    "CAN","Canada","Canada (CAN)","カナダ","goldenrod1",
                    "XE25","EU25","EU25 (XE25)","EU","tan1",
                    "XOC","Oceania","Oceania (XOC)","オセアニア","coral3",
                    "TUR","Turkey","Turkey (TUR)","トルコ","darkkhaki",
                    "XER","Rest of Europe","Rest of Europe (XER)","EUを除くヨーロッパ","lightskyblue1",
                    "CIS","Former Soviet Union","Former Soviet Union (CIS)","旧ソビエト連邦","steelblue3",
                    "CHN","China","China (CHN)","中国","khaki1",
                    "IND","India","India (IND)","インド","darkorange1",
                    "XSE","Southeast Asia","Southeast Asia (XSE)","その他の東・東南アジア","orchid3",
                    "XSA","Rest of Asia","Rest of Asia (XSA)","その他アジア","mediumpurple1",
                    "BRA","Brazil","Brazil (BRA)","ブラジル","lightcoral",
                    "XLM","Rest of South America","Rest of South America (XLM)","ラテンアメリカ","tan",
                    "XME","Middle East","Middle East (XME)","中東","darkolivegreen",
                    "XNF","North Africa","North Africa (XNF)","北アフリカ","chartreuse3",
                    "XAF","Rest of Africa","Rest of Africa (XAF)","サブサハラアフリカ","slateblue3")

R5_map <-  tribble(~short,~normal,~long,~JPN,~color,
                   "R5ASIA","Asia","Asia (R5ASIA)","アジア","#d95f02",
                   "R5REF","Eastern Europe and the Former Soviet Union","Eastern Europe and the Former Soviet Union (R5REF)","東ヨーロッパおよび旧ソビエト連邦","#7570b3",
                   "R5LAM","Latin America and the Caribbean","Latin America and the Caribbean (R5LAM)","アメリカおよびカリブ海地域","#66a61e",
                   "R5MAF","Middle East and Africa","Middle East and Africa (R5MAF)","中東およびアフリカ","#e7298a",
                   "R5OECD90+EU","OECD+EU","OECD+EU (R5OECD90+EU)","OECDおよびEU","#1b9e77")

R2_map <-  tribble(~short,~normal,~long,~JPN,~color,
                   "R2OECD","OECD","OECD (R2OECD)","先進国","coral3",
                   "R2NonOECD","NonOECD","NonOECD (R2NonOECD)","途上国","darkturquoise")

THEME <- theme_bw() +
  theme(panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(size = 12),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        strip.background = element_blank(),
        plot.margin = margin(0, 0, 0, 0))


df_world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name != "Antarctica")

map_r_Ragg <- read_delim(
  "RegionmapRagg.map",
  delim = "\t.\t",
  col_names = c("R", "Ragg_tmp"),
  show_col_types = FALSE
) %>% 
  mutate(Ragg = gsub('"', '', Ragg_tmp)) %>% 
  select(-Ragg_tmp)

map_R17agg <- map_r_Ragg %>% 
  filter(grepl("R17",Ragg)) %>% 
  rename(R17 = Ragg)

map_R5agg <- map_r_Ragg %>% 
  filter(grepl("R5",Ragg)) %>% 
  rename(R5 = Ragg)

map_R2agg <- map_r_Ragg %>% 
  filter(grepl("R2",Ragg)) %>% 
  rename(R2 = Ragg) 

df_region_map <- df_world %>% 
  left_join(map_R17agg, by = c("iso_a3_eh" = "R")) %>% 
  left_join(map_R5agg,  by = c("iso_a3_eh" = "R")) %>% 
  left_join(map_R2agg,  by = c("iso_a3_eh" = "R")) %>% 
  mutate(R17 = case_when(iso_a3_eh == "TWN" ~ "CHN",
                         iso_a3_eh == "ESH" ~ "XAF",
                         sovereignt == "Somaliland" ~ "XAF",
                         sovereignt == "Kosovo" ~ "XER",
                         TRUE ~ R17),
         R5 = case_when(iso_a3_eh == "TWN" ~ "R5ASIA",
                        iso_a3_eh == "ESH" ~ "R5MAF",
                        sovereignt == "Somaliland" ~ "R5MAF",
                        sovereignt == "Kosovo" ~ "R5OECD90+EU",
                        TRUE ~ R5),
         R2 = case_when(iso_a3_eh == "TWN" ~ "R2NonOECD",
                        iso_a3_eh == "ESH" ~ "R2NonOECD",
                        sovereignt == "Somaliland" ~ "R2NonOECD",
                        sovereignt == "Kosovo" ~ "R2OECD",
                        TRUE ~ R2))

sg1 <- df_region_map %>% 
  filter(!is.na(R17)) %>% 
  mutate(R17 = gsub("R17", "", R17)) %>% 
  left_join(R17_map, by = c("R17" = "short")) %>% 
  mutate(R17 = factor(long, levels = R17_map$long)) %>%     
  ggplot(aes(fill = R17), color = "black") +
  geom_sf(size = 0.2) +
  scale_fill_manual(values = setNames(R17_map$color, R17_map$long),     
                    na.value = "black") +
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90)) +
  THEME +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))       

ggsave(
  filename = file.path(output_dir, "SF1.png"),
  plot = sg1,
  width = 12,
  height = 6,
  units = "in",
  dpi = 300,
  limitsize = FALSE
)

sg2 <- df_region_map %>% 
  filter(!is.na(R5)) %>% 
  left_join(R5_map, by = c("R5" = "short")) %>% 
  mutate(R5 = factor(long, levels = R5_map$long)) %>%     
  ggplot(aes(fill = R5), color = "black") +
  geom_sf(size = 0.2) +
  scale_fill_manual(values = setNames(R5_map$color, R5_map$long),   
                    na.value = "black") +
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90)) +
  THEME +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))     

ggsave(
  filename = file.path(output_dir, "SF2.png"),
  plot = sg2,
  width = 12,
  height = 6,
  units = "in",
  dpi = 300,
  limitsize = FALSE
)

sg3 <- df_region_map %>% 
  filter(!is.na(R2)) %>% 
  left_join(R2_map, by = c("R2" = "short")) %>% 
  mutate(R2 = factor(long, levels = R2_map$long)) %>%     
  ggplot(aes(fill = R2), color = "black") +
  geom_sf(size = 0.2) +
  scale_fill_manual(values = setNames(R2_map$color, R2_map$long),   
                    na.value = "black") +
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90)) +
  THEME

ggsave(
  filename = file.path(output_dir, "SF3.png"),
  plot = sg3,
  width = 12,
  height = 6,
  units = "in",
  dpi = 300,
  limitsize = FALSE
)


# S_Fig. 4 ---------------------------------------------------------------------


df <- rgdx.param("global_17_SSP2_400C_2030CP_trs100_GDPP_NoCC_No.gdx", "GDP_load")
region <- c("CHN", "IND", "XAF", "XNF", "XME", "BRA", "XSE", "XSA", "CIS", "XLM")
df_filtered <- df %>%
  mutate(i = as.numeric(as.character(i))) %>%
  filter(i >= 2030, i <= 2050, j %in% region) %>%
  rename(Region = "j")
sg4_1 <- ggplot(df_filtered, aes(x = i, y = value / 100000, color = Region, group = Region)) +
  geom_point(size = 3, shape = 21, fill = "white", stroke = 1.2)+
  geom_line(linewidth = 1) +      
  scale_x_continuous(             
    breaks = seq(2030, 2050, by = 5)
  ) +
  labs(y = "GDP (trillion US$2010)", x = "Year") +
  theme_1



df <- rgdx.param("global_17_SSP2_400C_2030CP_trs100_GDPP_NoCC_No.gdx", "Ppopulation")
region <- c("CHN", "IND", "XAF", "XNF", "XME", "BRA", "XSE", "XSA", "CIS", "XLM")
df_filtered <- df %>%
  mutate(i = as.numeric(as.character(i))) %>%
  filter(i >= 2030, i <= 2050, j %in% region) %>%
  rename(Region = "j")
sg4_2 <- ggplot(df_filtered, aes(x = i, y = value / 1000000, group = Region, color = Region)) +
  geom_point(size = 3, shape = 21, fill = "white", stroke = 1.2)+
  geom_line(linewidth = 1) +      
  scale_x_continuous(
    breaks = seq(2030, 2050, by = 5),
    name = "Year"
  ) +
  labs(y = "Population (billion people)") +
  theme_1


sg4_1_with_legend <- sg4_1 + theme(legend.position = "right")
sg4_1_legend <- get_legend(sg4_1_with_legend)

sg4_2_with_legend <- sg4_2 + theme(legend.position = "right")
sg4_2_legend <- get_legend(sg4_2_with_legend)

sg4 <-ggdraw() +
  draw_plot(sg4_1+theme(legend.position='none'),x=0,y=0,width=0.4,height=1) +
  draw_plot(sg4_2+theme(legend.position='none'),x=0.4,y=0,width=0.4,height=1) +
  draw_plot(sg4_2_legend,x=0.83,y=0.2,width=0.1,height=0.7) +
  draw_plot_label(
    label=c('a','b'),
    x=c(0,0.4),
    y=c(1,1),
    size=14
  )

ggsave(
  filename = file.path(output_dir, "SF4.png"),
  plot = sg4,
  width = 16,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
  )


# S_Fig. 5 ----------------------------------------------------------------


gdx_files <- list(
  "SSP2_BaU_NoCC_No.gdx" = "NCP",
  "SSP2_400C_2030CP_NoCC_No.gdx" = "1.5C-0%",
  "SSP2_400C_2030CP_trs100_GDPP_NoCC_No.gdx" = "1.5C-100%"
)

start_year <- 2030  
end_year <- 2050    
textsize <- 14  

region_code <- c("World", "R5OECD90+EU", "R2NonOECD", "R5ASIA", "R5REF", "R5MAF", "R5LAM")  
OECDorNON <- "on"  
non_OECD <- if (OECDorNON == "on") c("R5ASIA", "R5REF", "R5MAF", "R5LAM") else NULL

color_ene <- c("Coal|w/o CCS" = "grey50", "Coal|w/ CCS" = "grey30", "Oil|w/o CCS" = "tan3",
               "Oil|w/ CCS" = "sandybrown", "Gas|w/o CCS" = "lightgoldenrod", "Gas|w/ CCS" = "lightgoldenrod3",
               "Hydro" = "lightsteelblue", "Nuclear" = "moccasin", "Solar" = "lightsalmon", "Wind" = "lightskyblue3",
               "Biomass|w/o CCS" = "darkolivegreen2", "Biomass|w/ CCS" = "darkolivegreen4", "Geothermal" = "peru")



process_data <- function(data, region_code, start_year, end_year, OECDorNON, non_OECD) {
  df2 <- data %>% filter(REMF %in% region_code)
  df2$Y <- as.numeric(as.character(df2$Y))
  df3 <- df2 %>% filter(Y %in% seq(start_year, end_year, 5))
  df4 <- df3 %>% filter(VEMF %in% c("Prm_Ene_Coa_w_CCS", "Prm_Ene_Coa_wo_CCS", 
                                    "Prm_Ene_Gas_w_CCS", "Prm_Ene_Gas_wo_CCS", 
                                    "Prm_Ene_Oil_w_CCS", "Prm_Ene_Oil_wo_CCS", 
                                    "Prm_Ene_Geo", "Prm_Ene_Hyd", 
                                    "Prm_Ene_Solar", "Prm_Ene_Win", 
                                    "Prm_Ene_Oth", "Prm_Ene_Nuc", 
                                    "Prm_Ene_Bio_w_CCS", "Prm_Ene_Bio_wo_CCS"))
  
  if (OECDorNON == "on") {
    df4_OECD <- df4 %>% filter(REMF == "R5OECD90+EU")
    df4_OECD$REMF <- gsub("R5OECD90+EU", "OECD", df4_OECD$REMF, fixed = TRUE)
    
    df4_nonOECD <- df4 %>% filter(REMF == "R2NonOECD")
    df4_nonOECD$REMF <- gsub("R2NonOECD", "NonOECD", df4_nonOECD$REMF, fixed = TRUE)
    
    
    df4 <- rbind(df4_OECD, df4_nonOECD)
  }
  
  df4$VEMF <- gsub("Prm_Ene_Oth", "Other", df4$VEMF)
  df4$VEMF <- gsub("Prm_Ene_Geo", "Geothermal", df4$VEMF)
  df4$VEMF <- gsub("Prm_Ene_Hyd", "Hydro", df4$VEMF)
  df4$VEMF <- gsub("Prm_Ene_Solar", "Solar", df4$VEMF)
  df4$VEMF <- gsub("Prm_Ene_Win", "Wind", df4$VEMF)
  df4$VEMF <- gsub("Prm_Ene_Nuc", "Nuclear", df4$VEMF)
  df4$VEMF <- gsub("Prm_Ene_Bio_w_CCS", "Biomass|w/ CCS", df4$VEMF)
  df4$VEMF <- gsub("Prm_Ene_Bio_wo_CCS", "Biomass|w/o CCS", df4$VEMF)
  df4$VEMF <- gsub("Prm_Ene_Gas_w_CCS", "Gas|w/ CCS", df4$VEMF)
  df4$VEMF <- gsub("Prm_Ene_Gas_wo_CCS", "Gas|w/o CCS", df4$VEMF)
  df4$VEMF <- gsub("Prm_Ene_Oil_w_CCS", "Oil|w/ CCS", df4$VEMF)
  df4$VEMF <- gsub("Prm_Ene_Oil_wo_CCS", "Oil|w/o CCS", df4$VEMF)
  df4$VEMF <- gsub("Prm_Ene_Coa_w_CCS", "Coal|w/ CCS", df4$VEMF)
  df4$VEMF <- gsub("Prm_Ene_Coa_wo_CCS", "Coal|w/o CCS", df4$VEMF)
  
  return(df4)
}

df_list <- lapply(names(gdx_files), function(file_name) {
  data <- rgdx.param(file_name, "IAMC_template")
  df <- process_data(data, region_code, start_year, end_year, OECDorNON, non_OECD)
  
  if (nrow(df) > 0) {
    df$scenario <- gdx_files[[file_name]]
  }
  return(df)
})

df5 <- do.call(rbind, df_list)

df5$scenario <- factor(df5$scenario, levels = c("NCP", "1.5C-0%", "1.5C-100%"))
df5$REMF <- factor(df5$REMF, levels = c("OECD",  "NonOECD"))
df5$VEMF <- factor(df5$VEMF, levels = c("Coal|w/ CCS", "Coal|w/o CCS","Oil|w/ CCS", "Oil|w/o CCS","Gas|w/ CCS", "Gas|w/o CCS","Geothermal","Hydro","Nuclear","Solar", "Wind","Biomass|w/ CCS","Biomass|w/o CCS"))


sg5 <- ggplot(data = df5) +
  geom_bar(
    mapping = aes(x = factor(Y), y = IAMC_template, fill = VEMF),
    stat = "identity",
    position = "stack",
    width = 0.9 
  ) +
  ylab("Primary energy (EJ/yr)") +
  facet_grid(REMF ~ scenario, scales = "free_y") +
  scale_fill_manual(
    values = color_ene,
    breaks = c("Coal|w/ CCS", "Coal|w/o CCS",
               "Oil|w/ CCS", "Oil|w/o CCS",
               "Gas|w/ CCS", "Gas|w/o CCS",
               "Geothermal", 
               "Hydro", 
               "Nuclear",
               "Solar", 
               "Wind", 
               "Biomass|w/ CCS", 
               "Biomass|w/o CCS")
  ) +
  labs(fill = "", x = "Year") +
  theme_1 +
  theme(
    text = element_text(size = textsize),
    axis.text.x = element_text(angle = 45, size = textsize, hjust = 1),
    legend.position = "right",
    strip.background = element_blank()
  )


ggsave(
  filename = file.path(output_dir, "SF5.png"),
  plot = sg5,
  width = 12,
  height = 8,
  units = "in",
  dpi = 300,
  bg = "white"
)


# S_Fig. 6 ----------------------------------------------------------------


gdx_files <- c(  "SSP2_BaU_NoCC_No.gdx",
                 "SSP2_400C_2030CP_NoCC_No.gdx" ,
                 "SSP2_400C_2030CP_trs100_GDPP_NoCC_No.gdx")
scenarios <- c("NCP", "1.5C-0%", "1.5C-100%")  

prm_vector <- c("Prm_Ene_Coa_w_CCS", "Prm_Ene_Coa_wo_CCS", "Prm_Ene_Gas_w_CCS", "Prm_Ene_Gas_wo_CCS", "Prm_Ene_Oil_w_CCS", "Prm_Ene_Oil_wo_CCS", 
                "Prm_Ene_Solar", "Prm_Ene_Win",  "Prm_Ene_Nuc", "Prm_Ene_Bio_w_CCS", "Prm_Ene_Bio_wo_CCS")
sec_vector <- gsub("Prm_Ene", "Sec_Ene_Ele", prm_vector)

region_code <- c("World", "USA", "XE25", "XER", "TUR", "XOC", "CHN", "IND", "JPN", "XSE", "XSA", "CAN", "BRA", "XLM", "CIS", "XME", "XNF", "XAF")
region_code <- c("R5ASIA", "R5OECD90+EU", "R5REF", "R5MAF", "R5LAM", "World")
region_code <- c("R2NonOECD", "R5OECD90+EU")


years <- seq(2030, 2050, 5)

process_data <- function(gdx_file, scenario, region_code, years, sec_vector) {
  df <- rgdx.param(gdx_file, "IAMC_template") %>%
    filter(REMF %in% region_code) %>%
    filter(Y %in% years) %>%
    filter(VEMF %in% sec_vector)
  
  df$scenario <- scenario
  
  df$VEMF <- gsub("Sec_Ene_Ele_Solar", "Solar", df$VEMF)
  df$VEMF <- gsub("Sec_Ene_Ele_Win", "Wind", df$VEMF)
  df$VEMF <- gsub("Sec_Ene_Ele_Nuc", "Nuclear", df$VEMF)
  df$VEMF <- gsub("Sec_Ene_Ele_Gas_w_CCS|Sec_Ene_Ele_Gas_wo_CCS|Sec_Ene_Ele_Oil_w_CCS|Sec_Ene_Ele_Oil_wo_CCS|
                  Sec_Ene_Ele_Coa_w_CCS|Sec_Ene_Ele_Coa_wo_CCS", 
                  "Fossil Fuels", df$VEMF)
  df$VEMF <- gsub("Sec_Ene_Ele_Bio_w_CCS|Sec_Ene_Ele_Bio_wo_CCS", "Biomass", df$VEMF)
  
  return(df)
}

df_all <- bind_rows(
  lapply(1:length(gdx_files), function(i) {
    process_data(gdx_files[i], scenarios[i], region_code, years, sec_vector)
  })
)

df_all$scenario <- factor(df_all$scenario, levels = c("NCP", "1.5C-0%", "1.5C-100%"))

df_all <- df_all %>%
  mutate(REMF = recode(REMF,
                       "R5OECD90+EU" = "OECD",
                       "R2NonOECD"  = "NonOECD"))

df_all$REMF <- factor(df_all$REMF, levels = c("OECD", "NonOECD"))
df_all$VEMF <- factor(df_all$VEMF, levels = c("Fossil Fuels", "Nuclear", "Solar","Wind","Biomass","Sec_Ene_Ele_Coa_w_CCS"))
df_all$Y <- factor(df_all$Y)   


sg6 <- ggplot(df_all, aes(x = Y, y = IAMC_template, fill = VEMF)) +
  geom_bar(stat = "identity", position = "stack", width = 0.9) +
  facet_grid(REMF ~ scenario, scales = "free_y") +
  ylab("Power generation (EJ/yr)") +
  scale_fill_manual(values = c(
    "Fossil Fuels"  = "grey50", 
    "Nuclear"       = "moccasin", 
    "Solar"         = "lightsalmon",
    "Wind"          = "lightskyblue3",
    "Biomass"       = "darkolivegreen2"
  )) +
  labs(x = "Year", fill = NULL) +
  theme_1 +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot(sg6)

ggsave(
  filename = file.path(output_dir, "SF6.png"),
  plot = sg6,
  width = 12,
  height = 8,
  units = "in",
  dpi = 300,
  bg = "white"
)


# S_Fig. 7 ----------------------------------------------------------------


years <- seq(2030, 2050, 5)

load_and_format_data <- function(gdx_file, scenario_name, region_code, years) {
  df <- rgdx.param(gdx_file, "IAMC_template") %>%
    filter(REMF %in% region_code) %>%
    filter(Y %in% years) %>%
    filter(VEMF %in% c("Fin_Ene_Ele",
                       "Fin_Ene_Gas",
                       "Fin_Ene_Heat",
                       "Fin_Ene_Hyd",
                       "Fin_Ene_Liq_Oil",
                       "Fin_Ene_Liq_Bio",
                       "Fin_Ene_SolidsCoa",
                       "Fin_Ene_SolidsBio"))
  
  df$VEMF <- gsub("Fin_Ene_Ele", "Electricity", df$VEMF)
  df$VEMF <- gsub("Fin_Ene_Gas", "Gas", df$VEMF)
  df$VEMF <- gsub("Fin_Ene_Heat", "Heat", df$VEMF)
  df$VEMF <- gsub("Fin_Ene_Hyd", "Hydrogen", df$VEMF)
  df$VEMF <- gsub("Fin_Ene_Liq_Oil", "Oil", df$VEMF)
  df$VEMF <- gsub("Fin_Ene_Liq_Bio", "Biofuel", df$VEMF)
  df$VEMF <- gsub("Fin_Ene_SolidsCoa", "Coal", df$VEMF)
  df$VEMF <- gsub("Fin_Ene_SolidsBio", "Biomass", df$VEMF)
  
  df$scenario <- scenario_name
  
  return(df)
}

scenarios <- list(
  list(gdx_file = "SSP2_BaU_NoCC_No.gdx", scenario_name = "NCP"),
  list(gdx_file = "SSP2_400C_2030CP_NoCC_No.gdx", scenario_name = "1.5C-0%"),
  list(gdx_file = "SSP2_400C_2030CP_trs100_GDPP_NoCC_No.gdx", scenario_name = "1.5C-100%")
)

region_code <- c("R2NonOECD", "R5OECD90+EU")

all_data <- purrr::map_dfr(scenarios, function(scenario) {
  load_and_format_data(scenario$gdx_file, scenario$scenario_name, region_code, years)
})

all_data$scenario <- factor(all_data$scenario, levels = c("NCP", "1.5C-0%", "1.5C-100%"))

all_data <- all_data %>%
  mutate(REMF = recode(REMF,
                       "R5OECD90+EU" = "OECD",
                       "R2NonOECD"  = "NonOECD"))

all_data$REMF <- factor(all_data$REMF, levels = c("OECD", "NonOECD"))

all_data$Y <- factor(all_data$Y, levels = years)

color <- c( 
  "Coal"="grey70",
  "Oil"="sandybrown",
  "Gas"="moccasin",
  "Biomass"="#A9D65D",
  "Biofuel"="#DBFF70",
  "Electricity"="lightsteelblue",
  "Heat"="salmon",
  "Hydrogen"="thistle2"
)
all_data$VEMF <- factor(all_data$VEMF, levels = c("Coal", "Oil", "Gas", "Biomass", "Biofuel", "Electricity","Heat","Hydrogen"))

sg7 <- ggplot(data = all_data) +
  geom_bar(aes(x = Y, y = IAMC_template, fill = VEMF), 
           stat = "identity", width = 0.9) + 
  ylab("Final Energy (EJ/yr)") +
  xlab("Year") +
  facet_grid(REMF ~ scenario, scales = "free_y") +
  scale_fill_manual(values = color) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  labs(fill = NULL) +
  theme_1 + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = file.path(output_dir, "SF7.png"),
  plot = sg7,
  width = 12,
  height = 8,
  units = "in",
  dpi = 300,
  bg = "white"
)


# S_Fig. 8 ----------------------------------------------------------------

scenario_mapping <- data.frame(
  scenario = c("SSP2_BaU_NoCC_No", 
               "SSP2_400C_2030CP_NoCC_No",
               "SSP2_400C_2030CP_trs100_GDPP_NoCC_No", 
               "SSP2_400C_2030CP_trs100_POP_NoCC_No", 
               "SSP2_800C_2030CP_NoCC_No", 
               "SSP2_800C_2030CP_trs100_NoCC_No", 
               "SSP2_400C_2030CP_trs40_POP_NoCC_No", 
               "SSP2_800C_2030CP_trs40_NoCC_No"),
  label = c("BaU", "1.5C-0%", "1.5C-100%","1.5C-100%_POP","2C-0%","2C-100%" , "1.5C-40%_POP", "2C-40%"),
  stringsAsFactors = FALSE
)

region_labels <- c("WLD" = "World")

target_refs <- c("1.5C-0%", "1.5C-100%", "1.5C-40%_POP", "1.5C-100%_POP", "2C-0%", "2C-40%", "2C-100%")


process_gdx <- function(file, source_label) {
  df <- rgdx.param(file, "PoVExp") %>%
    rename(Year = Y) %>%
    filter(TH == "pop_2.15", Year == 2050) %>%
    mutate(
      Ref = scenario_mapping$label[match(Ref, scenario_mapping$scenario)],
      Region = recode(R, !!!region_labels)
    ) %>%
    filter(!is.na(Ref), Region %in% region_labels) %>%
    mutate(PoVExp_Million = PoVExp / 1e6)
  
  df_BaU <- df %>% filter(Ref == "BaU") %>% select(Region, PoVExp_Million)
  
  df_diff <- map_dfr(target_refs, function(ref_label) {
    df %>%
      filter(Ref == ref_label) %>%
      left_join(df_BaU, by = "Region", suffix = c("", "_BaU")) %>%
      mutate(
        Ref = ref_label,
        value = PoVExp_Million - PoVExp_Million_BaU,
        Source = source_label
      ) %>%
      select(Region, Ref, Source, value)
  })
  
  return(df_diff)
}


df_income <- process_gdx("AnalysisExpenditure_finalPeff.gdx", "Price")
df_price  <- process_gdx("AnalysisExpenditure_finalIeff.gdx", "Income")

df_all <- bind_rows(df_income, df_price)
df_all$Ref <- factor(df_all$Ref, levels = target_refs)

sg8 <- ggplot(df_all, aes(x = Ref, y = value, fill = Source)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    x = "Scenario",
    y = "Additional Poverty (million)"
  ) +
  theme_1 +
  theme(legend.title = element_blank())

plot(sg8)


ggsave(
  filename = file.path(output_dir, "SF8.png"),
  plot = sg8,
  width = 14,
  height = 8,
  units = "in",
  dpi = 300,
  bg = "white"
)


# S_Fig. 9 ------------------------------------------------------------------


thema <- "Emi_Kyo_Gas"

files <- list(
  list(file = "SSP2_800C_2030CP_NoCC_No.gdx", scenario = "2C"),
  list(file = "SSP2_400C_2030CP_NoCC_No.gdx", scenario = "1.5C"),
  list(file = "SSP2_BaU_NoCC_No.gdx", scenario = "NCP")
)

df <- data.frame()

for (file_info in files) {
  gdx_data <- rgdx.param(file_info$file, "IAMC_template")
  df_temp <- gdx_data %>%
    filter(VEMF == thema) %>%
    rename(Year = "Y") %>%
    rename(CP = "IAMC_template") %>%
    mutate(VEMF = file_info$scenario)
  
  df <- rbind(df, df_temp)
}

region <- c("R2NonOECD", "R5OECD90+EU", "World")

df1 <-    df %>% 
  filter(REMF %in% region) 

region_replacement <- c(
  "R5OECD90+EU" = "OECD",
  "RNonOECDexCIS" = "NonOECD_CIS",
  "R5ASIA" = "Asia",
  "R5REF" = "Former Soviet Union",
  "R5MAF" = "Middle East and Africa",
  "R5LAM" = "Latin America",
  "R2NonOECD" = "NonOECD"
)

df1$REMF <- recode(df1$REMF, !!!region_replacement)



sg9_1 <- df1 %>% 
  filter(as.numeric(as.character(Year)) %% 5 == 0) %>%
  ggplot(aes(x = Year, y = CP/1000, group = VEMF, color = VEMF)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +
  scale_x_discrete(
    breaks = c("2010", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
  ) +
  scale_y_continuous(
    name = expression(paste("GHG emission (", GtCO[2], "e", q, ")")),
  ) +
  scale_color_manual(
    name = "scenario",
    values = c(
      "NCP" = "black",
      "2C" = "red",
      "1.5C"   = "blue"      ),
    breaks = c("NCP", "2C", "1.5C"),
    labels = c("NCP", "2C", "1.5C")
  ) + 
  facet_wrap(~ REMF, scales = "free_y") +
  theme_1 +
  theme(
    legend.position = "right",
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold")
  )


thema <- "Prc_Car"

files <- list(
  list(file = "SSP2_800C_2030CP_NoCC_No.gdx", scenario = "2C"),
  list(file = "SSP2_400C_2030CP_NoCC_No.gdx", scenario = "1.5C")
)

df <- data.frame()

for (file_info in files) {
  gdx_data <- rgdx.param(file_info$file, "IAMC_template")
  df_temp <- gdx_data %>%
    filter(VEMF == thema) %>%
    rename(Year = "Y") %>%
    rename(CP = "IAMC_template") %>%
    mutate(VEMF = file_info$scenario)
  
  df <- rbind(df, df_temp)
}

region <- "World"

df1<-df %>% 
  filter(REMF==region)

theme_1 <- theme_bw() +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, size = 16, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        legend.position = "right", 
        #legend.title = element_blank(),
        strip.background = element_blank())

sg9_2 <- df1 %>% 
  filter(as.numeric(as.character(Year)) %% 5 == 0) %>%
  ggplot(aes(x = Year, y = CP, group = VEMF, color = VEMF)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +  
  scale_x_discrete(
    breaks = c("2010", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
  ) +
  scale_color_manual(
    name = "scenario",
    values = c(
      "2C" = "red",
      "1.5C"   = "blue"      ),
    breaks = c("2C", "1.5C"),
    labels = c("2C", "1.5C")
  ) + 
  #geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  labs(y = "Carbon Price (US$2010/t-CO2)") +
  theme_1+
  theme(
    legend.position = "right",
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold")
  )


sg9_1_with_legend <- sg9_1 + theme(legend.position = "right")
sg9_1_legend <- get_legend(sg9_1_with_legend)

sg9_2_with_legend <- sg9_2 + theme(legend.position = "right")
sg9_2_legend <- get_legend(sg9_2_with_legend)

sg9 <-ggdraw() +
  draw_plot(sg9_1+theme(legend.position='none'),x=0,y=0,width=0.45,height=1) +
  draw_plot(sg9_2+theme(legend.position='none'),x=0.45,y=0,width=0.45,height=1) +
  draw_plot(sg9_1_legend,x=0.91,y=0.15,width=0.08,height=0.7) +
  draw_plot_label(
    label=c('a','b'),
    x=c(0,0.45),
    y=c(1,1),
    size=14
  )

plot(sg9)

ggsave(
  filename = file.path(output_dir, "SF9.png"),
  plot = sg9,
  width = 14,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)




# S_Fig. 10 ---------------------------------------------------------------


thema <- "Rev_gov_Tax_Car_Tax"

files <- list(
  list(file = "SSP2_800C_2030CP_trs100_NoCC_No.gdx", scenario = "2C-100%", scale=1.0),
  list(file = "SSP2_400C_2030CP_trs100_GDPP_NoCC_No.gdx", scenario = "1.5C-100%", scale=1.0),
  list(file = "SSP2_400C_2030CP_trs80_NoCC_No.gdx", scenario = "1.5C-80%", scale=0.8),
  list(file = "SSP2_400C_2030CP_trs60_NoCC_No.gdx", scenario = "1.5C-60%", scale=0.6),
  list(file = "SSP2_400C_2030CP_trs40_NoCC_No.gdx", scenario = "1.5C-40%", scale=0.4),
  list(file = "SSP2_400C_2030CP_trs20_NoCC_No.gdx", scenario = "1.5C-20%", scale=0.2),
  list(file = "SSP2_400C_2030CP_NoCC_No.gdx", scenario = "1.5C-0%", scale=0)
)

df <- data.frame()

for (file_info in files) {
  gdx_data <- rgdx.param(file_info$file, "IAMC_template")
  df_temp <- gdx_data %>%
    filter(VEMF == thema) %>%
    rename(Year = "Y") %>%
    rename(CP = "IAMC_template") %>%
    mutate(
      VEMF = file_info$scenario,
      CP   = CP * file_info$scale   
    )%>% 
    filter(as.numeric(as.character(Year)) >= 2030)
  
  df <- rbind(df, df_temp)
}

region <- "R2OECD"

df1<-df %>% 
  filter(REMF==region)

theme_1 <- theme_bw() +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, size = 16, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        legend.position = "right", 
        #legend.title = element_blank(),
        strip.background = element_blank())

sg10 <- df1 %>% 
  #filter(as.numeric(as.character(Year)) %% 5 == 0) %>%
  ggplot(aes(x = Year, y = CP/1000000, group = VEMF, color = VEMF)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +  
  scale_x_discrete(
    breaks = c("2010", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
  ) +
  scale_color_manual(
    name = "scenario",
    values = c(
      "2C-100%"    = "red",
      "1.5C-100%"  = "#08306B",
      "1.5C-80%"   = "#2171B5",
      "1.5C-60%"   = "#4292C6",
      "1.5C-40%"   = "#6BAED6",
      "1.5C-20%"   = "#9ECAE1",
      "1.5C-0%"   = "grey50"      ),
    breaks = c("2C-100%","1.5C-100%", "1.5C-80%", "1.5C-60%", "1.5C-40%", "1.5C-20%", "1.5C-0%"),
    labels = c("2C-100%","1.5C-100%", "1.5C-80%", "1.5C-60%", "1.5C-40%", "1.5C-20%", "1.5C-0%")
  ) + 
  #geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  labs(y = "Carbon Tax Revenue transferred to developing regions  \n(billion US$2010)") +
  theme_1


ggsave(
  filename = file.path(output_dir, "SF10.png"),
  plot = sg10,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)


# S_Fig. 11 ---------------------------------------------------------------

files <- list(
  list(file = "SSP2_400C_2030CP_NoCC_No.gdx", scenario = "1.5C-0%"),
  list(file = "SSP2_400C_2030CP_trs100_GDPP_NoCC_No.gdx", scenario = "1.5C-100%"),
  list(file = "SSP2_800C_2030CP_NoCC_No.gdx", scenario = "2C-0%"),
  list(file = "SSP2_800C_2030CP_trs100_NoCC_No.gdx", scenario = "2C-100%")
)

year <- 2050
region <- c("World", "R5OECD90+EU", "RNonOECDexCIS")
VEMF_list <- c("Pol_Cos_Cns_Los_rat_NPV_5pc")

region_replacement <- c(
  "R5OECD90+EU" = "OECD",
  "RNonOECDexCIS" = "NonOECD",
  "R5ASIA" = "Asia",
  "R5REF" = "Former Soviet Union",
  "R5MAF" = "Middle East and Africa",
  "R5LAM" = "Latin America"
)

plot_list <- data.frame()

for (file_info in files) {
  df_temp <- rgdx.param(file_info$file, "IAMC_template") %>% 
    filter(Y == year) %>%
    filter(REMF %in% region) %>%
    filter(VEMF %in% VEMF_list)
  
  df_temp$scenario <- file_info$scenario
  plot_list <- rbind(plot_list, df_temp)
}

plot_list$VEMF <- gsub("Pol_Cos_Cns_Los_rat_NPV_5pc", "5 %", plot_list$VEMF, fixed = TRUE)
plot_list$REMF <- recode(plot_list$REMF, !!!region_replacement)
plot_list$IAMC_template <- signif(plot_list$IAMC_template, digits = 3)
plot_list <- plot_list %>% arrange(desc(IAMC_template))



sg11 <- ggplot(plot_list, aes(x = REMF, y = IAMC_template,
                           color = scenario, shape = scenario)) +
  geom_point(size = 5) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  scale_y_continuous(name = "Cumulative Consumption loss rate \n from 2030 to 2050 (%)") +
  
  scale_color_manual(
    values = c(
      "1.5C-0%"   = "blue",  
      "1.5C-100%" = "blue",  
      "2C-0%"     = "red",  
      "2C-100%"   = "red"   
    )
  ) +
  scale_shape_manual(
    values = c(
      "1.5C-0%"   = 16, 
      "1.5C-100%" = 17,  
      "2C-0%"     = 16,
      "2C-100%"   = 17
    )
  ) +
  theme_1

plot(sg11)


ggsave(
  filename = file.path(output_dir, "SF11.png"),
  plot = sg11,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)


# S_Fig. 12 ---------------------------------------------------------------



scenario_file_mapping <- data.frame(
  scenario = c("SSP2_400C_2030CP_NoCC_No",
               "SSP2_400C_2030CP_trs40_NoCC_No",
               "SSP2_400C_2030CP_trs100_POP_NoCC_No",
               "SSP2_400C_2030CP_trs100_GDPP_NoCC_No"),
  scenario_label = c("1.5C-0%",
                     "1.5C-40%",
                     "1.5C-100%_POP",
                     "1.5C-100%"),
  stringsAsFactors = FALSE
)

regions <- c("R5REF", "R5LAM", "R5ASIA", "R5MAF", "R5OECD90+EU", "World")
#regions <- c("R5OECD90+EU", "Non-OECD", "World")

df_combined_all <- data.frame()



for (i in 1:nrow(scenario_file_mapping)) {
  gdx_file <- paste0(scenario_file_mapping$scenario[i], ".gdx")
  df_gdx <- rgdx.param(gdx_file, "IAMC_template")
  
  for (region in regions) {
    df_temp <- df_gdx %>%
      filter(REMF == region, VEMF == "Pol_Cos_Cns_Los_rat") %>%
      rename(Year = "Y", LossRate = "IAMC_template") %>%
      mutate(VEMF = scenario_file_mapping$scenario_label[i], Region = region)
    
    df_combined_all <- rbind(df_combined_all, df_temp)
  }
}

df_combined_all <- df_combined_all %>%
  mutate(
    Year = as.numeric(as.character(Year)),  
    REMF = recode(REMF,
                  "R5OECD90+EU" = "OECD90+EU",
                  "R5ASIA" = "Asia",
                  "R5REF" = "Former Soviet Union",
                  "R5MAF" = "Middle East and Africa",
                  "R5LAM" = "Latin America")
  ) %>%
  filter(Year >= 2030)                  



df_combined_all$VEMF <- factor(
  df_combined_all$VEMF,
  levels = c("1.5C-0%", "1.5C-40%", "1.5C-100%", "1.5C-100%_POP")
)

sg12 <- ggplot(df_combined_all, aes(x = Year, y = LossRate, group = VEMF, color = VEMF)) + 
  geom_line(linewidth = 1) +
  geom_point(
    data = df_combined_all %>% filter(Year %% 5 == 0), 
    size = 3
  ) +
  scale_x_continuous(
    limits = c(2030, 2050),
    breaks = seq(2030, 2050, by = 5)
  ) +
  labs(
    y = "Consumption Loss Rate (%)",
    color = "Scenario"
  ) +
  facet_wrap(~ REMF, scales = "free_y") +
  theme_1

plot(sg12)


ggsave(
  filename = file.path(output_dir, "SF12.png"),
  plot = sg12,
  width = 14,
  height = 8,
  units = "in",
  dpi = 300,
  bg = "white"
)


# S_Fig. 13 ---------------------------------------------------------------


df0 <- rgdx.param("AnalysisExpenditure_final.gdx", "PoVExp")

df1 <- df0 %>% 
  filter(R == "WLD") %>% 
  rename(Y = "Y") %>% 
  filter(TH == "pop_2.15") %>% 
  mutate(
    Ref = case_when(
      Ref == "SSP2_BaU_NoCC_No" ~ "NCP",          
      Ref == "SSP2_400C_2030CP_NoCC_No" ~ "1.5C-0%",
      Ref == "SSP2_400C_2030CP_trs100_POP_NoCC_No" ~ "1.5C-100%_POP",
      Ref == "SSP2_800C_2030CP_NoCC_No" ~ "2C-0%",
      Ref == "SSP2_800C_2030CP_trs100_NoCC_No" ~ "2C-100%",
      Ref == "SSP2_400C_2030CP_trs100_GDPP_NoCC_No" ~ "1.5C-100%"
    )
  )

cpt <- function(data, title) {
  data0 <- data %>% mutate(Y = as.numeric(as.character(Y)))
  data1 <- data0 %>% filter(Y >= 2020, Y <= 2050, !is.na(Ref))
  
  ggplot(data1, aes(x = Y, y = PoVExp / 1e6, group = Ref, color = Ref)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    scale_x_continuous(breaks = seq(2020, 2050, by = 10)) +
    labs(
      y = "Poverty headcount (million)",
      x = "Year",
      color = "Scenario"
    ) +
    scale_color_discrete(
      breaks = c("1.5C-0%", "1.5C-100%", "2C-0%", "2C-100%", "1.5C-100%_POP", "NCP"),
      labels = c("1.5C-0%", "1.5C-100%", "2C-0%", "2C-100%", "1.5C-100%_POP", "NCP")
    ) +
    theme_1
}

sg13_1 <- cpt(df1, "2.15 $/day/capita")



region_colors <- c(
  "OECD90+EU" = "#1b9e77",
  "Asia" = "#d95f02",
  "Former Soviet Union" = "#7570b3",
  "Middle East and Africa" = "#e7298a",
  "Latin America" = "#66a61e"
)

df_PoVExp <- rgdx.param("AnalysisExpenditure_final.gdx", "PoVExp")

scenario_mapping <- data.frame(
  scenario = c("SSP2_BaU_NoCC_No", 
               "SSP2_400C_2030CP_NoCC_No",
               "SSP2_400C_2030CP_trs100_GDPP_NoCC_No", 
               "SSP2_400C_2030CP_trs100_POP_NoCC_No", 
               "SSP2_800C_2030CP_NoCC_No", 
               "SSP2_800C_2030CP_trs100_NoCC_No"),
  label = c("BaU", "1.5C-0%", "1.5C-100%", "1.5C-100%_POP", "2C-0%", "2C-100%"),
  stringsAsFactors = FALSE
)

region_labels <- c(
  "R5OECD90+EU" = "OECD90+EU",
  "R5ASIA" = "Asia",
  "R5LAM" = "Latin America",
  "R5REF" = "Former Soviet Union",
  "R5MAF" = "Middle East and Africa",
  "WLD" = "World"
)

df_2_15_2050 <- df_PoVExp %>%
  rename(Year = Y) %>%
  filter(TH == "pop_2.15", Year == 2050) %>%
  mutate(
    Ref = scenario_mapping$label[match(Ref, scenario_mapping$scenario)],
    Region = recode(R, !!!region_labels)
  ) %>%
  filter(!is.na(Ref), Region %in% region_labels) %>%
  mutate(PoVExp_Million = PoVExp / 1e6)


df_BaU <- df_2_15_2050 %>% filter(Ref == "BaU") %>% select(Region, PoVExp_Million)

target_refs <- c("1.5C-0%", "1.5C-100%","1.5C-100%_POP", "2C-0%", "2C-100%")

df_diff <- map_dfr(target_refs, function(ref_label) {
  df_2_15_2050 %>%
    filter(Ref == ref_label) %>%
    left_join(df_BaU, by = "Region", suffix = c("", "_BaU")) %>%
    mutate(
      Ref = ref_label,
      value = PoVExp_Million - PoVExp_Million_BaU
    ) %>%
    select(Region, Ref, value) %>%
    filter(Region != "World")
})

df_diff$Ref <- factor(df_diff$Ref, levels = target_refs)


sg13_2 <- ggplot(df_diff, aes(x = Ref, y = value, fill = Region)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = region_colors) +
  labs(x = "Reference", y = "Additional Poverty (million)") +
  guides(fill = guide_legend(ncol = 1)) +
  theme_1 


sg13_1_with_legend <- sg13_1 + theme(legend.position = "right")
sg13_1_legend <- get_legend(sg13_1_with_legend)

sg13_2_with_legend <- sg13_2 + theme(legend.position = "right")
sg13_2_legend <- get_legend(sg13_2_with_legend)

sg13 <-ggdraw() +
  draw_plot(sg13_1+theme(legend.position='none'),x=0,y=0,width=0.4,height=1) +
  draw_plot(sg13_2+theme(legend.position='none'),x=0.4,y=0,width=0.4,height=1) +
  draw_plot(sg13_1_legend,x=0.83,y=0.35,width=0.1,height=0.7) +
  draw_plot(sg13_2_legend,x=0.84,y=0.05,width=0.1,height=0.7) +
  draw_plot_label(
    label=c('a','b'),
    x=c(0,0.4),
    y=c(1,1),
    size=14
  )

plot(sg13)

ggsave(
  filename = file.path(output_dir, "SF13.png"),
  plot = sg13,
  width = 14,
  height = 8,
  units = "in",
  dpi = 300,
  bg = "white"
)
