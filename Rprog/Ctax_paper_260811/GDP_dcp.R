
library(tidyverse)
library(readxl)
library(gdxrrw)
library(patchwork)
library(scales)
output_dir <- file.path("../..", "output/Figure")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
Mytheme <- theme_bw()+
  theme(text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, size = 16, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        legend.position = "bottom", 
        legend.title = element_blank(),)
# GDP_dcp -----------------------------------------------------------------
year_selected <- c(2050)
region_selected <- c( "USA","XE25","JPN","CAN","XER","XOC","TUR", "CHN","CIS","XLM","IND","XSE","XSA", "BRA", "XME","XNF","XAF" )
region_group_map <- tibble::tibble( region = c("USA","XE25","JPN","CAN","XER","XOC","TUR", "CHN","CIS","XLM","IND","XSE","XSA", "BRA", "XME","XNF","XAF"), 
Region_group = c(
    rep("Provider", 10),  rep("Recipient", 7) ) )
scenario_name <- c( "SSP2_400C_2030CP_NoCC_No", "SSP2_400C_2030CP_15th_NoCC_No",
  "SSP2_BaU_NoCC_No" )
scenario_revise <- function(dataframe) {
  if (str_detect(scenario_name[[i]], "iadjadd") == TRUE) {
    iadj_num <- regmatches( scenario_name[[i]], regexpr("iadjadd([0-9]+)", scenario_name[[i]]) )
    iadj_num <- str_sub(iadj_num, start = 10, end = 11)
    scenario <- rep(paste0("ACF", iadj_num), times = nrow(dataframe))
  } else if (str_detect(scenario_name[[i]], "SSP2_400C_2030CP_NoCC_No") == TRUE) {
    scenario <- rep("Def", times = nrow(dataframe))
  } else if (str_detect(scenario_name[[i]], "SSP2_400C_2030CP_15th_NoCC_No") == TRUE) {
    scenario <- rep("Aid", times = nrow(dataframe))
  } else if (str_detect(scenario_name[[i]], "SSP2_BaU_NoCC_No") == TRUE) {
    scenario <- rep("BaU", times = nrow(dataframe))
  } else {
    scenario <- rep(scenario_name[[i]], times = nrow(dataframe))
  }
  return(scenario)
}
dataframe_list <- vector("list", length = length(scenario_name))
for (i in seq_along(scenario_name)) {
  file_path <- paste0(scenario_name[[i]], ".gdx")
  analysis_GDP_s <- rgdx.param(file_path, "GDP_s")
  SCENARIO <- scenario_revise(analysis_GDP_s)
  analysis_GDP_s <- cbind(SCENARIO, analysis_GDP_s)
  dataframe_list[[i]] <- analysis_GDP_s
}
analysis_GDP_s <- do.call(rbind, dataframe_list)
analysis_GDP_s <- analysis_GDP_s %>%
  rename( year = Y, region = R, category = INS_MCR, value = GDP_s ) %>%
  filter( region %in% region_selected, year %in% year_selected ) %>%
  left_join(region_group_map, by = "region") %>%
  filter(!is.na(Region_group)) %>%
  mutate( category = case_when( category %in% c("ROW", "IMP") ~ "Net export", category == "HURB" ~ "Consumption", category == "GOV"  ~ "Government",
      category == "S-I"  ~ "Investment", TRUE ~ category ) ) %>%
  group_by(year, Region_group, SCENARIO, category) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
analysis_GDP_s <- analysis_GDP_s %>%
  bind_rows( analysis_GDP_s %>%
      filter(category != "Total") %>%
      group_by(year, Region_group, SCENARIO) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(category = "Total") )
analysis_GDP_s_diff <- analysis_GDP_s %>%
  group_by(year, Region_group, category) %>%
  mutate( bau_value = value[SCENARIO == "BaU"][1] ) %>%
  ungroup() %>%
  group_by(year, Region_group) %>%
  mutate( bau_total = value[SCENARIO == "BaU" & category == "Total"][1] ) %>%
  ungroup() %>%
  filter(SCENARIO != "BaU") %>%
  mutate( bau_percent = (value - bau_value) / bau_total ) %>%
  filter(is.finite(bau_percent)) %>%
  mutate( Region_group = factor( Region_group, levels = c("Provider", "Recipient") ), SCENARIO = factor( SCENARIO, levels = c("Def", "Aid") ),
    category = factor( category, levels = c("Consumption", "Government", "Investment", "Net export", "Total") ) )
ipcc_gdp_cols <- c( "Consumption" = "#4575B4", "Government"  = "#4D4D4D", "Investment"  = "#FDAE61", "Net export"  = "#1A9850" )
g <- ggplot() +
  geom_col( data = filter(analysis_GDP_s_diff, category != "Total"), aes( x = SCENARIO, y = bau_percent * 100, fill = category ), width = 0.68, color = "white", linewidth = 0.15
  ) +
  geom_point( data = filter(analysis_GDP_s_diff, category == "Total"), aes( x = SCENARIO, y = bau_percent * 100 ), size = 2.3, color = "black" ) +
  geom_hline( yintercept = 0, linetype = "dashed", linewidth = 0.35, color = "grey35" ) +
  facet_wrap( ~Region_group, ncol = 3, scales = "free_y" ) +
  scale_fill_manual( values = ipcc_gdp_cols, name = NULL ) +
  labs( x = NULL, y = "GDP change from BaU in 2050 (%)" ) +
  Mytheme
plot(g)
name  <- "GDP_dcp.png"
ggsave( filename = file.path(output_dir, name), plot = g, width = 12, height = 8, dpi = 600, )

