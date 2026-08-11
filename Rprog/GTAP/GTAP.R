# GDP_dcp -----------------------------------------------------------------

year_selected <- c(2050)

region_selected <- c(
  "USA","XE25","JPN","CAN","XER","XOC","TUR",
  "CHN","IND","XSE","XSA",
  "BRA","XLM",
  "CIS",
  "XME","XNF","XAF"
)

region_group_map <- tibble::tibble(
  region = c(
    "USA","XE25","JPN","CAN","XER","XOC","TUR",
    "CHN","IND","XSE","XSA",
    "BRA","XLM",
    "XME","XNF","XAF",
    "CIS"
  ),
  Region_group = c(
    rep("OECD90+EU", 7),
    rep("R5ASIA", 4),
    rep("R5LAM", 2),
    rep("R5MAF", 3),
    "R5REF"
  )
)

scenario_name <- c(
  "SSP2_400C_2030CP_base_NoCC_No",
  "SSP2_400C_2030CP_GDP_NoCC_No",
  # "SSP2_400C_2030CP_POP_NoCC_No",
  "SSP2_BaU_NoCC_No"
)

scenario_revise <- function(dataframe) {
  if (str_detect(scenario_name[[i]], "iadjadd") == TRUE) {
    iadj_num <- regmatches(
      scenario_name[[i]],
      regexpr("iadjadd([0-9]+)", scenario_name[[i]])
    )
    iadj_num <- str_sub(iadj_num, start = 10, end = 11)
    scenario <- rep(paste0("ACF", iadj_num), times = nrow(dataframe))
  } else if (str_detect(scenario_name[[i]], "SSP2_400C_2030CP_base_NoCC_No") == TRUE) {
    scenario <- rep("NonAid", times = nrow(dataframe))
  } else if (str_detect(scenario_name[[i]], "SSP2_400C_2030CP_GDP_NoCC_No") == TRUE) {
    scenario <- rep("Aid", times = nrow(dataframe))
  } else if (str_detect(scenario_name[[i]], "SSP2_400C_2030CP_POP_NoCC_No") == TRUE) {
    scenario <- rep("POP", times = nrow(dataframe))
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
  rename(
    year = Y,
    region = R,
    category = INS_MCR,
    value = GDP_s
  ) %>%
  filter(
    region %in% region_selected,
    year %in% year_selected
  ) %>%
  left_join(region_group_map, by = "region") %>%
  filter(!is.na(Region_group)) %>%
  mutate(
    category = case_when(
      category %in% c("ROW", "IMP") ~ "Net export",
      category == "HURB" ~ "Consumption",
      category == "GOV"  ~ "Government",
      category == "S-I"  ~ "Investment",
      TRUE ~ category
    )
  ) %>%
  group_by(year, Region_group, SCENARIO, category) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

analysis_GDP_s <- analysis_GDP_s %>%
  bind_rows(
    analysis_GDP_s %>%
      filter(category != "Total") %>%
      group_by(year, Region_group, SCENARIO) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(category = "Total")
  )

analysis_GDP_s_diff <- analysis_GDP_s %>%
  group_by(year, Region_group, category) %>%
  mutate(
    bau_value = value[SCENARIO == "BaU"][1]
  ) %>%
  ungroup() %>%
  group_by(year, Region_group) %>%
  mutate(
    bau_total = value[SCENARIO == "BaU" & category == "Total"][1]
  ) %>%
  ungroup() %>%
  filter(SCENARIO != "BaU") %>%
  mutate(
    bau_percent = (value - bau_value) / bau_total
  ) %>%
  filter(is.finite(bau_percent)) %>%
  mutate(
    Region_group = factor(
      Region_group,
      levels = c("OECD90+EU", "R5ASIA", "R5LAM", "R5MAF", "R5REF")
    ),
    SCENARIO = factor(
      SCENARIO,
      levels = c("NonAid", "Aid", "POP")
    ),
    category = factor(
      category,
      levels = c("Consumption", "Government", "Investment", "Net export", "Total")
    )
  )

ipcc_gdp_cols <- c(
  "Consumption" = "#4575B4",
  "Government"  = "#4D4D4D",
  "Investment"  = "#FDAE61",
  "Net export"  = "#1A9850"
)

g <- ggplot() +
  geom_col(
    data = filter(analysis_GDP_s_diff, category != "Total"),
    aes(
      x = SCENARIO,
      y = bau_percent * 100,
      fill = category
    ),
    width = 0.68,
    color = "white",
    linewidth = 0.15
  ) +
  geom_point(
    data = filter(analysis_GDP_s_diff, category == "Total"),
    aes(
      x = SCENARIO,
      y = bau_percent * 100
    ),
    size = 2.3,
    color = "black"
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.35,
    color = "grey35"
  ) +
  facet_wrap(
    ~Region_group,
    ncol = 3,
    scales = "free_y"
  ) +
  scale_fill_manual(
    values = ipcc_gdp_cols,
    name = NULL
  ) +
  labs(
    x = NULL,
    y = "GDP differences in 2050 (%)"
  ) +
  Mytheme

plot(g)

name  <- "GDP_dcp.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 8,
  dpi = 600,
)

# (Export change rate)-(import change rate)>>bad example ---------------------------------------

df_base <- rgdx.param(
  "global_17_SSP2_400C_2030CP_base_NoCC_No.gdx",
  "PSAM_value"
)

df_gdp <- rgdx.param(
  "global_17_SSP2_400C_2030CP_GDP_NoCC_No.gdx",
  "PSAM_value"
)

com_categories <- list(
  Trans = c("COM_TRS", "COM_CSS"),
  Min   = c("COM_COA", "COM_OIL", "COM_OMN", "COM_GAS"),
  Manu  = c(
    "COM_FPR", "COM_OMT", "COM_LIN", "COM_PPP", "COM_CRP",
    "COM_NMM", "COM_I_S", "COM_NFM", "COM_OMF"
  ),
  Ene   = c("COM_P_P", "COM_COP", "COM_ELY"),
  Agr   = c(
    "COM_pdr", "COM_wht", "COM_gro", "COM_osd", "COM_oth_a",
    "COM_ctl", "COM_rmk", "COM_oth_l", "COM_FRS"
  )
)

region_group_map <- tibble::tibble(
  Region = c(
    "USA", "XE25", "JPN", "CAN", "XER", "XOC", "TUR",
    "CHN", "IND", "XSE", "XSA",
    "BRA", "XLM",
    "XME", "XNF", "XAF",
    "CIS"
  ),
  Region_group = c(
    rep("OECD90+EU", 7),
    rep("R5ASIA", 4),
    rep("R5LAM", 2),
    rep("R5MAF", 3),
    "R5REF"
  )
)

target_regions <- region_group_map$Region

calc_trade <- function(df, region_codes, flow) {
  partner_col <- if (flow == "exp") "i4" else "i3"
  com_col     <- if (flow == "exp") "i3" else "i4"

  bind_rows(lapply(names(com_categories), function(sector) {
    df %>%
      filter(
        grepl("ROW", .data[[partner_col]]),
        i2 %in% region_codes,
        .data[[com_col]] %in% com_categories[[sector]]
      ) %>%
      left_join(region_group_map, by = c("i2" = "Region")) %>%
      filter(!is.na(Region_group)) %>%
      group_by(i1, Region_group) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(Sector = sector)
  }))
}

calc_flow_diff <- function(flow) {
  base <- calc_trade(df_base, target_regions, flow) %>%
    rename(base = value)

  gdp <- calc_trade(df_gdp, target_regions, flow) %>%
    rename(gdp = value)

  gdp %>%
    left_join(base, by = c("i1", "Region_group", "Sector")) %>%
    mutate(
      i1 = as.numeric(as.character(i1)),
      value = 100 * (gdp - base) / base,
      flow = flow
    ) %>%
    filter(
      i1 >= 2030,
      i1 <= 2050,
      is.finite(value)
    ) %>%
    select(i1, Sector, Region_group, flow, value)
}

exp_diff <- calc_flow_diff("exp") %>%
  rename(exp_diff = value)

imp_diff <- calc_flow_diff("imp") %>%
  rename(imp_diff = value)

df <- exp_diff %>%
  left_join(
    imp_diff,
    by = c("i1", "Sector", "Region_group")
  ) %>%
  mutate(
    value = exp_diff - imp_diff,
    Region_group = factor(
      Region_group,
      levels = c("OECD90+EU", "R5ASIA", "R5LAM", "R5MAF", "R5REF")
    )
  ) %>%
  select(i1, Sector, Region_group, value)

calc_flow_diff <- function(flow) {
  base <- calc_trade(df_base, target_regions, flow) %>%
    rename(base = value)

  gdp <- calc_trade(df_gdp, target_regions, flow) %>%
    rename(gdp = value)

  gdp %>%
    left_join(base, by = c("i1", "Region_group", "Sector")) %>%
    mutate(
      i1 = as.numeric(as.character(i1)),
      value = 100 * (gdp - base) / base,
      flow = flow
    ) %>%
    filter(
      i1 == 2050,
      is.finite(value)
    ) %>%
    select(i1, Sector, Region_group, flow, value)
}
ipcc_sector_cols <- c(
  "Trans" = "#4575B4",
  "Min"   = "#8C510A",
  "Manu"  = "#D73027",
  "Ene"   = "#FDAE61",
  "Agr"   = "#1A9850"
)
g <- ggplot(
  df,
  aes(
    x = Region_group,
    y = value,
    fill = Region_group
  )
) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.35,
    color = "grey35"
  ) +
  geom_col(
    width = 0.72
  ) +
  facet_wrap(
    ~Sector,
    ncol = 3,
    scales = "free_y"
  ) +
  scale_fill_manual(
    values = c(
      "OECD90+EU" = "#4575B4",
      "R5ASIA"    = "#D73027",
      "R5LAM"     = "#1A9850",
      "R5MAF"     = "#FDAE61",
      "R5REF"     = "#8C510A"
    ),
    name = NULL
  ) +
  labs(
    x = NULL,
    y = "Difference in export-import balance in 2050 (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      linewidth = 0.25,
      color = "grey85"
    ),
    axis.text.x = element_text(
      angle = 35,
      hjust = 1,
      color = "grey20"
    ),
    axis.text.y = element_text(color = "grey20"),
    axis.title.y = element_text(
      color = "grey20",
      margin = margin(r = 8)
    ),
    strip.background = element_blank(),
    strip.text = element_text(
      face = "bold",
      size = 11,
      color = "grey15"
    ),
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

print(g)

#Net export change (volume)--------------------------------

df_base <- rgdx.param(
  "global_17_SSP2_400C_2030CP_base_NoCC_No.gdx",
  "PSAM_value"
)

df_gdp <- rgdx.param(
  "global_17_SSP2_400C_2030CP_GDP_NoCC_No.gdx",
  "PSAM_value"
)

com_categories <- list(
  Transport = c("COM_TRS", "COM_CSS"),
  Mining   = c("COM_COA", "COM_OIL", "COM_OMN", "COM_GAS"),
  Manufacturing  = c(
    "COM_FPR", "COM_OMT", "COM_LIN", "COM_PPP", "COM_CRP",
    "COM_NMM", "COM_I_S", "COM_NFM", "COM_OMF"
  ),
  Energy   = c("COM_P_P", "COM_COP", "COM_ELY"),
  Agriculture   = c(
    "COM_pdr", "COM_wht", "COM_gro", "COM_osd", "COM_oth_a",
    "COM_ctl", "COM_rmk", "COM_oth_l", "COM_FRS"
  )
)

region_group_map <- tibble::tibble(
  Region = c(
    "USA", "XE25", "JPN", "CAN", "XER", "XOC", "TUR",
    "CHN", "IND", "XSE", "XSA",
    "BRA", "XLM",
    "XME", "XNF", "XAF",
    "CIS"
  ),
  Region_group = c(
    rep("OECD90+EU", 7),
    rep("R5ASIA", 4),
    rep("R5LAM", 2),
    rep("R5MAF", 3),
    "R5REF"
  )
)

target_regions <- region_group_map$Region

calc_trade_value <- function(df, region_codes, flow) {
  partner_col <- if (flow == "exp") "i4" else "i3"
  com_col     <- if (flow == "exp") "i3" else "i4"

  bind_rows(lapply(names(com_categories), function(sector) {
    df %>%
      filter(
        grepl("ROW", .data[[partner_col]]),
        i2 %in% region_codes,
        .data[[com_col]] %in% com_categories[[sector]]
      ) %>%
      left_join(region_group_map, by = c("i2" = "Region")) %>%
      filter(!is.na(Region_group)) %>%
      group_by(i1, Region_group) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(Sector = sector)
  }))
}

exp_base <- calc_trade_value(df_base, target_regions, "exp") %>%
  rename(exp_base = value)

imp_base <- calc_trade_value(df_base, target_regions, "imp") %>%
  rename(imp_base = value)

exp_gdp <- calc_trade_value(df_gdp, target_regions, "exp") %>%
  rename(exp_gdp = value)

imp_gdp <- calc_trade_value(df_gdp, target_regions, "imp") %>%
  rename(imp_gdp = value)

df_net <- exp_base %>%
  left_join(imp_base, by = c("i1", "Region_group", "Sector")) %>%
  left_join(exp_gdp,  by = c("i1", "Region_group", "Sector")) %>%
  left_join(imp_gdp,  by = c("i1", "Region_group", "Sector")) %>%
  mutate(
    i1 = as.numeric(as.character(i1)),
    net_base = exp_base - imp_base,
    net_gdp  = exp_gdp - imp_gdp,
    value = net_gdp - net_base,
    Region_group = factor(
      Region_group,
      levels = c("OECD90+EU", "R5ASIA", "R5LAM", "R5MAF", "R5REF")
    ),
    Sector = factor(
      Sector,
      levels = c("Transport", "Mining", "Manufacturing", "Energy", "Agriculture")
    )
  ) %>%
  filter(
    i1 == 2050,
    is.finite(value)
  ) %>%
  select(i1, Sector, Region_group, value)

ipcc_region_cols <- c(
  "OECD90+EU" = "#4575B4",
  "R5ASIA"    = "#D73027",
  "R5LAM"     = "#1A9850",
  "R5MAF"     = "#FDAE61",
  "R5REF"     = "#8C510A"
)

g <- ggplot(
  df_net,
  aes(
    x = Region_group,
    y = value,
    fill = Region_group
  )
) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.4,
    color = "black"
  ) +
  geom_col(
    width = 0.72,
    color = NA
  ) +
  facet_wrap(
    ~Sector,
    ncol = 3,
    scales = "free_y"
  ) +
  scale_fill_manual(
    values = ipcc_region_cols,
    name = NULL
  ) +
  labs(
    x = NULL,
    y = "Change in net exports in 2050\n(GDP Aid − NonAid)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      linewidth = 0.25,
      color = "grey85"
    ),
    axis.text.x = element_text(
      angle = 35,
      hjust = 1,
      color = "grey20"
    ),
    axis.text.y = element_text(color = "grey20"),
    axis.title.y = element_text(
      color = "grey20",
      margin = margin(r = 8)
    ),
    strip.background = element_blank(),
    strip.text = element_text(
      face = "bold",
      size = 11,
      color = "grey15"
    ),
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

print(g)


# Normalize net export change by GDP_load -------------------------------------

gdp_base_load <- rgdx.param(
  "global_17_SSP2_400C_2030CP_base_NoCC_No.gdx",
  "GDP_load"
) %>%
  rename(
    i1 = i,
    Region = j,
    gdp_base = value
  ) %>%
  mutate(
    i1 = as.numeric(as.character(i1)),
    gdp_base = as.numeric(gdp_base)
  ) %>%
  filter(
    i1 == 2050,
    Region %in% target_regions
  ) %>%
  left_join(region_group_map, by = "Region") %>%
  filter(!is.na(Region_group)) %>%
  group_by(i1, Region_group) %>%
  summarise(
    gdp_base = sum(gdp_base, na.rm = TRUE),
    .groups = "drop"
  )

df_net_gdp_ratio <- df_net %>%
  left_join(
    gdp_base_load,
    by = c("i1", "Region_group")
  ) %>%
  mutate(
    value_gdp_ratio = 100 * value / gdp_base
  ) %>%
  filter(is.finite(value_gdp_ratio))

g_ratio <- ggplot(
  df_net_gdp_ratio,
  aes(
    x = Region_group,
    y = value_gdp_ratio,
    fill = Region_group
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "black") +
  geom_col(width = 0.72, color = NA) +
  facet_wrap(~Sector, ncol = 3, scales = "free_y") +
  scale_fill_manual(values = ipcc_region_cols, name = NULL) +
  labs(
    x = NULL,
    y = "Change in net exports in 2050\n(% of GDP)"
  ) +
  Mytheme

print(g_ratio)

name  <- "Trade.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g_ratio,
  width = 12,
  height = 8,
  dpi = 600,
)
#Fossil share------------------------


df_pe <- rgdx.param("global_17_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF %in% c("Prm_Ene",
                     "Prm_Ene_Fos")) %>%
  filter(SCENARIO %in% CLP) %>%
  filter(REMF %in% Region) %>%
  mutate(
    SCENARIO = case_when(
      SCENARIO == "SSP2_400C_2030CP_base_NoCC_No" ~ "NonAid",
      SCENARIO == "SSP2_400C_2030CP_GDP_NoCC_No" ~ "Aid",
      TRUE ~ SCENARIO
    ),
    Region = REMF,
    Year = as.numeric(as.character(YEMF)),
    value = as.numeric(IAMC_Template)
  ) %>%
  select(SCENARIO, Region, Year, VEMF, value) %>%
  pivot_wider(names_from = VEMF,
              values_from = value) %>%
  mutate(
    FossilShare = 100 * `Prm_Ene_Fos` / `Prm_Ene`
  )

p <- ggplot(df_pe,
            aes(Year, FossilShare,
                color = SCENARIO,
                linetype = SCENARIO)) +
  geom_line(linewidth = 1) +
  facet_wrap(~Region, nrow = 4) +
  scale_color_manual(values = c(
    "NonAid" = "#D55E00",
    "Aid" = "#0072B2"
  )) +
  labs(
    x = NULL,
    y = "Fossil share of primary energy (%)",
    title = "Share of fossil fuels in primary energy",
    color = NULL,
    linetype = NULL
  ) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

print(p)

##world map--------------------
Region <- c("XE25","JPN","IND","TUR","CHN","USA","XER","XOC","XSE","XSA","CAN","BRA","XLM","CIS","XME","XNF","XAF")

CLP <- c("SSP2_400C_2030CP_base_NoCC_No")

RegionmapRagg.map <- read.table("RegionmapRagg.map", header=FALSE, stringsAsFactors=FALSE)

Region_map <- RegionmapRagg.map %>%
  as_tibble() %>%
  select(iso3c=1, Region=3) %>%
  mutate(Region=str_remove_all(Region,'"'),
         Region=str_remove(Region,"^R17")) %>%
  filter(Region %in% Region)

df_pe <- rgdx.param("global_17_IAMC.gdx","IAMC_template") %>%
  filter(VEMF %in% c("Prm_Ene","Prm_Ene_Fos")) %>%
  filter(SCENARIO %in% CLP, REMF %in% Region) %>%
  mutate(SCENARIO=case_when(
    SCENARIO=="SSP2_400C_2030CP_base_NoCC_No" ~ "NonAid",
    SCENARIO=="SSP2_400C_2030CP_GDP_NoCC_No" ~ "Aid",
    TRUE ~ SCENARIO),
    Region=REMF, Year=as.numeric(as.character(YEMF)),
    value=as.numeric(IAMC_Template)) %>%
  select(SCENARIO,Region,Year,VEMF,value) %>%
  pivot_wider(names_from=VEMF, values_from=value) %>%
  mutate(FossilShare=100*`Prm_Ene_Fos`/`Prm_Ene`) %>%
  filter(is.finite(FossilShare))

df_decline <- df_pe %>%
  filter(Year %in% c(2020,2050)) %>%
  select(SCENARIO,Region,Year,FossilShare) %>%
  pivot_wider(names_from=Year, values_from=FossilShare, names_prefix="y") %>%
  filter(!is.na(y2020), !is.na(y2050)) %>%
  mutate(decline=y2020-y2050) %>%
  group_by(SCENARIO) %>%
  mutate(decline_norm=(decline-min(decline,na.rm=TRUE))/
           (max(decline,na.rm=TRUE)-min(decline,na.rm=TRUE))) %>%
  ungroup()

world17 <- ne_countries(scale="medium", returnclass="sf") %>%
  select(iso3c=iso_a3, geometry) %>%
  filter(iso3c!="ATA") %>%
  left_join(Region_map, by="iso3c") %>%
  left_join(df_decline, by="Region")

world17_plot <- world17 %>%
  filter(!is.na(SCENARIO), !is.na(decline_norm))

p_map <- ggplot(world17_plot) +
  geom_sf(aes(fill=decline_norm), color="grey70", linewidth=0.1) +
  #facet_wrap(~SCENARIO) +
  scale_fill_gradient(low="#f7fbff", high="#08306b", limits=c(0,1),
                      na.value="grey90", name="Fossil phase out rate") +
  theme_void() +
  theme(legend.position="bottom",
        strip.background=element_blank(),
        plot.title=element_text(face="bold"))

print(p_map)


#R5 consumption loss pentagon-----------------------
CLP <- c("SSP2_400C_2030CP_NoCC_No",
          "SSP2_400C_2030CP_trs100_POP_NoCC_No",
        "SSP2_400C_2030CP_trs100_GDPP_NoCC_No")


Region <- c("XE25","JPN","IND","TUR","CHN","USA","XER","XOC","XSE","XSA","CAN","BRA","XLM","CIS","XME","XNF","XAF")
Region <- c("R5REF", "R5LAM", "R5ASIA", "R5MAF", "R5OECD90+EU")

thema <- "Pol_Cos_Cns_Los_rat_NPV_5pc"

df <- rgdx.param("global_17_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF == thema) %>%
  filter(YEMF == "2050") %>% 
  filter(SCENARIO %in% CLP) %>%
  filter(REMF %in% Region)

df$SCENARIO <- gsub("SSP2_400C_2030CP_NoCC_No", "NonAid", df$SCENARIO)
df$SCENARIO <- gsub("SSP2_400C_2030CP_trs100_POP_NoCC_No", "AidPOP", df$SCENARIO)
df$SCENARIO <- gsub("SSP2_400C_2030CP_trs100_GDPP_NoCC_No", "AidGDP", df$SCENARIO)

plot_df <- df %>%
  mutate(
    value = as.numeric(IAMC_Template), 
    REMF = factor(REMF, levels = Region),
    SCENARIO = factor(SCENARIO, levels = c("NonAid", "AidGDP", "AidPOP"))
  )

# 値が0に近いほど外側、値が大きいほど内側
max_val <- max(plot_df$value, na.rm = TRUE)

plot_df <- plot_df %>%
  mutate(radius = 1 - value / max_val)

# 正五角形の頂点座標
angle_df <- tibble(
  REMF = factor(Region, levels = Region),
  angle = seq(pi/2, pi/2 + 2*pi, length.out = length(Region) + 1)[1:length(Region)]
)

plot_df <- plot_df %>%
  left_join(angle_df, by = "REMF") %>%
  mutate(
    x = radius * cos(angle),
    y = radius * sin(angle)
  ) %>%
  arrange(SCENARIO, REMF)

# 各シナリオの線を閉じる
plot_df_closed <- plot_df %>%
  group_by(SCENARIO) %>%
  arrange(REMF, .by_group = TRUE) %>%
  bind_rows(slice(., 1)) %>%
  ungroup()

value_breaks <- pretty(c(0, max_val), n = 5)
value_breaks <- value_breaks[value_breaks >= 0 & value_breaks <= max_val]

grid_df <- expand.grid(
  value = value_breaks,
  REMF = factor(Region, levels = Region)
) %>%
  mutate(radius = 1 - value / max_val) %>%
  left_join(angle_df, by = "REMF") %>%
  mutate(
    x = radius * cos(angle),
    y = radius * sin(angle)
  ) %>%
  group_by(value) %>%
  arrange(REMF, .by_group = TRUE) %>%
  bind_rows(slice(., 1)) %>%
  ungroup()

# ラベルは上方向の軸に少しずらして表示
label_df <- tibble(
  value = value_breaks,
  radius = 1 - value / max_val,
  x = 0.04,
  y = radius,
  label = ifelse(
    abs(value) < 1,
    paste0(round(value * 100), "%"),
    paste0(round(value), "%")
  )
)
# 軸とラベル
axis_df <- angle_df %>%
  mutate(
    x = cos(angle),
    y = sin(angle),
    x_lab = 1.14 * cos(angle),
    y_lab = 1.14 * sin(angle)
  )

g<-ggplot() +
  geom_path(
    data = grid_df,
    aes(x = x, y = y, group = value),
    color = "grey70",
    linewidth = 0.8
  ) +
  geom_segment(
    data = axis_df,
    aes(x = 0, y = 0, xend = x, yend = y),
    color = "grey65",
    linewidth = 0.9
  ) +
  geom_text(
    data = label_df,
    aes(x = x, y = y, label = label),
    size = 5,
    color = "black",
    fontface = "plain",
    hjust = 0
  ) +
  geom_polygon(
    data = plot_df_closed,
    aes(x = x, y = y,
        group = SCENARIO,
        fill = SCENARIO,
        color = SCENARIO),
    alpha = 0.1,
    linewidth = 1.2
  ) +
  geom_path(
    data = plot_df_closed,
    aes(x = x, y = y,
        group = SCENARIO,
        color = SCENARIO),
    linewidth = 2
  ) +
  geom_point(
    data = plot_df,
    aes(x = x, y = y, color = SCENARIO),
    size = 4
  ) +
  geom_text(
    data = axis_df,
    aes(x = x_lab, y = y_lab, label = REMF),
    size = 6,
    fontface = "plain",
    color = "black"
  ) +
  coord_equal(
  xlim = c(-1.35, 1.35),
  ylim = c(-1.25, 1.25),
  clip = "off"
) +

scale_color_manual(values = c(
  "NonAid" = "#E64B35",
  "AidGDP" = "#4DBBD5",
  "AidPOP" = "#00A087"
)) +

scale_fill_manual(values = c(
  "NonAid" = "#E64B35",
  "AidGDP" = "#4DBBD5",
  "AidPOP" = "#00A087"
)) +
  labs(
    title = "Cumulative consumption Loss Rate by 2050",
    fill = "Scenario",
    color = "Scenario"
  ) +

  theme_minimal(base_size = 20) +

  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),

    plot.title = element_text(
      size = 26,
      face = "plain",
      hjust = 0.5
    ),

    plot.subtitle = element_text(
      size = 18,
      hjust = 0.5,
      margin = margin(b = 20)
    ),

    legend.position = "bottom",

    legend.title = element_text(
      size = 18,
      face = "plain"
    ),

    legend.text = element_text(
      size = 16
    ),

    plot.margin = margin(30, 60, 30, 60)
  )
plot(g)

name  <- paste0(thema, ".png")
output_dir <- file.path("..", "output")

ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 12,
  dpi = 600,
)


thema_line <- "Pol_Cos_Cns_Los_rat"

df_line <- rgdx.param("global_17_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF == thema_line) %>%
  filter(SCENARIO %in% CLP) %>%
  filter(REMF %in% Region) %>%
  mutate(
    value = as.numeric(IAMC_Template),
    YEMF = as.numeric(as.character(YEMF)),
    REMF = factor(REMF, levels = Region)
  )%>%
  filter(YEMF >= 2025)

df_line$SCENARIO <- gsub("SSP2_400C_2030CP_NoCC_No", "NonAid", df_line$SCENARIO)
df_line$SCENARIO <- gsub("SSP2_400C_2030CP_trs100_POP_NoCC_No", "AidPOP", df_line$SCENARIO)
df_line$SCENARIO <- gsub("SSP2_400C_2030CP_trs100_GDPP_NoCC_No", "AidGDP", df_line$SCENARIO)

df_line <- df_line %>%
  mutate(SCENARIO = factor(SCENARIO, levels = c("NonAid", "AidGDP", "AidPOP")))

make_line_plot <- function(region_name) {
  ggplot(
    df_line %>% filter(REMF == region_name),
    aes(x = YEMF, y = value, color = SCENARIO, group = SCENARIO)
  ) +
    geom_line(linewidth = 1.1) +
#    geom_point(size = 1.8) +
    scale_color_manual(values = c(
      "NonAid" = "#E64B35",
      "AidGDP" = "#4DBBD5",
      "AidPOP" = "#00A087"
    )) +
#    scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
      scale_x_continuous(
    breaks = c(2020,2030,2040,2050)
  ) +
    labs(title = region_name, x = NULL, y = NULL) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 11, face = "plain", hjust = 0.5),
      axis.text = element_text(size = 8, color = "black"),
      axis.title = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
      plot.margin = margin(2, 2, 2, 2)
    )
}

p_R5REF      <- make_line_plot("R5REF")
p_R5LAM      <- make_line_plot("R5LAM")
p_R5ASIA     <- make_line_plot("R5ASIA")
p_R5MAF      <- make_line_plot("R5MAF")
p_R5OECD90EU <- make_line_plot("R5OECD90+EU")

g2 <- g +
  labs(title = NULL, subtitle = NULL) +
  theme(
    legend.position = "none",
    plot.margin = margin(0, 0, 0, 0)
  )

p <- ggdraw() +

  # =========================
  # 中央レーダー図（やや大きく）
  # =========================
  draw_plot(
    g2,
    x = 0.25, y = 0.24,
    width = 0.50, height = 0.50
  ) +

  # =========================
  # 周囲の小グラフ（やや小さく）
  # =========================

  # 上
  draw_plot(
    p_R5REF,
    x = 0.36, y = 0.78,
    width = 0.28, height = 0.16
  ) +

  # 左上
  draw_plot(
    p_R5LAM,
    x = 0.08, y = 0.58,
    width = 0.22, height = 0.16
  ) +

  # 右上
  draw_plot(
    p_R5OECD90EU,
    x = 0.71, y = 0.58,
    width = 0.22, height = 0.16
  ) +

  # 左下
  draw_plot(
    p_R5ASIA,
    x = 0.08, y = 0.08,
    width = 0.22, height = 0.16
  ) +

  # 右下
  draw_plot(
    p_R5MAF,
    x = 0.71, y = 0.08,
    width = 0.22, height = 0.16
  ) +

  # =========================
  # タイトル
  # =========================
  draw_label(
    "Consumption loss rate",
    x = 0.50, y = 0.73,
    size = 20,
    fontface = "plain"
  ) +

  # =========================
  # 凡例
  # =========================
  draw_plot(
    get_legend(
      g +
        theme(
          legend.position = "bottom",

          legend.title = element_text(
            size = 15,
            face = "plain"
          ),

          legend.text = element_text(
            size = 13,
            face = "plain"
          )
        )
    ),
    x = 0.32, y = 0.20,
    width = 0.36, height = 0.06
  )

plot(p)

name  <- paste0(thema, "_all.png")
output_dir <- file.path("..", "output")

ggsave(
  filename = file.path(output_dir, name),
  plot = p,
  width = 13,
  height = 11,
  dpi = 600,
  bg = "white"
)

#Policy cost map------------------------------------------------------------
Region <- c("XE25","JPN","IND","TUR","CHN","USA","XER","XOC","XSE","XSA","CAN","BRA","XLM","CIS","XME","XNF","XAF")

CLP <- c("SSP2_400C_2030CP_POP_NoCC_No","SSP2_400C_2030CP_base_NoCC_No", "SSP2_400C_2030CP_GDP_NoCC_No")
CLP <- c("SSP2_400C_2030CP_base_NoCC_No", "SSP2_400C_2030CP_GDP_NoCC_No")

Indicator <- c("Pol_Cos_GDP_Los_rat_NPV_5pc")
Indicator <- c("Pol_Cos_Cns_Los_rat_NPV_5pc")
#Indicator <- c("Pol_Cos_Cns_Los_NPV_5pc")

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
      SCENARIO == "SSP2_400C_2030CP_base_NoCC_No" ~ "NonAid",
      SCENARIO == "SSP2_400C_2030CP_POP_NoCC_No"  ~ "POPAid",
      SCENARIO == "SSP2_400C_2030CP_GDP_NoCC_No"  ~ "GDPAid",
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

name  <- "Pol_Cns_map.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = p_map_cost,
  width = 12,
  height = 8,
  dpi = 600,
)

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

name  <- "Pol_Cns_map_diff.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = p_map_recovery,
  width = 12,
  height = 8,
  dpi = 600,
)
#Policy cost map POP ver------------------------------------------------
Region <- c("XE25","JPN","IND","TUR","CHN","USA","XER","XOC","XSE","XSA","CAN","BRA","XLM","CIS","XME","XNF","XAF")

CLP <- c("SSP2_400C_2030CP_POP_NoCC_No","SSP2_400C_2030CP_base_NoCC_No", "SSP2_400C_2030CP_GDP_NoCC_No")
CLP <- c("SSP2_400C_2030CP_base_NoCC_No", "SSP2_400C_2030CP_POP_NoCC_No")

Indicator <- c("Pol_Cos_GDP_Los_rat_NPV_5pc")
Indicator <- c("Pol_Cos_Cns_Los_rat_NPV_5pc")
#Indicator <- c("Pol_Cos_Cns_Los_NPV_5pc")

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
      SCENARIO == "SSP2_400C_2030CP_base_NoCC_No" ~ "NonAid",
      SCENARIO == "SSP2_400C_2030CP_POP_NoCC_No"  ~ "POPAid",
      SCENARIO == "SSP2_400C_2030CP_GDP_NoCC_No"  ~ "GDPAid",
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
  mutate(SCENARIO = factor(SCENARIO,levels = c("NonAid", "POPAid")) )

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

name  <- "Pol_Cns_map_POP.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = p_map_cost,
  width = 12,
  height = 8,
  dpi = 600,
)

df_recovery <- df_cost %>%
  select(SCENARIO, Region, value) %>%
  pivot_wider(names_from = SCENARIO, values_from = value) %>%
  mutate(
    recovery_GDPAid =  (NonAid - POPAid) 
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

name  <- "Pol_Cns_map_diff_POP.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = p_map_recovery,
  width = 12,
  height = 8,
  dpi = 600,
)

#Cumulative GDP 17 region---------------------
Region <- c(
  "USA","XE25","JPN","CAN","XER","XOC","TUR","CHN","IND","XSE","XSA","BRA","XLM","CIS","XME","XNF","XAF"
)

CLP <- c(
  "SSP2_400C_2030CP_base_NoCC_No",
  "SSP2_400C_2030CP_GDP_NoCC_No"
)

df_cost <- rgdx.param("global_17_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF == "Pol_Cos_GDP_Los_rat_NPV_5pc") %>%
  filter(SCENARIO %in% CLP) %>%
  filter(REMF %in% Region) %>%
  mutate(
    SCENARIO = case_when(
      SCENARIO == "SSP2_400C_2030CP_base_NoCC_No" ~ "NonAid",
      SCENARIO == "SSP2_400C_2030CP_GDP_NoCC_No"  ~ "Aid",
      TRUE ~ SCENARIO
    ),
    Region_plot = factor(REMF, levels = Region),
    Year = as.numeric(as.character(YEMF)),
    value = as.numeric(IAMC_Template)
  ) %>%
  filter(Year == 2050) %>%
  select(SCENARIO, Region_plot, value) %>%
  filter(is.finite(value))

df_plot <- df_cost %>%
  pivot_wider(
    names_from = SCENARIO,
    values_from = value
  ) %>%
  filter(!is.na(NonAid), !is.na(Aid))

p <- ggplot(df_plot, aes(x = Region_plot)) +
  geom_segment(
    aes(
      xend = Region_plot,
      y = NonAid,
      yend = Aid
    ),
    color = "grey50",
    linetype = "dashed",
    linewidth = 0.7
  ) +
  geom_point(
    aes(y = NonAid, color = "NonAid"),
    size = 3
  ) +
  geom_point(
    aes(y = Aid, color = "Aid"),
    size = 3
  ) +
  scale_color_manual(
    values = c(
      "NonAid" = "#D55E00",
      "Aid" = "#0072B2"
    )
  ) +
  labs(
    x = NULL,
    y = "Policy cost (% GDP loss, NPV 5%)",
    title = "Policy cost in 2050",
    color = NULL
  ) +
  Mytheme

print(p)