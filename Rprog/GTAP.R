


# GDP_dcp -----------------------------------------------------------------

year_selected = c(2050)

region_selected = c("World","R5OECD90+EU","Non-OECD")
region_selected = c("CHN","IND","XSE", "XSA", "BRA", "XLM", "XME", "XNF", "XAF", "CIS")
region_selected = c("USA","CAN","XE25", "XER", "JPN", "XOC", "TUR")


scenario_name <- c("SSP2_400C_2030CP_base_NoCC_No",
#                   "SSP2_400C_2030CP_GDP_NoCC_No",
                   "SSP2_400C_2030CP_POP_NoCC_No",
                   "SSP2_BaU_NoCC_No")


scenario_revise <- function(dataframe){
  if(str_detect(scenario_name[[i]], "iadjadd") == T){
    iadj_num <- regmatches(scenario_name[[i]], regexpr("iadjadd([0-9]+)", scenario_name[[i]]))
    iadj_num <- str_sub(iadj_num, start = 10, end = 11)
    scenario <- rep(paste0("ACF", iadj_num), times = nrow(dataframe))
  }else if(str_detect(scenario_name[[i]], "SSP2_400C_2030CP_base_NoCC_No") == T){
    scenario <- rep("NonAid", times = nrow(dataframe))
  }else if(str_detect(scenario_name[[i]], "SSP2_400C_2030CP_GDP_NoCC_No") == T){
    scenario <- rep("GDP", times = nrow(dataframe))
  }else if(str_detect(scenario_name[[i]], "SSP2_400C_2030CP_POP_NoCC_No") == T){
    scenario <- rep("POP", times = nrow(dataframe))
  }else{
    scenario <- rep(scenario_name[[i]], times = nrow(dataframe))
  }
  return(scenario)
}

dataframe_list <- vector("list", length = length(scenario_name))

for (i in 1:length(scenario_name)) {
  file_path <- paste0(scenario_name[[i]], ".gdx")
  analysis_GDP_s <- rgdx.param(file_path, "GDP_s")
  SCENARIO <- scenario_revise(analysis_GDP_s)
  analysis_GDP_s <- cbind(SCENARIO, analysis_GDP_s)
  dataframe_list[[i]] <- analysis_GDP_s
}
analysis_GDP_s <- do.call(rbind, dataframe_list)

scenario_vec <- unlist(unique(analysis_GDP_s %>% select(SCENARIO)))

analysis_GDP_s <- analysis_GDP_s%>% 
  rename(year=Y,region=R,category=INS_MCR,value=GDP_s) %>% 
  filter(region %in% region_selected) %>% 
  filter(year %in% year_selected) %>% 
  filter(SCENARIO %in% scenario_vec) %>%
  pivot_wider(names_from = category,values_from = value,values_fill = 0) %>% 
  pivot_longer(cols = !c(SCENARIO,year,region),names_to="category")

analysis_GDP_s<-analysis_GDP_s %>% 
  aggregate(value~year+region+SCENARIO,FUN = "sum") %>% 
  mutate(category="Total") %>% 
  bind_rows(analysis_GDP_s)

analysis_GDP_s_diff<-group_by(analysis_GDP_s,year,region,category) %>% 
  mutate(bau_percent=(value-value[SCENARIO=="SSP2_BaU_NoCC_No"])) %>% 
  ungroup() %>% 
  group_by(year,region) %>% 
  mutate(bau_percent=bau_percent/value[SCENARIO=="SSP2_BaU_NoCC_No" & category=="Total"]) %>% 
  filter(SCENARIO!="SSP2_BaU_NoCC_No") %>% 
  select(-value) %>% 
  mutate(category=recode_factor(category,"HURB"="Consumption","GOV"="Government","ROW"="Export","IMP"="Import","S-I"="Investment")) %>% 
  mutate(region=factor(region,levels=region_selected))


analysis_GDP_s_diff_total<-analysis_GDP_s %>% 
  aggregate(value~year+region+SCENARIO,FUN = "sum") %>% 
  mutate(bau_percent=(1-value/value[SCENARIO=="SSP2_BaU_NoCC_No"])*100) %>% 
  filter(SCENARIO!="SSP2_BaU_NoCC_No") %>% 
  mutate(region=factor(region,levels=region_selected))

analysis_GDP_s_diff$region <- gsub("R5OECD90+EU", "OECD", fixed = TRUE, analysis_GDP_s_diff$region)
#plot_list$REMF <- gsub("XAF", "Rest of Africa", plot_list$REMF)

scenario_order <- c("NonAid", "GDP", "POP")

analysis_GDP_s_diff$SCENARIO <- factor(analysis_GDP_s_diff$SCENARIO, levels = scenario_order)

g <- ggplot() +
  geom_bar(filter(analysis_GDP_s_diff, category != "Total"), 
           mapping = aes(x = SCENARIO, y = bau_percent * 100, fill = category), stat = "identity") +
  geom_point(filter(analysis_GDP_s_diff, category == "Total"), 
             mapping = aes(x = SCENARIO, y = bau_percent * 100)) +
  geom_abline(slope = 0, intercept = 0, linetype = 2) +
  facet_wrap(~region, ncol = 3, scales = "free_y") +
  ylab("GDP differences in 2050 (%)") +
  scale_fill_manual(values = c("Consumption" = "grey", "Government" = "skyblue", "Export" = "orange", 
                               "Import" = "purple", "Investment" = "yellow")) +
  theme_1 +
  theme(legend.position = "bottom")

plot(g)


name  <- "GDP_dcp_2.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 8,
  dpi = 600,
)


##NonOECD-----------------------------------------------------------------------------------------
df_base <- rgdx.param("global_17_SSP2_BaU_NoCC_No.gdx", "PSAM_value")
df_gdp <- rgdx.param("global_17_SSP2_400C_2030CP_base_NoCC_No.gdx", "PSAM_value")
df_pop <- rgdx.param("global_17_SSP2_400C_2030CP_POP_NoCC_No.gdx", "PSAM_value")

com_categories <- list(
  Trans = c("COM_TRS", "COM_CSS"),
  Min = c("COM_COA", "COM_OIL", "COM_OMN", "COM_GAS"),
  Manu = c("COM_FPR", "COM_OMT", "COM_LIN", "COM_PPP", "COM_CRP", "COM_NMM", "COM_I_S", "COM_NFM", "COM_OMF"),
  Ene = c("COM_P_P", "COM_COP", "COM_ELY"),
  Agr = c("COM_pdr", "COM_wht", "COM_gro", "COM_osd", "COM_oth_a", "COM_ctl", "COM_rmk", "COM_oth_l", "COM_FRS")
)

non_oecd_regions <- c("IND", "XSE", "XSA", "BRA", "XLM", "CIS", "XME", "XNF", "XAF")

calc_trade <- function(df, region_code, flow) {
  partner_col <- if (flow == "exp") "i4" else "i3"
  com_col <- if (flow == "exp") "i3" else "i4"

  bind_rows(lapply(names(com_categories), function(sector) {
    df %>%
      filter(grepl("ROW", .data[[partner_col]]), i2 == region_code, .data[[com_col]] %in% com_categories[[sector]]) %>%
      group_by(i1) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(Sector = sector)
  }))
}

calc_ratio <- function(df, base, region_code, flow, scenario_name) {
  calc_trade(df, region_code, flow) %>%
    left_join(
      calc_trade(base, region_code, flow) %>% select(i1, Sector, base = value),
      by = c("i1", "Sector")
    ) %>%
    mutate(value = value / base, dataset = scenario_name) %>%
    select(i1, Sector, dataset, value)
}

calc_region <- function(region_code) {
  exp_ratio <- bind_rows(
    calc_ratio(df_gdp, df_base, region_code, "exp", "GDP"),
    calc_ratio(df_pop, df_base, region_code, "exp", "POP")
  ) %>%
    rename(exp_ratio = value)

  imp_ratio <- bind_rows(
    calc_ratio(df_gdp, df_base, region_code, "imp", "GDP"),
    calc_ratio(df_pop, df_base, region_code, "imp", "POP")
  ) %>%
    rename(imp_ratio = value)

  exp_ratio %>%
    left_join(imp_ratio, by = c("i1", "Sector", "dataset")) %>%
    mutate(
      i1 = as.numeric(as.character(i1)),
      value = 100 * (exp_ratio - imp_ratio),
      Region = region_code
    ) %>%
    filter(i1 >= 2030, i1 <= 2050, is.finite(value)) %>%
    select(i1, Sector, dataset, Region, value)
}

df <- bind_rows(lapply(non_oecd_regions, calc_region))

g <- ggplot(df, aes(x = i1, y = value, fill = Sector)) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey40") +
  geom_area(position = "stack", alpha = 0.85, linewidth = 0.15, color = "white") +
  facet_grid(dataset ~ Region, scales = "free_y") +
  scale_x_continuous(breaks = c(2030, 2040, 2050), limits = c(2029, 2051)) +
  scale_fill_brewer(palette = "Set2", name = "Sector") +
  labs(x = NULL, y = "Export ratio − Import ratio (percentage points)") +
  theme_1 +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

print(g)

#Fossil share------------------------


df_pe <- rgdx.param("global_17_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF %in% c("Prm_Ene",
                     "Prm_Ene_Fos")) %>%
  filter(SCENARIO %in% CLP) %>%
  filter(REMF %in% Region) %>%
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
    SCENARIO=="SSP2_400C_2030CP_POP_NoCC_No" ~ "Aid",
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


#R5 consumption loss-----------------------
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