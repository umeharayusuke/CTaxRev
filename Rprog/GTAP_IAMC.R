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

#setwd("CTaxRev/data")

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