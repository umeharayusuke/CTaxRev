library(readxl)
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

theme_plot <- function(base_size = 18, base_family = "Times New Roman", bg = "white") 
{
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(text = element_text(family = base_family,colour = "#111111"),
      plot.title = element_text(face = "bold",size = base_size * 1.5,colour = "#111111", hjust = 0.5),
      plot.subtitle = element_text(size = base_size * 0.95, colour = "#333333", hjust = 0.5,margin = margin(b = 12)),
      axis.title.y = element_text(face = "bold",size = base_size * 1.15,colour = "#111111",margin = margin(r = 10)),
      axis.title.x = element_text(face = "bold",size = base_size * 1.15,colour = "#111111",margin = margin(t = 10)),
      axis.text = element_text(size = base_size,colour = "#111111"),
      axis.line = element_line( colour = "#111111",linewidth = 0.7),
      axis.ticks = element_line(colour = "#111111",linewidth = 0.6),
      axis.ticks.length = unit(0.18, "cm"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(size = base_size * 0.9, colour = "#111111"),
      legend.key.width = unit(1.3, "cm"),
      legend.key.height = unit(0.55, "cm"),
      legend.spacing.x = unit(0.35, "cm"),
      panel.background = element_rect(fill = bg,colour = NA),
      plot.background = element_rect(fill = bg,colour = NA),
      plot.margin = margin(18, 28, 18, 18))
}

output_dir <- file.path("..", "output")
