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
library(readxl)


theme_1 <- theme_bw() +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, size = 16, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        legend.position = "right")

df <- read_excel("LMDI.xlsx", sheet = "Provider")

#df <- df %>% arrange(Year)

log_mean <- function(a,b){
  if(a==b) return(a)
  (a-b)/(log(a)-log(b))
}

run_glmdi <- function(base, target){
  
  gross0 <- base$Gro_Emi
  gross1 <- target$Gro_Emi
  
  rem0 <- base$Gro_Rem
  rem1 <- target$Gro_Rem
  
  factors <- c("GDP","EI","Ele","CI")
  
  f0 <- as.numeric(base[factors])
  f1 <- as.numeric(target[factors])
  
  names(f0) <- factors
  names(f1) <- factors
  
  Lg <- log_mean(gross1, gross0)
  effects <- Lg * log(f1/f0)
  
  Lr <- log_mean(abs(rem1), abs(rem0))
  rem_effect <- - Lr * log(abs(rem1)/abs(rem0))
  
  contrib <- c(effects, Removal = rem_effect)
  
  net0 <- gross0 + rem0
  net1 <- gross1 + rem1
  
  df_out <- data.frame(
    Factor = c("Start", names(contrib), "End"),
    Value = c(net0, contrib, net1)
  )
  
  df_out
}

wf <- run_glmdi(df[1,], df[2,]) %>%
  mutate(Period="2020-2050")


wf <- wf %>%
  group_by(Period) %>%
  mutate(
    cumulative = cumsum(lag(Value, default = 0)),
    ymin = pmin(cumulative, cumulative + Value),
    ymax = pmax(cumulative, cumulative + Value)
  )
wf$Factor <- factor(
  wf$Factor,
  levels=c("Start","GDP","EI","Ele","CI","Removal","End")
)
wf <- wf %>%
  mutate(
    change_label = case_when(
      Factor %in% c("Start","End") ~ format(round(Value,0), big.mark=","),
      TRUE ~ sprintf("%+.0f", Value)  # ← + / - 自動
    )
  )

cols <- c(
  "GDP"="#d73027",
  "EI"="#4575b4",
  "Ele"="#fdae61",
  "CI"="#1a9850",
  "Removal"="#542788",
  "Start"="grey40",
  "End"="black"
)

g<-ggplot(wf, aes(x=Factor, fill=Factor)) +
  
  geom_rect(aes(
    xmin=as.numeric(factor(Factor))-0.4,
    xmax=as.numeric(factor(Factor))+0.4,
    ymin=ymin,
    ymax=ymax
  )) +
  geom_hline(
    yintercept = 0,
    linewidth = 1.3,
    color = "black",
    alpha = 0.7
  )+
  geom_text(
    aes(
      y = ifelse(Value > 0, ymax, ymin),
      label = change_label
    ),
    vjust = ifelse(wf$Value > 0, -0.4, 1.2),
    size = 4.5,
    fontface = "bold"
  )+
  #acet_wrap(~Period, scales="free_x") +
  
  scale_fill_manual(
    values = cols,
    breaks = c("GDP","EI","Ele","CI","Removal"),
    labels = c(
      "GDP" = "Economic activity",
      "EI" = "Energy intensity",
      "Ele" = "Electrification",
      "CI" = "Carbon intensity",
      "Removal" = "Carbon removal"
    ),
    name = "Drivers"
  )+
  
  theme_minimal(base_size=15) +
  labs(
    y="Net CO2 change (Mt)",
    x="",
    title=""
  ) +
  
  theme(
    legend.position = "right"
  )

plot(g)


name="LMDI2.png"
ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)