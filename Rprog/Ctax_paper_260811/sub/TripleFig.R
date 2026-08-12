# Decisive figures for international carbon-tax revenue transfers
# Required input: global_17_IAMC.gdx in the working directory

library(tidyverse)
library(ggplot2)
library(gdxrrw)
library(patchwork)
library(scales)
library(readxl)

gdx_file <- "global_17_IAMC.gdx"
ar6_meta_file <- "AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx"
ar6_world_file <- "AR6_Scenarios_Database_World_v1.1.csv"
output_dir <- "decisive_figures_output"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

scenario_def <- "SSP2_400C_2030CP_NoCC_No"
scenario_aid <- "SSP2_400C_2030CP_15th_NoCC_No"
start_year <- 2030
end_year <- 2050
providers <- c("XE25","JPN","TUR","CHN","USA","XER","XOC","CAN","XLM","CIS")
recipients <- c("XSA","IND","XNF","XAF","XSE","BRA","XME")
all_regions <- c(providers, recipients)

provider_color <- "#356C9B"
recipient_color <- "#D27755"
def_color <- "#9B9B9B"
aid_color <- "#188977"
def_color <- "#F8766D"
aid_color <- "#00BFC4"
ink <- "#252525"

theme_paper <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(color = "#E5E5E5", linewidth = 0.3),
        plot.title = element_text(face = "bold", size = 14), plot.subtitle = element_text(color = "#555555"),
        axis.title = element_text(face = "bold"), legend.position = "bottom", legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"), plot.caption = element_text(color = "#666666", hjust = 0))

trapz_sum <- function(year, value) {
  ok <- is.finite(year) & is.finite(value)
  year <- year[ok]; value <- value[ok]
  if (length(year) < 2) return(NA_real_)
  o <- order(year); year <- year[o]; value <- value[o]
  sum(diff(year) * (head(value, -1) + tail(value, -1)) / 2)
}

required_files <- c(gdx_file, ar6_meta_file, ar6_world_file)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) stop("Place the following required file(s) in the working directory: ", paste(missing_files, collapse = ", "))

iamc <- rgdx.param(gdx_file, "IAMC_template") %>%
  mutate(Year = as.numeric(as.character(YEMF)))

# Shared transfer calculations: annual revenue is pooled and allocated by recipient GDP_PPP shares.
provider_annual <- iamc %>%
  filter(VEMF == "Rev_gov_Tax_Car_Tax", SCENARIO == scenario_aid, REMF %in% providers,
         Year >= start_year, Year <= end_year) %>%
  transmute(Year, REMF, Transfer = as.numeric(IAMC_Template))

total_annual <- provider_annual %>%
  group_by(Year) %>% summarise(TotalTransfer = sum(Transfer, na.rm = TRUE), .groups = "drop")

recipient_annual <- iamc %>%
  filter(VEMF == "GDP_PPP", SCENARIO == scenario_aid, REMF %in% recipients,
         Year >= start_year, Year <= end_year) %>%
  group_by(Year) %>% mutate(GDPShare = IAMC_Template / sum(IAMC_Template, na.rm = TRUE)) %>% ungroup() %>%
  left_join(total_annual, by = "Year") %>%
  transmute(Year, REMF, Transfer = TotalTransfer * GDPShare)

provider_cum <- provider_annual %>% group_by(REMF) %>%
  summarise(CumTransfer = trapz_sum(Year, Transfer), .groups = "drop") %>%
  mutate(Type = "Provider")

recipient_cum <- recipient_annual %>% group_by(REMF) %>%
  summarise(CumTransfer = trapz_sum(Year, Transfer), .groups = "drop") %>%
  mutate(Type = "Recipient")

gdp_cum <- iamc %>%
  filter(VEMF == "GDP_MER", SCENARIO == scenario_aid, REMF %in% all_regions,
         Year >= start_year, Year <= end_year) %>%
  group_by(REMF) %>% summarise(CumGDP = trapz_sum(Year, as.numeric(IAMC_Template)), .groups = "drop")

transfer_summary <- bind_rows(provider_cum, recipient_cum) %>%
  left_join(gdp_cum, by = "REMF") %>%
  mutate(CumTrillion = CumTransfer / 1e6,
         # CumTransfer is in million US$2010; CumGDP is in billion US$2010.
         # Multiplying CumGDP by 1,000 aligns both quantities to million US$2010.
         NetTransferPct = if_else(Type == "Provider", -100 * CumTransfer / (CumGDP * 1000),
                                 100 * CumTransfer / (CumGDP * 1000)),
         Figure1Pct = if_else(Type == "Provider", abs(NetTransferPct), -abs(NetTransferPct))) %>%
  filter(is.finite(NetTransferPct))

# Consumption-loss effect used by Options 1 and 4.
loss <- iamc %>%
  filter(VEMF == "Pol_Cos_Cns_Los_rat_NPV_5pc", SCENARIO %in% c(scenario_def, scenario_aid),
         REMF %in% all_regions, Year == end_year) %>%
  mutate(Scenario = case_when(SCENARIO == scenario_def ~ "Def", SCENARIO == scenario_aid ~ "Aid", TRUE ~ SCENARIO)) %>%
  group_by(REMF, Scenario) %>% summarise(Value = mean(as.numeric(IAMC_Template), na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Scenario, values_from = Value) %>%
  filter(is.finite(Def), is.finite(Aid)) %>%
  mutate(Change = Aid - Def, Type = if_else(REMF %in% providers, "Provider", "Recipient"))

# Global CO2 pathways used as the small consistency panel in Option 1.
emissions <- iamc %>%
  filter(VEMF == "Emi_CO2", SCENARIO %in% c(scenario_def, scenario_aid), REMF == "World",
         Year >= 2020, Year <= end_year) %>%
  transmute(Year, Scenario = case_when(SCENARIO == scenario_def ~ "Def", SCENARIO == scenario_aid ~ "Aid", TRUE ~ SCENARIO),
            Emissions = as.numeric(IAMC_Template) / 1000)

# AR6 C1/C2 global CO2-emissions ranges for the climate-pathway panel.
ar6_category <- read_excel(ar6_meta_file, sheet = "meta_Ch3vetted_withclimate") %>%
  transmute(Case = paste0(Model, "_", Scenario), Category)

df_load_ar6 <- read_csv(ar6_world_file, show_col_types = FALSE) %>%
  mutate(Case = paste0(Model, "_", Scenario)) %>%
  left_join(ar6_category, by = "Case") %>%
  filter(Category %in% c("C1", "C2"), Variable == "Emissions|CO2") %>%
  pivot_longer(cols = matches("^[0-9]{4}$"), names_to = "Year", values_to = "value", names_transform = as.numeric) %>%
  filter(!is.na(value), Year >= 2020, Year <= end_year)

df_ar6_range <- df_load_ar6 %>%
  mutate(value = value / 1000) %>%
  group_by(Category, Year) %>%
  summarise(p05 = quantile(value, 0.05, na.rm = TRUE), p25 = quantile(value, 0.25, na.rm = TRUE),
            p50 = median(value, na.rm = TRUE), p75 = quantile(value, 0.75, na.rm = TRUE),
            p95 = quantile(value, 0.95, na.rm = TRUE), .groups = "drop")

df_ar6_box <- df_ar6_range %>%
  filter(Year %in% seq(2020, end_year, 5)) %>%
  mutate(PlotYear = Year + if_else(Category == "C1", -0.7, 0.7))

# -----------------------------------------------------------------------------
# OPTION 1: who pays/receives + economic effect + emissions consistency
# -----------------------------------------------------------------------------
provider_order <- transfer_summary %>% filter(Type == "Provider") %>% arrange(desc(Figure1Pct)) %>% pull(REMF)
recipient_order <- transfer_summary %>% filter(Type == "Recipient") %>% arrange(abs(Figure1Pct)) %>% pull(REMF)
transfer_option1 <- transfer_summary %>%
  mutate(REMF_plot = factor(REMF, levels = c(provider_order, recipient_order)),
         Type = factor(Type, levels = c("Provider", "Recipient")))

p1a <- ggplot(transfer_option1, aes(REMF_plot, Figure1Pct, fill = Type)) +
  geom_col(width = 0.78, color = "white", linewidth = 0.25) +
  geom_hline(yintercept = 0, color = ink, linewidth = 0.45) +
  geom_vline(xintercept = length(provider_order) + 0.5, linetype = "dashed", color = "#8A8A8A") +
  scale_fill_manual(values = c(Provider = provider_color, Recipient = recipient_color)) +
  scale_y_continuous(labels = function(x) label_number(accuracy = 0.1, suffix = "%")(abs(x)),
                     expand = expansion(mult = c(0.08, 0.08))) +
  labs(title = "a", x = NULL,
       y = "Cumulative transfer / cumulative GDP\n(2030–2050, %)") +
  theme_paper + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())

loss_order <- rev(c(provider_order, recipient_order))
p1b <- loss %>% mutate(REMF_plot = factor(REMF, levels = loss_order)) %>%
  ggplot(aes(y = REMF_plot)) +
  geom_segment(aes(x = Def, xend = Aid, yend = REMF_plot), color = "#B5B5B5", linewidth = 0.8) +
  geom_point(aes(x = Def, color = "Def"), size = 2.8) +
  geom_point(aes(x = Aid, color = "Aid"), size = 2.8) +
  scale_color_manual(name = NULL, values = c(Def = def_color, Aid = aid_color),
                     guide = guide_legend(order = 2)) +
  labs(title = "b", x = "Cumulative consumption loss in 2050 (%)", y = NULL) +
  theme_paper

p1c <- ggplot() +
  geom_hline(yintercept = 0, color = "#777777", linewidth = 0.3) +
  geom_boxplot(data = df_ar6_box,
               aes(x = PlotYear, ymin = p05, lower = p25, middle = p50, upper = p75, ymax = p95,
                   fill = Category, group = interaction(Year, Category)),
               stat = "identity", width = 1.15, alpha = 0.55, color = "grey30", linewidth = 0.4) +
  geom_line(data = emissions, aes(Year, Emissions, color = Scenario, group = Scenario), linewidth = 1) +
  geom_point(data = emissions, aes(Year, Emissions, color = Scenario), size = 1.8) +
  scale_fill_manual(name = "AR6 category", values = c(C1 = "#4E79A7", C2 = "#F28E2B"),
                    guide = guide_legend(order = 1)) +
  scale_color_manual(name = "AIM scenario", values = c(Def = def_color, Aid = aid_color),
                     guide = "none") +
  scale_x_continuous(limits = c(2018.5, end_year + 1.5), breaks = seq(2020, end_year, 10)) +
  labs(title = "c", x = NULL, y = expression(Global~CO[2]~emissions~(Gt~yr^{-1}))) +
  theme_paper

figure1 <- (p1a | p1b | p1c) + plot_layout(widths = c(1.2, 1.1, 1.15), guides = "collect") &
  theme(legend.position = "bottom")
ggsave(file.path(output_dir, "option1_transfer_and_economic_effect.png"), figure1, width = 18, height = 7.5, dpi = 600)
ggsave(file.path(output_dir, "option1_transfer_and_economic_effect.pdf"), figure1, width = 18, height = 7.5)
