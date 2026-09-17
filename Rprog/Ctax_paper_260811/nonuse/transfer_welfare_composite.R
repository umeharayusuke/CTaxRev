# Transfer, welfare, and inequality composite figure ------------------------
library(tidyverse)
library(gdxrrw)
library(patchwork)
library(scales)

# Configuration -------------------------------------------------------------
gdx_candidates <- c("../../data/AR6database/global_17_IAMC.gdx",
                    "data/AR6database/global_17_IAMC.gdx", "global_17_IAMC.gdx")
gdx_file <- gdx_candidates[file.exists(gdx_candidates)][1]
if (is.na(gdx_file)) stop("Could not find data/AR6database/global_17_IAMC.gdx.")

output_dir <- if (dir.exists("../../output")) "../../output/Figure" else "output/Figure"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

scenario_codes <- c(Def = "SSP2_400C_2030CP_NoCC_No",
                    Aid = "SSP2_400C_2030CP_15th_NoCC_No")
start_year <- 2030
end_year <- 2050
base_year <- 2030
rho <- 0.008
main_eta <- 1.5

providers <- c("XE25", "JPN", "TUR", "CHN", "USA", "XER", "XOC", "CAN", "XLM", "CIS")
recipients <- c("XSA", "IND", "XNF", "XAF", "XSE", "BRA", "XME")
all_regions <- c(providers, recipients)

group_colors <- c(Provider = "#356C9B", Recipient = "#D27755", World = "#333333")
scenario_labels <- c(Def = "Def", Aid = "Aid")

# Utility functions ---------------------------------------------------------
trapz_sum <- function(year, value) {
  ok <- is.finite(year) & is.finite(value)
  year <- year[ok]
  value <- value[ok]
  if (length(year) < 2) return(NA_real_)
  o <- order(year)
  year <- year[o]
  value <- value[o]
  sum(diff(year) * (head(value, -1) + tail(value, -1)) / 2)
}

crra_utility <- function(cons_pc, eta) {
  if (abs(eta - 1) < 1e-12) return(log(cons_pc))
  cons_pc^(1 - eta) / (1 - eta)
}

ce_from_welfare <- function(W, W0, eta, baseline_weight) {
  if (abs(eta - 1) < 1e-12) return(exp((W - W0) / baseline_weight) - 1)
  ratio <- W / W0
  if (!is.finite(ratio) || ratio <= 0) return(NA_real_)
  ratio^(1 / (1 - eta)) - 1
}

ede_consumption <- function(cons_pc, population, eta) {
  ok <- is.finite(cons_pc) & cons_pc > 0 & is.finite(population) & population > 0
  cons_pc <- cons_pc[ok]
  population <- population[ok]
  share <- population / sum(population)
  if (abs(eta - 1) < 1e-12) exp(sum(share * log(cons_pc))) else
    sum(share * cons_pc^(1 - eta))^(1 / (1 - eta))
}

find_iamc_column <- function(data, candidates, role) {
  hit <- names(data)[tolower(names(data)) %in% tolower(candidates)]
  if (length(hit) == 0) {
    stop("Could not identify the ", role, " column. Available columns: ",
         paste(names(data), collapse = ", "))
  }
  hit[[1]]
}

standardise_iamc_columns <- function(data) {
  scenario_col <- find_iamc_column(data, c("SCENARIO", "Sc", "S"), "scenario")
  region_col <- find_iamc_column(data, c("REMF", "Sr", "R"), "region")
  variable_col <- find_iamc_column(data, c("VEMF", "Sv", "V"), "variable")
  year_col <- find_iamc_column(data, c("YEMF", "Y"), "year")
  value_col <- find_iamc_column(data, c("IAMC_Template", "IAMC_template", "value", "Val"), "value")

  data %>%
    transmute(SCENARIO = as.character(.data[[scenario_col]]),
              REMF = as.character(.data[[region_col]]),
              VEMF = as.character(.data[[variable_col]]),
              Year = as.numeric(as.character(.data[[year_col]])),
              IAMC_value = as.numeric(.data[[value_col]]))
}

# Read the GDX once; all three panels are calculated below from this table.
iamc <- rgdx.param(gdx_file, "IAMC_template") %>% standardise_iamc_columns()

# Panel a: transfer burden and recovery in cumulative consumption loss ------
provider_annual <- iamc %>%
  filter(VEMF == "Rev_gov_Tax_Car_Tax", SCENARIO == scenario_codes[["Aid"]],
         REMF %in% providers, between(Year, start_year, end_year)) %>%
  group_by(Year, REMF) %>%
  summarise(Transfer = sum(IAMC_value, na.rm = TRUE), .groups = "drop")

total_annual <- provider_annual %>%
  group_by(Year) %>%
  summarise(TotalTransfer = sum(Transfer, na.rm = TRUE), .groups = "drop")

recipient_annual <- iamc %>%
  filter(VEMF == "GDP_PPP", SCENARIO == scenario_codes[["Aid"]],
         REMF %in% recipients, between(Year, start_year, end_year)) %>%
  group_by(Year, REMF) %>%
  summarise(GDP_PPP = sum(IAMC_value, na.rm = TRUE), .groups = "drop") %>%
  group_by(Year) %>%
  mutate(GDPShare = GDP_PPP / sum(GDP_PPP, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(total_annual, by = "Year") %>%
  transmute(Year, REMF, Transfer = TotalTransfer * GDPShare)

transfer_cumulative <- bind_rows(
  provider_annual %>% group_by(REMF) %>%
    summarise(CumTransfer = trapz_sum(Year, Transfer), .groups = "drop") %>%
    mutate(Group = "Provider"),
  recipient_annual %>% group_by(REMF) %>%
    summarise(CumTransfer = trapz_sum(Year, Transfer), .groups = "drop") %>%
    mutate(Group = "Recipient")
)

gdp_cumulative <- iamc %>%
  filter(VEMF == "GDP_MER", SCENARIO == scenario_codes[["Aid"]],
         REMF %in% all_regions, between(Year, start_year, end_year)) %>%
  group_by(REMF) %>%
  summarise(CumGDP = trapz_sum(Year, IAMC_value), .groups = "drop")

transfer_summary <- transfer_cumulative %>%
  left_join(gdp_cumulative, by = "REMF") %>%
  mutate(CumTrillion = CumTransfer / 1e6,
         NetTransferPct = if_else(Group == "Provider",
                                  -100 * CumTransfer / (CumGDP * 1000),
                                   100 * CumTransfer / (CumGDP * 1000)))

loss_recovery <- iamc %>%
  filter(VEMF == "Pol_Cos_Cns_Los_rat_NPV_5pc",
         SCENARIO %in% unname(scenario_codes), REMF %in% all_regions,
         Year == end_year) %>%
  mutate(Scenario = recode(SCENARIO, !!!setNames(names(scenario_codes), scenario_codes))) %>%
  group_by(REMF, Scenario) %>%
  summarise(LossRate = mean(IAMC_value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Scenario, values_from = LossRate) %>%
  mutate(Recovery = Def - Aid)

scatter_data <- transfer_summary %>%
  left_join(loss_recovery, by = "REMF") %>%
  mutate(Group = factor(Group, levels = c("Provider", "Recipient"))) %>%
  filter(is.finite(NetTransferPct), is.finite(Recovery), is.finite(CumTrillion))

theme_panel <- theme_bw(base_size = 12.5) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
        plot.title = element_text(size = 13, face = "plain"),
        plot.subtitle = element_text(size = 10.5, color = "grey35"),
        axis.title = element_text(face = "plain"), legend.position = "bottom",
        legend.title = element_text(face = "plain"), plot.margin = margin(8, 8, 8, 8))

p_a <- ggplot(scatter_data, aes(NetTransferPct, Recovery, color = Group, size = CumTrillion)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf,
           fill = "#188977", alpha = 0.04) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.45) +
  geom_vline(xintercept = 0, color = "grey50", linewidth = 0.45) +
  geom_point(alpha = 0.88) +
  scale_color_manual(name = NULL, values = group_colors[c("Provider", "Recipient")]) +
  scale_size_continuous(name = "Cumulative transfer\n(trillion US$2010)",
                        range = c(3, 11), labels = label_number(accuracy = 0.1)) +
  scale_x_continuous(labels = label_number(accuracy = 0.1, suffix = "%")) +
  scale_y_continuous(labels = label_number(accuracy = 0.1, suffix = "%")) +
  labs(title = "a  Transfer burden and consumption-loss recovery",
       x = "Net cumulative transfer / cumulative GDP (%)\n← provides revenue                         receives revenue →",
       y = "Recovery in cumulative consumption loss: Def − Aid (%)") +
  theme_panel

if (requireNamespace("ggrepel", quietly = TRUE)) {
  p_a <- p_a +
    ggrepel::geom_text_repel(aes(label = REMF), size = 3.1, show.legend = FALSE,
                             box.padding = 0.35, point.padding = 0.25, max.overlaps = Inf)
} else {
  p_a <- p_a +
    geom_text(aes(label = REMF), size = 3, nudge_y = 0.03,
              check_overlap = TRUE, show.legend = FALSE)
}

# Panels c and d: welfare-equivalent effect and between-region inequality ----
consumption_data <- iamc %>%
  filter(SCENARIO %in% unname(scenario_codes),
         VEMF %in% c("CNS", "Pop"), REMF %in% all_regions,
         between(Year, start_year, end_year)) %>%
  mutate(Scenario = recode(SCENARIO, !!!setNames(names(scenario_codes), scenario_codes))) %>%
  group_by(Scenario, REMF, Year, VEMF) %>%
  summarise(value = sum(IAMC_value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = VEMF, values_from = value) %>%
  transmute(Scenario = factor(Scenario, levels = c("Def", "Aid")), Region = REMF, Year,
            Group = if_else(Region %in% providers, "Provider", "Recipient"),
            consumption = CNS, population = Pop, cons_pc = CNS / Pop,
            discount = 1 / (1 + rho)^(Year - base_year))

model_years <- sort(unique(consumption_data$Year))
if (length(model_years) < 2) stop("At least two model years are required for welfare calculation.")
year_intervals <- diff(model_years)
year_weights <- tibble(Year = model_years, dt = c(year_intervals, median(year_intervals)))
consumption_data <- consumption_data %>% left_join(year_weights, by = "Year")

if (any(!is.finite(consumption_data$cons_pc) | consumption_data$cons_pc <= 0)) {
  stop("CNS/Pop contains missing, non-finite, or non-positive values.")
}

welfare_detail <- consumption_data %>%
  mutate(utility = crra_utility(cons_pc, main_eta),
         welfare = population * discount * dt * utility,
         welfare_weight = population * discount * dt)

group_ce_data <- bind_rows(
  welfare_detail %>% group_by(Scenario, Group) %>%
    summarise(W = sum(welfare), Weight = sum(welfare_weight), .groups = "drop"),
  welfare_detail %>% group_by(Scenario) %>%
    summarise(W = sum(welfare), Weight = sum(welfare_weight), .groups = "drop") %>%
    mutate(Group = "World")
)

group_ce_baseline <- group_ce_data %>%
  filter(Scenario == "Def") %>%
  transmute(Group, W0 = W, BaselineWeight = Weight)

group_ce_aid <- group_ce_data %>%
  filter(Scenario == "Aid") %>%
  left_join(group_ce_baseline, by = "Group") %>%
  rowwise() %>%
  mutate(CE = ce_from_welfare(W, W0, main_eta, BaselineWeight), CE_percent = 100 * CE) %>%
  ungroup() %>%
  mutate(Group = factor(Group, levels = c("Provider", "Recipient", "World")))

p_c <- ggplot(group_ce_aid, aes(Group, CE_percent, fill = Group)) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.4) +
  geom_col(width = 0.62) +
  geom_text(aes(label = sprintf("%+.1f%%", CE_percent)),
            vjust = ifelse(group_ce_aid$CE_percent >= 0, -0.45, 1.3), size = 3.8) +
  scale_fill_manual(values = group_colors) +
  scale_y_continuous(labels = label_number(accuracy = 0.1, suffix = "%"),
                     expand = expansion(mult = c(0.14, 0.17))) +
  labs(title = "b  Welfare-equivalent effect",
       #subtitle = paste0("2030–2050, inequality aversion η = ", main_eta),
       x = NULL, y = "Consumption-equivalent change (%)") +
  theme_panel + guides(fill = "none")

atkinson_data <- consumption_data %>%
  filter(Year == end_year) %>%
  group_by(Scenario) %>%
  summarise(mean_cons = weighted.mean(cons_pc, population),
            ede_cons = ede_consumption(cons_pc, population, main_eta),
            Atkinson_percent = 100 * (1 - ede_cons / mean_cons), .groups = "drop") %>%
  mutate(ScenarioLabel = factor(recode(as.character(Scenario), !!!scenario_labels),
                                levels = c("Def", "Aid")))

atk_def <- atkinson_data$Atkinson_percent[atkinson_data$Scenario == "Def"]
atk_aid <- atkinson_data$Atkinson_percent[atkinson_data$Scenario == "Aid"]
atk_change <- atk_aid - atk_def

p_d <- ggplot(atkinson_data, aes(ScenarioLabel, Atkinson_percent)) +
  geom_col(width = 0.56, fill = "grey55") +
  geom_text(aes(label = sprintf("%.1f%%", Atkinson_percent)), vjust = -0.5, size = 3.8) +
  scale_y_continuous(labels = label_number(accuracy = 0.1, suffix = "%"),
                     expand = expansion(mult = c(0, 0.17))) +
  labs(title = paste0("c  Between-region inequality in ", end_year),
       #subtitle = paste0("Aid − Def = ", sprintf("%+.1f percentage points", atk_change)),
       x = NULL, y = "Atkinson index (%)") +
  theme_panel

# Layout and output ---------------------------------------------------------
right_column <- (p_c / p_d) + plot_layout(heights = c(1, 1))
composite_figure <- (p_a | right_column) + plot_layout(widths = c(1.75, 1))

plot(composite_figure)
ggsave(file.path(output_dir, "transfer_welfare_composite.png"), composite_figure,
       width = 15, height = 8.5, dpi = 600, bg = "white")
if (requireNamespace("svglite", quietly = TRUE)) {
  ggsave(file.path(output_dir, "transfer_welfare_composite.svg"), composite_figure,
         device = svglite::svglite, width = 15, height = 8.5, bg = "white")
}
