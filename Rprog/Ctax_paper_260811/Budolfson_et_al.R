# Budolfson-style welfare and between-region inequality assessment
#
# Inputs are AIM17 regional household consumption (CNS) and population (Pop)
# from the Def and Aid scenarios. The analysis covers 2030-2050.
# CNS is assumed to be billion US$2010/yr and Pop million people, so CNS/Pop is
# thousand US$2010 per person. CE and Atkinson results are invariant to a common
# rescaling of the consumption unit.

library(tidyverse)
library(gdxrrw)
library(patchwork)
library(scales)

# Paths work both when run from this script's folder and from the project root.
gdx_candidates <- c(
  "../../data/AR6database/global_17_IAMC.gdx",
  "data/AR6database/global_17_IAMC.gdx"
)
gdx_file <- gdx_candidates[file.exists(gdx_candidates)][1]
if (is.na(gdx_file)) stop("Could not find data/AR6database/global_17_IAMC.gdx.")

output_dir <- if (dir.exists("../../output")) {
  "../../output/Figure"
} else {
  "output/Figure"
}
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

scenario_names <- c(
  "SSP2_400C_2030CP_NoCC_No" = "Def",
  "SSP2_400C_2030CP_15th_NoCC_No" = "Aid"
)
scenario_levels <- c("Def", "Aid")
reference_scenario <- "Def"

# Change these two names here if a different AIM output definition is desired.
consumption_variable <- "CNS"
population_variable <- "Pop"

start_year <- 2030
end_year <- 2050
base_year <- 2030
rho <- 0.008
eta_values <- c(0, 1, 1.5, 2)
baseline_eta <- 1.5

providers <- c("XE25", "JPN", "TUR", "CHN", "USA", "XER", "XOC", "CAN", "XLM", "CIS")
recipients <- c("XSA", "IND", "XNF", "XAF", "XSE", "BRA", "XME")
aim17 <- c(providers, recipients)

region_names <- c(
  XE25 = "EU25", JPN = "Japan", TUR = "Türkiye", CHN = "China",
  USA = "United States", XER = "Rest of Europe", XOC = "Oceania",
  CAN = "Canada", XLM = "Latin America", CIS = "Former Soviet Union",
  XSA = "Rest of Asia", IND = "India", XNF = "North Africa",
  XAF = "Rest of Africa", XSE = "Southeast Asia", BRA = "Brazil",
  XME = "Middle East"
)

eta_colors <- c(
  `eta = 0` = "#6E6E6E", `eta = 1` = "#7B61A8",
  `eta = 1.5` = "#009E73", `eta = 2` = "#CC79A7"
)
group_colors <- c(Provider = "#2E6F9E", Recipient = "#D27755", World = "#333333")

# Vectorised CRRA utility, including the eta = 1 log-utility case.
crra_utility <- function(cons_pc, eta) {
  out <- rep(NA_real_, length(cons_pc))
  log_case <- abs(eta - 1) < 1e-12
  out[log_case] <- log(cons_pc[log_case])
  out[!log_case] <-
    cons_pc[!log_case] ^ (1 - eta[!log_case]) / (1 - eta[!log_case])
  out
}

# Consumption-equivalent change relative to the Def baseline.
# For eta = 1, scaling baseline consumption adds log(1 + lambda) to utility.
ce_from_welfare <- function(W, W0, eta, baseline_weight) {
  if (abs(eta - 1) < 1e-12) {
    return(exp((W - W0) / baseline_weight) - 1)
  }
  ratio <- W / W0
  if (!is.finite(ratio) || ratio <= 0) return(NA_real_)
  ratio ^ (1 / (1 - eta)) - 1
}

# Population-weighted equally distributed equivalent consumption.
ede_consumption <- function(cons_pc, population, eta) {
  ok <- is.finite(cons_pc) & cons_pc > 0 & is.finite(population) & population > 0
  cons_pc <- cons_pc[ok]
  population <- population[ok]
  share <- population / sum(population)
  if (abs(eta - 1) < 1e-12) {
    exp(sum(share * log(cons_pc)))
  } else {
    sum(share * cons_pc ^ (1 - eta)) ^ (1 / (1 - eta))
  }
}

# rgdx.param() uses the parameter's domain names as column names. In these
# scenario GDX files IAMC_template is declared as (REMF, VEMF, Y), whereas some
# combined GDX files use YEMF. Normalise either form before filtering.
find_iamc_column <- function(data, candidates, role) {
  hit <- names(data)[tolower(names(data)) %in% tolower(candidates)]
  if (length(hit) == 0) {
    stop(
      "Could not identify the ", role, " column in IAMC_template. Available columns: ",
      paste(names(data), collapse = ", ")
    )
  }
  hit[[1]]
}

standardise_iamc_columns <- function(data) {
  scenario_col <- find_iamc_column(data, c("SCENARIO", "Sc", "S"), "scenario")
  region_col <- find_iamc_column(data, c("REMF", "Sr", "R"), "region")
  variable_col <- find_iamc_column(data, c("VEMF", "Sv", "V"), "variable")
  year_col <- find_iamc_column(data, c("YEMF", "Y"), "year")
  value_col <- find_iamc_column(
    data,
    c("IAMC_Template", "IAMC_template", "value", "Val"),
    "value"
  )

  data %>%
    transmute(
      SCENARIO = as.character(.data[[scenario_col]]),
      REMF = as.character(.data[[region_col]]),
      VEMF = as.character(.data[[variable_col]]),
      Year = as.numeric(as.character(.data[[year_col]])),
      IAMC_value = as.numeric(.data[[value_col]])
    )
}

# Read CNS and Pop from the combined IAMC_template --------------------------
iamc_raw <- rgdx.param(gdx_file, "IAMC_template")

consumption_data <- standardise_iamc_columns(iamc_raw) %>%
  filter(SCENARIO %in% names(scenario_names),
         VEMF %in% c(consumption_variable, population_variable),
         REMF %in% aim17) %>%
  transmute(
    Scenario = recode(SCENARIO, !!!scenario_names),
    Region = REMF,
    Year,
    Variable = VEMF,
    value = IAMC_value
  ) %>%
  filter(Year >= start_year, Year <= end_year) %>%
  group_by(Scenario, Region, Year, Variable) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Variable, values_from = value) %>%
  rename(consumption = all_of(consumption_variable),
         population = all_of(population_variable)) %>%
  mutate(
    Scenario = factor(Scenario, levels = scenario_levels),
    Group = if_else(Region %in% providers, "Provider", "Recipient"),
    cons_pc = consumption / population,
    discount = 1 / ((1 + rho) ^ (Year - base_year))
  )

# Use the model years actually stored in the GDX. For the current file these
# are 2030, 2035, 2040, 2045, and 2050. Each observation represents a five-year
# period, so its welfare contribution is multiplied by delta-t = 5. The final
# point uses the median observed interval, following the same convention.
model_years <- sort(unique(consumption_data$Year))
if (length(model_years) < 2) {
  stop("At least two model years are required for intertemporal welfare.")
}
year_intervals <- diff(model_years)
if (any(year_intervals <= 0)) stop("Model years are not strictly increasing.")
if (length(unique(year_intervals)) > 1) {
  warning("Model-year intervals are uneven; each year uses the interval to the next observation.")
}
year_weights <- tibble(
  Year = model_years,
  dt = c(year_intervals, median(year_intervals))
)
consumption_data <- consumption_data %>%
  left_join(year_weights, by = "Year")

expected_grid <- expand_grid(
  Scenario = factor(scenario_levels, levels = scenario_levels),
  Region = aim17,
  Year = model_years
)
missing_grid <- expected_grid %>%
  anti_join(consumption_data, by = c("Scenario", "Region", "Year"))
if (nrow(missing_grid) > 0) {
  stop("CNS/Pop data are missing for ", nrow(missing_grid),
       " combination(s) across the model years: ",
       paste(model_years, collapse = ", "), ".")
}
if (any(!is.finite(consumption_data$consumption) | consumption_data$consumption <= 0)) {
  stop("CNS contains missing, non-finite, or non-positive values.")
}
if (any(!is.finite(consumption_data$population) | consumption_data$population <= 0)) {
  stop("Pop contains missing, non-finite, or non-positive values.")
}
message("Welfare integration uses model years ",
        paste(model_years, collapse = ", "),
        " with delta-t weights ", paste(year_weights$dt, collapse = ", "), ".")

# Welfare calculations -------------------------------------------------------
welfare_detail <- consumption_data %>%
  tidyr::crossing(eta = eta_values) %>%
  mutate(
    utility = crra_utility(cons_pc, eta),
    welfare = population * discount * dt * utility,
    welfare_weight = population * discount * dt
  )

welfare_global <- welfare_detail %>%
  group_by(Scenario, eta) %>%
  summarise(W = sum(welfare), Weight = sum(welfare_weight), .groups = "drop")

global_baseline <- welfare_global %>%
  filter(Scenario == reference_scenario) %>%
  transmute(eta, W0 = W, BaselineWeight = Weight)

global_ce <- welfare_global %>%
  left_join(global_baseline, by = "eta") %>%
  rowwise() %>%
  mutate(CE = ce_from_welfare(W, W0, eta, BaselineWeight),
         CE_percent = 100 * CE) %>%
  ungroup() %>%
  mutate(Eta = factor(paste0("eta = ", eta), levels = names(eta_colors)))

welfare_region <- welfare_detail %>%
  group_by(Scenario, Region, Group, eta) %>%
  summarise(W = sum(welfare), Weight = sum(welfare_weight), .groups = "drop")

# Additive provider/recipient decomposition of delta W, normalised by the
# absolute global baseline welfare so the vertical scale is dimensionless.
group_welfare <- welfare_region %>%
  filter(abs(eta - baseline_eta) < 1e-12) %>%
  group_by(Scenario, Group) %>%
  summarise(W = sum(W), .groups = "drop")

group_baseline <- group_welfare %>%
  filter(Scenario == reference_scenario) %>%
  select(Group, W0 = W)
world_W0 <- sum(group_baseline$W0)

group_delta <- group_welfare %>%
  left_join(group_baseline, by = "Group") %>%
  mutate(DeltaW = W - W0) %>%
  select(Scenario, Group, DeltaW) %>%
  bind_rows(
    group_by(., Scenario) %>%
      summarise(DeltaW = sum(DeltaW), .groups = "drop") %>%
      mutate(Group = "World")
  ) %>%
  mutate(
    NormalizedDeltaW = 100 * DeltaW / abs(world_W0),
    Group = factor(Group, levels = c("Provider", "Recipient", "World"))
  )

group_delta_aid <- group_delta %>%
  filter(Scenario == "Aid")

# Regional CE for Aid relative to Def (Budolfson baseline eta = 1.5).
regional_baseline <- welfare_region %>%
  filter(Scenario == reference_scenario, abs(eta - baseline_eta) < 1e-12) %>%
  select(Region, W0 = W, BaselineWeight = Weight)

regional_ce_aid <- welfare_region %>%
  filter(Scenario == "Aid", abs(eta - baseline_eta) < 1e-12) %>%
  left_join(regional_baseline, by = "Region") %>%
  rowwise() %>%
  mutate(CE = ce_from_welfare(W, W0, eta, BaselineWeight),
         CE_percent = 100 * CE) %>%
  ungroup() %>%
  mutate(
    RegionLabel = paste0(Region, " | ", unname(region_names[Region])),
    RegionLabel = fct_reorder(RegionLabel, CE_percent),
    Group = factor(Group, levels = c("Provider", "Recipient"))
  )

# Atkinson between-region inequality at 2050 -------------------------------
atkinson_2050 <- consumption_data %>%
  filter(Year == end_year) %>%
  tidyr::crossing(eta = eta_values) %>%
  group_by(Scenario, eta) %>%
  summarise(
    mean_cons = weighted.mean(cons_pc, population),
    ede_cons = ede_consumption(cons_pc, population, first(eta)),
    Atkinson = 1 - ede_cons / mean_cons,
    .groups = "drop"
  ) %>%
  mutate(
    Atkinson_percent = 100 * Atkinson,
    Eta = factor(paste0("eta = ", eta), levels = names(eta_colors))
  )

# Main figure: Aid vs No Aid -----------------------------------------------
scenario_label <- c(Def = "No aid", Aid = "Aid")
main_eta <- baseline_eta

# Panel a: provider/recipient per-capita consumption over time
group_cons_change <- consumption_data %>%
  group_by(Scenario, Group, Year) %>%
  summarise(consumption = sum(consumption), population = sum(population), .groups = "drop") %>%
  mutate(cons_pc = consumption / population) %>%
  select(Scenario, Group, Year, cons_pc) %>%
  pivot_wider(names_from = Scenario, values_from = cons_pc) %>%
  mutate(Change_pct = 100 * (Aid / Def - 1), Group = factor(Group, levels = c("Provider", "Recipient")))

p_time <- ggplot(group_cons_change, aes(Year, Change_pct, color = Group)) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey50") +
  geom_line(linewidth = 1.15) +
  geom_point(size = 2.5) +
  scale_color_manual(values = group_colors[c("Provider", "Recipient")]) +
  scale_x_continuous(breaks = model_years) +
  scale_y_continuous(labels = label_number(accuracy = 0.1, suffix = "%")) +
  labs(title = "a  Economic effect of international aid", subtitle = "Per-capita household consumption: Aid relative to No aid",
       x = NULL, y = "Change from No aid")

# Panel b: AIM17 regional per-capita consumption in 2050
regional_change_2050 <- consumption_data %>%
  filter(Year == end_year) %>%
  select(Scenario, Region, Group, cons_pc) %>%
  pivot_wider(names_from = Scenario, values_from = cons_pc) %>%
  mutate(Change_pct = 100 * (Aid / Def - 1), RegionLabel = fct_reorder(unname(region_names[Region]), Change_pct),
         Group = factor(Group, levels = c("Provider", "Recipient")))

p_region <- ggplot(regional_change_2050, aes(RegionLabel, Change_pct, fill = Group)) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey45") +
  geom_col(width = 0.72) +
  coord_flip() +
  scale_fill_manual(values = group_colors[c("Provider", "Recipient")]) +
  scale_y_continuous(labels = label_number(accuracy = 0.1, suffix = "%"), expand = expansion(mult = c(0.08, 0.08))) +
  labs(title = paste0("b  Regional effect in ", end_year), subtitle = "Per-capita consumption: Aid relative to No aid",
       x = NULL, y = "Change from No aid")

# Panel c: consumption-equivalent welfare for provider/recipient/world
group_ce_regional <- welfare_detail %>%
  filter(abs(eta - main_eta) < 1e-12) %>%
  group_by(Scenario, Group) %>%
  summarise(W = sum(welfare), Weight = sum(welfare_weight), .groups = "drop")

group_ce_world <- welfare_global %>%
  filter(abs(eta - main_eta) < 1e-12) %>%
  transmute(Scenario, Group = "World", W, Weight)

group_ce_data <- bind_rows(group_ce_regional, group_ce_world)
group_ce_baseline <- group_ce_data %>%
  filter(Scenario == reference_scenario) %>%
  transmute(Group, W0 = W, BaselineWeight = Weight)

group_ce_aid <- group_ce_data %>%
  filter(Scenario == "Aid") %>%
  left_join(group_ce_baseline, by = "Group") %>%
  rowwise() %>%
  mutate(CE = ce_from_welfare(W, W0, main_eta, BaselineWeight), CE_percent = 100 * CE) %>%
  ungroup() %>%
  mutate(Group = factor(Group, levels = c("Provider", "Recipient", "World")))

p_ce_main <- ggplot(group_ce_aid, aes(Group, CE_percent, fill = Group)) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.4) +
  geom_col(width = 0.62) +
  geom_text(aes(label = sprintf("%+.2f%%", CE_percent)), vjust = ifelse(group_ce_aid$CE_percent >= 0, -0.45, 1.3), size = 4) +
  scale_fill_manual(values = group_colors) +
  scale_y_continuous(labels = label_number(accuracy = 0.01, suffix = "%"), expand = expansion(mult = c(0.12, 0.15))) +
  labs(title = "c  Welfare-equivalent effect", subtitle = paste0("2030–2050, inequality aversion η = ", main_eta),
       x = NULL, y = "Consumption-equivalent change")

# Panel d: Atkinson between-region inequality in 2050
atkinson_main <- atkinson_2050 %>%
  filter(abs(eta - main_eta) < 1e-12) %>%
  mutate(ScenarioLabel = recode(as.character(Scenario), !!!scenario_label),
         ScenarioLabel = factor(ScenarioLabel, levels = c("No aid", "Aid")))

atk_def <- atkinson_main$Atkinson_percent[atkinson_main$Scenario == "Def"]
atk_aid <- atkinson_main$Atkinson_percent[atkinson_main$Scenario == "Aid"]
atk_change <- atk_aid - atk_def

p_atkinson_main <- ggplot(atkinson_main, aes(ScenarioLabel, Atkinson_percent)) +
  geom_col(width = 0.56, fill = "grey55") +
  geom_text(aes(label = sprintf("%.1f%%", Atkinson_percent)), vjust = -0.5, size = 4) +
  scale_y_continuous(labels = label_number(accuracy = 0.1, suffix = "%"), expand = expansion(mult = c(0, 0.15))) +
  labs(title = paste0("d  Between-region inequality in ", end_year),
       subtitle = paste0("Aid − No aid = ", sprintf("%+.2f percentage points", atk_change)),
       x = NULL, y = "Atkinson index")

# Common theme and layout ----------------------------------------------------
theme_main <- theme_bw(base_size = 12.5) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 13, face = "plain"),
        plot.subtitle = element_text(size = 10.5, color = "grey35"),
        axis.title = element_text(face = "plain"), legend.position = "bottom", legend.title = element_blank(),
        plot.margin = margin(8, 8, 8, 8))

p_time <- p_time + theme_main
p_region <- p_region + theme_main + theme(axis.text.y = element_text(size = 9))
p_ce_main <- p_ce_main + theme_main + guides(fill = "none")
p_atkinson_main <- p_atkinson_main + theme_main

figure_main <- p_time / (p_region | (p_ce_main / p_atkinson_main)) +
  plot_layout(heights = c(0.8, 1.55), widths = c(1.55, 1), guides = "collect")
figure_main <- figure_main & theme(legend.position = "bottom")

plot(figure_main)
ggsave(file.path(output_dir, "Aid_vs_NoAid_main_figure.png"), figure_main, width = 13, height = 11, dpi = 600, bg = "white")
if (requireNamespace("svglite", quietly = TRUE)) {
  ggsave(file.path(output_dir, "Aid_vs_NoAid_main_figure.svg"), figure_main, device = svglite::svglite,
         width = 13, height = 11, bg = "white")
}
