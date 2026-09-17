# AIM17 consumption loss, between-region inequality, and welfare in 2050
# Run from the project root or from Rprog/Ctax_paper_260811.

library_paths <- c(Sys.getenv("R_LIBS_USER"), "C:/ENVI5809/R_library")
library_paths <- library_paths[nzchar(library_paths) & dir.exists(library_paths)]
.libPaths(unique(c(library_paths, .libPaths())))

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(patchwork)
  library(gdxrrw)
})

# Inputs and definitions -----------------------------------------------------
gdx_candidates <- c("data/AR6database/global_17_IAMC.gdx",
                    "../../data/AR6database/global_17_IAMC.gdx")
gdx_file <- gdx_candidates[file.exists(gdx_candidates)][1]
if (is.na(gdx_file)) stop("Cannot find data/AR6database/global_17_IAMC.gdx.")
output_dir <- if (dir.exists("output")) "output/Figure" else "../../output/Figure"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

gams_candidates <- c(Sys.getenv("GAMS_SYSDIR"), "C:/GAMS/34", "C:/GAMS/win64/26.1")
gams_candidates <- gams_candidates[nzchar(gams_candidates) & dir.exists(gams_candidates)]
if (length(gams_candidates)) igdx(gams_candidates[1])

scenario_codes <- c(Def = "SSP2_400C_2030CP_NoCC_No",
                    Aid = "SSP2_400C_2030CP_15th_NoCC_No")
scenario_levels <- c("Def", "Aid")
providers <- c("XE25", "JPN", "USA", "XER", "XOC", "CAN", "TUR", "XLM", "CHN", "CIS")
recipients <- c("XME", "XNF", "BRA", "XAF", "XSE", "IND", "XSA")
aim17 <- c(providers, recipients)
region_names <- c(
  XE25 = "EU25", JPN = "Japan", TUR = "Turkiye", CHN = "China",
  USA = "United States", XER = "Rest of Europe", XOC = "Oceania",
  CAN = "Canada", XLM = "Latin America", CIS = "Former Soviet Union",
  XSA = "Rest of Asia", IND = "India", XNF = "North Africa",
  XAF = "Rest of Africa", XSE = "Southeast Asia", BRA = "Brazil",
  XME = "Middle East"
)
target_year <- 2050
eta_values <- c(1, 1.5, 2)
central_eta <- 1.5
required_variables <- c("Pol_Cos_Cns_Los_rat_NPV_5pc", "CNS", "Pop")
scenario_colors <- c(Def = "#76818A", Aid = "#177C81")
ink <- "#193747"

# Population-weighted equally distributed equivalent (EDE) consumption.
# CNS: billion US$2010/yr; Pop: million people; CNS/Pop: thousand US$2010/person/yr.
ede_consumption <- function(cons_pc, population, eta) {
  share <- population / sum(population)
  if (abs(eta - 1) < 1e-12) {
    exp(sum(share * log(cons_pc)))
  } else {
    sum(share * cons_pc^(1 - eta))^(1 / (1 - eta))
  }
}

# Read once, select only the 17 regions and 2050, and check every input cell.
raw <- rgdx.param(gdx_file, "IAMC_template")
year_column <- intersect(c("YEMF", "Y"), names(raw))[1]
if (is.na(year_column)) stop("Cannot identify the year column in IAMC_template.")
keep <- as.character(raw[["SCENARIO"]]) %in% unname(scenario_codes) &
  as.character(raw[["REMF"]]) %in% aim17 &
  as.character(raw[["VEMF"]]) %in% required_variables &
  as.character(raw[[year_column]]) == as.character(target_year)
data <- raw[keep, , drop = FALSE] %>%
  transmute(Scenario_id = as.character(SCENARIO), Region = as.character(REMF),
            Variable = as.character(VEMF), Value = as.numeric(IAMC_Template),
            Scenario = factor(names(scenario_codes)[match(Scenario_id, scenario_codes)],
                              levels = scenario_levels))
rm(raw)

expected <- expand_grid(Scenario_id = unname(scenario_codes), Region = aim17,
                        Variable = required_variables)
missing <- anti_join(expected, data, by = c("Scenario_id", "Region", "Variable"))
if (nrow(missing)) {
  stop("Missing 2050 AIM17 input records: ", nrow(missing), ". First: ",
       paste(unlist(missing[1, ]), collapse = " / "))
}
if (anyDuplicated(data[c("Scenario_id", "Region", "Variable")])) {
  stop("Duplicate scenario-region-variable records; cannot calculate unambiguously.")
}
if (any(!is.finite(data$Value))) stop("Input values include NA, NaN, or Inf.")

# a: 2050 value of the model's cumulative consumption-loss-rate variable.
loss <- data %>%
  filter(Variable == "Pol_Cos_Cns_Los_rat_NPV_5pc") %>%
  mutate(Region_plot = factor(Region, levels = aim17, labels = unname(region_names[aim17])))
loss_pairs <- loss %>%
  select(Region, Region_plot, Scenario, Value) %>%
  pivot_wider(names_from = Scenario, values_from = Value)

# b/c: cross-regional Atkinson index and EDE-based welfare index in 2050.
# WI is indexed to Def = 100 at each eta, because the present GDX comparison is Def/Aid.
consumption <- data %>%
  filter(Variable %in% c("CNS", "Pop")) %>%
  select(Scenario, Region, Variable, Value) %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  mutate(cons_pc = CNS / Pop)
if (any(!is.finite(consumption$CNS) | consumption$CNS <= 0 |
        !is.finite(consumption$Pop) | consumption$Pop <= 0 |
        !is.finite(consumption$cons_pc) | consumption$cons_pc <= 0)) {
  stop("CNS and Pop must be finite and strictly positive in all 34 scenario-region rows.")
}

metrics <- consumption %>%
  crossing(eta = eta_values) %>%
  group_by(Scenario, eta) %>%
  summarise(Mean_pc = sum(CNS) / sum(Pop),
            EDE_pc = ede_consumption(cons_pc, Pop, first(eta)), .groups = "drop") %>%
  mutate(Atkinson = 100 * (1 - EDE_pc / Mean_pc))
reference_ede <- metrics %>% filter(Scenario == "Def") %>%
  select(eta, EDE_Def = EDE_pc)
metrics <- metrics %>%
  left_join(reference_ede, by = "eta") %>%
  mutate(WI = 100 * EDE_pc / EDE_Def)
if (any(!is.finite(metrics$Atkinson) | !is.finite(metrics$WI))) {
  stop("Atkinson index or welfare index could not be calculated.")
}

metric_summary <- metrics %>%
  group_by(Scenario) %>%
  summarise(Atkinson_min = min(Atkinson), Atkinson_max = max(Atkinson),
            Atkinson_mid = Atkinson[which.min(abs(eta - central_eta))],
            WI_min = min(WI), WI_max = max(WI),
            WI_mid = WI[which.min(abs(eta - central_eta))], .groups = "drop") %>%
  mutate(Scenario = factor(as.character(Scenario), levels = c("Aid", "Def")))

# Three vertically stacked panels ------------------------------------------
theme_paper <- theme_minimal(base_size = 12.5) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#E4EAEC", linewidth = 0.35),
        plot.title = element_text(face = "bold", size = 14, color = ink),
        axis.title = element_text(color = ink),
        axis.text = element_text(color = ink), legend.position = "bottom",
        plot.margin = margin(8, 12, 8, 12))

p_a <- ggplot() +
  annotate("rect", xmin = 0.5, xmax = length(providers) + 0.5,
           ymin = 0, ymax = Inf, fill = "#EDF5F8") +
  annotate("rect", xmin = 0.5, xmax = length(providers) + 0.5,
           ymin = -Inf, ymax = 0, fill = "#F7FAFB") +
  annotate("rect", xmin = length(providers) + 0.5, xmax = length(aim17) + 0.5,
           ymin = 0, ymax = Inf, fill = "#FFF5EC") +
  annotate("rect", xmin = length(providers) + 0.5, xmax = length(aim17) + 0.5,
           ymin = -Inf, ymax = 0, fill = "#EEF7F3") +
  geom_hline(yintercept = 0, color = "#71848D", linewidth = 0.65) +
  geom_vline(xintercept = length(providers) + 0.5, linetype = "dotted",
             color = "#71848D", linewidth = 0.75) +
  geom_segment(data = loss_pairs,
               aes(x = Region_plot, xend = Region_plot, y = Def, yend = Aid),
               linetype = "dashed", linewidth = 0.7, color = "#9BA8AE") +
  geom_point(data = loss, aes(Region_plot, Value, color = Scenario), size = 2.8) +
  scale_color_manual(values = scenario_colors, breaks = scenario_levels, name = NULL) +
  scale_y_continuous(labels = label_number(accuracy = 0.1, suffix = "%")) +
  labs(title = "a  Cumulative consumption loss rate by AIM17 region",
       x = NULL, y = "Cumulative consumption loss rate (%)") +
  theme_paper + theme(axis.text.x = element_text(angle = 52, hjust = 1, vjust = 1, size = 9),
                      panel.grid.major.x = element_blank())

make_metric_panel <- function(data, prefix, lower, middle, upper, title,
                              axis_label, reference = NULL, suffix = "") {
  view <- data %>%
    transmute(Scenario, Lower = .data[[lower]], Middle = .data[[middle]],
              Upper = .data[[upper]], Label = sprintf("%.2f%s", Middle, suffix))
  p <- ggplot(view, aes(y = Scenario)) +
    geom_segment(aes(x = Lower, xend = Upper, yend = Scenario),
                 color = "#AACDD0", linewidth = 3.2, lineend = "round") +
    geom_point(aes(x = Middle, fill = Scenario), shape = 21, color = ink,
               stroke = 0.45, size = 4.5) +
    geom_text(aes(x = Middle, label = Label), nudge_y = 0.23, size = 3.7,
              color = ink, show.legend = FALSE) +
    scale_fill_manual(values = scenario_colors, guide = "none") +
    scale_x_continuous(labels = label_number(accuracy = 0.1, suffix = suffix),
                       expand = expansion(mult = c(0.11, 0.11))) +
    labs(title = paste0(prefix, "  ", title), x = axis_label, y = NULL) + theme_paper +
    theme(legend.position = "none")
  if (!is.null(reference)) {
    p <- p + geom_vline(xintercept = reference, linetype = "dotted",
                        linewidth = 0.6, color = "#819199")
  }
  p
}

p_b <- make_metric_panel(
  metric_summary, "b", "Atkinson_min", "Atkinson_mid", "Atkinson_max",
  "Between-region Atkinson index", "Atkinson index (%)", suffix = "%"
)
p_c <- make_metric_panel(
  metric_summary, "c", "WI_min", "WI_mid", "WI_max",
  "Welfare index (WI)", "WI (Def = 100)", reference = 100
)

figure <- (p_a / p_b / p_c) + plot_layout(heights = c(2.3, 1, 1))

output_stem <- file.path(output_dir, "AIM17_loss_Atkinson_WI_2050")
ggsave(paste0(output_stem, ".png"), figure, device = ragg::agg_png,
       width = 15.5, height = 12.5, dpi = 300, bg = "white")
ggsave(paste0(output_stem, ".svg"), figure, device = svglite::svglite,
       width = 15.5, height = 12.5, bg = "white")
print(metrics %>% select(Scenario, eta, Atkinson, WI) %>% arrange(eta, Scenario))
message("Saved: ", output_stem, ".png and .svg")
