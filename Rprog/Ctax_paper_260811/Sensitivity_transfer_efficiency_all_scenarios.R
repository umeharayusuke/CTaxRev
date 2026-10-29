# Annual Consumption/Transfer efficiency for all valid aid scenarios --------
#
# This is the efficiency-only sensitivity counterpart to panel d of
# 03_transfer_timing_consumption_gdp.png. For each aid threshold and SSP:
#   developing ratio = (Default loss - Aid loss) / transfer received
#   developed ratio  = -(Aid loss - Default loss) / transfer provided
# The developed ratio is shown below zero, following the source panel. SSP3
# 10k and 15k are excluded because those runs are invalid for the full horizon.

find_project_root <- function(start) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    marker <- file.path(current, "data", "AR6database", "global_17_IAMC.gdx")
    if (file.exists(marker)) return(current)
    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }
  NA_character_
}

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg[[1]]), winslash = "/"))
} else {
  getwd()
}

project_root <- find_project_root(getwd())
if (is.na(project_root)) project_root <- find_project_root(script_dir)
if (is.na(project_root)) stop("Could not locate the project root.")

library_candidates <- c(
  Sys.getenv("R_LIBS_USER"),
  file.path(project_root, "Rlib"),
  "C:/ENVI5809/R_library",
  "C:/Users/umeha/AppData/Local/R/win-library/4.5"
)
library_candidates <- unique(
  library_candidates[nzchar(library_candidates) & dir.exists(library_candidates)]
)
.libPaths(unique(c(library_candidates, .libPaths())))

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(gdxrrw)
})

gams_candidates <- c(
  Sys.getenv("GAMS_SYSDIR", unset = ""),
  "C:/GAMS/34",
  "C:/GAMS/win64/26.1"
)
gams_candidates <- unique(
  gams_candidates[nzchar(gams_candidates) & dir.exists(gams_candidates)]
)
if (!length(gams_candidates)) stop("A GAMS system directory was not found.")
igdx(gams_candidates[[1]])

gdx_file <- file.path(
  project_root, "data", "AR6database", "global_17_IAMC.gdx"
)
output_dir <- file.path(project_root, "output", "Figure", "analysis")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_stem <- file.path(
  output_dir, "11_transfer_efficiency_all_scenarios_2030_2050"
)

# Settings and provider/recipient classifications ---------------------------

model_years <- seq(2030, 2050, by = 5)
ssp_levels <- paste0("SSP", 1:3)
threshold_levels <- c("10th", "15th", "20th")

region_membership <- bind_rows(
  tibble(
    Threshold = "10th",
    Region_code = c("IND", "XSA", "XNF", "XAF"),
    Role = "Developing"
  ),
  tibble(
    Threshold = "10th",
    Region_code = c(
      "XE25", "XER", "TUR", "XOC", "CHN", "JPN", "XSE",
      "CAN", "USA", "BRA", "XLM", "CIS", "XME"
    ),
    Role = "Developed"
  ),
  tibble(
    Threshold = "15th",
    Region_code = c("IND", "BRA", "XSA", "XSE", "XME", "XNF", "XAF"),
    Role = "Developing"
  ),
  tibble(
    Threshold = "15th",
    Region_code = c(
      "XE25", "XER", "TUR", "XOC", "CHN", "JPN", "CAN", "USA",
      "XLM", "CIS"
    ),
    Role = "Developed"
  ),
  tibble(
    Threshold = "20th",
    Region_code = c(
      "CHN", "IND", "BRA", "XSA", "XSE", "XME", "XLM", "XNF",
      "XAF", "CIS"
    ),
    Role = "Developing"
  ),
  tibble(
    Threshold = "20th",
    Region_code = c("XE25", "XER", "TUR", "XOC", "JPN", "CAN", "USA"),
    Role = "Developed"
  )
)

membership_check <- region_membership %>%
  count(Threshold, Region_code) %>%
  filter(n != 1L)
if (nrow(membership_check)) {
  stop("Each region must occur exactly once within each threshold classification.")
}
coverage_check <- region_membership %>% count(Threshold)
if (any(coverage_check$n != 17L)) {
  stop("Each threshold must classify all 17 AIM regions.")
}

scenario_grid <- crossing(
  SSP = ssp_levels,
  Threshold = threshold_levels
) %>%
  mutate(
    Default_scenario = paste0(SSP, "_400C_2030CP_NoCC_No"),
    Aid_scenario = paste0(SSP, "_400C_2030CP_", Threshold, "_NoCC_No")
  ) %>%
  filter(!(SSP == "SSP3" & Threshold %in% c("10th", "15th")))

scenarios_needed <- unique(c(
  scenario_grid$Default_scenario,
  scenario_grid$Aid_scenario
))
variables_needed <- c("Pol_Cos_Cns_Los", "CNS", "Rev_gov_Tax_Car_Tax")

# IAMC data -----------------------------------------------------------------

message("Reading IAMC_template from: ", gdx_file)
raw <- rgdx.param(gdx_file, "IAMC_template")
required_columns <- c("SCENARIO", "REMF", "VEMF", "YEMF", "IAMC_Template")
if (!all(required_columns %in% names(raw))) {
  stop("IAMC_template does not have the expected columns.")
}

iamc <- raw %>%
  transmute(
    Scenario = as.character(SCENARIO),
    Region = as.character(REMF),
    Variable = as.character(VEMF),
    Year = as.numeric(as.character(YEMF)),
    Value = as.numeric(IAMC_Template)
  ) %>%
  filter(
    Scenario %in% scenarios_needed,
    Region %in% unique(region_membership$Region_code),
    Variable %in% variables_needed,
    Year %in% model_years
  )

if (anyDuplicated(iamc[c("Scenario", "Region", "Variable", "Year")])) {
  stop("Duplicate IAMC observations were found.")
}

sum_required <- function(variable, scenario, regions, year) {
  values <- iamc %>%
    filter(
      Variable == variable,
      Scenario == scenario,
      Region %in% regions,
      Year == year
    )
  if (nrow(values) != length(regions) || any(!is.finite(values$Value))) {
    return(NA_real_)
  }
  sum(values$Value)
}

# Calculate exactly the two ratios used in panel d of the source figure.
efficiency_wide <- bind_rows(lapply(seq_len(nrow(scenario_grid)), function(i) {
  scenario <- scenario_grid[i, ]
  providers <- region_membership %>%
    filter(Threshold == scenario$Threshold, Role == "Developed") %>%
    pull(Region_code)
  recipients <- region_membership %>%
    filter(Threshold == scenario$Threshold, Role == "Developing") %>%
    pull(Region_code)

  bind_rows(lapply(model_years, function(year) {
    transfer_musd <- sum_required(
      "Rev_gov_Tax_Car_Tax", scenario$Aid_scenario, providers, year
    )
    transfer_busd <- transfer_musd / 1000

    recipient_default_loss <- sum_required(
      "Pol_Cos_Cns_Los", scenario$Default_scenario, recipients, year
    )
    recipient_aid_loss <- sum_required(
      "Pol_Cos_Cns_Los", scenario$Aid_scenario, recipients, year
    )
    provider_default_loss <- sum_required(
      "Pol_Cos_Cns_Los", scenario$Default_scenario, providers, year
    )
    provider_aid_loss <- sum_required(
      "Pol_Cos_Cns_Los", scenario$Aid_scenario, providers, year
    )

    recipient_default_cns <- sum_required(
      "CNS", scenario$Default_scenario, recipients, year
    )
    recipient_aid_cns <- sum_required(
      "CNS", scenario$Aid_scenario, recipients, year
    )
    provider_default_cns <- sum_required(
      "CNS", scenario$Default_scenario, providers, year
    )
    provider_aid_cns <- sum_required(
      "CNS", scenario$Aid_scenario, providers, year
    )

    complete <- all(is.finite(c(
      transfer_busd,
      recipient_default_loss, recipient_aid_loss,
      provider_default_loss, provider_aid_loss,
      recipient_default_cns, recipient_aid_cns,
      provider_default_cns, provider_aid_cns
    ))) && transfer_busd > 0

    if (!complete) {
      return(tibble(
        SSP = scenario$SSP,
        Threshold = scenario$Threshold,
        Aid_scenario = scenario$Aid_scenario,
        Year = year,
        Transfer_billion_USD2010 = ifelse(
          is.finite(transfer_busd), transfer_busd, NA_real_
        ),
        Recipient_benefit_billion_USD2010 = NA_real_,
        Provider_burden_billion_USD2010 = NA_real_,
        Recipient_efficiency = NA_real_,
        Provider_burden_ratio = NA_real_,
        Recipient_identity_gap = NA_real_,
        Provider_identity_gap = NA_real_
      ))
    }

    recipient_benefit <- recipient_default_loss - recipient_aid_loss
    provider_burden <- provider_aid_loss - provider_default_loss
    recipient_cns_gain <- recipient_aid_cns - recipient_default_cns
    provider_cns_loss <- provider_default_cns - provider_aid_cns

    tibble(
      SSP = scenario$SSP,
      Threshold = scenario$Threshold,
      Aid_scenario = scenario$Aid_scenario,
      Year = year,
      Transfer_billion_USD2010 = transfer_busd,
      Recipient_benefit_billion_USD2010 = recipient_benefit,
      Provider_burden_billion_USD2010 = provider_burden,
      Recipient_efficiency = recipient_benefit / transfer_busd,
      Provider_burden_ratio = provider_burden / transfer_busd,
      Recipient_identity_gap = recipient_benefit - recipient_cns_gain,
      Provider_identity_gap = provider_burden - provider_cns_loss
    )
  }))
}))

finite_identity_gaps <- c(
  efficiency_wide$Recipient_identity_gap,
  efficiency_wide$Provider_identity_gap
)
finite_identity_gaps <- finite_identity_gaps[is.finite(finite_identity_gaps)]
maximum_identity_gap <- if (length(finite_identity_gaps)) {
  max(abs(finite_identity_gaps))
} else {
  NA_real_
}
if (!is.finite(maximum_identity_gap) || maximum_identity_gap > 1e-6) {
  stop("Pol_Cos_Cns_Los and CNS imply inconsistent annual consumption effects.")
}

efficiency_long <- efficiency_wide %>%
  select(
    SSP, Threshold, Aid_scenario, Year,
    Transfer_billion_USD2010,
    Recipient_efficiency, Provider_burden_ratio
  ) %>%
  pivot_longer(
    c(Recipient_efficiency, Provider_burden_ratio),
    names_to = "Effect",
    values_to = "Absolute_ratio"
  ) %>%
  mutate(
    Signed_ratio = if_else(
      Effect == "Provider_burden_ratio",
      -Absolute_ratio,
      Absolute_ratio
    ),
    Effect = recode(
      Effect,
      "Recipient_efficiency" = "Developing: consumption gain / transfer",
      "Provider_burden_ratio" = "Developed: consumption loss / transfer"
    ),
    SSP = factor(SSP, levels = ssp_levels),
    Threshold = factor(Threshold, levels = threshold_levels),
    Effect = factor(
      Effect,
      levels = c(
        "Developing: consumption gain / transfer",
        "Developed: consumption loss / transfer"
      )
    )
  )

valid_plot_data <- efficiency_long %>% filter(is.finite(Signed_ratio))
if (!nrow(valid_plot_data)) stop("No finite transfer-efficiency values were found.")

scenario_validity <- efficiency_long %>%
  group_by(SSP, Threshold) %>%
  summarise(
    Finite_points = sum(is.finite(Signed_ratio)),
    .groups = "drop"
  )
if (any(scenario_validity$Finite_points == 0L)) {
  warning(
    "Entire scenario series omitted because all efficiency values are missing: ",
    paste(
      paste0(
        scenario_validity$SSP[scenario_validity$Finite_points == 0L], " ",
        scenario_validity$Threshold[scenario_validity$Finite_points == 0L]
      ),
      collapse = ", "
    )
  )
}

write.csv(
  efficiency_long %>%
    mutate(
      SSP = as.character(SSP),
      Threshold = as.character(Threshold),
      Effect = as.character(Effect)
    ),
  paste0(output_stem, "_data.csv"),
  row.names = FALSE
)

# Figure ---------------------------------------------------------------------

threshold_colours <- c(
  "10th" = "#0072B2",
  "15th" = "#D55E00",
  "20th" = "#009E73"
)
threshold_labels <- c(
  "10th" = "10k USD",
  "15th" = "15k USD",
  "20th" = "20k USD"
)
effect_shapes <- c(
  "Developing: consumption gain / transfer" = 16,
  "Developed: consumption loss / transfer" = 1
)
effect_linetypes <- c(
  "Developing: consumption gain / transfer" = "solid",
  "Developed: consumption loss / transfer" = "22"
)
effect_labels <- c(
  "Developing: consumption gain / transfer" = "Developing gain / transfer",
  "Developed: consumption loss / transfer" = "Developed loss / transfer"
)
ink <- "#252525"

theme_nature <- function() {
  theme_classic(base_family = "Arial", base_size = 8) +
    theme(
      text = element_text(colour = ink),
      axis.title = element_text(size = 8.3),
      axis.text = element_text(size = 7.4, colour = ink),
      axis.ticks = element_line(colour = ink, linewidth = 0.25),
      axis.ticks.length = grid::unit(1.2, "mm"),
      axis.line = element_blank(),
      panel.border = element_rect(fill = NA, colour = ink, linewidth = 0.32),
      panel.grid.major.y = element_line(colour = "#E2E2E2", linewidth = 0.22),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = 8.2),
      panel.spacing.x = grid::unit(3.0, "mm"),
      legend.position = "bottom",
      legend.justification = "center",
      legend.box = "vertical",
      legend.title = element_text(size = 7.3, face = "plain"),
      legend.text = element_text(size = 7.1),
      legend.key.height = grid::unit(3.5, "mm"),
      legend.key.width = grid::unit(6.0, "mm"),
      legend.spacing.x = grid::unit(1.2, "mm"),
      plot.margin = margin(4, 5, 3, 4, unit = "mm")
    )
}

p <- ggplot(
  valid_plot_data,
  aes(
    x = Year,
    y = Signed_ratio,
    colour = Threshold,
    shape = Effect,
    linetype = Effect,
    group = interaction(Threshold, Effect)
  )
) +
  geom_hline(yintercept = 0, colour = ink, linewidth = 0.34) +
  geom_line(linewidth = 0.66, na.rm = FALSE) +
  geom_point(size = 2.15, stroke = 0.62, na.rm = TRUE) +
  facet_wrap(~SSP, nrow = 1, drop = FALSE) +
  scale_colour_manual(
    values = threshold_colours,
    breaks = threshold_levels,
    labels = threshold_labels,
    drop = FALSE,
    name = "GDP per capita threshold"
  ) +
  scale_shape_manual(
    values = effect_shapes,
    breaks = names(effect_shapes),
    labels = effect_labels,
    drop = FALSE,
    name = "Consumption effect"
  ) +
  scale_linetype_manual(
    values = effect_linetypes,
    breaks = names(effect_linetypes),
    labels = effect_labels,
    drop = FALSE,
    name = "Consumption effect"
  ) +
  scale_x_continuous(
    breaks = sort(unique(valid_plot_data$Year)),
    limits = range(valid_plot_data$Year),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    labels = label_number(accuracy = 0.1),
    expand = expansion(mult = c(0.10, 0.12))
  ) +
  labs(
    x = "Year",
    y = "Consumption / Transfer\n(+ developing gain; - developed loss)"
  ) +
  guides(
    colour = guide_legend(order = 1, nrow = 1),
    shape = guide_legend(order = 2, nrow = 1),
    linetype = guide_legend(order = 2, nrow = 1)
  ) +
  theme_nature()

ggsave(
  filename = paste0(output_stem, ".png"),
  plot = p,
  device = "png",
  type = "cairo",
  width = 180,
  height = 105,
  units = "mm",
  dpi = 400,
  bg = "white",
  limitsize = FALSE
)

message("Created: ", paste0(output_stem, ".png"))
message("Created: ", paste0(output_stem, "_data.csv"))
message("Maximum CNS identity gap (billion US$2010/yr): ", maximum_identity_gap)
message("Finite plotted points by SSP and threshold:")
print(scenario_validity, n = Inf)
