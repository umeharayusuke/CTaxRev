# Transfer-recovery scatter plot ---------------------------------------------

# Make the script independent of the RStudio working directory and use the
# project-local package libraries when they are available.
project_library_candidates <- c(
  file.path(getwd(), "Rlib"),
  file.path(getwd(), "..", "Rlib"),
  file.path(getwd(), "..", "..", "Rlib"),
  "C:/ENVI5809/R_library",
  "C:/Users/umeha/AppData/Local/R/win-library/4.5"
)
project_library_candidates <- project_library_candidates[
  dir.exists(project_library_candidates)
]
if (length(project_library_candidates) > 0) {
  .libPaths(unique(c(project_library_candidates, .libPaths())))
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(gdxrrw)
  library(scales)
})

find_project_dir <- function(start_dir) {
  candidate <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(candidate, "CTaxRev.Rproj"))) return(candidate)
    parent <- dirname(candidate)
    if (identical(parent, candidate)) {
      stop("Could not find the CTaxRev project root.")
    }
    candidate <- parent
  }
}

command_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", command_args, value = TRUE)
start_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(
    sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE
  ))
} else {
  getwd()
}

project_dir <- find_project_dir(start_dir)
gdx_file <- file.path(
  project_dir, "data", "AR6database", "global_17_IAMC.gdx"
)
output_dir <- file.path(project_dir, "output", "Figure")
csv_dir <- file.path(output_dir, "csv")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(csv_dir, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(gdx_file)) {
  stop("GDX input was not found: ", gdx_file)
}

gams_candidates <- unique(c(
  Sys.getenv("GAMS_SYSDIR", unset = ""),
  "C:/GAMS/34",
  "C:/GAMS/win64/26.1"
))
gams_candidates <- gams_candidates[nzchar(gams_candidates)]
gams_dir <- gams_candidates[dir.exists(gams_candidates)][1]
if (is.na(gams_dir)) {
  stop("GAMS system directory was not found; gdxrrw cannot read the GDX.")
}
igdx(gams_dir)

scenario_def <- "SSP2_400C_2030CP_NoCC_No"
scenario_aid <- "SSP2_400C_2030CP_15th_NoCC_No"
scenario_bau <- "SSP2_BaU_NoCC_No"
start_year <- 2030
end_year <- 2050

providers <- c("XE25","JPN","TUR","CHN","USA","XER","XOC","CAN","XLM","CIS")
recipients <- c("XSA","IND","XNF","XAF","XSE","BRA","XME")
all_regions <- c(providers, recipients)

provider_color <- "#356C9B"
recipient_color <- "#D27755"

theme_paper <- theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#E5E5E5", linewidth = 0.3),
    #plot.title = element_text(face = "bold", size = 14),
    #plot.subtitle = element_text(color = "#555555"),
    axis.title = element_text(face = "plain"),
    legend.position = "bottom",
    #legend.title = element_text(face = "bold"),
    plot.caption = element_text(color = "#666666", hjust = 0)
  )

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

iamc <- rgdx.param(gdx_file, "IAMC_template") %>%
  mutate(
    Year = as.numeric(as.character(YEMF)),
    IAMC_Template = as.numeric(IAMC_Template)
  )

# Provider transfers
provider_annual <- iamc %>%
  filter(
    VEMF == "Rev_gov_Tax_Car_Tax",
    SCENARIO == scenario_aid,
    REMF %in% providers,
    Year >= start_year,
    Year <= end_year
  ) %>%
  transmute(Year, REMF, Transfer = IAMC_Template) %>%
  group_by(Year, REMF) %>%
  summarise(Transfer = sum(Transfer, na.rm = TRUE), .groups = "drop") %>%
  # GDX is sparse: omitted region-year rows represent exact zero revenue.
  complete(
    Year = seq(start_year, end_year, 5),
    REMF = providers,
    fill = list(Transfer = 0)
  )

total_annual <- provider_annual %>%
  group_by(Year) %>%
  summarise(
    TotalTransfer = sum(Transfer, na.rm = TRUE),
    .groups = "drop"
  )

# Recipient transfers allocated by GDP_PPP share
recipient_annual <- iamc %>%
  filter(
    VEMF == "GDP_PPP",
    SCENARIO == scenario_aid,
    REMF %in% recipients,
    Year >= start_year,
    Year <= end_year
  ) %>%
  group_by(Year) %>%
  mutate(GDPShare = IAMC_Template / sum(IAMC_Template, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(total_annual, by = "Year") %>%
  transmute(Year, REMF, Transfer = TotalTransfer * GDPShare)

# Cumulative transfer
transfer_cumulative <- bind_rows(
  provider_annual %>%
    group_by(REMF) %>%
    summarise(
      CumTransfer = trapz_sum(Year, Transfer),
      .groups = "drop"
    ) %>%
    mutate(Type = "Provider"),
  recipient_annual %>%
    group_by(REMF) %>%
    summarise(
      CumTransfer = trapz_sum(Year, Transfer),
      .groups = "drop"
    ) %>%
    mutate(Type = "Recipient")
)

# Cumulative GDP_MER for the original Aid denominator and the new BaU denominator
gdp_cumulative <- iamc %>%
  filter(
    VEMF == "GDP_MER",
    SCENARIO %in% c(scenario_aid, scenario_bau),
    REMF %in% all_regions,
    Year >= start_year,
    Year <= end_year
  ) %>%
  mutate(GDPBasis = if_else(SCENARIO == scenario_aid, "Aid", "BaU")) %>%
  group_by(GDPBasis, REMF) %>%
  summarise(
    CumGDP = trapz_sum(Year, IAMC_Template),
    .groups = "drop"
  )

transfer_summary <- transfer_cumulative %>%
  left_join(gdp_cumulative, by = "REMF") %>%
  mutate(
    CumTrillion = CumTransfer / 1e6,
    NetTransferPct = if_else(
      Type == "Provider",
      -100 * CumTransfer / (CumGDP * 1000),
       100 * CumTransfer / (CumGDP * 1000)
    )
  )

# Consumption-loss recovery: sign-reversed Aid − Def
loss <- iamc %>%
  filter(
    VEMF == "Pol_Cos_Cns_Los_rat_NPV_5pc",
    SCENARIO %in% c(scenario_def, scenario_aid),
    REMF %in% all_regions,
    Year == end_year
  ) %>%
  mutate(
    Scenario = case_when(
      SCENARIO == scenario_def ~ "Def",
      SCENARIO == scenario_aid ~ "Aid"
    )
  ) %>%
  group_by(REMF, Scenario) %>%
  summarise(
    Value = mean(IAMC_Template, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = Scenario, values_from = Value) %>%
  filter(is.finite(Def), is.finite(Aid)) %>%
  mutate(Recovery = Def - Aid)

scatter_data <- transfer_summary %>%
  select(GDPBasis, REMF, Type, NetTransferPct, CumTrillion) %>%
  left_join(loss %>% select(REMF, Def, Aid, Recovery), by = "REMF") %>%
  mutate(Type = factor(Type, levels = c("Provider", "Recipient"))) %>%
  filter(
    is.finite(NetTransferPct),
    is.finite(Recovery),
    is.finite(CumTrillion)
  )

# Five-year observations -----------------------------------------------------
# Unlike the cumulative figure above, each point below represents one region
# in one model year. Providers transfer their own carbon-tax revenue;
# recipients receive the combined provider revenue allocated by GDP_PPP share.
plot_years <- seq(start_year, end_year, 5)

annual_transfer <- bind_rows(
  provider_annual %>%
    filter(Year %in% plot_years) %>%
    mutate(Type = "Provider"),
  recipient_annual %>%
    filter(Year %in% plot_years) %>%
    mutate(Type = "Recipient")
)

gdp_annual <- iamc %>%
  filter(
    VEMF == "GDP_MER",
    SCENARIO %in% c(scenario_aid, scenario_bau),
    REMF %in% all_regions,
    Year %in% plot_years
  ) %>%
  transmute(
    Year, REMF,
    GDPBasis = if_else(SCENARIO == scenario_aid, "Aid", "BaU"),
    GDP = IAMC_Template
  )

# Direct annual consumption effect of Aid relative to Def. Population is
# common across scenarios within SSP2, so the aggregate CNS percentage change
# is also the per-capita consumption percentage change.
annual_consumption <- iamc %>%
  filter(
    VEMF == "CNS",
    SCENARIO %in% c(scenario_def, scenario_aid),
    REMF %in% all_regions,
    Year %in% plot_years
  ) %>%
  mutate(
    Scenario = case_when(
      SCENARIO == scenario_def ~ "Def",
      SCENARIO == scenario_aid ~ "Aid"
    )
  ) %>%
  group_by(Year, REMF, Scenario) %>%
  summarise(
    Value = mean(IAMC_Template, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = Scenario, values_from = Value) %>%
  filter(is.finite(Def), is.finite(Aid), Def > 0) %>%
  transmute(
    Year, REMF,
    CNS_Def = Def,
    CNS_Aid = Aid,
    ConsumptionChangePct = 100 * (CNS_Aid - CNS_Def) / CNS_Def
  )

annual_scatter_data <- annual_transfer %>%
  left_join(gdp_annual, by = c("Year", "REMF")) %>%
  mutate(
    Type = factor(Type, levels = c("Provider", "Recipient")),
    AnnualTrillion = Transfer / 1e6,
    NetTransferPct = if_else(
      Type == "Provider",
      -100 * Transfer / (GDP * 1000),
       100 * Transfer / (GDP * 1000)
    )
  ) %>%
  left_join(
    annual_consumption,
    by = c("Year", "REMF")
  ) %>%
  filter(
    is.finite(NetTransferPct),
    is.finite(ConsumptionChangePct),
    is.finite(AnnualTrillion)
  )

write.csv(
  annual_scatter_data %>%
    arrange(GDPBasis, Year, Type, REMF) %>%
    select(
      GDPBasis, Year, REMF, Type, Transfer, GDP,
      NetTransferPct, AnnualTrillion, CNS_Def, CNS_Aid,
      ConsumptionChangePct
    ),
  file.path(csv_dir, "transfer_recovery_scatter_5year_source_data.csv"),
  row.names = FALSE
)

# Per-year linear slopes and a joint test for differences among years. The
# interaction model allows both intercepts and slopes to vary by model year.
annual_slope_statistics <- annual_scatter_data %>%
  mutate(Year = factor(Year, levels = plot_years)) %>%
  group_by(GDPBasis, Year) %>%
  group_modify(~ {
    model <- lm(ConsumptionChangePct ~ NetTransferPct, data = .x)
    model_summary <- summary(model)
    coefficients <- model_summary$coefficients
    tibble(
      N = nrow(.x),
      Intercept = unname(coefficients[1, "Estimate"]),
      Slope = unname(coefficients[2, "Estimate"]),
      SlopeSE = unname(coefficients[2, "Std. Error"]),
      SlopePValue = unname(coefficients[2, "Pr(>|t|)"]),
      R2 = unname(model_summary$r.squared)
    )
  }) %>%
  ungroup()

annual_slope_difference_test <- bind_rows(lapply(c("Aid", "BaU"), function(basis) {
  test_data <- annual_scatter_data %>%
    filter(GDPBasis == basis) %>%
    mutate(Year = factor(Year, levels = plot_years))
  common_slope <- lm(
    ConsumptionChangePct ~ NetTransferPct + Year,
    data = test_data
  )
  year_specific_slopes <- lm(
    ConsumptionChangePct ~ NetTransferPct * Year,
    data = test_data
  )
  comparison <- anova(common_slope, year_specific_slopes)
  tibble(
    GDPBasis = basis,
    NumeratorDF = comparison$Df[2],
    DenominatorDF = comparison$Res.Df[2],
    FStatistic = comparison$F[2],
    PValue = comparison$`Pr(>F)`[2]
  )
}))

write.csv(
  annual_slope_statistics,
  file.path(csv_dir, "transfer_recovery_scatter_all_years_slopes.csv"),
  row.names = FALSE
)
write.csv(
  annual_slope_difference_test,
  file.path(csv_dir, "transfer_recovery_scatter_all_years_slope_test.csv"),
  row.names = FALSE
)

make_scatter <- function(gdp_basis) {
  figure <- ggplot(
    filter(scatter_data, GDPBasis == gdp_basis),
    aes(NetTransferPct, Recovery, color = Type, size = CumTrillion)
  ) +
  geom_hline(yintercept = 0, color = "#777777", linewidth = 0.45) +
  geom_vline(xintercept = 0, color = "#777777", linewidth = 0.45) +
  geom_point(alpha = 0.88) +
  scale_color_manual(
    name = NULL,
    values = c(
      Provider = provider_color,
      Recipient = recipient_color
    )
  ) +
  scale_size_continuous(
    name = "Cumulative transfer\n(trillion US$2010)",
    range = c(3, 12),
    labels = label_number(accuracy = 0.01)
  ) +
  scale_x_continuous(
    labels = label_number(accuracy = 0.1, suffix = "%")
  ) +
  scale_y_continuous(
    labels = label_number(accuracy = 0.1, suffix = "%")
  ) +
  labs(
    x = paste0("Net cumulative transfer / cumulative ", gdp_basis,
               " GDP (%)\nProviders (negative)                         Recipients (positive)"),
    y = "Recovery in cumulative consumption loss: Def - Aid (%)\nNegative = worse; positive = better"
  ) +
  theme_paper

  if (requireNamespace("ggrepel", quietly = TRUE)) {
    figure <- figure +
      ggrepel::geom_text_repel(
        aes(label = REMF), size = 3.2, show.legend = FALSE,
        box.padding = 0.35, point.padding = 0.25, max.overlaps = Inf
      )
  } else {
    figure <- figure +
      geom_text(aes(label = REMF), size = 3, nudge_y = 0.03,
                check_overlap = TRUE, show.legend = FALSE)
  }
  figure
}

make_annual_scatter <- function(gdp_basis) {
  figure <- ggplot(
    filter(annual_scatter_data, GDPBasis == gdp_basis),
    aes(NetTransferPct, ConsumptionChangePct, color = Type)
  ) +
    geom_hline(yintercept = 0, color = "#777777", linewidth = 0.4) +
    geom_vline(xintercept = 0, color = "#777777", linewidth = 0.4) +
    geom_point(size = 3.4, alpha = 0.88) +
    facet_wrap(vars(Year), ncol = 3) +
    scale_color_manual(
      name = NULL,
      values = c(
        Provider = provider_color,
        Recipient = recipient_color
      )
    ) +
    scale_x_continuous(
      labels = label_number(accuracy = 0.1, suffix = "%")
    ) +
    scale_y_continuous(
      labels = label_number(accuracy = 0.1)
    ) +
    labs(
      x = paste0(
        "Annual transfer / annual ", gdp_basis,
        " GDP (%)\nProviders (negative)                         Recipients (positive)"
      ),
      y = "Consumption change under Aid relative to Def (%)"
    ) +
    theme_paper +
    theme(
      strip.text = element_text(face = "bold"),
      panel.spacing = grid::unit(5, "mm")
    )

  if (requireNamespace("ggrepel", quietly = TRUE)) {
    figure <- figure +
      ggrepel::geom_text_repel(
        aes(label = REMF), size = 2.6, show.legend = FALSE,
        box.padding = 0.25, point.padding = 0.18,
        min.segment.length = 0, max.overlaps = Inf,
        seed = 260811
      )
  } else {
    figure <- figure +
      geom_text(
        aes(label = REMF), size = 2.5, nudge_y = 0.05,
        check_overlap = TRUE, show.legend = FALSE
      )
  }
  figure
}

make_all_years_scatter <- function(gdp_basis) {
  plot_data <- annual_scatter_data %>%
    filter(GDPBasis == gdp_basis) %>%
    mutate(Year = factor(Year, levels = plot_years))

  # Estimate one line per year. geom_abline clips each infinite fitted line at
  # the common panel boundary, so line length no longer depends on data range.
  regression_lines <- plot_data %>%
    group_by(Year) %>%
    group_modify(~ {
      model <- lm(ConsumptionChangePct ~ NetTransferPct, data = .x)
      tibble(
        Intercept = unname(coef(model)[1]),
        Slope = unname(coef(model)[2])
      )
    }) %>%
    ungroup() %>%
    filter(as.character(Year) != "2030")

  axis_limits <- c(-50, 50)
  axis_breaks <- seq(axis_limits[1], axis_limits[2], by = 10)

  year_colors <- c(
    "2030" = "#4E79A7",
    "2035" = "#59A14F",
    "2040" = "#F28E2B",
    "2045" = "#E15759",
    "2050" = "#B07AA1"
  )

  ggplot(plot_data, aes(NetTransferPct, ConsumptionChangePct)) +
    geom_hline(yintercept = 0, color = "#777777", linewidth = 0.4) +
    geom_vline(xintercept = 0, color = "#777777", linewidth = 0.4) +
    geom_abline(
      intercept = 0, slope = 1,
      color = "black", linetype = "dotted", linewidth = 0.55
    ) +
    geom_abline(
      data = regression_lines,
      aes(
        intercept = Intercept, slope = Slope,
        color = Year
      ),
      linewidth = 0.7, alpha = 0.55,
      show.legend = FALSE,
      inherit.aes = FALSE
    ) +
    geom_point(
      aes(color = Year, shape = Type),
      size = 3.8, alpha = 0.82, stroke = 0.8,
      show.legend = c(color = TRUE, shape = TRUE)
    ) +
    scale_color_manual(
      name = "Year", values = year_colors,
      limits = as.character(plot_years), drop = FALSE
    ) +
    scale_shape_manual(
      name = NULL,
      values = c(Provider = 16, Recipient = 17)
    ) +
    scale_x_continuous(
      breaks = axis_breaks,
      labels = label_number(accuracy = 1, suffix = "%"),
      expand = expansion(mult = 0)
    ) +
    scale_y_continuous(
      breaks = axis_breaks,
      labels = label_number(accuracy = 1, suffix = "%"),
      expand = expansion(mult = 0)
    ) +
    coord_fixed(
      ratio = 1,
      xlim = axis_limits,
      ylim = axis_limits,
      expand = FALSE
    ) +
    labs(
      x = paste0(
        "Annual transfer / annual ", gdp_basis,
        " GDP (%)\nProviders (negative)                         Recipients (positive)"
      ),
      y = "Consumption change under Aid relative to Def (%)"
    ) +
    guides(
      color = guide_legend(
        order = 1,
        override.aes = list(
          size = 3.2,
          alpha = 0.8,
          shape = 16,
          linetype = 0,
          linewidth = 0
        )
      ),
      shape = guide_legend(
        order = 2,
        override.aes = list(size = 3.2, linetype = 0, linewidth = 0)
      )
    ) +
    theme_paper +
    theme(
      axis.title.x = element_text(size = 13.5),
      axis.title.y = element_text(size = 12.5),
      legend.box = "vertical",
      legend.box.just = "center",
      legend.direction = "horizontal"
    )
}

save_scatter <- function(figure, stem, width = 11, height = 8) {
  ggsave(file.path(output_dir, paste0(stem, ".png")), figure,
         width = width, height = height, dpi = 600, bg = "white")
  if (requireNamespace("svglite", quietly = TRUE)) {
    ggsave(file.path(output_dir, paste0(stem, ".svg")), figure,
           device = svglite::svglite,
           width = width, height = height, bg = "white")
  }
}

figure_aid_gdp <- make_scatter("Aid")
figure_bau_gdp <- make_scatter("BaU")
figure_5year_aid_gdp <- make_annual_scatter("Aid")
figure_5year_bau_gdp <- make_annual_scatter("BaU")
figure_all_years_aid_gdp <- make_all_years_scatter("Aid")
figure_all_years_bau_gdp <- make_all_years_scatter("BaU")
if (interactive()) {
  plot(figure_aid_gdp)
  plot(figure_bau_gdp)
  plot(figure_5year_aid_gdp)
  plot(figure_5year_bau_gdp)
  plot(figure_all_years_aid_gdp)
  plot(figure_all_years_bau_gdp)
}

# Keep the original filename for the Aid-GDP version; give the BaU version its own name.
save_scatter(figure_aid_gdp, "transfer_recovery_scatter")
save_scatter(figure_bau_gdp, "transfer_recovery_scatter_BaUGDP")
save_scatter(
  figure_5year_aid_gdp, "transfer_recovery_scatter_5year",
  width = 13, height = 8
)
save_scatter(
  figure_5year_bau_gdp, "transfer_recovery_scatter_5year_BaUGDP",
  width = 13, height = 8
)
save_scatter(
  figure_all_years_aid_gdp, "transfer_recovery_scatter_all_years",
  width = 10, height = 10.5
)
save_scatter(
  figure_all_years_bau_gdp, "transfer_recovery_scatter_all_years_BaUGDP",
  width = 10, height = 10.5
)

# SSP and GDP-per-capita-threshold sensitivity ------------------------------
# The original SSP2/15k figures above remain unchanged. The additional
# figures below repeat the same annual analysis for every feasible SSP x
# threshold Aid scenario. SSP3/10k and SSP3/15k are intentionally excluded.
sensitivity_ssp_levels <- paste0("SSP", 1:3)
sensitivity_threshold_levels <- c("10th", "15th", "20th")

sensitivity_region_membership <- bind_rows(
  tibble(
    Threshold = "10th",
    Region = c("IND", "XSA", "XNF", "XAF"),
    Type = "Recipient"
  ),
  tibble(
    Threshold = "10th",
    Region = c(
      "XE25", "XER", "TUR", "XOC", "CHN", "JPN", "XSE",
      "CAN", "USA", "BRA", "XLM", "CIS", "XME"
    ),
    Type = "Provider"
  ),
  tibble(
    Threshold = "15th",
    Region = c("IND", "BRA", "XSA", "XSE", "XME", "XNF", "XAF"),
    Type = "Recipient"
  ),
  tibble(
    Threshold = "15th",
    Region = c(
      "XE25", "XER", "TUR", "XOC", "CHN", "JPN", "CAN", "USA",
      "XLM", "CIS"
    ),
    Type = "Provider"
  ),
  tibble(
    Threshold = "20th",
    Region = c(
      "CHN", "IND", "BRA", "XSA", "XSE", "XME", "XLM", "XNF",
      "XAF", "CIS"
    ),
    Type = "Recipient"
  ),
  tibble(
    Threshold = "20th",
    Region = c("XE25", "XER", "TUR", "XOC", "JPN", "CAN", "USA"),
    Type = "Provider"
  )
)

sensitivity_membership_check <- sensitivity_region_membership %>%
  count(Threshold, Region) %>%
  filter(n != 1L)
if (nrow(sensitivity_membership_check)) {
  stop("Each region must occur exactly once within every threshold.")
}
if (any((sensitivity_region_membership %>% count(Threshold))$n != 17L)) {
  stop("Each threshold must classify all 17 AIM regions.")
}

sensitivity_scenarios <- crossing(
  SSP = sensitivity_ssp_levels,
  Threshold = sensitivity_threshold_levels
) %>%
  mutate(
    AidScenario = paste0(
      SSP, "_400C_2030CP_", Threshold, "_NoCC_No"
    ),
    DefScenario = paste0(SSP, "_400C_2030CP_NoCC_No"),
    BaUScenario = paste0(SSP, "_BaU_NoCC_No")
  ) %>%
  filter(!(SSP == "SSP3" & Threshold %in% c("10th", "15th")))

sensitivity_scenarios_needed <- unique(c(
  sensitivity_scenarios$AidScenario,
  sensitivity_scenarios$DefScenario,
  sensitivity_scenarios$BaUScenario
))
sensitivity_missing_scenarios <- setdiff(
  sensitivity_scenarios_needed,
  unique(as.character(iamc$SCENARIO))
)
if (length(sensitivity_missing_scenarios)) {
  stop(
    "Required sensitivity scenarios are absent from the GDX: ",
    paste(sensitivity_missing_scenarios, collapse = ", ")
  )
}

sensitivity_region_year_grid <- sensitivity_scenarios %>%
  inner_join(
    sensitivity_region_membership,
    by = "Threshold",
    relationship = "many-to-many"
  ) %>%
  crossing(Year = plot_years)

# Providers contribute their own carbon-tax revenue. Sparse missing values in
# the GDX are exact zeros and are completed explicitly.
sensitivity_revenue_values <- iamc %>%
  filter(
    VEMF == "Rev_gov_Tax_Car_Tax",
    SCENARIO %in% sensitivity_scenarios$AidScenario,
    REMF %in% all_regions,
    Year %in% plot_years
  ) %>%
  transmute(
    AidScenario = as.character(SCENARIO),
    Region = as.character(REMF), Year,
    Transfer = as.numeric(IAMC_Template)
  ) %>%
  group_by(AidScenario, Region, Year) %>%
  summarise(Transfer = sum(Transfer, na.rm = TRUE), .groups = "drop")

sensitivity_provider_annual <- sensitivity_region_year_grid %>%
  filter(Type == "Provider") %>%
  left_join(
    sensitivity_revenue_values,
    by = c("AidScenario", "Region", "Year")
  ) %>%
  mutate(Transfer = coalesce(Transfer, 0)) %>%
  select(SSP, Threshold, Year, Region, Type, Transfer)

sensitivity_total_transfer <- sensitivity_provider_annual %>%
  group_by(SSP, Threshold, Year) %>%
  summarise(TotalTransfer = sum(Transfer), .groups = "drop")

# Recipients split total provider revenue in proportion to their Aid-scenario
# GDP_PPP, matching the allocation used in the original SSP2/15k figure.
sensitivity_gdp_ppp_values <- iamc %>%
  filter(
    VEMF == "GDP_PPP",
    SCENARIO %in% sensitivity_scenarios$AidScenario,
    REMF %in% all_regions,
    Year %in% plot_years
  ) %>%
  transmute(
    AidScenario = as.character(SCENARIO),
    Region = as.character(REMF), Year,
    GDP_PPP = as.numeric(IAMC_Template)
  ) %>%
  group_by(AidScenario, Region, Year) %>%
  summarise(GDP_PPP = mean(GDP_PPP), .groups = "drop")

sensitivity_recipient_annual <- sensitivity_region_year_grid %>%
  filter(Type == "Recipient") %>%
  left_join(
    sensitivity_gdp_ppp_values,
    by = c("AidScenario", "Region", "Year")
  ) %>%
  group_by(SSP, Threshold, Year) %>%
  mutate(GDPShare = GDP_PPP / sum(GDP_PPP)) %>%
  ungroup() %>%
  left_join(
    sensitivity_total_transfer,
    by = c("SSP", "Threshold", "Year")
  ) %>%
  transmute(
    SSP, Threshold, Year, Region, Type,
    Transfer = TotalTransfer * GDPShare
  )

sensitivity_annual_transfer <- bind_rows(
  sensitivity_provider_annual,
  sensitivity_recipient_annual
)

# Annual GDP denominators for both the Aid and BaU versions of the figure.
sensitivity_gdp_scenario_grid <- bind_rows(
  sensitivity_scenarios %>%
    transmute(SSP, Threshold, GDPBasis = "Aid", GDPScenario = AidScenario),
  sensitivity_scenarios %>%
    transmute(SSP, Threshold, GDPBasis = "BaU", GDPScenario = BaUScenario)
) %>%
  inner_join(
    sensitivity_region_membership %>% select(Threshold, Region),
    by = "Threshold",
    relationship = "many-to-many"
  ) %>%
  crossing(Year = plot_years)

sensitivity_gdp_values <- iamc %>%
  filter(
    VEMF == "GDP_MER",
    SCENARIO %in% unique(sensitivity_gdp_scenario_grid$GDPScenario),
    REMF %in% all_regions,
    Year %in% plot_years
  ) %>%
  transmute(
    GDPScenario = as.character(SCENARIO),
    Region = as.character(REMF), Year,
    GDP = as.numeric(IAMC_Template)
  ) %>%
  group_by(GDPScenario, Region, Year) %>%
  summarise(GDP = mean(GDP), .groups = "drop")

sensitivity_annual_gdp <- sensitivity_gdp_scenario_grid %>%
  left_join(
    sensitivity_gdp_values,
    by = c("GDPScenario", "Region", "Year")
  ) %>%
  select(SSP, Threshold, GDPBasis, Year, Region, GDP)

# Aid-induced CNS change relative to the corresponding no-aid Def scenario.
sensitivity_cns_scenario_grid <- bind_rows(
  sensitivity_scenarios %>%
    transmute(SSP, Threshold, ScenarioType = "Def", CNSScenario = DefScenario),
  sensitivity_scenarios %>%
    transmute(SSP, Threshold, ScenarioType = "Aid", CNSScenario = AidScenario)
) %>%
  inner_join(
    sensitivity_region_membership %>% select(Threshold, Region),
    by = "Threshold",
    relationship = "many-to-many"
  ) %>%
  crossing(Year = plot_years)

sensitivity_cns_values <- iamc %>%
  filter(
    VEMF == "CNS",
    SCENARIO %in% unique(sensitivity_cns_scenario_grid$CNSScenario),
    REMF %in% all_regions,
    Year %in% plot_years
  ) %>%
  transmute(
    CNSScenario = as.character(SCENARIO),
    Region = as.character(REMF), Year,
    CNS = as.numeric(IAMC_Template)
  ) %>%
  group_by(CNSScenario, Region, Year) %>%
  summarise(CNS = mean(CNS), .groups = "drop")

sensitivity_annual_consumption <- sensitivity_cns_scenario_grid %>%
  left_join(
    sensitivity_cns_values,
    by = c("CNSScenario", "Region", "Year")
  ) %>%
  select(SSP, Threshold, ScenarioType, Year, Region, CNS) %>%
  pivot_wider(names_from = ScenarioType, values_from = CNS) %>%
  filter(is.finite(Def), is.finite(Aid), Def > 0) %>%
  transmute(
    SSP, Threshold, Year, Region,
    CNS_Def = Def,
    CNS_Aid = Aid,
    ConsumptionChangePct = 100 * (CNS_Aid - CNS_Def) / CNS_Def
  )

sensitivity_scatter_data <- sensitivity_annual_transfer %>%
  left_join(
    sensitivity_annual_gdp,
    by = c("SSP", "Threshold", "Year", "Region")
  ) %>%
  left_join(
    sensitivity_annual_consumption,
    by = c("SSP", "Threshold", "Year", "Region")
  ) %>%
  mutate(
    Type = factor(Type, levels = c("Provider", "Recipient")),
    NetTransferPct = if_else(
      Type == "Provider",
      -100 * Transfer / (GDP * 1000),
       100 * Transfer / (GDP * 1000)
    )
  ) %>%
  filter(
    is.finite(GDP), GDP > 0,
    is.finite(NetTransferPct),
    is.finite(ConsumptionChangePct)
  )

sensitivity_expected_rows <-
  nrow(sensitivity_scenarios) * 17L * length(plot_years) * 2L
if (nrow(sensitivity_scatter_data) != sensitivity_expected_rows) {
  stop(
    "Expected ", sensitivity_expected_rows,
    " SSP-threshold scatter rows; found ",
    nrow(sensitivity_scatter_data), "."
  )
}

write.csv(
  sensitivity_scatter_data %>%
    arrange(GDPBasis, SSP, Threshold, Year, Type, Region) %>%
    select(
      GDPBasis, SSP, Threshold, Year, Region, Type,
      Transfer, GDP, NetTransferPct,
      CNS_Def, CNS_Aid, ConsumptionChangePct
    ),
  file.path(
    csv_dir,
    "transfer_consumption_change_SSP_threshold_facets_source_data.csv"
  ),
  row.names = FALSE
)

sensitivity_slope_statistics <- sensitivity_scatter_data %>%
  group_by(GDPBasis, SSP, Threshold, Year) %>%
  group_modify(~ {
    model <- lm(ConsumptionChangePct ~ NetTransferPct, data = .x)
    model_summary <- summary(model)
    coefficients <- model_summary$coefficients
    tibble(
      N = nrow(.x),
      Intercept = unname(coefficients[1, "Estimate"]),
      Slope = unname(coefficients[2, "Estimate"]),
      SlopeSE = unname(coefficients[2, "Std. Error"]),
      SlopePValue = unname(coefficients[2, "Pr(>|t|)"]),
      R2 = unname(model_summary$r.squared)
    )
  }) %>%
  ungroup()

write.csv(
  sensitivity_slope_statistics,
  file.path(
    csv_dir,
    "transfer_consumption_change_SSP_threshold_facets_slopes.csv"
  ),
  row.names = FALSE
)

make_sensitivity_facet_scatter <- function(gdp_basis) {
  plot_data <- sensitivity_scatter_data %>%
    filter(GDPBasis == gdp_basis) %>%
    mutate(
      SSP = factor(SSP, levels = sensitivity_ssp_levels),
      Threshold = factor(Threshold, levels = sensitivity_threshold_levels),
      Year = factor(Year, levels = plot_years)
    )

  regression_lines <- plot_data %>%
    group_by(SSP, Threshold, Year) %>%
    group_modify(~ {
      model <- lm(ConsumptionChangePct ~ NetTransferPct, data = .x)
      tibble(
        Intercept = unname(coef(model)[1]),
        Slope = unname(coef(model)[2])
      )
    }) %>%
    ungroup() %>%
    filter(as.character(Year) != "2030")

  feasible_facets <- plot_data %>% distinct(SSP, Threshold)
  infeasible_facets <- tibble(
    SSP = factor(rep("SSP3", 2), levels = sensitivity_ssp_levels),
    Threshold = factor(
      c("10th", "15th"), levels = sensitivity_threshold_levels
    ),
    LabelX = 0,
    LabelY = 0,
    Label = "Infeasible"
  )

  axis_limits <- c(-50, 50)
  axis_breaks <- seq(axis_limits[1], axis_limits[2], by = 10)
  year_colors <- c(
    "2030" = "#4E79A7",
    "2035" = "#59A14F",
    "2040" = "#F28E2B",
    "2045" = "#E15759",
    "2050" = "#B07AA1"
  )

  ggplot(plot_data, aes(NetTransferPct, ConsumptionChangePct)) +
    geom_hline(
      data = feasible_facets,
      aes(yintercept = 0),
      color = "#777777", linewidth = 0.35,
      inherit.aes = FALSE
    ) +
    geom_vline(
      data = feasible_facets,
      aes(xintercept = 0),
      color = "#777777", linewidth = 0.35,
      inherit.aes = FALSE
    ) +
    geom_abline(
      data = feasible_facets,
      aes(intercept = 0, slope = 1),
      color = "black", linetype = "dotted", linewidth = 0.45,
      inherit.aes = FALSE
    ) +
    geom_abline(
      data = regression_lines,
      aes(intercept = Intercept, slope = Slope, color = Year),
      linewidth = 0.55, alpha = 0.55,
      show.legend = FALSE,
      inherit.aes = FALSE
    ) +
    geom_point(
      aes(color = Year, shape = Type),
      size = 2.5, alpha = 0.82, stroke = 0.7,
      show.legend = c(color = TRUE, shape = TRUE)
    ) +
    geom_text(
      data = infeasible_facets,
      aes(x = LabelX, y = LabelY, label = Label),
      color = "#777777", size = 4,
      inherit.aes = FALSE
    ) +
    facet_grid(
      rows = vars(SSP),
      cols = vars(Threshold),
      drop = FALSE,
      labeller = labeller(
        Threshold = c(
          "10th" = "10k USD",
          "15th" = "15k USD",
          "20th" = "20k USD"
        )
      )
    ) +
    scale_color_manual(
      name = "Year", values = year_colors,
      limits = as.character(plot_years), drop = FALSE
    ) +
    scale_shape_manual(
      name = NULL,
      values = c(Provider = 16, Recipient = 17)
    ) +
    scale_x_continuous(
      breaks = axis_breaks,
      labels = label_number(accuracy = 1, suffix = "%"),
      expand = expansion(mult = 0)
    ) +
    scale_y_continuous(
      breaks = axis_breaks,
      labels = label_number(accuracy = 1, suffix = "%"),
      expand = expansion(mult = 0)
    ) +
    coord_fixed(
      ratio = 1,
      xlim = axis_limits,
      ylim = axis_limits,
      expand = FALSE
    ) +
    labs(
      x = paste0(
        "Annual transfer / annual ", gdp_basis,
        " GDP (%)\nProviders (negative)                    Recipients (positive)"
      ),
      y = "Consumption change under Aid relative to Def (%)"
    ) +
    guides(
      color = guide_legend(
        order = 1,
        override.aes = list(
          size = 3,
          alpha = 0.8,
          shape = 16,
          linetype = 0,
          linewidth = 0
        )
      ),
      shape = guide_legend(
        order = 2,
        override.aes = list(size = 3, linetype = 0, linewidth = 0)
      )
    ) +
    theme_paper +
    theme(
      axis.title = element_text(size = 13),
      axis.text = element_text(size = 8.5),
      strip.text = element_text(face = "bold", size = 11),
      panel.spacing = grid::unit(4, "mm"),
      legend.box = "vertical",
      legend.box.just = "center",
      legend.direction = "horizontal"
    )
}

figure_sensitivity_aid_gdp <- make_sensitivity_facet_scatter("Aid")
figure_sensitivity_bau_gdp <- make_sensitivity_facet_scatter("BaU")

if (interactive()) {
  plot(figure_sensitivity_aid_gdp)
  plot(figure_sensitivity_bau_gdp)
}

save_scatter(
  figure_sensitivity_aid_gdp,
  "transfer_consumption_change_SSP_threshold_facets",
  width = 15, height = 15.5
)
save_scatter(
  figure_sensitivity_bau_gdp,
  "transfer_consumption_change_SSP_threshold_facets_BaUGDP",
  width = 15, height = 15.5
)
