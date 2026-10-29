# Cumulative transfer and the recipient zero-loss funding requirement ---------
#
# This script builds a paper-ready figure that compares two estimates of the
# total funding needed to eliminate the recipient-region consumption loss:
#
#   1. CGE-implied funding: the x-intercept of the straight line through the
#      recipient Def and Aid outcomes.
#   2. Accounting benchmark: the undiscounted 2030-2050 integral of the
#      recipient's annual Def Pol_Cos_Cns_Los.
#
# The horizontal quantities are undiscounted and expressed in trillion 2010
# USD. Aid uses the model-reported 2050 Cum_Rev_gov_Tax_Car_Tax directly. The
# accounting benchmark uses trapezoidal integration because no corresponding
# model-reported undiscounted cumulative loss is available. The vertical
# outcome is the model-reported cumulative consumption-loss rate at 2050 (5%
# NPV). The top axis is the cumulative amount divided by the 20-year interval
# (2030-2050).

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
if (is.na(project_root)) {
  stop("Could not locate data/AR6database/global_17_IAMC.gdx.")
}

local_library <- file.path(project_root, "Rlib")
if (dir.exists(local_library)) .libPaths(c(local_library, .libPaths()))
shared_libraries <- c(Sys.getenv("R_LIBS_USER"), "C:/ENVI5809/R_library")
shared_libraries <- shared_libraries[
  nzchar(shared_libraries) & dir.exists(shared_libraries)
]
.libPaths(unique(c(shared_libraries, .libPaths())))

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(scales)
})

scenario_def <- "SSP2_400C_2030CP_NoCC_No"
scenario_aid <- "SSP2_400C_2030CP_15th_NoCC_No"
provider_region <- "Rprovider15th"
recipient_region <- "Rrecipient15th"
sensitivity_region_membership <- bind_rows(
  tibble::tibble(
    Threshold = "10th",
    Region = c("IND", "XSA", "XNF", "XAF"),
    Group = "Recipient"
  ),
  tibble::tibble(
    Threshold = "10th",
    Region = c(
      "XE25", "XER", "TUR", "XOC", "CHN", "JPN", "XSE",
      "CAN", "USA", "BRA", "XLM", "CIS", "XME"
    ),
    Group = "Provider"
  ),
  tibble::tibble(
    Threshold = "15th",
    Region = c("IND", "BRA", "XSA", "XSE", "XME", "XNF", "XAF"),
    Group = "Recipient"
  ),
  tibble::tibble(
    Threshold = "15th",
    Region = c(
      "XE25", "XER", "TUR", "XOC", "CHN", "JPN", "CAN", "USA",
      "XLM", "CIS"
    ),
    Group = "Provider"
  ),
  tibble::tibble(
    Threshold = "20th",
    Region = c(
      "CHN", "IND", "BRA", "XSA", "XSE", "XME", "XLM", "XNF",
      "XAF", "CIS"
    ),
    Group = "Recipient"
  ),
  tibble::tibble(
    Threshold = "20th",
    Region = c("XE25", "XER", "TUR", "XOC", "JPN", "CAN", "USA"),
    Group = "Provider"
  )
)

membership_check <- sensitivity_region_membership %>%
  count(Threshold, Region) %>%
  filter(n != 1L)
if (nrow(membership_check) ||
    any((sensitivity_region_membership %>% count(Threshold))$n != 17L)) {
  stop("Each GDP threshold must classify all 17 AIM regions exactly once.")
}

sensitivity_cases <- expand.grid(
  SSP = paste0("SSP", 1:3),
  Threshold = c("10th", "15th", "20th"),
  stringsAsFactors = FALSE
) %>%
  mutate(
    DefScenario = paste0(SSP, "_400C_2030CP_NoCC_No"),
    AidScenario = paste0(
      SSP, "_400C_2030CP_", Threshold, "_NoCC_No"
    )
  ) %>%
  filter(!(SSP == "SSP3" & Threshold %in% c("10th", "15th")))
start_year <- 2030
end_year <- 2050
model_years <- seq(start_year, end_year, by = 5)
period_years <- end_year - start_year

loss_rate_variable <- "Pol_Cos_Cns_Los_rat_NPV_5pc"
loss_amount_variable <- "Pol_Cos_Cns_Los"
revenue_variable <- "Rev_gov_Tax_Car_Tax"
cumulative_revenue_variable <- "Cum_Rev_gov_Tax_Car_Tax"
variables_needed <- c(
  loss_rate_variable,
  loss_amount_variable,
  revenue_variable,
  cumulative_revenue_variable
)
scenarios_needed <- unique(c(
  scenario_def,
  scenario_aid,
  sensitivity_cases$DefScenario,
  sensitivity_cases$AidScenario
))

gdx_file <- file.path(project_root, "data", "AR6database", "global_17_IAMC.gdx")
output_dir <- file.path(project_root, "output", "Figure")
csv_dir <- file.path(output_dir, "csv")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)

# gdxdump is used here instead of loading the complete 128-MB GDX parameter
# into R. The temporary CSV is scanned in chunks and only the records needed
# for this figure are retained.
find_gdxdump <- function() {
  from_path <- unname(Sys.which(c("gdxdump", "gdxdump.exe")))
  from_path <- from_path[nzchar(from_path)]
  candidates <- unique(c(
    from_path,
    "C:/GAMS/34/gdxdump.exe",
    "C:/GAMS/win64/26.1/gdxdump.exe"
  ))
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) return(NA_character_)
  normalizePath(hit[[1]], winslash = "/")
}

read_iamc_subset <- function(path, scenarios, variables) {
  gdxdump <- find_gdxdump()
  if (is.na(gdxdump)) stop("Could not locate gdxdump.exe.")

  dump_file <- tempfile("IAMC_template_", fileext = ".csv")
  on.exit(unlink(dump_file), add = TRUE)

  result <- system2(
    gdxdump,
    args = c(
      shQuote(normalizePath(path, winslash = "/")),
      "symb=IAMC_template",
      "format=csv",
      paste0("output=", shQuote(dump_file))
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(result, "status")
  if (is.null(status)) status <- 0L
  if (status != 0L || !file.exists(dump_file)) {
    stop("gdxdump failed: ", paste(result, collapse = "\n"))
  }

  con <- file(dump_file, open = "r", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  invisible(readLines(con, n = 1L, warn = FALSE))

  pieces <- list()
  piece_id <- 0L
  repeat {
    lines <- readLines(con, n = 200000L, warn = FALSE)
    if (!length(lines)) break

    scenario_hit <- rep(FALSE, length(lines))
    for (scenario in scenarios) {
      scenario_hit <- scenario_hit |
        startsWith(lines, paste0('"', scenario, '",'))
    }

    variable_hit <- rep(FALSE, length(lines))
    for (variable in variables) {
      variable_hit <- variable_hit |
        grepl(paste0(',"', variable, '",'), lines, fixed = TRUE)
    }

    selected <- lines[scenario_hit & variable_hit]
    if (length(selected)) {
      piece_id <- piece_id + 1L
      pieces[[piece_id]] <- read.csv(
        text = paste(selected, collapse = "\n"),
        header = FALSE,
        col.names = c("SCENARIO", "REMF", "VEMF", "Year", "Value"),
        stringsAsFactors = FALSE
      )
    }
  }

  if (!length(pieces)) stop("No required IAMC_template records were found.")
  bind_rows(pieces) %>%
    mutate(
      Year = as.numeric(Year),
      Value = as.numeric(Value)
    )
}

message("Reading selected IAMC_template records from: ", gdx_file)
iamc <- read_iamc_subset(gdx_file, scenarios_needed, variables_needed) %>%
  filter(
    SCENARIO %in% scenarios_needed,
    VEMF %in% variables_needed,
    is.finite(Year),
    is.finite(Value)
  )

one_value <- function(variable, scenario, region, year) {
  values <- iamc$Value[
    iamc$VEMF == variable &
      iamc$SCENARIO == scenario &
      iamc$REMF == region &
      iamc$Year == year
  ]
  if (length(values) != 1L || !is.finite(values)) {
    stop(
      "Expected one finite value for ", variable, ", ", scenario,
      ", ", region, ", ", year, "; found ", length(values), "."
    )
  }
  values
}

five_year_series <- function(variable, scenario, region) {
  rows <- iamc %>%
    filter(
      VEMF == variable,
      SCENARIO == scenario,
      REMF == region,
      Year %in% model_years
    ) %>%
    select(Year, Value) %>%
    arrange(Year)

  if (nrow(rows) != length(model_years) ||
      anyDuplicated(rows$Year) ||
      !identical(rows$Year, as.numeric(model_years)) ||
      any(!is.finite(rows$Value))) {
    stop(
      "Expected one finite ", variable, " observation for every model year ",
      start_year, "-", end_year, " in ", scenario, ", ", region, "."
    )
  }
  rows
}

trapezoid_integral <- function(year, annual_flow) {
  if (length(year) < 2L || length(year) != length(annual_flow)) {
    stop("A valid annual-flow series is required for integration.")
  }
  sum(diff(year) * (head(annual_flow, -1L) + tail(annual_flow, -1L)) / 2)
}

# Horizontal amounts ----------------------------------------------------------
# Cum_Rev_gov_Tax_Car_Tax is the model-reported undiscounted cumulative
# revenue. Its 2050 value is used directly for Aid. The annual revenue series
# is retained only for source-data inspection. The Def consumption loss still
# requires trapezoidal integration because no matching undiscounted cumulative
# loss variable is available in IAMC_template.
revenue_annual <- five_year_series(
  revenue_variable, scenario_aid, provider_region
) %>%
  mutate(Flow = "Provider carbon-tax revenue", Unit = "million USD2010/yr")

revenue_cumulative <- five_year_series(
  cumulative_revenue_variable, scenario_aid, provider_region
) %>%
  mutate(
    Flow = "Provider cumulative carbon-tax revenue",
    Unit = "million USD2010"
  )

loss_amount_annual <- five_year_series(
  loss_amount_variable, scenario_def, recipient_region
) %>%
  mutate(Flow = "Recipient Def consumption loss", Unit = "billion USD2010/yr")

actual_cumulative_aid <- one_value(
  cumulative_revenue_variable, scenario_aid, provider_region, end_year
) / 1e6

accounting_zero_loss_funding <- trapezoid_integral(
  loss_amount_annual$Year, loss_amount_annual$Value
) / 1000

if (!is.finite(actual_cumulative_aid) || actual_cumulative_aid <= 0) {
  stop("The cumulative provider transfer must be positive.")
}
if (!is.finite(accounting_zero_loss_funding) ||
    accounting_zero_loss_funding <= 0) {
  stop("The cumulative Def recipient consumption loss must be positive.")
}

# Vertical outcomes -----------------------------------------------------------
plot_data <- tibble::tribble(
  ~Group,      ~Scenario, ~Region,            ~ScenarioCode,
  "Provider",  "Def",     provider_region,    scenario_def,
  "Provider",  "Aid",     provider_region,    scenario_aid,
  "Recipient", "Def",     recipient_region,   scenario_def,
  "Recipient", "Aid",     recipient_region,   scenario_aid
) %>%
  rowwise() %>%
  mutate(
    CumulativeAid_trillion_USD2010 = if_else(
      Scenario == "Def", 0, actual_cumulative_aid
    ),
    AverageAnnualAid_trillion_USD2010_per_year =
      CumulativeAid_trillion_USD2010 / period_years,
    CumulativeConsumptionLossRate_pct = one_value(
      loss_rate_variable, ScenarioCode, Region, end_year
    )
  ) %>%
  ungroup() %>%
  mutate(
    Group = factor(Group, levels = c("Provider", "Recipient")),
    Scenario = factor(Scenario, levels = c("Def", "Aid"))
  )

recipient_points <- plot_data %>%
  filter(Group == "Recipient") %>%
  arrange(Scenario)
recipient_def_loss_rate <- recipient_points$CumulativeConsumptionLossRate_pct[
  recipient_points$Scenario == "Def"
]
recipient_aid_loss_rate <- recipient_points$CumulativeConsumptionLossRate_pct[
  recipient_points$Scenario == "Aid"
]

denominator <- recipient_def_loss_rate - recipient_aid_loss_rate
if (!is.finite(denominator) || denominator <= 0) {
  stop("The recipient Def-Aid line does not imply a positive zero-loss crossing.")
}
cge_zero_loss_funding <-
  actual_cumulative_aid * recipient_def_loss_rate / denominator

if (cge_zero_loss_funding < 0 ||
    cge_zero_loss_funding > actual_cumulative_aid) {
  stop("The CGE-implied zero-loss crossing is outside the Def-Aid interval.")
}

funding_gap <- accounting_zero_loss_funding - cge_zero_loss_funding
funding_ratio <- accounting_zero_loss_funding / cge_zero_loss_funding

benchmarks <- tibble::tibble(
  Metric = c(
    "Actual cumulative Aid transfer",
    "CGE-implied recipient zero-loss funding",
    "Def-loss accounting benchmark",
    "Accounting benchmark minus CGE",
    "Accounting benchmark / CGE"
  ),
  Value = c(
    actual_cumulative_aid,
    cge_zero_loss_funding,
    accounting_zero_loss_funding,
    funding_gap,
    funding_ratio
  ),
  Unit = c(
    rep("trillion USD2010, undiscounted", 4),
    "ratio"
  )
)

# Publication figure ----------------------------------------------------------
provider_colour <- "#3F6C8E"
recipient_colour <- "#B6573F"
ink <- "#222222"

x_max <- max(
  actual_cumulative_aid,
  cge_zero_loss_funding,
  accounting_zero_loss_funding
) * 1.12
y_values <- plot_data$CumulativeConsumptionLossRate_pct
y_span <- diff(range(c(y_values, 0)))
if (!is.finite(y_span) || y_span == 0) y_span <- 1
y_min <- min(y_values, 0) - 0.13 * y_span
y_max <- max(y_values, 0) + 0.18 * y_span

benchmark_points <- tibble::tibble(
  Benchmark = c("CGE-implied", "Def-loss benchmark"),
  X = c(cge_zero_loss_funding, accounting_zero_loss_funding),
  Y = 0,
  Label = c(
    number(cge_zero_loss_funding, accuracy = 0.1),
    number(accounting_zero_loss_funding, accuracy = 0.1)
  ),
  LabelY = c(0.075 * y_span, -0.10 * y_span)
)

x_breaks <- pretty(c(0, x_max), n = 6)
x_breaks <- x_breaks[x_breaks >= 0 & x_breaks <= x_max]

figure <- ggplot(
  plot_data,
  aes(
    x = CumulativeAid_trillion_USD2010,
    y = CumulativeConsumptionLossRate_pct,
    color = Group,
    group = Group
  )
) +
  geom_hline(yintercept = 0, color = ink, linewidth = 0.55) +
  geom_vline(xintercept = 0, color = ink, linewidth = 0.55) +
  geom_line(linetype = "dotted", linewidth = 1.0) +
  geom_point(
    aes(shape = Scenario, fill = interaction(Group, Scenario)),
    size = 4.2,
    stroke = 1.05
  ) +
  geom_segment(
    data = tibble::tibble(
      x = min(accounting_zero_loss_funding, cge_zero_loss_funding),
      xend = max(accounting_zero_loss_funding, cge_zero_loss_funding),
      y = 0,
      yend = 0
    ),
    aes(x = x, xend = xend, y = y, yend = yend),
    inherit.aes = FALSE,
    color = "#999999",
    linewidth = 3.2,
    alpha = 0.35,
    lineend = "round"
  ) +
  annotate(
    "label",
    x = mean(c(cge_zero_loss_funding, accounting_zero_loss_funding)),
    y = -0.035 * y_span,
    label = number(funding_gap, accuracy = 0.1),
    size = 3.0,
    linewidth = 0,
    fill = alpha("white", 0.90),
    color = "#666666"
  ) +
  geom_point(
    data = benchmark_points,
    aes(x = X, y = Y, shape = Benchmark),
    inherit.aes = FALSE,
    size = 4.0,
    stroke = 1.0,
    color = ink,
    fill = "white"
  ) +
  geom_label(
    data = benchmark_points,
    aes(x = X, y = LabelY, label = Label),
    inherit.aes = FALSE,
    size = 3.25,
    lineheight = 0.95,
    linewidth = 0,
    fill = alpha("white", 0.90),
    color = ink
  ) +
  scale_color_manual(
    values = c(Provider = provider_colour, Recipient = recipient_colour),
    name = NULL
  ) +
  scale_fill_manual(
    values = c(
      "Provider.Def" = "white",
      "Provider.Aid" = provider_colour,
      "Recipient.Def" = "white",
      "Recipient.Aid" = recipient_colour
    ),
    guide = "none"
  ) +
  scale_shape_manual(
    values = c(
      Def = 1,
      Aid = 16,
      `CGE-implied` = 23,
      `Def-loss benchmark` = 24
    ),
    breaks = c("Def", "Aid", "CGE-implied", "Def-loss benchmark"),
    labels = c(
      "Def", "Aid", "CGE-implied zero loss", "Def-loss benchmark"
    ),
    name = NULL
  ) +
  scale_x_continuous(
    breaks = x_breaks,
    labels = label_number(accuracy = 1),
    limits = c(0, x_max),
    expand = expansion(mult = c(0, 0)),
    sec.axis = sec_axis(
      ~ . / period_years,
      breaks = x_breaks / period_years,
      labels = label_number(accuracy = 0.1),
      name = "Average annual amount, 2030-2050 (trillion US$2010/yr)"
    )
  ) +
  scale_y_continuous(
    labels = label_number(accuracy = 1, suffix = "%"),
    limits = c(y_min, y_max),
    expand = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Cumulative transfer from 2030 to 2050 (trillion US$2010)",
    y = "Cumulative consumption loss rate in 2050"
  ) +
  guides(
    color = guide_legend(order = 1, override.aes = list(linetype = 0, size = 3)),
    shape = guide_legend(order = 2, override.aes = list(color = ink))
  ) +
  theme_classic(base_size = 11, base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.line = element_line(color = ink, linewidth = 0.55),
    axis.ticks = element_line(color = ink, linewidth = 0.45),
    axis.title = element_text(color = ink, size = 10.5),
    axis.text = element_text(color = ink, size = 9.5),
    legend.position = "inside",
    legend.position.inside = c(0.80, 0.76),
    legend.direction = "vertical",
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_blank(),
    plot.margin = margin(10, 12, 10, 10)
  )

png_file <- file.path(output_dir, "Cumulative_transfer_zero_loss_2050.png")
svg_file <- file.path(output_dir, "Cumulative_transfer_zero_loss_2050.svg")
source_csv <- file.path(
  csv_dir, "Cumulative_transfer_zero_loss_2050_source_data.csv"
)
benchmark_csv <- file.path(
  csv_dir, "Cumulative_transfer_zero_loss_2050_benchmarks.csv"
)
annual_csv <- file.path(
  csv_dir, "Cumulative_transfer_zero_loss_2050_annual_flows.csv"
)

ggsave(png_file, figure, width = 7.4, height = 6.2, dpi = 600, bg = "white")
if (requireNamespace("svglite", quietly = TRUE)) {
  ggsave(svg_file, figure, width = 7.4, height = 6.2,
         device = svglite::svglite, bg = "white")
}

write.csv(plot_data, source_csv, row.names = FALSE, na = "")
write.csv(benchmarks, benchmark_csv, row.names = FALSE, na = "")
write.csv(
  bind_rows(
    revenue_annual %>%
      mutate(
        ConvertedValue = Value / 1e6,
        ConvertedUnit = "trillion USD2010/yr"
      ),
    revenue_cumulative %>%
      mutate(
        ConvertedValue = Value / 1e6,
        ConvertedUnit = "trillion USD2010"
      ),
    loss_amount_annual %>%
      mutate(
        ConvertedValue = Value / 1000,
        ConvertedUnit = "trillion USD2010/yr"
      )
  ) %>%
    select(Flow, Year, Value, Unit, ConvertedValue, ConvertedUnit),
  annual_csv,
  row.names = FALSE,
  na = ""
)

# SSP and GDP-threshold sensitivity ------------------------------------------
calculate_sensitivity_case <- function(case_row) {
  ssp <- case_row$SSP[[1]]
  threshold <- case_row$Threshold[[1]]
  def_scenario <- case_row$DefScenario[[1]]
  aid_scenario <- case_row$AidScenario[[1]]
  provider_aggregate <- paste0("Rprovider", threshold)
  recipient_aggregate <- paste0("Rrecipient", threshold)
  revenue <- five_year_series(
    revenue_variable, aid_scenario, provider_aggregate
  )
  cumulative_revenue <- five_year_series(
    cumulative_revenue_variable, aid_scenario, provider_aggregate
  )

  loss_amount <- five_year_series(
    loss_amount_variable, def_scenario, recipient_aggregate
  )
  cumulative_aid <- one_value(
    cumulative_revenue_variable, aid_scenario, provider_aggregate, end_year
  ) / 1e6
  accounting_funding <- trapezoid_integral(
    loss_amount$Year, loss_amount$Value
  ) / 1000

  points <- tibble::tribble(
    ~Group,      ~Scenario, ~Region,              ~ScenarioCode,
    "Provider",  "Def",     provider_aggregate,   def_scenario,
    "Provider",  "Aid",     provider_aggregate,   aid_scenario,
    "Recipient", "Def",     recipient_aggregate,  def_scenario,
    "Recipient", "Aid",     recipient_aggregate,  aid_scenario
  ) %>%
    rowwise() %>%
    mutate(
      SSP = ssp,
      Threshold = threshold,
      CumulativeAid_trillion_USD2010 = if_else(
        Scenario == "Def", 0, cumulative_aid
      ),
      AverageAnnualAid_trillion_USD2010_per_year =
        CumulativeAid_trillion_USD2010 / period_years,
      CumulativeConsumptionLossRate_pct = one_value(
        loss_rate_variable, ScenarioCode, Region, end_year
      )
    ) %>%
    ungroup()

  recipient_def <- points$CumulativeConsumptionLossRate_pct[
    points$Group == "Recipient" & points$Scenario == "Def"
  ]
  recipient_aid <- points$CumulativeConsumptionLossRate_pct[
    points$Group == "Recipient" & points$Scenario == "Aid"
  ]
  response <- recipient_def - recipient_aid
  if (length(response) != 1L || !is.finite(response) || response <= 0) {
    stop("No valid recipient zero-loss crossing for ", ssp, " / ", threshold, ".")
  }
  cge_funding <- cumulative_aid * recipient_def / response
  if (!is.finite(cge_funding) || cge_funding < 0) {
    stop("No non-negative CGE zero-loss crossing for ",
         ssp, " / ", threshold, ".")
  }

  benchmark_points <- tibble::tibble(
    SSP = ssp,
    Threshold = threshold,
    Benchmark = c("CGE-implied", "Def-loss benchmark"),
    X = c(cge_funding, accounting_funding),
    Y = 0,
    Label = number(c(cge_funding, accounting_funding), accuracy = 0.1)
  )

  metrics <- tibble::tibble(
    SSP = ssp,
    Threshold = threshold,
    ActualAid_trillion_USD2010 = cumulative_aid,
    CGE_zero_loss_trillion_USD2010 = cge_funding,
    Def_loss_benchmark_trillion_USD2010 = accounting_funding,
    Benchmark_minus_CGE_trillion_USD2010 = accounting_funding - cge_funding,
    Benchmark_over_CGE = accounting_funding / cge_funding,
    RecipientDefLossRate_pct = recipient_def,
    RecipientAidLossRate_pct = recipient_aid
  )

  annual <- bind_rows(
    revenue %>%
      mutate(
        Flow = "Provider carbon-tax revenue",
        RawUnit = "million USD2010/yr",
        ConvertedValue = Value / 1e6,
        ConvertedUnit = "trillion USD2010/yr"
      ),
    cumulative_revenue %>%
      mutate(
        Flow = "Provider cumulative carbon-tax revenue",
        RawUnit = "million USD2010",
        ConvertedValue = Value / 1e6,
        ConvertedUnit = "trillion USD2010"
      ),
    loss_amount %>%
      mutate(
        Flow = "Recipient Def consumption loss",
        RawUnit = "billion USD2010/yr",
        ConvertedValue = Value / 1000,
        ConvertedUnit = "trillion USD2010/yr"
      )
  ) %>%
    mutate(SSP = ssp, Threshold = threshold) %>%
    select(
      SSP, Threshold, Flow, Year, Value, RawUnit,
      ConvertedValue, ConvertedUnit
    )

  list(points = points, benchmarks = benchmark_points,
       metrics = metrics, annual = annual)
}

sensitivity_results <- lapply(
  seq_len(nrow(sensitivity_cases)),
  function(i) calculate_sensitivity_case(sensitivity_cases[i, , drop = FALSE])
)
sensitivity_points <- bind_rows(lapply(sensitivity_results, `[[`, "points"))
sensitivity_benchmark_points <- bind_rows(
  lapply(sensitivity_results, `[[`, "benchmarks")
)
sensitivity_metrics <- bind_rows(lapply(sensitivity_results, `[[`, "metrics"))
sensitivity_annual <- bind_rows(lapply(sensitivity_results, `[[`, "annual"))
sensitivity_extrapolation <- sensitivity_metrics %>%
  filter(CGE_zero_loss_trillion_USD2010 > ActualAid_trillion_USD2010) %>%
  transmute(
    SSP,
    Threshold,
    X = ActualAid_trillion_USD2010,
    Y = RecipientAidLossRate_pct,
    Xend = CGE_zero_loss_trillion_USD2010,
    Yend = 0
  )

expected_cases <- nrow(sensitivity_cases)
if (nrow(sensitivity_points) != expected_cases * 4L ||
    nrow(sensitivity_benchmark_points) != expected_cases * 2L ||
    any(sensitivity_points$SSP == "SSP3" &
        sensitivity_points$Threshold %in% c("10th", "15th"))) {
  stop("The feasible SSP-by-threshold sensitivity grid is inconsistent.")
}

sensitivity_points <- sensitivity_points %>%
  mutate(
    SSP = factor(SSP, levels = paste0("SSP", 1:3)),
    Threshold = factor(Threshold, levels = c("10th", "15th", "20th")),
    Group = factor(Group, levels = c("Provider", "Recipient")),
    Scenario = factor(Scenario, levels = c("Def", "Aid"))
  )
sensitivity_benchmark_points <- sensitivity_benchmark_points %>%
  mutate(
    SSP = factor(SSP, levels = paste0("SSP", 1:3)),
    Threshold = factor(Threshold, levels = c("10th", "15th", "20th"))
  )

sensitivity_y_span <- diff(range(c(
  sensitivity_points$CumulativeConsumptionLossRate_pct, 0
)))
if (!is.finite(sensitivity_y_span) || sensitivity_y_span == 0) {
  sensitivity_y_span <- 1
}
sensitivity_benchmark_points <- sensitivity_benchmark_points %>%
  mutate(
    LabelY = if_else(
      Benchmark == "CGE-implied",
      0.045 * sensitivity_y_span,
      -0.045 * sensitivity_y_span
    )
  )

sensitivity_x_max <- max(
  sensitivity_points$CumulativeAid_trillion_USD2010,
  sensitivity_benchmark_points$X
) * 1.10
sensitivity_x_breaks <- pretty(c(0, sensitivity_x_max), n = 5)
sensitivity_x_breaks <- sensitivity_x_breaks[
  sensitivity_x_breaks >= 0 & sensitivity_x_breaks <= sensitivity_x_max
]
sensitivity_y_min <- min(
  sensitivity_points$CumulativeConsumptionLossRate_pct,
  sensitivity_benchmark_points$LabelY,
  0
) - 0.08 * sensitivity_y_span
sensitivity_y_max <- max(
  sensitivity_points$CumulativeConsumptionLossRate_pct,
  sensitivity_benchmark_points$LabelY,
  0
) + 0.10 * sensitivity_y_span

threshold_labels <- c(
  `10th` = "10k USD threshold",
  `15th` = "15k USD threshold",
  `20th` = "20k USD threshold"
)

sensitivity_figure <- ggplot(
  sensitivity_points,
  aes(
    x = CumulativeAid_trillion_USD2010,
    y = CumulativeConsumptionLossRate_pct,
    color = Group,
    group = Group
  )
) +
  geom_hline(yintercept = 0, color = ink, linewidth = 0.42) +
  geom_vline(xintercept = 0, color = ink, linewidth = 0.42) +
  geom_line(linewidth = 0.75, linetype = "dotted") +
  geom_segment(
    data = sensitivity_extrapolation,
    aes(
      x = X, y = Y, xend = Xend, yend = Yend
    ),
    inherit.aes = FALSE,
    color = recipient_colour,
    linewidth = 0.75,
    linetype = "dotted"
  ) +
  geom_point(aes(shape = Scenario), size = 3.0, stroke = 0.85) +
  geom_segment(
    data = sensitivity_metrics,
    aes(
      x = pmin(
        CGE_zero_loss_trillion_USD2010,
        Def_loss_benchmark_trillion_USD2010
      ),
      xend = pmax(
        CGE_zero_loss_trillion_USD2010,
        Def_loss_benchmark_trillion_USD2010
      ),
      y = 0,
      yend = 0
    ),
    inherit.aes = FALSE,
    color = "#A5A5A5",
    linewidth = 2.2,
    alpha = 0.25,
    lineend = "round"
  ) +
  geom_point(
    data = sensitivity_benchmark_points,
    aes(x = X, y = Y, shape = Benchmark),
    inherit.aes = FALSE,
    size = 3.2,
    stroke = 0.75,
    color = ink,
    fill = "white"
  ) +
  geom_text(
    data = sensitivity_benchmark_points,
    aes(x = X, y = LabelY, label = Label),
    inherit.aes = FALSE,
    size = 2.7,
    color = ink
  ) +
  facet_grid(
    rows = vars(SSP),
    cols = vars(Threshold),
    labeller = labeller(
      Threshold = as_labeller(threshold_labels),
      SSP = label_value
    ),
    drop = FALSE
  ) +
  scale_color_manual(
    values = c(Provider = provider_colour, Recipient = recipient_colour),
    name = NULL
  ) +
  scale_shape_manual(
    values = c(
      Def = 1,
      Aid = 16,
      `CGE-implied` = 23,
      `Def-loss benchmark` = 24
    ),
    breaks = c("Def", "Aid", "CGE-implied", "Def-loss benchmark"),
    labels = c(
      "Def", "Aid", "CGE-implied zero loss", "Def-loss benchmark"
    ),
    name = NULL
  ) +
  scale_x_continuous(
    breaks = sensitivity_x_breaks,
    labels = label_number(accuracy = 1),
    limits = c(0, sensitivity_x_max),
    expand = expansion(mult = c(0, 0)),
    sec.axis = sec_axis(
      ~ . / period_years,
      breaks = sensitivity_x_breaks / period_years,
      labels = label_number(accuracy = 0.1),
      name = "Average annual amount, 2030-2050 (trillion US$2010/yr)"
    )
  ) +
  scale_y_continuous(
    labels = label_number(accuracy = 1, suffix = "%"),
    limits = c(sensitivity_y_min, sensitivity_y_max),
    expand = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Cumulative amount, 2030-2050 (trillion US$2010, undiscounted)",
    y = "Cumulative consumption loss rate to 2050 (5% NPV)"
  ) +
  guides(
    color = guide_legend(
      order = 1,
      override.aes = list(linetype = 0, shape = 16, size = 2.5)
    ),
    shape = guide_legend(
      order = 2,
      override.aes = list(color = ink, fill = "white", linetype = 0)
    )
  ) +
  theme_classic(base_size = 10, base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(fill = NA, color = ink, linewidth = 0.35),
    axis.line = element_blank(),
    axis.ticks = element_line(color = ink, linewidth = 0.40),
    axis.title = element_text(color = ink, size = 10),
    axis.text = element_text(color = ink, size = 8.8),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", color = ink, size = 10),
    panel.spacing = grid::unit(5, "mm"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.key = element_blank(),
    legend.text = element_text(size = 8.5),
    legend.title = element_text(size = 8.5),
    plot.margin = margin(8, 10, 8, 8)
  )

sensitivity_png <- file.path(
  output_dir, "Cumulative_transfer_zero_loss_SSP_threshold_facets.png"
)
sensitivity_svg <- file.path(
  output_dir, "Cumulative_transfer_zero_loss_SSP_threshold_facets.svg"
)
sensitivity_source_csv <- file.path(
  csv_dir, "Cumulative_transfer_zero_loss_SSP_threshold_facets_source_data.csv"
)
sensitivity_benchmark_csv <- file.path(
  csv_dir, "Cumulative_transfer_zero_loss_SSP_threshold_facets_benchmarks.csv"
)
sensitivity_annual_csv <- file.path(
  csv_dir, "Cumulative_transfer_zero_loss_SSP_threshold_facets_annual_flows.csv"
)

ggsave(
  sensitivity_png, sensitivity_figure,
  width = 13.2, height = 10.2, dpi = 600, bg = "white"
)
if (requireNamespace("svglite", quietly = TRUE)) {
  ggsave(
    sensitivity_svg, sensitivity_figure,
    width = 13.2, height = 10.2,
    device = svglite::svglite, bg = "white"
  )
}
write.csv(sensitivity_points, sensitivity_source_csv,
          row.names = FALSE, na = "")
write.csv(sensitivity_metrics, sensitivity_benchmark_csv,
          row.names = FALSE, na = "")
write.csv(sensitivity_annual, sensitivity_annual_csv,
          row.names = FALSE, na = "")

message("Saved figure: ", normalizePath(png_file, winslash = "/"))
if (file.exists(svg_file)) {
  message("Saved vector figure: ", normalizePath(svg_file, winslash = "/"))
}
message("Saved source data: ", normalizePath(source_csv, winslash = "/"))
message(
  "CGE-implied zero-loss funding: ",
  number(cge_zero_loss_funding, accuracy = 0.01), " trillion USD2010"
)
message(
  "Def-loss accounting benchmark: ",
  number(accounting_zero_loss_funding, accuracy = 0.01), " trillion USD2010"
)
message(
  "Difference (benchmark minus CGE): ",
  number(funding_gap, accuracy = 0.01), " trillion USD2010"
)
message(
  "Saved SSP-threshold figure: ",
  normalizePath(sensitivity_png, winslash = "/")
)

if (interactive()) {
  print(figure)
  print(sensitivity_figure)
}
