# Five-year cumulative revenue and consumption-loss trajectories -------------
#
# The figure follows the SSP2 / GDP-cap 15th Aid case used in the main
# cumulative-transfer figure. The horizontal coordinate is the model-reported
# cumulative carbon-tax revenue of the provider aggregate and the vertical
# coordinate is the model-reported cumulative consumption-loss rate of the
# provider or recipient aggregate. Values are shown at five-year model years
# from 2030 through 2050. The Def case is excluded because it has no transfer.

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

scenario_lookup <- tibble::tribble(
  ~Scenario, ~ScenarioCode,
  "Aid", "SSP2_400C_2030CP_15th_NoCC_No"
)
region_lookup <- tibble::tribble(
  ~Group, ~Region,
  "Provider", "Rprovider15th",
  "Recipient", "Rrecipient15th"
)

model_years <- seq(2030, 2050, by = 5)
cumulative_revenue_variable <- "Cum_Rev_gov_Tax_Car_Tax"
loss_rate_variable <- "Pol_Cos_Cns_Los_rat_NPV_5pc"
variables_needed <- c(cumulative_revenue_variable, loss_rate_variable)
scenarios_needed <- scenario_lookup$ScenarioCode

gdx_file <- file.path(project_root, "data", "AR6database", "global_17_IAMC.gdx")
output_dir <- file.path(project_root, "output", "Figure")
csv_dir <- file.path(output_dir, "csv")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)

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
    mutate(Year = as.numeric(Year), Value = as.numeric(Value))
}

message("Reading selected IAMC_template records from: ", gdx_file)
iamc <- read_iamc_subset(gdx_file, scenarios_needed, variables_needed) %>%
  filter(
    SCENARIO %in% scenarios_needed,
    VEMF %in% variables_needed,
    Year %in% model_years,
    is.finite(Value)
  )

# Revenue is taken from the provider aggregate in each scenario. The same
# scenario-year revenue coordinate is used for the provider and recipient loss
# rates, because it is the size of the common carbon-tax revenue pool.
revenue_data <- iamc %>%
  filter(
    VEMF == cumulative_revenue_variable,
    REMF == "Rprovider15th"
  ) %>%
  inner_join(scenario_lookup, by = c("SCENARIO" = "ScenarioCode")) %>%
  transmute(
    Scenario,
    Year,
    CumulativeRevenue_trillion_USD2010 = Value / 1e6
  )

loss_data <- iamc %>%
  filter(
    VEMF == loss_rate_variable,
    REMF %in% region_lookup$Region
  ) %>%
  inner_join(scenario_lookup, by = c("SCENARIO" = "ScenarioCode")) %>%
  inner_join(region_lookup, by = c("REMF" = "Region")) %>%
  transmute(
    Scenario,
    Group,
    Year,
    CumulativeConsumptionLossRate_pct = Value
  )

expected_revenue_rows <- nrow(scenario_lookup) * length(model_years)
expected_loss_rows <-
  nrow(scenario_lookup) * nrow(region_lookup) * length(model_years)
if (nrow(revenue_data) != expected_revenue_rows ||
    anyDuplicated(revenue_data[c("Scenario", "Year")])) {
  stop("The cumulative carbon-tax revenue series is incomplete or duplicated.")
}
if (nrow(loss_data) != expected_loss_rows ||
    anyDuplicated(loss_data[c("Scenario", "Group", "Year")])) {
  stop("The cumulative consumption-loss-rate series is incomplete or duplicated.")
}

plot_data <- loss_data %>%
  left_join(revenue_data, by = c("Scenario", "Year")) %>%
  mutate(
    Scenario = factor(Scenario, levels = "Aid"),
    Group = factor(Group, levels = c("Provider", "Recipient")),
    LabelHjust = if_else(Group == "Provider", -0.18, 1.18),
    LabelVjust = if_else(Group == "Provider", -0.65, 1.35)
  ) %>%
  arrange(Scenario, Group, Year)

if (any(!is.finite(plot_data$CumulativeRevenue_trillion_USD2010)) ||
    any(!is.finite(plot_data$CumulativeConsumptionLossRate_pct))) {
  stop("Non-finite plotting values were found.")
}

provider_colour <- "#3F6C8E"
recipient_colour <- "#B6573F"
ink <- "#222222"

figure <- ggplot(
  plot_data,
  aes(
    x = CumulativeRevenue_trillion_USD2010,
    y = CumulativeConsumptionLossRate_pct,
    color = Group,
    group = Group
  )
) +
  geom_hline(yintercept = 0, color = ink, linewidth = 0.45) +
  geom_path(linewidth = 0.9, lineend = "round") +
  geom_point(size = 3.1) +
  geom_text(
    aes(label = Year, hjust = LabelHjust, vjust = LabelVjust),
    size = 3.0,
    color = ink,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(Provider = provider_colour, Recipient = recipient_colour),
    breaks = c("Provider", "Recipient"),
    name = NULL
  ) +
  scale_x_continuous(
    labels = label_number(accuracy = 1),
    expand = expansion(mult = c(0.09, 0.13))
  ) +
  scale_y_continuous(
    labels = label_number(accuracy = 1, suffix = "%"),
    expand = expansion(mult = c(0.12, 0.12))
  ) +
  labs(
    x = "Cumulative carbon-tax revenue (trillion US$2010)",
    y = "Cumulative consumption loss rate"
  ) +
  theme_classic(base_size = 11, base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.line = element_line(color = ink, linewidth = 0.55),
    axis.ticks = element_line(color = ink, linewidth = 0.45),
    axis.title = element_text(color = ink, size = 10.5),
    axis.text = element_text(color = ink, size = 9.5),
    strip.background = element_blank(),
    strip.text = element_text(color = ink, face = "plain", size = 10.5),
    legend.position = "top",
    legend.justification = "right",
    legend.key = element_blank(),
    panel.spacing.x = grid::unit(1.5, "lines"),
    plot.margin = margin(10, 12, 10, 10)
  )

png_file <- file.path(
  output_dir, "Cumulative_revenue_consumption_loss_trajectory.png"
)
svg_file <- file.path(
  output_dir, "Cumulative_revenue_consumption_loss_trajectory.svg"
)
csv_file <- file.path(
  csv_dir, "Cumulative_revenue_consumption_loss_trajectory_source_data.csv"
)

ggsave(png_file, figure, width = 9.0, height = 5.4, dpi = 600, bg = "white")
if (requireNamespace("svglite", quietly = TRUE)) {
  ggsave(
    svg_file,
    figure,
    device = svglite::svglite,
    width = 9.0,
    height = 5.4,
    bg = "white"
  )
}
write.csv(plot_data, csv_file, row.names = FALSE, na = "")

message("Saved figure: ", png_file)
message("Saved source data: ", csv_file)
