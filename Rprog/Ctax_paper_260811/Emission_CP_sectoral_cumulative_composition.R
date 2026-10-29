# Emissions, carbon price and cumulative sectoral composition ----------------
#
# This script creates a separate three-panel version of Emission_CP_sectoral.
# Panels a and b are reused without modification. Panel c shows absolute
# 2020-2050 cumulative CO2 emissions for BaU, Def and Aid in the provider and
# recipient aggregates, decomposed into six model sectors. Sector integrals are
# proportionally reconciled to the model-reported Emi_CO2_Cum increment so each
# stacked bar reproduces the corresponding net cumulative total.

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
  library(patchwork)
})

output_dir <- file.path(project_root, "output", "Figure")
csv_dir <- file.path(output_dir, "csv")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)

# Build the original plot and data objects in memory. Mask all writers so the
# existing Emission_CP_sectoral files and source-data CSV remain untouched.
base_script <- file.path(
  project_root, "Rprog", "Ctax_paper_260811", "Emission_CP_sectoral.R"
)
if (!file.exists(base_script)) stop("Missing base figure script: ", base_script)

base_env <- new.env(parent = globalenv())
base_env$ggsave <- function(...) invisible(NULL)
base_env$write.csv <- function(...) invisible(NULL)
sys.source(base_script, envir = base_env)

required_objects <- c(
  "p_global", "p_price", "df_emissions", "cumulative_emissions",
  "theme_panel", "col_emissions", "CO2Order", "figure_font"
)
missing_objects <- required_objects[
  !vapply(required_objects, exists, logical(1), envir = base_env)
]
if (length(missing_objects)) {
  stop("Missing object(s) from base script: ",
       paste(missing_objects, collapse = ", "))
}

# Absolute sectoral cumulative emissions -------------------------------------
sector_cumulative <- base_env$df_emissions %>%
  arrange(REMF, SCENARIO, VEMF, Year) %>%
  group_by(REMF, SCENARIO, VEMF) %>%
  summarise(
    RawCumulative = sum(
      (value + lead(value)) / 2 * (lead(Year) - Year),
      na.rm = TRUE
    ),
    .groups = "drop"
  )

reported_totals <- base_env$cumulative_emissions %>%
  transmute(
    REMF,
    SCENARIO = factor(
      as.character(Scenario),
      levels = c("BaU", "Def", "Aid")
    ),
    ReportedCumulative = Cumulative
  )

sector_cumulative <- sector_cumulative %>%
  mutate(
    REMF = factor(as.character(REMF), levels = c("Provider", "Recipient")),
    SCENARIO = factor(
      as.character(SCENARIO),
      levels = c("BaU", "Def", "Aid")
    ),
    VEMF = factor(as.character(VEMF), levels = base_env$CO2Order)
  ) %>%
  group_by(REMF, SCENARIO) %>%
  mutate(RawNet = sum(RawCumulative)) %>%
  ungroup() %>%
  left_join(reported_totals, by = c("REMF", "SCENARIO")) %>%
  mutate(
    ReconciliationFactor = if_else(
      abs(RawNet) > .Machine$double.eps,
      ReportedCumulative / RawNet,
      1
    ),
    Cumulative = RawCumulative * ReconciliationFactor
  ) %>%
  arrange(REMF, SCENARIO, VEMF)

expected_rows <- 2L * 3L * length(base_env$CO2Order)
if (nrow(sector_cumulative) != expected_rows ||
    anyDuplicated(sector_cumulative[c("REMF", "SCENARIO", "VEMF")]) ||
    any(!is.finite(sector_cumulative$Cumulative))) {
  stop("The cumulative sector-region-scenario grid is incomplete or invalid.")
}

sector_totals <- sector_cumulative %>%
  group_by(REMF, SCENARIO) %>%
  summarise(
    StackedNet = sum(Cumulative),
    ReportedCumulative = first(ReportedCumulative),
    .groups = "drop"
  )

reconciliation_error <- max(abs(
  sector_totals$StackedNet - sector_totals$ReportedCumulative
))
if (!is.finite(reconciliation_error) || reconciliation_error > 1e-8) {
  stop("Reconciled sector stacks do not reproduce Emi_CO2_Cum totals.")
}

# Panel c --------------------------------------------------------------------
p_cumulative_sector <- ggplot(
  sector_cumulative,
  aes(x = SCENARIO, y = Cumulative, fill = VEMF)
) +
  geom_hline(yintercept = 0, linewidth = 0.32, colour = "#505050") +
  geom_col(
    width = 0.62,
    position = position_stack(reverse = TRUE),
    colour = "white",
    linewidth = 0.18
  ) +
  facet_wrap(
    vars(REMF),
    nrow = 1,
    labeller = as_labeller(c(
      Provider = "Provider",
      Recipient = "Recipient"
    ))
  ) +
  scale_fill_manual(
    name = NULL,
    values = base_env$col_emissions,
    limits = base_env$CO2Order,
    breaks = base_env$CO2Order,
    drop = FALSE
  ) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(
    labels = label_number(accuracy = 1),
    expand = expansion(mult = c(0.07, 0.13))
  ) +
  labs(
    tag = "c",
    x = NULL,
    y = expression("Cumulative emissions, 2020-2050 (Gt CO"[2]*")")
  ) +
  guides(
    fill = guide_legend(order = 1, nrow = 2, byrow = TRUE)
  ) +
  base_env$theme_panel() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(
      fill = NA, colour = "#202020", linewidth = 0.32
    ),
    axis.line = element_blank(),
    axis.text.x = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 6.8),
    panel.spacing.x = grid::unit(5, "mm"),
    plot.margin = margin(5, 4, 4, 5, unit = "mm")
  )

# Layout: preserve panels a-b and give the new composition panel full width. --
top_row <- base_env$p_global + base_env$p_price +
  plot_layout(widths = c(1.8, 1))

figure_composite <- (top_row / p_cumulative_sector) +
  plot_layout(guides = "collect", heights = c(1.00, 1.12)) &
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.justification = "center",
    plot.background = element_rect(fill = "white", colour = NA)
  )

output_stem <- file.path(
  output_dir, "Emission_CP_sectoral_cumulative_composition"
)
png_file <- paste0(output_stem, ".png")
pdf_file <- paste0(output_stem, ".pdf")
svg_file <- paste0(output_stem, ".svg")
csv_file <- file.path(
  csv_dir, "Emission_CP_sectoral_cumulative_composition_source_data.csv"
)

ggplot2::ggsave(
  png_file,
  figure_composite,
  width = 180,
  height = 158,
  units = "mm",
  dpi = 300,
  bg = "white",
  limitsize = FALSE,
  device = grDevices::png,
  type = "cairo-png"
)
ggplot2::ggsave(
  pdf_file,
  figure_composite,
  width = 180,
  height = 158,
  units = "mm",
  bg = "white",
  limitsize = FALSE,
  device = grDevices::cairo_pdf
)
if (requireNamespace("svglite", quietly = TRUE)) {
  ggplot2::ggsave(
    svg_file,
    figure_composite,
    device = svglite::svglite,
    width = 180 / 25.4,
    height = 158 / 25.4,
    bg = "white",
    limitsize = FALSE
  )
}

utils::write.csv(
  sector_cumulative %>%
    transmute(
      Region = as.character(REMF),
      Scenario = as.character(SCENARIO),
      Sector = as.character(VEMF),
      Raw_sector_cumulative_GtCO2 = RawCumulative,
      Reconciliation_factor = ReconciliationFactor,
      Sector_cumulative_GtCO2 = Cumulative,
      Net_cumulative_GtCO2 = ReportedCumulative
    ),
  csv_file,
  row.names = FALSE,
  na = ""
)

message("Saved three-panel figure: ", png_file)
message("Saved source data: ", csv_file)
message(
  "Maximum cumulative-emissions reconciliation error: ",
  format(reconciliation_error, scientific = TRUE)
)
