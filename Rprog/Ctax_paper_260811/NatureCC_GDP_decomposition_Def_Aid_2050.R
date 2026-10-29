# Nature Climate Change-style GDP expenditure decomposition for Def and Aid
#
# Inputs:
#   data/AR6database/SSP2_BaU_NoCC_No.gdx
#   data/AR6database/SSP2_400C_2030CP_NoCC_No.gdx
#   data/AR6database/SSP2_400C_2030CP_15th_NoCC_No.gdx
#
# Outputs:
#   output/Figure/analysis/NatureCC_GDP_decomposition_Def_Aid_2050.{png,pdf,svg}
#   output/Figure/analysis/NatureCC_GDP_decomposition_Def_Aid_2050_data.csv

project_library_candidates <- c(
  file.path(getwd(), ".r-lib"),
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

# Paths -----------------------------------------------------------------------

find_project_dir <- function(start_dir) {
  candidate <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(candidate, "CTaxRev.Rproj"))) return(candidate)
    parent <- dirname(candidate)
    if (identical(parent, candidate)) stop("Could not find the CTaxRev project root.")
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
data_dir <- file.path(project_dir, "data", "AR6database")
output_dir <- file.path(project_dir, "output", "Figure", "analysis")
output_stem <- file.path(
  output_dir, "NatureCC_GDP_decomposition_Def_Aid_2050"
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

scenario_files <- c(
  BaU = "SSP2_BaU_NoCC_No.gdx",
  Def = "SSP2_400C_2030CP_NoCC_No.gdx",
  Aid = "SSP2_400C_2030CP_15th_NoCC_No.gdx"
)
scenario_paths <- setNames(
  file.path(data_dir, unname(scenario_files)),
  names(scenario_files)
)
missing_files <- scenario_paths[!file.exists(scenario_paths)]
if (length(missing_files) > 0) {
  stop("Missing GDX file(s): ", paste(missing_files, collapse = ", "))
}

gams_dirs <- c(
  Sys.getenv("GAMS_SYSDIR"),
  "C:/GAMS/34",
  "C:/GAMS/win64/26.1"
)
gams_dirs <- gams_dirs[nzchar(gams_dirs) & dir.exists(gams_dirs)]
if (length(gams_dirs) > 0) igdx(gams_dirs[[1]])

# Region and category definitions ---------------------------------------------

# The group membership follows the current Def/Aid analysis in this project.
providers <- c(
  "USA", "XE25", "JPN", "CAN", "XER", "XOC", "TUR", "CHN", "XLM", "CIS"
)
recipients <- c("IND", "XSE", "XSA", "BRA", "XME", "XNF", "XAF")
region_map <- tibble(
  region = c(providers, recipients),
  group = c(
    rep("Provider regions", length(providers)),
    rep("Recipient regions", length(recipients))
  )
)

scenario_levels <- c("Def", "Aid")
group_levels <- c("Provider regions", "Recipient regions")
category_levels <- c(
  "Consumption", "Government spending", "Investment", "Net exports"
)

read_gdp <- function(scenario, path) {
  result <- rgdx.param(path, "GDP_s")
  required_columns <- c("Y", "R", "INS_MCR", "GDP_s")
  if (!all(required_columns %in% names(result))) {
    stop("Unexpected GDP_s columns in ", basename(path), ".")
  }

  result %>%
    transmute(
      scenario = scenario,
      year = as.numeric(as.character(Y)),
      region = as.character(R),
      category_code = as.character(INS_MCR),
      category = case_when(
        category_code == "HURB" ~ "Consumption",
        category_code == "GOV" ~ "Government spending",
        category_code == "S-I" ~ "Investment",
        category_code %in% c("ROW", "IMP") ~ "Net exports",
        TRUE ~ NA_character_
      ),
      value = as.numeric(GDP_s)
    )
}

raw_gdp <- bind_rows(Map(
  read_gdp,
  names(scenario_paths),
  unname(scenario_paths)
))

if (any(is.na(raw_gdp$year)) || any(!is.finite(raw_gdp$value))) {
  stop("GDP_s contains a non-numeric year or non-finite value.")
}
if (any(is.na(raw_gdp$category))) {
  stop(
    "Unrecognised GDP_s category code(s): ",
    paste(unique(raw_gdp$category_code[is.na(raw_gdp$category)]), collapse = ", ")
  )
}

# Decomposition ---------------------------------------------------------------

# BaU has no explicit GOV records. Completing the full grid with zeros is
# required so that the policy-induced government-spending contribution is not
# silently discarded.
group_gdp <- raw_gdp %>%
  filter(year == 2050, region %in% region_map$region) %>%
  left_join(region_map, by = "region") %>%
  group_by(scenario, group, category) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  complete(
    scenario = names(scenario_files),
    group = group_levels,
    category = category_levels,
    fill = list(value = 0)
  )

expected_rows <- length(scenario_files) * length(group_levels) *
  length(category_levels)
if (nrow(group_gdp) != expected_rows || anyDuplicated(group_gdp[c(
  "scenario", "group", "category"
)])) {
  stop("The completed scenario-group-category grid is not unique and complete.")
}

contributions <- group_gdp %>%
  group_by(group, category) %>%
  mutate(bau_category_value = value[scenario == "BaU"][[1]]) %>%
  ungroup() %>%
  group_by(group, scenario) %>%
  mutate(
    bau_total = sum(bau_category_value),
    contribution_pp = 100 * (value - bau_category_value) / bau_total
  ) %>%
  ungroup() %>%
  filter(scenario %in% scenario_levels) %>%
  mutate(
    scenario = factor(scenario, levels = scenario_levels),
    group = factor(group, levels = group_levels),
    category = factor(category, levels = category_levels)
  )

totals <- contributions %>%
  group_by(group, scenario) %>%
  summarise(
    scenario_total = sum(value),
    bau_total = first(bau_total),
    total_change_pct = sum(contribution_pp),
    .groups = "drop"
  ) %>%
  mutate(
    direct_total_change_pct = 100 * (scenario_total - bau_total) / bau_total,
    total_label = sprintf("%+.2f", total_change_pct)
  )

decomposition_error <- max(abs(
  totals$total_change_pct - totals$direct_total_change_pct
))
if (!is.finite(decomposition_error) || decomposition_error > 1e-10) {
  stop("Expenditure contributions do not reproduce the total GDP change.")
}

figure_data <- bind_rows(
  contributions %>%
    transmute(
      Year = 2050,
      Group = as.character(group),
      Scenario = as.character(scenario),
      Component = as.character(category),
      Scenario_value = value,
      BaU_component_value = bau_category_value,
      BaU_total_value = bau_total,
      Contribution_percentage_points = contribution_pp,
      Total_GDP_change_percent = NA_real_
    ),
  totals %>%
    transmute(
      Year = 2050,
      Group = as.character(group),
      Scenario = as.character(scenario),
      Component = "Net GDP change",
      Scenario_value = scenario_total,
      BaU_component_value = NA_real_,
      BaU_total_value = bau_total,
      Contribution_percentage_points = NA_real_,
      Total_GDP_change_percent = total_change_pct
    )
)
write.csv(
  figure_data,
  paste0(output_stem, "_data.csv"),
  row.names = FALSE,
  na = ""
)

# Figure ----------------------------------------------------------------------

component_colors <- c(
  "Consumption" = "#0072B2",
  "Government spending" = "#CC79A7",
  "Investment" = "#E69F00",
  "Net exports" = "#009E73"
)

# Label placement adapts to the plotted data and remains outside the diamond.
y_span <- diff(range(c(contributions$contribution_pp, totals$total_change_pct)))
label_offset <- max(0.25, 0.035 * y_span)
totals <- totals %>%
  mutate(
    label_y = total_change_pct + if_else(total_change_pct >= 0, label_offset, -label_offset),
    label_vjust = if_else(total_change_pct >= 0, 0, 1)
  )

theme_nature <- function() {
  theme_classic(base_size = 7, base_family = "sans") +
    theme(
      text = element_text(colour = "#222222"),
      axis.title = element_text(size = 7),
      axis.text = element_text(size = 6.5, colour = "#222222"),
      axis.line.x = element_line(colour = "#333333", linewidth = 0.30),
      axis.line.y = element_blank(),
      axis.ticks = element_line(colour = "#333333", linewidth = 0.25),
      axis.ticks.length = grid::unit(1.2, "mm"),
      panel.grid.major.y = element_line(colour = "#E7E7E7", linewidth = 0.25),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "#F2F2F2", colour = NA),
      strip.text = element_text(face = "bold", size = 7.2, margin = margin(2, 2, 2, 2)),
      panel.spacing.x = grid::unit(5, "mm"),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 6.5),
      legend.key.height = grid::unit(3.0, "mm"),
      legend.key.width = grid::unit(4.5, "mm"),
      legend.spacing.x = grid::unit(1.0, "mm"),
      plot.caption = element_text(size = 6.2, colour = "#555555", hjust = 0),
      plot.margin = margin(4, 4, 3, 4, unit = "mm")
    )
}

figure <- ggplot(
  contributions,
  aes(x = scenario, y = contribution_pp, fill = category)
) +
  geom_hline(yintercept = 0, colour = "#333333", linewidth = 0.35) +
  geom_col(width = 0.58, colour = "white", linewidth = 0.20) +
  geom_point(
    data = totals,
    aes(x = scenario, y = total_change_pct, shape = "Net GDP change"),
    inherit.aes = FALSE,
    size = 2.6, stroke = 0.65, fill = "white", colour = "black"
  ) +
  geom_text(
    data = totals,
    aes(x = scenario, y = label_y, label = total_label, vjust = label_vjust),
    inherit.aes = FALSE,
    size = 2.25, family = "sans", colour = "#222222"
  ) +
  facet_grid(. ~ group) +
  scale_fill_manual(values = component_colors, breaks = category_levels) +
  scale_shape_manual(values = c("Net GDP change" = 23)) +
  scale_y_continuous(
    labels = label_number(accuracy = 0.1),
    expand = expansion(mult = c(0.13, 0.16))
  ) +
  guides(
    fill = guide_legend(
      order = 1, nrow = 1, override.aes = list(colour = NA)
    ),
    shape = guide_legend(
      order = 2,
      override.aes = list(fill = "white", colour = "black", size = 2.4)
    )
  ) +
  labs(
    x = NULL,
    y = "Contribution to GDP change relative to BaU (%)",
    caption = paste(
      "Bars show expenditure-component contributions; diamonds and labels show net GDP change.",
      "Values are for 2050 and aggregated across regions in each group."
    )
  ) +
  theme_nature()

# Export at Nature's final double-column width (180 mm), using a compact height.
png_file <- paste0(output_stem, ".png")
pdf_file <- paste0(output_stem, ".pdf")
svg_file <- paste0(output_stem, ".svg")

ggsave(
  png_file, figure, width = 180, height = 92, units = "mm",
  dpi = 600, bg = "white", limitsize = FALSE,
  device = grDevices::png, type = "cairo-png"
)
ggsave(
  pdf_file, figure, width = 180, height = 92, units = "mm",
  bg = "white", limitsize = FALSE, device = grDevices::cairo_pdf
)
ggsave(
  svg_file, figure, width = 180, height = 92, units = "mm",
  bg = "white", limitsize = FALSE,
  device = function(filename, ...) {
    grDevices::svg(filename = filename, family = "sans", onefile = TRUE, ...)
  }
)

print(totals %>% select(group, scenario, total_change_pct))
message("Saved: ", png_file)
message("Saved: ", pdf_file)
message("Saved: ", svg_file)
message("Saved: ", paste0(output_stem, "_data.csv"))
message(
  "Maximum decomposition error: ",
  format(decomposition_error, scientific = TRUE)
)
