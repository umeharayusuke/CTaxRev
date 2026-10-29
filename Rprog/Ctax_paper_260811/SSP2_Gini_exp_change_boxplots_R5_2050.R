# SSP2 country expenditure-Gini changes from BaU in 2050 --------------------
#
# Country-level Gini_exp changes are shown as boxplots for the World and the
# five R5 aggregates. The policy cases are SSP2 Default (no aid) and SSP2 Aid
# under the 10k, 15k, and 20k USD thresholds. Gini_exp is reported on a 0-1
# scale in the GDX and converted here to changes on a 0-100 index-point scale.

find_project_root <- function(start) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    marker <- file.path(
      current, "data", "AR6database", "AnalysisExpenditure.gdx"
    )
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
  project_root, "data", "AR6database", "AnalysisExpenditure.gdx"
)
region_map_file <- file.path(
  project_root, "data", "AR6database", "RegionmapRagg.map"
)
output_dir <- file.path(project_root, "output", "Figure", "analysis")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_stem <- file.path(
  output_dir, "12_SSP2_Gini_exp_change_boxplots_R5_2050"
)

if (!file.exists(region_map_file)) {
  stop("Missing country-to-R5 mapping: ", region_map_file)
}

# Settings ------------------------------------------------------------------

comparison_year <- 2050
scenario_codes <- c(
  BaU = "SSP2_BaU_NoCC_No",
  Def = "SSP2_400C_2030CP_NoCC_No",
  `10th` = "SSP2_400C_2030CP_10th_NoCC_No",
  `15th` = "SSP2_400C_2030CP_15th_NoCC_No",
  `20th` = "SSP2_400C_2030CP_20th_NoCC_No"
)
policy_levels <- c("Def", "10th", "15th", "20th")
policy_labels <- c(
  "Def" = "Default (no aid)",
  "10th" = "Aid (10k USD)",
  "15th" = "Aid (15k USD)",
  "20th" = "Aid (20k USD)"
)
policy_colours <- c(
  "Def" = "#8C6BB1",
  "10th" = "#0072B2",
  "15th" = "#D55E00",
  "20th" = "#009E73"
)

r5_codes <- c("R5OECD90+EU", "R5REF", "R5ASIA", "R5MAF", "R5LAM")
r5_labels <- c(
  "R5OECD90+EU" = "OECD90 + EU",
  "R5REF" = "Reforming economies",
  "R5ASIA" = "Asia",
  "R5MAF" = "Middle East & Africa",
  "R5LAM" = "Latin America"
)
aggregate_levels <- c("World", unname(r5_labels[r5_codes]))
ink <- "#252525"

check_grid <- function(data, expected, keys, context) {
  if (anyDuplicated(as.data.frame(data[keys]))) {
    stop("Duplicate records in ", context, ".")
  }
  missing <- anti_join(expected, data, by = keys)
  if (nrow(missing)) {
    stop(
      "Missing ", nrow(missing), " records in ", context, "; first: ",
      paste(unlist(missing[1, ]), collapse = " / ")
    )
  }
}

# Country-to-R5 mapping ------------------------------------------------------

region_map_raw <- read.table(
  region_map_file,
  header = FALSE,
  quote = "\"",
  stringsAsFactors = FALSE,
  fill = TRUE,
  comment.char = ""
)
if (ncol(region_map_raw) < 3L) {
  stop("RegionmapRagg.map does not have the expected three columns.")
}

r5_map <- region_map_raw %>%
  transmute(
    ISO3 = as.character(V1),
    R5_code = as.character(V3)
  ) %>%
  filter(R5_code %in% r5_codes) %>%
  distinct(ISO3, R5_code)

if (anyDuplicated(r5_map$ISO3)) {
  duplicates <- unique(r5_map$ISO3[duplicated(r5_map$ISO3)])
  stop("Countries assigned to multiple R5 groups: ", paste(duplicates, collapse = ", "))
}

# Country Gini changes -------------------------------------------------------

message("Reading Gini_exp from: ", gdx_file)
gini <- rgdx.param(gdx_file, "Gini_exp") %>%
  transmute(
    Scenario_id = as.character(Ref),
    ISO3 = as.character(R),
    Year = as.numeric(as.character(Y)),
    Gini = as.numeric(Gini_exp)
  ) %>%
  filter(
    Scenario_id %in% unname(scenario_codes),
    Year == comparison_year
  ) %>%
  mutate(
    Scenario = names(scenario_codes)[match(Scenario_id, scenario_codes)]
  )

country_codes <- sort(unique(gini$ISO3))
check_grid(
  gini,
  crossing(
    Scenario_id = unname(scenario_codes),
    ISO3 = country_codes,
    Year = comparison_year
  ),
  c("Scenario_id", "ISO3", "Year"),
  "2050 country Gini_exp"
)
if (any(!is.finite(gini$Gini) | gini$Gini < 0 | gini$Gini > 1)) {
  stop("Gini_exp must be finite and on the 0-1 scale.")
}

unmapped_countries <- setdiff(country_codes, r5_map$ISO3)
if (length(unmapped_countries)) {
  stop(
    "Gini_exp countries missing from the R5 mapping: ",
    paste(unmapped_countries, collapse = ", ")
  )
}

country_changes <- gini %>%
  select(ISO3, Scenario, Gini) %>%
  pivot_wider(names_from = Scenario, values_from = Gini) %>%
  transmute(
    ISO3,
    Def = 100 * (Def - BaU),
    `10th` = 100 * (`10th` - BaU),
    `15th` = 100 * (`15th` - BaU),
    `20th` = 100 * (`20th` - BaU)
  ) %>%
  pivot_longer(
    all_of(policy_levels),
    names_to = "Policy",
    values_to = "Gini_change"
  ) %>%
  left_join(r5_map, by = "ISO3") %>%
  mutate(
    Policy = factor(Policy, levels = policy_levels),
    R5_region = unname(r5_labels[R5_code])
  )

if (any(!is.finite(country_changes$Gini_change)) ||
    any(is.na(country_changes$R5_region))) {
  stop("Country Gini changes or R5 classifications are incomplete.")
}

plot_data <- bind_rows(
  country_changes %>% mutate(Aggregate = "World"),
  country_changes %>% mutate(Aggregate = R5_region)
) %>%
  mutate(Aggregate = factor(Aggregate, levels = aggregate_levels))

expected_plot_rows <- length(country_codes) * length(policy_levels) * 2L
if (nrow(plot_data) != expected_plot_rows) {
  stop("The World-plus-R5 plotting data have an unexpected number of rows.")
}

write.csv(
  plot_data %>%
    mutate(
      Policy = as.character(Policy),
      Aggregate = as.character(Aggregate)
    ) %>%
    select(ISO3, R5_code, R5_region, Aggregate, Policy, Gini_change),
  paste0(output_stem, "_data.csv"),
  row.names = FALSE
)

summary_data <- plot_data %>%
  group_by(Aggregate, Policy) %>%
  summarise(
    Countries = n(),
    Median = median(Gini_change),
    Q1 = quantile(Gini_change, 0.25),
    Q3 = quantile(Gini_change, 0.75),
    Minimum = min(Gini_change),
    Maximum = max(Gini_change),
    .groups = "drop"
  )
write.csv(
  summary_data,
  paste0(output_stem, "_summary.csv"),
  row.names = FALSE
)

# Figure ---------------------------------------------------------------------

theme_nature <- function() {
  theme_classic(base_family = "Arial", base_size = 8) +
    theme(
      text = element_text(colour = ink),
      axis.title = element_text(size = 8.3),
      axis.text = element_text(size = 7.3, colour = ink),
      axis.ticks = element_line(colour = ink, linewidth = 0.25),
      axis.ticks.length = grid::unit(1.1, "mm"),
      axis.line = element_blank(),
      panel.border = element_rect(fill = NA, colour = ink, linewidth = 0.32),
      panel.grid.major.y = element_line(colour = "#E2E2E2", linewidth = 0.22),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = 8.0, margin = margin(b = 1.5)),
      panel.spacing.x = grid::unit(3.0, "mm"),
      panel.spacing.y = grid::unit(4.0, "mm"),
      legend.position = "bottom",
      legend.justification = "center",
      legend.title = element_blank(),
      legend.text = element_text(size = 7.2),
      legend.key.height = grid::unit(3.5, "mm"),
      legend.key.width = grid::unit(6.0, "mm"),
      plot.margin = margin(4, 5, 3, 4, unit = "mm")
    )
}

p <- ggplot(
  plot_data,
  aes(x = Policy, y = Gini_change, fill = Policy)
) +
  geom_hline(
    yintercept = 0,
    linetype = "12",
    colour = ink,
    linewidth = 0.30
  ) +
  geom_boxplot(
    width = 0.62,
    linewidth = 0.36,
    colour = ink,
    outlier.shape = 16,
    outlier.size = 1.15,
    outlier.stroke = 0,
    outlier.colour = ink
  ) +
  facet_wrap(~Aggregate, ncol = 3, drop = FALSE) +
  scale_fill_manual(
    values = policy_colours,
    breaks = policy_levels,
    labels = policy_labels,
    drop = FALSE
  ) +
  scale_x_discrete(
    labels = c(
      "Def" = "Default",
      "10th" = "10k",
      "15th" = "15k",
      "20th" = "20k"
    ),
    drop = FALSE
  ) +
  scale_y_continuous(
    labels = label_number(accuracy = 0.1),
    expand = expansion(mult = c(0.08, 0.12))
  ) +
  labs(
    x = NULL,
    y = "Change in expenditure Gini from BaU\n(0-100 index points)"
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  theme_nature()

ggsave(
  filename = paste0(output_stem, ".png"),
  plot = p,
  device = "png",
  type = "cairo",
  width = 180,
  height = 125,
  units = "mm",
  dpi = 400,
  bg = "white",
  limitsize = FALSE
)

message("Created: ", paste0(output_stem, ".png"))
message("Created: ", paste0(output_stem, "_data.csv"))
message("Created: ", paste0(output_stem, "_summary.csv"))
message("Countries included: ", length(country_codes))
print(summary_data, n = Inf)
