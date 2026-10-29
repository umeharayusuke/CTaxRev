# 2025 regional GDP per capita and aid-threshold classification -------------
#
# GDP_per_cap is stored in kUSD_2010/person. The 10k, 15k, and 20k thresholds
# classify a region as Recipient below the threshold and Provider at or above
# it. The classification is checked against all SSP1-SSP3 BaU values; SSP2 is
# then used as the representative value series in the figure.

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
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(patchwork)
})

if (!requireNamespace("gdxrrw", quietly = TRUE)) {
  stop("The gdxrrw package is required to read IAMC_template.")
}

gams_candidates <- c(
  Sys.getenv("GAMSDIR", unset = ""),
  "C:/GAMS/34",
  "C:/GAMS/win64/26.1"
)
gams_candidates <- unique(gams_candidates[nzchar(gams_candidates)])
gams_dir <- gams_candidates[dir.exists(gams_candidates)][1]
if (is.na(gams_dir)) stop("Could not locate a GAMS installation.")
gdxrrw::igdx(gams_dir)

regions <- c(
  "XE25", "JPN", "IND", "TUR", "CHN", "USA", "XER", "XOC", "XSE",
  "XSA", "CAN", "BRA", "XLM", "CIS", "XME", "XNF", "XAF"
)
scenarios <- paste0("SSP", 1:3, "_BaU_NoCC_No")
thresholds <- data.frame(
  Threshold_kUSD = c(10, 15, 20),
  Threshold = factor(c("10k", "15k", "20k"), levels = c("10k", "15k", "20k"))
)

expected_recipients <- list(
  `10` = c("IND", "XSA", "XNF", "XAF"),
  `15` = c("IND", "BRA", "XSA", "XSE", "XME", "XNF", "XAF"),
  `20` = c(
    "CHN", "IND", "BRA", "XSA", "XSE", "XME", "XLM", "XNF", "XAF", "CIS"
  )
)

gdx_file <- file.path(
  project_root, "data", "AR6database", "global_17_IAMC.gdx"
)
message("Reading IAMC_template from: ", gdx_file)
raw <- gdxrrw::rgdx.param(gdx_file, "IAMC_template")

required_columns <- c("SCENARIO", "REMF", "VEMF", "YEMF", "IAMC_Template")
if (!all(required_columns %in% names(raw))) {
  stop("IAMC_template does not have the expected columns.")
}

gdp_all <- raw %>%
  transmute(
    SCENARIO = as.character(SCENARIO),
    Region = as.character(REMF),
    VEMF = as.character(VEMF),
    Year = as.numeric(as.character(YEMF)),
    GDPpc_kUSD = as.numeric(IAMC_Template)
  ) %>%
  filter(
    SCENARIO %in% scenarios,
    Region %in% regions,
    VEMF == "GDP_per_cap",
    Year == 2025
  ) %>%
  select(SCENARIO, Region, GDPpc_kUSD)

expected_rows <- length(scenarios) * length(regions)
if (nrow(gdp_all) != expected_rows) {
  stop(
    "Expected ", expected_rows, " regional 2025 GDP-per-capita values; found ",
    nrow(gdp_all), "."
  )
}
if (anyDuplicated(gdp_all[c("SCENARIO", "Region")])) {
  stop("Duplicate regional GDP-per-capita values were found.")
}
if (any(!is.finite(gdp_all$GDPpc_kUSD)) || any(gdp_all$GDPpc_kUSD <= 0)) {
  stop("Regional GDP-per-capita values must be finite and positive.")
}

# Verify that every SSP yields exactly the region memberships supplied for the
# three thresholds, even though unrounded GDP values differ slightly by SSP.
classification_all <- crossing(gdp_all, thresholds) %>%
  mutate(
    Classification = if_else(
      GDPpc_kUSD < Threshold_kUSD, "Recipient", "Provider"
    )
  )

for (scenario in scenarios) {
  for (threshold in thresholds$Threshold_kUSD) {
    observed <- classification_all %>%
      filter(
        SCENARIO == scenario,
        Threshold_kUSD == threshold,
        Classification == "Recipient"
      ) %>%
      pull(Region)
    expected <- expected_recipients[[as.character(threshold)]]
    if (!setequal(observed, expected)) {
      stop(
        "Recipient classification differs from the supplied mapping for ",
        scenario, " at ", threshold, "k."
      )
    }
  }
}

reference_scenario <- "SSP2_BaU_NoCC_No"
reference_data <- gdp_all %>%
  filter(SCENARIO == reference_scenario) %>%
  arrange(GDPpc_kUSD)
region_levels <- reference_data$Region

reference_data <- reference_data %>%
  mutate(
    Region = factor(Region, levels = region_levels),
    Value_label = sprintf("%.1f", GDPpc_kUSD)
  )

classification_data <- crossing(
  reference_data %>% select(Region, GDPpc_kUSD),
  thresholds
) %>%
  mutate(
    Classification = if_else(
      GDPpc_kUSD < Threshold_kUSD, "Recipient", "Provider"
    ),
    Classification = factor(
      Classification, levels = c("Recipient", "Provider")
    ),
    Cell_label = if_else(Classification == "Provider", "P", "R")
  )

band_data <- data.frame(
  xmin = c(0, 10, 15, 20),
  xmax = c(10, 15, 20, 68),
  Fill = c("#F7ECE8", "#F7F3E8", "#EEF4EE", "#EAF1F5")
)
threshold_lines <- data.frame(
  Threshold_kUSD = c(10, 15, 20),
  Threshold = factor(c("10k", "15k", "20k"), levels = c("10k", "15k", "20k"))
)
threshold_colours <- c("10k" = "#0072B2", "15k" = "#D55E00", "20k" = "#009E73")
classification_colours <- c("Recipient" = "#B6573F", "Provider" = "#3F6C8E")
ink <- "#252525"

p_values <- ggplot(reference_data, aes(y = Region)) +
  geom_rect(
    data = band_data,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = Fill),
    inherit.aes = FALSE,
    alpha = 0.72
  ) +
  scale_fill_identity() +
  geom_vline(
    data = threshold_lines,
    aes(xintercept = Threshold_kUSD, colour = Threshold),
    linewidth = 0.65,
    linetype = "22",
    show.legend = FALSE
  ) +
  geom_segment(
    aes(x = 0, xend = GDPpc_kUSD, yend = Region),
    linewidth = 0.35,
    colour = "#A9A9A9"
  ) +
  geom_point(aes(x = GDPpc_kUSD), size = 2.25, colour = ink) +
  geom_text(
    aes(x = GDPpc_kUSD, label = Value_label),
    nudge_x = 0.75,
    hjust = 0,
    family = "Arial",
    size = 2.35,
    colour = ink
  ) +
  scale_colour_manual(values = threshold_colours) +
  scale_x_continuous(
    name = expression(paste("2025 GDP per capita (kUS$"[2010], "/person)")),
    breaks = c(0, 10, 15, 20, 40, 60),
    limits = c(0, 68),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_discrete(limits = region_levels, drop = FALSE, expand = expansion(add = 0.55)) +
  coord_cartesian(clip = "off") +
  theme_classic(base_family = "Arial", base_size = 8) +
  theme(
    text = element_text(colour = ink),
    axis.line = element_blank(),
    axis.ticks = element_line(linewidth = 0.25, colour = ink),
    axis.title.x = element_text(size = 8.2),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 7.4, colour = ink),
    panel.border = element_rect(fill = NA, colour = ink, linewidth = 0.30),
    panel.grid = element_blank(),
    plot.margin = margin(4, 7, 4, 4)
  )

p_classification <- ggplot(
  classification_data,
  aes(x = Threshold, y = Region, fill = Classification)
) +
  geom_tile(width = 0.90, height = 0.90, colour = "white", linewidth = 0.45) +
  geom_text(
    aes(label = Cell_label),
    family = "Arial",
    fontface = "bold",
    size = 2.6,
    colour = "white"
  ) +
  scale_fill_manual(
    values = classification_colours,
    breaks = c("Recipient", "Provider"),
    labels = c(
      "Recipient" = "Developing / Recipient",
      "Provider" = "Developed / Provider"
    ),
    name = NULL
  ) +
  scale_x_discrete(position = "bottom", drop = FALSE) +
  scale_y_discrete(limits = region_levels, drop = FALSE, expand = expansion(add = 0.55)) +
  labs(x = "GDP per capita threshold", y = NULL) +
  guides(fill = guide_legend(nrow = 1, title.position = "top")) +
  theme_classic(base_family = "Arial", base_size = 8) +
  theme(
    text = element_text(colour = ink),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(size = 8.2, margin = margin(t = 2)),
    axis.text.x = element_text(size = 7.4, face = "bold", colour = ink),
    axis.text.y = element_blank(),
    panel.border = element_rect(fill = NA, colour = ink, linewidth = 0.30),
    legend.position = "bottom",
    legend.justification = "center",
    legend.text = element_text(size = 7.4),
    legend.key.size = grid::unit(3.7, "mm"),
    plot.margin = margin(4, 4, 4, 1)
  )

p <- wrap_plots(
  A = p_values,
  B = p_classification,
  C = guide_area(),
  design = "AB\nCC",
  widths = c(2.35, 1),
  heights = c(1, 0.10),
  guides = "collect"
) +
  plot_annotation(tag_levels = "a") &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.tag = element_text(
      family = "Arial", face = "bold", size = 9.0, colour = ink
    ),
    plot.tag.position = "topleft"
  )

output_dir <- file.path(project_root, "output", "Figure", "analysis")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_file <- file.path(
  output_dir, "06_GDP_per_capita_threshold_classification_2025.png"
)

ggsave(
  filename = output_file,
  plot = p,
  device = "png",
  type = "cairo",
  width = 180,
  height = 125,
  units = "mm",
  dpi = 400,
  bg = "white"
)

message("Created: ", output_file)
message("2025 GDP per capita used in the figure (kUSD_2010/person):")
print(
  reference_data %>% select(Region, GDPpc_kUSD),
  row.names = FALSE,
  digits = 6
)
