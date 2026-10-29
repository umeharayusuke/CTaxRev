# AR6 ranges and AIM emissions/price pathways
# Nature Portfolio double-column figure with sectoral panels

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
  library(readr)
  library(readxl)
  library(ggplot2)
  library(gdxrrw)
  library(patchwork)
  library(scales)
})

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
gdx_file <- file.path(data_dir, "global_17_IAMC.gdx")
ar6_meta_file <- file.path(
  data_dir, "AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx"
)
ar6_world_file <- file.path(
  data_dir, "AR6_Scenarios_Database_World_v1.1.csv"
)
output_dir <- file.path(project_dir, "output", "Figure")
csv_dir <- file.path(output_dir, "csv")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(csv_dir, showWarnings = FALSE, recursive = TRUE)

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

scenario_names <- c("SSP2_BaU_NoCC_No" = "BaU", "SSP2_400C_2030CP_NoCC_No" = "Def", "SSP2_400C_2030CP_15th_NoCC_No" = "Aid")
scenario_colors <- c("BaU" = "#252525", "Def" = "#B6573F", "Aid" = "#3F6C8E")
scenario_shapes <- c("BaU" = 16, "Def" = 1, "Aid" = 15)
scenario_linetypes <- c("BaU" = "solid", "Def" = "22", "Aid" = "solid")
ar6_colors <- c("C1" = "#8AB9D1", "C2" = "#E7B65A")
figure_font <- "Arial"
year_breaks <- seq(2020, 2050, 5)

vec_emissions <- c("Emi_CO2_AFO", "Emi_CO2_Ene_Sup", "Emi_CO2_Ene_Dem",
                   "Emi_CO2_Ind_Pro", "Emi_CO2_Pro_Use", "Emi_CO2_Cap_and_Rem")
CO2Order <- c("Energy supply", "Energy demand", "Industrial processes",
              "Product use", "AFOLU", "Capture and removal")
col_emissions <- c(
  "Energy supply" = "#70C2A5",
  "Energy demand" = "#90A1CC",
  "Industrial processes" = "#954FA4",
  "Product use" = "#FDFE22",
  "AFOLU" = "#F58C60",
  "Capture and removal" = "#437FB9"
)
CLP <- names(scenario_names)
Region <- c("Rprovider15th", "Rrecipient15th")

required_files <- c(gdx_file, ar6_meta_file, ar6_world_file)
if (any(!file.exists(required_files))) stop("Missing file(s): ", paste(required_files[!file.exists(required_files)], collapse = ", "))

# AR6 data: read and reshape only once ---------------------------------------
ar6_category <- read_xlsx(ar6_meta_file, sheet = "meta_Ch3vetted_withclimate") %>%
  transmute(Case = paste0(Model, "_", Scenario), Category) %>%
  filter(Category %in% c("C1", "C2")) %>% distinct(Case, .keep_all = TRUE)

ar6_long <- read_csv(ar6_world_file, show_col_types = FALSE) %>%
  mutate(Case = paste0(Model, "_", Scenario)) %>%
  inner_join(ar6_category, by = "Case", relationship = "many-to-one") %>%
  filter(Region == "World", Variable %in% c("Emissions|CO2", "Price|Carbon")) %>%
  pivot_longer(cols = matches("^[0-9]{4}$"), names_to = "Year", values_to = "value", names_transform = list(Year = as.numeric)) %>%
  filter(!is.na(value), Year %in% year_breaks) %>%
  mutate(value = if_else(Variable == "Emissions|CO2", value/1000, value))

ar6_box <- ar6_long %>% group_by(Variable, Category, Year) %>%
  summarise(p05 = quantile(value, 0.05, na.rm = TRUE), p25 = quantile(value, 0.25, na.rm = TRUE),
            p50 = median(value, na.rm = TRUE), p75 = quantile(value, 0.75, na.rm = TRUE),
            p95 = quantile(value, 0.95, na.rm = TRUE),
            n = sum(is.finite(value)), .groups = "drop") %>%
  mutate(PlotYear = Year + if_else(Category == "C1", -0.7, 0.7))

# AIM data -------------------------------------------------------------------
iamc <- rgdx.param(gdx_file, "IAMC_template") %>%
  mutate(Year = as.numeric(as.character(YEMF)), Value = as.numeric(IAMC_Template),
         Scenario = recode(SCENARIO, !!!scenario_names),
         Scenario = factor(Scenario, levels = c("BaU", "Def", "Aid")))

aim_emissions <- iamc %>%
  filter(VEMF == "Emi_CO2", SCENARIO %in% names(scenario_names),
         REMF %in% c("World", "Rprovider15th", "Rrecipient15th"), Year >= 2020, Year <= 2050) %>%
  transmute(Year, REMF, Scenario, value = Value/1000)

aim_price <- iamc %>%
  filter(VEMF == "Prc_Car", SCENARIO %in% names(scenario_names), REMF == "World", Year >= 2020, Year <= 2050) %>%
  transmute(Year, Scenario, value = Value)

# Sectoral emissions. Original unit: MtCO2/yr; converted to GtCO2/yr.
df_emissions <- iamc %>%
  filter(VEMF %in% vec_emissions, SCENARIO %in% CLP, REMF %in% Region) %>%
  transmute(
    Year = as.numeric(as.character(YEMF)),
    REMF = recode(REMF,
                  "Rprovider15th" = "Provider",
                  "Rrecipient15th" = "Recipient"),
    SCENARIO = recode(SCENARIO,
                      "SSP2_BaU_NoCC_No" = "BaU",
                      "SSP2_400C_2030CP_NoCC_No" = "Def",
                      "SSP2_400C_2030CP_15th_NoCC_No" = "Aid"),
    VEMF = recode(VEMF,
                  "Emi_CO2_AFO" = "AFOLU",
                  "Emi_CO2_Ene_Sup" = "Energy supply",
                  "Emi_CO2_Ene_Dem" = "Energy demand",
                  "Emi_CO2_Ind_Pro" = "Industrial processes",
                  "Emi_CO2_Pro_Use" = "Product use",
                  "Emi_CO2_Cap_and_Rem" = "Capture and removal"),
    value = as.numeric(IAMC_Template) / 1000
  ) %>%
  filter(Year >= 2020, Year <= 2050) %>%
  group_by(REMF, SCENARIO, Year, VEMF) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    REMF = factor(REMF, levels = c("Provider", "Recipient")),
    SCENARIO = factor(SCENARIO, levels = c("BaU", "Def", "Aid")),
    VEMF = factor(VEMF, levels = CO2Order)
  ) %>%
  complete(REMF, SCENARIO, Year, VEMF, fill = list(value = 0))

# Integrate each annual sector pathway and express policy scenarios relative
# to BaU. The trapezoidal rule is used because IAMC time steps need not be
# strictly annual in every input file.
sector_cumulative_change <- df_emissions %>%
  arrange(REMF, SCENARIO, VEMF, Year) %>%
  group_by(REMF, SCENARIO, VEMF) %>%
  summarise(
    Cumulative = sum(
      (value + lead(value)) / 2 * (lead(Year) - Year),
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  left_join(
    df_emissions %>%
      filter(SCENARIO == "BaU") %>%
      arrange(REMF, VEMF, Year) %>%
      group_by(REMF, VEMF) %>%
      summarise(
        BaU = sum(
          (value + lead(value)) / 2 * (lead(Year) - Year),
          na.rm = TRUE
        ),
        .groups = "drop"
      ),
    by = c("REMF", "VEMF")
  ) %>%
  filter(SCENARIO != "BaU") %>%
  mutate(
    Change = Cumulative - BaU,
    SCENARIO = factor(SCENARIO, levels = c("Aid", "Def"))
  )

# Use the model's cumulative-emissions variable directly for the absolute
# regional totals. Subtracting its 2020 value gives the 2020--2050 increment.
cumulative_emissions <- iamc %>%
  filter(
    VEMF == "Emi_CO2_Cum", SCENARIO %in% CLP,
    REMF %in% Region, Year %in% c(2020, 2050)
  ) %>%
  transmute(
    REMF = recode(REMF,
                  "Rprovider15th" = "Provider",
                  "Rrecipient15th" = "Recipient"),
    Scenario = recode(SCENARIO, !!!scenario_names),
    Year, value = Value / 1000
  ) %>%
  pivot_wider(names_from = Year, values_from = value, names_prefix = "y") %>%
  mutate(
    Cumulative = y2050 - y2020,
    REMF = factor(REMF, levels = c("Provider", "Recipient")),
    Scenario = factor(Scenario, levels = c("BaU", "Def", "Aid"))
  ) %>%
  group_by(REMF) %>%
  mutate(
    BaU = Cumulative[Scenario == "BaU"],
    Change = Cumulative - BaU,
    Reduction = 100 * (BaU - Cumulative) / BaU
  ) %>%
  ungroup()

# Reconcile the sectoral integral to the model-reported cumulative total. The
# six reported sectors do not sum exactly to Emi_CO2_Cum because of timing and
# accounting-boundary differences; proportional scaling retains the sector
# pattern while making the panel-c net change equal panel d exactly.
direct_cumulative_change <- cumulative_emissions %>%
  filter(Scenario != "BaU") %>%
  transmute(
    REMF,
    SCENARIO = factor(as.character(Scenario), levels = c("Aid", "Def")),
    DirectChange = Change
  )

sector_cumulative_change <- sector_cumulative_change %>%
  group_by(REMF, SCENARIO) %>%
  mutate(RawNet = sum(Change)) %>%
  ungroup() %>%
  left_join(direct_cumulative_change, by = c("REMF", "SCENARIO")) %>%
  mutate(
    ReconciliationFactor = if_else(
      abs(RawNet) > .Machine$double.eps,
      DirectChange / RawNet, 1
    ),
    Change = Change * ReconciliationFactor
  )

# Nature Portfolio source-data table -----------------------------------------
ar6_source <- ar6_box %>%
  select(Variable, Category, Year, p05, p25, p50, p75, p95, n) %>%
  pivot_longer(
    cols = c(p05, p25, p50, p75, p95, n),
    names_to = "Statistic", values_to = "Value"
  ) %>%
  transmute(
    Panel = if_else(Variable == "Emissions|CO2", "a", "b"),
    Dataset = "AR6", Region = "World", Scenario = Category,
    Sector = NA_character_, Year, Statistic, Value,
    Unit = if_else(Variable == "Emissions|CO2",
                   "Gt CO2 yr-1", "USD t CO2-1")
  )

aim_source <- bind_rows(
  aim_emissions %>%
    filter(REMF == "World") %>%
    transmute(
      Panel = "a", Dataset = "AIM", Region = "World",
      Scenario = as.character(Scenario), Sector = NA_character_, Year,
      Statistic = "value", Value = value, Unit = "Gt CO2 yr-1"
    ),
  aim_price %>%
    transmute(
      Panel = "b", Dataset = "AIM", Region = "World",
      Scenario = as.character(Scenario), Sector = NA_character_, Year,
      Statistic = "value", Value = value, Unit = "USD t CO2-1"
    )
)

sector_source <- sector_cumulative_change %>%
  transmute(
    Panel = "c", Dataset = "AIM", Region = as.character(REMF),
    Scenario = as.character(SCENARIO), Sector = as.character(VEMF),
    Year = 2050, Statistic = "cumulative_change_from_BaU",
    Value = Change, Unit = "Gt CO2 (2020-2050)"
  )

cumulative_source <- cumulative_emissions %>%
  transmute(
    Panel = "d", Dataset = "AIM", Region = as.character(REMF),
    Scenario = as.character(Scenario), Sector = NA_character_,
    Year = 2050, Statistic = "cumulative_2020_2050",
    Value = Cumulative, Unit = "Gt CO2"
  )

source_data_file <- file.path(csv_dir, "Emission_CP_sectoral_source_data.csv")
write.csv(
  bind_rows(ar6_source, aim_source, sector_source, cumulative_source),
  source_data_file, row.names = FALSE
)

scenario_scale <- function() {
  scale_color_manual(
    name = NULL, values = scenario_colors,
    limits = c("BaU", "Def", "Aid"), drop = FALSE
  )
}
scenario_shape_scale <- function() {
  scale_shape_manual(
    name = NULL, values = scenario_shapes,
    limits = c("BaU", "Def", "Aid"), drop = FALSE
  )
}
scenario_linetype_scale <- function() {
  scale_linetype_manual(
    name = NULL, values = scenario_linetypes,
    limits = c("BaU", "Def", "Aid"), drop = FALSE
  )
}
ar6_scale <- function() {
  scale_fill_manual(
    name = "AR6 category", values = ar6_colors,
    limits = c("C1", "C2"), drop = FALSE
  )
}
x_scale <- function(start_year = 2020) {
  scale_x_continuous(
    limits = c(start_year, 2050),
    breaks = seq(start_year, 2050, 5),
    labels = seq(start_year, 2050, 5),
    expand = expansion(mult = c(0, 0.015))
  )
}

# Set at the final 180-mm figure size. Use an unbroken white background and
# no grid lines so the data marks carry the visual hierarchy.
theme_panel <- function() theme_classic(base_size = 7.2, base_family = figure_font) +
  theme(
    text = element_text(colour = "#202020"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    axis.text = element_text(size = 6.2, colour = "#303030"),
    axis.title = element_text(size = 6.8),
    axis.line.x.bottom = element_line(
      colour = "#202020", linewidth = 0.32, lineend = "square"
    ),
    axis.line.y.left = element_line(
      colour = "#202020", linewidth = 0.32, lineend = "square"
    ),
    axis.ticks = element_line(colour = "#606060", linewidth = 0.22),
    axis.ticks.length = grid::unit(1.0, "mm"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    plot.tag = element_text(face = "bold", size = 8.2, colour = "black"),
    plot.tag.position = c(0, 1),
    strip.background = element_blank(),
    legend.title = element_text(face = "plain", size = 6.4),
    legend.text = element_text(size = 6.0),
    legend.key.height = grid::unit(2.8, "mm"),
    legend.key.width = grid::unit(4.2, "mm"),
    legend.spacing.x = grid::unit(0.7, "mm"),
    legend.margin = margin(0, 0, 0, 0),
    plot.margin = margin(5, 4, 4, 5, unit = "mm")
  )

make_ar6_panel <- function(variable, aim_data, y_label, tag,
                           start_year = 2020) {
  range_data <- ar6_box %>%
    filter(Variable == variable, Year >= start_year)
  aim_plot_data <- aim_data %>% filter(Year >= start_year)

  ggplot() +
    geom_hline(yintercept = 0, linewidth = 0.28, colour = "#8A8A8A") +
    geom_ribbon(
      data = range_data,
      aes(Year, ymin = p05, ymax = p95, fill = Category, group = Category),
      alpha = 0.14, colour = NA
    ) +
    geom_ribbon(
      data = range_data,
      aes(Year, ymin = p25, ymax = p75, fill = Category, group = Category),
      alpha = 0.34, colour = NA
    ) +
    geom_line(
      data = filter(range_data, Category == "C1"),
      aes(Year, p50), colour = ar6_colors[["C1"]],
      linewidth = 0.38, linetype = "13"
    ) +
    geom_line(
      data = filter(range_data, Category == "C2"),
      aes(Year, p50), colour = ar6_colors[["C2"]],
      linewidth = 0.38, linetype = "13"
    ) +
    geom_line(
      data = aim_plot_data,
      aes(Year, value, colour = Scenario, linetype = Scenario,
          group = Scenario),
      linewidth = 0.65
    ) +
    geom_point(
      data = filter(aim_plot_data, Year %in% year_breaks),
      aes(Year, value, colour = Scenario, shape = Scenario),
      size = 1.45, stroke = 0.45
    ) +
    ar6_scale() + scenario_scale() + scenario_shape_scale() +
    scenario_linetype_scale() + x_scale(start_year) +
    labs(
      tag = tag, x = NULL, y = y_label
    ) +
    guides(
      fill = guide_legend(order = 1, nrow = 1,
                          override.aes = list(alpha = 0.30)),
      colour = guide_legend(order = 2, nrow = 1,
                            override.aes = list(linewidth = 0.65, size = 1.5)),
      shape = guide_legend(order = 2, nrow = 1),
      linetype = guide_legend(order = 2, nrow = 1)
    ) +
    theme_panel()
}

p_sector_change <- ggplot(
  sector_cumulative_change,
  aes(y = SCENARIO, x = Change, fill = VEMF)
) +
  geom_vline(xintercept = 0, linewidth = 0.32, colour = "#505050") +
  geom_col(
    width = 0.58, position = position_stack(reverse = TRUE),
    colour = "white", linewidth = 0.18
  ) +
  facet_wrap(vars(REMF), ncol = 1, strip.position = "top") +
  scale_fill_manual(
    name = NULL, values = col_emissions,
    limits = CO2Order, breaks = CO2Order, drop = FALSE
  ) +
  scale_x_continuous(labels = label_number(accuracy = 1)) +
  labs(
    tag = "c",
    x = expression("Change in cumulative emissions (Gt CO"[2]*")"),
    y = NULL
  ) +
  guides(fill = guide_legend(order = 3, nrow = 2, byrow = TRUE)) +
  theme_panel() +
  theme(
    panel.grid.major.y = element_blank(),
    strip.text = element_text(face = "bold", size = 6.5, hjust = 0),
    axis.text.y = element_text(face = "bold"),
    plot.margin = margin(5, 4, 4, 5, unit = "mm")
  )

cumulative_plot_data <- cumulative_emissions %>%
  mutate(
    RegionY = if_else(REMF == "Provider", 2, 1),
    PlotY = RegionY + recode(
      as.character(Scenario), "BaU" = 0, "Def" = 0.15, "Aid" = -0.15
    )
  )

cumulative_segments <- cumulative_plot_data %>%
  filter(Scenario != "BaU") %>%
  transmute(
    REMF, Scenario, x = BaU, xend = Cumulative,
    y = RegionY, yend = PlotY
  )

p_cumulative <- ggplot() +
  geom_segment(
    data = cumulative_segments,
    aes(x = x, xend = xend, y = y, yend = yend, colour = Scenario),
    linewidth = 0.65, alpha = 0.62
  ) +
  geom_point(
    data = cumulative_plot_data,
    aes(Cumulative, PlotY, colour = Scenario, shape = Scenario),
    size = 2.15, stroke = 0.55
  ) +
  geom_text(
    data = filter(cumulative_plot_data, Scenario != "BaU"),
    aes(Cumulative, PlotY,
        label = paste0("-", sprintf("%.0f", Reduction), "%")),
    nudge_x = -12, hjust = 1, size = 2.0, colour = "#303030"
  ) +
  scenario_scale() + scenario_shape_scale() +
  scale_x_continuous(
    limits = c(100, 1000), breaks = seq(200, 1000, 200),
    labels = label_number(accuracy = 1), expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = c(1, 2), labels = c("Recipient", "Provider"),
    limits = c(0.55, 2.45), expand = c(0, 0)
  ) +
  labs(
    tag = "d",
    x = expression("Cumulative emissions (Gt CO"[2]*")"), y = NULL
  ) +
  guides(colour = "none", shape = "none") +
  theme_panel() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(face = "bold"),
    plot.margin = margin(5, 5, 4, 4, unit = "mm")
  )

# Four panels with deliberately asymmetric widths: the analytical panels (a,
# c) carry the main comparison, while b and d serve as compact companions.
p_global <- make_ar6_panel(
  "Emissions|CO2", filter(aim_emissions, REMF == "World"),
  expression("CO"[2]~"emissions (Gt CO"[2]~"yr"^{-1}*")"), "a"
)
p_price <- make_ar6_panel(
  "Price|Carbon", aim_price,
  expression("Carbon price (US$ t CO"[2]^{-1}*")"), "b", 2030
) + guides(fill = "none", colour = "none", shape = "none", linetype = "none")

# Individual outputs ---------------------------------------------------------
save_figure <- function(plot_object, name, width_mm = 180, height_mm = 150) {
  output_stem <- file.path(output_dir, name)
  ggsave(
    paste0(output_stem, ".png"), plot_object,
    width = width_mm, height = height_mm, units = "mm", dpi = 300,
    bg = "white", limitsize = FALSE,
    device = grDevices::png, type = "cairo-png"
  )
  ggsave(
    paste0(output_stem, ".pdf"), plot_object,
    width = width_mm, height = height_mm, units = "mm",
    bg = "white", limitsize = FALSE, device = grDevices::cairo_pdf
  )
  ggsave(
    paste0(output_stem, ".svg"), plot_object,
    width = width_mm, height = height_mm, units = "mm",
    bg = "white", limitsize = FALSE,
    device = function(filename, ...) {
      grDevices::svg(filename = filename, family = figure_font, onefile = TRUE, ...)
    }
  )
}

# Asymmetric 2:1 layout -------------------------------------------------------
top_row <- p_global + p_price + plot_layout(widths = c(1.8, 1))
bottom_row <- p_sector_change + p_cumulative + plot_layout(widths = c(1.8, 1))

figure_composite <- (top_row / bottom_row) +
  plot_layout(guides = "collect", heights = c(1.03, 1)) &
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.justification = "center",
    plot.background = element_rect(fill = "white", colour = NA)
  )

# Nature Portfolio double-column size, with editable PDF/SVG versions.
save_figure(figure_composite, "Emission_CP_sectoral", 180, 145)
