# 2050 consumption-loss rate above 17-region changes in trade balance and
# industrial value added. Independent of the existing two-panel figures.
# Run from the project root or Rprog/Ctax_paper_260811.

library_paths <- c(Sys.getenv("R_LIBS_USER"), "C:/ENVI5809/R_library")
library_paths <- library_paths[nzchar(library_paths) & dir.exists(library_paths)]
.libPaths(unique(c(library_paths, .libPaths())))
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  library(gdxrrw)
})

root <- if (dir.exists("data/AR6database")) "." else "../.."
gdx_file <- file.path(root, "data/AR6database/global_17_IAMC.gdx")
if (!file.exists(gdx_file)) stop("Missing GDX: ", gdx_file)
output_dir <- file.path(root, "output/Figure")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
gams_dirs <- c(Sys.getenv("GAMS_SYSDIR"), "C:/GAMS/34", "C:/GAMS/win64/26.1")
gams_dirs <- gams_dirs[nzchar(gams_dirs) & dir.exists(gams_dirs)]
if (length(gams_dirs)) igdx(gams_dirs[1])

scenarios <- c(Def = "SSP2_400C_2030CP_NoCC_No",
               Aid = "SSP2_400C_2030CP_15th_NoCC_No")
providers <- c("XE25", "JPN", "USA", "XER", "XOC", "CAN", "TUR", "XLM", "CHN", "CIS")
recipients <- c("XME", "XNF", "BRA", "XAF", "XSE", "IND", "XSA")
regions <- c(providers, recipients)
region_names <- c(
  XE25 = "EU25", JPN = "Japan", USA = "United States", XER = "Rest of Europe",
  XOC = "Oceania", CAN = "Canada", TUR = "Turkiye", XLM = "Latin America",
  CHN = "China", CIS = "Former Soviet Union", XME = "Middle East",
  XNF = "North Africa", BRA = "Brazil", XAF = "Rest of Africa",
  XSE = "Southeast Asia", IND = "India", XSA = "Rest of Asia"
)
loss_code <- "Pol_Cos_Cns_Los_rat_NPV_5pc"
gdp_loss_code <- "Pol_Cos_GDP_Los_rat_NPV_5pc"
trade_code <- "Trd_Goo_Val"
industry_code <- "Val_Add_Ind"
agriculture_code <- "Val_Add_Agr"
services_code <- "Val_Add_Ser"
variables <- c(loss_code, gdp_loss_code, trade_code, industry_code,
               agriculture_code, services_code)
scenario_colors <- c(Def = "#6C7B86", Aid = "#167985")
group_colors <- c(Provider = "#315D88", Recipient = "#D88937")
ink <- "#193747"

raw <- rgdx.param(gdx_file, "IAMC_template")
required <- c("SCENARIO", "REMF", "VEMF", "YEMF", "IAMC_Template")
if (!all(required %in% names(raw))) stop("Unexpected IAMC_template columns.")
selected <- raw %>%
  filter(as.character(SCENARIO) %in% unname(scenarios),
         as.character(REMF) %in% regions,
         as.character(VEMF) %in% variables,
         as.character(YEMF) == "2050") %>%
  transmute(scenario = names(scenarios)[match(as.character(SCENARIO), scenarios)],
            region = as.character(REMF), variable = as.character(VEMF),
            value = as.numeric(IAMC_Template))
rm(raw)
expected <- expand_grid(scenario = names(scenarios), region = regions, variable = variables)
missing <- anti_join(expected, selected, by = c("scenario", "region", "variable"))
if (nrow(missing)) {
  print(missing)
  stop("Missing 2050 scenario-region-variable records.")
}
if (anyDuplicated(selected[c("scenario", "region", "variable")])) {
  stop("Duplicate scenario-region-variable records.")
}
if (any(!is.finite(selected$value))) stop("Non-finite input values.")
units <- rgdx.set(gdx_file, "VUMAP") %>%
  transmute(variable = as.character(VEMF), unit = as.character(UEMF))
expected_units <- c(rep("%/year", 2), rep("billion USD_2010/yr", 4))
if (!identical(units$unit[match(variables, units$variable)], expected_units)) {
  stop("Unexpected GDX units for the selected variables.")
}

selected <- selected %>%
  mutate(region_name = unname(region_names[region]),
         group = if_else(region %in% providers, "Provider", "Recipient"),
         scenario = factor(scenario, levels = c("Def", "Aid")),
         region_x = factor(region_name, levels = unname(region_names[regions])),
         region_y = factor(region_name, levels = rev(unname(region_names[regions]))),
         group = factor(group, levels = c("Provider", "Recipient")))

loss <- selected %>% filter(variable == loss_code)
loss_pairs <- loss %>%
  select(region, region_x, scenario, value) %>%
  pivot_wider(names_from = scenario, values_from = value)
gdp_loss <- selected %>% filter(variable == gdp_loss_code)
gdp_loss_pairs <- gdp_loss %>%
  select(region, region_x, scenario, value) %>%
  pivot_wider(names_from = scenario, values_from = value)
absolute <- selected %>% filter(variable %in% c(trade_code, industry_code,
                                                agriculture_code, services_code)) %>%
  select(region, region_name, region_y, group, variable, scenario, value) %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(delta = Aid - Def,
         sign_flip = sign(Aid) != sign(Def),
         label = paste0(sprintf("%+.0f", delta),
                        if_else(variable == trade_code & sign_flip, "*", "")))
axis_limit <- ceiling(max(abs(absolute$delta)) * 1.20 / 100) * 100

theme_paper <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "#E4EAEC", linewidth = 0.35),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(face = "bold", color = ink),
        axis.text = element_text(color = ink),
        axis.title = element_text(color = ink),
        plot.margin = margin(8, 12, 8, 8))

p_loss <- ggplot() +
  annotate("rect", xmin = 0.5, xmax = length(providers) + 0.5,
           ymin = 0, ymax = Inf, fill = "#EDF5F8") +
  annotate("rect", xmin = 0.5, xmax = length(providers) + 0.5,
           ymin = -Inf, ymax = 0, fill = "#F7FAFB") +
  annotate("rect", xmin = length(providers) + 0.5, xmax = length(regions) + 0.5,
           ymin = 0, ymax = Inf, fill = "#FFF5EC") +
  annotate("rect", xmin = length(providers) + 0.5, xmax = length(regions) + 0.5,
           ymin = -Inf, ymax = 0, fill = "#EEF7F3") +
  geom_hline(yintercept = 0, colour = "#71848D", linewidth = 0.6) +
  geom_vline(xintercept = length(providers) + 0.5, linetype = "dotted",
             colour = "#71848D", linewidth = 0.7) +
  geom_segment(data = loss_pairs,
               aes(x = region_x, xend = region_x, y = Def, yend = Aid),
               colour = "#9BA8AE", linetype = "dashed", linewidth = 0.65) +
  geom_point(data = loss, aes(x = region_x, y = value, colour = scenario), size = 2.7) +
  scale_colour_manual(values = scenario_colors, name = NULL) +
  scale_y_continuous(labels = scales::label_number(accuracy = 1, suffix = "%")) +
  labs(title = "a  Cumulative consumption loss rate", x = NULL,
       y = "Loss rate (%)") +
  theme_paper +
  theme(axis.text.x = element_text(angle = 48, hjust = 1, vjust = 1, size = 9),
        panel.grid.major.x = element_blank(), legend.position = "bottom")

make_absolute_panel <- function(data, title, show_region = TRUE) {
  p <- ggplot(data, aes(x = delta, y = region_y, fill = group)) +
    geom_vline(xintercept = 0, colour = "#586779", linewidth = 0.5) +
    geom_hline(yintercept = length(recipients) + 0.5, colour = "#B7C1CB",
               linetype = "dashed", linewidth = 0.4) +
    geom_col(width = 0.62, show.legend = FALSE) +
    geom_text(aes(label = label, hjust = if_else(delta < 0, 1.12, -0.12)), size = 2.8) +
    scale_fill_manual(values = group_colors) +
    scale_x_continuous(limits = c(-axis_limit, axis_limit),
                       labels = scales::label_number(big.mark = ",")) +
    labs(title = title, x = "Aid - Def (billion US$2010/yr)", y = NULL) +
    theme_paper + theme(axis.text.y = element_text(size = 9))
  if (!show_region) p <- p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  p
}
p_trade <- make_absolute_panel(absolute %>% filter(variable == trade_code),
                               "b  Goods trade balance")
p_industry <- make_absolute_panel(absolute %>% filter(variable == industry_code),
                                  "c  Industrial value added", show_region = FALSE)

figure <- p_loss / (p_trade | p_industry) +
  plot_layout(heights = c(1.0, 1.65)) +
  plot_annotation(
    title = "Consumption losses and economic shifts under Aid in 2050",
    subtitle = "17 AIM regions; Def and Aid are compared in panel a, while panels b-c show Aid minus Def.",
    caption = "Panel a: Pol_Cos_Cns_Los_rat_NPV_5pc (5% NPV). Panels b-c share a common money axis. * indicates a trade-balance sign reversal."
  ) & theme(plot.title = element_text(face = "bold"))

png_file <- file.path(output_dir, "Consumption_trade_industry_17regions_2050.png")
svg_file <- file.path(output_dir, "Consumption_trade_industry_17regions_2050.svg")
ggsave(png_file, figure, width = 16, height = 13, dpi = 300, bg = "white")
grDevices::svg(svg_file, width = 16, height = 13, bg = "white")
print(figure)
grDevices::dev.off()
write.csv(loss %>% transmute(region, region_name, group = as.character(group),
                            scenario = as.character(scenario), loss_rate = value),
          file.path(output_dir, "Consumption_loss_17regions_2050.csv"), row.names = FALSE)
write.csv(absolute %>% transmute(region, region_name, group = as.character(group),
                                 variable, Def, Aid, delta, sign_flip) %>%
            filter(variable %in% c(trade_code, industry_code)),
          file.path(output_dir, "Trade_industry_17regions_2050_combined_values.csv"), row.names = FALSE)
message("Saved: ", normalizePath(png_file, winslash = "/"))
message("Saved: ", normalizePath(svg_file, winslash = "/"))

# Extended composition: keep the original b/c bar figure above, and add a
# compact three-sector matrix in a new output. Cell color is sign only;
# printed numbers retain the absolute USD difference across sectors.
sectoral <- absolute %>%
  filter(variable %in% c(agriculture_code, industry_code, services_code)) %>%
  mutate(sector = case_when(variable == agriculture_code ~ "Agriculture",
                            variable == industry_code ~ "Industry",
                            TRUE ~ "Services"),
         sector = factor(sector, levels = c("Agriculture", "Industry", "Services")),
         direction = if_else(delta < 0, "Lower under Aid", "Higher under Aid"),
         delta_label = if_else(abs(delta) < 1,
                               sprintf("%+.1f", delta), sprintf("%+.0f", delta)))

p_sectoral <- ggplot(sectoral, aes(x = sector, y = region_y)) +
  geom_hline(yintercept = length(recipients) + 0.5, colour = "#B7C1CB",
             linetype = "dashed", linewidth = 0.4) +
  geom_tile(aes(fill = direction), width = 0.94, height = 0.82,
            colour = "white", linewidth = 0.7) +
  geom_text(aes(label = delta_label), size = 2.9, colour = ink) +
  scale_fill_manual(values = c("Lower under Aid" = "#DDEAF2",
                               "Higher under Aid" = "#F6DDD9"), guide = "none") +
  scale_x_discrete(position = "top") +
  labs(title = "c  Sectoral value added: Aid - Def", x = NULL, y = NULL) +
  theme_paper +
  theme(panel.grid = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), axis.text.x = element_text(size = 10))

sectoral_figure <- p_loss / (p_trade | p_sectoral) +
  plot_layout(heights = c(1.0, 1.65), widths = c(1.4, 1)) +
  plot_annotation(
    title = "Consumption losses, trade, and sectoral value added under Aid",
    subtitle = "2050 comparison across 17 AIM regions: Def/Aid levels above, Aid minus Def below.",
    caption = "Panel a uses the 5% NPV loss-rate variable. Panels b-c: billion US$2010/yr. In c, pale red = higher and pale blue = lower under Aid; cell numbers show the amount."
  ) & theme(plot.title = element_text(face = "bold"))

sectoral_png <- file.path(output_dir, "Consumption_trade_sectoral_value_added_17regions_2050.png")
sectoral_svg <- file.path(output_dir, "Consumption_trade_sectoral_value_added_17regions_2050.svg")
ggsave(sectoral_png, sectoral_figure, width = 16, height = 13, dpi = 300, bg = "white")
grDevices::svg(sectoral_svg, width = 16, height = 13, bg = "white")
print(sectoral_figure)
grDevices::dev.off()
write.csv(sectoral %>% transmute(region, region_name, group = as.character(group),
                                sector = as.character(sector), Def, Aid, delta),
          file.path(output_dir, "Sectoral_value_added_17regions_2050.csv"), row.names = FALSE)
message("Saved: ", normalizePath(sectoral_png, winslash = "/"))
message("Saved: ", normalizePath(sectoral_svg, winslash = "/"))

# Expanded figure: show both 5% NPV loss-rate indicators in the top row.
# Their y axes are independent so that changes in each indicator remain legible.
p_gdp_loss <- ggplot() +
  annotate("rect", xmin = 0.5, xmax = length(providers) + 0.5,
           ymin = 0, ymax = Inf, fill = "#EDF5F8") +
  annotate("rect", xmin = 0.5, xmax = length(providers) + 0.5,
           ymin = -Inf, ymax = 0, fill = "#F7FAFB") +
  annotate("rect", xmin = length(providers) + 0.5, xmax = length(regions) + 0.5,
           ymin = 0, ymax = Inf, fill = "#FFF5EC") +
  annotate("rect", xmin = length(providers) + 0.5, xmax = length(regions) + 0.5,
           ymin = -Inf, ymax = 0, fill = "#EEF7F3") +
  geom_hline(yintercept = 0, colour = "#71848D", linewidth = 0.6) +
  geom_vline(xintercept = length(providers) + 0.5, linetype = "dotted",
             colour = "#71848D", linewidth = 0.7) +
  geom_segment(data = gdp_loss_pairs,
               aes(x = region_x, xend = region_x, y = Def, yend = Aid),
               colour = "#9BA8AE", linetype = "dashed", linewidth = 0.65) +
  geom_point(data = gdp_loss, aes(x = region_x, y = value, colour = scenario), size = 2.7) +
  scale_colour_manual(values = scenario_colors, name = NULL) +
  scale_y_continuous(labels = scales::label_number(accuracy = 1, suffix = "%")) +
  labs(title = "b  Cumulative GDP loss rate", x = NULL,
       y = "Loss rate (%)") +
  theme_paper +
  theme(axis.text.x = element_text(angle = 48, hjust = 1, vjust = 1, size = 9),
        panel.grid.major.x = element_blank(), legend.position = "bottom")

top_loss <- (p_loss | p_gdp_loss) + plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
bottom_economy <- (p_trade + labs(title = "c  Goods trade balance")) |
  (p_sectoral + labs(title = "d  Sectoral value added: Aid - Def"))
combined_loss_figure <- top_loss / bottom_economy +
  plot_layout(heights = c(1.05, 1.65))

combined_png <- file.path(output_dir, "Consumption_GDP_trade_sectoral_17regions_2050.png")
combined_svg <- file.path(output_dir, "Consumption_GDP_trade_sectoral_17regions_2050.svg")
ggsave(combined_png, combined_loss_figure, width = 20, height = 14, dpi = 300,
       bg = "white")
grDevices::svg(combined_svg, width = 20, height = 14, bg = "white")
print(combined_loss_figure)
grDevices::dev.off()
write.csv(gdp_loss %>% transmute(region, region_name, group = as.character(group),
                                scenario = as.character(scenario), loss_rate = value),
          file.path(output_dir, "GDP_loss_17regions_2050.csv"), row.names = FALSE)
message("Saved: ", normalizePath(combined_png, winslash = "/"))
message("Saved: ", normalizePath(combined_svg, winslash = "/"))
