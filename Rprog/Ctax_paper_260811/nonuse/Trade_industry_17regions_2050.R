# 2050 Def-to-Aid changes for 17 AIM regions: goods trade balance and
# industrial value added. Run from the project root or this script's folder.

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
region_labels <- c(
  XE25 = "EU25", JPN = "Japan", USA = "United States", XER = "Rest of Europe",
  XOC = "Oceania", CAN = "Canada", TUR = "Turkiye", XLM = "Latin America",
  CHN = "China", CIS = "Former Soviet Union", XME = "Middle East",
  XNF = "North Africa", BRA = "Brazil", XAF = "Rest of Africa",
  XSE = "Southeast Asia", IND = "India", XSA = "Rest of Asia"
)
variables <- c("Trd_Goo_Val", "Val_Add_Ind")
group_colors <- c(Provider = "#315D88", Recipient = "#D88937")

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
grid <- expand_grid(scenario = names(scenarios), region = regions, variable = variables)
missing <- anti_join(grid, selected, by = c("scenario", "region", "variable"))
if (nrow(missing)) {
  print(missing)
  stop("Missing scenario-region-variable observations.")
}
if (anyDuplicated(selected[c("scenario", "region", "variable")])) {
  stop("Duplicate scenario-region-variable observations.")
}
if (any(!is.finite(selected$value))) stop("Non-finite observations.")
units <- rgdx.set(gdx_file, "VUMAP") %>%
  transmute(variable = as.character(VEMF), unit = as.character(UEMF))
if (!all(units$unit[match(variables, units$variable)] == "billion USD_2010/yr")) {
  stop("Unexpected units for trade or value added.")
}

# |Def| keeps the sign intuitive when the Def trade balance is negative.
comparison <- selected %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(delta = Aid - Def,
         change_pct = 100 * delta / abs(Def),
         sign_flip = sign(Aid) != sign(Def),
         group = if_else(region %in% providers, "Provider", "Recipient"),
         region_name = unname(region_labels[region]),
         region_name = factor(region_name, levels = rev(unname(region_labels[regions]))),
         group = factor(group, levels = c("Provider", "Recipient")),
         label = paste0(sprintf("%+.1f%%", change_pct), if_else(sign_flip, "*", "")))
if (any(!is.finite(comparison$change_pct))) stop("Cannot calculate percentage change from zero Def.")

trade <- comparison %>% filter(variable == "Trd_Goo_Val")
industry <- comparison %>% filter(variable == "Val_Add_Ind")
trade_cap <- 150
trade <- trade %>% mutate(plot_pct = pmax(-trade_cap, pmin(trade_cap, change_pct)))

base_theme <- theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 13),
        axis.text.y = element_text(size = 10),
        plot.margin = margin(8, 22, 8, 8))

p_trade <- ggplot(trade, aes(x = plot_pct, y = region_name, fill = group)) +
  geom_vline(xintercept = 0, colour = "#586779", linewidth = 0.5) +
  geom_hline(yintercept = length(recipients) + 0.5, colour = "#B7C1CB",
             linetype = "dashed", linewidth = 0.4) +
  geom_col(width = 0.64, show.legend = FALSE) +
  geom_text(aes(label = label, hjust = if_else(plot_pct < 0, 1.12, -0.12)),
            size = 3.1) +
  scale_fill_manual(values = group_colors) +
  scale_x_continuous(limits = c(-188, 188), breaks = seq(-150, 150, 50),
                     labels = scales::label_number(suffix = "%")) +
  labs(title = "a  Goods trade balance", x = "Change relative to |Def| (%)", y = NULL) +
  base_theme

p_industry <- ggplot(industry, aes(x = change_pct, y = region_name, fill = group)) +
  geom_vline(xintercept = 0, colour = "#586779", linewidth = 0.5) +
  geom_hline(yintercept = length(recipients) + 0.5, colour = "#B7C1CB",
             linetype = "dashed", linewidth = 0.4) +
  geom_col(width = 0.64, show.legend = FALSE) +
  geom_text(aes(label = label, hjust = if_else(change_pct < 0, 1.12, -0.12)),
            size = 3.1) +
  scale_fill_manual(values = group_colors) +
  scale_x_continuous(limits = c(-13.5, 8.5), breaks = c(-10, -5, 0, 5),
                     labels = scales::label_number(suffix = "%")) +
  labs(title = "b  Industrial value added", x = "Change relative to |Def| (%)", y = NULL) +
  base_theme

figure <- (p_trade / p_industry) +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(
    title = "Aid vs Def in 2050 across 17 AIM regions",
    subtitle = "Percentage change = (Aid - Def) / |Def| x 100. Navy: Provider; orange: Recipient.",
    caption = "Trade bars are capped at +/-150% for readability; labels show full values. * indicates a trade-balance sign reversal."
  ) & theme(plot.title = element_text(face = "bold"))

png_file <- file.path(output_dir, "Trade_industry_17regions_2050_percent.png")
svg_file <- file.path(output_dir, "Trade_industry_17regions_2050_percent.svg")
ggsave(png_file, figure, width = 11, height = 15, dpi = 300, bg = "white")
grDevices::svg(svg_file, width = 11, height = 15, bg = "white")
print(figure)
grDevices::dev.off()
write.csv(comparison %>% mutate(region_name = as.character(region_name),
                                group = as.character(group)) %>%
            select(region, region_name, group, variable, Def, Aid, change_pct, sign_flip),
          file.path(output_dir, "Trade_industry_17regions_2050_percent.csv"),
          row.names = FALSE)
message("Saved: ", normalizePath(png_file, winslash = "/"))
message("Saved: ", normalizePath(svg_file, winslash = "/"))

# Absolute differences use a common scale because both reported indicators
# have the same unit (billion USD_2010 per year).
absolute_limit <- ceiling(max(abs(comparison$delta)) * 1.2 / 100) * 100
make_absolute_panel <- function(data, title) {
  data <- data %>%
    mutate(delta_label = paste0(sprintf("%+.0f", delta),
                                if_else(variable == "Trd_Goo_Val" & sign_flip, "*", "")))
  ggplot(data, aes(x = delta, y = region_name, fill = group)) +
    geom_vline(xintercept = 0, colour = "#586779", linewidth = 0.5) +
    geom_hline(yintercept = length(recipients) + 0.5, colour = "#B7C1CB",
               linetype = "dashed", linewidth = 0.4) +
    geom_col(width = 0.64, show.legend = FALSE) +
    geom_text(aes(label = delta_label, hjust = if_else(delta < 0, 1.12, -0.12)),
              size = 3.1) +
    scale_fill_manual(values = group_colors) +
    scale_x_continuous(limits = c(-absolute_limit, absolute_limit),
                       labels = scales::label_number(big.mark = ",")) +
    labs(title = title, x = "Aid - Def (billion US$2010/yr)", y = NULL) +
    base_theme
}
p_trade_absolute <- make_absolute_panel(comparison %>% filter(variable == "Trd_Goo_Val"),
                                        "a  Goods trade balance")
p_industry_absolute <- make_absolute_panel(comparison %>% filter(variable == "Val_Add_Ind"),
                                           "b  Industrial value added")
absolute_figure <- (p_trade_absolute / p_industry_absolute) +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(
    title = "Aid minus Def in 2050 across 17 AIM regions",
    subtitle = "Absolute change in billion US$2010/yr; both panels use the same x-axis scale. Navy: Provider; orange: Recipient.",
    caption = "Positive values are higher under Aid. * indicates that the goods trade balance changes sign between Def and Aid."
  ) & theme(plot.title = element_text(face = "bold"))

absolute_png <- file.path(output_dir, "Trade_industry_17regions_2050_absolute_change.png")
absolute_svg <- file.path(output_dir, "Trade_industry_17regions_2050_absolute_change.svg")
ggsave(absolute_png, absolute_figure, width = 11, height = 15, dpi = 300, bg = "white")
grDevices::svg(absolute_svg, width = 11, height = 15, bg = "white")
print(absolute_figure)
grDevices::dev.off()
write.csv(comparison %>% mutate(region_name = as.character(region_name),
                                group = as.character(group)) %>%
            select(region, region_name, group, variable, Def, Aid, delta, sign_flip),
          file.path(output_dir, "Trade_industry_17regions_2050_absolute_change.csv"),
          row.names = FALSE)
message("Saved: ", normalizePath(absolute_png, winslash = "/"))
message("Saved: ", normalizePath(absolute_svg, winslash = "/"))
