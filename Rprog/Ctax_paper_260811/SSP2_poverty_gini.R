# SSP2 poverty and national expenditure-Gini comparison: BaU, Def, Aid.
# Run from the project root or Rprog/Ctax_paper_260811.
# The requested data/AnalysisExpenditure.gdx was absent. The AR6database copy
# contains all three exact scenario IDs; the Gini/scenario copies do not.
# Style reference: Soergel et al. (2021), Nature Communications 12, 2342,
# Figs. 2-4: black baseline, red policy, blue redistribution, country difference maps.
# https://doi.org/10.1038/s41467-021-22315-9
# AIM poverty reference: Zhao et al. (2022), Sustainability Science 17, 2513-2528.
# https://doi.org/10.1007/s11625-022-01206-y
# Colors follow the literature's roles, not an exact reproduction of its palette.

library_paths <- c(Sys.getenv("R_LIBS_USER"), "C:/ENVI5809/R_library")
library_paths <- library_paths[nzchar(library_paths) & dir.exists(library_paths)]
.libPaths(unique(c(library_paths, .libPaths())))
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(patchwork)
  library(gdxrrw)
  library(sf)
  library(rnaturalearth)
})

# Settings -----------------------------------------------------------------
root <- if (dir.exists("data/AR6database")) "." else "../.."
gdx_file <- file.path(root, "data/AR6database/AnalysisExpenditure.gdx")
if (!file.exists(gdx_file)) stop("Missing GDX: ", gdx_file)
output_dir <- file.path(root, "output/Figure")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
gams_dirs <- c(Sys.getenv("GAMS_SYSDIR"), "C:/GAMS/34", "C:/GAMS/win64/26.1")
gams_dirs <- gams_dirs[nzchar(gams_dirs) & dir.exists(gams_dirs)]
if (length(gams_dirs)) igdx(gams_dirs[1])

scenario_codes <- c(BaU = "SSP2_BaU_NoCC_No", Def = "SSP2_400C_2030CP_NoCC_No",
                    Aid = "SSP2_400C_2030CP_15th_NoCC_No")
scenario_levels <- names(scenario_codes)
scenario_colors <- c(BaU = "#252525", Def = "#D6604D", Aid = "#2166AC")
plot_years <- seq(2020, 2050, 10)
bar_years <- c(2030, 2040, 2050)
map_year <- 2050
thresholds <- c("pop_2.15", "pop_3.65", "pop_6.85")
threshold_labels <- c("$2.15 / person / day", "$3.65 / person / day", "$6.85 / person / day")
# Thresholds retain the source definitions. No new PPP conversion is applied.

check_grid <- function(data, expected, keys, context) {
  if (anyDuplicated(as.data.frame(data[keys]))) stop("Duplicate keys in ", context, ".")
  missing <- anti_join(expected, data, by = keys)
  if (nrow(missing)) {
    stop("Missing ", nrow(missing), " records in ", context, "; first: ",
         paste(unlist(missing[1, ]), collapse = " / "))
  }
}

# Poverty headcount: use WLD directly, never add country and aggregate rows.
poverty_raw <- rgdx.param(gdx_file, "PoVExp")
poverty <- poverty_raw %>%
  transmute(Scenario_id = as.character(Ref), Region = as.character(R),
            Year = as.numeric(as.character(Y)), Threshold_id = as.character(TH),
            People = as.numeric(PoVExp)) %>%
  filter(Scenario_id %in% unname(scenario_codes), Region == "WLD",
         Year %in% plot_years, Threshold_id %in% thresholds) %>%
  mutate(Scenario = factor(names(scenario_codes)[match(Scenario_id, scenario_codes)],
                            levels = scenario_levels),
         Threshold = factor(Threshold_id, levels = thresholds, labels = threshold_labels),
         Million = People / 1e6)
rm(poverty_raw)
check_grid(poverty, expand_grid(Scenario_id = unname(scenario_codes), Year = plot_years,
                                Threshold_id = thresholds),
           c("Scenario_id", "Year", "Threshold_id"), "world poverty")
if (any(!is.finite(poverty$People) | poverty$People < 0)) stop("Invalid poverty headcount.")

baseline <- poverty %>% filter(Scenario == "BaU") %>%
  select(Year, Threshold_id, BaU_people = People)
poverty <- poverty %>% left_join(baseline, by = c("Year", "Threshold_id")) %>%
  mutate(Additional_people = People - BaU_people,
         Additional_million = Additional_people / 1e6)
additional <- poverty %>% filter(Scenario != "BaU", Year %in% bar_years) %>%
  mutate(Year_plot = factor(Year, levels = bar_years))

# Gini_exp is country-specific and on a 0-1 scale. There is no WLD record.
# Country Ginis must not be averaged to claim a global interpersonal Gini.
gini <- rgdx.param(gdx_file, "Gini_exp") %>%
  transmute(Scenario_id = as.character(Ref), ISO3 = as.character(R),
            Year = as.numeric(as.character(Y)), Gini = as.numeric(Gini_exp)) %>%
  filter(Scenario_id %in% unname(scenario_codes), Year == map_year) %>%
  mutate(Scenario = names(scenario_codes)[match(Scenario_id, scenario_codes)])
country_codes <- sort(unique(gini$ISO3))
check_grid(gini, expand_grid(Scenario_id = unname(scenario_codes), ISO3 = country_codes,
                            Year = map_year),
           c("Scenario_id", "ISO3", "Year"), "country Gini")
if (any(!is.finite(gini$Gini) | gini$Gini < 0 | gini$Gini > 1)) {
  stop("Gini_exp must be finite and on the 0-1 scale.")
}
gini_changes <- gini %>% select(ISO3, Year, Scenario, Gini) %>%
  pivot_wider(names_from = Scenario, values_from = Gini) %>%
  mutate(`Def - BaU` = 100 * (Def - BaU), `Aid - BaU` = 100 * (Aid - BaU),
         `Aid - Def` = 100 * (Aid - Def))
comparisons <- c("Def - BaU", "Aid - BaU", "Aid - Def")
gini_long <- gini_changes %>%
  pivot_longer(all_of(comparisons), names_to = "Comparison", values_to = "Gini_change") %>%
  mutate(Comparison = factor(Comparison, levels = comparisons))

# Natural Earth medium resolution is provided by the installed data package.
# Match the available ISO field explicitly, including France and Norway.
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin != "Antarctica") %>%
  mutate(ISO3 = case_when(iso_a3_eh %in% country_codes ~ iso_a3_eh,
                          iso_a3 %in% country_codes ~ iso_a3,
                          adm0_a3 %in% country_codes ~ adm0_a3,
                          TRUE ~ NA_character_)) %>%
  select(ISO3, geometry)
unmapped <- setdiff(country_codes, world$ISO3)
if (length(unmapped)) stop("Gini countries missing from the map: ", paste(unmapped, collapse = ", "))
map_data <- bind_rows(lapply(comparisons, function(comparison) {
  world %>% left_join(filter(gini_long, Comparison == comparison) %>%
                       select(ISO3, Gini_change), by = "ISO3") %>%
    mutate(Comparison = factor(comparison, levels = comparisons))
}))

# Identical bins in all maps. The neutral bin avoids magnifying numerical noise.
bin_breaks <- c(-Inf, -5, -2, -1, -0.2, 0.2, 1, 2, 5, Inf)
bin_labels <- c("<= -5", "-5 to -2", "-2 to -1", "-1 to -0.2", "-0.2 to 0.2",
                "0.2 to 1", "1 to 2", "2 to 5", "> 5")
map_palette <- setNames(c("#053061", "#2166AC", "#4393C3", "#92C5DE", "#F7F7F7",
                          "#F4A582", "#D6604D", "#B2182B", "#67001F", "#D7D7D7"),
                        c(bin_labels, "No data"))
map_data <- map_data %>%
  mutate(Change_class = as.character(cut(Gini_change, breaks = bin_breaks,
                                         labels = bin_labels, right = TRUE)),
         Change_class = factor(replace_na(Change_class, "No data"), levels = names(map_palette)))

# Plot ---------------------------------------------------------------------
theme_paper <- theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#EAEAEA", linewidth = 0.3),
        panel.border = element_rect(color = "#A7A7A7", linewidth = 0.45),
        strip.background = element_rect(fill = "#F3F3F3", color = "#D0D0D0", linewidth = 0.4),
        strip.text = element_text(face = "bold", size = 11),
        plot.title = element_text(face = "bold", size = 15, hjust = 0),
        axis.text = element_text(color = "#303030"),
        legend.position = "bottom", legend.title = element_blank(),
        plot.margin = margin(8, 10, 8, 10))

# Linear absolute counts, in millions, with zero included in every facet.
# Blue dashed line and open circle remain distinguishable when Aid overlaps Def.
p_a <- ggplot(poverty, aes(Year, Million, color = Scenario, linetype = Scenario, shape = Scenario)) +
  geom_line(linewidth = 0.9) + geom_point(size = 2.8, stroke = 0.85) +
  facet_wrap(~Threshold, nrow = 1, scales = "free_y") +
  scale_color_manual(values = scenario_colors, drop = FALSE) +
  scale_linetype_manual(values = c(BaU = "solid", Def = "solid", Aid = "22")) +
  scale_shape_manual(values = c(BaU = 16, Def = 17, Aid = 1)) +
  scale_x_continuous(breaks = plot_years) +
  scale_y_continuous(labels = label_number(big.mark = ","), limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(title = "a  Global poverty headcount", x = NULL, y = "Population in poverty (million)") +
  theme_paper

p_b <- ggplot(additional, aes(Year_plot, Additional_million, fill = Scenario)) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "#555555") +
  geom_col(position = position_dodge(width = 0.76), width = 0.68) +
  geom_text(aes(label = sprintf("%.1f", Additional_million)),
             position = position_dodge(width = 0.76), vjust = -0.4, size = 3.2) +
  facet_wrap(~Threshold, nrow = 1, scales = "free_y") +
  scale_fill_manual(values = scenario_colors, guide = "none") +
  scale_y_continuous(labels = label_number(big.mark = ","),
                     expand = expansion(mult = c(0.02, 0.14))) +
  labs(title = "b  Additional poverty relative to BaU", x = "Year",
       y = "Additional population (million)") + theme_paper

p_c <- ggplot(map_data) +
  geom_sf(aes(fill = Change_class), color = "#A1A1A1", linewidth = 0.1, show.legend = TRUE) +
  facet_wrap(~Comparison, nrow = 1) +
  scale_fill_manual(values = map_palette, drop = FALSE,
                     name = "Change in expenditure Gini (0-100 index points)") +
  coord_sf(crs = "+proj=robin", datum = NA, expand = FALSE) +
  labs(title = paste0("c  National expenditure-Gini changes in ", map_year)) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE, title.position = "top")) +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0),
        strip.text = element_text(face = "bold", size = 12),
        legend.position = "bottom", legend.title = element_text(size = 11),
        legend.text = element_text(size = 10), legend.key.width = grid::unit(0.8, "cm"),
        plot.margin = margin(8, 10, 8, 10))

figure <- (p_a / p_b / p_c) + plot_layout(heights = c(1.1, 1, 1.12))
save_pair <- function(plot, stem, width, height) {
  ggsave(file.path(output_dir, paste0(stem, ".png")), plot,
         width = width, height = height, dpi = 300, bg = "white")
  grDevices::svg(file.path(output_dir, paste0(stem, ".svg")),
                 width = width, height = height, bg = "white")
  print(plot)
  grDevices::dev.off()
}
save_pair(figure, "SSP2_poverty_gini", 16, 12.2)
save_pair(p_c, "SSP2_gini_difference_maps_2050", 16, 4.8)

# Export exact plot values and report near-identical Def/Aid results.
write.csv(poverty %>% select(Scenario, Year, Threshold_id, People, Million,
                             Additional_people, Additional_million),
           file.path(output_dir, "SSP2_poverty_plot_data.csv"), row.names = FALSE)
write.csv(gini_changes, file.path(output_dir, "SSP2_gini_changes_2050.csv"), row.names = FALSE)
poverty_gap <- poverty %>% select(Scenario, Year, Threshold_id, People) %>%
  pivot_wider(names_from = Scenario, values_from = People) %>%
  mutate(Aid_minus_Def_people = Aid - Def)
cat("Input: ", normalizePath(gdx_file, winslash = "/"), "\n", sep = "")
cat("Mapped Gini countries: ", length(country_codes), "\n", sep = "")
cat("Maximum absolute Aid-Def poverty difference (people): ",
    max(abs(poverty_gap$Aid_minus_Def_people)), "\n", sep = "")
cat("Maximum absolute Aid-Def Gini difference (0-100 points): ",
    max(abs(gini_changes$`Aid - Def`)), "\n", sep = "")
print(poverty %>% filter(Year == map_year) %>%
        select(Scenario, Threshold_id, Million, Additional_million))
message("Saved SSP2_poverty_gini and SSP2_gini_difference_maps_2050 (PNG/SVG), and plot-data CSVs.")
