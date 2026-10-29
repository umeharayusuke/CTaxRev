# Change in all Out_* variables between Def and Aid in 2050, AIM17 regions.
# Red cells increase under Aid; blue cells decrease. Numbers show percent
# change relative to Def, keeping small and large output sectors comparable.
# Run from the project root or from Rprog/Ctax_paper_260811.

library_paths <- c(Sys.getenv("R_LIBS_USER"), "C:/ENVI5809/R_library")
library_paths <- library_paths[nzchar(library_paths) & dir.exists(library_paths)]
.libPaths(unique(c(library_paths, .libPaths())))
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(gdxrrw)
})

root <- if (file.exists("data/AR6database/global_17_IAMC.gdx")) "." else "../.."
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
sector_names <- c(
  Out_Agr = "Agriculture", Out_I_S = "Iron &\nsteel",
  Out_Non_Fer_Met = "Non-ferrous\nmetals", Out_Che = "Chemicals",
  Out_Pul_Pap = "Pulp &\npaper", Out_Non_Met_Min = "Non-metallic\nminerals",
  Out_Oth_Ind = "Other\nindustry", Out_Ser = "Services"
)
variables <- names(sector_names)

raw <- rgdx.param(gdx_file, "IAMC_template")
required <- c("SCENARIO", "REMF", "VEMF", "YEMF", "IAMC_Template")
if (!all(required %in% names(raw))) stop("Unexpected IAMC_template columns.")
all_out_variables <- unique(as.character(raw$VEMF)[startsWith(as.character(raw$VEMF), "Out_")])
if (!setequal(all_out_variables, variables)) {
  stop("Out_* variable set changed: ", paste(setdiff(all_out_variables, variables),
                                               collapse = ", "))
}
selected <- raw %>%
  filter(as.character(SCENARIO) %in% unname(scenarios),
         as.character(REMF) %in% regions,
         as.character(VEMF) %in% variables,
         as.character(YEMF) == "2050") %>%
  transmute(scenario = names(scenarios)[match(as.character(SCENARIO), scenarios)],
            region = as.character(REMF), variable = as.character(VEMF),
            value = as.numeric(IAMC_Template))
rm(raw)
if (anyDuplicated(selected[c("scenario", "region", "variable")])) {
  stop("Duplicate scenario-region-variable records.")
}
missing <- anti_join(expand_grid(scenario = names(scenarios), region = regions,
                                 variable = variables), selected,
                     by = c("scenario", "region", "variable"))
if (nrow(missing)) {
  print(missing)
  stop("Missing 2050 Out_* records.")
}
if (any(!is.finite(selected$value))) stop("Non-finite input values.")
units <- rgdx.set(gdx_file, "VUMAP") %>%
  transmute(variable = as.character(VEMF), unit = as.character(UEMF))
if (!all(units$unit[match(variables, units$variable)] == "billion USD_2010/yr")) {
  stop("Unexpected Out_* units.")
}

change <- selected %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(delta = Aid - Def,
         change_pct = 100 * delta / Def,
         direction = case_when(delta > 0 ~ "Increase under Aid",
                               delta < 0 ~ "Decrease under Aid",
                               TRUE ~ "No change"),
         label = if_else(abs(change_pct) < 0.1,
                         sprintf("%+.2f%%", change_pct),
                         sprintf("%+.1f%%", change_pct)),
         sector = factor(unname(sector_names[variable]), levels = unname(sector_names)),
         region_name = factor(unname(region_names[region]),
                              levels = rev(unname(region_names[regions]))),
         group = factor(if_else(region %in% providers, "Provider", "Recipient"),
                        levels = c("Provider", "Recipient")))
if (any(selected$value <= 0) || any(!is.finite(change$change_pct))) {
  stop("Output values must be positive to calculate the Def-relative change.")
}

direction_colors <- c("Increase under Aid" = "#F3CFCB",
                      "Decrease under Aid" = "#D2E3F0",
                      "No change" = "#ECEFF1")
figure <- ggplot(change, aes(sector, region_name, fill = direction)) +
  geom_tile(width = 0.96, height = 0.86, colour = "white", linewidth = 0.65) +
  geom_text(aes(label = label), colour = "#193747", size = 3.2) +
  facet_grid(group ~ ., scales = "free_y", space = "free_y") +
  scale_fill_manual(values = direction_colors, drop = FALSE,
                    guide = guide_legend(nrow = 1)) +
  scale_x_discrete(position = "top", expand = expansion(add = 0.15)) +
  scale_y_discrete(expand = expansion(add = 0.15)) +
  labs(x = NULL, y = NULL, fill = NULL,
       caption = "Cell value = 100 * (Aid - Def) / Def in 2050. Out_* inputs: billion US$2010/yr.") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        panel.spacing.y = grid::unit(0.35, "lines"),
        axis.text.x = element_text(colour = "#193747", face = "bold", size = 10),
        axis.text.y = element_text(colour = "#193747", size = 10),
        axis.ticks = element_blank(),
        strip.text.y = element_text(angle = 0, colour = "#193747", face = "bold"),
        strip.background = element_rect(fill = "#F1F5F7", colour = NA),
        legend.position = "bottom", legend.text = element_text(size = 10),
        plot.caption = element_text(colour = "#60717D", hjust = 0, size = 9),
        plot.margin = margin(12, 18, 12, 12))

png_file <- file.path(output_dir, "Out_sector_change_Aid_Def_2050_AIM17_percent.png")
svg_file <- file.path(output_dir, "Out_sector_change_Aid_Def_2050_AIM17_percent.svg")
ggsave(png_file, figure, width = 17, height = 11.5, dpi = 300, bg = "white")
grDevices::svg(svg_file, width = 17, height = 11.5, bg = "white")
print(figure)
grDevices::dev.off()
message("Saved: ", normalizePath(png_file, winslash = "/"))
message("Saved: ", normalizePath(svg_file, winslash = "/"))
