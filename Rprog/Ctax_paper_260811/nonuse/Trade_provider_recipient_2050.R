# Trade reported for the Provider/Recipient aggregates in 2050.
# Def and Aid are compared without treating region-level gross trade as
# observed bilateral trade. Run from the project root or this script's folder.

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
regions <- c(Rprovider15th = "Provider", Rrecipient15th = "Recipient")
scenario_colors <- c(Def = "#3B638B", Aid = "#D88937")

# Net energy series are signed: exports minus imports. Avoid overlapping
# aggregates, e.g. solids biomass appears in both primary and secondary lists.
energy_spec <- tibble::tribble(
  ~code,                         ~stage,             ~carrier,
  "Trd_Prm_Ene_Coa_Vol",        "Primary energy",   "Coal",
  "Trd_Prm_Ene_Gas_Vol",        "Primary energy",   "Natural gas",
  "Trd_Prm_Ene_Oil_Vol",        "Primary energy",   "Oil",
  "Trd_Prm_Ene_Bio_Vol",        "Primary energy",   "Biomass",
  "Trd_Sec_Ene_Ele_Vol",        "Secondary energy", "Electricity",
  "Trd_Sec_Ene_Hyd_Vol",        "Secondary energy", "Hydrogen",
  "Trd_Sec_Ene_Liq_Bio_Vol",    "Secondary energy", "Liquid biofuel"
)
gross_spec <- expand_grid(carrier = c("Coal", "Natural gas", "Oil", "Biomass"),
                          direction = c("Exports", "Imports")) %>%
  mutate(fuel_code = c("Coa", "Gas", "Oil", "Bio")[match(carrier,
                                                        c("Coal", "Natural gas", "Oil", "Biomass"))],
         code = paste0("Trd_Prm_Ene_", if_else(direction == "Exports", "Exp", "Imp"),
                       "_", fuel_code, "_Vol")) %>%
  select(code, carrier, direction)
agriculture_spec <- tibble::tribble(
  ~code,                  ~item,
  "Trd_Agr_Cro_Cer",     "Cereals",
  "Trd_Agr_Cro_Oil_Cro", "Oil crops",
  "Trd_Agr_Cro_Sug_Cro", "Sugar crops",
  "Trd_Agr_Liv",         "Livestock"
)
trade_codes <- unique(c(energy_spec$code, gross_spec$code,
                        agriculture_spec$code, "Trd_Goo_Val"))

# The IAMC table has one region dimension, not origin and destination.
# Therefore only the signed *net* energy balance can support a two-block
# exchange inference after confirming that the groups cover World and balance.
raw <- rgdx.param(gdx_file, "IAMC_template")
required <- c("SCENARIO", "REMF", "VEMF", "YEMF", "IAMC_Template")
if (!all(required %in% names(raw))) stop("Unexpected IAMC_template columns.")
trade_inventory <- raw %>%
  filter(as.character(SCENARIO) %in% unname(scenarios),
         as.character(REMF) %in% names(regions),
         as.character(YEMF) == "2050",
         startsWith(as.character(VEMF), "Trd_")) %>%
  transmute(code = as.character(VEMF)) %>% distinct()
selected <- raw %>%
  filter(as.character(SCENARIO) %in% unname(scenarios),
         as.character(REMF) %in% c(names(regions), "World"),
         as.character(VEMF) %in% c(trade_codes, "Pop"),
         as.character(YEMF) == "2050") %>%
  transmute(scenario = names(scenarios)[match(as.character(SCENARIO), scenarios)],
            region_code = as.character(REMF), code = as.character(VEMF),
            value = as.numeric(IAMC_Template))
rm(raw)
if (anyDuplicated(selected[c("scenario", "region_code", "code")])) {
  stop("Duplicate scenario-region-variable records.")
}
if (any(!is.finite(selected$value))) stop("Non-finite trade values.")
units <- rgdx.set(gdx_file, "VUMAP") %>%
  transmute(code = as.character(VEMF), unit = as.character(UEMF))
trade_inventory <- trade_inventory %>% left_join(units, by = "code") %>% arrange(code)
unit_check <- units %>% filter(code %in% trade_codes)
if (nrow(unit_check) != length(trade_codes)) stop("Missing unit definitions.")
if (any(units$unit[match(energy_spec$code, units$code)] != "EJ/yr") ||
    any(units$unit[match(gross_spec$code, units$code)] != "EJ/yr")) {
  stop("Unexpected energy-trade units.")
}

pop <- selected %>% filter(code == "Pop") %>%
  select(scenario, region_code, value) %>%
  pivot_wider(names_from = region_code, values_from = value)
if (nrow(pop) != 2L || any(is.na(pop[c("World", names(regions))]))) {
  stop("Cannot verify Provider/Recipient population coverage.")
}
if (any(abs(pop$Rprovider15th + pop$Rrecipient15th - pop$World) > 1e-4)) {
  stop("Provider and Recipient do not cover World population.")
}

region_grid <- expand_grid(scenario = names(scenarios), region_code = names(regions),
                           code = trade_codes)
regional <- region_grid %>%
  left_join(selected, by = c("scenario", "region_code", "code")) %>%
  left_join(units, by = "code") %>%
  mutate(value = replace_na(value, 0),
         region = unname(regions[region_code]),
         scenario = factor(scenario, levels = names(scenarios)),
         region = factor(region, levels = unname(regions)))

# Require observed records for the main energy balances; a missing code is
# not automatically an observed zero. A missing single region-year zero may
# be supplied by the GDX zero-omission convention only if the counterpart exists.
missing_energy <- expand_grid(scenario = names(scenarios), code = energy_spec$code) %>%
  anti_join(selected %>% filter(region_code %in% names(regions)) %>%
              distinct(scenario, code), by = c("scenario", "code"))
if (nrow(missing_energy)) {
  print(missing_energy)
  stop("Missing energy net-trade variable(s) for a scenario.")
}

energy_audit <- regional %>%
  filter(code %in% energy_spec$code) %>%
  select(scenario, code, region, value) %>%
  pivot_wider(names_from = region, values_from = value) %>%
  left_join(energy_spec, by = "code") %>%
  mutate(balance_error = Provider + Recipient,
         max_side = pmax(abs(Provider), abs(Recipient)),
         balance_tolerance = pmax(0.01, 0.05 * max_side),
         # Symmetrize the two reports; electricity has a small rounding gap.
         net_provider_to_recipient = (Provider - Recipient) / 2,
         unit = "EJ/yr")
if (any(abs(energy_audit$balance_error) > energy_audit$balance_tolerance)) {
  print(energy_audit %>% filter(abs(balance_error) > balance_tolerance))
  stop("Energy net trade does not balance across the two aggregates.")
}

net_plot <- energy_audit %>%
  mutate(stage = factor(stage, levels = c("Primary energy", "Secondary energy")),
         carrier = factor(carrier, levels = rev(unique(energy_spec$carrier))))
p_net <- ggplot(net_plot, aes(x = net_provider_to_recipient, y = carrier, fill = scenario)) +
  geom_vline(xintercept = 0, colour = "#657284", linewidth = 0.55) +
  geom_col(position = position_dodge(width = 0.72), width = 0.62) +
  facet_wrap(~ stage, ncol = 1, scales = "free") +
  scale_fill_manual(values = scenario_colors) +
  scale_x_continuous(expand = expansion(mult = 0.12),
                     labels = scales::label_number(accuracy = 0.01)) +
  labs(title = "Net energy exchange between Provider and Recipient, 2050",
       subtitle = "Left: Recipient -> Provider   |   Right: Provider -> Recipient",
       x = "Net exchange (EJ/yr)", y = NULL, fill = NULL,
       caption = "Derived from the two reported regional net balances, which cancel within 0.01 EJ or 5%. This is net exchange, not observed gross bilateral flows.") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(), strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"), legend.position = "bottom")

gross <- regional %>%
  inner_join(gross_spec, by = "code") %>%
  mutate(signed_EJ = if_else(direction == "Imports", -value, value),
         carrier = factor(carrier, levels = rev(c("Coal", "Natural gas", "Oil", "Biomass"))))
if (any(gross$value < -1e-8)) stop("Gross exports/imports contain negative values.")
p_gross <- ggplot(gross, aes(x = signed_EJ, y = carrier, fill = direction)) +
  geom_vline(xintercept = 0, colour = "#657284", linewidth = 0.45) +
  geom_col(width = 0.64) +
  facet_grid(region ~ scenario) +
  scale_fill_manual(values = c(Exports = "#3B638B", Imports = "#D88937")) +
  scale_x_continuous(labels = scales::label_number(accuracy = 1)) +
  labs(title = "Reported gross primary-energy trade, 2050",
       subtitle = "Exports to the right, imports to the left; all partners are included.",
       x = "Reported gross volume (EJ/yr)", y = NULL, fill = NULL,
       caption = "These totals may include trade within either aggregate. They must not be read as direct Provider-Recipient gross flows.") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank(), strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"), legend.position = "bottom")

goods <- regional %>% filter(code == "Trd_Goo_Val")
agriculture <- regional %>%
  inner_join(agriculture_spec, by = "code") %>%
  mutate(item = factor(item, levels = rev(agriculture_spec$item)))
p_goods <- ggplot(goods, aes(x = value, y = region, fill = scenario)) +
  geom_vline(xintercept = 0, colour = "#657284", linewidth = 0.45) +
  geom_col(position = position_dodge(width = 0.7), width = 0.62) +
  scale_fill_manual(values = scenario_colors) +
  scale_x_continuous(labels = scales::label_number(big.mark = ",")) +
  labs(title = "Goods trade balance", x = "Billion US$2010/yr", y = NULL, fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank(), plot.title = element_text(face = "bold"),
        legend.position = "none")
p_agriculture <- ggplot(agriculture, aes(x = value, y = item, fill = scenario)) +
  geom_vline(xintercept = 0, colour = "#657284", linewidth = 0.45) +
  geom_col(position = position_dodge(width = 0.7), width = 0.62) +
  facet_wrap(~ region, ncol = 2, scales = "free_x") +
  scale_fill_manual(values = scenario_colors) +
  labs(title = "Selected agricultural trade indicators", x = "Million tonnes dry matter/yr",
       y = NULL, fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank(), strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"), legend.position = "bottom")
p_other <- (p_goods / p_agriculture) +
  plot_layout(heights = c(0.8, 1.6)) +
  plot_annotation(
    title = "Other reported trade positions in 2050",
    subtitle = "Regional indicators, not measured bilateral transactions",
    caption = "Agricultural balances and goods value do not sum to zero across these aggregates; no trading partner is assigned."
  ) & theme(plot.title = element_text(face = "bold"))

save_pair <- function(plot, stem, width, height) {
  png_file <- file.path(output_dir, paste0(stem, ".png"))
  svg_file <- file.path(output_dir, paste0(stem, ".svg"))
  ggsave(png_file, plot, width = width, height = height, dpi = 300, bg = "white")
  grDevices::svg(svg_file, width = width, height = height, bg = "white")
  print(plot)
  grDevices::dev.off()
  message("Saved: ", normalizePath(png_file, winslash = "/"))
}
save_pair(p_net, "Trade_provider_recipient_net_energy_2050", 12, 8)
save_pair(p_gross, "Trade_provider_recipient_gross_energy_2050", 13, 8)
save_pair(p_other, "Trade_provider_recipient_other_2050", 13, 9)

write.csv(energy_audit, file.path(output_dir, "Trade_provider_recipient_net_energy_2050.csv"),
          row.names = FALSE)
write.csv(gross, file.path(output_dir, "Trade_provider_recipient_gross_energy_2050.csv"),
          row.names = FALSE)
write.csv(bind_rows(goods %>% mutate(indicator = "Goods trade balance"),
                    agriculture %>% rename(indicator = item)),
          file.path(output_dir, "Trade_provider_recipient_other_2050.csv"),
          row.names = FALSE)
write.csv(trade_inventory, file.path(output_dir, "Trade_reported_variables_2050.csv"),
          row.names = FALSE)
