# Transfer–recovery scatter plot ---------------------------------------
library(tidyverse)
library(ggplot2)
library(gdxrrw)
library(scales)

gdx_file <- "global_17_IAMC.gdx"
output_dir <- "../../output/Figure"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

scenario_def <- "SSP2_400C_2030CP_NoCC_No"
scenario_aid <- "SSP2_400C_2030CP_15th_NoCC_No"
scenario_bau <- "SSP2_BaU_NoCC_No"
start_year <- 2030
end_year <- 2050

providers <- c("XE25","JPN","TUR","CHN","USA","XER","XOC","CAN","XLM","CIS")
recipients <- c("XSA","IND","XNF","XAF","XSE","BRA","XME")
all_regions <- c(providers, recipients)

provider_color <- "#356C9B"
recipient_color <- "#D27755"

theme_paper <- theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#E5E5E5", linewidth = 0.3),
    #plot.title = element_text(face = "bold", size = 14),
    #plot.subtitle = element_text(color = "#555555"),
    axis.title = element_text(face = "plain"),
    legend.position = "bottom",
    #legend.title = element_text(face = "bold"),
    plot.caption = element_text(color = "#666666", hjust = 0)
  )

trapz_sum <- function(year, value) {
  ok <- is.finite(year) & is.finite(value)
  year <- year[ok]
  value <- value[ok]
  if (length(year) < 2) return(NA_real_)
  o <- order(year)
  year <- year[o]
  value <- value[o]
  sum(diff(year) * (head(value, -1) + tail(value, -1)) / 2)
}

if (!file.exists(gdx_file)) {
  stop("Place global_17_IAMC.gdx in the working directory.")
}

iamc <- rgdx.param(gdx_file, "IAMC_template") %>%
  mutate(
    Year = as.numeric(as.character(YEMF)),
    IAMC_Template = as.numeric(IAMC_Template)
  )

# Provider transfers
provider_annual <- iamc %>%
  filter(
    VEMF == "Rev_gov_Tax_Car_Tax",
    SCENARIO == scenario_aid,
    REMF %in% providers,
    Year >= start_year,
    Year <= end_year
  ) %>%
  transmute(Year, REMF, Transfer = IAMC_Template)

total_annual <- provider_annual %>%
  group_by(Year) %>%
  summarise(
    TotalTransfer = sum(Transfer, na.rm = TRUE),
    .groups = "drop"
  )

# Recipient transfers allocated by GDP_PPP share
recipient_annual <- iamc %>%
  filter(
    VEMF == "GDP_PPP",
    SCENARIO == scenario_aid,
    REMF %in% recipients,
    Year >= start_year,
    Year <= end_year
  ) %>%
  group_by(Year) %>%
  mutate(GDPShare = IAMC_Template / sum(IAMC_Template, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(total_annual, by = "Year") %>%
  transmute(Year, REMF, Transfer = TotalTransfer * GDPShare)

# Cumulative transfer
transfer_cumulative <- bind_rows(
  provider_annual %>%
    group_by(REMF) %>%
    summarise(
      CumTransfer = trapz_sum(Year, Transfer),
      .groups = "drop"
    ) %>%
    mutate(Type = "Provider"),
  recipient_annual %>%
    group_by(REMF) %>%
    summarise(
      CumTransfer = trapz_sum(Year, Transfer),
      .groups = "drop"
    ) %>%
    mutate(Type = "Recipient")
)

# Cumulative GDP_MER for the original Aid denominator and the new BaU denominator
gdp_cumulative <- iamc %>%
  filter(
    VEMF == "GDP_MER",
    SCENARIO %in% c(scenario_aid, scenario_bau),
    REMF %in% all_regions,
    Year >= start_year,
    Year <= end_year
  ) %>%
  mutate(GDPBasis = if_else(SCENARIO == scenario_aid, "Aid", "BaU")) %>%
  group_by(GDPBasis, REMF) %>%
  summarise(
    CumGDP = trapz_sum(Year, IAMC_Template),
    .groups = "drop"
  )

transfer_summary <- transfer_cumulative %>%
  left_join(gdp_cumulative, by = "REMF") %>%
  mutate(
    CumTrillion = CumTransfer / 1e6,
    NetTransferPct = if_else(
      Type == "Provider",
      -100 * CumTransfer / (CumGDP * 1000),
       100 * CumTransfer / (CumGDP * 1000)
    )
  )

# Consumption-loss recovery: sign-reversed Aid − Def
loss <- iamc %>%
  filter(
    VEMF == "Pol_Cos_Cns_Los_rat_NPV_5pc",
    SCENARIO %in% c(scenario_def, scenario_aid),
    REMF %in% all_regions,
    Year == end_year
  ) %>%
  mutate(
    Scenario = case_when(
      SCENARIO == scenario_def ~ "Def",
      SCENARIO == scenario_aid ~ "Aid"
    )
  ) %>%
  group_by(REMF, Scenario) %>%
  summarise(
    Value = mean(IAMC_Template, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = Scenario, values_from = Value) %>%
  filter(is.finite(Def), is.finite(Aid)) %>%
  mutate(Recovery = Def - Aid)

scatter_data <- transfer_summary %>%
  select(GDPBasis, REMF, Type, NetTransferPct, CumTrillion) %>%
  left_join(loss %>% select(REMF, Def, Aid, Recovery), by = "REMF") %>%
  mutate(Type = factor(Type, levels = c("Provider", "Recipient"))) %>%
  filter(
    is.finite(NetTransferPct),
    is.finite(Recovery),
    is.finite(CumTrillion)
  )

make_scatter <- function(gdp_basis) {
  figure <- ggplot(
    filter(scatter_data, GDPBasis == gdp_basis),
    aes(NetTransferPct, Recovery, color = Type, size = CumTrillion)
  ) +
  annotate(
    "rect",
    xmin = -Inf, xmax = Inf,
    ymin = 0, ymax = Inf,
    fill = "#188977", alpha = 0.04
  ) +
  geom_hline(yintercept = 0, color = "#777777", linewidth = 0.45) +
  geom_vline(xintercept = 0, color = "#777777", linewidth = 0.45) +
  geom_point(alpha = 0.88) +
  scale_color_manual(
    name = NULL,
    values = c(
      Provider = provider_color,
      Recipient = recipient_color
    )
  ) +
  scale_size_continuous(
    name = "Cumulative transfer\n(trillion US$2010)",
    range = c(3, 12),
    labels = label_number(accuracy = 0.01)
  ) +
  scale_x_continuous(
    labels = label_number(accuracy = 0.1, suffix = "%")
  ) +
  scale_y_continuous(
    labels = label_number(accuracy = 0.1, suffix = "%")
  ) +
  labs(
    x = paste0("Net cumulative transfer / cumulative ", gdp_basis,
               " GDP (%)\nProviders (negative)                         Recipients (positive)"),
    y = "Recovery in cumulative consumption loss: Def - Aid (%)\nNegative = worse; positive = better"
  ) +
  theme_paper

  if (requireNamespace("ggrepel", quietly = TRUE)) {
    figure <- figure +
      ggrepel::geom_text_repel(
        aes(label = REMF), size = 3.2, show.legend = FALSE,
        box.padding = 0.35, point.padding = 0.25, max.overlaps = Inf
      )
  } else {
    figure <- figure +
      geom_text(aes(label = REMF), size = 3, nudge_y = 0.03,
                check_overlap = TRUE, show.legend = FALSE)
  }
  figure
}

save_scatter <- function(figure, stem) {
  ggsave(file.path(output_dir, paste0(stem, ".png")), figure,
         width = 11, height = 8, dpi = 600, bg = "white")
  if (requireNamespace("svglite", quietly = TRUE)) {
    ggsave(file.path(output_dir, paste0(stem, ".svg")), figure,
           device = svglite::svglite, width = 11, height = 8, bg = "white")
  }
}

figure_aid_gdp <- make_scatter("Aid")
figure_bau_gdp <- make_scatter("BaU")
plot(figure_aid_gdp)
plot(figure_bau_gdp)

# Keep the original filename for the Aid-GDP version; give the BaU version its own name.
save_scatter(figure_aid_gdp, "transfer_recovery_scatter")
save_scatter(figure_bau_gdp, "transfer_recovery_scatter_BaUGDP")
