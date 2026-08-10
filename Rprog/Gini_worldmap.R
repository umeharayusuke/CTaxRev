library(tidyverse)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(gdxrrw)
library(patchwork)

# =========================
# Settings
# =========================

gdx_file <- "AnalysisExpenditure.gdx"
param_name <- "Gini_exp"

refs <- c(
  "SSP2_BaU_NoCC_No",
  "SSP2_400C_2030CP_base_NoCC_No"
)

Y_target <- 2050
output_dir <- "output"
dir.create(output_dir, showWarnings = FALSE)

# =========================
# Country correspondence table
# =========================

country_map <- tribble(
  ~country_name, ~dot, ~R,
  "Albania", ".", "ALB",
  "Algeria", ".", "DZA",
  "Angola", ".", "AGO",
  "Anguilla", ".", "ANG",
  "Antigua and Barbuda", ".", "ATG",
  "Armenia", ".", "ARM",
  "Aruba", ".", "ABW",
  "Australia", ".", "AUS",
  "Austria", ".", "AUT",
  "Azerbaijan", ".", "AZE",
  "Bahamas, The", ".", "BHS",
  "Bahrain", ".", "BHR",
  "Bangladesh", ".", "BGD",
  "Barbados", ".", "BRB",
  "Belarus", ".", "BLR",
  "Belgium", ".", "BEL",
  "Belize", ".", "BLZ",
  "Benin", ".", "BEN",
  "Bermuda", ".", "BMU",
  "Bhutan", ".", "BTN",
  "Bolivia", ".", "BOL",
  "Bosnia and Herzegovina", ".", "BIH",
  "Botswana", ".", "BWA",
  "Brazil", ".", "BRA",
  "Brunei Darussalam", ".", "BRN",
  "Bulgaria", ".", "BGR",
  "Burkina Faso", ".", "BFA",
  "Burundi", ".", "BDI",
  "Cambodia", ".", "KHM",
  "Cameroon", ".", "CMR",
  "Canada", ".", "CAN",
  "Cape Verde", ".", "CPV",
  "Cayman Islands", ".", "CYM",
  "Central African Republic", ".", "CAF",
  "Chad", ".", "TCD",
  "Chile", ".", "CHL",
  "China", ".", "CHN",
  "Colombia", ".", "COL",
  "Comoros", ".", "COM",
  "Congo, Dem. Rep.", ".", "COD",
  "Congo, Rep.", ".", "COG",
  "Costa Rica", ".", "CRI",
  "Cote d'Ivoire", ".", "CIV",
  "Croatia", ".", "HRV",
  "Curacao", ".", "CUW",
  "Cuba", ".", "CUB",
  "Cyprus", ".", "CYP",
  "Czech Republic", ".", "CZE",
  "Denmark", ".", "DNK",
  "Djibouti", ".", "DJI",
  "Dominica", ".", "DMA",
  "Dominican Republic", ".", "DOM",
  "Ecuador", ".", "ECU",
  "Egypt, Arab Rep.", ".", "EGY",
  "El Salvador", ".", "SLV",
  "Equatorial Guinea", ".", "GNQ",
  "Estonia", ".", "EST",
  "Ethiopia", ".", "ETH",
  "Fiji", ".", "FJI",
  "Finland", ".", "FIN",
  "France", ".", "FRA",
  "Gabon", ".", "GAB",
  "Gambia, The", ".", "GMB",
  "Georgia", ".", "GEO",
  "Germany", ".", "DEU",
  "Ghana", ".", "GHA",
  "Greece", ".", "GRC",
  "Grenada", ".", "GRD",
  "Guatemala", ".", "GTM",
  "Guinea", ".", "GIN",
  "Guinea-Bissau", ".", "GNB",
  "Haiti", ".", "HTI",
  "Honduras", ".", "HND",
  "Hong Kong SAR, China", ".", "HKG",
  "Hungary", ".", "HUN",
  "Iceland", ".", "ISL",
  "India", ".", "IND",
  "Indonesia", ".", "IDN",
  "Iran, Islamic Rep.", ".", "IRN",
  "Iraq", ".", "IRQ",
  "Ireland", ".", "IRL",
  "Israel", ".", "ISR",
  "Italy", ".", "ITA",
  "Jamaica", ".", "JAM",
  "Japan", ".", "JPN",
  "Jordan", ".", "JOR",
  "Kazakhstan", ".", "KAZ",
  "Kenya", ".", "KEN",
  "Korea, Rep.", ".", "KOR",
  "Kuwait", ".", "KWT",
  "Kyrgyzstan", ".", "KGZ",
  "Lao PDR", ".", "LAO",
  "Latvia", ".", "LVA",
  "Lesotho", ".", "LSO",
  "Liberia", ".", "LBR",
  "Lithuania", ".", "LTU",
  "Luxembourg", ".", "LUX",
  "Macao SAR, China", ".", "MAC",
  "Macedonia, FYR", ".", "MKD",
  "Madagascar", ".", "MDG",
  "Malawi", ".", "MWI",
  "Malaysia", ".", "MYS",
  "Maldives", ".", "MDV",
  "Mali", ".", "MLI",
  "Malta", ".", "MLT",
  "Mauritania", ".", "MRT",
  "Mauritius", ".", "MUS",
  "Mexico", ".", "MEX",
  "Moldova", ".", "MDA",
  "Mongolia", ".", "MNG",
  "Montenegro", ".", "MNE",
  "Montserrat", ".", "MON",
  "Morocco", ".", "MAR",
  "Mozambique", ".", "MOZ",
  "Myanmar", ".", "MMR",
  "Namibia", ".", "NAM",
  "Nepal", ".", "NPL",
  "Netherlands", ".", "NLD",
  "New Zealand", ".", "NZL",
  "Nicaragua", ".", "NIC",
  "Niger", ".", "NER",
  "Nigeria", ".", "NGA",
  "Norway", ".", "NOR",
  "Oman", ".", "OMN",
  "Pakistan", ".", "PAK",
  "Palestinian Territory", ".", "PLS",
  "Panama", ".", "PAN",
  "Paraguay", ".", "PRY",
  "Peru", ".", "PER",
  "Philippines", ".", "PHL",
  "Poland", ".", "POL",
  "Portugal", ".", "PRT",
  "Qatar", ".", "QAT",
  "Romania", ".", "ROU",
  "Russian Federation", ".", "RUS",
  "Rwanda", ".", "RWA",
  "Sao Tome and Principe", ".", "STP",
  "Saudi Arabia", ".", "SAU",
  "Senegal", ".", "SEN",
  "Serbia", ".", "SRB",
  "Seychelles", ".", "SYC",
  "Sierra Leone", ".", "SLE",
  "Singapore", ".", "SGP",
  "Sint Maarten", ".", "MAF",
  "Slovakia", ".", "SVK",
  "Slovenia", ".", "SVN",
  "South Africa", ".", "ZAF",
  "Spain", ".", "ESP",
  "Sri Lanka", ".", "LKA",
  "St. Kitts and Nevis", ".", "KNA",
  "St. Lucia", ".", "LCA",
  "St. Vincent and the Grenadines", ".", "VCT",
  "Sudan", ".", "SDN",
  "Suriname", ".", "SUR",
  "Swaziland", ".", "SWZ",
  "Sweden", ".", "SWE",
  "Switzerland", ".", "CHE",
  "Taiwan, China", ".", "TWN",
  "Tajikistan", ".", "TJK",
  "Tanzania", ".", "TZA",
  "Thailand", ".", "THA",
  "Togo", ".", "TGO",
  "Trinidad and Tobago", ".", "TTO",
  "Tunisia", ".", "TUN",
  "Turkey", ".", "TUR",
  "Turks and Caicos Islands", ".", "TCA",
  "Uganda", ".", "UGA",
  "Ukraine", ".", "UKR",
  "United Arab Emirates", ".", "ARE",
  "United Kingdom", ".", "GBR",
  "United States", ".", "USA",
  "Uruguay", ".", "URY",
  "Venezuela, RB", ".", "VEN",
  "Vietnam", ".", "VNM",
  "Virgin Islands, British", ".", "VIR",
  "Yemen", ".", "YEM",
  "Zambia", ".", "ZMB",
  "Zimbabwe", ".", "ZWE"
) %>%
  select(country_name, R)

# =========================
# Load GDX data
# =========================

gini_exp <- rgdx.param(
  gdx_file,
  param_name,
  names = c("Ref", "R", "Y", "Value"),
  compress = FALSE
) %>%
  as_tibble() %>%
  mutate(
    Y = as.numeric(as.character(Y)),
    Value = as.numeric(Value)
  ) %>%
  filter(
    Ref %in% refs,
    Y == Y_target
  )

# =========================
# World map
# =========================

world <- ne_countries(
  scale = "medium",
  returnclass = "sf"
) %>%
  filter(name != "Antarctica") %>%
  mutate(
    iso3 = case_when(
      iso_a3_eh == "-99" ~ iso_a3,
      TRUE ~ iso_a3_eh
    )
  )

plot_data <- world %>%
  left_join(gini_exp, by = c("iso3" = "R")) %>%
  filter(!is.na(Ref))

# =========================
# Plot
# =========================

p <- ggplot(plot_data) +
  geom_sf(
    aes(fill = Value),
    color = "grey40",
    linewidth = 0.1
  ) +
  facet_wrap(
    ~ Ref,
    ncol = 1
  ) +
  scale_fill_gradient(
    low = "white",
    high = "darkred",
    na.value = "grey85",
    name = "Gini_exp"
  ) +
  coord_sf(
    xlim = c(-180, 180),
    ylim = c(-60, 85),
    expand = FALSE
  ) +
  labs(
    title = paste0("Gini_exp in ", Y_target),
    subtitle = "Darker color indicates higher Gini_exp"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    axis.title = element_blank(),
    axis.text = element_blank()
  )

print(p)

ggsave(
  filename = file.path(paste0("../../output/Gini_exp_worldmap_", Y_target, ".png")),
  plot = p,
  width = 12,
  height = 9,
  dpi = 300
)

# =========================
# Difference map
# =========================

diff_data <- gini_exp %>%
  select(Ref, R, Y, Value) %>%
  pivot_wider(
    names_from = Ref,
    values_from = Value
  ) %>%
  mutate(
    diff = `SSP2_400C_2030CP_base_NoCC_No` - `SSP2_BaU_NoCC_No`
  )

plot_diff <- world %>%
  left_join(diff_data, by = c("iso3" = "R"))

p_diff <- ggplot(plot_diff) +
  geom_sf(
    aes(fill = diff),
    color = "grey40",
    linewidth = 0.1
  ) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    na.value = "grey85",
    name = "Difference"
  ) +
  coord_sf(
    xlim = c(-180, 180),
    ylim = c(-60, 85),
    expand = FALSE
  ) +
  labs(
    title = paste0("Difference in Gini_exp in ", Y_target),
    subtitle = "SSP2_400C_2030CP_base_NoCC_No - SSP2_BaU_NoCC_No"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.title = element_blank(),
    axis.text = element_blank()
  )

print(p_diff)

ggsave(
  filename = file.path(paste0("../../output/Gini_exp_difference_worldmap_", Y_target, ".png")),
  plot = p_diff,
  width = 12,
  height = 6,
  dpi = 300
)