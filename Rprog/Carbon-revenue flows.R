# Option 2: two alternative visualizations of international carbon-tax revenue flows
# Required files in the working directory:
#   global_17_IAMC.gdx
#   RegionmapRagg.map

library(tidyverse)
library(ggplot2)
library(gdxrrw)
library(scales)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggnewscale)

gdx_file <- "global_17_IAMC.gdx"
region_map_file <- "RegionmapRagg.map"
output_dir <- "option2_output"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

scenario_aid <- "SSP2_400C_2030CP_15th_NoCC_No"
start_year <- 2030
end_year <- 2050
providers <- c("XE25","JPN","TUR","CHN","USA","XER","XOC","CAN","XLM","CIS")
recipients <- c("XSA","IND","XNF","XAF","XSE","BRA","XME")
all_regions <- c(providers, recipients)

provider_colors <- c("XE25"="#0072B2","JPN"="#009E73","TUR"="#56B4E9","CHN"="#6A3D9A","USA"="#1B9E77",
                     "XER"="#377EB8","XOC"="#984EA3","CAN"="#4DAF4A","XLM"="#00A6D6","CIS"="#2F4858")
recipient_colors <- c("XSA"="#D55E00","IND"="#E69F00","XNF"="#F0C541","XAF"="#CC79A7","XSE"="#E41A1C",
                      "BRA"="#A65628","XME"="#F781BF")
provider_blue <- "#2E6F9E"
recipient_orange <- "#D27755"
pool_gold <- "#E6B85C"
ink <- "#252525"

trapz_sum <- function(year, value) {
  ok <- is.finite(year) & is.finite(value)
  year <- year[ok]; value <- value[ok]
  if (length(year) < 2) return(NA_real_)
  o <- order(year); year <- year[o]; value <- value[o]
  sum(diff(year) * (head(value, -1) + tail(value, -1)) / 2)
}

stack_nodes <- function(x) {
  x %>% arrange(desc(CumTrillion)) %>%
    mutate(ymin = cumsum(lag(CumTrillion, default = 0)) - sum(CumTrillion) / 2,
           ymax = cumsum(CumTrillion) - sum(CumTrillion) / 2,
           ymid = (ymin + ymax) / 2)
}

ribbon_polygon <- function(x0, x1, ymin0, ymax0, ymin1, ymax1, region, n = 80) {
  t <- seq(0, 1, length.out = n); s <- 3*t^2 - 2*t^3; x <- x0 + (x1-x0)*t
  lower <- ymin0 + (ymin1-ymin0)*s; upper <- ymax0 + (ymax1-ymax0)*s
  tibble(x = c(x, rev(x)), y = c(lower, rev(upper)), REMF = region)
}

make_ribbons <- function(nodes, x0, x1, pool_scale = 0.72, direction = "to_pool") {
  map_dfr(seq_len(nrow(nodes)), function(i) {
    r <- nodes[i,]
    if (direction == "to_pool") ribbon_polygon(x0, x1, r$ymin, r$ymax, r$ymin*pool_scale, r$ymax*pool_scale, as.character(r$REMF))
    else ribbon_polygon(x0, x1, r$ymin*pool_scale, r$ymax*pool_scale, r$ymin, r$ymax, as.character(r$REMF))
  })
}

if (!file.exists(gdx_file)) stop("Place global_17_IAMC.gdx in the working directory.")
if (!file.exists(region_map_file)) stop("Place RegionmapRagg.map in the working directory.")

iamc <- rgdx.param(gdx_file, "IAMC_template") %>% mutate(Year = as.numeric(as.character(YEMF)))

provider_raw <- iamc %>%
  filter(VEMF == "Rev_gov_Tax_Car_Tax", SCENARIO == scenario_aid, REMF %in% providers,
         Year >= start_year, Year <= end_year) %>%
  transmute(Year, REMF, Transfer = as.numeric(IAMC_Template))

model_years <- sort(unique(provider_raw$Year))
provider_annual <- provider_raw %>%
  group_by(Year, REMF) %>% summarise(Transfer = sum(Transfer, na.rm = TRUE), .groups = "drop") %>%
  complete(Year = model_years, REMF = providers, fill = list(Transfer = 0))

total_annual <- provider_annual %>% group_by(Year) %>%
  summarise(TotalTransfer = sum(Transfer, na.rm = TRUE), .groups = "drop")

recipient_gdp <- iamc %>%
  filter(VEMF == "GDP_PPP", SCENARIO == scenario_aid, REMF %in% recipients,
         Year >= start_year, Year <= end_year) %>%
  transmute(Year, REMF, GDP_PPP = as.numeric(IAMC_Template)) %>%
  group_by(Year, REMF) %>% summarise(GDP_PPP = sum(GDP_PPP, na.rm = TRUE), .groups = "drop") %>%
  complete(Year = model_years, REMF = recipients, fill = list(GDP_PPP = 0)) %>%
  group_by(Year) %>% mutate(TotalGDP_PPP = sum(GDP_PPP, na.rm = TRUE)) %>% ungroup()

if (any(recipient_gdp$TotalGDP_PPP <= 0)) stop("GDP_PPP is missing for all recipient regions in at least one model year.")

recipient_annual <- recipient_gdp %>%
  mutate(GDPShare = GDP_PPP / TotalGDP_PPP) %>%
  left_join(total_annual, by = "Year") %>%
  transmute(Year, REMF, Transfer = TotalTransfer * GDPShare)

provider_cum <- provider_annual %>% group_by(REMF) %>%
  summarise(CumTransfer = trapz_sum(Year, Transfer), .groups = "drop") %>%
  mutate(CumTrillion = CumTransfer / 1e6, Type = "Provider")

recipient_cum <- recipient_annual %>% group_by(REMF) %>%
  summarise(CumTransfer = trapz_sum(Year, Transfer), .groups = "drop") %>%
  mutate(CumTrillion = CumTransfer / 1e6, Type = "Recipient")

pool_total <- sum(provider_cum$CumTrillion, na.rm = TRUE)
recipient_total <- sum(recipient_cum$CumTrillion, na.rm = TRUE)
relative_gap <- abs(pool_total - recipient_total) / max(abs(pool_total), 1e-12)
if (relative_gap > 1e-8)
  warning(paste0("Provider and recipient cumulative totals differ by ", percent(relative_gap, accuracy = 0.001), "."))

# -----------------------------------------------------------------------------
# VERSION B: two R17 world maps connected through the pooled fund
# -----------------------------------------------------------------------------
region_map <- read.table(region_map_file, header = FALSE, stringsAsFactors = FALSE) %>%
  as_tibble() %>% select(iso3c = 1, REMF = 3) %>%
  mutate(iso3c = str_remove_all(iso3c, '"'), REMF = str_remove_all(REMF, '"'), REMF = str_remove(REMF, "^R17")) %>%
  filter(REMF %in% all_regions)

world_raw <- ne_countries(scale = "medium", returnclass = "sf")
# Natural Earth occasionally stores -99 in iso_a3. adm0_a3 and explicit FRA/NOR
# fallbacks prevent France and Norway from being dropped during the ISO3 join.
world_raw$iso3c <- world_raw$iso_a3
world_raw$iso3c[world_raw$iso3c == "-99" | is.na(world_raw$iso3c)] <-
  world_raw$adm0_a3[world_raw$iso3c == "-99" | is.na(world_raw$iso3c)]
world_raw$iso3c[world_raw$admin == "France"] <- "FRA"
world_raw$iso3c[world_raw$admin == "Norway"] <- "NOR"

world <- world_raw %>% select(iso3c, geometry) %>% filter(iso3c != "ATA") %>%
  left_join(region_map, by = "iso3c") %>% st_transform("+proj=robin")

bbox <- st_bbox(world); map_width <- as.numeric(bbox["xmax"] - bbox["xmin"])
shift_x <- map_width * 1.28; fund_x <- shift_x / 2; fund_y <- 0
map_crs <- st_crs(world)

world_right <- world
st_geometry(world_right) <- st_geometry(world_right) + c(shift_x, 0)
st_crs(world_right) <- map_crs

provider_map <- world %>% left_join(provider_cum %>% select(REMF, CumTrillion), by = "REMF") %>% filter(!is.na(CumTrillion))
recipient_map <- world_right %>% left_join(recipient_cum %>% select(REMF, CumTrillion), by = "REMF") %>% filter(!is.na(CumTrillion))

provider_regions_sf <- provider_map %>% select(REMF) %>% group_by(REMF) %>% summarise(do_union = TRUE, .groups = "drop")
recipient_regions_sf <- recipient_map %>% select(REMF) %>% group_by(REMF) %>% summarise(do_union = TRUE, .groups = "drop")
provider_centers_sf <- suppressWarnings(st_point_on_surface(provider_regions_sf)) %>%
  left_join(provider_cum %>% select(REMF, CumTrillion), by = "REMF")
recipient_centers_sf <- suppressWarnings(st_point_on_surface(recipient_regions_sf)) %>%
  left_join(recipient_cum %>% select(REMF, CumTrillion), by = "REMF")

# Manual flow anchors in geographic coordinates. These override automatic
# representative points for geographically dispersed aggregate regions.
anchor_override_ll <- tribble(
  ~REMF, ~lon, ~lat,
  "XLM",  -70, -12,  # Mexico, Central/South America excluding Brazil
  "XSE",  105,  12   # Southeast Asia
)
anchor_override_sf <- st_as_sf(anchor_override_ll, coords = c("lon", "lat"), crs = 4326) %>% st_transform(map_crs)
anchor_override_xy <- bind_cols(st_drop_geometry(anchor_override_sf),
                                as_tibble(st_coordinates(anchor_override_sf), .name_repair = "minimal")) %>%
  transmute(REMF, x_override = X, y_override = Y)

provider_centers <- bind_cols(st_drop_geometry(provider_centers_sf), as_tibble(st_coordinates(provider_centers_sf), .name_repair = "minimal")) %>%
  rename(x = X, y = Y) %>%
  left_join(anchor_override_xy %>% filter(REMF %in% providers), by = "REMF") %>%
  mutate(x = coalesce(x_override, x), y = coalesce(y_override, y), xend = fund_x, yend = fund_y) %>%
  select(-x_override, -y_override)
recipient_centers <- bind_cols(st_drop_geometry(recipient_centers_sf), as_tibble(st_coordinates(recipient_centers_sf), .name_repair = "minimal")) %>%
  rename(x = X, y = Y) %>%
  left_join(anchor_override_xy %>% filter(REMF %in% recipients) %>%
              mutate(x_override = x_override + shift_x), by = "REMF") %>%
  mutate(x = coalesce(x_override, x), y = coalesce(y_override, y), xstart = fund_x, ystart = fund_y) %>%
  select(-x_override, -y_override)

figure2b <- ggplot() +
  geom_sf(data = world, fill = "#F3F3F3", color = "white", linewidth = 0.08) +
  geom_sf(data = provider_map, aes(fill = CumTrillion), color = "white", linewidth = 0.08) +
  scale_fill_gradient(low = "#CFE1EE", high = "#174F78", name = "Provider contribution\n(trillion US$2010)",
                      guide = guide_colorbar(order = 1, title.position = "top")) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = world_right, fill = "#F3F3F3", color = "white", linewidth = 0.08) +
  geom_sf(data = recipient_map, aes(fill = CumTrillion), color = "white", linewidth = 0.08) +
  scale_fill_gradient(low = "#F6D8C9", high = "#A83D2C", name = "Recipient receipt\n(trillion US$2010)",
                      guide = guide_colorbar(order = 2, title.position = "top")) +
  geom_curve(data = provider_centers, aes(x = x, y = y, xend = xend, yend = yend, linewidth = CumTrillion),
             curvature = 0.12, color = provider_blue, alpha = 0.48, lineend = "round", inherit.aes = FALSE) +
  geom_curve(data = recipient_centers, aes(x = xstart, y = ystart, xend = x, yend = y, linewidth = CumTrillion),
             curvature = -0.12, color = recipient_orange, alpha = 0.48, lineend = "round", inherit.aes = FALSE) +
  scale_linewidth_continuous(range = c(0.5, 4.5), guide = "none") +
  geom_point(data = provider_centers, aes(x, y), color = provider_blue, fill = "white", shape = 21, size = 2.5, stroke = 0.8, inherit.aes = FALSE) +
  geom_point(data = recipient_centers, aes(x, y), color = recipient_orange, fill = "white", shape = 21, size = 2.5, stroke = 0.8, inherit.aes = FALSE) +
  geom_text(data = provider_centers, aes(x, y, label = REMF), nudge_y = map_width*0.012, size = 2.7, color = ink, inherit.aes = FALSE) +
  geom_text(data = recipient_centers, aes(x, y, label = REMF), nudge_y = map_width*0.012, size = 2.7, color = ink, inherit.aes = FALSE) +
  annotate("point", x = fund_x, y = fund_y, shape = 21, size = 70, fill = pool_gold, color = "white", stroke = 1.2) +
  annotate("text", x = fund_x, y = fund_y + map_width*0.010, label = "CARBON-REVENUE\nPOOL", fontface = "bold", size = 8, color = ink) +
  annotate("text", x = fund_x, y = fund_y - map_width*0.08,
           label = paste0(number(pool_total, accuracy = 0.01), " trillion US$2010"), size = 6, color = ink) +
  annotate("text", x = 0, y = as.numeric(bbox["ymax"])*0.92, label = "PROVIDER REGIONS", fontface = "bold", color = provider_blue, size = 4) +
  annotate("text", x = shift_x, y = as.numeric(bbox["ymax"])*0.92, label = "RECIPIENT REGIONS", fontface = "bold", color = recipient_orange, size = 4) +
  coord_sf(xlim = c(as.numeric(bbox["xmin"]), as.numeric(bbox["xmax"])+shift_x),
           ylim = c(as.numeric(bbox["ymin"]), as.numeric(bbox["ymax"])), expand = FALSE, datum = NA) +
  labs(title = "Carbon Tax Revenue Transfer Cumulative Flows") +
  theme_void(base_size = 12) +
  theme(legend.position = "bottom", legend.box = "horizontal", plot.title = element_text(face = "bold", size = 15,hjust = 0.5),
        plot.subtitle = element_text(color = "#555555"), plot.caption = element_text(color = "#666666", hjust = 0.5),
        plot.margin = margin(10, 15, 10, 15))

ggsave(file.path(output_dir, "option2b_world_maps_flow.png"), figure2b, width = 18, height = 7.5, dpi = 600)
ggsave(file.path(output_dir, "option2b_world_maps_flow.pdf"), figure2b, width = 18, height = 7.5)

message("Saved Option 2 variants to: ", normalizePath(output_dir))
