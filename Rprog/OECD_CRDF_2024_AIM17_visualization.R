# Visualise every sheet in data/OECD_CRDF_2024_AIM17_analysis.xlsx
# Outputs: output/OECD_CRDF_2024_AIM17/

if (dir.exists("Rlib")) .libPaths(c(normalizePath("Rlib"), .libPaths()))
required <- c("readxl", "dplyr", "tidyr", "ggplot2", "scales")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install packages: ", paste(missing, collapse = ", "))

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(ggplot2); library(scales)
})

input_file <- file.path("data", "OECD_CRDF_2024_AIM17_analysis.xlsx")
output_dir <- file.path("output", "OECD_CRDF_2024_AIM17")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
stopifnot(file.exists(input_file))

# ---- Visual system --------------------------------------------------------
ink <- "#14213D"; cyan <- "#00A6A6"; coral <- "#F07167"; gold <- "#F2C14E"
blue <- "#2D6CDF"; violet <- "#725AC1"; green <- "#4C956C"; mist <- "#E8EEF5"
palette <- c(cyan, blue, coral, gold, violet, green, "#8D99AE")

theme_aim <- theme_minimal(base_size = 13) +
  theme(text = element_text(colour = ink), plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(colour = "#56657A", margin = margin(b = 12)),
        plot.caption = element_text(colour = "#667085", hjust = 0),
        panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "bottom", legend.title = element_blank(),
        plot.margin = margin(18, 24, 18, 18))

save_plot <- function(p, filename, width = 11, height = 7) {
  ggsave(file.path(output_dir, filename), p, width = width, height = height,
         dpi = 200, bg = "white")
}
fmt_m <- label_number(big.mark = ",", accuracy = 1, suffix = " M")

read_sheet <- function(name, col_names = TRUE) read_excel(input_file, sheet = name, col_names = col_names)
drop_total <- function(x) x %>% filter(!is.na(.data[[1]]), .data[[1]] != "Total")

stacked_chart <- function(data, id_col, total_col, title, subtitle, filename,
                          share = FALSE, exclude_zero = TRUE) {
  z <- data %>% filter(!is.na(.data[[id_col]]))
  if (exclude_zero) z <- z %>% filter(.data[[total_col]] > 0)
  z <- z %>% select(-all_of(total_col)) %>%
    pivot_longer(-all_of(id_col), names_to = "category", values_to = "value") %>%
    mutate(value = replace_na(as.numeric(value), 0))
  ord <- data %>% filter(.data[[id_col]] %in% unique(z[[id_col]])) %>%
    arrange(.data[[total_col]]) %>% pull(all_of(id_col))
  z[[id_col]] <- factor(z[[id_col]], levels = ord)
  p <- ggplot(z, aes(value, .data[[id_col]], fill = category)) +
    geom_col(position = if (share) "fill" else "stack", width = .72) +
    scale_fill_manual(values = rep(palette, length.out = length(unique(z$category)))) +
    labs(x = if (share) "Share of regional total" else "2024 USD million", y = NULL,
         title = title, subtitle = subtitle,
         caption = "Source: OECD_CRDF_2024_AIM17_analysis.xlsx") + theme_aim
  if (share) p <- p + scale_x_continuous(labels = label_percent())
  else p <- p + scale_x_continuous(labels = fmt_m, expand = expansion(mult = c(0, .03)))
  save_plot(p, filename)
}

card_page <- function(title, items, filename, subtitle = NULL) {
  png(file.path(output_dir, filename), width = 2200, height = 1400, res = 200, bg = "white")
  grid::grid.newpage()
  grid::grid.rect(gp = grid::gpar(fill = "white", col = NA))
  grid::grid.rect(x = .02, y = .5, width = .018, height = 1,
                  gp = grid::gpar(fill = cyan, col = NA))
  grid::grid.text(title, x = .07, y = .93, just = "left",
                  gp = grid::gpar(fontsize = 25, fontface = "bold", col = ink))
  if (!is.null(subtitle)) grid::grid.text(subtitle, x = .07, y = .885, just = "left",
    gp = grid::gpar(fontsize = 12, col = "#667085"))
  n <- length(items); ys <- seq(.79, .11, length.out = n)
  for (i in seq_along(items)) {
    grid::grid.roundrect(x = .52, y = ys[i], width = .88, height = min(.105, .63/n), r = unit(.02,"snpc"),
      gp = grid::gpar(fill = if (i %% 2) "#F5F8FC" else "white", col = mist, lwd = 1.5))
    label_text <- paste(strwrap(names(items)[i], width = 26), collapse = "\n")
    value_text <- paste(strwrap(items[[i]], width = 105), collapse = "\n")
    grid::grid.text(label_text, x = .105, y = ys[i], just = "left",
      gp = grid::gpar(fontsize = 10.5, fontface = "bold", col = cyan))
    grid::grid.text(value_text, x = .32, y = ys[i], just = "left",
      gp = grid::gpar(fontsize = 8.8, col = ink))
  }
  dev.off()
}

# ---- README ---------------------------------------------------------------
readme_raw <- read_sheet("README", col_names = FALSE)
readme_items <- readme_raw %>% filter(!is.na(...1), !is.na(...2), ...1 != "Sheets")
items <- setNames(as.character(readme_items$...2), as.character(readme_items$...1))
card_page("Workbook guide", items[seq_len(min(7, length(items)))], "00_README_overview.png",
          "Scope, interpretation and mapping rules")

# ---- Dashboard ------------------------------------------------------------
dash <- read_sheet("Dashboard", col_names = FALSE)
kpis <- dash[3:7, 1:3]; names(kpis) <- c("metric", "value", "note")
kpis$value <- as.numeric(kpis$value)
p_kpi <- kpis %>% filter(metric != "Unallocated share") %>%
  mutate(metric = factor(metric, levels = rev(metric))) %>%
  ggplot(aes(value, metric)) + geom_col(fill = cyan, width = .65) +
  geom_text(aes(label = fmt_m(value)), hjust = -0.08, fontface = "bold", colour = ink) +
  scale_x_continuous(labels = fmt_m, expand = expansion(mult = c(0, .18))) +
  labs(x = "2024 USD million", y = NULL, title = "Dashboard | Core climate-finance totals",
       subtitle = paste0("Unallocated share: ", percent(kpis$value[kpis$metric == "Unallocated share"], accuracy = .1)),
       caption = "AIM17-mapped and unallocated amounts reconcile to total CRDF commitments.") + theme_aim
save_plot(p_kpi, "01_Dashboard_KPI.png", 10, 6)

dash_regions <- dash[11:20, 1:6]; names(dash_regions) <- c("code","region","usd_m","share_all","share_mapped","dac_m")
dash_regions$usd_m <- as.numeric(dash_regions$usd_m); dash_regions$dac_m <- as.numeric(dash_regions$dac_m)
p_dr <- dash_regions %>% mutate(region = factor(region, levels = rev(region))) %>%
  ggplot(aes(usd_m, region)) + geom_col(fill = blue, width = .68) +
  geom_point(aes(x = dac_m), colour = gold, size = 3) +
  scale_x_continuous(labels = fmt_m, expand = expansion(mult = c(0,.12))) +
  labs(x = "2024 USD million", y = NULL, title = "Dashboard | Top AIM17 recipient regions",
       subtitle = "Bars: all providers  •  Gold dots: DAC members",
       caption = "Regional ranking shown as provided in the Dashboard sheet.") + theme_aim
save_plot(p_dr, "02_Dashboard_top_regions.png", 11, 7)

dash_rec <- dash[24:33, 1:6]; names(dash_rec) <- c("recipient","code","region","oecd_region","usd_m","dac_m")
dash_rec$usd_m <- as.numeric(dash_rec$usd_m); dash_rec$dac_m <- as.numeric(dash_rec$dac_m)
p_dt <- dash_rec %>% mutate(recipient = factor(recipient, levels = rev(recipient))) %>%
  ggplot(aes(usd_m, recipient)) + geom_col(fill = coral, width = .68) +
  geom_point(aes(x = dac_m), colour = ink, size = 2.8) +
  scale_x_continuous(labels = fmt_m, expand = expansion(mult = c(0,.13))) +
  labs(x = "2024 USD million", y = NULL, title = "Dashboard | Top recipient countries",
       subtitle = "Bars: all providers  •  Dark dots: DAC members") + theme_aim
save_plot(p_dt, "03_Dashboard_top_recipients.png", 10, 6.5)

# ---- AIM17 region ---------------------------------------------------------
region <- read_sheet("AIM17_地域別")
names(region) <- c("code","total","share","adaptation","mitigation","overlap")
region_nz <- region %>% filter(!is.na(code), total > 0)
p_region <- region_nz %>% mutate(code = factor(code, levels = rev(code))) %>%
  ggplot(aes(total, code, fill = code == "UNALLOC")) + geom_col(width = .7) +
  geom_text(aes(label = fmt_m(total)), hjust = -0.08, size = 3.5, colour = ink) +
  scale_fill_manual(values = c(`FALSE` = blue, `TRUE` = "#A8B2C1"), guide = "none") +
  scale_x_continuous(labels = fmt_m, expand = expansion(mult = c(0,.16))) +
  labs(x = "2024 USD million", y = NULL, title = "AIM17_地域別 | Climate finance received",
       subtitle = "UNALLOC is shown separately and excluded from mapped-region shares") + theme_aim
save_plot(p_region, "04_AIM17_region_total.png")

purpose <- region_nz %>% transmute(code, total,
  `Adaptation only` = pmax(0, adaptation-overlap),
  `Mitigation only` = pmax(0, mitigation-overlap), `Cross-cutting` = overlap)
stacked_chart(purpose, "code", "total", "AIM17_地域別 | Climate objective mix",
              "Mutually exclusive split avoids double counting overlap",
              "05_AIM17_region_climate_mix.png", share = TRUE)

# ---- AIM17 matrices -------------------------------------------------------
provider <- read_sheet("AIM17_拠出主体")
stacked_chart(provider, "AIM Code", "Total", "AIM17_拠出主体 | Provider composition",
              "Composition by provider type within each recipient region",
              "06_AIM17_provider_type.png", share = TRUE)

instrument <- read_sheet("AIM17_金融手段")
stacked_chart(instrument, "AIM Code", "Total", "AIM17_金融手段 | Financing instrument mix",
              "Grant, loan and other instruments as a share of each region",
              "07_AIM17_financial_instrument.png", share = TRUE)

income <- read_sheet("AIM17_所得階層")
stacked_chart(income, "AIM Code", "Total", "AIM17_所得階層 | Recipient income composition",
              "OECD recipient income groups within AIM17 regions",
              "08_AIM17_income_group.png", share = TRUE)

objective <- read_sheet("AIM17_気候目的")
stacked_chart(objective, "AIM Code", "Total", "AIM17_気候目的 | Rio-marker composition",
              "Principal, significant and climate-component finance",
              "09_AIM17_climate_objective.png", share = TRUE)

# ---- Recipient mapping ----------------------------------------------------
mapping <- read_sheet("受取地域マッピング")
names(mapping) <- c("recipient","code","total","share","adaptation","mitigation","overlap")
top30 <- mapping %>% filter(!is.na(recipient), recipient != "Developing countries, unspecified",
                            !grepl("regional", recipient, ignore.case = TRUE)) %>%
  slice_max(total, n = 30, with_ties = FALSE) %>% arrange(total)
p_top <- top30 %>% mutate(recipient = factor(recipient, levels = recipient)) %>%
  ggplot(aes(total, recipient, colour = code)) + geom_segment(aes(x=0,xend=total,yend=recipient),colour=mist,linewidth=2) +
  geom_point(size=4) + scale_x_continuous(labels=fmt_m,expand=expansion(mult=c(0,.08))) +
  labs(x="2024 USD million",y=NULL,title="受取地域マッピング | Top 30 recipients",
       subtitle="Regional aggregates and unspecified recipients excluded") + theme_aim +
  theme(legend.position="right")
save_plot(p_top,"10_recipient_mapping_top30.png",12,10)

scatter <- mapping %>% filter(total > 0, recipient != "Developing countries, unspecified")
p_scatter <- ggplot(scatter,aes(adaptation,mitigation,size=total,colour=code)) +
  geom_abline(slope=1,intercept=0,linetype=3,colour="#8D99AE") + geom_point(alpha=.72) +
  geom_text(data=slice_max(scatter,total,n=10),aes(label=recipient),size=3,vjust=-.8,check_overlap=TRUE,show.legend=FALSE) +
  scale_x_continuous(labels=fmt_m)+scale_y_continuous(labels=fmt_m)+scale_size_area(max_size=13,guide="none") +
  labs(x="Adaptation (USD million)",y="Mitigation (USD million)",
       title="受取地域マッピング | Adaptation vs mitigation",
       subtitle="Bubble area represents total climate finance") + theme_aim + theme(legend.position="right")
save_plot(p_scatter,"11_recipient_mapping_adaptation_vs_mitigation.png",11,8)

# ---- Model comparison -----------------------------------------------------
model <- read_sheet("モデル比較") %>% filter(!is.na(`AIM Code`))
model_input <- suppressWarnings(as.numeric(model$`Model transfer input (USD million)`))
if (all(is.na(model_input))) {
  p_model <- model %>% filter(`Climate Finance received (USD million)` > 0) %>%
    mutate(`AIM Code`=factor(`AIM Code`,levels=rev(`AIM Code`))) %>%
    ggplot(aes(`Climate Finance received (USD million)`,`AIM Code`)) +
    geom_col(fill=violet,width=.7)+scale_x_continuous(labels=fmt_m,expand=expansion(mult=c(0,.08)))+
    labs(x="2024 USD million",y=NULL,title="モデル比較 | OECD observed allocation",
         subtitle="Model transfer input is currently blank; populate column D to activate comparison",
         caption="The script automatically creates a paired OECD/model comparison after model values are entered.")+theme_aim
} else {
  z <- model %>% transmute(code=`AIM Code`,OECD=`Climate Finance received (USD million)`,Model=model_input) %>%
    pivot_longer(c(OECD,Model),names_to="series",values_to="value")
  p_model <- ggplot(z,aes(value,reorder(code,value),fill=series))+geom_col(position="dodge")+
    scale_fill_manual(values=c(OECD=violet,Model=gold))+scale_x_continuous(labels=fmt_m)+
    labs(x="USD million",y=NULL,title="モデル比較 | OECD vs model transfer allocation")+theme_aim
}
save_plot(p_model,"12_model_comparison.png",10,7)

# ---- References -----------------------------------------------------------
refs <- read_sheet("References")
ref_items <- setNames(as.character(refs[[2]]), as.character(refs[[1]]))
card_page("References & methodology", ref_items, "13_References_notes.png",
          "Sources, units, provider filter and AIM17 mapping assumptions")

manifest <- data.frame(
  sheet = c("README","Dashboard","Dashboard","Dashboard","AIM17_地域別","AIM17_地域別",
            "AIM17_拠出主体","AIM17_金融手段","AIM17_所得階層","AIM17_気候目的",
            "受取地域マッピング","受取地域マッピング","モデル比較","References"),
  output = sprintf("%02d_%s",0:13,c("README_overview.png","Dashboard_KPI.png","Dashboard_top_regions.png",
    "Dashboard_top_recipients.png","AIM17_region_total.png","AIM17_region_climate_mix.png",
    "AIM17_provider_type.png","AIM17_financial_instrument.png","AIM17_income_group.png",
    "AIM17_climate_objective.png","recipient_mapping_top30.png",
    "recipient_mapping_adaptation_vs_mitigation.png","model_comparison.png","References_notes.png"))
)
write.csv(manifest,file.path(output_dir,"manifest.csv"),row.names=FALSE,fileEncoding="UTF-8")
writeLines(c("Source: data/OECD_CRDF_2024_AIM17_analysis.xlsx",
             "All monetary values are 2024 USD million.",
             "Each workbook sheet is represented by at least one visual.",
             "Model comparison updates after values are entered in column D of モデル比較."),
           file.path(output_dir,"README.txt"))
message("Created ", nrow(manifest), " visuals in ", output_dir)
