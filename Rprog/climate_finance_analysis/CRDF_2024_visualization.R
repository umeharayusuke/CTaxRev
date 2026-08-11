# OECD CRDF-RP 2024 climate-finance visualisation
# Input: data/CRDF-RP_2024.xlsx  Output: output/CRDF_2024/

if (dir.exists("Rlib")) .libPaths(c(normalizePath("Rlib"), .libPaths()))

required <- c("readxl", "dplyr", "tidyr", "ggplot2", "scales", "ggalluvial", "countrycode")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install required packages first: install.packages(c(",
                          paste(sprintf("'%s'", missing), collapse = ", "), "))")

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(scales)
})

input_file <- file.path("data", "CRDF-RP_2024.xlsx")
output_dir <- file.path("output", "CRDF_2024")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
stopifnot(file.exists(input_file))

d <- readxl::read_excel(input_file, sheet = "2024") %>%
  mutate(across(contains("2024 USD thousand"), ~replace_na(as.numeric(.x), 0)),
         `Sector Name` = replace_na(`Sector Name`, "Unallocated / unspecified"),
         adaptation = `Adaptation - 2024 USD thousand`,
         mitigation = `Mitigation - 2024 USD thousand`,
         overlap = `Overlap - 2024 USD thousand`,
         # Avoid double counting adaptation-mitigation overlap.
         total = adaptation + mitigation - overlap,
         `Adaptation only` = pmax(0, adaptation - overlap),
         `Mitigation only` = pmax(0, mitigation - overlap),
         `Cross-cutting (overlap)` = pmax(0, overlap),
         instrument_group = case_when(
           `Financial Instrument` %in% c("Standard grant", "Reimbursable grant",
                                        "Reflow-based reimbursable grant") ~ "Grant",
           `Financial Instrument` %in% c("Standard loan", "Subordinated loan", "Bonds",
                                        "Other debt securities") ~ "Loan / debt",
           `Financial Instrument` %in% c("Common equity", "Preferred equity",
                                        "Shares in collective investment vehicles") ~ "Equity",
           grepl("hybrid", `Financial Instrument`, ignore.case = TRUE) ~ "Hybrid",
           TRUE ~ "Unspecified"))

# countrycode handles country recipients; explicit labels handle OECD regional rows.
regional_labels <- c(
  "Africa, regional"="Africa", "South of Sahara, regional"="Africa",
  "Western Africa, regional"="Africa", "Eastern Africa, regional"="Africa",
  "Southern Africa, regional"="Africa", "North of Sahara, regional"="Africa",
  "Middle Africa, regional"="Africa", "Asia, regional"="Asia",
  "Far East Asia, regional"="Asia", "South Asia, regional"="Asia",
  "Central Asia, regional"="Asia", "South & Central Asia, regional"="Asia",
  "Middle East, regional"="Asia", "Europe, regional"="Europe",
  "America, regional"="Latin America & Caribbean",
  "South America, regional"="Latin America & Caribbean",
  "Caribbean, regional"="Latin America & Caribbean",
  "Caribbean & Central America, regional"="Latin America & Caribbean",
  "Central America, regional"="Latin America & Caribbean",
  "Oceania, regional"="Oceania", "Polynesia, regional"="Oceania",
  "Melanesia, regional"="Oceania")

country_region <- function(x) {
  aliases <- c("China (People's Republic of)"="China", "Türkiye"="Turkey",
               "Viet Nam"="Vietnam", "Côte d'Ivoire"="Ivory Coast",
               "Lao People's Democratic Republic"="Laos",
               "Democratic Republic of the Congo"="Democratic Republic of the Congo",
               "West Bank and Gaza Strip"="Palestine")
  y <- ifelse(x %in% names(aliases), aliases[x], x)
  cont <- suppressWarnings(countrycode::countrycode(y, "country.name", "continent"))
  out <- recode(cont, Americas="Latin America & Caribbean", .default=cont)
  out[x %in% names(regional_labels)] <- unname(regional_labels[x[x %in% names(regional_labels)]])
  replace_na(out, "Unspecified / global")
}

d <- d %>% mutate(
  recipient_region = country_region(`Recipient Name`),
  provider_country_region = country_region(`Provider Name`),
  provider_region = case_when(
    `Provider Type` %in% c("Other multilateral", "Multilateral development bank") ~ "Multilateral / global",
    `Provider Name` %in% c("United States", "Canada") ~ "North America",
    provider_country_region == "Unspecified / global" ~ "Other / private",
    TRUE ~ provider_country_region))

theme_crdf <- theme_minimal(base_size = 13) +
  theme(panel.grid.major.y = element_blank(), legend.position = "bottom",
        plot.title = element_text(face = "bold"), plot.caption = element_text(hjust = 0))
palette <- c("#176B87", "#64CCC5", "#DAA520", "#D95F59", "#6C5B7B", "#7A9E48")

summarise_amount <- function(data, group) data %>%
  group_by({{group}}) %>% summarise(usd_thousand=sum(total, na.rm=TRUE), .groups="drop") %>%
  mutate(usd_billion=usd_thousand/1e6, share=usd_thousand/sum(usd_thousand)) %>%
  arrange(desc(usd_billion))

save_bar <- function(tab, filename, title, n=Inf, fill=palette[1]) {
  z <- slice_head(tab, n=n) %>% mutate(category=reorder(as.character(category), usd_billion))
  p <- ggplot(z, aes(usd_billion, category)) + geom_col(fill=fill, width=.72) +
    geom_text(aes(label=paste0(number(usd_billion, accuracy=.1), " bn")), hjust=-.08, size=3.5) +
    scale_x_continuous(labels=label_number(suffix=" bn"), expand=expansion(mult=c(0,.16))) +
    labs(x="2024 USD billion", y=NULL, title=title,
         caption="Total = adaptation + mitigation - overlap") + theme_crdf
  ggsave(file.path(output_dir, filename), p, width=10, height=6.5, dpi=180, bg="white")
}

specs <- list(
  provider_type=list("Provider Type", "01_provider_type.png", "Climate finance by provider type", Inf),
  recipient_income=list("Recipient Income Group (OECD Classification)", "02_recipient_income.png", "Climate finance by recipient income group", Inf),
  instrument=list("instrument_group", "04_financial_instrument.png", "Climate finance by financial instrument", Inf),
  sector=list("Sector Name", "05_sector_top15.png", "Top 15 sectors receiving climate finance", 15),
  provider=list("Provider Name", "06_provider_top15.png", "Top 15 providers", 15),
  recipient=list("Recipient Name", "07_recipient_top15.png", "Top 15 recipients", 15),
  flow_type=list("Type of Flow Name", "08_flow_type.png", "Climate finance by flow type", Inf),
  recipient_region=list("recipient_region", "09_recipient_region.png", "Climate finance by recipient region", Inf))

tables <- list()
for (nm in names(specs)) {
  col <- specs[[nm]][[1]]
  tab <- summarise_amount(d, .data[[col]]) %>% rename(category=1)
  tables[[nm]] <- tab
  write.csv(tab, file.path(output_dir, paste0(nm, ".csv")), row.names=FALSE, fileEncoding="UTF-8")
  save_bar(tab, specs[[nm]][[2]], specs[[nm]][[3]], specs[[nm]][[4]])
}

purpose <- d %>% summarise(across(c(`Adaptation only`, `Mitigation only`,
  `Cross-cutting (overlap)`), sum)) %>% pivot_longer(everything(), names_to="category",
  values_to="usd_thousand") %>% mutate(usd_billion=usd_thousand/1e6,
  share=usd_thousand/sum(usd_thousand))
write.csv(purpose, file.path(output_dir,"purpose.csv"), row.names=FALSE, fileEncoding="UTF-8")
save_bar(purpose, "03_climate_objective.png", "Climate objective: mutually exclusive allocation")

heat <- d %>% group_by(`Provider Type`, `Recipient Income Group (OECD Classification)`) %>%
  summarise(usd_billion=sum(total)/1e6, .groups="drop")
p_heat <- ggplot(heat, aes(`Recipient Income Group (OECD Classification)`, `Provider Type`, fill=usd_billion)) +
  geom_tile(color="white") + geom_text(aes(label=number(usd_billion, accuracy=.1)), size=3.4) +
  scale_fill_viridis_c(option="C", name="USD bn") + labs(x=NULL,y=NULL,
  title="Provider type × recipient income group") + theme_crdf +
  theme(axis.text.x=element_text(angle=35,hjust=1))
ggsave(file.path(output_dir,"10_provider_type_by_income_heatmap.png"), p_heat, width=10,height=6.5,dpi=180,bg="white")
write.csv(heat, file.path(output_dir,"provider_type_by_income.csv"),row.names=FALSE,fileEncoding="UTF-8")

conc <- bind_rows(Provider=tables$provider, Recipient=tables$recipient, .id="side") %>%
  group_by(side) %>% arrange(desc(usd_thousand),.by_group=TRUE) %>%
  mutate(rank=row_number(), cumulative_share=cumsum(usd_thousand)/sum(usd_thousand))
p_conc <- ggplot(conc,aes(rank,cumulative_share,color=side))+geom_line(linewidth=1.2)+
  geom_hline(yintercept=c(.5,.8),linetype=3,color="grey60")+
  scale_y_continuous(labels=label_percent())+scale_color_manual(values=c(palette[1],palette[4]))+
  labs(x="Number of entities (ranked largest first)",y="Cumulative share",title="Concentration of climate finance")+theme_crdf
ggsave(file.path(output_dir,"11_concentration_curves.png"),p_conc,width=10,height=6,dpi=180,bg="white")
write.csv(conc,file.path(output_dir,"concentration.csv"),row.names=FALSE,fileEncoding="UTF-8")

flows <- d %>% group_by(provider_region,recipient_region) %>%
  summarise(usd_thousand=sum(total),.groups="drop") %>%
  mutate(usd_billion=usd_thousand/1e6,share=usd_thousand/sum(usd_thousand)) %>%
  arrange(desc(usd_thousand))
write.csv(flows,file.path(output_dir,"regional_flows.csv"),row.names=FALSE,fileEncoding="UTF-8")
p_flow <- flows %>% filter(share>=.01) %>%
  ggplot(aes(axis1=provider_region,axis2=recipient_region,y=usd_billion))+
  ggalluvial::geom_alluvium(aes(fill=provider_region),alpha=.72,width=1/12)+
  ggalluvial::geom_stratum(width=1/12,fill="#EEEEEE",color="#777777")+
  ggalluvial::stat_stratum(geom="text",aes(label=after_stat(stratum)),size=3)+
  scale_x_discrete(limits=c("Provider region","Recipient region"),expand=c(.12,.08))+
  scale_fill_manual(values=setNames(rep(palette,length.out=length(unique(flows$provider_region))),unique(flows$provider_region)))+
  labs(y="2024 USD billion",x=NULL,title="Regional climate-finance flows",
       subtitle="Flows below 1% omitted; multilateral institutions classified as global")+theme_crdf+theme(legend.position="none")
ggsave(file.path(output_dir,"12_regional_flow_sankey.png"),p_flow,width=12,height=7,dpi=180,bg="white")

metrics <- data.frame(metric=c("Total climate finance (USD bn)","Adaptation-only share",
 "Mitigation-only share","Cross-cutting share","Grant share","Loan/debt share","Records"),
 value=c(sum(d$total)/1e6,purpose$share[match("Adaptation only",purpose$category)],
 purpose$share[match("Mitigation only",purpose$category)],purpose$share[match("Cross-cutting (overlap)",purpose$category)],
 tables$instrument$share[match("Grant",tables$instrument$category)],
 tables$instrument$share[match("Loan / debt",tables$instrument$category)],nrow(d)))
write.csv(metrics,file.path(output_dir,"00_key_metrics.csv"),row.names=FALSE,fileEncoding="UTF-8")
writeLines(c("Source: data/CRDF-RP_2024.xlsx","Total = adaptation + mitigation - overlap.",
 "Amounts shown in 2024 USD billion; source columns are USD thousand.",
 "Regional rules are documented in Rprog/CRDF_2024_visualization.R."),file.path(output_dir,"README.txt"))
message("Created charts and tables in ", output_dir)
