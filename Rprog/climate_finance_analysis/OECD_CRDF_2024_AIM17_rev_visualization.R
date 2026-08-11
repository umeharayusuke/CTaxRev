# Visualise OECD_CRDF_2024_AIM17_analysis_rev.xlsx
# Output: output/OECD_CRDF_2024_AIM17_rev/

if (dir.exists("Rlib")) .libPaths(c(normalizePath("Rlib"), .libPaths()))
required <- c("readxl", "dplyr", "tidyr", "ggplot2", "scales", "ggalluvial")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install packages: ", paste(missing, collapse = ", "))

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(ggplot2); library(scales)
})

input_file <- file.path("data", "OECD_CRDF_2024_AIM17_analysis_rev.xlsx")
output_dir <- file.path("output", "OECD_CRDF_2024_AIM17_rev")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
stopifnot(file.exists(input_file))

# ---- Visual system --------------------------------------------------------
ink <- "#14213D"; cyan <- "#00A6A6"; blue <- "#2D6CDF"; coral <- "#F07167"
gold <- "#F2C14E"; violet <- "#725AC1"; green <- "#4C956C"; mist <- "#E8EEF5"
pal <- c(cyan, blue, coral, gold, violet, green, "#8D99AE", "#EE964B")

theme_rev <- theme_minimal(base_size = 13) +
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
fmt_bn <- label_number(big.mark = ",", accuracy = .1, suffix = " bn")
read_sheet <- function(name, col_names = TRUE) read_excel(input_file, sheet = name, col_names = col_names)

card_page <- function(title, items, filename, subtitle = NULL) {
  png(file.path(output_dir, filename), width = 2200, height = 1400, res = 200, bg = "white")
  grid::grid.newpage(); grid::grid.rect(gp = grid::gpar(fill = "white", col = NA))
  grid::grid.rect(x=.02,y=.5,width=.018,height=1,gp=grid::gpar(fill=cyan,col=NA))
  grid::grid.text(title,x=.07,y=.93,just="left",gp=grid::gpar(fontsize=25,fontface="bold",col=ink))
  if (!is.null(subtitle)) grid::grid.text(subtitle,x=.07,y=.885,just="left",
    gp=grid::gpar(fontsize=12,col="#667085"))
  n <- length(items); ys <- seq(.79,.11,length.out=n)
  for (i in seq_along(items)) {
    grid::grid.roundrect(x=.52,y=ys[i],width=.88,height=min(.105,.63/n),r=unit(.02,"snpc"),
      gp=grid::gpar(fill=if(i%%2)"#F5F8FC" else "white",col=mist,lwd=1.5))
    lab <- paste(strwrap(names(items)[i],width=26),collapse="\n")
    val <- paste(strwrap(items[[i]],width=105),collapse="\n")
    grid::grid.text(lab,x=.105,y=ys[i],just="left",gp=grid::gpar(fontsize=10.5,fontface="bold",col=cyan))
    grid::grid.text(val,x=.32,y=ys[i],just="left",gp=grid::gpar(fontsize=8.8,col=ink))
  }
  dev.off()
}

rank_bar <- function(data, category, value, title, subtitle, filename,
                     colour = blue, n = Inf, width = 11, height = 7) {
  z <- data %>% filter(!is.na(.data[[category]]), !is.na(.data[[value]])) %>%
    slice_max(.data[[value]], n = n, with_ties = FALSE) %>% arrange(.data[[value]])
  z[[category]] <- factor(z[[category]], levels = z[[category]])
  p <- ggplot(z, aes(.data[[value]], .data[[category]])) + geom_col(fill=colour,width=.68) +
    geom_text(aes(label=fmt_bn(.data[[value]])),hjust=-.08,size=3.4,colour=ink) +
    scale_x_continuous(labels=fmt_bn,expand=expansion(mult=c(0,.16))) +
    labs(x="2024 USD billion",y=NULL,title=title,subtitle=subtitle,
         caption="Source: OECD_CRDF_2024_AIM17_analysis_rev.xlsx") + theme_rev
  save_plot(p,filename,width,height)
}

stack_share <- function(data, id, total, components, labels, title, subtitle, filename) {
  z <- data %>% filter(!is.na(.data[[id]]), .data[[total]] > 0) %>%
    select(all_of(c(id,total,components))) %>% pivot_longer(all_of(components),
      names_to="component",values_to="value") %>% mutate(value=replace_na(as.numeric(value),0),
      component=factor(component,levels=components,labels=labels))
  ord <- data %>% filter(.data[[id]] %in% unique(z[[id]])) %>% arrange(.data[[total]]) %>% pull(all_of(id))
  z[[id]] <- factor(z[[id]],levels=ord)
  p <- ggplot(z,aes(value,.data[[id]],fill=component))+geom_col(position="fill",width=.72)+
    scale_x_continuous(labels=label_percent())+scale_fill_manual(values=pal)+
    labs(x="Share within region",y=NULL,title=title,subtitle=subtitle,
         caption="Zero-total AIM17 regions are omitted.")+theme_rev
  save_plot(p,filename)
}

# ---- README ---------------------------------------------------------------
readme <- read_sheet("README", col_names=FALSE) %>% filter(!is.na(...1),!is.na(...2),...1!="Item")
card_page("Revised workbook guide",setNames(as.character(readme$...2),as.character(readme$...1)),
          "00_README_overview.png","Scope, units and mapping hierarchy")

# ---- Dashboard ------------------------------------------------------------
dash <- read_sheet("Dashboard",col_names=FALSE)
row_kpi <- which(dash[[1]]=="KPI"); row_rec <- which(dash[[1]]=="AIM17 recipient")
row_broad <- which(dash[[1]]=="Broad recipient region")
kpi <- dash[(row_kpi+1):(row_kpi+5),1:3]; names(kpi)<-c("metric","value","unit"); kpi$value<-as.numeric(kpi$value)
provider_type_dash <- dash[(row_kpi+1):(row_kpi+5),5:7]; names(provider_type_dash)<-c("category","usd_bn","share")
provider_type_dash$usd_bn<-as.numeric(provider_type_dash$usd_bn)
recipient_dash <- dash[(row_rec+1):(row_rec+10),1:4]; names(recipient_dash)<-c("code","region","usd_bn","share")
recipient_dash$usd_bn<-as.numeric(recipient_dash$usd_bn)
provider_dash <- dash[(row_rec+1):(row_rec+10),6:9]; names(provider_dash)<-c("code","region","usd_bn","share")
provider_dash$usd_bn<-as.numeric(provider_dash$usd_bn)
broad_dash <- dash[(row_broad+1):(row_broad+7),1:3]; names(broad_dash)<-c("region","usd_bn","share")
broad_dash$usd_bn<-as.numeric(broad_dash$usd_bn)

kpi_amount <- kpi %>% filter(unit=="USD billion")
rank_bar(kpi_amount,"metric","value","Dashboard | Core totals","Mapped and unallocated amounts reconcile to total CRDF",
         "01_Dashboard_KPI.png",cyan,width=10,height=6)
rank_bar(provider_type_dash,"category","usd_bn","Dashboard | Provider categories","Multilateral development banks account for the largest share",
         "02_Dashboard_provider_categories.png",violet,width=10,height=6)
rank_bar(recipient_dash,"region","usd_bn","Dashboard | Top AIM17 recipient regions","AIM17 hierarchy: region-level allocation",
         "03_Dashboard_recipient_regions.png",blue,width=11,height=7)
rank_bar(provider_dash,"region","usd_bn","Dashboard | Top AIM17 provider regions","Country-attributed providers plus multilateral and private categories",
         "04_Dashboard_provider_regions.png",coral,width=11,height=7)
rank_bar(broad_dash,"region","usd_bn","Dashboard | Broad recipient regions","AIM17 results regrouped into broad geographic regions",
         "05_Dashboard_broad_regions.png",green,width=10,height=6)

# ---- AIM17 recipient hierarchy -------------------------------------------
rec <- read_sheet("AIM17_受取地域別")
rank_bar(rec,"Region","CRDF total (USD bn)","AIM17_受取地域別 | Total received",
         "UNSPEC is retained as a separate hierarchy node","06_AIM17_recipient_total.png",blue)
stack_share(rec,"AIM17","CRDF total (USD bn)",
 c("DAC member (USD bn)","MDB (USD bn)","Other multilateral (USD bn)","Private donor (USD bn)"),
 c("DAC member","MDB","Other multilateral","Private donor"),
 "AIM17_受取地域別 | Provider composition","Provider type within each recipient region",
 "07_AIM17_recipient_provider_mix.png")
stack_share(rec,"AIM17","CRDF total (USD bn)",
 c("Grant (USD bn)","Loan (USD bn)","Other instrument (USD bn)"),
 c("Grant","Loan","Other / unspecified"),
 "AIM17_受取地域別 | Instrument composition","Grant, loan and other instruments",
 "08_AIM17_recipient_instrument_mix.png")
stack_share(rec,"AIM17","CRDF total (USD bn)",
 c("Mitigation only (USD bn)","Adaptation only (USD bn)","Cross-cutting (USD bn)"),
 c("Mitigation only","Adaptation only","Cross-cutting"),
 "AIM17_受取地域別 | Climate-objective composition","Mutually exclusive objective categories",
 "09_AIM17_recipient_climate_mix.png")

# ---- AIM17 provider hierarchy --------------------------------------------
prov <- read_sheet("AIM17_拠出地域別")
rank_bar(prov,"Provider region/category","CRDF total (USD bn)","AIM17_拠出地域別 | Total provided",
         "Multilateral and private providers remain explicit non-country nodes",
         "10_AIM17_provider_total.png",coral)
stack_share(prov,"Provider AIM17","CRDF total (USD bn)",
 c("DAC member (USD bn)","Non-DAC member (USD bn)","MDB (USD bn)","Other multilateral (USD bn)","Private donor (USD bn)"),
 c("DAC member","Non-DAC member","MDB","Other multilateral","Private donor"),
 "AIM17_拠出地域別 | Provider-type composition","Provider status within each source-region node",
 "11_AIM17_provider_type_mix.png")

# ---- Provider type --------------------------------------------------------
ptype <- read_sheet("拠出主体タイプ")
rank_bar(ptype,"Provider Type","USD bn","拠出主体タイプ | Climate finance by provider type",
         "Total commitments by institutional provider category",
         "12_provider_type.png",violet,width=10,height=6)

# ---- Provider x recipient matrix -----------------------------------------
matrix <- read_sheet("AIM17_拠出×受取")
recipient_cols <- setdiff(names(matrix),c("Provider AIM17","Provider region/category","Provider total (USD bn)"))
flow <- matrix %>% pivot_longer(all_of(recipient_cols),names_to="recipient",values_to="usd_bn") %>%
  transmute(provider=`Provider AIM17`,provider_region=`Provider region/category`,recipient,
            usd_bn=replace_na(as.numeric(usd_bn),0)) %>% filter(usd_bn>0)
write.csv(flow,file.path(output_dir,"provider_recipient_flows.csv"),row.names=FALSE,fileEncoding="UTF-8")

p_heat <- ggplot(flow,aes(recipient,reorder(provider,usd_bn,FUN=sum),fill=usd_bn))+
  geom_tile(colour="white",linewidth=.4)+
  scale_fill_gradientn(colours=c("#F4F7FB",cyan,blue,ink),trans="sqrt",labels=fmt_bn,name="USD bn")+
  labs(x="Recipient AIM17",y="Provider AIM17",title="AIM17_拠出×受取 | Flow matrix",
       subtitle="Colour intensity uses a square-root scale so smaller bilateral flows remain visible")+
  theme_rev+theme(axis.text.x=element_text(angle=45,hjust=1),legend.position="right")
save_plot(p_heat,"13_provider_recipient_heatmap.png",13,7)

flow_major <- flow %>% mutate(share=usd_bn/sum(usd_bn)) %>% filter(share>=.005)
p_flow <- ggplot(flow_major,aes(axis1=provider,axis2=recipient,y=usd_bn))+
  ggalluvial::geom_alluvium(aes(fill=provider),alpha=.72,width=1/14)+
  ggalluvial::geom_stratum(width=1/14,fill="#EEF2F7",colour="#7A8798")+
  ggalluvial::stat_stratum(geom="text",aes(label=after_stat(stratum)),size=3)+
  scale_fill_manual(values=setNames(rep(pal,length.out=length(unique(flow_major$provider))),
                                    unique(flow_major$provider)))+
  scale_x_discrete(limits=c("Provider AIM17","Recipient AIM17"),expand=c(.12,.08))+
  labs(x=NULL,y="2024 USD billion",title="AIM17_拠出×受取 | Major finance flows",
       subtitle="Flows below 0.5% of the total are omitted for readability")+
  theme_rev+theme(legend.position="none")
save_plot(p_flow,"14_provider_recipient_sankey.png",13,8)

# ---- Instruments, objectives, income -------------------------------------
fin <- read_sheet("AIM17_金融手段")
stack_share(fin,"AIM17","Total (USD bn)",
 c("Loan (USD bn)","Grant (USD bn)","Other/unspecified (USD bn)"),c("Loan","Grant","Other / unspecified"),
 "AIM17_金融手段 | Instrument shares","Regional reliance on loans versus grants",
 "15_financial_instrument.png")

clim <- read_sheet("AIM17_気候目的")
stack_share(clim,"AIM17","Total (USD bn)",
 c("Mitigation only (USD bn)","Adaptation only (USD bn)","Cross-cutting (USD bn)"),
 c("Mitigation only","Adaptation only","Cross-cutting"),
 "AIM17_気候目的 | Climate-objective shares","Exclusive mitigation, adaptation and cross-cutting allocation",
 "16_climate_objective.png")
stack_share(clim,"AIM17","Total (USD bn)",
 c("Principal (USD bn)","Significant (USD bn)","Climate components (USD bn)"),
 c("Principal","Significant","Climate components"),
 "AIM17_気候目的 | Rio-marker / component shares","Alternative climate-purpose classification in the revised workbook",
 "17_rio_marker_components.png")

income <- read_sheet("AIM17_所得階層")
stack_share(income,"AIM17","Total (USD bn)",
 c("LDCs (USD bn)","Other LICs (USD bn)","LMICs (USD bn)","UMICs (USD bn)","Part I unallocated by income (USD bn)"),
 c("LDCs","Other LICs","LMICs","UMICs","Income unallocated"),
 "AIM17_所得階層 | Recipient-income composition","Income hierarchy within each AIM17 recipient region",
 "18_income_group.png")

# ---- Recipient countries and mapping quality -----------------------------
countries <- read_sheet("受取国_TOP")
top30 <- countries %>% filter(!is.na(ISO3),!grepl("regional|unspecified",`Mapping flag`,ignore.case=TRUE)) %>%
  slice_max(`USD bn`,n=30,with_ties=FALSE)
rank_bar(top30,"Recipient Name","USD bn","受取国_TOP | Top 30 recipient countries",
         "Regional aggregates and unspecified recipients excluded",
         "19_recipient_country_top30.png",cyan,n=30,width=12,height=10)

mapping <- read_sheet("受取地域マッピング")
map_quality <- mapping %>% mutate(`Mapping flag`=replace_na(`Mapping flag`,"Missing flag")) %>%
  group_by(`Mapping flag`) %>% summarise(usd_bn=sum(`USD bn`,na.rm=TRUE),recipients=n(),.groups="drop")
rank_bar(map_quality,"Mapping flag","usd_bn","受取地域マッピング | Mapping quality by amount",
         "Shows how much finance relies on exact, fallback or aggregate mapping",
         "20_mapping_quality.png",gold,width=12,height=6)

region_map <- read_sheet("地域対応表") %>% filter(!is.na(AIM17)) %>% count(AIM17,Region,name="iso3_count")
p_map <- ggplot(region_map,aes(iso3_count,reorder(paste0(AIM17," | ",Region),iso3_count)))+
  geom_col(fill=green,width=.7)+geom_text(aes(label=iso3_count),hjust=-.15,colour=ink)+
  scale_x_continuous(expand=expansion(mult=c(0,.1)))+
  labs(x="Number of ISO3 economies",y=NULL,title="地域対応表 | Coverage of AIM17 mapping",
       subtitle="Country/economy count assigned to each AIM17 hierarchy node")+theme_rev
save_plot(p_map,"21_region_mapping_coverage.png",11,8)

# ---- Model comparison -----------------------------------------------------
model <- read_sheet("モデル比較") %>% filter(!is.na(AIM17))
model_input <- suppressWarnings(as.numeric(model$`Model transfer (USD bn) INPUT`))
if (all(is.na(model_input))) {
  p_model <- model %>% filter(`OECD CRDF (USD bn)`>0) %>%
    ggplot(aes(`OECD CRDF (USD bn)`,reorder(paste0(AIM17," | ",Region),`OECD CRDF (USD bn)`)))+
    geom_col(fill=violet,width=.7)+scale_x_continuous(labels=fmt_bn,expand=expansion(mult=c(0,.08)))+
    labs(x="2024 USD billion",y=NULL,title="モデル比較 | OECD observed allocation",
         subtitle="Model input is blank; populate column E to activate paired comparison",
         caption="After model values are entered, rerunning this script displays OECD and model side by side.")+theme_rev
} else {
  z <- model %>% transmute(code=AIM17,region=Region,OECD=`OECD CRDF (USD bn)`,Model=model_input) %>%
    pivot_longer(c(OECD,Model),names_to="series",values_to="usd_bn")
  p_model <- ggplot(z,aes(usd_bn,reorder(paste0(code," | ",region),usd_bn),fill=series))+
    geom_col(position="dodge")+scale_fill_manual(values=c(OECD=violet,Model=gold))+
    scale_x_continuous(labels=fmt_bn)+labs(x="USD billion",y=NULL,
      title="モデル比較 | OECD versus model allocation")+theme_rev
}
save_plot(p_model,"22_model_comparison.png",11,8)

# ---- References -----------------------------------------------------------
refs <- read_sheet("References")
card_page("References & methodology",setNames(as.character(refs[[2]]),as.character(refs[[1]])),
          "23_References_notes.png","Sources and processing assumptions")

manifest <- data.frame(
  number=0:23,
  sheet=c("README",rep("Dashboard",5),rep("AIM17_受取地域別",4),rep("AIM17_拠出地域別",2),
          "拠出主体タイプ",rep("AIM17_拠出×受取",2),"AIM17_金融手段",rep("AIM17_気候目的",2),
          "AIM17_所得階層","受取国_TOP","受取地域マッピング","地域対応表","モデル比較","References"),
  output=c("00_README_overview.png","01_Dashboard_KPI.png","02_Dashboard_provider_categories.png",
    "03_Dashboard_recipient_regions.png","04_Dashboard_provider_regions.png","05_Dashboard_broad_regions.png",
    "06_AIM17_recipient_total.png","07_AIM17_recipient_provider_mix.png","08_AIM17_recipient_instrument_mix.png",
    "09_AIM17_recipient_climate_mix.png","10_AIM17_provider_total.png","11_AIM17_provider_type_mix.png",
    "12_provider_type.png","13_provider_recipient_heatmap.png","14_provider_recipient_sankey.png",
    "15_financial_instrument.png","16_climate_objective.png","17_rio_marker_components.png",
    "18_income_group.png","19_recipient_country_top30.png","20_mapping_quality.png",
    "21_region_mapping_coverage.png","22_model_comparison.png","23_References_notes.png")
)
write.csv(manifest,file.path(output_dir,"manifest.csv"),row.names=FALSE,fileEncoding="UTF-8")
writeLines(c("Source: data/OECD_CRDF_2024_AIM17_analysis_rev.xlsx",
             "Amounts are 2024 USD billion.",
             "Hierarchy: AIM17 region -> broad region -> recipient country.",
             "All 14 workbook sheets are represented by at least one visual.",
             "Model comparison updates after input values are entered in モデル比較 column E."),
           file.path(output_dir,"README.txt"))
message("Created ",nrow(manifest)," visuals in ",output_dir)
