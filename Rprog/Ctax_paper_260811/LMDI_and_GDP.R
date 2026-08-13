library(tidyverse)
library(gdxrrw)
library(patchwork)
library(scales)

# Settings ----------------------------------------------------------------
gdx_file <- "global_17_IAMC.gdx"
output_dir <- "../../output/Figure"
dir.create(output_dir, showWarnings=FALSE, recursive=TRUE)

scenario_def <- "SSP2_400C_2030CP_NoCC_No"
scenario_aid <- "SSP2_400C_2030CP_15th_NoCC_No"
scenario_bau <- "SSP2_BaU_NoCC_No"
target_year <- 2050

region_aggregates <- c("Rprovider15th","Rrecipient15th")
region_levels <- c("Provider","Recipient")
region_selected <- c("USA","XE25","JPN","CAN","XER","XOC","TUR","CHN","CIS","XLM",
                     "IND","XSE","XSA","BRA","XME","XNF","XAF")
region_group_map <- tibble(
  region=region_selected,
  Region=c(rep("Provider",10),rep("Recipient",7))
)

theme_common <- theme_minimal(base_size=13) +
  theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_line(color="grey85",linewidth=0.3),
        axis.text=element_text(size=11,color="black"),
        axis.text.x=element_text(size=11,color="black",face="bold"),
        axis.title=element_text(size=12,color="black"),
        strip.text=element_text(size=13,color="black",face="bold"),
        legend.position="right", legend.title=element_text(size=12,face="bold"),
        legend.text=element_text(size=11),
        plot.title=element_text(size=14,face="bold",color="black"),
        plot.tag=element_text(size=16,face="bold",color="black"),
        plot.margin=margin(8,8,8,8))

# (a) GLMDI decomposition --------------------------------------------------
vars_lmdi <- c("Gro_Emi_CO2","GDP_MER","Fin_Ene","Fin_Ene_Ele","Gro_Rem_CO2")
removal_stored_as_negative <- TRUE

df_lmdi_raw <- rgdx.param(gdx_file,"IAMC_template") %>%
  filter(VEMF %in% vars_lmdi, SCENARIO %in% c(scenario_def,scenario_aid),
         REMF %in% region_aggregates, as.numeric(as.character(YEMF))==target_year) %>%
  transmute(REMF,SCENARIO,VEMF,Value=as.numeric(IAMC_Template)) %>%
  group_by(REMF,SCENARIO,VEMF) %>%
  summarise(Value=sum(Value,na.rm=TRUE),.groups="drop")

missing_lmdi <- expand_grid(REMF=region_aggregates,
                            SCENARIO=c(scenario_def,scenario_aid),
                            VEMF=vars_lmdi) %>%
  anti_join(df_lmdi_raw,by=c("REMF","SCENARIO","VEMF"))

if(nrow(missing_lmdi)>0){
  print(missing_lmdi)
  stop("Some variables required for GLMDI are missing.")
}

df_lmdi <- df_lmdi_raw %>%
  pivot_wider(names_from=VEMF,values_from=Value) %>%
  mutate(Region=recode(REMF,"Rprovider15th"="Provider","Rrecipient15th"="Recipient"),
         Region=factor(Region,levels=region_levels),
         Scenario=recode(SCENARIO,
                         "SSP2_400C_2030CP_NoCC_No"="Def",
                         "SSP2_400C_2030CP_15th_NoCC_No"="Aid"),
         Scenario=factor(Scenario,levels=c("Def","Aid")),
         Removal=if(removal_stored_as_negative) Gro_Rem_CO2 else -Gro_Rem_CO2,
         GDP=GDP_MER, EI=Fin_Ene/GDP_MER, Ele=Fin_Ene_Ele/Fin_Ene,
         CI=Gro_Emi_CO2/Fin_Ene_Ele, NetCO2=Gro_Emi_CO2+Removal)

if(removal_stored_as_negative && any(df_lmdi$Gro_Rem_CO2>0))
  warning("Some Gro_Rem_CO2 values are positive. Check the sign convention.")

if(any(df_lmdi$Gro_Emi_CO2<=0 | df_lmdi$GDP<=0 | df_lmdi$EI<=0 |
       df_lmdi$Ele<=0 | df_lmdi$CI<=0))
  stop("Gross emissions and all multiplicative GLMDI factors must be positive.")

log_mean <- function(a,b){
  if(!is.finite(a)||!is.finite(b)||a<=0||b<=0)
    stop("Logarithmic mean requires positive finite values.")
  if(isTRUE(all.equal(a,b))) return(a)
  (a-b)/(log(a)-log(b))
}

run_glmdi <- function(data_region){
  base <- data_region %>% filter(Scenario=="Def")
  target <- data_region %>% filter(Scenario=="Aid")
  if(nrow(base)!=1||nrow(target)!=1)
    stop("Each region must contain one Def and one Aid observation.")

  gross0 <- base$Gro_Emi_CO2[[1]]; gross1 <- target$Gro_Emi_CO2[[1]]
  rem0 <- base$Removal[[1]]; rem1 <- target$Removal[[1]]
  f0 <- c(GDP=base$GDP[[1]],EI=base$EI[[1]],Ele=base$Ele[[1]],CI=base$CI[[1]])
  f1 <- c(GDP=target$GDP[[1]],EI=target$EI[[1]],Ele=target$Ele[[1]],CI=target$CI[[1]])

  effects <- c(log_mean(gross1,gross0)*log(f1/f0),Removal=rem1-rem0)
  net0 <- gross0+rem0; net1 <- gross1+rem1
  residual <- net1-net0-sum(effects)

  if(abs(residual)>1e-6*max(1,abs(net0),abs(net1)))
    warning(paste0("GLMDI residual: ",format(residual,scientific=TRUE)))

  before <- net0+c(0,cumsum(head(effects,-1)))
  after <- before+effects

  bind_rows(
    tibble(Step="Start",Value=net0,ymin=pmin(0,net0),ymax=pmax(0,net0),EndLevel=net0),
    tibble(Step=names(effects),Value=as.numeric(effects),
           ymin=pmin(before,after),ymax=pmax(before,after),EndLevel=after),
    tibble(Step="End",Value=net1,ymin=pmin(0,net1),ymax=pmax(0,net1),EndLevel=net1)
  )
}

step_order <- c("Start","GDP","EI","Ele","CI","Removal","End")

wf <- df_lmdi %>%
  group_by(Region) %>% group_modify(~run_glmdi(.x)) %>% ungroup() %>%
  mutate(Step=factor(Step,levels=step_order),
         x=match(as.character(Step),step_order),
         change_label=if_else(Step %in% c("Start","End"),
                              format(round(Value,0),big.mark=","),
                              sprintf("%+.0f",Value)),
         label_y=if_else(Value>=0,ymax,ymin),
         label_vjust=if_else(Value>=0,-0.45,1.25))

connectors <- wf %>% filter(Step!="End") %>%
  transmute(Region,x=x+0.4,xend=x+0.6,y=EndLevel,yend=EndLevel)

cols_lmdi <- c("GDP"="#D73027","EI"="#4575B4","Ele"="#FDAE61",
               "CI"="#1A9850","Removal"="#542788",
               "Start"="grey45","End"="black")

g_lmdi <- ggplot(wf) +
  geom_hline(yintercept=0,linewidth=0.7,color="black") +
  geom_segment(data=connectors,aes(x=x,xend=xend,y=y,yend=yend),
               inherit.aes=FALSE,color="grey55",linewidth=0.5) +
  geom_rect(aes(xmin=x-0.4,xmax=x+0.4,ymin=ymin,ymax=ymax,fill=Step)) +
  geom_text(aes(x=x,y=label_y,label=change_label,vjust=label_vjust),
            size=4,fontface="bold") +
  scale_x_continuous(breaks=c(1,7),labels=c("Def","Aid"),
                     limits=c(0.5,7.5),expand=expansion(mult=c(0,0))) +
  scale_y_continuous(labels=label_number(accuracy=1,big.mark=",")) +
  scale_fill_manual(
    values=cols_lmdi, breaks=c("GDP","EI","Ele","CI","Removal"),
    labels=c("GDP"="Economic activity","EI"="Energy intensity",
             "Ele"="Electrification","CI"="Carbon intensity",
             "Removal"="Carbon removal"),
    name="Emission drivers"
  ) +
  facet_wrap(~Region,nrow=1,scales="free_y") +
  labs(title="Decomposition of net CO₂ emission changes: Aid − Def",
       x=NULL,y="Net CO₂ emissions and contributions (MtCO₂/yr in 2050)") +
  coord_cartesian(clip="off") + theme_common +
  theme(axis.ticks.x=element_blank())

# (b) GDP decomposition ----------------------------------------------------
scenario_files <- c(Def=scenario_def,Aid=scenario_aid,BaU=scenario_bau)
gdp_file_paths <- paste0(scenario_files,".gdx")
missing_gdp_files <- gdp_file_paths[!file.exists(gdp_file_paths)]

if(length(missing_gdp_files)>0)
  stop("GDP_s input file(s) not found: ",paste(missing_gdp_files,collapse=", "))

gdp_raw <- imap_dfr(scenario_files,function(file_stem,scenario_label){
  rgdx.param(paste0(file_stem,".gdx"),"GDP_s") %>%
    mutate(SCENARIO=scenario_label)
})

gdp_component_order <- c("Consumption","Investment","Net export")

gdp_components_raw <- gdp_raw %>%
  transmute(SCENARIO,
            year=as.numeric(as.character(Y)),
            region=as.character(R),
            category=case_when(
              INS_MCR %in% c("ROW","IMP")~"Net export",
              INS_MCR=="HURB"~"Consumption",
              INS_MCR=="S-I"~"Investment",
              TRUE~NA_character_
            ),
            value=as.numeric(GDP_s)) %>%
  filter(region %in% region_selected,year==target_year,!is.na(category)) %>%
  left_join(region_group_map,by="region") %>%
  filter(!is.na(Region)) %>%
  group_by(year,Region,SCENARIO,category) %>%
  summarise(value=sum(value,na.rm=TRUE),.groups="drop")

# 地域・シナリオ全体が欠落している場合はエラー
missing_coverage <- expand_grid(
  year=target_year,Region=region_levels,SCENARIO=names(scenario_files)
) %>%
  anti_join(gdp_components_raw %>% distinct(year,Region,SCENARIO),
            by=c("year","Region","SCENARIO"))

if(nrow(missing_coverage)>0){
  print(missing_coverage)
  stop("GDP_s data are missing for some regions or scenarios.")
}

# GDXで保存されていないゼロ値の構成項目を0として補完
gdp_grid <- expand_grid(
  year=target_year,Region=region_levels,
  SCENARIO=names(scenario_files),category=gdp_component_order
)

sparse_zero_records <- gdp_grid %>%
  anti_join(gdp_components_raw,by=c("year","Region","SCENARIO","category"))

if(nrow(sparse_zero_records)>0)
  message(nrow(sparse_zero_records),
          " sparse GDP_s record(s) were absent and treated as zero.")

gdp_components <- gdp_grid %>%
  left_join(gdp_components_raw,
            by=c("year","Region","SCENARIO","category")) %>%
  mutate(value=replace_na(value,0))

gdp_all <- bind_rows(
  gdp_components,
  gdp_components %>%
    group_by(year,Region,SCENARIO) %>%
    summarise(value=sum(value,na.rm=TRUE),.groups="drop") %>%
    mutate(category="Total")
)

gdp_bau_category <- gdp_all %>% filter(SCENARIO=="BaU") %>%
  select(year,Region,category,bau_value=value)

gdp_bau_total <- gdp_all %>% filter(SCENARIO=="BaU",category=="Total") %>%
  select(year,Region,bau_total=value)

gdp_diff <- gdp_all %>%
  filter(SCENARIO %in% c("Def","Aid")) %>%
  left_join(gdp_bau_category,by=c("year","Region","category")) %>%
  left_join(gdp_bau_total,by=c("year","Region")) %>%
  mutate(bau_percent=100*(value-bau_value)/bau_total,
         Region=factor(Region,levels=region_levels),
         SCENARIO=factor(SCENARIO,levels=c("Def","Aid")),
         category=factor(category,levels=c(gdp_component_order,"Total")))

if(any(!is.finite(gdp_diff$bau_percent)))
  stop("Non-finite GDP decomposition values were generated. Check BaU GDP totals.")

cols_gdp <- c("Consumption"="#4575B4","Investment"="#FDAE61","Net export"="#1A9850")

g_gdp <- ggplot() +
  geom_col(data=filter(gdp_diff,category!="Total"),
           aes(x=SCENARIO,y=bau_percent,fill=category),
           width=0.68,color="white",linewidth=0.15) +
  geom_point(data=filter(gdp_diff,category=="Total"),
             aes(x=SCENARIO,y=bau_percent),size=3,color="black") +
  geom_hline(yintercept=0,linetype="dashed",linewidth=0.4,color="grey35") +
  scale_y_continuous(labels=label_number(accuracy=0.1)) +
  scale_fill_manual(values=cols_gdp,breaks=gdp_component_order,
                    name="GDP components",drop=FALSE) +
  facet_wrap(~Region,nrow=1,scales="free_y") +
  labs(title="Decomposition of GDP changes relative to BaU",
       x=NULL,y="GDP change from BaU in 2050 (%)") +
  theme_common

# Individual figures -------------------------------------------------------
#plot(g_lmdi)
#plot(g_gdp)

#ggsave(file.path(output_dir,"LMDI_2050.png"),g_lmdi,width=14,height=7,dpi=600,bg="white")

#ggsave(file.path(output_dir,"GDP_decomposition_2050.png"),g_gdp,width=14,height=7,dpi=600,bg="white")

# Combined figure ----------------------------------------------------------
g_combined <- (g_lmdi/g_gdp) +
  plot_layout(ncol=1,heights=c(1,1),guides="collect") +
  plot_annotation(tag_levels="a",tag_prefix="(",tag_suffix=")")

g_combined <- g_combined &
  theme(legend.position="right",
        plot.tag=element_text(size=16,face="bold",color="black"))

plot(g_combined)

ggsave(file.path(output_dir,"F5_LMDI_GDP_2050.png"),
       g_combined,width=14,height=13,dpi=600,bg="white")