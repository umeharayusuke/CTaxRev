library(tidyverse)
library(dplyr)
library(ggplot2)
library(gdxrrw)
library(stringr)
library(gridExtra)
library(patchwork)
library(cowplot)
library(lemon)
library(purrr)
library(rnaturalearthdata)
library(rnaturalearth)
library(readxl)


gdx_file <- "global_17_IAMC.gdx"; output_dir <- "../../output/Figure"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

CLP <- c("SSP2_400C_2030CP_NoCC_No","SSP2_400C_2030CP_15th_NoCC_No")
Region <- c("Rprovider15th","Rrecipient15th")
vars_lmdi <- c("Gro_Emi_CO2","GDP_MER","Fin_Ene","Fin_Ene_Ele","Gro_Rem_CO2")

# TRUE：Gro_Rem_CO2が負値、FALSE：正の除去量として格納されている場合
removal_stored_as_negative <- TRUE

df_lmdi_raw <- rgdx.param(gdx_file, "IAMC_template") %>%
  filter(VEMF %in% vars_lmdi, SCENARIO %in% CLP, REMF %in% Region,
         as.numeric(as.character(YEMF)) == 2050) %>%
  transmute(REMF, SCENARIO, VEMF, Value = as.numeric(IAMC_Template)) %>%
  group_by(REMF, SCENARIO, VEMF) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")

missing_data <- expand_grid(REMF=Region, SCENARIO=CLP, VEMF=vars_lmdi) %>%
  anti_join(df_lmdi_raw, by=c("REMF","SCENARIO","VEMF"))
if(nrow(missing_data)>0){print(missing_data); stop("Some variables required for GLMDI are missing.")}

df_lmdi <- df_lmdi_raw %>%
  pivot_wider(names_from=VEMF, values_from=Value) %>%
  mutate(Region=recode(REMF,"Rrecipient15th"="Recipient","Rprovider15th"="Provider"),
         Region=factor(Region,levels=c("Provider","Recipient")),
         Scenario=recode(SCENARIO,"SSP2_400C_2030CP_NoCC_No"="Def",
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
  if(!is.finite(a)||!is.finite(b)||a<=0||b<=0) stop("Logarithmic mean requires positive finite values.")
  if(isTRUE(all.equal(a,b))) return(a)
  (a-b)/(log(a)-log(b))
}

run_glmdi <- function(data_region){
  base <- data_region %>% filter(Scenario=="Def")
  target <- data_region %>% filter(Scenario=="Aid")
  if(nrow(base)!=1||nrow(target)!=1) stop("Each region must contain one Def and one Aid observation.")

  gross0 <- base$Gro_Emi_CO2[[1]]; gross1 <- target$Gro_Emi_CO2[[1]]
  rem0 <- base$Removal[[1]]; rem1 <- target$Removal[[1]]
  f0 <- c(GDP=base$GDP[[1]],EI=base$EI[[1]],Ele=base$Ele[[1]],CI=base$CI[[1]])
  f1 <- c(GDP=target$GDP[[1]],EI=target$EI[[1]],Ele=target$Ele[[1]],CI=target$CI[[1]])
  effects <- c(log_mean(gross1,gross0)*log(f1/f0),Removal=rem1-rem0)
  net0 <- gross0+rem0; net1 <- gross1+rem1
  residual <- net1-net0-sum(effects)
  if(abs(residual)>1e-6*max(1,abs(net0),abs(net1)))
    warning(paste0("GLMDI residual: ",format(residual,scientific=TRUE)))

  before <- net0+c(0,cumsum(head(effects,-1))); after <- before+effects
  bind_rows(
    tibble(Step="Start",Value=net0,ymin=pmin(0,net0),ymax=pmax(0,net0),EndLevel=net0),
    tibble(Step=names(effects),Value=as.numeric(effects),ymin=pmin(before,after),
           ymax=pmax(before,after),EndLevel=after),
    tibble(Step="End",Value=net1,ymin=pmin(0,net1),ymax=pmax(0,net1),EndLevel=net1)
  )
}

step_order <- c("Start","GDP","EI","Ele","CI","Removal","End")
wf <- df_lmdi %>% group_by(Region) %>% group_modify(~run_glmdi(.x)) %>% ungroup() %>%
  mutate(Step=factor(Step,levels=step_order),x=match(as.character(Step),step_order),
         change_label=if_else(Step %in% c("Start","End"),
                              format(round(Value,0),big.mark=","),sprintf("%+.0f",Value)),
         label_y=if_else(Value>=0,ymax,ymin),label_vjust=if_else(Value>=0,-0.45,1.25))

connectors <- wf %>% filter(Step!="End") %>%
  transmute(Region,x=x+0.4,xend=x+0.6,y=EndLevel,yend=EndLevel)

cols_lmdi <- c("GDP"="#D73027","EI"="#4575B4","Ele"="#FDAE61","CI"="#1A9850",
               "Removal"="#542788","Start"="grey45","End"="black")

g_lmdi <- ggplot(wf) +
  geom_hline(yintercept=0,linewidth=0.8,color="black",alpha=0.7) +
  geom_segment(data=connectors,aes(x=x,xend=xend,y=y,yend=yend),
               inherit.aes=FALSE,color="grey55",linewidth=0.5) +
  geom_rect(aes(xmin=x-0.4,xmax=x+0.4,ymin=ymin,ymax=ymax,fill=Step)) +
  geom_text(aes(x=x,y=label_y,label=change_label,vjust=label_vjust),
            size=4,fontface="bold") +
  scale_x_continuous(breaks=c(1,7),labels=c("Def","Aid"),
                     limits=c(0.5,7.5),expand=expansion(mult=c(0,0))) +
  scale_fill_manual(values=cols_lmdi,breaks=c("GDP","EI","Ele","CI","Removal"),
                    labels=c("GDP"="Economic activity","EI"="Energy intensity",
                             "Ele"="Electrification","CI"="Carbon intensity",
                             "Removal"="Carbon removal"),name="Drivers") +
  facet_wrap(~Region,nrow=1,scales="free_y") +
  labs(x=NULL,y="Net CO₂ emissions and contributions (MtCO₂ in 2050)") +
  coord_cartesian(clip="off") +
  theme_minimal(base_size=13) +
  theme(panel.grid.minor=element_blank(),panel.grid.major.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.x=element_text(size=11,face="bold"),
        strip.text=element_text(size=13,face="bold"),legend.position="right",
        legend.title=element_text(face="bold"))

plot(g_lmdi)

ggsave(file.path(output_dir,"LMDI_2050.png"),
       g_lmdi,width=14,height=7,dpi=600)
