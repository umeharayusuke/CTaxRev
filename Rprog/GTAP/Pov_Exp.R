

df0 <- rgdx.param("AnalysisExpenditure.gdx", "PoVExp")

df1 <- df0 %>% 
  filter(R == "WLD") %>% 
  rename(Y = "Y") %>% 
  filter(TH == "pop_2.15") %>% 
  mutate(
    Ref = case_when(
      Ref == "SSP2_BaU_NoCC_No" ~ "BaU",          
      Ref == "SSP2_400C_2030CP_base_NoCC_No" ~ "No_Trs",
      Ref == "SSP2_400C_2030CP_GDP_NoCC_No" ~ "GDP_Trs",
      Ref == "SSP2_400C_2030CP_POP_NoCC_No" ~ "POP_Trs"
    ),
      Ref = factor(Ref, levels = c("BaU", "No_Trs", "GDP_Trs", "POP_Trs"))
  )

cpt <- function(data, title) {
  data0 <- data %>% mutate(Y = as.numeric(as.character(Y)))
  data1 <- data0 %>% filter(Y >= 2020, Y <= 2050, !is.na(Ref))
  
  ggplot(data1, aes(x = Y, y = PoVExp / 1e6, group = Ref, color = Ref)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    scale_x_continuous(breaks = seq(2020, 2050, by = 5)) +
    labs(
      y = "Poverty headcount (million)",x = "Year",color = "Scenario"
    ) +
    Mytheme
}

g7_1 <- cpt(df1, "2.15 $/day/capita")

ggsave("../../output/poverty.png", plot = g7_1, width = 12, height = 8, dpi = 300)


region_colors <- c(
  "OECD90+EU" = "#1b9e77",
  "Asia" = "#d95f02",
  "Former Soviet Union" = "#7570b3",
  "Middle East and Africa" = "#e7298a",
  "Latin America" = "#66a61e"
)

df_PoVExp <- rgdx.param("AnalysisExpenditure.gdx", "PoVExp")

scenario_mapping <- data.frame(
  scenario = c("SSP2_BaU_NoCC_No", 
               "SSP2_400C_2030CP_base_NoCC_No",
               "SSP2_400C_2030CP_GDP_NoCC_No", 
               "SSP2_400C_2030CP_POP_NoCC_No"),
  label = c("BaU", "No_Trs", "GDP_Trs", "POP_Trs"),
  stringsAsFactors = FALSE
)

region_labels <- c(
  "R5OECD90+EU" = "OECD90+EU",
  "R5ASIA" = "Asia",
  "R5LAM" = "Latin America",
  "R5REF" = "Former Soviet Union",
  "R5MAF" = "Middle East and Africa",
  "WLD" = "World"
)

df_2_15_2050 <- df_PoVExp %>%
  rename(Year = Y) %>%
  filter(TH == "pop_2.15", Year == 2050) %>%
  mutate(
    Ref = scenario_mapping$label[match(Ref, scenario_mapping$scenario)],
    Region = recode(R, !!!region_labels)
  ) %>%
  filter(!is.na(Ref), Region %in% region_labels) %>%
  mutate(PoVExp_Million = PoVExp / 1e6)


df_BaU <- df_2_15_2050 %>% filter(Ref == "BaU") %>% select(Region, PoVExp_Million)

target_refs <- c("No_Trs", "GDP_Trs", "POP_Trs")

df_diff <- map_dfr(target_refs, function(ref_label) {
  df_2_15_2050 %>%
    filter(Ref == ref_label) %>%
    left_join(df_BaU, by = "Region", suffix = c("", "_BaU")) %>%
    mutate(
      Ref = ref_label,
      value = PoVExp_Million - PoVExp_Million_BaU
    ) %>%
    select(Region, Ref, value) %>%
    filter(Region != "World")
})

df_diff$Ref <- factor(df_diff$Ref, levels = target_refs)


g7_2 <- ggplot(df_diff, aes(x = Ref, y = value, fill = Region)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = region_colors) +
  labs(x = "Reference", y = "Additional Poverty (million)") +
  guides(fill = guide_legend(ncol = 1)) +
  theme_1 


ggsave("../../output/Add_poverty.png", plot = g7_2, width = 12, height = 8, dpi = 300)

