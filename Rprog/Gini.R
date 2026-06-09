output_dir <- file.path("../..", "output")
n_split<-10

## Load data ----------------------------------------------------------

scenario_refs <- c("SSP2_BaU_NoCC_No",
                   "SSP2_400C_2030CP_POP_NoCC_No",
                  "SSP2_400C_2030CP_base_NoCC_No",
                  "SSP2_400C_2030CP_GDP_NoCC_No")
#Budget
Exp_n_split_seg <- rgdx.param(paste0("DecileResults_country_",n_split,".gdx"), "Budget", c("R", "Ref", "Y", "DEC", "Budget")) |> 
    filter(Ref %in% scenario_refs)
Exp_national_ave <- Exp_n_split_seg |> 
    filter(DEC == "Ave") |> 
    select(-DEC)
Exp_default_seg <- rgdx.param("ConsumptionResults.gdx", "BudgetInitial", c("R", "DEC", "Budget")) |> 
  select(-R) |> 
  distinct()
#Population
Pop_national_ave <- rgdx.param("Inputdata.gdx", "Population") |> 
  filter(Ref %in% scenario_refs)
Pop_default_seg <- rgdx.param("ConsumptionResults.gdx", "FreqSegall_exp", c("R", "Ref", "Y", "DEC", "Poprate")) |> 
  group_by(R, Ref, Y) |> 
  mutate(Poprate_norm = Poprate / sum(Poprate, na.rm = TRUE)) |> 
  ungroup() |> left_join(Pop_national_ave, c("R", "Ref", "Y")) |> 
  mutate(Population = Population * Poprate_norm) |> 
  select(-Poprate, -Poprate_norm)
## Budget-Population data----------------------------------------------
BP_national_ave <- Exp_national_ave |> left_join(Pop_national_ave, c("R", "Ref", "Y"))

BP_default_seg <- Pop_default_seg |> left_join(Exp_default_seg, c("DEC"))

cols_keep <- c("Ref", "Y", "Budget", "Population")
df_all <- list(Country = BP_national_ave,  Income = BP_default_seg) |> 
  map(~ select(.x, all_of(cols_keep))) |> 
  imap_dfr(~ .x |> mutate(source = .y)) |> 
  mutate(Budget = as.numeric(as.character(Budget)), Population = as.numeric(as.character(Population))) |> 
  filter(!is.na(Budget), !is.na(Population), Population > 0) |> 
  group_by(source, Ref, Y, Budget) |> 
  summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop")
## Lorenz curve----------------------------------------------------------
make_lorenz <- function(df) {
  df <- df |> 
    arrange(Budget) |> 
    mutate(income_total = Budget * Population, cum_pop = cumsum(Population), cum_income = cumsum(income_total), pop_share = cum_pop / sum(Population), income_share = cum_income / sum(income_total)) |> 
    select(Budget, Population, income_total, pop_share, income_share)
  bind_rows(tibble(Budget = 0, Population = 0, income_total = 0, pop_share = 0, income_share = 0), df)
}

df_lorenz <- df_all |> 
  group_by(source, Ref, Y) |> 
  group_modify(~ make_lorenz(.x)) |> 
  ungroup() |> 
  mutate(
    Ref = case_when(
      Ref == "SSP2_BaU_NoCC_No" ~ "BaU",
      Ref == "SSP2_400C_2030CP_base_NoCC_No" ~ "No_Trs",
      Ref == "SSP2_400C_2030CP_POP_NoCC_No" ~ "POP_Trs",
      Ref == "SSP2_400C_2030CP_GDP_NoCC_No" ~ "GDP_Trs",
      TRUE ~ Ref
    ),
    Ref = factor(Ref, levels = c("BaU", "No_Trs", "GDP_Trs", "POP_Trs"))
  )
## Gini coefficient----------------------------------------------------
calc_gini_from_lorenz <- function(df) {
  df <- df |> arrange(pop_share)
  if (nrow(df) < 2 || max(df$income_share, na.rm = TRUE) == 0) return(tibble(Gini = NA_real_))
  area <- sum(diff(df$pop_share) * (head(df$income_share, -1) + tail(df$income_share, -1)) / 2, na.rm = TRUE)
  tibble(Gini = 1 - 2 * area)
}

df_gini <- df_lorenz |> 
  group_by(source, Ref, Y) |> 
  group_modify(~ calc_gini_from_lorenz(.x)) |> 
  ungroup() |> 
  arrange(source, Y, Ref)
##output---------------------------------------------------------------
df_gini_gdx <- df_gini |>
  mutate(
    source = as.character(source),Ref = as.character(Ref),Y = as.character(Y),Gini = as.numeric(Gini)) |>
  filter(!is.na(Gini)) |> 
  rename(value=Gini)

m <- Container$new()
src <- Set$new(
  m,"src", records = data.frame(uni = unique(df_gini_gdx$source))
)
Ref <- Set$new(
  m,"Ref", records = data.frame(uni = unique(df_gini_gdx$Ref))
)
Y <- Set$new(
  m, "Y", records = data.frame(uni = unique(df_gini_gdx$Y))
)
Parameter$new(
  m, "Gini", domain = list(src, Ref, Y),records = df_gini_gdx |>
    rename(src = source) |>
    select(src, Ref, Y, value)
)
out_gdx <- file.path("gini_result.gdx")
m$write(out_gdx)
##visualization--------------------------------------------------------
df_lorenz_plot <- df_lorenz |> 
  mutate(Y_num = as.numeric(as.character(Y))) |> 
  filter(2030 <= Y_num) |> 
  filter(Y_num <= 2050)
df_gini_plot <- df_gini |> 
  mutate(Y_num = as.numeric(as.character(Y))) |> 
  filter(Y_num <= 2050)

p_lorenz <- ggplot(df_lorenz_plot, aes(x = pop_share, y = income_share, color = source, group = source)) + 
  geom_line(linewidth = 1) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") + 
  facet_grid(Y ~ Ref) + 
  labs(x = "Cumulative population share", y = "Cumulative income share", color = "Scenario") + 
  Mytheme
ggsave("../../output/Lorenz_curve.png", plot = p_lorenz, width = 12, height = 8, dpi = 300)

p_gini_trend <- ggplot(df_gini_plot, aes(x = Y_num, y = Gini, color = Ref, group = Ref)) + 
  geom_line(linewidth = 1) + 
  geom_point(size = 2) + 
  facet_grid(. ~ source) + 
  labs(x = "Year", y = "Gini coefficient", color = "Scenario") + 
  Mytheme
ggsave("../../output/Gini.png", plot = p_gini_trend, width = 12, height = 8, dpi = 300)

df_lorenz_plot2 <- df_lorenz |>
  mutate(Y_num = as.numeric(as.character(Y))) |>
  filter(Y_num %in% c(2030, 2040, 2050))

p_lorenz <- ggplot(
  df_lorenz_plot2, aes(x = pop_share, y = income_share, color = Ref, linetype = factor(Y_num),group = interaction(Ref, Y_num))) +
  geom_line(linewidth = 0.9) +
  geom_abline(intercept = 0, slope = 1,linetype = "dashed", color = "grey40") +
  facet_wrap(~ source, nrow = 1) +
  coord_equal() +
  labs(
    x = "Cumulative population share",
    y = "Cumulative income share",
    color = "Scenario",
    linetype = "Year"
  ) +
  Mytheme

plot(p_lorenz)

ggsave("../../output/Gini2.png", plot = p_lorenz, width = 12, height = 8, dpi = 300)
