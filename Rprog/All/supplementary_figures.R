generate_supplementary_figures <- function(project_root, output_dirs) {
  build_figure(
    project_root,
    output_dirs,
    script_name = "Sensitivity_GDP_per_capita_threshold_classification_2025.R",
    stop_pattern = "^output_dir <-",
    plot_object = "p",
    filename_stem =
      "SupplementaryFig1_GDP_per_capita_threshold_classification_2025",
    width_mm = 180,
    height_mm = 125
  )

  build_figure(
    project_root,
    output_dirs,
    script_name = "Transfer_emissions_efficiency_timing.R",
    stop_pattern = "^save_png\\($",
    plot_object = "p_emissions_combined",
    filename_stem =
      "SupplementaryFig2_transfer_emissions_equivalent_combined_2030_2050",
    width_mm = 180,
    height_mm = 88
  )

  build_figure(
    project_root,
    output_dirs,
    script_name = "SI_primary_energy_supply_3x3.R",
    stop_pattern = "^ggsave\\(",
    plot_object = "figure",
    filename_stem = "SupplementaryFig3_primary_energy_supply_2020_2050",
    width_mm = 180,
    height_mm = 180
  )

  build_figure(
    project_root,
    output_dirs,
    script_name = "SI_cumulative_consumption_gdp_loss_2050.R",
    stop_pattern = "^output_base <-",
    plot_object = "p_si",
    filename_stem =
      "SupplementaryFig4_cumulative_consumption_gdp_loss_2050",
    width_mm = 180,
    height_mm = 92
  )

  build_figure(
    project_root,
    output_dirs,
    script_name = "SSP2_additional_poverty_all_lines_2050.R",
    stop_pattern = "^ggsave\\(",
    plot_object = "p",
    filename_stem =
      "SupplementaryFig5_SSP2_additional_poverty_all_lines_2050",
    width_mm = 180,
    height_mm = function(env) env$figure_height
  )

  build_figure(
    project_root,
    output_dirs,
    script_name = "Sensitivity_world_population_GDP_SSP1_SSP3.R",
    stop_pattern = "^output_dir <-",
    plot_object = "p",
    filename_stem =
      "SupplementaryFig6_sensitivity_world_population_GDP_SSP1_SSP3",
    width_mm = 180,
    height_mm = 82
  )

  build_figure(
    project_root,
    output_dirs,
    script_name = "Sensitivity_provider_carbon_revenue_SSP1_SSP3.R",
    stop_pattern = "^output_dir <-",
    plot_object = "p",
    filename_stem =
      "SupplementaryFig7_sensitivity_provider_carbon_revenue_SSP1_SSP3",
    width_mm = 180,
    height_mm = 190
  )

  build_figure(
    project_root,
    output_dirs,
    script_name = "Sensitivity_cumulative_consumption_GDP_loss_thresholds.R",
    stop_pattern = "^output_dir <-",
    plot_object = "p",
    filename_stem = paste0(
      "SupplementaryFig8_",
      "sensitivity_cumulative_consumption_GDP_loss_thresholds"
    ),
    width_mm = 180,
    height_mm = 142
  )

  build_figure(
    project_root,
    output_dirs,
    script_name = "Sensitivity_poverty_population_SSP1_SSP3.R",
    stop_pattern = "^output_dir <-",
    plot_object = "p",
    filename_stem =
      "SupplementaryFig9_sensitivity_poverty_population_SSP1_SSP3",
    width_mm = 180,
    height_mm = 158
  )

  build_figure(
    project_root,
    output_dirs,
    script_name = "Sensitivity_lorenz_curves_2050_SSP1_SSP3.R",
    stop_pattern = "^output_dir <-",
    plot_object = "plot_lorenz",
    filename_stem =
      "SupplementaryFig10_sensitivity_lorenz_curves_2050_SSP1_SSP3",
    width_mm = 180,
    height_mm = 83
  )

  build_figure(
    project_root,
    output_dirs,
    script_name = "Sensitivity_inequality_welfare_2050_SSP1_SSP3.R",
    stop_pattern = "^ggsave\\(",
    plot_object = "figure",
    filename_stem =
      "SupplementaryFig11_sensitivity_inequality_welfare_2050_SSP1_SSP3",
    width_mm = 180,
    height_mm = 155
  )

  build_figure(
    project_root,
    output_dirs,
    script_name = "Sensitivity_welfare_vs_cumulative_consumption_loss_2050.R",
    stop_pattern = "^ggsave\\(",
    plot_object = "p",
    filename_stem = paste0(
      "SupplementaryFig12_",
      "welfare_vs_cumulative_consumption_loss_2050"
    ),
    width_mm = 150,
    height_mm = 112
  )

  build_figure(
    project_root,
    output_dirs,
    script_name = "SSP2_Gini_exp_change_boxplots_R5_2050.R",
    stop_pattern = "^ggsave\\(",
    plot_object = "p",
    filename_stem =
      "SupplementaryFig13_SSP2_Gini_exp_change_boxplots_R5_2050",
    width_mm = 180,
    height_mm = 125
  )

  invisible(NULL)
}
