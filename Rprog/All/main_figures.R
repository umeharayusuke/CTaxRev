prepare_transfer_figure_data <- function(project_root) {
  message("Preparing transfer-figure source data ...")
  evaluate_script_prefix(
    project_root = project_root,
    script_name = "transfer-benefit.R",
    stop_pattern = "^# Per-year linear slopes",
    working_dir = project_root
  )
  evaluate_script_prefix(
    project_root = project_root,
    script_name = "Cumulative_transfer_zero_loss_2050.R",
    stop_pattern = "^# SSP and GDP-threshold sensitivity",
    working_dir = project_root
  )
  invisible(NULL)
}

generate_main_figures <- function(project_root, output_dirs) {
  build_figure(
    project_root,
    output_dirs,
    script_name = "Emission_CP_revenue_flow_composite.R",
    stop_pattern = "^png_file <-",
    plot_object = "figure_composite",
    filename_stem = "Fig2_Emission_CP_revenue_flow_composite",
    width_mm = 195,
    height_mm = 232
  )

  prepare_transfer_figure_data(project_root)
  build_figure(
    project_root,
    output_dirs,
    script_name = "Transfer_timing_recovery_zero_loss_gdp_four_panel.R",
    stop_pattern = "^output_dir <-",
    plot_object = "p_four_panel",
    filename_stem = "Fig3_transfer_timing_recovery_zero_loss_gdp_four_panel",
    width_mm = 180,
    height_mm = 174
  )

  build_figure(
    project_root,
    output_dirs,
    script_name = "Poverty_inequality_welfare_detailed_expenditure_2020_2050.R",
    stop_pattern = "^ggsave\\(",
    plot_object = "figure",
    filename_stem =
      "Fig4_poverty_inequality_welfare_detailed_expenditure_2020_2050",
    width_mm = 180,
    height_mm = 202
  )

  invisible(NULL)
}
