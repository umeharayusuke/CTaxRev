##Figure1-------------------------------------------------------
g3_noleg <- ggdraw() +
  draw_plot(
    g3_1 + theme(legend.position='none'),
    x = 0,
    y = 0,
    width = 0.5,
    height = 1
  ) +
  draw_plot(
    g3_2 + theme(legend.position='none'),
    x = 0.5,
    y = 0,
    width = 0.5,
    height = 1
  ) +
  draw_plot_label(
    label = c('b', 'c'),
    x = c(0, 0.5),
    y = c(1, 1),
    size = 14
  )

final_plot <- ggdraw() +
  draw_plot(
    g2_noleg,
    x = 0,
    y = 0.5,
    width = 0.82,
    height = 0.5
  ) +
  draw_plot_label(
    label = "a",
    x = 0,
    y = 1,
    size = 14
  ) +
  draw_plot(
    g3_legend,
    x = 0.82,
    y = 0.58,
    width = 0.18,
    height = 0.32
  ) +
  draw_plot(
    g3_noleg,
    x = 0,
    y = 0,
    width = 1,
    height = 0.5
  )
plot(final_plot)

ggsave(
  filename = file.path(output_dir, "IAMC1.png"),
  plot = final_plot,
  width = 14,
  height = 12,
  units = "in",
  dpi = 300,
  bg = "white"
)

##Figure2--------------------------------------------------------------
g7_1_nolegend <- g7_1 + theme(legend.position = "none")
g7_2_nolegend <- g7_2 + theme(legend.position = "none")
g7_stack <- plot_grid(
  g7_1_nolegend,
  g7_2_nolegend,
  ncol = 2,
  align = "h",
  labels = c("c", "d"),
  label_size = 14
)
g4_clean <- plot_grid(
  g4_1 + theme(legend.position = "none"),
  g4_2 + theme(legend.position = "none"),
  ncol = 2,
  align = "h",
  labels = c("a", "b"),
  label_size = 14
)
main_plot <- plot_grid(
  g4_clean,
  g7_stack,
  ncol = 1,
  rel_heights = c(1, 1.1)
)
g4_1_l <- get_legend(g4_1 + theme(legend.position = "right"))
g4_2_l <- get_legend(g4_2 + theme(legend.position = "right"))
g7_1_l <- get_legend(g7_1 + theme(legend.position = "right"))
legend_col <- plot_grid(
  g4_1_l,
  g4_2_l,
  g7_1_l,
  ncol = 1,
  rel_heights = c(1, 1, 1)
)
final_plot <- plot_grid(
  main_plot,
  legend_col,
  ncol = 2,
  rel_widths = c(0.82, 0.18)
)
plot(final_plot)

ggsave(
  filename = file.path(output_dir, "IAMC2.png"),
  plot = final_plot,
  width = 14,
  height = 12,
  units = "in",
  dpi = 300,
  bg = "white"
)