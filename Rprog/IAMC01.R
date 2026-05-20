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
  
  # 上段 g2
  draw_plot(
    g2_noleg,
    x = 0,
    y = 0.5,
    width = 0.82,
    height = 0.5
  ) +
  
  # 上段ラベル a
  draw_plot_label(
    label = "a",
    x = 0,
    y = 1,
    size = 14
  ) +
  
  # 凡例
  draw_plot(
    g3_legend,
    x = 0.82,
    y = 0.58,
    width = 0.18,
    height = 0.32
  ) +
  
  # 下段
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