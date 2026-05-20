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

# =========================
# ② g4（凡例なし・a,b）
# =========================

g4_clean <- plot_grid(
  g4_1 + theme(legend.position = "none"),
  g4_2 + theme(legend.position = "none"),
  ncol = 2,
  align = "h",
  labels = c("a", "b"),
  label_size = 14
)

# =========================
# ③ g4 + g7 統合（メイン図）
# =========================

main_plot <- plot_grid(
  g4_clean,
  g7_stack,
  ncol = 1,
  rel_heights = c(1, 1.1)
)

# =========================
# ④ 凡例3つだけ抽出
# =========================

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

# =========================
# ⑤ 最終統合
# =========================

final_plot <- plot_grid(
  main_plot,
  legend_col,
  ncol = 2,
  rel_widths = c(0.82, 0.18)
)

# =========================
# ⑥ 出力
# =========================

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