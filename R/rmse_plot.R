#' RMSEの比較グラフを描画する関数
#' 
#' @param comparison_df compile_rmse_dataで作成したデータフレーム
#' @param title グラフのタイトル（例: "分散エージェント"）
#' @return ggplotオブジェクト
plot_rmse_comparison <- function(df, target_method = "PLSR", title_prefix = "") {
  
  # 指定された手法だけをフィルタリング
  plot_data <- df |> dplyr::filter(Method == target_method)
  
  g <- ggplot(plot_data, aes(x = Domain, y = RMSE, fill = Domain)) +
    
    # シンプルな棒グラフ
    geom_col(alpha = 0.8, width = 0.7) +
    
    # 数値をグラフ上に表示（これがあると分かりやすいです）
    geom_text(aes(label = signif(RMSE, 3)), vjust = -0.5, size = 3) +
    
    # 目的変数ごとにパネルを分割（Y軸のスケールを個別に設定）
    facet_wrap(~ Response, scales = "free_y", ncol = 3) +
    
    # 色設定
    scale_fill_manual(values = c("Building" = "#999999", "Road" = "#E69F00", "Integrated" = "#56B4E9")) +
    
    # ラベルとテーマ
    labs(
      title = paste0(title_prefix, " (", target_method, ") 平均RMSE比較"),
      y = "平均 RMSE",
      x = NULL
    ) +
    theme_bw() +
    theme(
      legend.position = "top",
      axis.text.x = element_blank(), # X軸の文字は凡例と同じなので消す
      axis.ticks.x = element_blank()
    )
  
  return(g)
}