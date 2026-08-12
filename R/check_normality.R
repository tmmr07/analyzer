# ライブラリの読み込み
library(ggplot2)
library(tidyr)

#' 正規性の確認と対応のあるt検定を行い，結果を保存する関数
#' @param y 実測値
#' @param pred_old 旧モデルの予測値
#' @param pred_all 総合モデルの予測値
#' @param target_name 目的変数名
#' @param agent_name エージェント名
#' @param output_dir 出力先のベースディレクトリ
save_normality_check <- function(y, pred_old, pred_all, target_name, agent_name, output_dir = "./out") {
  # 結果の保存先ディレクトリを指定
  save_dir <- file.path(output_dir, "normality_diff")
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  
  # 実測値と予測値を数値ベクトルに変換
  y_vec <- as.numeric(y)
  old_vec <- as.numeric(pred_old)
  all_vec <- as.numeric(pred_all)
  
  # 二乗誤差の計算
  sq_err_old <- (y_vec - old_vec)^2
  sq_err_all <- (y_vec - all_vec)^2
  # 二乗誤差の差を計算（=改善量の算出）
  diff_sq_err <- sq_err_old - sq_err_all
  
  # ファイル名の共通プレフィックスを定義
  file_prefix <- paste0(agent_name, "_", target_name)
  
  # Diffという列名のデータフレームを作成
  df_plot <- data.frame(Diff = diff_sq_err)
  
  # ヒストグラムの作成
  g_hist <- ggplot(df_plot, aes(x = Diff)) +
    geom_histogram(fill = "steelblue", color = "white", alpha = 0.7, bins = 20) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
    labs(
      title = paste0(agent_name, " ： ", target_name),
      x = "予測誤差の差（旧モデルの誤差 - 総合モデルの誤差）", 
      y = "度数"
    ) +
    theme_bw(base_size = 14)
  
  # 作成した画像をPNG形式で保存
  ggsave(file.path(save_dir, paste0("hist_diff_", file_prefix, ".png")), g_hist, width = 8, height = 6)
  
  # Q-Qプロット
  qq_png_name <- file.path(save_dir, paste0("qq_diff_", file_prefix, ".png"))
  png(qq_png_name, width = 600, height = 600)
  qqnorm(diff_sq_err, main = paste("Q-Q Plot of Diff:", target_name)); 
  qqline(diff_sq_err, col = "red", lwd = 2)
  dev.off() 
  
  # シャピロ・ウィルク検定
  sw_diff <- shapiro.test(diff_sq_err) 
  
  # 対応のあるt検定
  t_res <- t.test(sq_err_old, sq_err_all, paired = TRUE, alternative = "greater")
  
  # 結果をテキストファイルに保存
  txt_filename <- file.path(save_dir, "statistical_results_diff.txt")
  
  out_lines <- c(
    "==================================================",
    paste("Date:", Sys.time()),
    paste("Agent:", agent_name, "| Target:", target_name),
    "==================================================",
    "",
    "--- 1. Normality Check of Difference (Improvement) ---",
    "  Target Variable: Diff = (Old_SqError - All_SqError)",
    "  (p < 0.05 indicates non-normal distribution)",
    "",
    paste0("  Shapiro-Wilk W = ", round(sw_diff$statistic, 4)),
    paste0("  p-value        = ", format.pval(sw_diff$p.value, digits = 4)),
    "",
    "  [Judgement]",
    if(sw_diff$p.value >= 0.05) "  -> Diff is Normal. (t-test is valid)" else "  -> Diff is NOT Normal. (t-test is unreliable, use Wilcoxon)",
    "",
    "--- 2. Paired t-test Result ---",
    "  Hypothesis: Mean(Diff) > 0",
    paste0("  t-value: ", round(t_res$statistic, 4)),
    paste0("  df:      ", round(t_res$parameter, 4)),
    paste0("  p-value: ", format.pval(t_res$p.value, digits = 4)),
    "",
    "  [Conclusion]",
    if(t_res$p.value < 0.05) "  -> Significant Improvement (p < 0.05)" else "  -> NO Significant Improvement",
    "",
    "" 
  )
  
  cat(paste(out_lines, collapse = "\n"), file = txt_filename, append = TRUE)
  
  message(paste("Saved diff analysis for:", file_prefix))
}