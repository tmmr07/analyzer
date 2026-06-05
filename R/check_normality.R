library(ggplot2)
library(tidyr)

#' 正規性の確認と対応のあるt検定を行い、結果を保存する関数
#' @param y 実測値ベクトル
#' @param pred_old 旧モデルの予測値ベクトル
#' @param pred_all 総合モデルの予測値ベクトル
#' @param target_name 目的変数名
#' @param agent_name エージェント名
#' @param output_dir 出力先のベースディレクトリ
save_normality_check <- function(y, pred_old, pred_all, target_name, agent_name, output_dir = "./out") {
  
  # --- 0. 準備 ---
  save_dir <- file.path(output_dir, "normality_diff") # フォルダ名も少し変えました
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  
  # ベクトル化
  y_vec <- as.numeric(y)
  old_vec <- as.numeric(pred_old)
  all_vec <- as.numeric(pred_all)
  
  # --- 1. 計算ロジック (ここが重要) ---
  # t検定(RMSE比較)の対象となる「二乗誤差の差」を作ります
  # プラスの値 = Oldの方が誤差が大きい = 「改善した」
  # マイナスの値 = Allの方が誤差が大きい = 「改悪した」
  sq_err_old <- (y_vec - old_vec)^2
  sq_err_all <- (y_vec - all_vec)^2
  diff_sq_err <- sq_err_old - sq_err_all
  
  # ファイル名の共通プレフィックス
  file_prefix <- paste0(agent_name, "_", target_name)
  
  # --- 2. ヒストグラムの保存 (改善量の分布) ---
  df_plot <- data.frame(Diff = diff_sq_err)
  
  g_hist <- ggplot(df_plot, aes(x = Diff)) +
    geom_histogram(fill = "steelblue", color = "white", alpha = 0.7, bins = 20) +
    # 0のライン（改善なし）を赤線で引く
    geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
    labs(
      title = paste0(agent_name, " ： ", target_name),
      x = "予測誤差の差（旧の誤差 - 総合の誤差）", 
      y = "度数"
    ) +
    theme_bw(base_size = 14)
  
  ggsave(file.path(save_dir, paste0("hist_diff_", file_prefix, ".png")), g_hist, width = 8, height = 6)
  
  # --- 3. Q-Qプロットの保存 (差分のみ) ---
  png_filename <- file.path(save_dir, paste0("qq_diff_", file_prefix, ".png"))
  
  png(png_filename, width = 600, height = 600) # 正方形でOK
  
  qqnorm(diff_sq_err, main = paste("Q-Q Plot of Diff:", target_name)); 
  qqline(diff_sq_err, col = "red", lwd = 2)
  
  dev.off() 
  
  # --- 4. 正規性検定 (Shapiro-Wilk) ---
  sw_diff <- shapiro.test(diff_sq_err) 
  
  # --- 5. 対応のあるt検定 (Paired t-test) ---
  # alternative = "greater" : 差分の平均 > 0 (改善) を検定
  # t.test(diff_sq_err, alternative = "greater") と書いても同じ意味です
  t_res <- t.test(sq_err_old, sq_err_all, paired = TRUE, alternative = "greater")
  
  # --- 6. 結果をテキストファイルに保存 ---
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