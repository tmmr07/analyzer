library(dplyr)
library(effsize) # Cohen's d計算用

#' 2つのモデル間のRMSEについて対応のあるt検定を行う関数
#' 
#' @param results_A 比較対象Aの結果リスト（例: plsr_results$old_models）
#' @param results_B 比較対象Bの結果リスト（例: plsr_results$new_models）
#' @param name_A Aの名前（例: "Building"）
#' @param name_B Bの名前（例: "Road"）
#' @return 検定結果のデータフレーム
perform_ttest_rmse <- function(results_A, results_B, name_A = "Model A", name_B = "Model B") {
  
  # 共通の目的変数を取得
  common_responses <- intersect(names(results_A), names(results_B))
  
  stats_list <- list()
  idx <- 1
  
  for (resp in common_responses) {
    # 各FoldのRMSEベクトルを取得
    rmse_vec_A <- results_A[[resp]]$k_fold_rmse
    rmse_vec_B <- results_B[[resp]]$k_fold_rmse
    
    # ベクトル長が一致することを確認（対応のある検定のため必須）
    if (length(rmse_vec_A) != length(rmse_vec_B)) {
      warning(paste("Skipping", resp, ": Fold counts do not match."))
      next
    }
    
    # 1. 対応のあるt検定
    # alternative = "two.sided" (両側検定: 差があるかどうか)
    t_res <- t.test(rmse_vec_A, rmse_vec_B, paired = TRUE)
    
    # 2. 効果量 (Cohen's d)
    # Pairedなので、差分を用いて計算
    d_res <- cohen.d(rmse_vec_A, rmse_vec_B, paired = TRUE)
    
    # 3. 平均差 (A - B)
    # 正の値なら A > B (Aの方がRMSEが大きい = Bの方が精度が良い)
    mean_diff <- mean(rmse_vec_A) - mean(rmse_vec_B)
    
    # 勝者の判定
    winner <- "No Sig. Diff"
    if (t_res$p.value < 0.05) {
      if (mean_diff > 0) {
        winner <- name_B # Bの方が誤差が小さい
      } else {
        winner <- name_A # Aの方が誤差が小さい
      }
    }
    
    stats_list[[idx]] <- data.frame(
      Response = resp,
      Mean_RMSE_A = mean(rmse_vec_A),
      Mean_RMSE_B = mean(rmse_vec_B),
      Diff_Mean = mean_diff,     # 正ならBの勝ち
      t_statistic = t_res$statistic,
      p_value = t_res$p.value,
      Cohens_d = d_res$estimate, # 効果量の大きさ
      Winner_05 = winner,        # p<0.05基準での勝者
      stringsAsFactors = FALSE
    )
    idx <- idx + 1
  }
  
  # データフレーム化
  result_df <- do.call(rbind, stats_list)
  
  # 4. 多重検定の補正 (FDR: Benjamini-Hochberg法)
  result_df$p_adj_fdr <- p.adjust(result_df$p_value, method = "fdr")
  
  # 補正後の勝者判定
  result_df$Winner_Adj <- ifelse(result_df$p_adj_fdr < 0.05, 
                                 ifelse(result_df$Diff_Mean > 0, name_B, name_A), 
                                 "No Sig. Diff")
  
  return(result_df)
}