library(dplyr)
library(tidyr)
library(ggplot2)

#' 分析結果リストからRMSEデータを抽出してデータフレームにまとめる関数
#' 
#' @param mrm_res mrm_cv関数の実行結果
#' @param plsr_res plsr_cv関数の実行結果
#' @return グラフ描画用のデータフレーム
compile_rmse_data <- function(mrm_res, plsr_res) {
  summary_list <- list()
  idx <- 1
  
  # 重回帰の結果抽出
  for (domain in names(mrm_res)) {
    for (resp in names(mrm_res[[domain]])) {
      summary_list[[idx]] <- data.frame(
        Method = "MRM",
        Domain_Key = domain,
        Response = resp,
        RMSE = mrm_res[[domain]][[resp]]$k_fold_rmse_mean # 平均値のみ
      )
      idx <- idx + 1
    }
  }
  
  # PLSRの結果抽出
  for (domain in names(plsr_res)) {
    for (resp in names(plsr_res[[domain]])) {
      summary_list[[idx]] <- data.frame(
        Method = "PLSR",
        Domain_Key = domain,
        Response = resp,
        RMSE = plsr_res[[domain]][[resp]]$k_fold_rmse_mean
      )
      idx <- idx + 1
    }
  }
  
  # データ整形
  final_df <- do.call(rbind, summary_list) |>
    mutate(
      Domain = case_when(
        Domain_Key == "old_models" ~ "Building",
        Domain_Key == "new_models" ~ "Road",
        Domain_Key == "all_models" ~ "Integrated"
      ),
      Domain = factor(Domain, levels = c("Building", "Road", "Integrated"))
    )
  
  return(final_df)
}