library(dplyr)
library(tibble)
library(car)
library(readr)

#' 分割サブセットでVIFを計算し、多重共線性を除去する関数
#' @param features 説明変数データ（Map列あり）
#' @param response 目的変数データ（map列と目的変数列を含む）
#' @param output_path 出力先ファイルパス
#' @param n_splits サブセットに分割する数
#' @param vif_threshold VIFの閾値（デフォルト10）
#' @return 多重共線性を除去した特徴量データ

check_vif_subsets <- function(features, response, output_path,
                              n_splits = 4, vif_threshold = 10) {
  
  # Map列を除いた説明変数名
  predictor_names <- colnames(features %>% select(-Map))
  n_vars <- length(predictor_names)
  
  # サブセットを作るインデックス
  split_indices <- split(1:n_vars, cut(1:n_vars, breaks = n_splits, labels = FALSE))
  
  # 各サブセットでVIFが高い変数を記録
  removed_vars_list <- vector("list", n_splits)
  
  for (i in seq_along(split_indices)) {
    idx <- split_indices[[i]]
    subset_vars <- predictor_names[idx]
    
    # サブセットの特徴量データを作成
    subset_features <- features %>% select(Map, all_of(subset_vars))
    
    # 目的変数データと結合
    model_data <- left_join(response, subset_features, by = c("map" = "Map")) %>%
      select(-map) %>%           # 文字列列を除外
      select(where(is.numeric))  # 数値列のみ
    
    current_vars <- subset_vars
    removed_vars <- c()
    
    while(TRUE) {
      formula_str <- paste(names(response)[2], "~", paste(current_vars, collapse = " + "))
      model <- lm(as.formula(formula_str), data = model_data)
      
      # 完全共線性の確認
      aliased_vars <- names(coef(model)[is.na(coef(model))])
      aliased_vars <- setdiff(aliased_vars, "(Intercept)")
      if(length(aliased_vars) > 0) {
        removed_vars <- c(removed_vars, aliased_vars)
        current_vars <- setdiff(current_vars, aliased_vars)
        next
      }
      
      vif_values <- vif(model)
      if(max(vif_values, na.rm = TRUE) < vif_threshold) break
      
      # 最大VIFの変数を除去
      max_var <- names(vif_values)[which.max(vif_values)]
      removed_vars <- c(removed_vars, max_var)
      current_vars <- setdiff(current_vars, max_var)
    }
    
    removed_vars_list[[i]] <- removed_vars
  }
  
  # 全サブセットで共通して除外される変数を決定
  common_removed <- Reduce(intersect, removed_vars_list)
  
  # 結果を出力
  write_lines("全サブセットで共通して除去された変数:", output_path)
  if(length(common_removed) > 0) {
    write_lines(common_removed, output_path, append = TRUE)
  } else {
    write_lines("なし", output_path, append = TRUE)
  }
  
  # 選定済み特徴量を返す
  selected_features <- features %>% select(-all_of(common_removed))
  
  return(selected_features)
}
