library(dplyr)
library(tibble)
library(caret)
library(readr)
library(corrplot)

#' 特徴量選択により指標を選定する関数
#' @param features_data 特徴指標データ
#' @param simulation_data シミュレーション結果データ
#' @param output_path 結果を出力するファイルパス
#' @param correlation_cutoff 相関係数の閾値
#' @param data_type 使用する特徴指標データの種類
#' @return 選択された特徴指標データ

feature_selection <- function(features_data, simulation_data, output_path, correlation_cutoff, data_type) {
  
  if (data_type == "new") {
    write_lines("----------------------新指標群の特徴量選択を実行----------------------", output_path, append = TRUE)
  } else if (data_type == "all") {
    write_lines("----------------------総合指標群の特徴量選択を実行----------------------", output_path, append = TRUE)
  }
  
  # Map列を残しつつ数値列のみ抽出
  feature_data <- features_data |> dplyr::select(Map, where(is.numeric))
  feature_names <- colnames(feature_data)[-1]
  
  response_data <- simulation_data |> dplyr::select(map, where(is.numeric))
  response_names <- colnames(response_data)[colnames(response_data) != "map"]
  
  selected_features_list <- list()
  
  # 各目的変数ごとに処理
  for (resp in response_names) {
    Y <- response_data[[resp]]
    X <- feature_data |> dplyr::select(-Map)
    
    # 相関行列と高相関の特徴量検出
    cor_matrix <- cor(X)
    highly_correlated_cols <- caret::findCorrelation(cor_matrix, cutoff = correlation_cutoff)
    
    if (length(highly_correlated_cols) > 0) {
      X_filtered <- X[, -highly_correlated_cols, drop = FALSE]
    } else {
      X_filtered <- X
    }
    
    # 線形回帰でRMSEを計算
    model_before <- caret::train(x = X, y = Y, method = "lm")
    model_after  <- caret::train(x = X_filtered, y = Y, method = "lm")
    
    rmse_table <- tibble(
      Response = resp,
      Num_Features_Before = ncol(X),
      Num_Features_After = ncol(X_filtered),
      RMSE_Before = min(model_before$results$RMSE),
      RMSE_After = min(model_after$results$RMSE)
    )
    
    # 出力
    write_lines(paste0("\n=== ", resp, " の相関フィルターとRMSE ==="), output_path, append = TRUE)
    write_lines(capture.output(print(rmse_table)), output_path, append = TRUE)
    
    selected_features_list[[resp]] <- colnames(X_filtered)
  }
  
  # 全目的変数で残った特徴量の共通部分を選択
  selected_features <- Reduce(intersect, selected_features_list)
  
  selected_features_with_map <- feature_data %>% dplyr::select(Map, all_of(selected_features))
  
  # 選択された特徴量を出力
  write_lines("\n=== 選択された特徴量 ===", output_path, append = TRUE)
  write_lines(selected_features, output_path, append = TRUE)
  
  if (data_type == "new") {
    write_lines("----------------------新指標群の特徴量選択を終了----------------------", output_path, append = TRUE)
    write_lines("", output_path, append = TRUE)
    write_lines("", output_path, append = TRUE)
    write_lines("", output_path, append = TRUE)
    write_lines("", output_path, append = TRUE)
  } else if (data_type == "all") {
    write_lines("----------------------総合指標群の特徴量選択を終了----------------------", output_path, append = TRUE)
  }
  
  return(selected_features_with_map)
}
