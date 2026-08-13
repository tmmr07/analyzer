library(readr)
library(dplyr)
library(stringr)
library(purrr)

#' 重回帰モデルを構築し、k分割交差検証（RMSE）をおこなう関数
#' 
#' @param data_simulation 各エージェントにおけるシミュレーション結果のデータ
#' @param old_features 選定済み旧指標群のデータ
#' @param new_features 選定済み新指標群のデータ
#' @param all_features 選定済み総合指標群のデータ
#' @param agent_type エージェントの種類（"distributed" または "concentration"）
#' @param output_path 結果を出力するファイルパス
#' @param k 分割交差検証の分割数
#' 
#' @return 作成した重回帰モデルと各フォールドのRMSEを格納したリスト

mrm_cv <- function(data_simulation, old_features, new_features, all_features, agent_type, output_path, k) {
  
  set.seed(123) # シード固定
  
  # ヘッダー出力
  agent_label <- if(agent_type == "distributed") "分散救助" else "集中救助"
  write_lines(paste0("----------------------", agent_label, "エージェントの重回帰モデルにおける交差検証----------------------"), output_path, append = TRUE)
  
  # データ準備（Map列除外）
  datasets <- list(
    old = list(data = old_features |> dplyr::select(-Map), label = "旧指標群"),
    new = list(data = new_features |> dplyr::select(-Map), label = "新指標群"),
    all = list(data = all_features |> dplyr::select(-Map), label = "総合指標群")
  )
  
  # 目的変数リスト
  response_vars <- data_simulation |> dplyr::select(where(is.numeric)) |> colnames()
  
  # 【重要修正】分割ルール（folds）をここで一回だけ作成し、全モデルで共有する
  n <- nrow(data_simulation)
  # mapの順序が変わらないよう、結合前のdata_simulationの行数に基づいて作成
  common_folds <- sample(rep(1:k, length.out = n))
  
  # 内部関数：交差検証実行ロジック
  run_mrm_cv <- function(features_data, label) {
    write_lines(paste0("\n\n\n", label, "での交差検証"), output_path, append = TRUE)
    
    # データ結合
    model_data <- left_join(data_simulation, features_data |> mutate(Map = data_simulation$map), by = c("map" = "Map"))
    predictors <- colnames(features_data)
    predictor_str <- paste(predictors, collapse = " + ")
    
    purrr::map(response_vars, ~{
      resp_var <- .x
      formula <- as.formula(paste(resp_var, "~", predictor_str))
      
      rmse_list <- c()
      
      for (i in 1:k) {
        # 共通のfoldsを使用
        test_idx <- which(common_folds == i)
        test_data <- model_data[test_idx, ]
        train_data <- model_data[-test_idx, ]
        
        # モデル構築と予測
        # singular.ok = TRUE はデフォルトですが明示しておくと安全です
        model <- lm(formula, data = train_data, na.action = na.omit)
        pred <- predict(model, newdata = test_data)
        actual <- test_data[[resp_var]]
        
        rmse <- sqrt(mean((pred - actual)^2, na.rm = TRUE))
        rmse_list <- c(rmse_list, rmse)
      }
      
      # ログ出力
      write_lines(paste0("\n---目的変数：", resp_var, "---"), output_path, append = TRUE)
      write_lines(paste("RMSE値:", paste(round(rmse_list, 3), collapse = ", "), 
                        " (平均:", round(mean(rmse_list), 3), ")"), output_path, append = TRUE)
      
      return(list(
        k_fold_rmse = rmse_list,
        k_fold_rmse_mean = mean(rmse_list)
      ))
    }) |> set_names(response_vars)
  }
  
  # 各指標群で実行
  results <- list(
    old_models = run_mrm_cv(datasets$old$data, datasets$old$label),
    new_models = run_mrm_cv(datasets$new$data, datasets$new$label),
    all_models = run_mrm_cv(datasets$all$data, datasets$all$label)
  )
  
  write_lines(paste0("\n----------------------", agent_label, "エージェントの重回帰モデルにおける交差検証完了----------------------"), output_path, append = TRUE)
  
  return(results)
}
