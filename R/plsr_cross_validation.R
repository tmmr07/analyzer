library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(pls)

#' PLSRモデルを構築し、k分割交差検証（RMSE）をおこなう関数
#' 
#' @param data_simulation 各エージェントにおけるシミュレーション結果のデータ
#' @param old_features 旧指標群のデータ
#' @param new_features 新指標群のデータ
#' @param all_features 総合指標群のデータ
#' @param agent_type エージェントの種類（"distributed" または "concentration"）
#' @param output_path 結果を出力するファイルパス
#' @param k 分割交差検証の分割数（デフォルト10）
#' @param max_ncomp_old 旧指標群の最大PLSR成分数
#' @param max_ncomp_new 新指標群の最大PLSR成分数
#' @param max_ncomp_all 総合指標群の最大PLSR成分数
#' 
#' @return 各目的変数ごとの各フォールドRMSEと成分数を格納したリスト

plsr_cv <- function(data_simulation, old_features, new_features, all_features,
                    agent_type, output_path, k, max_ncomp_old, max_ncomp_new, max_ncomp_all) {
  
  set.seed(123)
  
  agent_label <- if(agent_type == "distributed") "分散救助" else "集中救助"
  write_lines(paste0("----------------------", agent_label, "エージェントのPLSR交差検証----------------------"), output_path, append = TRUE)
  
  datasets <- list(
    old = list(data = old_features |> dplyr::select(-Map), max_comp = max_ncomp_old, label = "旧指標群"),
    new = list(data = new_features |> dplyr::select(-Map), max_comp = max_ncomp_new, label = "新指標群"),
    all = list(data = all_features |> dplyr::select(-Map), max_comp = max_ncomp_all, label = "総合指標群")
  )
  
  response_vars <- data_simulation |> dplyr::select(where(is.numeric)) |> colnames()
  
  # 【重要修正】ここでも共通の分割ルールを作成
  n <- nrow(data_simulation)
  common_folds <- sample(rep(1:k, length.out = n))
  
  # 内部関数
  run_plsr_cv <- function(features_data, max_ncomp, label) {
    write_lines(paste0("\n", label, "でのPLSR交差検証"), output_path, append = TRUE)
    
    model_data <- left_join(data_simulation, features_data |> mutate(Map = data_simulation$map), by = c("map" = "Map"))
    predictors <- colnames(features_data)
    predictor_str <- paste(predictors, collapse = " + ")
    
    purrr::map(response_vars, ~{
      resp_var <- .x
      formula <- as.formula(paste(resp_var, "~", predictor_str))
      
      rmse_list <- c()
      ncomp_list <- c()
      
      for (i in 1:k) {
        test_idx <- which(common_folds == i) # 共通foldsを使用
        test_data <- model_data[test_idx, ]
        train_data <- model_data[-test_idx, ]
        
        # あなたが実装した素晴らしいエラーハンドリングロジック（そのまま採用）
        valid_ncomp <- min(max_ncomp, nrow(train_data) - 1)
        valid_ncomp <- max(1, valid_ncomp)
        
        # Inner loop (LOO) で成分数決定
        model <- plsr(formula, data = train_data, ncomp = valid_ncomp,
                      scale = TRUE, validation = "LOO", na.action = na.omit)
        
        ncomp_selected <- selectNcomp(model, method = "onesigma", plot = FALSE)
        if (ncomp_selected == 0) ncomp_selected <- valid_ncomp
        
        ncomp_to_use <- min(ncomp_selected, model$ncomp)
        ncomp_to_use <- max(1, ncomp_to_use)
        ncomp_list <- c(ncomp_list, ncomp_to_use)
        
        # 予測
        pred <- predict(model, newdata = test_data, ncomp = ncomp_to_use)[,,1]
        actual <- test_data[[resp_var]]
        
        rmse <- sqrt(mean((pred - actual)^2, na.rm = TRUE))
        rmse_list <- c(rmse_list, rmse)
      }
      
      # ログ出力
      write_lines(paste0("---目的変数：", resp_var, "---"), output_path, append = TRUE)
      write_lines(paste("各フォールドRMSE:", paste(round(rmse_list,3), collapse=", ")), output_path, append = TRUE)
      write_lines(paste("各フォールド成分数:", paste(ncomp_list, collapse=", ")), output_path, append = TRUE)
      write_lines(paste("平均RMSE:", round(mean(rmse_list),3)), output_path, append = TRUE)
      
      return(list(
        k_fold_rmse = rmse_list,
        k_fold_ncomp = ncomp_list,
        k_fold_rmse_mean = mean(rmse_list),
        k_fold_ncomp_mean = mean(ncomp_list)
      ))
    }) |> set_names(response_vars)
  }
  
  results <- list(
    old_models = run_plsr_cv(datasets$old$data, datasets$old$max_comp, datasets$old$label),
    new_models = run_plsr_cv(datasets$new$data, datasets$new$max_comp, datasets$new$label),
    all_models = run_plsr_cv(datasets$all$data, datasets$all$max_comp, datasets$all$label)
  )
  
  write_lines("----------------------PLSR交差検証完了----------------------", output_path, append = TRUE)
  return(results)
}