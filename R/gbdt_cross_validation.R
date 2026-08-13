library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(xgboost)

#' GBDTモデルを構築し、k分割交差検証（最適パラメータCSV使用）
#'
#' @param data_simulation 各エージェントにおけるシミュレーション結果のデータ
#' @param old_features 旧指標群のデータ
#' @param new_features 新指標群のデータ
#' @param all_features 総合指標群のデータ
#' @param agent_type エージェントの種類（"distributed" または "concentration"）
#' @param output_path 結果を出力するファイルパス
#' @param k 分割交差検証の分割数（デフォルト10）
#'
#' @return 各目的変数ごとの各フォールドRMSEを格納したリスト

gbdt_cv <- function(data_simulation, old_features, new_features, all_features,
                                    agent_type, output_path, k) {
  
  # CSVパスをエージェント種別で決定
  if (agent_type == "distributed") {
    write_lines("----------------------分散救助エージェントのGBDT交差検証----------------------", output_path, append = TRUE)
    old_param_csv <- "./out/gbdt_distributed_old_param.csv"
    new_param_csv <- "./out/gbdt_distributed_new_param.csv"
    all_param_csv <- "./out/gbdt_distributed_all_param.csv"
  } else {
    write_lines("----------------------集中救助エージェントのGBDT交差検証----------------------", output_path, append = TRUE)
    old_param_csv <- "./out/gbdt_concentration_old_param.csv"
    new_param_csv <- "./out/gbdt_concentration_new_param.csv"
    all_param_csv <- "./out/gbdt_concentration_all_param.csv"
  }
  
  # 指標列だけ取得
  old_features_data <- old_features |> dplyr::select(-all_of("Map"))
  new_features_data <- new_features |> dplyr::select(-all_of("Map"))
  all_features_data <- all_features |> dplyr::select(-all_of("Map"))
  
  old_predictor_names <- colnames(old_features_data)
  new_predictor_names <- colnames(new_features_data)
  all_predictor_names <- colnames(all_features_data)
  
  response_variable_names <- data_simulation |>
    dplyr::select(-any_of(c("map", "agent"))) |>
    dplyr::select(where(is.numeric)) |>
    colnames()
  
  # モデル用データ結合
  model_data_old <- left_join(data_simulation, old_features, by = c("map" = "Map"))
  model_data_new <- left_join(data_simulation, new_features, by = c("map" = "Map"))
  model_data_all <- left_join(data_simulation, all_features, by = c("map" = "Map"))
  
  # CSV読み込み
  old_param_df <- read_csv(old_param_csv, show_col_types = FALSE)
  new_param_df <- read_csv(new_param_csv, show_col_types = FALSE)
  all_param_df <- read_csv(all_param_csv, show_col_types = FALSE)
  
  run_cv_with_param <- function(model_data, predictor_names, param_df) {
    purrr::map(response_variable_names, ~{
      response_var <- .x
      n <- nrow(model_data)
      folds <- sample(rep(1:k, length.out = n))
      rmse_list <- c()
      
      # この目的変数の最適パラメータを取得
      param_row <- param_df |> filter(response_variable == response_var)
      if (nrow(param_row) == 0) stop(paste("パラメータが見つかりません:", response_var))
      
      # xgb.train用のパラメータリスト
      xgb_params <- list(
        objective = "reg:squarederror",
        max_depth = param_row$max_depth,
        min_child_weight = param_row$min_child_weight,
        colsample_bytree = param_row$colsample_bytree,
        subsample = param_row$subsample,
        eta = param_row$eta,
        gamma = param_row$gamma
      )
      nrounds <- param_row$nrounds
      
      for (i in 1:k) {
        test_idx <- which(folds == i)
        train_idx <- setdiff(seq_len(n), test_idx)
        
        train_data <- model_data[train_idx, ]
        test_data <- model_data[test_idx, ]
        
        dtrain <- xgb.DMatrix(data = as.matrix(train_data[, predictor_names]), label = train_data[[response_var]])
        dtest  <- xgb.DMatrix(data = as.matrix(test_data[, predictor_names]), label = test_data[[response_var]])
        
        model <- xgb.train(params = xgb_params, data = dtrain, nrounds = nrounds, verbose = 0)
        
        pred <- predict(model, dtest)
        actual <- test_data[[response_var]]
        rmse <- sqrt(mean((pred - actual)^2, na.rm = TRUE))
        rmse_list <- c(rmse_list, rmse)
      }
      
      write_lines(paste("---目的変数：", response_var, "---"), output_path, append = TRUE)
      write_lines(paste("各フォールドRMSE:", paste(round(rmse_list, 3), collapse = ", ")), output_path, append = TRUE)
      write_lines(paste("平均RMSE:", round(mean(rmse_list), 3)), output_path, append = TRUE)
      
      return(list(
        k_fold_rmse = rmse_list,
        k_fold_rmse_mean = mean(rmse_list)
      ))
    }) |> set_names(response_variable_names)
  }
  
  write_lines("旧指標群でのGBDT交差検証", output_path, append = TRUE)
  gbdt_old <- run_cv_with_param(model_data_old, old_predictor_names, old_param_df)
  
  write_lines("新指標群でのGBDT交差検証", output_path, append = TRUE)
  gbdt_new <- run_cv_with_param(model_data_new, new_predictor_names, new_param_df)
  
  write_lines("新指標群でのGBDT交差検証", output_path, append = TRUE)
  gbdt_all <- run_cv_with_param(model_data_all, all_predictor_names, all_param_df)
  
  write_lines("----------------------GBDT交差検証完了----------------------", output_path, append = TRUE)
  
  return(list(
    old_models = gbdt_old,
    new_models = gbdt_new,
    all_models = gbdt_all
  ))
}
