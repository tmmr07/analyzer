library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(caret)
library(e1071)  # SVM/SVR

#' SVRモデルを構築し、CSVから読み込んだ最適パラメータでk分割交差検証
#'
#' @param data_simulation 各エージェントにおけるシミュレーション結果のデータ
#' @param old_features 旧指標群のデータ
#' @param new_features 新指標群のデータ
#' @param all_features 総合指標群のデータ
#' @param agent_type エージェントの種類（"distributed" または "concentration"）
#' @param output_path 結果を出力するファイルパス
#' @param k 分割交差検証の分割数（デフォルト10）
#'
#' @return 各目的変数ごとの各フォールドRMSEと使用したパラメータを格納したリスト
svr_cv <- function(data_simulation, old_features, new_features, all_features, agent_type, output_path, k) {
  
  # CSVパスをエージェント種別で決定
  if (agent_type == "distributed") {
    write_lines("----------------------分散救助エージェントのSVR交差検証----------------------", output_path, append = TRUE)
    old_param_csv <- "./out/svr_distributed_old_param.csv"
    new_param_csv <- "./out/svr_distributed_new_param.csv"
    all_param_csv <- "./out/svr_distributed_all_param.csv"
  } else {
    write_lines("----------------------集中救助エージェントのSVR交差検証----------------------", output_path, append = TRUE)
    old_param_csv <- "./out/svr_concentration_old_param.csv"
    new_param_csv <- "./out/svr_concentration_new_param.csv"
    all_param_csv <- "./out/svr_concentration_all_param.csv"
  }
  
  # パラメータCSVを読み込み
  old_param_table <- read_csv(old_param_csv, show_col_types = FALSE)
  new_param_table <- read_csv(new_param_csv, show_col_types = FALSE)
  all_param_table <- read_csv(all_param_csv, show_col_types = FALSE)
  
  # 特徴量名
  old_predictor_names <- colnames(old_features |> dplyr::select(-all_of("Map")))
  new_predictor_names <- colnames(new_features |> dplyr::select(-all_of("Map")))
  all_predictor_names <- colnames(all_features |> dplyr::select(-all_of("Map")))
  
  # 目的変数
  response_variable_names <- data_simulation |>
    dplyr::select(-all_of(c("map", "agent"))) |>
    dplyr::select(where(is.numeric)) |>
    colnames()
  
  # 内部関数：CV実行
  run_svr_cv <- function(model_data, predictor_names, param_table) {
    purrr::map(response_variable_names, ~{
      response_var <- .x
      formula_str <- paste(response_var, "~", paste(predictor_names, collapse = " + "))
      model_formula <- as.formula(formula_str)
      
      # CSVから目的変数の最適パラメータ取得
      param_row <- param_table %>% filter(response_variable == response_var)
      best_C <- param_row$cost
      best_sigma <- param_row$gamma
      
      n <- nrow(model_data)
      folds <- sample(rep(1:k, length.out = n))
      rmse_list <- c()
      
      for(i in 1:k){
        test_idx <- which(folds == i)
        train_data <- model_data[-test_idx, ]
        test_data  <- model_data[test_idx, ]
        
        svr_model <- svm(
          model_formula,
          data = train_data,
          kernel = "radial",
          cost = best_C,
          gamma = best_sigma,
          scale = TRUE
        )
        
        pred <- predict(svr_model, newdata = test_data)
        actual <- test_data[[response_var]]
        rmse_list <- c(rmse_list, sqrt(mean((pred - actual)^2, na.rm = TRUE)))
      }
      
      # 結果をファイルに出力
      write_lines("", output_path, append = TRUE)
      write_lines(paste("---目的変数：", response_var, "---"), output_path, append = TRUE)
      write_lines("使用パラメータ (CSVから読み込み)", output_path, append = TRUE)
      write_lines(paste("C =", best_C, ", sigma =", best_sigma), output_path, append = TRUE)
      write_lines(paste("各フォールドRMSE:", paste(round(rmse_list,3), collapse = ", ")), output_path, append = TRUE)
      write_lines(paste("平均RMSE:", round(mean(rmse_list),3)), output_path, append = TRUE)
      
      return(list(
        used_params = list(C = best_C, sigma = best_sigma),
        k_fold_rmse = rmse_list,
        k_fold_rmse_mean = mean(rmse_list)
      ))
    }) |> set_names(response_variable_names)
  }
  
  # 旧指標群
  model_data_old <- left_join(data_simulation, old_features, by = c("map" = "Map"))
  write_lines("旧指標群でのSVR交差検証", output_path, append = TRUE)
  svr_old <- run_svr_cv(model_data_old, old_predictor_names, old_param_table)
  
  # 新指標群
  model_data_new <- left_join(data_simulation, new_features, by = c("map" = "Map"))
  write_lines("新指標群でのSVR交差検証", output_path, append = TRUE)
  svr_new <- run_svr_cv(model_data_new, new_predictor_names, new_param_table)
  
  # 総合指標群
  model_data_all <- left_join(data_simulation, all_features, by = c("map" = "Map"))
  write_lines("総合指標群でのSVR交差検証", output_path, append = TRUE)
  svr_all <- run_svr_cv(model_data_all, all_predictor_names, all_param_table)
  
  # 終了ログ
  if (agent_type == "distributed") {
    write_lines("----------------------分散救助エージェントのSVR交差検証終了----------------------", output_path, append = TRUE)
  } else {
    write_lines("----------------------集中救助エージェントのSVR交差検証終了----------------------", output_path, append = TRUE)
  }
  
  return(list(
    old_models = svr_old,
    new_models = svr_new,
    all_models = svr_all
  ))
}
