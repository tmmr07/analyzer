# パッケージの読み込み
library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(e1071)

#' SVRを構築する関数
#' 最適パラメータをCSVに出力
#'
#' @param data_simulation 各エージェントにおけるシミュレーション結果のデータ
#' @param old_features 旧指標群のデータ
#' @param new_features 新指標群のデータ
#' @param all_features 総合指標群のデータ
#' @param agent_type エージェントの種類（"distributed" または "concentration"）
#' @param output_path 結果を出力するファイルパス
#'
#' @return 作成したSVRを格納したリスト

make_svr <- function(data_simulation, old_features, new_features, all_features, agent_type, output_path) {
  
  # エージェント種別に応じてログとCSV出力先を設定
  if (agent_type == "distributed") {
    write_lines("----------------------分散救助エージェントのSVRを作成----------------------", output_path, append = TRUE)
    old_param_results_path <- "./out/svr_distributed_old_param.csv"
    new_param_results_path <- "./out/svr_distributed_new_param.csv"
    all_param_results_path <- "./out/svr_distributed_all_param.csv"
  } else if (agent_type == "concentration") {
    write_lines("----------------------集中救助エージェントのSVRを作成----------------------", output_path, append = TRUE)
    old_param_results_path <- "./out/svr_concentration_old_param.csv"
    new_param_results_path <- "./out/svr_concentration_new_param.csv"
    all_param_results_path <- "./out/svr_concentration_all_param.csv"
  }
  
  # 指標列だけ取得
  old_features_data <- old_features |> dplyr::select(-all_of("Map"))
  new_features_data <- new_features |> dplyr::select(-all_of("Map"))
  all_features_data <- all_features |> dplyr::select(-all_of("Map"))
  
  old_predictor_names <- colnames(old_features_data)
  old_predictor_formula_str <- paste(old_predictor_names, collapse = " + ")
  
  new_predictor_names <- colnames(new_features_data)
  new_predictor_formula_str <- paste(new_predictor_names, collapse = " + ")
  
  all_predictor_names <- colnames(all_features_data)
  all_predictor_formula_str <- paste(all_predictor_names, collapse = " + ")
  
  response_variable_names <- data_simulation |>
    dplyr::select(-all_of(c("map", "agent"))) |>
    dplyr::select(where(is.numeric)) |>
    colnames()
  
  svr_tune_ranges <- list(
    gamma = 2^(seq(-3.0, 0.01, length.out = 10)),
    cost = 10^(-1:1)
  )
  
  # 結果格納用データフレーム初期化
  old_param_results <- tibble(response_variable = character(),
                              gamma = numeric(),
                              cost = numeric())
  new_param_results <- tibble(response_variable = character(),
                              gamma = numeric(),
                              cost = numeric())
  all_param_results <- tibble(response_variable = character(),
                              gamma = numeric(),
                              cost = numeric())
  
  # 旧指標群データとシミュレーション結果データを結合
  model_data_old <- left_join(data_simulation, old_features, by = c("map" = "Map"))
  write_lines("\n旧指標群を説明変数としたSVRの構築", output_path, append = TRUE)
  
  # 旧指標群でループ
  model_list_old <- purrr::map(response_variable_names, ~{
    current_response_var <- .x
    old_formula_str <- paste(current_response_var, "~", old_predictor_formula_str)
    old_model_formula <- as.formula(old_formula_str)
    
    tune_result_old <- tune(
      svm,
      train.x = old_model_formula,
      data = model_data_old,
      ranges = svr_tune_ranges,
      type = "eps-regression",
      kernel = "radial",
      scale = TRUE,
      na.action = na.omit
    )
    
    # パラメータをデータフレームに追加
    old_param_results <<- bind_rows(old_param_results,
                                    tibble(response_variable = current_response_var,
                                           gamma = tune_result_old$best.parameters$gamma,
                                           cost = tune_result_old$best.parameters$cost))
    
    # 結果を出力
    model_summary_old <- capture.output(print(tune_result_old))
    write_lines(paste("\n---目的変数：", current_response_var, "---"), output_path, append = TRUE)
    write_lines("最適なパラメータ", output_path, append = TRUE)
    write_lines(capture.output(print(tune_result_old$best.parameters)), output_path, append = TRUE)
    write_lines("CV結果詳細", output_path, append = TRUE)
    write_lines(model_summary_old, output_path, append = TRUE)
    
    return(tune_result_old$best.model)
  }) |> set_names(response_variable_names)
  
  # 新指標群データとシミュレーション結果データを結合
  model_data_new <- left_join(data_simulation, new_features, by = c("map" = "Map"))
  write_lines("\n新指標群を説明変数としたSVRの構築", output_path, append = TRUE)
  
  # 新指標群でループ
  model_list_new <- purrr::map(response_variable_names, ~{
    current_response_var <- .x
    new_formula_str <- paste(current_response_var, "~", new_predictor_formula_str)
    new_model_formula <- as.formula(new_formula_str)
    
    tune_result_new <- tune(
      svm,
      train.x = new_model_formula,
      data = model_data_new,
      ranges = svr_tune_ranges,
      type = "eps-regression",
      kernel = "radial",
      scale = TRUE,
      na.action = na.omit
    )
    
    # パラメータをデータフレームに追加
    new_param_results <<- bind_rows(new_param_results,
                                    tibble(response_variable = current_response_var,
                                           gamma = tune_result_new$best.parameters$gamma,
                                           cost = tune_result_new$best.parameters$cost))
    
    # 結果を出力
    model_summary_new <- capture.output(print(tune_result_new))
    write_lines(paste("\n---目的変数：", current_response_var, "---"), output_path, append = TRUE)
    write_lines("最適なパラメータ", output_path, append = TRUE)
    write_lines(capture.output(print(tune_result_new$best.parameters)), output_path, append = TRUE)
    write_lines("CV結果詳細", output_path, append = TRUE)
    write_lines(model_summary_new, output_path, append = TRUE)
    
    return(tune_result_new$best.model)
  }) |> set_names(response_variable_names)
  
  # 総合指標群データとシミュレーション結果データを結合
  model_data_all <- left_join(data_simulation, all_features, by = c("map" = "Map"))
  write_lines("\n総合指標群を説明変数としたSVRの構築", output_path, append = TRUE)
  
  # 総合指標群でループ
  model_list_all <- purrr::map(response_variable_names, ~{
    current_response_var <- .x
    all_formula_str <- paste(current_response_var, "~", all_predictor_formula_str)
    all_model_formula <- as.formula(all_formula_str)
    
    tune_result_all <- tune(
      svm,
      train.x = all_model_formula,
      data = model_data_all,
      ranges = svr_tune_ranges,
      type = "eps-regression",
      kernel = "radial",
      scale = TRUE,
      na.action = na.omit
    )
    
    # パラメータをデータフレームに追加
    all_param_results <<- bind_rows(all_param_results,
                                    tibble(response_variable = current_response_var,
                                           gamma = tune_result_all$best.parameters$gamma,
                                           cost = tune_result_all$best.parameters$cost))
    
    # 結果を出力
    model_summary_all <- capture.output(print(tune_result_all))
    write_lines(paste("\n---目的変数：", current_response_var, "---"), output_path, append = TRUE)
    write_lines("最適なパラメータ", output_path, append = TRUE)
    write_lines(capture.output(print(tune_result_all$best.parameters)), output_path, append = TRUE)
    write_lines("CV結果詳細", output_path, append = TRUE)
    write_lines(model_summary_all, output_path, append = TRUE)
    
    return(tune_result_all$best.model)
  }) |> set_names(response_variable_names)
  
  # パラメータCSVに出力
  write_csv(old_param_results, old_param_results_path)
  write_csv(new_param_results, new_param_results_path)
  write_csv(all_param_results, all_param_results_path)
  
  if (agent_type == "distributed") {
    write_lines("----------------------分散救助エージェントのSVR作成終了----------------------", output_path, append = TRUE)
  } else if (agent_type == "concentration") {
    write_lines("----------------------集中救助エージェントのSVR作成終了----------------------", output_path, append = TRUE)
  }
  
  old_and_new_and_all_models_list <- list(
    old_models = model_list_old,
    new_models = model_list_new,
    all_models = model_list_all
  )

	# 作成したSVRのリストを返す
	return(old_and_new_and_all_models_list)
}