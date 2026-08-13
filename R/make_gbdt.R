# パッケージの読み込み
library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(xgboost)
library(caret)

#' GBDTを構築する関数
#' @param data_simulation 各エージェントにおけるシミュレーション結果のデータ
#' @param old_features 旧指標群のデータ
#' @param new_features 新指標群のデータ
#' @param all_features 総合指標群のデータ
#' @param agent_type エージェントの種類（"distributed"（分散救助エージェント）または "concentration"（集中救助エージェント））
#' @param output_path 結果を出力するファイルパス
#' @return 作成したGBDTを格納したリスト

make_gbdt <- function(data_simulation, old_features, new_features, all_features, agent_type, output_path) {

	if (agent_type == "distributed") {
		write_lines("----------------------分散救助エージェントのGBDTを作成----------------------", output_path, append = TRUE)
	  
	  # 最適パラメータの出力パス
	  old_param_results_path <- "./out/gbdt_distributed_old_param.csv"
	  new_param_results_path <- "./out/gbdt_distributed_new_param.csv"
	  all_param_results_path <- "./out/gbdt_distributed_all_param.csv"
	} else if (agent_type == "concentration") {
		write_lines("----------------------集中救助エージェントのGBDTを作成----------------------", output_path, append = TRUE)
	  
	  # 最適パラメータの出力パス
	  old_param_results_path <- "./out/gbdt_concentration_old_param.csv"
	  new_param_results_path <- "./out/gbdt_concentration_new_param.csv"
	  all_param_results_path <- "./out/gbdt_concentration_all_param.csv"
	}

  # 旧指標群データから，特徴指標のデータ列だけを取得（Map列を除外）
  old_features_data <- old_features |>
    dplyr::select(-all_of("Map"))
  
  # 新指標群データから，特徴指標のデータ列だけを取得（Map列を除外）
  new_features_data <- new_features |>
    dplyr::select(-all_of("Map"))
  
  # 総合指標群データから，特徴指標のデータ列だけを取得（Map列を除外）
  all_features_data <- all_features |>
    dplyr::select(-all_of("Map"))
  
  # 旧指標群データの指標名リストを取得
  old_predictor_names <- colnames(old_features_data)
  old_predictor_formula_str <- paste(old_predictor_names, collapse = " + ")
  
  # 新指標群データの指標名リストを取得
  new_predictor_names <- colnames(new_features_data)
  new_predictor_formula_str <- paste(new_predictor_names, collapse = " + ")
  
  # 総合指標群データの指標名リストを取得
  all_predictor_names <- colnames(all_features_data)
  all_predictor_formula_str <- paste(all_predictor_names, collapse = " + ")
  
  # シミュレーション結果データの指標名リストを取得
  response_variable_names <- data_simulation |>
    dplyr::select(-all_of(c("map", "agent"))) |>
    dplyr::select(where(is.numeric)) |>
    colnames()
	
	# k分割交差検証の設定
	cv_setting <- trainControl(
	  method = "cv",
	  number = 10 # 分割数
	)
	
	# ハイパーパラメータの値の候補を設定
	xgb_tune_grid <- expand.grid(
	  nrounds = c(500, 1000), # 決定木の数
	  max_depth = c(3, 5, 7), # 決定木の深さ
	  min_child_weight = c(3), # 決定木の葉の重みの下限
	  colsample_bytree = c(1.0), # 各回帰木においてランダムに抽出される列の割合
	  subsample = c(0.6, 0.8), # 各回帰木においてランダムに抽出される行の割合
	  eta = 0.1, # 学習率
	  gamma = c(0.0, 0.2) # 回帰木を分枝させるために最低限減らさないといけない目的変数のパラメータ
	)
	
	# CSVに出力するためのデータフレーム
	old_param_df <- data.frame()
	new_param_df <- data.frame()
	all_param_df <- data.frame()
	
	# 旧指標群データとシミュレーション結果データを結合
	model_data_old <- left_join(data_simulation, old_features, by = c("map" = "Map"))
	
	write_lines("", output_path, append = TRUE)
	write_lines("旧指標群を説明変数としたGBDTの構築", output_path, append = TRUE)

	# 目的変数でループ
	model_list_old <- purrr::map(response_variable_names, ~{
		current_response_var <- .x
		
		old_formula_str <- paste(current_response_var, "~", old_predictor_formula_str)
		old_model_formula <- as.formula(old_formula_str)
		
		old_model <- train(
		  old_model_formula,
		  data = model_data_old,
		  method = "xgbTree",
		  trControl = cv_setting,
		  tuneGrid = xgb_tune_grid,
		  na.action = na.omit,
		  verbose = FALSE,
		  alpha = 0, # 回帰木の重みに対するL1正則化項の強さ
		  lambda = 1 # 回帰木の重みに対するL2正則化項の強さ
		)
		
		# 最適パラメータを格納
		best_param <- old_model$bestTune
		best_param$response_variable <- current_response_var
		old_param_df <<- bind_rows(old_param_df, best_param)

		# モデルの概要をファイルに出力
		model_summary_old <- capture.output(print(old_model))
    write_lines("", output_path, append = TRUE)
    write_lines(paste("---目的変数：", current_response_var, "---"), output_path, append = TRUE)
    write_lines("最適なハイパーパラメータ", output_path, append = TRUE)
    write_lines(capture.output(print(old_model$bestTune)), output_path, append = TRUE)
    write_lines("", output_path, append = TRUE)
    write_lines("CV結果詳細", output_path, append = TRUE)
    write_lines(model_summary_old, output_path, append = TRUE)

		return(old_model)
	}) |> set_names(response_variable_names)
	
	write.csv(old_param_df, old_param_results_path, row.names = FALSE)
	
	# 新指標群データとシミュレーション結果データを結合
	model_data_new <- left_join(data_simulation, new_features, by = c("map" = "Map"))
	
	write_lines("", output_path, append = TRUE)
	write_lines("", output_path, append = TRUE)
	write_lines("", output_path, append = TRUE)
	write_lines("新指標群を説明変数としたGBDTの構築", output_path, append = TRUE)
	
	# 目的変数でループ
	model_list_new <- purrr::map(response_variable_names, ~{
	  current_response_var <- .x
	  
	  new_formula_str <- paste(current_response_var, "~", new_predictor_formula_str)
	  new_model_formula <- as.formula(new_formula_str)
	  
	  new_model <- train(
	    new_model_formula,
	    data = model_data_new,
	    method = "xgbTree",
	    trControl = cv_setting,
	    tuneGrid = xgb_tune_grid,
	    na.action = na.omit,
	    verbose = FALSE
	  )
	  
	  # 最適パラメータを格納
	  best_param <- new_model$bestTune
	  best_param$response_variable <- current_response_var
	  new_param_df <<- bind_rows(new_param_df, best_param)
	  
	  # モデルの概要をファイルに出力
	  model_summary_new <- capture.output(print(new_model))
	  write_lines("", output_path, append = TRUE)
	  write_lines(paste("---目的変数：", current_response_var, "---"), output_path, append = TRUE)
	  write_lines("最適なハイパーパラメータ", output_path, append = TRUE)
	  write_lines(capture.output(print(new_model$bestTune)), output_path, append = TRUE)
	  write_lines("", output_path, append = TRUE)
	  write_lines("CV結果詳細", output_path, append = TRUE)
	  write_lines(model_summary_new, output_path, append = TRUE)
	  
	  return(new_model)
	}) |> set_names(response_variable_names)
	
	write.csv(new_param_df, new_param_results_path, row.names = FALSE)

	# 総合指標群データとシミュレーション結果データを結合
	model_data_all <- left_join(data_simulation, all_features, by = c("map" = "Map"))
	
	write_lines("", output_path, append = TRUE)
	write_lines("", output_path, append = TRUE)
	write_lines("", output_path, append = TRUE)
	write_lines("総合指標群を説明変数としたGBDTの構築", output_path, append = TRUE)

	# 目的変数でループ
	model_list_all <- purrr::map(response_variable_names, ~{
		current_response_var <- .x
		
		all_formula_str <- paste(current_response_var, "~", all_predictor_formula_str)
		all_model_formula <- as.formula(all_formula_str)
		
		all_model <- train(
		  all_model_formula,
		  data = model_data_all,
		  method = "xgbTree",
		  trControl = cv_setting,
		  tuneGrid = xgb_tune_grid,
		  na.action = na.omit,
		  verbose = FALSE
		)
		
		# 最適パラメータを格納
		best_param <- all_model$bestTune
		best_param$response_variable <- current_response_var
		all_param_df <<- bind_rows(all_param_df, best_param)

		# モデルの概要をファイルに出力
		model_summary_all <- capture.output(print(all_model))
        write_lines("", output_path, append = TRUE)
        write_lines(paste("---目的変数：", current_response_var, "---"), output_path, append = TRUE)
        write_lines("最適なハイパーパラメータ", output_path, append = TRUE)
        write_lines(capture.output(print(all_model$bestTune)), output_path, append = TRUE)
        write_lines("", output_path, append = TRUE)
        write_lines("CV結果詳細", output_path, append = TRUE)
        write_lines(model_summary_all, output_path, append = TRUE)

		return(all_model)
	}) |> set_names(response_variable_names)
	
	write.csv(all_param_df, all_param_results_path, row.names = FALSE)

	if (agent_type == "distributed") {
		write_lines("----------------------分散救助エージェントのGBDTの作成終了----------------------", output_path, append = TRUE)
		write_lines("", output_path, append = TRUE)
	} else if (agent_type == "concentration") {
		write_lines("----------------------集中救助エージェントのGBDTの作成終了----------------------", output_path, append = TRUE)
		write_lines("", output_path, append = TRUE)
	}

	old_and_new_and_all_models_list <- list(
	  old_models = model_list_old,
	  new_models = model_list_new,
	  all_models = model_list_all
	)

	# 作成したGBDTのリストを返す
	return(old_and_new_and_all_models_list)
}