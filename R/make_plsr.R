# パッケージの読み込み
library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(pls)

#' PLSRモデルを構築する関数
#' @param data_simulation 各エージェントにおけるシミュレーション結果のデータ
#' @param old_features 選定済み旧指標群のデータ
#' @param new_features 新指標群のデータ
#' @param all_features 総合指標群のデータ
#' @param agent_type エージェントの種類
#' @param output_path 結果を出力するファイルパス
#' @return 作成したPLSRモデルを格納したリスト

make_plsr <- function(data_simulation, old_features, new_features, all_features, agent_type, output_path) {

	if (agent_type == "distributed") {
		write_lines("----------------------分散救助エージェントのPLSRモデルを作成----------------------", output_path, append = TRUE)
	} else if (agent_type == "concentration") {
		write_lines("----------------------集中救助エージェントのPLSRモデルを作成----------------------", output_path, append = TRUE)
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
	
	# 旧指標群データとシミュレーション結果データを結合
	model_data_old <- left_join(data_simulation, old_features, by = c("map" = "Map"))
	
	write_lines("", output_path, append = TRUE)
	write_lines("旧指標群を説明変数としたPLSRモデルの構築", output_path, append = TRUE)

	# 目的変数でループ
	model_list_old <- purrr::map(response_variable_names, ~{
		current_response_var <- .x
		
		# フォーミュラ文字列の作成と変換
		old_formula_str <- paste(current_response_var, "~", old_predictor_formula_str)
		old_model_formula <- as.formula(old_formula_str)

		# 欠損値を除外して，PLSRモデルを構築
		old_model <- plsr(old_model_formula, data = model_data_old, method = "kernelpls", validation = "CV", segments = 10, na.action = na.omit, scale = TRUE)

		ncomp_selected <- selectNcomp(old_model, method = "onesigma", plot = FALSE)

		# モデルの概要をファイルに出力
		model_summary_old <- capture.output(summary(old_model))
    write_lines("", output_path, append = TRUE)
    write_lines(paste("---目的変数：", current_response_var, "---"), output_path, append = TRUE)
    write_lines(model_summary_old, output_path, append = TRUE)

		write_lines(paste("最適な成分数：", ncomp_selected), output_path, append = TRUE)

		return(list(model = old_model, ncomp = ncomp_selected))
	}) |> set_names(response_variable_names)
	
	# 新指標群データとシミュレーション結果データを結合
	model_data_new <- left_join(data_simulation, new_features, by = c("map" = "Map"))
	
	write_lines("", output_path, append = TRUE)
	write_lines("", output_path, append = TRUE)
	write_lines("", output_path, append = TRUE)
	write_lines("新指標群を説明変数としたPLSRモデルの構築", output_path, append = TRUE)
	
	# 目的変数でループ
	model_list_new <- purrr::map(response_variable_names, ~{
	  current_response_var <- .x
	  
	  # フォーミュラ文字列の作成と変換
	  new_formula_str <- paste(current_response_var, "~", new_predictor_formula_str)
	  new_model_formula <- as.formula(new_formula_str)
	  
	  # 欠損値を除外して，PLSRモデルを構築
	  # kernelplsは，NIPALSと結果が一致すると言われているらしい
	  new_model <- plsr(new_model_formula, data = model_data_new, method = "kernelpls", validation = "CV", segments = 10, na.action = na.omit, scale = TRUE)
	  
	  ncomp_selected <- selectNcomp(new_model, method = "onesigma", plot = FALSE)
	  
	  # モデルの概要をファイルに出力
	  model_summary_new <- capture.output(summary(new_model))
	  write_lines("", output_path, append = TRUE)
	  write_lines(paste("---目的変数：", current_response_var, "---"), output_path, append = TRUE)
	  write_lines(model_summary_new, output_path, append = TRUE)
	  
	  write_lines(paste("最適な成分数：", ncomp_selected), output_path, append = TRUE)
	  
	  return(list(model = new_model, ncomp = ncomp_selected))
	}) |> set_names(response_variable_names)

	# 総合指標群データとシミュレーション結果データを結合
	model_data_all <- left_join(data_simulation, all_features, by = c("map" = "Map"))
	
	write_lines("", output_path, append = TRUE)
	write_lines("", output_path, append = TRUE)
	write_lines("", output_path, append = TRUE)
	write_lines("総合指標群を説明変数としたPLSRモデルの構築", output_path, append = TRUE)

	# 目的変数でループ
	model_list_all <- purrr::map(response_variable_names, ~{
		current_response_var <- .x
		
		# フォーミュラ文字列の作成と変換
		all_formula_str <- paste(current_response_var, "~", all_predictor_formula_str)
		all_model_formula <- as.formula(all_formula_str)

		# 欠損値を除外して，PLSRモデルを構築
		# kernelplsは，NIPALSと結果が一致すると言われているらしい
		all_model <- plsr(all_model_formula, data = model_data_all, method = "kernelpls", validation = "CV", segments = 10, na.action = na.omit, scale = TRUE)

		ncomp_selected <- selectNcomp(all_model, method = "onesigma", plot = FALSE)

		# モデルの概要をファイルに出力
		model_summary_all <- capture.output(summary(all_model))
    write_lines("", output_path, append = TRUE)
    write_lines(paste("---目的変数：", current_response_var, "---"), output_path, append = TRUE)
    write_lines(model_summary_all, output_path, append = TRUE)

		write_lines(paste("最適な成分数：", ncomp_selected), output_path, append = TRUE)

		return(list(model = all_model, ncomp = ncomp_selected))
	}) |> set_names(response_variable_names)
	

	if (agent_type == "distributed") {
		write_lines("----------------------分散救助エージェントのPLSRモデルの作成終了----------------------", output_path, append = TRUE)
		write_lines("", output_path, append = TRUE)
	} else if (agent_type == "concentration") {
		write_lines("----------------------集中救助エージェントのPLSRモデルの作成終了----------------------", output_path, append = TRUE)
		write_lines("", output_path, append = TRUE)
	}

	old_and_new_and_all_models_list <- list(
	  old_models = model_list_old,
	  new_models = model_list_new,
	  all_models = model_list_all
	)

	# 作成したPLSERモデルのリストを返す
	return(old_and_new_and_all_models_list)
}