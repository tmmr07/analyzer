# 必要なパッケージ
library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)

#' 重回帰モデルにおける予測誤差を算出する関数
#' 
#' @param data_simulation シミュレーション結果データ
#' @param old_features 旧指標群データ
#' @param new_features 新指標群データ
#' @param all_features 新指標群データ
#' @param agent_type エージェントの種類
#' @param output_path 結果ログを出力するファイルパス
#' @param models_list 作成した重回帰モデルのリスト
#'
#' @return 計算した予測誤差

calculate_lm_errors <- function(data_simulation, old_features, new_features, all_features, agent_type, output_path, models_list) {
  
  # ファイル出力に関連する設定
  old_max_print <- getOption("max.print")
  options(max.print = 5000)
  on.exit(options(max.print = old_max_print), add = TRUE)
  
  if (agent_type == "distributed") {
    write_lines("----------------------分散救助エージェントの重回帰モデルにおける予測誤差を算出----------------------", output_path, append = TRUE)
  } else if (agent_type == "concentration") {
    write_lines("----------------------集中救助エージェントの重回帰モデルにおける予測誤差を算出----------------------", output_path, append = TRUE)
  }
  
  # 説明変数データの作成
  old_features_data <- if("Map" %in% names(old_features)) old_features[, setdiff(names(old_features), "Map"), drop = FALSE] else old_features
  new_features_data <- if("Map" %in% names(new_features)) new_features[, setdiff(names(new_features), "Map"), drop = FALSE] else new_features
  all_features_data <- if("Map" %in% names(all_features)) all_features[, setdiff(names(all_features), "Map"), drop = FALSE] else all_features
  
  # 目的変数データの作成
  y_candidates <- data_simulation[, setdiff(names(data_simulation), c("map", "agent")), drop = FALSE]
  y_candidates <- y_candidates[, sapply(y_candidates, is.numeric), drop = FALSE]
  
  # 目的変数名リストを作成
  response_variable_names <- colnames(y_candidates)
  
  # データ結合
  model_data_old <- left_join(data_simulation, old_features, by = c("map" = "Map"))
  model_data_new <- left_join(data_simulation, new_features, by = c("map" = "Map"))
  model_data_all <- left_join(data_simulation, all_features, by = c("map" = "Map"))
  
  # モデルの取り出し
  old_models <- models_list$old_models
  new_models <- models_list$new_models
  all_models <- models_list$all_models
  
  write_lines("", output_path, append = TRUE)
  write_lines("旧指標群モデルの予測誤差", output_path, append = TRUE)
  
  # 予測誤差の計算
  errors_old_list <- purrr::map(response_variable_names, function(y) {
    model <- old_models[[y]]
    y_actual <- model_data_old[[y]]
    y_pred <- predict(model, newdata = model_data_old)
    set_names(list(y_actual - y_pred), y)
  })
  
  # データフレームの作成
  errors_old <- bind_cols(model_data_old[, "map", drop = FALSE], bind_cols(errors_old_list))
  
  write_lines(capture.output(print(as.data.frame(errors_old))), output_path, append = TRUE)
  
  write_lines("", output_path, append = TRUE)
  write_lines("", output_path, append = TRUE)
  write_lines("", output_path, append = TRUE)
  write_lines("", output_path, append = TRUE)
  write_lines("", output_path, append = TRUE)
  write_lines("新指標群モデルの予測誤差", output_path, append = TRUE)
  
  # 予測誤差の計算
  errors_new_list <- purrr::map(response_variable_names, function(y) {
    model <- new_models[[y]]
    y_actual <- model_data_new[[y]]
    y_pred <- predict(model, newdata = model_data_new)
    set_names(list(y_actual - y_pred), y)
  })
  
  # データフレームの作成
  errors_new <- bind_cols(model_data_new[, "map", drop = FALSE], bind_cols(errors_new_list))
  
  write_lines(capture.output(print(as.data.frame(errors_new))), output_path, append = TRUE)
  
  write_lines("", output_path, append = TRUE)
  write_lines("", output_path, append = TRUE)
  write_lines("", output_path, append = TRUE)
  write_lines("", output_path, append = TRUE)
  write_lines("", output_path, append = TRUE)
  write_lines("総合指標群モデルの予測誤差", output_path, append = TRUE)
  
  # 予測誤差の計算
  errors_all_list <- purrr::map(response_variable_names, function(y) {
    model <- all_models[[y]]
    y_actual <- model_data_all[[y]]
    y_pred <- predict(model, newdata = model_data_all)
    set_names(list(y_actual - y_pred), y)
  })
  
  # データフレームの作成
  errors_all <- bind_cols(model_data_all[, "map", drop = FALSE], bind_cols(errors_all_list))
  
  write_lines(capture.output(print(as.data.frame(errors_all))), output_path, append = TRUE)
  
  if (agent_type == "distributed") {
    write_lines("----------------------分散救助エージェントの重回帰モデルにおける予測誤差の計算終了----------------------", output_path, append = TRUE)
    write_lines("", output_path, append = TRUE)
  } else if (agent_type == "concentration") {
    write_lines("----------------------集中救助エージェントの重回帰モデルにおける予測誤差の計算終了----------------------", output_path, append = TRUE)
    write_lines("", output_path, append = TRUE)
  }
  
  return(list(
    errors_old = errors_old,
    errors_new = errors_new,
    errors_all = errors_all
  ))
}
