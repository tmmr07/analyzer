# 必要なパッケージ
library(readr)
library(dplyr)
library(purrr)
library(caret)

#' GBDTモデルにおける予測誤差を算出する関数
#' 
#' @param data_simulation シミュレーション結果データ
#' @param old_features 旧指標群データ
#' @param new_features 新指標群データ
#' @param all_features 総合指標群データ
#' @param agent_type エージェントの種類
#' @param output_path 結果ログを出力するファイルパス
#' @param gbdt_models 作成したGBDTモデルのリスト
#'
#' @return 計算した予測誤差

calculate_gbdt_errors <- function(data_simulation, old_features, new_features, all_features, agent_type, output_path, gbdt_models) {
  
  # ファイル出力に関連する設定
  old_max_print <- getOption("max.print")
  options(max.print = 5000)
  on.exit(options(max.print = old_max_print), add = TRUE)
  
  if (agent_type == "distributed") {
    write_lines("----------------------分散救助エージェントのGBDTモデルにおける予測誤差を算出----------------------", output_path, append = TRUE)
  } else if (agent_type == "concentration") {
    write_lines("----------------------集中救助エージェントのGBDTモデルにおける予測誤差を算出----------------------", output_path, append = TRUE)
  }
  
  # 目的変数名リストを作成
  response_variable_names <- data_simulation |>
    dplyr::select(-all_of(c("map", "agent"))) |>
    dplyr::select(where(is.numeric)) |>
    colnames()
  
  # データ結合
  model_data_old <- left_join(data_simulation, old_features, by = c("map" = "Map"))
  model_data_new <- left_join(data_simulation, new_features, by = c("map" = "Map"))
  model_data_all <- left_join(data_simulation, all_features, by = c("map" = "Map"))
  
  # 予測誤差を計算する内部関数
  .calc_error_for_y <- function(y_var, gbdt_model, data_df) {
    
    # 予測値の計算
    pred <- as.numeric(predict(gbdt_model,
                               newdata = data_df))
    
    # 実測値の取得
    actual <- data_df[[y_var]]
    
    # 誤差の計算
    error_vec <- actual - pred
    
    return(set_names(list(error_vec), y_var))
  }
  
  write_lines("", output_path, append = TRUE)
  write_lines("旧指標群モデルの予測誤差", output_path, append = TRUE)
  
  # 各目的変数ごとに予測誤差を計算
  errors_old_list <- purrr::map(response_variable_names, ~{
    gbdt_model <- gbdt_models$old_models[[.x]]
    .calc_error_for_y(.x, gbdt_model, model_data_old)
  })
  
  # データフレームの作成
  errors_old <- bind_cols(
    tibble(map = model_data_old$map),
    bind_cols(errors_old_list)
  )
  
  write_lines(capture.output(print(as.data.frame(errors_old))), 
              output_path, append = TRUE)
  
  write_lines("", output_path, append = TRUE)
  write_lines("", output_path, append = TRUE)
  write_lines("", output_path, append = TRUE)
  write_lines("", output_path, append = TRUE)
  write_lines("", output_path, append = TRUE)
  write_lines("新指標群モデルの予測誤差", output_path, append = TRUE)
  
  # 各目的変数ごとに予測誤差を計算
  errors_new_list <- purrr::map(response_variable_names, ~{
    gbdt_model <- gbdt_models$new_models[[.x]]
    .calc_error_for_y(.x, gbdt_model, model_data_new)
  })
  
  # データフレームの作成
  errors_new <- bind_cols(
    tibble(map = model_data_new$map),
    bind_cols(errors_new_list)
  )
  
  write_lines(capture.output(print(as.data.frame(errors_new))), 
              output_path, append = TRUE)
  
  write_lines("", output_path, append = TRUE)
  write_lines("", output_path, append = TRUE)
  write_lines("", output_path, append = TRUE)
  write_lines("", output_path, append = TRUE)
  write_lines("", output_path, append = TRUE)
  write_lines("総合指標群モデルの予測誤差", output_path, append = TRUE)
  
  # 各目的変数ごとに予測誤差を計算
  errors_all_list <- purrr::map(response_variable_names, ~{
    gbdt_model <- gbdt_models$all_models[[.x]]
    .calc_error_for_y(.x, gbdt_model, model_data_all)
  })
  
  # データフレームの作成
  errors_all <- bind_cols(
    tibble(map = model_data_all$map),
    bind_cols(errors_all_list)
  )
  
  write_lines(capture.output(print(as.data.frame(errors_all))), 
              output_path, append = TRUE)
  
  if (agent_type == "distributed") {
    write_lines("----------------------分散救助エージェントのGBDTモデルにおける予測誤差の計算終了----------------------", output_path, append = TRUE)
    write_lines("", output_path, append = TRUE)
  } else if (agent_type == "concentration") {
    write_lines("----------------------集中救助エージェントのGBDTモデルにおける予測誤差の計算終了----------------------", output_path, append = TRUE)
    write_lines("", output_path, append = TRUE)
  }
  
  return(list(
    errors_old = errors_old,
    errors_new = errors_new,
    errors_all = errors_all
  ))
}