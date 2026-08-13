library(dplyr)
library(readr)

#' ステップワイズ法による特徴量選択を行う関数
#' 
#' @param data_sim シミュレーション結果データ（目的変数）
#' @param data_features 特徴量データ（説明変数）
#' @param target_var 基準とする目的変数名（例: "final_alive"）
#' @param output_path 結果を出力するパス
#' @param label ログ出力用のラベル（"old", "new" など）
#' 
#' @return 選択された特徴量のみを含むデータフレーム

perform_stepwise <- function(data_sim, data_features, target_var = "final_alive", output_path, label) {
  
  # ログヘッダー
  write_lines(paste0("---------------------- ステップワイズ法による変数選択 (", label, ") ----------------------"), output_path, append = TRUE)
  write_lines(paste0("Target Variable: ", target_var), output_path, append = TRUE)
  
  # データの結合（Mapをキーにする）
  # 目的変数と説明変数を1つのデータフレームにする
  full_data <- left_join(data_sim, data_features, by = "map") |>
    dplyr::select(-map, -agent) # 計算に不要な列を除外
  
  # 数値データのみに絞る（念のため）
  full_data <- full_data |> dplyr::select(where(is.numeric))
  
  # 基準となる目的変数が存在するか確認
  if (!target_var %in% names(full_data)) {
    stop(paste("Error: Target variable", target_var, "not found in simulation data."))
  }
  
  # 全投入モデルの作成（目的変数 ~ 全説明変数）
  # formulaを作成: final_alive ~ .
  f <- as.formula(paste(target_var, "~ ."))
  
  # フルモデルの作成
  full_model <- lm(f, data = full_data)
  
  # ステップワイズ実行 (AIC基準, direction="both"で増減両方試す)
  # trace=0 にするとコンソールへの大量出力を抑制できます
  step_model <- step(full_model, direction = "both", trace = 0)
  
  # 選択された変数の名前を取得
  selected_vars <- names(coef(step_model))
  selected_vars <- selected_vars[selected_vars != "(Intercept)"] # 切片を除く
  
  # 結果の出力
  write_lines("\nSelected Variables:", output_path, append = TRUE)
  write_lines(paste(selected_vars, collapse = ", "), output_path, append = TRUE)
  
  # 統計量の出力
  write_lines("\nModel Summary:", output_path, append = TRUE)
  write_lines(capture.output(summary(step_model)), output_path, append = TRUE)
  write_lines("\n", output_path, append = TRUE)
  
  # 選択された変数 + Map列 を含むデータフレームを返す
  # 元の特徴量データから、選択された列だけを抽出
  result_df <- data_features %>%
    select(map, all_of(selected_vars))
  
  return(result_df)
}