# パッケージの読み込み
library(car)
library(dplyr)
library(tibble)
library(readr)

#' 多重共線性が存在するかどうかを確認する関数
#' @param map_features 多重共線性が存在するかどうかを確認したい特徴指標
#' @param response_variable 対象となる目的変数
#' @param output_path 結果を出力するファイルパス
#' @param data_type データの種類（"old"（旧指標群データ）または "all"（新指標群データ））
#' @return 選定された特徴指標のデータ

check_vif2 <- function(map_features, response_variable, output_path, data_type) {
  
  if (data_type == "old") {
    write_lines("-----------旧指標群の多重共線性を確認-----------", output_path, append = TRUE)
  } else if (data_type == "new") {
    write_lines("-----------新指標群の多重共線性を確認-----------", output_path, append = TRUE)
  } else if (data_type == "all") {
    write_lines("-----------総合指標群の多重共線性を確認-----------", output_path, append = TRUE)
  }
  
  # 特徴指標データから Map 列を除いて特徴指標名を取得
  initial_predictor_names <- map_features %>%
    dplyr::select(-all_of("Map")) %>%
    colnames()
  
  # 特徴指標データと目的変数データを結合
  model_data <- left_join(response_variable, map_features, by = c("map" = "Map"))
  
  # 除外された特徴指標名
  removed_features <- c()
  
  # 現時点の説明変数
  current_predictor_names <- initial_predictor_names
  
  # VIFが10未満になるまで繰り返す
  while (TRUE) {
    
    # 一時的なフォーミュラを作成（共線性チェック用）
    temp_formula_str <- paste("final_rsl21 ~", paste(current_predictor_names, collapse = " + "))
    temp_model <- lm(as.formula(temp_formula_str), data = model_data)
    
    # 完全共線性（NA になる係数）を検出
    aliased_vars <- names(coef(temp_model)[is.na(coef(temp_model))])
    aliased_vars <- setdiff(aliased_vars, "(Intercept)")
    
    if (length(aliased_vars) > 0) {
      # 1つずつ除外する
      predictor_to_remove <- aliased_vars[1]
      
      message(paste(predictor_to_remove, "が完全共線性であるため除去"))
      
      removed_features <- c(removed_features, predictor_to_remove)
      current_predictor_names <- setdiff(current_predictor_names, predictor_to_remove)
      
      # ループの先頭に戻る
      next
    }
    
    predictor_formula_part <- paste(current_predictor_names, collapse = " + ")
    formula_str <- paste("final_rsl21", "~", predictor_formula_part)
    model_formula <- as.formula(formula_str)
    
    # 線形モデルを構築
    model <- lm(model_formula, data = model_data)
    
    # VIF の計算
    vif_results <- vif(model)
    
    # 結果を出力
    vif_table_for_output <- enframe(vif_results, name = "Feature", value = "VIF")
    table_text <- capture.output(print(vif_table_for_output, n = Inf, digits = 10))
    write_lines("", output_path, append = TRUE)
    write_lines(table_text, output_path, append = TRUE)
    
    # VIF 値で降順に並べ替え
    vif_table <- enframe(vif_results, name = "Feature", value = "VIF") %>%
      arrange(desc(VIF))
    
    max_vif <- vif_table$VIF[1]
    
    # VIF が 10 未満なら終了
    if (max_vif < 10) {
      break
    }
    
    # それ以外なら最大 VIF の変数を除外
    predictor_to_remove <- vif_table$Feature[1]
    removed_features <- c(removed_features, predictor_to_remove)
    current_predictor_names <- setdiff(current_predictor_names, predictor_to_remove)
  }
  
  # 除外された特徴指標の一覧
  write_lines("", output_path, append = TRUE)
  write_lines("除外された特徴指標は以下の通りです．", output_path, append = TRUE)
  write_lines(removed_features, output_path, append = TRUE)
  write_lines("", output_path, append = TRUE)
  
  # 選定済みの特徴指標データを作成
  selected_features <- map_features %>%
    dplyr::select(-all_of(removed_features))
  
  # 最終的に選ばれた特徴指標を出力
  write_lines("", output_path, append = TRUE)
  write_lines("選定された特徴指標は以下の通りです．", output_path, append = TRUE)
  write_lines(colnames(selected_features)[-1], output_path, append = TRUE)
  write_lines("", output_path, append = TRUE)
  
  if (data_type == "old") {
    write_lines("-----------旧指標群の多重共線性の確認終了-----------", output_path, append = TRUE)
    write_lines("", output_path, append = TRUE)
  } else if (data_type == "new") {
    write_lines("-----------新指標群の多重共線性の確認終了-----------", output_path, append = TRUE)
    write_lines("", output_path, append = TRUE)
  } else if (data_type == "all") {
    write_lines("-----------総合指標群の多重共線性の確認終了-----------", output_path, append = TRUE)
    write_lines("", output_path, append = TRUE)
  }
  
  return(selected_features)
}
