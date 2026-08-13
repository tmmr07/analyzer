library(dplyr)
library(tibble)
library(caret)
library(readr)
library(MASS)  # stepAIC用

#' ステップワイズ法 + フィルター法による特徴量選択
#' @param features 説明変数データ（Map列あり）
#' @param response 目的変数データ（map列あり）
#' @param output_path 結果を出力するファイルパス
#' @param p_threshold P値の閾値（フィルター法に使う、デフォルト0.05）
#' @return 選択された特徴量データ（Map列付き）
select_features_stepwise <- function(features, response, output_path, p_threshold = 0.05) {
  
  # 数値列だけ抽出（Map列は残す）
  numeric_cols <- sapply(features, is.numeric)
  numeric_cols["Map"] <- TRUE  # Map列は残す
  feature_data <- features[, numeric_cols, drop = FALSE]
  feature_names <- setdiff(colnames(feature_data), "Map")
  
  # 目的変数（数値列のみ、map列は除く）
  response_numeric <- sapply(response, is.numeric)
  response_numeric["map"] <- FALSE
  response_data <- response[, response_numeric, drop = FALSE]
  response_names <- colnames(response_data)
  
  result_table <- tibble(Feature = character(),
                         Response = character(),
                         Intercept = numeric(),
                         Slope = numeric(),
                         R2 = numeric(),
                         P_value = numeric())
  
  selected_features_all <- c()
  
  for (resp in response_names) {
    Y <- response_data[[resp]]
    
    # 単回帰でP値フィルター
    for (feat in feature_names) {
      X <- feature_data[[feat]]
      model <- lm(Y ~ X)
      summ <- summary(model)
      pval <- coef(summ)[2, 4]
      R2 <- summ$r.squared
      intercept <- coef(model)[1]
      slope <- coef(model)[2]
      
      result_table <- add_row(result_table,
                              Feature = feat,
                              Response = resp,
                              Intercept = intercept,
                              Slope = slope,
                              R2 = R2,
                              P_value = pval)
    }
    
    # P値 < p_threshold の特徴量を残す
    candidate_features <- result_table$Feature[result_table$Response == resp & result_table$P_value < p_threshold]
    if (length(candidate_features) == 0) {
      next
    }
    
    # ステップワイズ法（前進・後退両方）
    X_candidate <- feature_data[, candidate_features, drop = FALSE]
    df_model <- cbind(Y = Y, X_candidate)
    full_model <- lm(Y ~ ., data = df_model)
    step_model <- stepAIC(full_model, direction = "both", trace = FALSE)
    
    selected_features <- names(coef(step_model))[-1]  # "(Intercept)"を除く
    selected_features_all <- union(selected_features_all, selected_features)
  }
  
  # 選択特徴量とMap列を返す
  if (length(selected_features_all) > 0) {
    X_selected <- feature_data[, selected_features_all, drop = FALSE]
    selected_features_with_map <- cbind(Map = feature_data$Map, X_selected)
    
    # 出力
    write_lines("=== ステップワイズ＋P値フィルターによる特徴量選択結果 ===", output_path)
    write_lines(capture.output(print(result_table)), output_path, append = TRUE)
    write_lines("\n=== 選択された特徴量一覧 ===", output_path, append = TRUE)
    write_lines(selected_features_all, output_path, append = TRUE)
    
    return(selected_features_with_map)
  } else {
    write_lines("選択された特徴量はありませんでした。", output_path)
    return(feature_data[, "Map", drop = FALSE])
  }
}
