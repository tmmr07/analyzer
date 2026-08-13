library(dplyr)
library(tibble)
library(readr)

# 単回帰分析を実施し、有意でない説明変数を抽出する関数
# 1つでも目的変数に対してP < 0.05であれば残す
run_univariate_regression <- function(features, response, output_path, p_threshold = 0.05) {
  
  # 数値列のみ抽出（Map列は除く）
  feature_data <- features %>% select(-Map)
  feature_names <- colnames(feature_data)
  
  response_data <- response %>% select(where(is.numeric))
  response_names <- colnames(response_data)
  
  # Map列で結合
  data <- left_join(response, features, by = c("map" = "Map"))
  
  # 出力用 tibble
  results <- tibble(
    Feature = character(),
    Response = character(),
    Intercept = numeric(),
    Slope = numeric(),
    R2 = numeric(),
    P_value = numeric()
  )
  
  # 目的変数ごとにループ
  for (resp in response_names) {
    for (feat in feature_names) {
      formula_str <- paste(resp, "~", feat)
      model <- lm(as.formula(formula_str), data = data)
      sum_model <- summary(model)
      
      intercept <- coef(model)[1]
      slope <- coef(model)[2]
      r2 <- sum_model$r.squared
      p_val <- coef(sum_model)[2, 4]  # 説明変数のp値
      
      results <- add_row(
        results,
        Feature = feat,
        Response = resp,
        Intercept = intercept,
        Slope = slope,
        R2 = r2,
        P_value = p_val
      )
    }
  }
  
  # 出力用にオプション設定
  old_scipen <- getOption("scipen")
  options(scipen = 999)
  old_width <- getOption("width")
  options(width = 200)
  
  write_lines("単回帰分析結果", output_path)
  write_lines(capture.output(print(results, n = nrow(results))), output_path, append = TRUE)
  
  # オプションを元に戻す
  options(scipen = old_scipen, width = old_width)
  
  # ここで除外すべき説明変数を抽出
  excluded_features <- results %>%
    group_by(Feature) %>%
    summarise(max_p = max(P_value, na.rm = TRUE)) %>%
    filter(max_p >= p_threshold) %>%
    pull(Feature)
  
  return(list(
    results_table = results,
    excluded_features = excluded_features
  ))
}