#' Lasso回帰をおこなう関数
#' @param target_df 目的変数（シミュレーション結果）データ
#' @param feature_df 説明変数（特徴指標）群データ
#' @param target_col_name 目的変数名
#' @param feature_set_name 説明変数群名
#' @return 係数が0でない変数の係数データフレーム
run_lasso_analysis <- function(target_df, feature_df, target_col_name, feature_set_name) {
  
  # データの結合
  data_merged <- inner_join(
    target_df |> dplyr::select(Map, all_of(target_col_name)),
    feature_df,
    by = "Map"
  ) |> 
    na.omit() 
  
  if (nrow(data_merged) < 3) return(NULL)
  
  # 行列(X)とベクトル(y)の作成
  x <- as.matrix(data_merged |> dplyr::select(-Map, -all_of(target_col_name)))
  y <- as.numeric(data_merged[[target_col_name]])
  
  # Lasso回帰
  set.seed(123)
  cv_fit <- cv.glmnet(x, y, alpha = 1, standardize = FALSE, nfolds = 5)
  
  # 最適なLambdaでの係数を抽出
  best_lambda <- cv_fit$lambda.min
  coef_obj <- coef(cv_fit, s = "lambda.min")
  
  # 最適なLambdaのインデックスを取得
  index_min <- which(cv_fit$lambda == cv_fit$lambda.min)
  
  # 決定係数の取得
  r2 <- cv_fit$glmnet.fit$dev.ratio[index_min]
  
  # RMSEの取得
  mse_val <- cv_fit$cvm[index_min]
  rmse_val <- sqrt(mse_val)
  
  # 結果をデータフレーム化
  coef_df <- data.frame(
    Variable = rownames(coef_obj),
    Coefficient = as.matrix(coef_obj)[, 1]
  ) |>
    filter(Coefficient != 0) |>
    filter(Variable != "(Intercept)") |>
    mutate(
      FeatureSet = feature_set_name,
      TargetVariable = target_col_name,
      Lambda = best_lambda,
      R_Squared = r2,
      RMSE = rmse_val
    )
  
  if (nrow(coef_df) == 0) return(NULL)
  
  return(coef_df)
}