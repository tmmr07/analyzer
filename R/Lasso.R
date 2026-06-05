#' Lasso回帰をおこなう関数 (RMSE算出追加版)
#' @param target_df 目的変数（シミュレーション結果）データ
#' @param feature_df 説明変数（特徴指標）群データ
#' @param target_col_name 目的変数名
#' @param feature_set_name 説明変数群名
#' @return 係数が0でない変数の係数データフレーム（R2とRMSEを含む）
run_lasso_analysis <- function(target_df, feature_df, target_col_name, feature_set_name) {
  
  # データの結合
  data_merged <- inner_join(
    target_df |> dplyr::select(Map, all_of(target_col_name)),
    feature_df,
    by = "Map"
  ) |> 
    na.omit() 
  
  # データ数が少なすぎる場合はNULLを返して終了（エラー回避）
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
  
  # 評価指標の取得
  # 1. 決定係数 (R2)
  r2 <- cv_fit$glmnet.fit$dev.ratio[index_min]
  
  # 2. RMSE (Root Mean Squared Error) ★ここを追加
  # cv_fit$cvm にはクロスバリデーションごとの平均誤差(MSE)が入っています
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
      RMSE = rmse_val  # ★データフレームに追加
    )
  
  # 有効な変数が1つもない場合でも、モデルの評価指標だけは返したい場合
  # もし係数が全部0でフィルタされて消えてしまうとRMSEも消えるため、
  # その場合は切片(Intercept)だけの行を作るか、NULLを返すかですが、
  # ここでは元のロジック通りNULLになります（係数ありの結果のみ返す仕様）
  if (nrow(coef_df) == 0) return(NULL)
  
  return(coef_df)
}