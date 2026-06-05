source("./R/check_normality.R")
compare_models_statistical <- function(target_df, old_feature_df, all_feature_df, target_col, agent_name) {
  
  # --- 1. データ準備 ---
  # 共通のマップのみを残して結合
  data_old <- inner_join(target_df[, c("Map", target_col)], old_feature_df, by = "Map") |> na.omit()
  data_all <- inner_join(target_df[, c("Map", target_col)], all_feature_df, by = "Map") |> na.omit()
  
  y <- data_all[[target_col]]
  x_old <- as.matrix(data_old |> select(-Map, -all_of(target_col)))
  x_all <- as.matrix(data_all |> select(-Map, -all_of(target_col)))
  
  # --- 2. 予測値の算出 (5-fold CV) ---
  # 【重要】比較のためseedを固定し、全く同じ分割で評価する
  set.seed(123)
  
  # keep = TRUE で、CV中の予測値(fit.preval)を保持させる
  fit_old <- cv.glmnet(x_old, y, nfolds = 5, alpha = 1, keep = TRUE, standardize = FALSE)
  fit_all <- cv.glmnet(x_all, y, nfolds = 5, alpha = 1, keep = TRUE, standardize = FALSE)
  
  # 最適なlambdaでの予測値を抽出
  idx_old <- which(fit_old$lambda == fit_old$lambda.min)
  pred_old <- fit_old$fit.preval[, idx_old]
  
  idx_all <- which(fit_all$lambda == fit_all$lambda.min)
  pred_all <- fit_all$fit.preval[, idx_all]
  
  # 予測誤差の正規性の確認
  save_normality_check(
    y =y,
    pred_old = pred_old,
    pred_all = pred_all,
    target_name = target_col,
    agent_name = agent_name,
    output_dir = "./out"
  )
  
  # --- 3. 二乗誤差の計算 ---
  sq_err_old <- (y - pred_old)^2
  sq_err_all <- (y - pred_all)^2
  
  # --- 4. ウィルコクソンの符号順位検定 (片側: All < Old ?) ---
  test_res <- wilcox.test(sq_err_old, sq_err_all, paired = TRUE, alternative = "greater")
  
  # 参考: RMSE
  rmse_old <- sqrt(mean(sq_err_old))
  rmse_all <- sqrt(mean(sq_err_all))
  
  return(data.frame(
    Target = target_col,
    RMSE_Old = rmse_old,
    RMSE_All = rmse_all,
    Diff_RMSE = rmse_old - rmse_all, # プラスなら改善
    P_Value = test_res$p.value,
    Significant = test_res$p.value < 0.05 # 5%水準で有意か
  ))
}