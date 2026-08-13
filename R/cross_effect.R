library(dplyr)
library(readr)
library(pls)

#' 相乗効果（相互作用）の検証を行う関数
#' 
#' @param data_simulation シミュレーション結果
#' @param old_features 建物指標データ（標準化済み）
#' @param new_features 道路指標データ（標準化済み）
#' @param plsr_results_old 建物指標のPLSR結果リスト
#' @param plsr_results_new 道路指標のPLSR結果リスト
#' @param output_path 結果出力パス
#' @param top_n 各領域から選抜する変数の数（デフォルト3）
analyze_synergy <- function(data_simulation, old_features, new_features, 
                            plsr_results_old, plsr_results_new, 
                            output_path, top_n = 3) {
  
  # --- 内部ヘルパー関数: VIP計算 ---
  calc_vip <- function(object) {
    if (object$method != "kernelpls") stop("Only kernelpls method is supported.")
    
    SS <- c(object$Yloadings)^2 * colSums(object$scores^2)
    Wnorm2 <- colSums(object$loading.weights^2)
    SSW <- sweep(object$loading.weights^2, 2, SS / Wnorm2, "*")
    
    vip <- sqrt(nrow(SSW) * apply(SSW, 1, cumsum) / cumsum(SS))
    return(vip[, object$ncomp, drop = FALSE]) # 最適成分数でのVIPを返す
  }
  # ----------------------------------
  
  write_lines("----------------------相乗効果（相互作用）の検証----------------------", output_path, append = TRUE)
  
  # 目的変数リスト
  response_vars <- data_simulation |> dplyr::select(where(is.numeric)) |> colnames()
  
  # データの結合 (Map列で結合)
  full_data <- left_join(data_simulation, old_features, by = c("map" = "Map")) |>
    left_join(new_features, by = c("map" = "Map"))
  
  results_list <- list()
  
  for (resp in response_vars) {
    
    # 1. 重要変数の特定 (VIP)
    # 建物モデルからTop N
    model_old <- plsr_results_old[[resp]]$model
    # ncompが1未満の場合は1にする（計算用）
    ncomp_old <- max(1, plsr_results_old[[resp]]$ncomp) 
    
    # calc_vipが使えない場合（成分数不足等）は係数の絶対値を使う簡易実装
    tryCatch({
      vip_old <- c(calc_vip(model_old)[, ncomp_old])
    }, error = function(e) {
      vip_old <- abs(c(coef(model_old, ncomp = ncomp_old, intercept = FALSE)))
    })
    top_old_vars <- names(sort(vip_old, decreasing = TRUE))[1:top_n]
    
    # 道路モデルからTop N
    model_new <- plsr_results_new[[resp]]$model
    ncomp_new <- max(1, plsr_results_new[[resp]]$ncomp)
    
    tryCatch({
      vip_new <- c(calc_vip(model_new)[, ncomp_new])
    }, error = function(e) {
      vip_new <- abs(c(coef(model_new, ncomp = ncomp_new, intercept = FALSE)))
    })
    top_new_vars <- names(sort(vip_new, decreasing = TRUE))[1:top_n]
    
    
    # 2. モデル式の作成
    # ベースモデル（足し算のみ）: Y ~ A1+A2+A3 + B1+B2+B3
    formula_base <- as.formula(paste(
      resp, "~", 
      paste(c(top_old_vars, top_new_vars), collapse = " + ")
    ))
    
    # 相互作用モデル（掛け算あり）: Y ~ (A1+A2+A3) * (B1+B2+B3)
    # ※ これだとすべての組み合わせ総当たりになる
    formula_int <- as.formula(paste(
      resp, "~ (", 
      paste(top_old_vars, collapse = " + "), ") * (",
      paste(top_new_vars, collapse = " + "), ")"
    ))
    
    # 3. LMモデル構築
    lm_base <- lm(formula_base, data = full_data)
    lm_int  <- lm(formula_int, data = full_data)
    
    # 4. ANOVAによる検定
    anova_res <- anova(lm_base, lm_int)
    
    # 結果の保存
    p_val <- anova_res$`Pr(>F)`[2]
    r2_base <- summary(lm_base)$adj.r.squared
    r2_int  <- summary(lm_int)$adj.r.squared
    
    # 有意な場合のみログ出力
    write_lines(paste0("\n--- 目的変数: ", resp, " ---"), output_path, append = TRUE)
    write_lines(paste("建物Top3:", paste(top_old_vars, collapse=", ")), output_path, append = TRUE)
    write_lines(paste("道路Top3:", paste(top_new_vars, collapse=", ")), output_path, append = TRUE)
    write_lines(paste("相乗効果のp値:", signif(p_val, 4)), output_path, append = TRUE)
    write_lines(paste("決定係数の変化:", round(r2_base, 3), "->", round(r2_int, 3)), output_path, append = TRUE)
    
    results_list[[resp]] <- list(
      top_old = top_old_vars,
      top_new = top_new_vars,
      p_value = p_val,
      r2_change = r2_int - r2_base,
      anova = anova_res
    )
  }
  
  return(results_list)
}