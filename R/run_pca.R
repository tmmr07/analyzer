#---------------------------------------------
# 主成分分析(PCA)を実行し、寄与率と負荷量を出力する関数
#---------------------------------------------
#' 主成分分析を実行して結果をファイルに出力
#' @param all_features 説明変数データフレーム
#' @param output_path 出力先ファイルパス
#' @param scale_data データを標準化（スケーリング）するかどうか（デフォルト: TRUE）
#' @return 結果リスト (summary: 寄与率のtibble, rotation: 負荷量のtibble)
perform_pca <- function(all_features, output_path, scale_data = TRUE) {
  # ファイル出力に関連する設定
  old_max_print <- getOption("max.print")
  options(max.print = 5000)
  on.exit(options(max.print = old_max_print), add = TRUE)
  
  # 数値列のみ抽出
  feature_data <- all_features %>% select(where(is.numeric))
  
  # 分散が0の列（すべての値が同じ列）があるとPCAでエラーになるため除外
  # var() はNAを含むとNAを返すため、na.rm=TRUEを設定
  valid_cols <- sapply(feature_data, function(x) var(x, na.rm = TRUE) != 0)
  feature_data <- feature_data[, valid_cols]
  
  # 除外された列があれば警告を表示（コンソール用）
  if(sum(!valid_cols) > 0) {
    warning(paste("分散が0のため除外された列:", 
                  paste(names(valid_cols)[!valid_cols], collapse = ", ")))
  }
  
  # 主成分分析の実行 (prcompはSVDを使用しており数値的に安定)
  # scale. = TRUE で相関行列に基づく分析（標準化）を行う
  pca_res <- prcomp(feature_data, scale. = scale_data)
  
  # --- 1. 寄与率 (Importance) の整形 ---
  pca_summary <- summary(pca_res)
  importance_df <- as.data.frame(pca_summary$importance) %>%
    tibble::rownames_to_column("Metric") %>%
    as_tibble()
  
  # --- 2. 因子負荷量 (Rotation/Loadings) の整形 ---
  # 各変数がどの主成分にどれくらい寄与しているか
  rotation_df <- as.data.frame(pca_res$rotation) %>%
    tibble::rownames_to_column("Feature") %>%
    as_tibble()
  
  # --- ファイルへの出力 ---
  # タイトル
  write_lines("### 主成分分析 (PCA) 結果レポート ###", output_path)
  write_lines(paste("実行日時:", Sys.time()), output_path, append = TRUE)
  write_lines(paste("データ標準化 (Scale):", scale_data), output_path, append = TRUE)
  write_lines("", output_path, append = TRUE)
  
  # 寄与率の出力
  write_lines("--- 1. 寄与率 (Importance of components) ---", output_path, append = TRUE)
  write_lines("Standard deviation: 標準偏差", output_path, append = TRUE)
  write_lines("Proportion of Variance: 寄与率", output_path, append = TRUE)
  write_lines("Cumulative Proportion: 累積寄与率", output_path, append = TRUE)
  write_lines("", output_path, append = TRUE)
  write_lines(capture.output(print(importance_df, n = Inf, width = Inf)), output_path, append = TRUE)
  write_lines("", output_path, append = TRUE)
  
  # 負荷量の出力
  write_lines("--- 2. 因子負荷量 (Rotation/Loadings) ---", output_path, append = TRUE)
  write_lines("各変数が各主成分(PC)に与える影響度", output_path, append = TRUE)
  write_lines("", output_path, append = TRUE)
  write_lines(capture.output(print(rotation_df, n = Inf, width = Inf)), output_path, append = TRUE)
  
  # 結果をリストで返す（後でグラフ描画などに使えるように）
  return(list(summary = importance_df, rotation = rotation_df, model = pca_res))
}