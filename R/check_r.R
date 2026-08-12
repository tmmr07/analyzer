# パッケージの読み込み
library(readr)
library(dplyr)
library(tibble)
library(stringr)
library(purrr)
library(pls)
library(car)

#---------------------------------------------
# 相関係数を算出し、高相関ペアを出力する関数
#---------------------------------------------
#' 説明変数間の相関係数を計算して高相関ペアを出力
#' @param all_features 説明変数データフレーム
#' @param output_path 出力先ファイルパス
#' @param threshold 相関係数の閾値（絶対値）
#' @return 高相関ペアの tibble
check_correlation <- function(all_features, output_path, threshold = 0.7) {
  # ファイル出力に関連する設定
  old_max_print <- getOption("max.print")
  options(max.print = 5000)
  on.exit(options(max.print = old_max_print), add = TRUE)
  
  # 数値列のみ抽出
  feature_data <- all_features %>% select(where(is.numeric))
  feature_names <- colnames(feature_data)
  
  result <- tibble(Feature1 = character(),
                   Feature2 = character(),
                   Correlation = numeric())
  
  # 二重ループで相関係数を算出（同一組み合わせは1回のみ）
  for (i in 1:(ncol(feature_data)-1)) {
    for (j in (i+1):ncol(feature_data)) {
      f1 <- feature_names[i]
      f2 <- feature_names[j]
      cor_val <- cor(feature_data[[i]], feature_data[[j]], use = "pairwise.complete.obs")
      result <- add_row(result, Feature1 = f1, Feature2 = f2, Correlation = cor_val)
    }
  }
  
  # 閾値以上の相関のみ
  result <- result %>% filter(abs(Correlation) >= threshold)
  
  # 高相関順にソート
  result <- result %>% arrange(desc(abs(Correlation)))
  
  # 出力
  if(nrow(result) == 0) {
    write_lines("高相関ペアはありませんでした。", output_path)
  } else {
    write_lines("高相関ペア一覧（絶対値 >= 閾値）", output_path)
    write_lines(capture.output(print(result, n = Inf)), output_path, append = TRUE)
  }
  
  return(result)
}