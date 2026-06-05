library(dplyr)
library(stringr)
library(tidyr)

preprocess <- function(file_path) {
  
  # 1. データの読み込み
  if (!file.exists(file_path)) {
    stop(paste("ファイルが見つかりません:", file_path))
  }
  
  # 基本的なクリーニングと平均値計算
  df_summarized <- read.csv(file_path) |>
    # Map 列のクリーニング
    mutate(Map = str_remove(Map, "_.*")) |>
    
    # --- エージェント名の正規化 ---
    mutate(agent = case_when(
      str_detect(agent, regex("concentration", ignore_case = TRUE)) ~ "Concentration",
      str_detect(agent, regex("distributed", ignore_case = TRUE)) ~ "Distributed",
      TRUE ~ agent
    )) |>
    
    # rsl21_rate を計算
    mutate(rsl21_rate = final_rsl21 / initial_rsl21) |>
    
    # map と agent ごとに数値列の平均値を計算
    group_by(Map, agent) |>
    summarise(rsl21_rate = mean(rsl21_rate, na.rm = TRUE), .groups = "drop")
  
  # 2. データの変形 (ワイド形式へ)
  df_wide <- df_summarized |>
    pivot_wider(names_from = agent, values_from = rsl21_rate)
  
  # 3. 指定された2列を追加し、差が大きい順に並べ替える
  df_wide <- df_wide |>
    mutate(
      # 差の絶対値を計算し、小数点以下第6位で丸める
      abs_diff = round(abs(Concentration - Distributed), 6),
      # 高かった方の名前を判定
      higher_agent = if_else(Concentration > Distributed, "Concentration", "Distributed")
    ) |>
    # --- 並べ替え（abs_diffの降順） ---
    arrange(desc(abs_diff))
  
  # 4. out ディレクトリへの保存
  if (!dir.exists("out")) {
    dir.create("out", recursive = TRUE)
  }
  
  output_file <- file.path("out", "rsl21_rate_comparison.csv")
  write.csv(df_wide, output_file, row.names = FALSE)
  
  message(paste("集計完了。保存先:", output_file))
  
  return(df_wide)
}