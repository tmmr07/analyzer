library(dplyr)
library(stringr)
library(tidyr)

preprocess <- function(file_path) {
  
  # データの読み込み
  if (!file.exists(file_path)) {
    stop(paste("ファイルが見つかりません:", file_path))
  }
  
  df_summarized <- read.csv(file_path) |>
    mutate(Map = str_remove(Map, "_.*")) |>
    
    mutate(agent = case_when(
      str_detect(agent, regex("concentration", ignore_case = TRUE)) ~ "Concentration",
      str_detect(agent, regex("distributed", ignore_case = TRUE)) ~ "Distributed",
      TRUE ~ agent
    )) |>
    
    # rsl21_rateを計算
    mutate(rsl21_rate = final_rsl21 / initial_rsl21) |>
    
    # map と agent ごとに数値列の平均値を計算
    group_by(Map, agent) |>
    summarise(rsl21_rate = mean(rsl21_rate, na.rm = TRUE), .groups = "drop")
  
  df_wide <- df_summarized |>
    pivot_wider(names_from = agent, values_from = rsl21_rate)
  
  df_wide <- df_wide |>
    mutate(
      abs_diff = round(abs(Concentration - Distributed), 6),
      higher_agent = if_else(Concentration > Distributed, "Concentration", "Distributed")
    ) |>
    arrange(desc(abs_diff))
  
  if (!dir.exists("out")) {
    dir.create("out", recursive = TRUE)
  }
  
  output_file <- file.path("out", "rsl21_rate_comparison.csv")
  write.csv(df_wide, output_file, row.names = FALSE)
  
  message(paste("集計完了。保存先:", output_file))
  
  return(df_wide)
}