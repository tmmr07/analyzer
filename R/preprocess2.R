library(dplyr)
library(stringr)

preprocess2 <- function(file_path) {
  
  df_summary <- read.csv(file_path) |>
    
    # map 列をクリーニング
    mutate(map = str_remove(map, "_.*")) |>
    
    # 【変更点1】 rsl21_rate の計算処理を削除しました
    # mutate(rsl21_rate = final_rsl21 / initial_rsl21) |>
    
    # 【変更点2】 final_rsl21 (または final_rsl21_rate) を削除対象から除外して残すようにしました
    # ※ initial_rsl21 は不要であれば削除リストに残します
    dplyr::select(-any_of(c("initial_timestep", "final_timestep", 
                            "initial_rsl21"))) |>
    
    # map と agent ごとに数値列の平均値を計算
    group_by(map, agent) |>
    summarise(across(where(is.numeric), ~mean(as.numeric(.x), na.rm = TRUE)), .groups = "drop")
  
  # agent ごとにデータフレームをリストに分割
  agent_df_list <- split(df_summary, df_summary$agent)
  
  return(agent_df_list)
}