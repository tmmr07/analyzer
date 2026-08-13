library(dplyr)
library(stringr)

preprocess <- function(file_path) {
  
  df_summary <- read.csv(file_path) |>
    
    # map 列をクリーニング
    mutate(map = str_remove(map, "_.*")) |>
    
    # rsl21_rate を計算
    mutate(rsl21_rate = final_rsl21 / initial_rsl21) |>
    
    # 不要列を削除
    dplyr::select(-any_of(c("initial_timestep", "final_timestep", 
                     "initial_rsl21", "final_rsl21"))) |>
    
    # map と agent ごとに数値列の平均値を計算
    group_by(map, agent) |>
    summarise(across(where(is.numeric), ~mean(as.numeric(.x), na.rm = TRUE)), .groups = "drop")
  
  # agent ごとにデータフレームをリストに分割
  agent_df_list <- split(df_summary, df_summary$agent)
  
  return(agent_df_list)
}
