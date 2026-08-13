# 必要なパッケージ
library(dplyr)
library(readr)
library(psych)

#' 予測誤差データを用いて，予測精度が有意的に改善されたかを検定する関数
#' 
#' @param errors_list 予測誤差データ
#' @param agent_type エージェントの種類
#' @param output_path 結果ログを出力するファイルパス
#'
#' @return MANOVAの結果

perform_manova_test <- function(errors_list, agent_type, output_path) {
  
  # ヘッダー
  if (agent_type == "distributed") {
    write_lines("----------------------分散救助エージェントのMANOVA検定----------------------", output_path, append = TRUE)
  } else if (agent_type == "concentration") {
    write_lines("----------------------集中救助エージェントのMANOVA検定----------------------", output_path, append = TRUE)
  }
  
  # データ取得
  errors_old <- errors_list$errors_old %>% select(-map)
  errors_new <- errors_list$errors_new %>% select(-map)
  errors_all <- errors_list$errors_all %>% select(-map)
  
  # 軸ラベル作成
  df <- rbind(
    cbind(errors_old, group = "old"),
    cbind(errors_new, group = "new"),
    cbind(errors_all, group = "all")
  )
  df$group <- factor(df$group)
  # df$group <- factor(df$group, levels = c("old", "new", "all"))
  
  # 誤差データの標準化
  response_vars <- colnames(errors_old)
  df[, response_vars] <- scale(df[, response_vars])
  
  write_lines("多変量正規性検定", output_path, append = TRUE)
  
  groups <- levels(df$group)
  
  for (g in groups) {
    write_lines(paste0("group: ", g), output_path, append = TRUE)
    
    sub <- df[df$group == g, response_vars]
    
    mrd <- psych::mardia(sub, plot = FALSE)
    
    write_lines("Skewness（歪度）検定:", output_path, append = TRUE)
    write_lines(capture.output(print(mrd$skew)), output_path, append = TRUE)
    
    write_lines("Kurtosis（尖度）検定:", output_path, append = TRUE)
    write_lines(capture.output(print(mrd$kurtosis)), output_path, append = TRUE)
  }
  
  write_lines("", output_path, append = TRUE)
  write_lines("", output_path, append = TRUE)
  write_lines("", output_path, append = TRUE)
  write_lines("", output_path, append = TRUE)
  
  manova_fit <- manova(as.matrix(df[, response_vars]) ~ group, data = df)
  
  write_lines("", output_path, append = TRUE)
  write_lines("全体の多変量検定（Wilks' Lambda）:", output_path, append = TRUE)
  write_lines(capture.output(print(summary(manova_fit, test = "Wilks"))), output_path, append = TRUE)
  
  write_lines("\n各目的変数ごとの単変量 ANOVA:", output_path, append = TRUE)
  write_lines(capture.output(print(summary.aov(manova_fit))), output_path, append = TRUE)
  
  # フッター
  if (agent_type == "distributed") {
    write_lines("----------------------分散救助エージェントのMANOVA検定終了----------------------", output_path, append = TRUE)
  } else if (agent_type == "concentration") {
    write_lines("----------------------集中救助エージェントのMANOVA検定終了----------------------", output_path, append = TRUE)
  }
  
  print(identical(errors_list$errors_old %>% select(-map), errors_list$errors_new %>% select(-map)))
  print(identical(errors_list$errors_new %>% select(-map), errors_list$errors_all %>% select(-map)))
  
  return(manova_fit)
}
