# データ読み込み
data <- read.csv("/Users/k22129kk/Desktop/tmp/new_scenario_hendoukeisuu.csv", header = FALSE, col.names = c("country", "value"))

# value列を明示的に数値に変換（必要に応じて文字列→数値）
data$value <- as.numeric(data$value)

# NA（数値変換できなかった行）を除外
data <- data %>% filter(!is.na(value))

# dplyrパッケージの読み込み
library(dplyr)

# 各国ごとの平均、標準偏差、変動係数（CV）を計算
summary_stats <- data %>%
  group_by(country) %>%
  summarise(
    mean = mean(value),
    sd = sd(value),
    cv = sd / mean,
    .groups = "drop"  # グループを解除しておくと便利
  )

# 結果表示
print(summary_stats)
