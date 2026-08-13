library(ggplot2)
library(dplyr)
library(tidyr)

source("./R/preprocess.R")

data_simulation <- "./data/simulation_scores.csv"

# シミュレーション結果データの整形
results_list <- preprocess(data_simulation)

# 各エージェントのデータを取り出す
(distributed <- results_list$distributed)
(concentration <- results_list$concentration2_cluster_9_group_none)

library(ggplot2)
library(dplyr)
library(tidyr)

# 指標リスト
metrics <- c("rsl21_rate", "transport_rate", "buried_rescue_rate", "road_clear_rate")

# distributed の最初の4マップを使用
map_data <- distributed[1:4, ]  # 4マップ分

# データ整形
map_long <- map_data %>%
  select(all_of(metrics)) %>%
  mutate(map = paste0("Map", 1:4)) %>%  # マップラベル
  pivot_longer(cols = all_of(metrics), names_to = "metric", values_to = "value")

# ===============================
# 棒グラフ（各指標ごとに4マップ分）
# ===============================
ggplot(map_long, aes(x = map, y = value, fill = map)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ metric, scales = "free_y") +
  labs(x = "Map", y = "値", title = "各指標ごとの4マップ比較（棒グラフ）") +
  theme_minimal() +
  theme(legend.position = "none")

# ===============================
# 折れ線グラフ（各指標ごとに4マップ分）
# ===============================
ggplot(map_long, aes(x = map, y = value, group = 1)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  facet_wrap(~ metric, scales = "free_y") +
  labs(x = "Map", y = "値", title = "各指標ごとの4マップ比較（折れ線グラフ）") +
  theme_minimal()

# ---------------------------------------------------------------------
map_data <- distributed[1:4, ]  # 4マップ分

# rsl21_rate 用にデータを整形
# ※あえて pivot_longer を使わずとも描けますが、
#   元のコードの変数名 "value" や "map" の作り方を踏襲します。
plot_data_rsl21 <- map_data %>%
  select(rsl21_rate) %>%
  mutate(map = paste0("Map", 1:4)) %>%  # マップラベル作成
  mutate(metric = "rsl21_rate") %>%     # ファセットのヘッダー用にあえて列を作る
  rename(value = rsl21_rate)            # y軸名を元のコードの "value" に合わせる

# ===============================
# rsl21_rate のみの棒グラフ
# ===============================
ggplot(plot_data_rsl21, aes(x = map, y = value, fill = map)) +
  geom_bar(stat = "identity", position = "dodge") +
  
  # 元のコードと同じく metric で facet を作る（1つだけですが見た目を揃えるため）
  facet_wrap(~ metric, scales = "free_y") + 
  
  # ラベル・キャプションも元のコードの構成に合わせる
  labs(x = "Map", y = "値", title = "rsl21_rateの4マップ比較（棒グラフ）") +
  
  # テーマと凡例削除の設定も完全に一致させる
  theme_minimal() +
  theme(legend.position = "none")
