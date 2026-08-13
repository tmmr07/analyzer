# パッケージのインストール（必要なものだけコメントアウトを外して実行）
# install.packages("car")
# install.packages("dplyr")
# install.packages("tibble")
# install.packages("readr")
# install.packages("stringr")
# install.packages("purrr")
# install.packages("pls")
# install.packages("xgboost")
# install.packages("caret")
# install.packages("e1071")
# install.packages("MVN")
# install.packages("psych")
# install.packages("corrplot")

# パッケージの読み込み
library(readr)
library(dplyr)
library(corrplot) # 相関行列の確認用
library(ggplot2)
library(reshape2)

# 各関数の読み込み
source("./R/preprocess.R")
source("./R/check_vif.R")
source("./R/make_mrm.R")
source("./R/partial_f_test.R")
source("./R/shap.R")

# 各データファイルのパス
data_simulation <- "./data/simulation_scores.csv"
data_old_features <- "./data/old_features.csv"
data_new_features <- "./data/new_features_d_all.csv"

# 分析結果の出力ファイルパス
# 多重共線性
vif_results_path <- "./out/vif_results.txt"
# 重回帰モデル
mrm_results_path <- "./out/mrm_results.txt"
# 部分F検定
partial_f_test_path <- "./out/partial_f_test_results.txt"
# 特徴量重要度
shap_output_dir <- "./out/shap_plots"

# シミュレーション結果データの整形
results_list <- preprocess(data_simulation)

# 各エージェントのデータを取り出す
(distributed <- results_list$distributed)
(concentration <- results_list$concentration2_cluster_9_group_none)

write_csv(distributed, "distributed_data.csv")

# 選定した道路指標群
road_target_cols <- c(
  "degree_sd",
  "centralization_degree",
  "bridges_ratio",
  "assortativity",
  "moran_i_degree",
  "getis_gi_degree_mean",
  "rosval_mean",
  "orientation_entropy",
  "d_value"
)

# 特徴指標データの読み込み
# 旧特徴指標データ（建物データ）の読み込み
building_features <- read_csv(data_old_features)
# 新特徴指標データ（道路データ）の読み込み
road_features <- read_csv(data_new_features)
# 新特徴指標データ（総合データ）
all_features <- left_join(road_features, building_features, by = "Map")
# 旧特徴指標データ（建物データ）の作成
# building_features <- building_features |> dplyr::select(-c14, -c15)
building_features <- building_features
# 新特徴指標データ（道路データ）の作成
road_features <- all_features |> dplyr::select(Map, all_of(road_target_cols))
# 新特徴指標データ（総合データ）の作成
all_features <- left_join(road_features, building_features, by = "Map")


# --- 1. データの準備 ---
# 数値データのみを抽出 (Map列などの文字データを除外)
cor_data <- all_features |>
  select(where(is.numeric))

# 相関行列を計算 (欠損値がある場合は除外)
cor_matrix <- cor(cor_data, use = "complete.obs")

# 上三角行列（対角線より上）を NA にして、下三角のみ残す
cor_matrix[upper.tri(cor_matrix)] <- NA

# ggplot2 用の形式に変換
melted_cor <- melt(cor_matrix, na.rm = TRUE)

# 軸の順序をデータの列順に固定（これがないとアルファベット順に勝手に変わります）
vars <- colnames(cor_data)
melted_cor$Var1 <- factor(melted_cor$Var1, levels = rev(vars)) # Y軸は逆順
melted_cor$Var2 <- factor(melted_cor$Var2, levels = vars)      # X軸は正順

# --- 2. ggplot2 による描画 ---
p <- ggplot(data = melted_cor, aes(x = Var2, y = Var1, fill = value)) +
  # (1) ヒートマップのタイルを描画
  geom_tile(color = "white") +
  
  # (2) 数値の表示 (変数が多くて数字が重なる場合は size を 2.5 くらいに小さくしてください)
  geom_text(aes(label = sprintf("%.2f", value)), size = 3, color = "black") +
  
  # (3) カラースケール (青-白-赤)
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  
  # (4) 軸ラベル設定 (変数名が表示されるので X, Y のタイトルは消す)
  labs(x = NULL, y = NULL) +
  
  # (5) 正方形に固定
  coord_fixed() +
  
  # (6) 見た目の調整
  theme_minimal() +
  theme(
    # X軸のラベルを45度回転させて重ならないようにする
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
    axis.text.y = element_text(vjust = 0.5, size = 10),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right"
  )

# 【PNG形式】 (一般的な画像、Wordやパワポ貼り付け用)
# dpi = 300 以上が高画質印刷の標準です（通常は72）
# width, height で画像の物理サイズ（インチ）を指定して、文字重なりを防ぎます
ggsave("correlation_matrix_high_res.png", plot = p, width = 12, height = 12, dpi = 300)

# 【PDF形式】 (論文・LaTeX用、どれだけ拡大しても劣化しないベクター画像)
ggsave("correlation_matrix.pdf", plot = p, width = 12, height = 12)



# building_features <- building_features |> dplyr::select(-c14)

all_features <- all_features |> dplyr::select(-c1, -c13, -centralization_degree, -degree_sd)

# 多重共線性の確認
# 建物データの多重共線性の確認（建物指標群データの作成）
selected_building_features <- check_vif(building_features, distributed, vif_results_path, "old")|>
  mutate(across(-Map, ~ as.numeric(scale(.))))
# 道路データの多重共線性の確認（道路指標群データの作成）
selected_road_features <- check_vif(road_features, distributed, vif_results_path, "new")|>
  mutate(across(-Map, ~ as.numeric(scale(.))))
# 総合データの多重共線性の確認（総合指標群データの作成）
selected_all_features <- check_vif(all_features, distributed, vif_results_path, "all")|>
  mutate(across(-Map, ~ as.numeric(scale(.))))

# 重回帰モデルの作成
# 分散救助エージェントの重回帰モデルの作成
distributed_mr_models <- make_mrm(distributed, selected_building_features, selected_road_features, selected_all_features, "distributed", mrm_results_path)
# 集中救助エージェントの重回帰モデルの作成
concentration_mr_models <- make_mrm(concentration, selected_building_features, selected_road_features, selected_all_features, "concentration", mrm_results_path)

# 分散救助エージェントの部分F検定
distributed_f_test_results <- perform_partial_f_test(
  old_models = distributed_mr_models$old_models,
  all_models = distributed_mr_models$all_models,
  agent_type = "distributed",
  output_path = partial_f_test_path
)

concentration_f_test_results <- perform_partial_f_test(
  old_models = concentration_mr_models$old_models,
  all_models = concentration_mr_models$all_models,
  agent_type = "concentration",
  output_path = partial_f_test_path
)

# 分散エージェント (Distributed) / 総合指標 (All) のSHAP分析
analyze_shap_lm(
  lm_models   = distributed_mr_models$all_models,
  feature_data = selected_all_features |> dplyr::select(-Map),
  output_path = paste0(shap_output_dir, "/distributed"),
  title_prefix = "Distributed"
)

# 集中エージェント (Concentration) / 総合指標 (All) のSHAP分析
analyze_shap_lm(
  lm_models   = concentration_mr_models$all_models,
  feature_data = selected_all_features |> dplyr::select(-Map),
  output_path = paste0(shap_output_dir, "/concentration"),
  title_prefix = "Concentration"
)

