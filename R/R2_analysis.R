# パッケージの読み込み
library(readr)
library(dplyr)
library(tidyverse)

# 各関数の読み込み
source("./R/preprocess2.R")
source("./R/check_vif2.R")
source("./R/make_mrm.R")
source("./R/make_plsr.R")
source("./R/mrm_prediction_error.R")
source("./R/plsr_prediction_error.R")
source("./R/mrm_cross_validation.R")
source("./R/plsr_cross_validation.R")
source("./R/rmse_results_compile.R")
source("./R/rmse_plot.R")
source("./R/ttest.R")
source("./R/cross_effect.R")
source("./R/shap.R")

vif_results_path <- "vif_check_log.txt"

# 各データファイルのパス
data_simulation <- "./data/simulation_scores.csv"
data_old_features <- "./data/old_features.csv"
data_new_features <- "./data/new_features.csv"

# シミュレーション結果データの整形
results_list <- preprocess2(data_simulation)

# 各エージェントのデータを取り出す
(distributed <- results_list$distributed)
(concentration <- results_list$concentration2_cluster_9_group_none)

# 特徴指標データの読み込み
# 旧特徴指標データ（建物データ）の読み込み
old_features <- read_csv(data_old_features)

selected_old_features <- check_vif2(old_features, distributed, vif_results_path, "old") |>
  mutate(across(-Map, ~ as.numeric(scale(.))))


# ==========================================
# 設定（ここを変更してください）
# ==========================================
input_csv_path   <- "AUR.csv"        # 全データ
# filter_csv_path <- ...             # フィルタリング機能は削除しました
target_var       <- "AUR"            # 目的変数

# 説明変数リスト（初期候補）
predictors <- c("c1", "c2", "c5", "c7", "c8", "c9", "c10", 
                "c11", "c12", "c13", "c14", "c15", "c16")

# ==========================================
# 1. データ読み込みと整形
# ==========================================

# 全データの読み込み
df_main <- read_csv(input_csv_path, show_col_types = FALSE)

# 必要な列だけ抽出 & Name列の .gml を削除
df_processed <- df_main %>%
  select(all_of(c(target_var, predictors, "Name"))) %>%
  mutate(Name = str_remove(Name, "\\.gml")) # .gml を削除

# 整形したデータをCSVに保存
output_filename <- paste0(target_var, ".csv")
write_csv(df_processed, output_filename)
message("整形済みデータを保存しました: ", output_filename)

# ==========================================
# 2. 分析用データの確定（全データを使用）
# ==========================================

# 以前のフィルタリング処理（inner_join）を削除し、
# 整形済みの全データ(df_processed)をそのまま分析対象(df_analysis)とします
df_analysis <- df_processed

message("分析対象のデータ数: ", nrow(df_analysis), "件")

# ==========================================
# 3. 多重共線性の確認と説明変数の更新
# ==========================================

# check_vif2に渡すためにデータを整形（関数が要求する列名に合わせる）
# 特徴量データ: NameをMapにリネーム
vif_features <- df_analysis %>% 
  select(Map = Name, all_of(predictors))

# 目的変数データ: Nameをmapに、ターゲットをfinal_rsl21にリネーム（check_vif2内の固定名に対応）
vif_response <- df_analysis %>% 
  select(map = Name, final_rsl21 = all_of(target_var))

# check_vif2を実行して選定された特徴量を取得
# ※ログファイルパス(vif_results_path)とデータタイプ("old")を指定
selected_features_df <- check_vif2(vif_features, vif_response, vif_results_path, "old")

# predictorsリストを更新（Map列以外を抽出）
predictors <- setdiff(colnames(selected_features_df), "Map")
message("選定された説明変数: ", paste(predictors, collapse = ", "))

# ==========================================
# 4. 重回帰分析
# ==========================================

# 回帰式を文字列で作成（更新されたpredictorsを使用）
formula_str <- paste(target_var, "~", paste(predictors, collapse = " + "))
formula_obj <- as.formula(formula_str)

# 重回帰分析の実行
model <- lm(formula_obj, data = df_analysis)

# 結果の表示
summary(model)
