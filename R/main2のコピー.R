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

# 各関数の読み込み
source("./R/preprocess.R")
source("./R/feature_selection.R")
source("./R/check_vif.R")
source("./R/make_mrm.R")
source("./R/make_plsr.R")
source("./R/make_gbdt.R")
source("./R/make_svr.R")
source("./R/manova.R")
source("./R/mrm_prediction_error.R")
source("./R/plsr_prediction_error.R")
source("./R/gbdt_prediction_error.R")
source("./R/svr_prediction_error.R")
source("./R/manova.R")
source("./R/mrm_cross_validation.R")
source("./R/plsr_cross_validation.R")
source("./R/gbdt_cross_validation.R")
source("./R/svr_cross_validation.R")
source("./R/rmse_results_compile.R")
source("./R/rmse_plot.R")
source("./R/ttest.R")
source("./R/cross_effect.R")
source("./R/shap.R")
source("./R/stepwise.R")

# 各データファイルのパス
data_simulation <- "./data/simulation_scores.csv"
data_old_features <- "./data/old_features.csv"
data_new_features <- "./data/new_features.csv"

# 分析結果の出力ファイルパス
# 特徴量選択
feature_selection_results_path <- "./out/feature_selection_results_path.txt"
# 多重共線性
vif_results_path <- "./out/vif_results.txt"
# 重回帰モデル
mrm_results_path <- "./out/mrm_results.txt"
# PLSRモデル
plsr_results_path <- "./out/plsr_results.txt"
# GBDTモデル
gbdt_results_path <- "./out/gbdt_results.txt"
# SVRモデル
svr_results_path <- "./out/svr_results.txt"
# 重回帰モデルの予測誤差
mrm_prediction_error_results_path <- "./out/mrm_prediction_error_results.txt"
# PLSRモデルの予測誤差
plsr_prediction_error_results_path <- "./out/plsr_prediction_error_results.txt"
# GBDTモデルの予測誤差
gbdt_prediction_error_results_path <- "./out/gbdt_prediction_error_results.txt"
# SVRモデルの予測誤差
svr_prediction_error_results_path <- "./out/svr_prediction_error_results.txt"
# 重回帰モデルのMANOVA
mrm_manova_results_path <- "./out/mrm_manova_results.txt"
# PLSRモデルのMANOVA
plsr_manova_results_path <- "./out/plsr_manova_results.txt"
# GBDTモデルのMANOVA
gbdt_manova_results_path <- "./out/gbdt_manova_results.txt"
# SVRモデルのMANOVA
svr_manova_results_path <- "./out/svr_manova_results.txt"
# 重回帰モデルの交差検証
mrm_cross_validation_results_path <- "./out/mrm_cross_validation_results.txt"
# PLSRモデルの交差検証
plsr_cross_validation_results_path <- "./out/plsr_cross_validation_results.txt"
# GBDTモデルの交差検証
gbdt_cross_validation_results_path <- "./out/gbdt_cross_validation_results.txt"
# SVRモデルの交差検証
svr_cross_validation_results_path <- "./out/svr_cross_validation_results.txt"
# ステップワイズ（今は使ってない）
stepwise_results_path <- "./out/stepwise_results.txt"
# 特徴量重要度
shap_output_dir <- "./out/shap_plots"

# 未使用出力ファイルパス
# 新指標群データの説明変数間の相関係数確認
# r_results_path <- "./out/r_results.txt"
# stepwise
# stepwise_results_path <- "./out/stepwise_results.txt"
# PCA
# pca_results_path <- "./out/pca_resutlts.txt"
# Tankaiki
# tankaiki_results_path <- "./out/tankaiki_results.txt"

# シミュレーション結果データの整形
results_list <- preprocess(data_simulation)

# 各エージェントのデータを取り出す
(distributed <- results_list$distributed)
(concentration <- results_list$concentration2_cluster_9_group_none)

# 選定した道路指標群
road_target_cols <- c(
  "avg_degree",
  "degree_sd",
  "avg_local_clustering",
  "avg_path_length",
  "global_efficiency",
  "bridges_ratio",
  "edge_connectivity",
  "algebraic_connectivity",
  "zero_local_clustering_ratio",
  "articulation_points_ratio",
  "modularity",
  "centralization_degree",
  "degree_gini",
  "moran_i_degree",
  "getis_gi_degree_mean",
  "density",
  "assortativity",
  "degree_entropy_norm",
  "c15"
)

# 特徴指標データの読み込み
# 旧特徴指標データ（建物データ）の読み込み
building_features <- read_csv(data_old_features)
# 新特徴指標データ（道路データ）の読み込み
road_features <- read_csv(data_new_features)
# 新特徴指標データ（総合データ）
all_features <- left_join(road_features, building_features, by = "Map")
# 旧特徴指標データ（建物データ）の作成
building_features <- building_features |> dplyr::select(-c14, -c15)
# 新特徴指標データ（道路データ）の作成
road_features <- all_features |> dplyr::select(Map, all_of(road_target_cols))
# 新特徴指標データ（総合データ）の作成
all_features <- left_join(road_features, building_features, by = "Map")

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

# 特徴指標数の取得（PLSRの交差検証に利用）
# 建物指標群データの特徴指標数を取得
building_features_number <- ncol(selected_building_features |> dplyr::select(-Map))
# 道路指標群データの特徴指標数を取得
road_features_number <- ncol(selected_road_features |> dplyr::select(-Map))
# 総合指標群データの特徴指標数を取得
all_features_number <- ncol(selected_all_features |> dplyr::select(-Map))

# 重回帰モデルの作成
# 分散救助エージェントの重回帰モデルの作成
distributed_mr_models <- make_mrm(distributed, selected_building_features, selected_road_features, selected_all_features, "distributed", mrm_results_path)
# 集中救助エージェントの重回帰モデルの作成
concentration_mr_models <- make_mrm(concentration, selected_building_features, selected_road_features, selected_all_features, "concentration", mrm_results_path)

# PLSRモデルの作成
# 分散救助エージェントのPLSRモデルの作成
distributed_plsr_models <- make_plsr(distributed, selected_building_features, selected_road_features, selected_all_features, "distributed", plsr_results_path)
# 集中救助エージェントのPLSRモデルの作成
concentration_plsr_models <- make_plsr(concentration, selected_building_features, selected_road_features, selected_all_features, "concentration", plsr_results_path)

# 重回帰モデルの交差検証
# 分散救助エージェントの重回帰モデルにおける交差検証を実行
mrm_cv_results_distributed <- mrm_cv(distributed, selected_building_features, selected_road_features, selected_all_features, "distributed", mrm_cross_validation_results_path, 5)
# 集中救助エージェントの重回帰モデルにおける交差検証を実行
mrm_cv_results_concentration <- mrm_cv(concentration, selected_building_features, selected_road_features, selected_all_features, "concentration", mrm_cross_validation_results_path, 5)

# PLSRモデルの交差検証
# 分散救助エージェントのPLSRモデルにおける交差検証を実行
plsr_cv_results_distributed <- plsr_cv(distributed, selected_building_features, selected_road_features, selected_all_features, "distributed", plsr_cross_validation_results_path, 5, building_features_number, road_features_number, all_features_number)
# 集中救助エージェントのPLSRモデルにおける交差検証を実行
plsr_cv_results_concentration <- plsr_cv(concentration, selected_building_features, selected_road_features, selected_all_features, "concentration", plsr_cross_validation_results_path, 5, building_features_number, road_features_number, all_features_number)

# RMSEデータの集計
rmse_data <- compile_rmse_data(mrm_cv_results_concentration, plsr_cv_results_concentration)

# RMSEの可視化
graph <- plot_rmse_comparison(rmse_data, title_prefix = "集中エージェントのRMSE比較")

# RMSE比較グラフ画像の保存
ggsave("rmse_comparison_concentration.png", graph, width = 15, height = 10)

# 対応のあるt検定
ttest_conc_new_vs_all <- perform_ttest_rmse(
  results_A = plsr_cv_results_concentration$old_models,
  results_B = plsr_cv_results_concentration$all_models,
  name_A = "Building(Old)",
  name_B = "Integrated(All)"
)

# 結果の出力
print(ttest_conc_new_vs_all)

# 分散エージェント (Distributed) / 総合指標 (All) のSHAP分析
analyze_shap(
  plsr_models = distributed_plsr_models$all_models,
  feature_data = selected_all_features |> dplyr::select(-Map), # 特徴量データ(X)
  output_path = paste0(shap_output_dir, "/distributed"),
  title_prefix = "Distributed"
)

# 集中エージェント (Concentration) / 総合指標 (All) のSHAP分析
analyze_shap(
  plsr_models = concentration_plsr_models$all_models,
  feature_data = selected_all_features |> dplyr::select(-Map),
  output_path = paste0(shap_output_dir, "/concentration"),
  title_prefix = "Concentration"
)