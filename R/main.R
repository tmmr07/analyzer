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

source("./R/stepwise.R")

# 未使用関数
# source("./R/check_r.R")
# source("./R/check_vif_subset.R")
# source("./R/run_pca.R")
# source("./R/tankaiki.R")
# source("./R/stepwise.R")

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

stepwise_results_path <- "./out/stepwise_results.txt"

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

# 選定した特徴指標（新・尖った8指標セット）


# 特徴指標データの読み込み
# 旧特徴指標データの読み込み（旧指標群データの作成）
old_features <- read_csv(data_old_features)
# 新特徴指標データの読み込み
new_features_tmp <- read_csv(data_new_features) |> dplyr::select(where(~ !anyNA(.)))
  # select(all_of(target_columns))
# 新特徴指標データに旧特徴指標データを結合
all_features_tmp <- left_join(new_features, old_features, by = "Map")

# 特徴量選択
# 新指標群データの特徴量選択
# selection_new_features <- feature_selection(new_features_tmp, distributed, feature_selection_results_path, 0.70, "new")
# 総合指標群データの特徴量選択
# selection_all_features <- feature_selection(all_features_tmp, distributed, feature_selection_results_path, 0.70, "all")

# 多重共線性の確認
# 旧指標群の多重共線性の確認（選定済み旧指標群データの作成）
selected_old_features <- check_vif(old_features, distributed, vif_results_path, "old")
# 新指標群の多重共線性の確認（新指標群データの作成，選定済み新指標群データも兼ねる）
new_features <- check_vif(new_features_tmp, distributed, vif_results_path, "new")
# 総合指標群の多重共線性の確認（総合指標群データの作成，選定済み総合指標群データも兼ねる）
all_features <- check_vif(all_features_tmp, distributed, vif_results_path, "all")



# 旧指標群のステップワイズ
selected_old_features <- perform_stepwise(
  data_sim = distributed, 
  data_features = selected_old_features, 
  target_var = "final_alive", 
  output_path = stepwise_results_path, 
  label = "old"
)

# 新指標群のステップワイズ (変数が少ないのでやらなくてもいいが、やるならこう)
# ※新指標は厳選した8個なので、ステップワイズせずそのまま使うのがおすすめですが、
#   機械的に減らしたい場合は以下を実行。今回はそのまま new_features に代入します。
# selected_new_features <- perform_stepwise(distributed, vif_checked_new_features, "final_alive", stepwise_results_path, "new")
new_features <- perform_stepwise(
  data_sim = distributed, 
  data_features = new_features, 
  target_var = "final_alive", 
  output_path = stepwise_results_path, 
  label = "new"
)

# 総合指標群のステップワイズ
# 旧指標と新指標が混ざると数が多くなるので、これは実行する価値が高いです
all_features <- perform_stepwise(
  data_sim = distributed, 
  data_features = vif_checked_all_features, 
  target_var = "final_alive", 
  output_path = stepwise_results_path, 
  label = "all"
)



# 特徴指標数の取得（PLSRの交差検証に利用）
# 旧指標群データの特徴指標数を取得
old_features_number <- ncol(old_features |> dplyr::select(-Map))
# 新指標群データの特徴指標数を取得
new_features_number <- ncol(new_features |> dplyr::select(-Map))
# 総合指標群データの特徴指標数を取得
all_features_number <- ncol(all_features |> dplyr::select(-Map))

# 重回帰モデルの作成
# 分散救助エージェントの重回帰モデルの作成
distributed_mr_models <- make_mrm(distributed, selected_old_features, new_features, all_features, "distributed", mrm_results_path)
# 集中救助エージェントの重回帰モデルの作成
concentration_mr_models <- make_mrm(concentration, selected_old_features, new_features, all_features, "concentration", mrm_results_path)

# PLSRモデルの作成
# 分散救助エージェントのPLSRモデルの作成
distributed_plsr_models <- make_plsr(distributed, old_features, new_features, all_features, "distributed", plsr_results_path)
# 集中救助エージェントのPLSRモデルの作成
concentration_plsr_models <- make_plsr(concentration, old_features, new_features, all_features, "concentration", plsr_results_path)

# GBDTモデルの作成
# 分散救助エージェントのGBDTの作成
distributed_gbdt_models <- make_gbdt(distributed, old_features, new_features, all_features, "distributed", gbdt_results_path)
# 集中救助エージェントのGBDTの作成
concentration_gbdt_models <- make_gbdt(concentration, old_features, new_features, all_features, "concentration", gbdt_results_path)

# SVRモデルの作成
# 分散救助エージェントのSVRの作成
distributed_svr_models <- make_svr(distributed, old_features, new_features, all_features, "distributed", svr_results_path)
# 集中救助エージェントのSVRの作成
concentration_svr_models <- make_svr(concentration, old_features, new_features, all_features, "concentration", svr_results_path)

# 重回帰モデルの予測誤差
# 分散救助エージェントの重回帰モデルにおける予測誤差を算出
distributed_mrm_prediction_error <- calculate_lm_errors(distributed, selected_old_features, new_features, all_features, "distributed", mrm_prediction_error_results_path, distributed_mr_models)
# 集中救助エージェントの重回帰モデルにおける予測誤差を算出
concentration_mrm_prediction_error <- calculate_lm_errors(concentration, selected_old_features, new_features, all_features, "concentration", mrm_prediction_error_results_path, concentration_mr_models)

# PLSRモデルの予測誤差
# 分散救助エージェントのPLSRモデルにおける予測誤差を算出
distributed_plsr_prediction_error <- calculate_plsr_errors(distributed, old_features, new_features, all_features, "distributed", plsr_prediction_error_results_path, distributed_plsr_models)
# 集中救助エージェントのPLSRモデルにおける予測誤差を算出
concentration_plsr_prediction_error <- calculate_plsr_errors(concentration, old_features, new_features, all_features, "concentration", plsr_prediction_error_results_path, concentration_plsr_models)

# GBDTモデルの予測誤差
# 分散救助エージェントのGBDTモデルにおける予測誤差を算出
distributed_gbdt_prediction_error <- calculate_gbdt_errors(distributed, old_features, new_features, all_features, "distributed", gbdt_prediction_error_results_path, distributed_gbdt_models)
# 集中救助エージェントのGBDTモデルにおける予測誤差を算出
concentration_gbdt_prediction_error <- calculate_gbdt_errors(concentration, old_features, new_features, all_features, "concentration", gbdt_prediction_error_results_path, concentration_gbdt_models)

# SVRモデルの予測誤差
# 分散救助エージェントのSVRモデルにおける予測誤差を算出
distributed_svr_prediction_error <- calculate_svr_errors(distributed, old_features, new_features, all_features, "distributed", svr_prediction_error_results_path, distributed_svr_models)
# 集中救助エージェントのSVRモデルにおける予測誤差を算出
concentration_svr_prediction_error <- calculate_svr_errors(concentration, old_features, new_features, all_features, "concentration", svr_prediction_error_results_path, concentration_svr_models)

# 重回帰モデルのMANOVA
# 分散救助エージェントの重回帰モデルの誤差データを用いたMANOVAを実行
distributed_mrm_manova <- perform_manova_test(distributed_mrm_prediction_error, "distributed", mrm_manova_results_path)
# 集中救助エージェントの重回帰モデルの誤差データを用いたMANOVAを実行
concentration_mrm_manova <- perform_manova_test(concentration_mrm_prediction_error, "concentration", mrm_manova_results_path)

# PLSRモデルのMANOVA
# 分散救助エージェントのPLSRモデルの誤差データを用いたMANOVAを実行
distributed_plsr_manova <- perform_manova_test(distributed_plsr_prediction_error, "distributed", plsr_manova_results_path)
# 集中救助エージェントのPLSRモデルの誤差データを用いたMANOVAを実行
concentration_plsr_manova <- perform_manova_test(concentration_plsr_prediction_error, "concentration", plsr_manova_results_path)

# GBDTモデルのMANOVA
# 分散救助エージェントのGBDTモデルの誤差データを用いたMANOVAを実行
distributed_gbdt_manova <- perform_manova_test(distributed_gbdt_prediction_error, "distributed", gbdt_manova_results_path)
# 集中救助エージェントのGBDTモデルの誤差データを用いたMANOVAを実行
concentration_gbdt_manova <- perform_manova_test(concentration_gbdt_prediction_error, "concentration", gbdt_manova_results_path)

# SVRモデルのMANOVA
# 分散救助エージェントのSVRモデルの誤差データを用いたMANOVAを実行
distributed_svr_manova <- perform_manova_test(distributed_svr_prediction_error, "distributed", svr_manova_results_path)
# 集中救助エージェントのSVRモデルの誤差データを用いたMANOVAを実行
concentration_svr_manova <- perform_manova_test(concentration_svr_prediction_error, "concentration", svr_manova_results_path)

# 重回帰モデルの交差検証
# 分散救助エージェントの重回帰モデルにおける交差検証を実行
mrm_cv(distributed, selected_old_features, new_features, all_features, "distributed", mrm_cross_validation_results_path, 5)
# 集中救助エージェントの重回帰モデルにおける交差検証を実行
mrm_cv(concentration, selected_old_features, new_features, all_features, "concentration", mrm_cross_validation_results_path, 5)

# PLSRモデルの交差検証
# 分散救助エージェントのPLSRモデルにおける交差検証を実行
plsr_cv(distributed, old_features, new_features, all_features, "distributed", plsr_cross_validation_results_path, 5, old_features_number, new_features_number, all_features_number)
# 集中救助エージェントのPLSRモデルにおける交差検証を実行
plsr_cv(concentration, old_features, new_features, all_features, "concentration", plsr_cross_validation_results_path, 5, old_features_number, new_features_number, all_features_number)

# GBDTモデルの交差検証
# 分散救助エージェントのGBDTモデルにおける交差検証を実行
gbdt_cv(distributed, old_features, new_features, all_features, "distributed", gbdt_cross_validation_results_path, 5)
# 集中救助エージェントのGBDTモデルにおける交差検証を実行
gbdt_cv(concentration, old_features, new_features, all_features, "concentration", gbdt_cross_validation_results_path, 5)

# SVRモデルの交差検証
# 分散救助エージェントのSVRモデルにおける交差検証を実行
svr_cv(distributed, old_features, new_features, all_features, "distributed", svr_cross_validation_results_path, 5)
# 集中救助エージェントのSVRモデルにおける交差検証を実行
svr_cv(concentration, old_features, new_features, all_features, "concentration", svr_cross_validation_results_path, 5)
