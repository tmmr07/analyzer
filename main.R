# パッケージの読み込み
library(glmnet)
library(dplyr)
library(tibble)
library(readr)

# 各関数の読み込み
source("./R/preprocess.R")
source("./R/Lasso.R")
source("./R/wilcox_test.R")
source("./R/shap.R")

# 各データファイルのパス
data_simulation <- "./data/simulation_scores.csv"
data_old_features <- "./data/old_features.csv"
data_new_features <- "./data/new_features_d_road_and_building.csv"

# 分析結果の出力ファイルパス

# シミュレーション結果データの整形
results_list <- preprocess(data_simulation)

# 各エージェントのデータを取り出す
(distributed <- results_list$distributed)
(concentration <- results_list$concentration2_cluster_9_group_none)

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
# 旧指標の読み込み（旧指標データの作成）
building_features <- read_csv(data_old_features)
# 新指標の読み込み
road_features <- read_csv(data_new_features)
# 旧指標と新指標の統合
all_features <- left_join(road_features, building_features, by = "Map")
# 新指標データの作成
road_features <- all_features |> dplyr::select(Map, all_of(road_target_cols))
# 総合指標データの作成
all_features <- left_join(road_features, building_features, by = "Map")

standardize_cols <- function(x) {
  as.numeric(scale(x))
}

# 各データフレームを標準化
building_features <- building_features |> 
  mutate(across(where(is.numeric), standardize_cols))

road_features <- road_features |> 
  mutate(across(where(is.numeric), standardize_cols))

all_features <- all_features |> 
  mutate(across(where(is.numeric), standardize_cols))

output_dir <- "./out/feature_sets"
dir.create(output_dir, showWarnings = FALSE)


write_csv(
  building_features,
  file.path(output_dir, "features_old.csv")
)

write_csv(
  road_features,
  file.path(output_dir, "features_new.csv")
)

write_csv(
  all_features,
  file.path(output_dir, "features_all.csv")
)


# 説明変数のパターンをリストにまとめる
feature_sets <- list(
  "Old_Features" = building_features,
  "New_Features" = road_features,
  "All_Features" = all_features
)

# 目的変数のデータセットをリストにまとめる
target_datasets <- list(
  "Distributed" = distributed,
  "Concentration" = concentration
)

all_results <- data.frame()

# --- 保存先のベースディレクトリ ---
output_base_dir <- "./out"

# --- 実行＆保存ループ ---

# 1. エージェントごとのループ (Distributed / Concentration)
for (target_set_name in names(target_datasets)) {
  
  print(paste("Processing Agent:", target_set_name, "..."))
  current_target_df <- target_datasets[[target_set_name]]
  
  # 数値列のみ抽出
  numeric_cols <- current_target_df |> 
    dplyr::select(where(is.numeric)) |> 
    colnames()
  target_columns <- setdiff(numeric_cols, "Map")
  
  # 2. 目的変数ごとのループ (例: score, time 等)
  for (target_col in target_columns) {
    
    # 3. 説明変数群ごとのループ (Old, New, All)
    for (feat_set_name in names(feature_sets)) {
      
      current_feature_df <- feature_sets[[feat_set_name]]
      
      # 分析実行
      res <- run_lasso_analysis(
        target_df = current_target_df, 
        feature_df = current_feature_df, 
        target_col_name = target_col, 
        feature_set_name = feat_set_name
      )
      
      # 結果が存在する場合のみ保存処理を行う
      if (!is.null(res)) {
        
        # エージェント名の列を追加（念のためデータ内にも保持）
        res$AgentType <- target_set_name
        
        # --- 保存先パスの構築 ---
        # 構造: ./out/{Agent}/{Target}/{FeatureSet}/result.csv
        save_dir <- file.path(output_base_dir, target_set_name, target_col, feat_set_name)
        
        # ディレクトリを再帰的に作成 (親フォルダがない場合も自動で作る)
        if (!dir.exists(save_dir)) {
          dir.create(save_dir, recursive = TRUE)
        }
        
        # ファイルパスの作成
        file_path <- file.path(save_dir, "result.csv")
        
        # 係数の絶対値順に並べ替え
        res_sorted <- res |> arrange(desc(abs(Coefficient)))
        
        # CSV保存
        write_csv(res_sorted, file_path)
        
        # --- 2. SHAP分析の実行 (★追加部分) ---
        
        # Lassoモデルはrun_lasso_analysis内で消えているため、SHAP用にデータを再構築してモデルを作る
        
        if (feat_set_name == "All_Features") {
          # データの結合と整形（run_lasso_analysisと同等の処理）
          data_merged <- inner_join(
            current_target_df |> dplyr::select(Map, all_of(target_col)),
            current_feature_df,
            by = "Map"
          ) |> na.omit()
          
          # SHAP用データフレーム（Mapと目的変数を除去した説明変数のみ）
          feature_data_for_shap <- data_merged |> dplyr::select(-Map, -all_of(target_col))
          
          # モデル学習用データ
          x_matrix <- as.matrix(feature_data_for_shap)
          y_vec <- as.numeric(data_merged[[target_col]])
          
          # Lassoモデルの再作成
          # ※ set.seed(123)を指定することで、さっきの分析と全く同じモデルになる
          set.seed(123)
          cv_fit_for_shap <- cv.glmnet(x_matrix, y_vec, alpha = 1)
          
          # SHAP結果の保存先: ./out/{Agent}/{Target}/{FeatureSet}/SHAP/
          shap_out_dir <- file.path(save_dir, "SHAP")
          
          # 関数の実行
          analyze_shap_lasso(
            lasso_model  = cv_fit_for_shap,
            feature_data = feature_data_for_shap,
            output_path  = shap_out_dir,
            target_name  = target_col,
            title_prefix = paste0(target_set_name, "_", feat_set_name)
          )
        }
      }
    }
  }
}




# 結果を貯めるデータフレーム
test_results_summary <- data.frame()

# 1. エージェントごとのループ
for (target_set_name in names(target_datasets)) {
  
  print(paste("Running Statistical Test for:", target_set_name, "..."))
  current_target_df <- target_datasets[[target_set_name]]
  
  # 数値列のみ抽出
  numeric_cols <- current_target_df |> 
    dplyr::select(where(is.numeric)) |> 
    colnames()
  target_columns <- setdiff(numeric_cols, "Map")
  
  # 2. 目的変数ごとのループ
  for (target_col in target_columns) {
    
    # --- 検定の実行 (Old vs All) ---
    # ここでは features_sets$Old_Features と features_sets$All_Features を比較
    res_test <- compare_models_statistical(
      target_df = current_target_df,
      old_feature_df = feature_sets[["Old_Features"]], # 比較対象1
      all_feature_df = feature_sets[["All_Features"]], # 比較対象2
      target_col = target_col,
      agent_name = target_set_name
    )
    
    # エージェント名を追加して結果リストに結合
    res_test$AgentType <- target_set_name
    test_results_summary <- bind_rows(test_results_summary, res_test)
  }
}

# --- 検定結果のCSV保存 ---
# 全エージェント・全目的変数の検定結果を1つのファイルに保存
output_test_file <- file.path(output_base_dir, "statistical_test_results_Old_vs_All.csv")
write_csv(test_results_summary, output_test_file)

print("Statistical testing finished.")
print(test_results_summary)

