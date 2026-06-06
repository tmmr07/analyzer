# パッケージの読み込み
library(glmnet)
library(dplyr)
library(tibble)
library(readr)

# 各関数の読み込み
source("./R/preprocess.R")

# 各データファイルのパス
# シミュレーション結果のデータ
data_simulation <- "./data/simulation_scores.csv"
# 旧指標群データ
data_old_features <- "./data/old_features.csv"
# 新指標群データ
data_new_features <- "./data/new_features_d_road_and_building.csv"

# 分析結果の出力ファイルパス

# シミュレーション結果データの整形
results_list <- preprocess(data_simulation)

# 各エージェントのデータを取り出す
# 分散救助戦略
(distributed <- results_list$distributed)
# 集中救助戦略
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

# 標準化をおこなう関数を定義
standardize_cols <- function(x) {
  as.numeric(scale(x))
}

# 作成した関数を用いて，各データフレームを標準化
building_features <- building_features |> 
  mutate(across(where(is.numeric), standardize_cols))

road_features <- road_features |> 
  mutate(across(where(is.numeric), standardize_cols))

all_features <- all_features |> 
  mutate(across(where(is.numeric), standardize_cols))

# 特徴指標データの保存先ディレクトリを作成
output_dir <- "./out/feature_sets"
dir.create(output_dir, showWarnings = FALSE)

# 各特徴指標データをCSVファイルとして保存
# 旧指標群
write_csv(
  building_features,
  file.path(output_dir, "features_old.csv")
)
# 新指標群
write_csv(
  road_features,
  file.path(output_dir, "features_new.csv")
)
# 総合指標群
write_csv(
  all_features,
  file.path(output_dir, "features_all.csv")
)


# 説明変数データをまとめたリストを作成
# 各関数を各説明変数データごとにループして実行するため
feature_sets <- list(
  "Old_Features" = building_features,
  "New_Features" = road_features,
  "All_Features" = all_features
)

# 目的変数のデータをまとめたリストを作成
target_datasets <- list(
  "Distributed" = distributed,
  "Concentration" = concentration
)

# 分析結果を格納するためのからのデータフレームを作成
all_results <- data.frame()

# 分析結果を保存するディレクトリのパスを設定
output_base_dir <- "./out"

# 分析の実行
# エージェントごとにループ
for (target_set_name in names(target_datasets)) {
  
  # 現在分析中のエージェント名を表示
  print(paste("Processing Agent:", target_set_name, "..."))
  # 分析対象のエージェントのデータフレームを取得
  current_target_df <- target_datasets[[target_set_name]]
  
  # 数値列のみ抽出
  numeric_cols <- current_target_df |> 
    dplyr::select(where(is.numeric)) |> 
    colnames()
  
  # 目的変数ごとにループ
  for (target_col in target_columns) {
    
    # 説明変数ごとにループ
    for (feat_set_name in names(feature_sets)) {
      
      current_feature_df <- feature_sets[[feat_set_name]]
      
      # Lasso回帰
      res <- run_lasso_analysis(
        target_df = current_target_df, 
        feature_df = current_feature_df, 
        target_col_name = target_col, 
        feature_set_name = feat_set_name
      )
      
      # 結果が存在する場合のみ保存処理
      if (!is.null(res)) {
        
        # エージェント名の列を追加
        res$AgentType <- target_set_name
        
        # 分析結果を保存するディレクトリを指定
        # 構造: ./out/{Agent}/{Target}/{FeatureSet}/result.csv
        save_dir_lasso <- file.path(output_base_dir, "LASSO", target_set_name, target_col, feat_set_name)
        
        # ディレクトリを作成
        if (!dir.exists(save_dir_lasso)) {
          dir.create(save_dir_lasso, recursive = TRUE)
        }
        
        # ファイルパスの作成
        file_path <- file.path(save_dir_lasso, "result.csv")
        
        # 係数の絶対値順に並べ替え
        res_sorted <- res |> arrange(desc(abs(Coefficient)))
        
        # CSVで保存
        write_csv(res_sorted, file_path)
        
        # SHAP分析
        if (feat_set_name == "All_Features") {
          # データの結合と整形
          data_merged <- inner_join(
            current_target_df |> dplyr::select(Map, all_of(target_col)),
            current_feature_df,
            by = "Map"
          ) |> na.omit()
          
          # SHAP分析用のデータフレームを作成（Map列と目的変数の列を削除）
          feature_data_for_shap <- data_merged |> dplyr::select(-Map, -all_of(target_col))
          
          # モデル学習用データの作成
          x_matrix <- as.matrix(feature_data_for_shap)
          y_vec <- as.numeric(data_merged[[target_col]])

          save_dir_shap <- file.path(output_base_dir, "LASSO", target_set_name, target_col, feat_set_name)
          
          # 上記でやったLasso回帰と同じモデルを作成するため，シード値を指定してLasso回帰モデルを作成
          set.seed(123)
          cv_fit_for_shap <- cv.glmnet(x_matrix, y_vec, alpha = 1)
          
          # SHAP結果の保存先: ./out/{Agent}/{Target}/{FeatureSet}/SHAP/
          shap_out_dir <- file.path(save_dir_shap, "SHAP")
          
          # SHAP分析の実行
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