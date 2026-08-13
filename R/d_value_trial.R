# ==============================================================================
# 修正版: 高解像度 (16384px) 対応・順次処理（メモリ安全重視）
# ==============================================================================

# 必要なパッケージの読み込み
library(rrstools)
library(sf)
library(stars)
library(ggplot2)
library(stringr)
library(dplyr)
library(readr)
library(purrr) # map_dfr を使うために読み込み（並列処理ではありません）

# ---- 1. 関数定義: 道路網のフラクタル次元(D値)を計算 ----
# ※地図が長方形でも自動的に正方形に補正して計算します
analyze_road_d_value_in_memory <- function(map, resolution = 4096) {
  
  # 1. 道路データを抽出
  roads <- get_roads(map)
  
  # 道路データがない場合はNAを返す
  if (nrow(roads) == 0) return(list(d_value = NA))
  
  # 2. 地図の範囲（バウンディングボックス）を取得
  bbox <- st_bbox(roads)
  x_range <- bbox["xmax"] - bbox["xmin"]
  y_range <- bbox["ymax"] - bbox["ymin"]
  
  # 3. 長辺に合わせて正方形の範囲を作成（パディング処理）
  max_dim <- max(x_range, y_range)
  
  center_x <- (bbox["xmin"] + bbox["xmax"]) / 2
  center_y <- (bbox["ymin"] + bbox["ymax"]) / 2
  
  new_bbox <- st_bbox(c(
    xmin = center_x - max_dim / 2, xmax = center_x + max_dim / 2,
    ymin = center_y - max_dim / 2, ymax = center_y + max_dim / 2
  ), crs = st_crs(roads))
  
  # 4. 正方形のグリッドを作成
  grid <- stars::st_as_stars(new_bbox, nx = resolution, ny = resolution, values = 1)
  
  # 5. ラスタライズ（道路を焼き付ける）
  # メモリ消費が最も大きい箇所です
  img_matrix <- stars::st_rasterize(roads, template = grid)[[1]]
  img_matrix[is.na(img_matrix)] <- 1 # 背景(NA)を白(1)に
  img_matrix[img_matrix == 1] <- 0   # 道路(1)を黒(0)に
  
  # 6. ボックスカウンティング法
  black_pixels <- which(img_matrix == 0, arr.ind = TRUE)
  if (length(black_pixels) == 0) return(list(d_value = NA))
  
  max_power <- floor(log2(min(dim(img_matrix)[1:2]) / 2))
  box_sizes <- 2^(1:max_power)
  
  box_counts <- sapply(box_sizes, function(s) {
    box_coords <- floor(black_pixels / s)
    return(nrow(unique(box_coords)))
  })
  
  if (length(box_counts) < 2) return(list(d_value = NA))
  
  df <- data.frame(log_s = log(box_sizes), log_N = log(box_counts))
  fit <- lm(log_N ~ log_s, data = df)
  d_value <- -coef(fit)[2]
  
  return(list(d_value = d_value))
}

# ---- 2. 実行設定 ----

gml_directory_path <- "./data/maps_tmp" 
d_value_output_csv <- "./out/d_values_list.csv"

# ---- 3. 順次計算処理 ----

gml_files <- list.files(gml_directory_path, pattern = "\\.gml$", full.names = TRUE)
message(paste0("処理対象ファイル数: ", length(gml_files), "件"))
message("順次処理を開始します（解像度: 4096）...")

# カウンター用
current_count <- 0
total_files <- length(gml_files)

# map_dfr を使って1つずつ順番に実行
final_d_value_df <- map_dfr(gml_files, function(file_path) {
  
  # 進捗状況の表示
  current_count <<- current_count + 1
  file_name <- basename(file_path)
  message(paste0("[", current_count, "/", total_files, "] Processing: ", file_name))
  
  map_name_clean <- str_remove(file_name, "\\.gml$")
  
  # 結果を格納する変数
  result_df <- data.frame(Map = map_name_clean, d_value = NA)
  
  # エラーハンドリング
  tryCatch({
    map_obj <- read_rrs_map(file_path)
    
    res <- analyze_road_d_value_in_memory(map_obj, resolution = 4096)
    
    result_df$d_value <- res$d_value
    
  }, error = function(e) {
    message(paste("Error processing", file_name, ":", e$message))
  })
  
  # ★重要: メモリ解放 (Garbage Collection)
  # 1回のループが終わるごとに強制的に不要メモリを削除してSSDスワップを防ぐ
  gc() 
  
  return(result_df)
})

# ---- 4. 結果の整形と出力 ----

# Map名の辞書順でソート
final_d_value_df <- final_d_value_df %>%
  arrange(Map)

write_csv(final_d_value_df, d_value_output_csv)

message("処理完了。結果を出力しました: ", d_value_output_csv)
print(head(final_d_value_df))
