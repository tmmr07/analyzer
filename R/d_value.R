# 必要なパッケージの読み込み
library(rrstools)
library(sf)
library(stars)
library(ggplot2)
library(stringr)

# D値を計算する関数
# ---- 修正版関数: 自動で正方形にパディングしてD値を計算 ----
analyze_road_d_value_in_memory <- function(map, resolution = 2048) {
  
  # 1. 道路データを抽出
  roads <- get_roads(map)
  if (nrow(roads) == 0) stop("地図データ内に道路が見つかりませんでした。")
  
  # 2. バウンディングボックス（地図の範囲）を取得
  bbox <- st_bbox(roads)
  x_range <- bbox["xmax"] - bbox["xmin"]
  y_range <- bbox["ymax"] - bbox["ymin"]
  
  # 3. 長辺に合わせて正方形の範囲（extent）を定義
  max_dim <- max(x_range, y_range)
  
  # 中心を維持したまま、正方形のグリッド範囲を作成
  center_x <- (bbox["xmin"] + bbox["xmax"]) / 2
  center_y <- (bbox["ymin"] + bbox["ymax"]) / 2
  
  new_bbox <- st_bbox(c(
    xmin = center_x - max_dim / 2,
    xmax = center_x + max_dim / 2,
    ymin = center_y - max_dim / 2,
    ymax = center_y + max_dim / 2
  ), crs = st_crs(roads))
  
  # 4. 正方形のグリッドを作成 (縦横のピクセル数を同じにする)
  # これでピクセル自体も正方形になり、歪みがなくなります
  grid <- stars::st_as_stars(new_bbox, nx = resolution, ny = resolution, values = 1)
  
  # 5. ラスタライズ（道路を焼き付ける）
  img_matrix <- stars::st_rasterize(roads, template = grid)[[1]]
  img_matrix[is.na(img_matrix)] <- 1 # 余白や背景は1(白)
  img_matrix[img_matrix == 1] <- 0   # 道路部分を0(黒)に変換
  
  # --- 以降はボックスカウンティング計算（変更なし） ---
  black_pixels <- which(img_matrix == 0, arr.ind = TRUE)
  
  # 黒ピクセルが一つもない場合の回避
  if (length(black_pixels) == 0) return(list(d_value = NA, plot = NULL))
  
  max_power <- floor(log2(min(dim(img_matrix)[1:2]) / 2))
  box_sizes <- 2^(1:max_power)
  
  box_counts <- sapply(box_sizes, function(s) {
    box_coords <- floor(black_pixels / s)
    return(nrow(unique(box_coords)))
  })
  
  # データ数が少なすぎて回帰できない場合の回避
  if (length(box_counts) < 2) return(list(d_value = NA, plot = NULL))
  
  df <- data.frame(log_s = log(box_sizes), log_N = log(box_counts))
  fit <- lm(log_N ~ log_s, data = df)
  d_value <- -coef(fit)[2]
  
  plot <- ggplot(df, aes(x = log_s, y = log_N)) +
    geom_point(color = "blue", size = 3) +
    geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
    labs(
      title = paste("D値 ≈", round(d_value, 3)),
      x = "log(s)", y = "log(N)"
    ) + theme_minimal()
  
  return(list(d_value = d_value, plot = plot))
}

# ---- 2. 実行設定 ----

# ★★★ ここを変更してください ★★★
# GMLファイルが格納されているディレクトリのパス
gml_directory_path <- "./data/maps" 

# 出力するCSVのパス
d_value_output_csv <- "./out/d_values_list.csv"


# ---- 3. 一括計算処理 ----

# 指定ディレクトリ内の .gml ファイル一覧を取得
gml_files <- list.files(gml_directory_path, pattern = "\\.gml$", full.names = TRUE)

message(paste0("D値計算対象ファイル数: ", length(gml_files), "件"))

# 結果を格納する空のデータフレームを作成
d_value_results <- data.frame(Map = character(), d_value = numeric(), stringsAsFactors = FALSE)

# 各ファイルに対して処理を実行
for (file_path in gml_files) {
  # ファイル名を取得 (例: "Kobe.gml")
  file_name <- basename(file_path)
  
  # 進捗表示
  message(paste("Processing:", file_name, "..."))
  
  # エラーで止まらないように tryCatch を使用
  tryCatch({
    # 地図データの読み込み
    map_obj <- read_rrs_map(file_path)
    
    # D値の計算 (解像度はデフォルト800を使用)
    res <- analyze_road_d_value_in_memory(map_obj)
    
    # Map名から拡張子 (.gml) を削除
    map_name_clean <- str_remove(file_name, "\\.gml$")
    
    # 結果を追加
    d_value_results <- bind_rows(d_value_results, 
                                 data.frame(Map = map_name_clean, d_value = res$d_value))
    
  }, error = function(e) {
    message(paste("Error processing", file_name, ":", e$message))
  })
}

# ---- 4. 結果の整形と出力 ----

# Map名の辞書順でソート
final_d_value_df <- d_value_results %>%
  arrange(Map)

# CSVに書き出し
write_csv(final_d_value_df, d_value_output_csv)

message("処理完了。結果を出力しました: ", d_value_output_csv)
print(head(final_d_value_df))