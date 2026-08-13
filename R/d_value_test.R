# ==============================================================================
# 修正版: 確実に道路を抽出するD値計算コード（画像確認機能付き）
# ==============================================================================

library(rrstools)
library(sf)
library(stars)
library(ggplot2)
library(stringr)
library(dplyr)
library(readr)
library(purrr)

# ---- 1. 関数定義 ----
analyze_road_d_value_in_memory <- function(map, resolution = 4096, debug_plot = FALSE, map_name = "test") {
  
  # 1. 道路データを抽出
  roads <- get_roads(map)
  if (nrow(roads) == 0) return(list(d_value = NA))
  
  # ★修正1: 道路データに「1」という値を明示的に持たせる
  roads <- roads %>% mutate(burn_val = 1)
  
  # 2. 地図の範囲を取得＆正方形化
  bbox <- st_bbox(roads)
  max_dim <- max(bbox["xmax"] - bbox["xmin"], bbox["ymax"] - bbox["ymin"])
  center_x <- (bbox["xmin"] + bbox["xmax"]) / 2
  center_y <- (bbox["ymin"] + bbox["ymax"]) / 2
  
  new_bbox <- st_bbox(c(
    xmin = center_x - max_dim / 2, xmax = center_x + max_dim / 2,
    ymin = center_y - max_dim / 2, ymax = center_y + max_dim / 2
  ), crs = st_crs(roads))
  
  # ★修正2: 背景を「0」で初期化したグリッドを作成
  grid <- stars::st_as_stars(new_bbox, nx = resolution, ny = resolution, values = 0)
  
  # ★修正3: 道路の「burn_val (=1)」列を使って焼き付ける
  # これで 道路=1, 背景=0 が確実になります
  img_stars <- stars::st_rasterize(roads["burn_val"], template = grid)
  img_matrix <- img_stars[[1]]
  
  # NAがある場合は背景(0)にする
  img_matrix[is.na(img_matrix)] <- 0
  
  # --- デバッグ用：最初の1枚だけ画像を保存して確認する ---
  if (debug_plot) {
    png(paste0(map_name, "_check.png"), width = 800, height = 800)
    image(img_matrix, main = paste("Check:", map_name), col = c("white", "black"))
    dev.off()
    message(paste("確認用画像を保存しました:", map_name, "_check.png"))
  }
  
  # 4. ボックスカウンティング法
  # 値が「1」のピクセル（道路）を探す
  road_pixels <- which(img_matrix == 1, arr.ind = TRUE)
  
  if (length(road_pixels) == 0) {
    message("警告: 道路ピクセルが見つかりませんでした。")
    return(list(d_value = NA))
  }
  
  max_power <- floor(log2(min(dim(img_matrix)[1:2]) / 2))
  # ★修正4: ボックスサイズの下限を少し上げる（ノイズ除去のため、最小4ピクセル程度から計算）
  box_sizes <- 2^(2:max_power) 
  
  box_counts <- sapply(box_sizes, function(s) {
    box_coords <- floor(road_pixels / s)
    return(nrow(unique(box_coords)))
  })
  
  if (length(box_counts) < 2) return(list(d_value = NA))
  
  df <- data.frame(log_s = log(box_sizes), log_N = log(box_counts))
  fit <- lm(log_N ~ log_s, data = df)
  d_value <- -coef(fit)[2]
  
  return(list(d_value = d_value))
}

# ---- 2. 実行設定 ----

gml_directory_path <- "./data/maps" 
d_value_output_csv <- "./out/d_values_list.csv"

# ---- 3. 順次計算処理 ----

gml_files <- list.files(gml_directory_path, pattern = "\\.gml$", full.names = TRUE)
message(paste0("処理対象ファイル数: ", length(gml_files), "件"))
message("順次処理を開始します（解像度: 4096）...")

current_count <- 0
total_files <- length(gml_files)

final_d_value_df <- map_dfr(gml_files, function(file_path) {
  
  current_count <<- current_count + 1
  file_name <- basename(file_path)
  map_name_clean <- str_remove(file_name, "\\.gml$")
  
  message(paste0("[", current_count, "/", total_files, "] Processing: ", file_name))
  
  result_df <- data.frame(Map = map_name_clean, d_value = NA)
  
  tryCatch({
    map_obj <- read_rrs_map(file_path)
    
    # ★最初の1ファイルだけ画像を保存して確認できるようにする (debug_plot = TRUE)
    is_first <- (current_count == 1)
    
    res <- analyze_road_d_value_in_memory(
      map_obj, 
      resolution = 4096, 
      debug_plot = is_first, 
      map_name = map_name_clean
    )
    
    result_df$d_value <- res$d_value
    
  }, error = function(e) {
    message(paste("Error processing", file_name, ":", e$message))
  })
  
  gc() 
  return(result_df)
})

# ---- 4. 出力 ----
final_d_value_df <- final_d_value_df %>% arrange(Map)
write_csv(final_d_value_df, d_value_output_csv)
message("完了")
print(head(final_d_value_df))