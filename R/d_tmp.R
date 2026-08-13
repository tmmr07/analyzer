# ==============================================================================
# 最終決定版: plot描画方式（確認した画像と同じ仕組み）でD値を算出するコード
# ==============================================================================

library(rrstools)
library(sf)
library(stars)
library(dplyr)
library(readr)
library(purrr)
library(stringr)
# 画像処理用パッケージ
if (!require("png")) install.packages("png")
library(png)

# ---- 1. 関数定義: 描画プロットからD値を計算 ----
calculate_d_value_via_plot <- function(map, resolution = 4096) {
  
  roads <- get_roads(map)
  if (nrow(roads) == 0) return(NA)
  
  # --- Step A: 確認用画像と同じ方法で「描画」する ---
  temp_png <- tempfile(fileext = ".png")
  
  # 正方形の枠を定義
  bbox <- st_bbox(roads)
  max_dim <- max(bbox["xmax"] - bbox["xmin"], bbox["ymax"] - bbox["ymin"])
  center_x <- (bbox["xmin"] + bbox["xmax"]) / 2
  center_y <- (bbox["ymin"] + bbox["ymax"]) / 2
  
  xlim_sq <- c(center_x - max_dim / 2, center_x + max_dim / 2)
  ylim_sq <- c(center_y - max_dim / 2, center_y + max_dim / 2)
  
  # PNGデバイス起動 (アンチエイリアスなし、背景白)
  png(temp_png, width = resolution, height = resolution, antialias = "none")
  par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i", bg = "white")
  
  # 空の枠を作ってから道路を描画 (太さ lwd=2 も同じにする)
  plot(0, 0, type = "n", xlim = xlim_sq, ylim = ylim_sq, axes = FALSE, ann = FALSE)
  plot(st_geometry(roads), col = "black", lwd = 1, add = TRUE)
  
  dev.off() 
  
  # --- Step B: 保存した画像を読み込んで行列にする ---
  img <- readPNG(temp_png)
  
  # カラー画像(RGB)として読み込まれた場合、レイヤーを統合
  if (length(dim(img)) == 3) {
    img <- img[,,1] 
  }
  
  file.remove(temp_png) # 一時ファイル削除
  
  # --- Step C: ボックスカウンティング法 ---
  # 黒いピクセル (値が0.5未満) を探す
  black_pixels <- which(img < 0.5, arr.ind = TRUE)
  
  if (length(black_pixels) == 0) return(NA)
  
  # ボックスサイズを変えながらカウント
  max_power <- floor(log2(min(dim(img)[1:2]) / 2))
  box_sizes <- 2^(2:max_power) 
  
  box_counts <- sapply(box_sizes, function(s) {
    box_coords <- floor(black_pixels / s)
    return(nrow(unique(box_coords)))
  })
  
  if (length(box_counts) < 2) return(NA)
  
  # 回帰分析
  df <- data.frame(log_s = log(box_sizes), log_N = log(box_counts))
  fit <- lm(log_N ~ log_s, data = df)
  d_value <- -coef(fit)[2]
  
  return(d_value)
}

# ---- 2. 実行設定 ----

gml_directory_path <- "./data/maps" 
d_value_output_csv <- "./out/d_values_final.csv"

# ---- 3. 順次計算処理 ----

gml_files <- list.files(gml_directory_path, pattern = "\\.gml$", full.names = TRUE)
message(paste0("処理対象ファイル数: ", length(gml_files), "件"))
message("計算を開始します（描画方式・解像度4096）...")

current_count <- 0
total_files <- length(gml_files)

final_d_value_df <- map_dfr(gml_files, function(file_path) {
  
  current_count <<- current_count + 1
  file_name <- basename(file_path)
  map_name_clean <- str_remove(file_name, "\\.gml$")
  
  message(paste0("[", current_count, "/", total_files, "] Processing: ", file_name))
  
  d_val <- NA
  tryCatch({
    map_obj <- read_rrs_map(file_path)
    d_val <- calculate_d_value_via_plot(map_obj, resolution = 4096)
  }, error = function(e) {
    message(paste("Error:", e$message))
  })
  
  gc() # メモリ掃除
  return(data.frame(Map = map_name_clean, d_value = d_val))
})

# ---- 4. 結果保存 ----
final_d_value_df <- final_d_value_df %>% arrange(Map)
write_csv(final_d_value_df, d_value_output_csv)

message("完了しました！結果を確認してください: ", d_value_output_csv)
print(head(final_d_value_df))

