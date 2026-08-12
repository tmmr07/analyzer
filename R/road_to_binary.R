library(rrstools)
library(sf)
library(stars)
library(dplyr)

#' RRSマップからD値分析用の二値画像を作成する関数
#'
#' @param map read_rrs_map()で読み込んだrrs_mapオブジェクト
#' @param output_path 保存するPNG画像のファイルパス
#' @param resolution 画像の解像度（デフォルト4096）
#' @return なし
create_binary_map_image <- function(map, output_path, resolution = 4096) {
  
  # 道路データを抽出
  roads <- get_roads(map)
  if (nrow(roads) == 0) {
    message("道路データがありません: ", output_path)
    return()
  }
  
  # 地図の範囲を取得し、正方形の枠（canvas）を定義する
  bbox <- st_bbox(roads)
  x_range <- bbox["xmax"] - bbox["xmin"]
  y_range <- bbox["ymax"] - bbox["ymin"]
  max_dim <- max(x_range, y_range)
  
  center_x <- (bbox["xmin"] + bbox["xmax"]) / 2
  center_y <- (bbox["ymin"] + bbox["ymax"]) / 2
  
  # 正方形の範囲を定義
  xlim_square <- c(center_x - max_dim / 2, center_x + max_dim / 2)
  ylim_square <- c(center_y - max_dim / 2, center_y + max_dim / 2)
  
  tryCatch({
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    
    png(output_path, width = resolution, height = resolution, antialias = "none")
    
    par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i", bg = "white")
    
    plot(0, 0, type = "n", xlim = xlim_square, ylim = ylim_square, axes = FALSE, ann = FALSE)
    
    plot(st_geometry(roads), col = "black", lwd = 2, add = TRUE)
    
    dev.off()
    message("保存しました: ", output_path)
    
  }, error = function(e) {
    message("保存エラー: ", e$message)
    if (dev.cur() > 1) dev.off()
  })
}

gml_path <- "./data/maps_tmp/argeria07.gml" 
map <- read_rrs_map(gml_path)

output_file <- "./out/check_images/argeria07_binary.png"

create_binary_map_image(map, output_file, resolution = 4096)

