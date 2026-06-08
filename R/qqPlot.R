qqPlot <- function(save_dir, file_prefix, diff_sq_err, target_name) {
	# Q-Qプロットの保存先ファイル名
	png_filename <- file.path(save_dir, paste0("qq_diff_", file_prefix, ".png"))
  
	png(png_filename, width = 600, height = 600)
  
	# Q-Qプロットの実行
	qqnorm(diff_sq_err, main = paste("Q-Q Plot of Diff:", target_name)); 
	qqline(diff_sq_err, col = "red", lwd = 2)
	
	# Q-Qプロットの保存
	dev.off() 
}