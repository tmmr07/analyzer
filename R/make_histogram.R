library(ggplot2)

#' ヒストグラムを作成する関数
#' @param save_dir 結果の保存先ディレクトリ
#' @param file_prefix ファイル名の共通プレフィックス
#' @param diff_sq_err 予測誤差の差のベクトル
make_hist <- function(save_dir, file_prefix, diff_sq_err) {
	# 予測誤差の差をデータフレームに変換
	df_plot <- data.frame(Diff = diff_sq_err)

	# ヒストグラムの作成
	g_hist <- ggplot(df_plot, aes(x = Diff)) +
    	geom_histogram(fill = "steelblue", color = "white", alpha = 0.7, bins = 20) +
    	geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
    	labs(
      		title = paste0(agent_name, " ： ", target_name),
      		x = "予測誤差の差（旧の誤差 - 総合の誤差）", 
      		y = "度数"
    	) +
    	theme_bw(base_size = 14)

	# ヒストグラムの保存
	ggsave(file.path(save_dir, paste0("hist_diff_", file_prefix, ".png")), g_hist, width = 8, height = 6)
}