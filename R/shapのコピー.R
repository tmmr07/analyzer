library(fastshap)
library(shapviz)
library(ggplot2)
library(dplyr)
library(readr)

#--------------------------------------------------
# 重回帰（lm）モデル用 SHAP 分析関数
#--------------------------------------------------
analyze_shap_lm <- function(lm_models,
                            feature_data,
                            output_path,
                            title_prefix = "",
                            max_display = 15,
                            font_size = 22) {
  
  # 保存先作成
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }
  
  # テキストファイルのパス
  txt_file <- file.path(output_path, "shap_results.txt")
  
  # ファイルの初期化（実行日時を書き込み、古い内容は上書きリセット）
  init_lines <- c(
    paste0("Analysis Run: ", Sys.time()),
    "" # 空行
  )
  write_lines(init_lines, txt_file, append = TRUE)
  
  response_vars <- names(lm_models)
  
  # 色設定（青〜赤ピンク）
  color_low_custom  <- "#008AFB"
  color_high_custom <- "#FF0051"
  
  for (resp in response_vars) {
    
    # 1. lmモデル取得
    model_obj <- lm_models[[resp]]
    
    # 2. 予測関数（lm 用）
    pfun <- function(object, newdata) {
      as.numeric(predict(object, newdata = newdata))
    }
    
    # 3. SHAP値計算
    shap_vals <- fastshap::explain(
      object       = model_obj,
      X            = feature_data,
      pred_wrapper = pfun,
      nsim         = 100,
      adjust       = TRUE
    )
    
    #--------------------------------------------------
    # ★追加処理：上位5個の名前を write_lines で保存
    #--------------------------------------------------
    
    # 重要度（絶対値の平均）を計算して降順ソート
    imp_score <- colMeans(abs(shap_vals))
    top_5_names <- names(sort(imp_score, decreasing = TRUE))[1:5]
    
    # NA除去（変数が5個未満の場合の対策）
    top_5_names <- top_5_names[!is.na(top_5_names)]
    
    # 書き込む行データをベクトルとして作成
    lines_to_write <- c(
      "----------------------------------------",
      paste0("Target: ", title_prefix, " : ", resp),
      "Top 5 Features:",
      paste0("  ", 1:length(top_5_names), ". ", top_5_names),
      "" # 末尾に空行を入れて見やすくする
    )
    
    # 追記モード (append = TRUE) で書き込み
    write_lines(lines_to_write, txt_file, append = TRUE)
    
    message("Saved Top 5 names to shap_results.txt for: ", resp)
    
    #--------------------------------------------------
    
    # 4. SHAP値をCSV出力
    shap_df <- as.data.frame(shap_vals)
    shap_df$sample_id <- seq_len(nrow(shap_df))
    
    csv_filename <- paste0(
      output_path, "/shap_values_",
      title_prefix, "_", resp, ".csv"
    )
    write.csv(shap_df, csv_filename, row.names = FALSE)
    
    message("Saved SHAP values CSV for: ", resp)
    
    # 5. shapviz オブジェクト
    sv <- shapviz(shap_vals, X = feature_data)
    
    # 6. プロット作成
    g <- sv_importance(
      sv,
      kind = "bee",
      max_display = max_display
    ) +
      scale_colour_gradient(
        low  = color_low_custom,
        high = color_high_custom
      ) +
      labs(
        title = paste0(title_prefix, ": ", resp),
        x = "特徴重要度",
        y = "特徴指標"
      ) +
      scale_x_continuous(
        labels = function(x) format(x, scientific = FALSE)
      ) +
      theme_bw(base_size = font_size) +
      theme(
        plot.title   = element_text(face = "bold", hjust = 0.5),
        axis.title.y = element_text(angle = 90, vjust = 0.5),
        legend.position = "right"
      ) +
      guides(color = "none")
    
    # 7. 表示と保存
    print(g)
    
    fig_filename <- paste0(
      output_path, "/shap_",
      title_prefix, "_", resp, ".png"
    )
    ggsave(fig_filename, g, width = 10, height = 8, dpi = 300)
    
    message("Saved SHAP plot for: ", resp)
  }
}
