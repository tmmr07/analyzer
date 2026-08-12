library(fastshap)
library(shapviz)
library(ggplot2)
library(dplyr)
library(readr)
library(glmnet)

analyze_shap_lasso <- function(lasso_model,       
                               feature_data,      
                               output_path,       
                               target_name,       
                               title_prefix = "", 
                               max_display = 15,
                               font_size = 20) {
  
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }
  
  feature_label_map <- c(
    c1  = "グロス建蔽率",
    c2  = "セミグロス建蔽率",
    c3  = "グロス容積率",
    c4  = "セミグロス容積率",
    c5  = "建物周長",
    c6  = "建物階数",
    c7  = "従属度",
    c8  = "最大隣接建物距離",
    c9  = "最小隣接建物距離",
    c10 = "配置配分比",
    c11 = "接道距離",
    c12 = "隙間率",
    c13 = "沿道建物数",
    c14 = "道路総延長",
    c15 = "道路密度",
    c16 = "棟数密度",
    
    degree_sd              = "次数の標準偏差",
    centralization_degree  = "次数集中度",
    bridges_ratio          = "橋の割合",
    assortativity          = "同類結合性",
    moran_i_degree         = "モランのI統計量",
    getis_gi_degree_mean   = "Getis-Ord Gi*統計量",
    d_value                = "D値",
    rosval_mean            = "平均探索情報量",
    orientation_entropy    = "配向秩序指標"
  )
  
  target_label_map <- c(
    final_alive        = "市民生存率",
    total_health       = "市民の総残存体力率",
    transport_rate     = "搬送率",
    buried_rescue_rate = "埋没市民救助率",
    road_clear_rate    = "道路啓開率",
    rsl21_rate         = "総合スコア維持率"
  )
  
  get_target_label <- function(target_name, label_map) {
    label <- label_map[target_name]
    
    if (is.na(label)) {
      return(target_name)
    } else {
      return(as.character(label))
    }
  }
  
  
  coefs <- coef(lasso_model, s = "lambda.min")

  coefs_mat <- as.matrix(coefs)
  active_vars <- rownames(coefs_mat)[coefs_mat[, 1] != 0]
  active_vars <- setdiff(active_vars, "(Intercept)")
  
  if (length(active_vars) == 0) {
    message(paste("Skipping SHAP plot for", target_name, ": No active features (all coefficients are 0)."))
    return(NULL)
  }
  
  pfun <- function(object, newdata) {
    as.numeric(predict(object, newx = as.matrix(newdata), s = "lambda.min"))
  }
  
  message("Calculating SHAP values for: ", target_name)
  
  shap_vals <- fastshap::explain(
    object       = lasso_model,
    X            = feature_data,
    pred_wrapper = pfun,
    nsim         = 100,
    adjust       = TRUE
  )
  
  shap_vals_filtered <- shap_vals[, active_vars, drop = FALSE]
  
  feature_data_filtered <- feature_data[, active_vars, drop = FALSE]
  
  txt_file <- file.path(output_path, "shap_results.txt")
  if (!file.exists(txt_file)) {
    write_lines(c(paste0("Analysis Run: ", Sys.time()), ""), txt_file)
  }
  
  imp_score <- colMeans(abs(shap_vals_filtered))
  top_5_names <- names(sort(imp_score, decreasing = TRUE))[1:5]
  top_5_names <- top_5_names[!is.na(top_5_names)]
  
  lines_to_write <- c(
    "----------------------------------------",
    paste0("Target: ", title_prefix, " : ", target_name),
    paste0("Active Features: ", length(active_vars), " (Filtered out zero-coef features)"),
    "Top 5 Features:",
    paste0("  ", 1:length(top_5_names), ". ", top_5_names),
    "" 
  )
  write_lines(lines_to_write, txt_file, append = TRUE)
  
  shap_df <- as.data.frame(shap_vals_filtered)
  write.csv(shap_df, file.path(output_path, paste0("shap_values_", title_prefix, "_", target_name, ".csv")), row.names = FALSE)
  
  jp_names <- feature_label_map[colnames(shap_vals_filtered)]
  jp_names[is.na(jp_names)] <- colnames(shap_vals_filtered)
  
  colnames(shap_vals_filtered) <- jp_names
  colnames(feature_data_filtered) <- jp_names
  
  sv <- shapviz(shap_vals_filtered, X = feature_data_filtered)
  
  color_low_custom  <- "#008AFB"
  color_high_custom <- "#FF0051"
  
  target_label_jp <- get_target_label(target_name, target_label_map)
  
  target_font <- "Hiragino Sans" 
  
  g <- sv_importance(sv, kind = "bee", max_display = max_display) +
    scale_colour_gradient(low = color_low_custom, high = color_high_custom) +
    labs(
      x = "特徴重要度",
      y = "特徴指標"
    ) +
    theme_bw(base_size = font_size, base_family = target_font) + 
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "none",
      text = element_text(family = target_font),
      axis.text = element_text(family = target_font)
    )
  
  
  ggsave(
    file.path(output_path, paste0("shap_", title_prefix, "_", target_name, ".pdf")), 
    g, 
    width = 10, 
    height = 8, 
    device = cairo_pdf
  )
  
  message("Saved SHAP plot (Active features only) for: ", target_name)
}