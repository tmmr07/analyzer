#' 旧指標群モデルと総合指標群モデルの部分F検定を行う関数
#'
#' @param old_models 旧指標群を用いた重回帰モデルのリスト
#' @param all_models 総合指標群を用いた重回帰モデルのリスト
#' @param agent_type エージェントの種類
#' @param output_path 結果を出力するファイルパス
#' @return 部分F検定結果のリスト

perform_partial_f_test <- function(old_models, all_models, agent_type, output_path) {
  
  if (agent_type == "distributed") {
    write_lines("----------------------分散救助エージェント：部分F検定----------------------",
                output_path, append = TRUE)
  } else if (agent_type == "concentration") {
    write_lines("----------------------集中救助エージェント：部分F検定----------------------",
                output_path, append = TRUE)
  }
  
  response_variable_names <- intersect(names(old_models), names(all_models))
  
  f_test_results <- purrr::map(response_variable_names, ~{
    response_var <- .x
    
    old_model <- old_models[[response_var]]
    all_model <- all_models[[response_var]]
    
    # 部分F検定（anovaはRSS差に基づくF検定）
    f_test <- anova(old_model, all_model)
    
    # 出力
    write_lines("", output_path, append = TRUE)
    write_lines(paste("---目的変数：", response_var, "---"),
                output_path, append = TRUE)
    write_lines(capture.output(f_test),
                output_path, append = TRUE)
    
    return(f_test)
  }) |> set_names(response_variable_names)
  
  if (agent_type == "distributed") {
    write_lines("----------------------分散救助エージェント：部分F検定終了----------------------",
                output_path, append = TRUE)
    write_lines("", output_path, append = TRUE)
  } else if (agent_type == "concentration") {
    write_lines("----------------------集中救助エージェント：部分F検定終了----------------------",
                output_path, append = TRUE)
    write_lines("", output_path, append = TRUE)
  }
  
  return(f_test_results)
}
