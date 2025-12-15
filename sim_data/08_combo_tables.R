here::i_am("08_combo_tables.R")

source("04_simulation_data_truth_comparison.R")
source("07_simulation_data_adaptive_truth_comparison.R")

library(tidyverse)

threshold_list <- c(0.05)

for(t in threshold_list){
  
  results_normal <- get_results_table(t = t, 
                                      truth_df_gs = truth_df_gs, 
                                      truth_df_host = truth_df_host, 
                                      gs_results_df = gs_results_df, 
                                      host_results_df = host_results_df)
  
  results_normal$type <- "Regular"
  
  results_data_adaptive <- get_data_adaptive_results_table(t = t,
                                                           gs_results_df = gs_results_df, 
                                                           host_results_df = host_results_df, 
                                                           gs_data_adaptive_truth_df = gs_data_adaptive_truth_df, 
                                                           host_data_adaptive_truth_df = host_data_adaptive_truth_df)
  
  results_data_adaptive$type <- "Data Adaptive"
  
  overall_results <- rbind(results_normal, results_data_adaptive)
  
  pivoted_results <- overall_results %>%
    pivot_wider(
      names_from = type, 
      values_from = c(`Gold Standard Rule`, `Host Rule`),
      names_glue = "{.value} {type}"
    )
  
  colnames(pivoted_results) <- c("Effect Estimate", "Performance Metric", "Regular", "Data Adaptive", "Regular ", "Data Adaptive ")
  
  table <- kbl(pivoted_results, format = 'html', caption = paste0("Simulation results for threshold = ", t, " (n = 6692, replicates = 1000)"), booktabs=T, digits=5) %>%
    kable_styling(latex_options = "striped") %>%
    column_spec(1, bold=T, background = NULL) %>%
    collapse_rows(columns = 1:2, latex_hline = "major", row_group_label_position = "first") %>%
    add_header_above(c(" ", " ", "Gold Standard Rule" = 2, "Host Rule" = 2))
  
  print(table)
    
}
