library(dplyr)
library(kableExtra)

here::i_am("04_average_results.R")

gs_results <- read.csv("results_csv/gold_standard_n_6692.csv")
host_results <- read.csv("results_csv/host_n_6692.csv")

gs_day3diar <- read.csv("results_csv/gold_standard_day3diar_n_6692.csv")
host_day3diar <- read.csv("results_csv/host_day3diar_n_6692.csv")

# Function to make a combined table
make_combined_table <- function(results, rule = "Gold Standard") {
  
  combined_results <- data.frame()
  
  for(t in unique(results$threshold)){
    threshold_results <- results[results$threshold == t,]
    
    average_results <- data.frame(
      threshold = t,  # Add threshold column
      EY_Ad_dZ1 = mean(threshold_results$aiptw_EY_Ad_dZ1),
      se_EY_Ad_dZ1 = sqrt(mean((threshold_results$se_aiptw_EY_Ad_dZ1^2))),
      EY_A0_dZ1 = mean(threshold_results$aiptw_EY_A0_dZ1),
      se_EY_A0_dZ1 = sqrt(mean((threshold_results$se_aiptw_EY_A0_dZ1^2))),
      E_dZ1 = mean(threshold_results$E_dZ1),
      se_E_dZ1 = sqrt(mean((threshold_results$se_E_dZ1^2))),
      subgroup_effect = mean(threshold_results$subgroup_effect),
      se_subgroup_effect = sqrt(mean((threshold_results$se_subgroup_effect^2))),
      subgroup_effect_dZ0 = mean(threshold_results$subgroup_effect_dZ0),
      se_subgroup_effect_dZ0 = sqrt(mean((threshold_results$se_subgroup_effect_dZ0^2))),
      subgroup_difference = mean(threshold_results$compare_subgroup_effect),
      se_subgroup_difference = sqrt(mean((threshold_results$se_compare_subgroup_effect^2))),
      treatment_effect = mean(threshold_results$treatment_effect),
      se_treatment_effect = sqrt(mean((threshold_results$se_treatment_effect^2)))
    )
    
    ci_df <- data.frame(
      threshold = t,  # Add threshold column for CI dataframe
      lower_EY_Ad_dZ1 = average_results$EY_Ad_dZ1 - 1.96*average_results$se_EY_Ad_dZ1,
      upper_EY_Ad_dZ1 = average_results$EY_Ad_dZ1 + 1.96*average_results$se_EY_Ad_dZ1,
      lower_EY_A0_dZ1 = average_results$EY_A0_dZ1 - 1.96*average_results$se_EY_A0_dZ1,
      upper_EY_A0_dZ1 = average_results$EY_A0_dZ1 + 1.96*average_results$se_EY_A0_dZ1,
      lower_EY_E_dZ1 = average_results$E_dZ1 - 1.96*average_results$se_E_dZ1,
      upper_EY_E_dZ1 = average_results$E_dZ1 + 1.96*average_results$se_E_dZ1,
      lower_subgroup_effect = average_results$subgroup_effect - 1.96*average_results$se_subgroup_effect,
      upper_subgroup_effect = average_results$subgroup_effect + 1.96*average_results$se_subgroup_effect,
      lower_subgroup_effect_dZ0 = average_results$subgroup_effect_dZ0 - 1.96*average_results$se_subgroup_effect_dZ0,
      upper_subgroup_effect_dZ0 = average_results$subgroup_effect_dZ0 + 1.96*average_results$se_subgroup_effect_dZ0,
      lower_subgroup_difference = average_results$subgroup_difference - 1.96*average_results$se_subgroup_difference,
      upper_subgroup_difference = average_results$subgroup_difference + 1.96*average_results$se_subgroup_difference,
      lower_treatment_effect = average_results$treatment_effect - 1.96*average_results$se_treatment_effect,
      upper_treatment_effect = average_results$treatment_effect + 1.96*average_results$se_treatment_effect
    )
    
    results_table <- data.frame(
      estimate = c("$E[Y(1) | d(Z) = 1]$", 
                   "$E[Y(0) | d(Z) = 1]$", 
                   "$E[d(Z) = 1]$", 
                   "$E[Y(1) - Y(0) | d(Z) = 1]$", 
                   "$E[Y(1) - Y(0) | d(Z) = 0]$",
                   "$E[Y(1) - Y(0) | d(Z) = 1] - E[Y(1) - Y(0) | d(Z) = 0]$",
                   "$E[Y(d) - Y(0)]$"),
      threshold = t,  
      value = c(average_results$EY_Ad_dZ1,
                average_results$EY_A0_dZ1,
                average_results$E_dZ1,
                average_results$subgroup_effect,
                average_results$subgroup_effect_dZ0,
                average_results$subgroup_difference,
                average_results$treatment_effect),
      se = c(average_results$se_EY_Ad_dZ1,
             average_results$se_EY_A0_dZ1,
             average_results$se_E_dZ1,
             average_results$se_subgroup_effect,
             average_results$se_subgroup_effect_dZ0,
             average_results$se_subgroup_difference,
             average_results$se_treatment_effect),
      lower_ci = c(ci_df$lower_EY_Ad_dZ1,
                   ci_df$lower_EY_A0_dZ1,
                   ci_df$lower_EY_E_dZ1,
                   ci_df$lower_subgroup_effect,
                   ci_df$lower_subgroup_effect_dZ0,
                   ci_df$lower_subgroup_difference,
                   ci_df$lower_treatment_effect),
      upper_ci = c(ci_df$upper_EY_Ad_dZ1,
                   ci_df$upper_EY_A0_dZ1,
                   ci_df$upper_EY_E_dZ1,
                   ci_df$upper_subgroup_effect,
                   ci_df$upper_subgroup_effect_dZ0,
                   ci_df$upper_subgroup_difference,
                   ci_df$upper_treatment_effect)
    )
    
    combined_results <- rbind(combined_results, results_table)
  }
  
  custom_column_name <- c("Effect Estimate", "Threshold", "Value", "Standard Error", "Lower 95% CI Bound", "Upper 95% CI Bound")
  colnames(combined_results) <- custom_column_name
  
  combined_results <- combined_results %>%
    arrange(`Effect Estimate`, `Threshold`)
  
  # Create a single table with kable
  table <- kbl(combined_results, caption = paste0("Real data analysis for ", rule, " rule across 10 seeds and different thresholds"), booktabs=T, digits=3) %>%
    kable_styling(latex_options = "striped") %>%
    column_spec(1, bold=T) %>%
    collapse_rows(columns = 1, latex_hline = "major", row_group_label_position = "identity")
  
  print(table)
  
}

make_combined_table(gs_results, rule = "Gold Standard")
make_combined_table(host_results, rule = "Host")

make_combined_table(gs_day3diar, rule = "Gold Standard")
make_combined_table(host_day3diar, rule = "Host")
