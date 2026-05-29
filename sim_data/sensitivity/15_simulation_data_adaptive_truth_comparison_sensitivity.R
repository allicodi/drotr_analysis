here::i_am("sensitivity/15_simulation_data_adaptive_truth_comparison_sensitivity.R")

library(dplyr)
library(kableExtra)

# missing_types <- c("mcar", "mar")
missing_types <- c("mcar")
threshold_list <- c(0.05)

get_data_adaptive_results_table <- function(t,
                                            gs_data_adaptive_truth_df,
                                            gs_results_df) {
  
  gs_truth_threshold_df <- gs_data_adaptive_truth_df[
    gs_data_adaptive_truth_df$threshold == t,
  ] %>%
    arrange(seed)
  
  colnames(gs_truth_threshold_df) <- ifelse(
    names(gs_truth_threshold_df) == "seed",
    "seed",
    paste0("true_", names(gs_truth_threshold_df))
  )
  
  gs_results_threshold_df <- gs_results_df[gs_results_df$threshold == t, ]
  
  gs_sim_and_truth <- left_join(
    gs_truth_threshold_df,
    gs_results_threshold_df,
    by = "seed"
  )
  
  # ----- Bias -----
  
  gs_bias_EY_A1_dZ1 <- mean(
    gs_sim_and_truth$aiptw_EY_Ad_dZ1 - gs_sim_and_truth$true_EY_A1_dZ1_gs,
    na.rm = TRUE
  )
  
  gs_bias_EY_A0_dZ1 <- mean(
    gs_sim_and_truth$aiptw_EY_A0_dZ1 - gs_sim_and_truth$true_EY_A0_dZ1_gs,
    na.rm = TRUE
  )
  
  gs_bias_E_dZ1 <- mean(
    gs_sim_and_truth$E_dZ1 - gs_sim_and_truth$true_E_dZ1_gs,
    na.rm = TRUE
  )
  
  gs_bias_subgroup_effect_treated <- mean(
    gs_sim_and_truth$subgroup_effect - gs_sim_and_truth$true_subgroup_effect_treated_gs,
    na.rm = TRUE
  )
  
  gs_bias_subgroup_effect_untreated <- mean(
    gs_sim_and_truth$subgroup_effect_dZ0 - gs_sim_and_truth$true_subgroup_effect_untreated_gs,
    na.rm = TRUE
  )
  
  gs_bias_treatment_effect <- mean(
    gs_sim_and_truth$treatment_effect - gs_sim_and_truth$true_treatment_effect_gs,
    na.rm = TRUE
  )
  
  gs_bias_subgroup_difference <- mean(
    gs_sim_and_truth$compare_subgroup_effect - gs_sim_and_truth$true_subgroup_difference_gs,
    na.rm = TRUE
  )
  
  # ----- CI Coverage -----
  
  gs_lower_ci_EY_A1_dZ1 <- gs_sim_and_truth$aiptw_EY_Ad_dZ1 -
    1.96 * gs_sim_and_truth$se_aiptw_EY_Ad_dZ1
  gs_upper_ci_EY_A1_dZ1 <- gs_sim_and_truth$aiptw_EY_Ad_dZ1 +
    1.96 * gs_sim_and_truth$se_aiptw_EY_Ad_dZ1
  
  gs_coverage_EY_A1_dZ1 <- mean(
    ifelse(
      gs_sim_and_truth$true_EY_A1_dZ1_gs > gs_lower_ci_EY_A1_dZ1 &
        gs_sim_and_truth$true_EY_A1_dZ1_gs < gs_upper_ci_EY_A1_dZ1,
      1, 0
    ),
    na.rm = TRUE
  )
  
  gs_lower_ci_EY_A0_dZ1 <- gs_sim_and_truth$aiptw_EY_A0_dZ1 -
    1.96 * gs_sim_and_truth$se_aiptw_EY_A0_dZ1
  gs_upper_ci_EY_A0_dZ1 <- gs_sim_and_truth$aiptw_EY_A0_dZ1 +
    1.96 * gs_sim_and_truth$se_aiptw_EY_A0_dZ1
  
  gs_coverage_EY_A0_dZ1 <- mean(
    ifelse(
      gs_sim_and_truth$true_EY_A0_dZ1_gs > gs_lower_ci_EY_A0_dZ1 &
        gs_sim_and_truth$true_EY_A0_dZ1_gs < gs_upper_ci_EY_A0_dZ1,
      1, 0
    ),
    na.rm = TRUE
  )
  
  gs_lower_ci_E_dZ1 <- gs_sim_and_truth$E_dZ1 -
    1.96 * gs_sim_and_truth$se_E_dZ1
  gs_upper_ci_E_dZ1 <- gs_sim_and_truth$E_dZ1 +
    1.96 * gs_sim_and_truth$se_E_dZ1
  
  gs_coverage_E_dZ1 <- mean(
    ifelse(
      gs_sim_and_truth$true_E_dZ1_gs > gs_lower_ci_E_dZ1 &
        gs_sim_and_truth$true_E_dZ1_gs < gs_upper_ci_E_dZ1,
      1, 0
    ),
    na.rm = TRUE
  )
  
  gs_lower_ci_subgroup_effect_treated <- gs_sim_and_truth$subgroup_effect -
    1.96 * gs_sim_and_truth$se_subgroup_effect
  gs_upper_ci_subgroup_effect_treated <- gs_sim_and_truth$subgroup_effect +
    1.96 * gs_sim_and_truth$se_subgroup_effect
  
  gs_coverage_subgroup_effect_treated <- mean(
    ifelse(
      gs_sim_and_truth$true_subgroup_effect_treated_gs > gs_lower_ci_subgroup_effect_treated &
        gs_sim_and_truth$true_subgroup_effect_treated_gs < gs_upper_ci_subgroup_effect_treated,
      1, 0
    ),
    na.rm = TRUE
  )
  
  gs_lower_ci_subgroup_effect_untreated <- gs_sim_and_truth$subgroup_effect_dZ0 -
    1.96 * gs_sim_and_truth$se_subgroup_effect_dZ0
  gs_upper_ci_subgroup_effect_untreated <- gs_sim_and_truth$subgroup_effect_dZ0 +
    1.96 * gs_sim_and_truth$se_subgroup_effect_dZ0
  
  gs_coverage_subgroup_effect_untreated <- mean(
    ifelse(
      gs_sim_and_truth$true_subgroup_effect_untreated_gs > gs_lower_ci_subgroup_effect_untreated &
        gs_sim_and_truth$true_subgroup_effect_untreated_gs < gs_upper_ci_subgroup_effect_untreated,
      1, 0
    ),
    na.rm = TRUE
  )
  
  gs_lower_ci_treatment_effect <- gs_sim_and_truth$treatment_effect -
    1.96 * gs_sim_and_truth$se_treatment_effect
  gs_upper_ci_treatment_effect <- gs_sim_and_truth$treatment_effect +
    1.96 * gs_sim_and_truth$se_treatment_effect
  
  gs_coverage_treatment_effect <- mean(
    ifelse(
      gs_sim_and_truth$true_treatment_effect_gs > gs_lower_ci_treatment_effect &
        gs_sim_and_truth$true_treatment_effect_gs < gs_upper_ci_treatment_effect,
      1, 0
    ),
    na.rm = TRUE
  )
  
  gs_lower_ci_subgroup_difference <- gs_sim_and_truth$compare_subgroup_effect -
    1.96 * gs_sim_and_truth$se_compare_subgroup_effect
  gs_upper_ci_subgroup_difference <- gs_sim_and_truth$compare_subgroup_effect +
    1.96 * gs_sim_and_truth$se_compare_subgroup_effect
  
  gs_coverage_subgroup_difference <- mean(
    ifelse(
      gs_sim_and_truth$true_subgroup_difference_gs > gs_lower_ci_subgroup_difference &
        gs_sim_and_truth$true_subgroup_difference_gs < gs_upper_ci_subgroup_difference,
      1, 0
    ),
    na.rm = TRUE
  )
  
  results_table <- data.frame(
    effect_est = c(
      rep("$E[Y(1) | d(Z) = 1]$", 2),
      rep("$E[Y(0) | d(Z) = 1]$", 2),
      rep("$E[d(Z) = 1]$", 2),
      rep("$E[Y(1) - Y(0) | d(Z) = 1]$", 2),
      rep("$E[Y(1) - Y(0) | d(Z) = 0]$", 2),
      rep("$E[Y(d) - Y(0)]$", 2),
      rep("$E[Y(1) - Y(0) | d(Z) = 1] - E[Y(1) - Y(0) | d(Z) = 0]$", 2)
    ),
    measure = rep(c("Bias", "95% CI Coverage"), 7),
    gold_standard = c(
      gs_bias_EY_A1_dZ1,
      gs_coverage_EY_A1_dZ1,
      gs_bias_EY_A0_dZ1,
      gs_coverage_EY_A0_dZ1,
      gs_bias_E_dZ1,
      gs_coverage_E_dZ1,
      gs_bias_subgroup_effect_treated,
      gs_coverage_subgroup_effect_treated,
      gs_bias_subgroup_effect_untreated,
      gs_coverage_subgroup_effect_untreated,
      gs_bias_treatment_effect,
      gs_coverage_treatment_effect,
      gs_bias_subgroup_difference,
      gs_coverage_subgroup_difference
    )
  )
  
  colnames(results_table) <- c(
    "Effect Estimate",
    "Performance Metric",
    "Gold Standard Rule"
  )
  
  return(results_table)
}

for (miss_type in missing_types) {
  
  gs_data_adaptive_truth_df <- read.csv(
    paste0("truth/", miss_type, "_gs_data_adaptive_truth_by_seed.csv")
  )
  
  gs_results_df <- read.csv(
    paste0("results_csv/gold_standard_", miss_type, "_n_6692.csv")
  )
  
  for (t in threshold_list) {
    
    results_table <- get_data_adaptive_results_table(
      t = t,
      gs_data_adaptive_truth_df = gs_data_adaptive_truth_df,
      gs_results_df = gs_results_df
    )
    
    table <- kbl(
      results_table,
      format = "html",
      caption = paste0(
        "Data adaptive simulation results for threshold = ", t,
        " (", toupper(miss_type), ", gold standard rule, n = 6692, replicates = 1000)"
      ),
      booktabs = TRUE,
      digits = 5
    ) %>%
      kable_styling(latex_options = "striped") %>%
      column_spec(1, bold = TRUE) %>%
      collapse_rows(
        columns = 1:2,
        latex_hline = "major",
        row_group_label_position = "first"
      )
    
    print(table)
  }
}
