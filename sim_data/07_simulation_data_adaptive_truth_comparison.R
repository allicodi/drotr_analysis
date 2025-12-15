here::i_am("07_simulation_data_adaptive_truth_comparison.R")

library(dplyr)
library(kableExtra)

# read in data adaptive truth by seed file
gs_data_adaptive_truth_df <- read.csv("truth/gs_data_adaptive_truth_by_seed.csv")
host_data_adaptive_truth_df <- read.csv("truth/host_data_adaptive_truth_by_seed.csv")

# read simulation results
gs_results_df <- read.csv("results_csv/gold_standard_n_6692.csv")
host_results_df <- read.csv("results_csv/host_n_6692.csv")

# list of same thresholds used for cate models
threshold_list <- c(0.05)

# function to make table for a given threshold
get_data_adaptive_results_table <- function(t, gs_data_adaptive_truth_df, host_data_adaptive_truth_df, gs_results_df, host_results_df){
  
  # get data adaptive truths for given threshold t
  gs_truth_threshold_df <- gs_data_adaptive_truth_df[gs_data_adaptive_truth_df$threshold == t,] %>%
    arrange(seed)
  
  # add prefix true to differentiate later on when adding in results
  colnames(gs_truth_threshold_df) <- ifelse(names(gs_truth_threshold_df) == "seed", "seed", paste0("true_", names(gs_truth_threshold_df)))
  
  host_truth_threshold_df <- host_data_adaptive_truth_df[host_data_adaptive_truth_df$threshold == t,] %>%
    arrange(seed)
  colnames(host_truth_threshold_df) <- ifelse(names(host_truth_threshold_df) == "seed", "seed", paste0("true_", names(host_truth_threshold_df)))
  
  # get results for threshold t
  gs_results_threshold_df <- gs_results_df[gs_results_df$threshold == t, ]
  
  host_results_threshold_df <- host_results_df[host_results_df$threshold == t, ]
  
  # join data adaptive truth with results by seed
  gs_sim_and_truth <- left_join(gs_truth_threshold_df, gs_results_threshold_df, by = "seed")
  host_sim_and_truth <- left_join(host_truth_threshold_df, host_results_threshold_df, by = "seed")
  
  # ----- Bias -----
  
  ## Gold Standard
  gs_bias_EY_A1_dZ1 <- mean(gs_sim_and_truth$aiptw_EY_Ad_dZ1 - gs_sim_and_truth$true_EY_A1_dZ1_gs, na.rm=TRUE)
  gs_bias_EY_A0_dZ1 <- mean(gs_sim_and_truth$aiptw_EY_A0_dZ1 - gs_sim_and_truth$true_EY_A0_dZ1_gs, na.rm=TRUE)
  gs_bias_E_dZ1 <- mean(gs_sim_and_truth$E_dZ1 - gs_sim_and_truth$true_E_dZ1_gs, na.rm=TRUE)
  gs_bias_subgroup_effect_treated <- mean(gs_sim_and_truth$subgroup_effect - gs_sim_and_truth$true_subgroup_effect_treated_gs, na.rm=TRUE)
  gs_bias_subgroup_effect_untreated <- mean(gs_sim_and_truth$subgroup_effect_dZ0 - gs_sim_and_truth$true_subgroup_effect_untreated_gs, na.rm=TRUE)
  gs_bias_treatment_effect <- mean(gs_sim_and_truth$treatment_effect - gs_sim_and_truth$true_treatment_effect_gs, na.rm=TRUE)
  gs_bias_subgroup_difference <- mean(gs_sim_and_truth$compare_subgroup_effect - gs_sim_and_truth$true_subgroup_difference_gs, na.rm=TRUE)
  
  ## Host
  host_bias_EY_A1_dZ1 <- mean(host_sim_and_truth$aiptw_EY_Ad_dZ1 - host_sim_and_truth$true_EY_A1_dZ1_host, na.rm=TRUE)
  host_bias_EY_A0_dZ1 <- mean(host_sim_and_truth$aiptw_EY_A0_dZ1 - host_sim_and_truth$true_EY_A0_dZ1_host, na.rm=TRUE)
  host_bias_E_dZ1 <- mean(host_sim_and_truth$E_dZ1 - host_sim_and_truth$true_E_dZ1_host, na.rm=TRUE)
  host_bias_subgroup_effect_treated <- mean(host_sim_and_truth$subgroup_effect - host_sim_and_truth$true_subgroup_effect_treated_host, na.rm=TRUE)
  host_bias_subgroup_effect_untreated <- mean(host_sim_and_truth$subgroup_effect_dZ0 - host_sim_and_truth$true_subgroup_effect_untreated_host, na.rm=TRUE)
  host_bias_treatment_effect <- mean(host_sim_and_truth$treatment_effect - host_sim_and_truth$true_treatment_effect_host, na.rm=TRUE)
  host_bias_subgroup_difference <- mean(host_sim_and_truth$compare_subgroup_effect - host_sim_and_truth$true_subgroup_difference_host, na.rm=TRUE)
  
  # ----- CI Coverage -----
  
  ## Gold Standard 
  gs_lower_ci_EY_A1_dZ1 <- gs_sim_and_truth$aiptw_EY_Ad_dZ1 - 1.96*gs_sim_and_truth$se_aiptw_EY_Ad_dZ1
  gs_upper_ci_EY_A1_dZ1 <- gs_sim_and_truth$aiptw_EY_Ad_dZ1 + 1.96*gs_sim_and_truth$se_aiptw_EY_Ad_dZ1
  gs_coverage_EY_A1_dZ1 <- mean(ifelse(gs_sim_and_truth$true_EY_A1_dZ1_gs > gs_lower_ci_EY_A1_dZ1 & 
                                         gs_sim_and_truth$true_EY_A1_dZ1_gs < gs_upper_ci_EY_A1_dZ1,
                                       1, 0), na.rm=TRUE)
  
  gs_lower_ci_EY_A0_dZ1 <- gs_sim_and_truth$aiptw_EY_A0_dZ1 - 1.96*gs_sim_and_truth$se_aiptw_EY_A0_dZ1
  gs_upper_ci_EY_A0_dZ1 <- gs_sim_and_truth$aiptw_EY_A0_dZ1 + 1.96*gs_sim_and_truth$se_aiptw_EY_A0_dZ1
  gs_coverage_EY_A0_dZ1 <- mean(ifelse(gs_sim_and_truth$true_EY_A0_dZ1_gs > gs_lower_ci_EY_A0_dZ1 & 
                                         gs_sim_and_truth$true_EY_A0_dZ1_gs < gs_upper_ci_EY_A0_dZ1,
                                       1, 0), na.rm=TRUE)
  
  # Proportion treated +- 1.96*se from package to get CI bounds
  gs_lower_ci_E_dZ1 <- gs_sim_and_truth$E_dZ1 - 1.96*gs_sim_and_truth$se_E_dZ1
  gs_upper_ci_E_dZ1 <- gs_sim_and_truth$E_dZ1 + 1.96*gs_sim_and_truth$se_E_dZ1
  
  # check if data adaptive truth falls inside bounds, then take mean to get CI coverage
  gs_coverage_E_dZ1 <- mean(ifelse(gs_sim_and_truth$true_E_dZ1_gs > gs_lower_ci_E_dZ1 & 
                                     gs_sim_and_truth$true_E_dZ1_gs < gs_upper_ci_E_dZ1,
                                   1, 0), na.rm=TRUE)
  
  
  gs_lower_ci_subgroup_effect_treated <- gs_sim_and_truth$subgroup_effect - 1.96*gs_sim_and_truth$se_subgroup_effect
  gs_upper_ci_subgroup_effect_treated <- gs_sim_and_truth$subgroup_effect + 1.96*gs_sim_and_truth$se_subgroup_effect
  gs_coverage_subgroup_effect_treated <- mean(ifelse(gs_sim_and_truth$true_subgroup_effect_treated_gs > gs_lower_ci_subgroup_effect_treated & 
                                                       gs_sim_and_truth$true_subgroup_effect_treated_gs < gs_upper_ci_subgroup_effect_treated,
                                                     1, 0), na.rm=TRUE)
  
  gs_lower_ci_subgroup_effect_untreated <- gs_sim_and_truth$subgroup_effect_dZ0 - 1.96*gs_sim_and_truth$se_subgroup_effect_dZ0
  gs_upper_ci_subgroup_effect_untreated <- gs_sim_and_truth$subgroup_effect_dZ0 + 1.96*gs_sim_and_truth$se_subgroup_effect_dZ0
  gs_coverage_subgroup_effect_untreated <- mean(ifelse(gs_sim_and_truth$true_subgroup_effect_untreated_gs > gs_lower_ci_subgroup_effect_untreated & 
                                                         gs_sim_and_truth$true_subgroup_effect_untreated_gs < gs_upper_ci_subgroup_effect_untreated,
                                                       1, 0), na.rm=TRUE)
  
  gs_lower_ci_treatment_effect <- gs_sim_and_truth$treatment_effect - 1.96*gs_sim_and_truth$se_treatment_effect
  gs_upper_ci_treatment_effect <- gs_sim_and_truth$treatment_effect + 1.96*gs_sim_and_truth$se_treatment_effect
  gs_coverage_treatment_effect <- mean(ifelse(gs_sim_and_truth$true_treatment_effect_gs > gs_lower_ci_treatment_effect & 
                                                gs_sim_and_truth$true_treatment_effect_gs < gs_upper_ci_treatment_effect,
                                              1, 0), na.rm=TRUE)
  
  gs_lower_ci_subgroup_difference <- gs_sim_and_truth$compare_subgroup_effect - 1.96*gs_sim_and_truth$se_compare_subgroup_effect
  gs_upper_ci_subgroup_difference <- gs_sim_and_truth$compare_subgroup_effect + 1.96*gs_sim_and_truth$se_compare_subgroup_effect
  gs_coverage_subgroup_difference <- mean(ifelse(gs_sim_and_truth$true_subgroup_difference_gs > gs_lower_ci_subgroup_difference & 
                                                   gs_sim_and_truth$true_subgroup_difference_gs < gs_upper_ci_subgroup_difference,
                                                 1, 0), na.rm=TRUE)
  
  ## Host 
  host_lower_ci_EY_A1_dZ1 <- host_sim_and_truth$aiptw_EY_Ad_dZ1 - 1.96*host_sim_and_truth$se_aiptw_EY_Ad_dZ1
  host_upper_ci_EY_A1_dZ1 <- host_sim_and_truth$aiptw_EY_Ad_dZ1 + 1.96*host_sim_and_truth$se_aiptw_EY_Ad_dZ1
  host_coverage_EY_A1_dZ1 <- mean(ifelse(host_sim_and_truth$true_EY_A1_dZ1_host > host_lower_ci_EY_A1_dZ1 & 
                                           host_sim_and_truth$true_EY_A1_dZ1_host < host_upper_ci_EY_A1_dZ1,
                                         1, 0), na.rm=TRUE)
  
  host_lower_ci_EY_A0_dZ1 <- host_sim_and_truth$aiptw_EY_A0_dZ1 - 1.96*host_sim_and_truth$se_aiptw_EY_A0_dZ1
  host_upper_ci_EY_A0_dZ1 <- host_sim_and_truth$aiptw_EY_A0_dZ1 + 1.96*host_sim_and_truth$se_aiptw_EY_A0_dZ1
  host_coverage_EY_A0_dZ1 <- mean(ifelse(host_sim_and_truth$true_EY_A0_dZ1_host > host_lower_ci_EY_A0_dZ1 & 
                                           host_sim_and_truth$true_EY_A0_dZ1_host < host_upper_ci_EY_A0_dZ1,
                                         1, 0), na.rm=TRUE)
  
  host_lower_ci_E_dZ1 <- host_sim_and_truth$E_dZ1 - 1.96*host_sim_and_truth$se_E_dZ1
  host_upper_ci_E_dZ1 <- host_sim_and_truth$E_dZ1 + 1.96*host_sim_and_truth$se_E_dZ1
  host_coverage_E_dZ1 <- mean(ifelse(host_sim_and_truth$true_E_dZ1_host > host_lower_ci_E_dZ1 & 
                                       host_sim_and_truth$true_E_dZ1_host < host_upper_ci_E_dZ1,
                                     1, 0), na.rm=TRUE)
  
  # NEW FOR TESTING
  host_lower_ci_E_dZ1_da <- host_sim_and_truth$E_dZ1 - 1.96*host_sim_and_truth$true_se_E_dZ1_host
  host_upper_ci_E_dZ1_da <- host_sim_and_truth$E_dZ1 + 1.96*host_sim_and_truth$true_se_E_dZ1_host
  host_coverage_E_dZ1_da <- mean(ifelse(host_sim_and_truth$true_E_dZ1_host > host_lower_ci_E_dZ1_da & 
                                        host_sim_and_truth$true_E_dZ1_host < host_upper_ci_E_dZ1_da,
                                      1, 0), na.rm=TRUE)
  
  host_lower_ci_subgroup_effect_treated <- host_sim_and_truth$subgroup_effect - 1.96*host_sim_and_truth$se_subgroup_effect
  host_upper_ci_subgroup_effect_treated <- host_sim_and_truth$subgroup_effect + 1.96*host_sim_and_truth$se_subgroup_effect
  host_coverage_subgroup_effect_treated <- mean(ifelse(host_sim_and_truth$true_subgroup_effect_treated_host > host_lower_ci_subgroup_effect_treated & 
                                                         host_sim_and_truth$true_subgroup_effect_treated_host < host_upper_ci_subgroup_effect_treated,
                                                       1, 0), na.rm=TRUE)
  
  host_lower_ci_subgroup_effect_untreated <- host_sim_and_truth$subgroup_effect_dZ0 - 1.96*host_sim_and_truth$se_subgroup_effect_dZ0
  host_upper_ci_subgroup_effect_untreated <- host_sim_and_truth$subgroup_effect_dZ0 + 1.96*host_sim_and_truth$se_subgroup_effect_dZ0
  host_coverage_subgroup_effect_untreated <- mean(ifelse(host_sim_and_truth$true_subgroup_effect_untreated_host > host_lower_ci_subgroup_effect_untreated & 
                                                           host_sim_and_truth$true_subgroup_effect_untreated_host < host_upper_ci_subgroup_effect_untreated,
                                                         1, 0), na.rm=TRUE)
  
  host_lower_ci_treatment_effect <- host_sim_and_truth$treatment_effect - 1.96*host_sim_and_truth$se_treatment_effect
  host_upper_ci_treatment_effect <- host_sim_and_truth$treatment_effect + 1.96*host_sim_and_truth$se_treatment_effect
  host_coverage_treatment_effect <- mean(ifelse(host_sim_and_truth$true_treatment_effect_host > host_lower_ci_treatment_effect & 
                                                  host_sim_and_truth$true_treatment_effect_host < host_upper_ci_treatment_effect,
                                                1, 0), na.rm=TRUE)
  
  host_lower_ci_subgroup_difference <- host_sim_and_truth$compare_subgroup_effect - 1.96*host_sim_and_truth$se_compare_subgroup_effect
  host_upper_ci_subgroup_difference <- host_sim_and_truth$compare_subgroup_effect + 1.96*host_sim_and_truth$se_compare_subgroup_effect
  host_coverage_subgroup_difference <- mean(ifelse(host_sim_and_truth$true_subgroup_difference_host > host_lower_ci_subgroup_difference & 
                                                     host_sim_and_truth$true_subgroup_difference_host < host_upper_ci_subgroup_difference,
                                                   1, 0), na.rm=TRUE)
  
  # Format results into table
  
  results_table <- data.frame(effect_est = c(rep(c("$E[Y(1) | d(Z) = 1]$"), 2),
                                             rep(c("$E[Y(0) | d(Z) = 1]$"), 2),
                                             rep(c("$E[d(Z) = 1]$"), 2),
                                             rep(c("$E[Y(1) - Y(0) | d(Z) = 1]$"), 2),
                                             rep(c("$E[Y(1) - Y(0) | d(Z) = 0]$"), 2),
                                             rep(c("$E[Y(d) - Y(0)]$"), 2),
                                             rep(c("$E[Y(1) - Y(0) | d(Z) = 1] - E[Y(1) - Y(0) | d(Z) = 0]$"), 2)),
                              measure = c(rep(c("Bias", "95% CI Coverage"), 7)),
                              gold_standard = c(gs_bias_EY_A1_dZ1,
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
                                                gs_coverage_subgroup_difference),
                              host = c(host_bias_EY_A1_dZ1,
                                       host_coverage_EY_A1_dZ1,
                                       host_bias_EY_A0_dZ1,
                                       host_coverage_EY_A0_dZ1,
                                       host_bias_E_dZ1,
                                       host_coverage_E_dZ1,
                                       host_bias_subgroup_effect_treated,
                                       host_coverage_subgroup_effect_treated,
                                       host_bias_subgroup_effect_untreated,
                                       host_coverage_subgroup_effect_untreated,
                                       host_bias_treatment_effect,
                                       host_coverage_treatment_effect,
                                       host_bias_subgroup_difference,
                                       host_coverage_subgroup_difference))


  custom_column_name <- c("Effect Estimate",
                          "Performance Metric",
                          "Gold Standard Rule",
                          "Host Rule")

  colnames(results_table) <- custom_column_name
   
  return(results_table)
}


# iterate over thresholds
for(t in threshold_list){
    
  # call function for each threshold
  results_table <- get_data_adaptive_results_table(t = t, 
                                                   gs_data_adaptive_truth_df = gs_data_adaptive_truth_df, 
                                                   host_data_adaptive_truth_df = host_data_adaptive_truth_df, 
                                                   gs_results_df = gs_results_df, 
                                                   host_results_df = host_results_df)
  # make table pretty
  table <- kbl(results_table, format = 'html', caption = paste0("Data adaptive simulation results for threshold = ", t, " (n = 6692, replicates = 1000)"), booktabs=T, digits=5) %>%
    kable_styling(latex_options = "striped") %>%
    column_spec(1, bold=T) %>%
    collapse_rows(columns = 1:2, latex_hline = "major", row_group_label_position = "first") 
  
  print(table)
    
}

