here::i_am("sim_data/04_simulation_data_truth_comparison.R")

library(knitr)
library(kableExtra)

# load truth and aggregated results
truth_df_gs <- read.csv("truth/average_truth_gs.csv")
truth_df_host <- read.csv("truth/average_truth_host.csv")
gs_results_df <- read.csv("results_csv/gold_standard_n_6692.csv")
host_results_df <- read.csv("results_csv/host_n_6692.csv")

threshold_list <- c(0.05)

# -------------------------------------
# Function to get table of results
# -------------------------------------
get_results_table <- function(t, truth_df_gs, truth_df_host, gs_results_df, host_results_df){
 
  true_threshold_gs_df <- truth_df_gs[truth_df_gs$t_list == t,]
  true_threshold_host_df <- truth_df_host[truth_df_host$t_list == t,]
  gs_threshold_df <- gs_results_df[gs_results_df$threshold == t,]
  host_threshold_df <- host_results_df[host_results_df$threshold == t,]
  
  #note same truths for both sets of covariates
  gs_sim_and_truth <- cbind(gs_threshold_df, data.frame(true_EY_A1_dZ1 = true_threshold_gs_df$EY_A1_dZ1[1],
                                                        true_EY_A0_dZ1 = true_threshold_gs_df$EY_A0_dZ1[1],
                                                        true_E_dZ1 = true_threshold_gs_df$E_dZ1[1],
                                                        true_subgroup_effect_treated = true_threshold_gs_df$subgroup_effect_treated[1],
                                                        true_subgroup_effect_untreated = true_threshold_gs_df$subgroup_effect_untreated[1],
                                                        true_treatment_effect = true_threshold_gs_df$treatment_effect[1],
                                                        true_subgroup_difference = true_threshold_gs_df$subgroup_difference[1]))
  
  host_sim_and_truth <- cbind(host_threshold_df, data.frame(true_EY_A1_dZ1 = true_threshold_host_df$EY_A1_dZ1[1],
                                                            true_EY_A0_dZ1 = true_threshold_host_df$EY_A0_dZ1[1],
                                                            true_E_dZ1 = true_threshold_host_df$E_dZ1[1],
                                                            true_subgroup_effect_treated = true_threshold_host_df$subgroup_effect_treated[1],
                                                            true_subgroup_effect_untreated = true_threshold_host_df$subgroup_effect_untreated[1],
                                                            true_treatment_effect = true_threshold_host_df$treatment_effect[1],
                                                            true_subgroup_difference = true_threshold_host_df$subgroup_difference[1]))
  
  # ----- Bias -----
  
  ## Gold Standard
  gs_bias_EY_A1_dZ1 <- mean(gs_sim_and_truth$aiptw_EY_Ad_dZ1 - gs_sim_and_truth$true_EY_A1_dZ1, na.rm=TRUE)
  gs_bias_EY_A0_dZ1 <- mean(gs_sim_and_truth$aiptw_EY_A0_dZ1 - gs_sim_and_truth$true_EY_A0_dZ1, na.rm=TRUE)
  gs_bias_E_dZ1 <- mean(gs_sim_and_truth$E_dZ1 - gs_sim_and_truth$true_E_dZ1, na.rm=TRUE)
  gs_bias_subgroup_effect_treated <- mean(gs_sim_and_truth$subgroup_effect - gs_sim_and_truth$true_subgroup_effect_treated, na.rm=TRUE)
  gs_bias_subgroup_effect_untreated <- mean(gs_sim_and_truth$subgroup_effect_dZ0 - gs_sim_and_truth$true_subgroup_effect_untreated, na.rm=TRUE)
  gs_bias_treatment_effect <- mean(gs_sim_and_truth$treatment_effect - gs_sim_and_truth$true_treatment_effect, na.rm=TRUE)
  gs_bias_subgroup_difference <- mean(gs_sim_and_truth$compare_subgroup_effect - gs_sim_and_truth$true_subgroup_difference, na.rm=TRUE)
  
  ## Host
  host_bias_EY_A1_dZ1 <- mean(host_sim_and_truth$aiptw_EY_Ad_dZ1 - host_sim_and_truth$true_EY_A1_dZ1, na.rm=TRUE)
  host_bias_EY_A0_dZ1 <- mean(host_sim_and_truth$aiptw_EY_A0_dZ1 - host_sim_and_truth$true_EY_A0_dZ1, na.rm=TRUE)
  host_bias_E_dZ1 <- mean(host_sim_and_truth$E_dZ1 - host_sim_and_truth$true_E_dZ1, na.rm=TRUE)
  host_bias_subgroup_effect_treated <- mean(host_sim_and_truth$subgroup_effect - host_sim_and_truth$true_subgroup_effect_treated, na.rm=TRUE)
  host_bias_subgroup_effect_untreated <- mean(host_sim_and_truth$subgroup_effect_dZ0 - host_sim_and_truth$true_subgroup_effect_untreated, na.rm=TRUE)
  host_bias_treatment_effect <- mean(host_sim_and_truth$treatment_effect - host_sim_and_truth$true_treatment_effect, na.rm=TRUE)
  host_bias_subgroup_difference <- mean(host_sim_and_truth$compare_subgroup_effect - host_sim_and_truth$true_subgroup_difference, na.rm=TRUE)
  
  # ----- CI Coverage -----
  
  ## Gold Standard 
  gs_lower_ci_EY_A1_dZ1 <- gs_sim_and_truth$aiptw_EY_Ad_dZ1 - 1.96*gs_sim_and_truth$se_aiptw_EY_Ad_dZ1
  gs_upper_ci_EY_A1_dZ1 <- gs_sim_and_truth$aiptw_EY_Ad_dZ1 + 1.96*gs_sim_and_truth$se_aiptw_EY_Ad_dZ1
  gs_coverage_EY_A1_dZ1 <- mean(ifelse(gs_sim_and_truth$true_EY_A1_dZ1 > gs_lower_ci_EY_A1_dZ1 & 
                                         gs_sim_and_truth$true_EY_A1_dZ1 < gs_upper_ci_EY_A1_dZ1,
                                       1, 0), na.rm=TRUE)
  
  gs_lower_ci_EY_A0_dZ1 <- gs_sim_and_truth$aiptw_EY_A0_dZ1 - 1.96*gs_sim_and_truth$se_aiptw_EY_A0_dZ1
  gs_upper_ci_EY_A0_dZ1 <- gs_sim_and_truth$aiptw_EY_A0_dZ1 + 1.96*gs_sim_and_truth$se_aiptw_EY_A0_dZ1
  gs_coverage_EY_A0_dZ1 <- mean(ifelse(gs_sim_and_truth$true_EY_A0_dZ1 > gs_lower_ci_EY_A0_dZ1 & 
                                         gs_sim_and_truth$true_EY_A0_dZ1 < gs_upper_ci_EY_A0_dZ1,
                                       1, 0), na.rm=TRUE)
  
  gs_lower_ci_E_dZ1 <- gs_sim_and_truth$E_dZ1 - 1.96*gs_sim_and_truth$se_E_dZ1
  gs_upper_ci_E_dZ1 <- gs_sim_and_truth$E_dZ1 + 1.96*gs_sim_and_truth$se_E_dZ1
  gs_coverage_E_dZ1 <- mean(ifelse(gs_sim_and_truth$true_E_dZ1 > gs_lower_ci_E_dZ1 & 
                                     gs_sim_and_truth$true_E_dZ1 < gs_upper_ci_E_dZ1,
                                   1, 0), na.rm=TRUE)
  
  gs_lower_ci_subgroup_effect_treated <- gs_sim_and_truth$subgroup_effect - 1.96*gs_sim_and_truth$se_subgroup_effect
  gs_upper_ci_subgroup_effect_treated <- gs_sim_and_truth$subgroup_effect + 1.96*gs_sim_and_truth$se_subgroup_effect
  gs_coverage_subgroup_effect_treated <- mean(ifelse(gs_sim_and_truth$true_subgroup_effect_treated > gs_lower_ci_subgroup_effect_treated & 
                                                       gs_sim_and_truth$true_subgroup_effect_treated < gs_upper_ci_subgroup_effect_treated,
                                                     1, 0), na.rm=TRUE)
  
  gs_lower_ci_subgroup_effect_untreated <- gs_sim_and_truth$subgroup_effect_dZ0 - 1.96*gs_sim_and_truth$se_subgroup_effect_dZ0
  gs_upper_ci_subgroup_effect_untreated <- gs_sim_and_truth$subgroup_effect_dZ0 + 1.96*gs_sim_and_truth$se_subgroup_effect_dZ0
  gs_coverage_subgroup_effect_untreated <- mean(ifelse(gs_sim_and_truth$true_subgroup_effect_untreated > gs_lower_ci_subgroup_effect_untreated & 
                                                         gs_sim_and_truth$true_subgroup_effect_untreated < gs_upper_ci_subgroup_effect_untreated,
                                                       1, 0), na.rm=TRUE)
  
  gs_lower_ci_treatment_effect <- gs_sim_and_truth$treatment_effect - 1.96*gs_sim_and_truth$se_treatment_effect
  gs_upper_ci_treatment_effect <- gs_sim_and_truth$treatment_effect + 1.96*gs_sim_and_truth$se_treatment_effect
  gs_coverage_treatment_effect <- mean(ifelse(gs_sim_and_truth$true_treatment_effect > gs_lower_ci_treatment_effect & 
                                                gs_sim_and_truth$true_treatment_effect < gs_upper_ci_treatment_effect,
                                              1, 0), na.rm=TRUE)
  
  gs_lower_ci_subgroup_difference <- gs_sim_and_truth$compare_subgroup_effect - 1.96*gs_sim_and_truth$se_compare_subgroup_effect
  gs_upper_ci_subgroup_difference <- gs_sim_and_truth$compare_subgroup_effect + 1.96*gs_sim_and_truth$se_compare_subgroup_effect
  gs_coverage_subgroup_difference <- mean(ifelse(gs_sim_and_truth$true_subgroup_difference > gs_lower_ci_subgroup_difference & 
                                                   gs_sim_and_truth$true_subgroup_difference < gs_upper_ci_subgroup_difference,
                                                 1, 0), na.rm=TRUE)
  
  ## Host 
  host_lower_ci_EY_A1_dZ1 <- host_sim_and_truth$aiptw_EY_Ad_dZ1 - 1.96*host_sim_and_truth$se_aiptw_EY_Ad_dZ1
  host_upper_ci_EY_A1_dZ1 <- host_sim_and_truth$aiptw_EY_Ad_dZ1 + 1.96*host_sim_and_truth$se_aiptw_EY_Ad_dZ1
  host_coverage_EY_A1_dZ1 <- mean(ifelse(host_sim_and_truth$true_EY_A1_dZ1 > host_lower_ci_EY_A1_dZ1 & 
                                           host_sim_and_truth$true_EY_A1_dZ1 < host_upper_ci_EY_A1_dZ1,
                                         1, 0), na.rm=TRUE)
  
  host_lower_ci_EY_A0_dZ1 <- host_sim_and_truth$aiptw_EY_A0_dZ1 - 1.96*host_sim_and_truth$se_aiptw_EY_A0_dZ1
  host_upper_ci_EY_A0_dZ1 <- host_sim_and_truth$aiptw_EY_A0_dZ1 + 1.96*host_sim_and_truth$se_aiptw_EY_A0_dZ1
  host_coverage_EY_A0_dZ1 <- mean(ifelse(host_sim_and_truth$true_EY_A0_dZ1 > host_lower_ci_EY_A0_dZ1 & 
                                           host_sim_and_truth$true_EY_A0_dZ1 < host_upper_ci_EY_A0_dZ1,
                                         1, 0), na.rm=TRUE)
  
  host_lower_ci_E_dZ1 <- host_sim_and_truth$E_dZ1 - 1.96*host_sim_and_truth$se_E_dZ1
  host_upper_ci_E_dZ1 <- host_sim_and_truth$E_dZ1 + 1.96*host_sim_and_truth$se_E_dZ1
  host_coverage_E_dZ1 <- mean(ifelse(host_sim_and_truth$true_E_dZ1 > host_lower_ci_E_dZ1 & 
                                       host_sim_and_truth$true_E_dZ1 < host_upper_ci_E_dZ1,
                                     1, 0), na.rm=TRUE)
  
  host_lower_ci_subgroup_effect_treated <- host_sim_and_truth$subgroup_effect - 1.96*host_sim_and_truth$se_subgroup_effect
  host_upper_ci_subgroup_effect_treated <- host_sim_and_truth$subgroup_effect + 1.96*host_sim_and_truth$se_subgroup_effect
  host_coverage_subgroup_effect_treated <- mean(ifelse(host_sim_and_truth$true_subgroup_effect_treated > host_lower_ci_subgroup_effect_treated & 
                                                         host_sim_and_truth$true_subgroup_effect_treated < host_upper_ci_subgroup_effect_treated,
                                                       1, 0), na.rm=TRUE)
  
  host_lower_ci_subgroup_effect_untreated <- host_sim_and_truth$subgroup_effect_dZ0 - 1.96*host_sim_and_truth$se_subgroup_effect_dZ0
  host_upper_ci_subgroup_effect_untreated <- host_sim_and_truth$subgroup_effect_dZ0 + 1.96*host_sim_and_truth$se_subgroup_effect_dZ0
  host_coverage_subgroup_effect_untreated <- mean(ifelse(host_sim_and_truth$true_subgroup_effect_untreated > host_lower_ci_subgroup_effect_untreated & 
                                                           host_sim_and_truth$true_subgroup_effect_untreated < host_upper_ci_subgroup_effect_untreated,
                                                         1, 0), na.rm=TRUE)
  
  host_lower_ci_treatment_effect <- host_sim_and_truth$treatment_effect - 1.96*host_sim_and_truth$se_treatment_effect
  host_upper_ci_treatment_effect <- host_sim_and_truth$treatment_effect + 1.96*host_sim_and_truth$se_treatment_effect
  host_coverage_treatment_effect <- mean(ifelse(host_sim_and_truth$true_treatment_effect > host_lower_ci_treatment_effect & 
                                                  host_sim_and_truth$true_treatment_effect < host_upper_ci_treatment_effect,
                                                1, 0), na.rm=TRUE)
  
  host_lower_ci_subgroup_difference <- host_sim_and_truth$compare_subgroup_effect - 1.96*host_sim_and_truth$se_compare_subgroup_effect
  host_upper_ci_subgroup_difference <- host_sim_and_truth$compare_subgroup_effect + 1.96*host_sim_and_truth$se_compare_subgroup_effect
  host_coverage_subgroup_difference <- mean(ifelse(host_sim_and_truth$true_subgroup_difference > host_lower_ci_subgroup_difference & 
                                                     host_sim_and_truth$true_subgroup_difference < host_upper_ci_subgroup_difference,
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

# get true values for given threshold
for(t in threshold_list){
  
  results_table <- get_results_table(t = t,
                                     truth_df_gs = truth_df_gs,
                                     truth_df_host = truth_df_host, 
                                     gs_results_df = gs_results_df, 
                                     host_results_df = host_results_df)
  
  table <- kbl(results_table, format = 'html', caption = paste0("Simulation Results for threshold = ", t, " (n = 6692, replicates = 1000)"), booktabs=T, digits=5) %>%
    kable_styling(latex_options = "striped") %>%
    column_spec(1, bold=T) %>%
    collapse_rows(columns = 1:2, latex_hline = "major", row_group_label_position = "first") 
  
  print(table)
  
}
  

