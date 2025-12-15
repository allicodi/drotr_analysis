
here::i_am("09_final_tables.R")

library(dplyr)
library(kableExtra)

# read in data adaptive truth by seed file
gs_data_adaptive_truth_df <- read.csv("truth/gs_data_adaptive_truth_by_seed.csv")
host_data_adaptive_truth_df <- read.csv("truth/host_data_adaptive_truth_by_seed.csv")

# 'optimal' truth df
gs_truth_df <- read.csv("truth/average_truth_gs.csv")
host_truth_df<- read.csv("truth/average_truth_host.csv")

# read simulation results
gs_results_df <- read.csv("results_csv/gold_standard_n_6692.csv")
host_results_df <- read.csv("results_csv/host_n_6692.csv")

# pick two thresholds for each rule

# Host - 0.05, 0.075
# Gold standard - 0.05, 0.25
t_list <- c(0.05, 0.075, 0.25)

da_results_tbl <- vector("list", length = length(t_list))
results_tbl <- vector("list", length = length(t_list))

# -------------------------------------
# Function to get table of optimal results
# -------------------------------------
get_results_table <- function(t, truth_df_gs, truth_df_host, gs_results_df, host_results_df){
  
  # Define a helper function to create an NA-filled data frame with the same number of rows as the input data frame
  create_na_dataframe <- function(df, true_df) {
    n <- nrow(df)
    if (n == 0) {
      # Return a data frame of NA values with 0 rows to match the input
      return(data.frame(true_EY_A1_dZ1 = rep(NA, n),
                        true_EY_A0_dZ1 = rep(NA, n),
                        true_E_dZ1 = rep(NA, n),
                        true_subgroup_effect_treated = rep(NA, n),
                        true_subgroup_effect_untreated = rep(NA, n),
                        true_treatment_effect = rep(NA, n),
                        true_subgroup_difference = rep(NA, n)))
    } else {
      # Return the actual values from the true data frame
      return(data.frame(true_EY_A1_dZ1 = rep(true_df$EY_A1_dZ1[1], n),
                        true_EY_A0_dZ1 = rep(true_df$EY_A0_dZ1[1], n),
                        true_E_dZ1 = rep(true_df$E_dZ1[1], n),
                        true_subgroup_effect_treated = rep(true_df$subgroup_effect_treated[1], n),
                        true_subgroup_effect_untreated = rep(true_df$subgroup_effect_untreated[1], n),
                        true_treatment_effect = rep(true_df$treatment_effect[1], n),
                        true_subgroup_difference = rep(true_df$subgroup_difference[1], n)))
    }
  }
  
  # Filter based on threshold
  true_threshold_gs_df <- truth_df_gs[truth_df_gs$t_list == t,]
  true_threshold_host_df <- truth_df_host[truth_df_host$t_list == t,]
  gs_threshold_df <- gs_results_df[gs_results_df$threshold == t,]
  host_threshold_df <- host_results_df[host_results_df$threshold == t,]
  
  # Create sim and truth data frames, ensuring consistent number of rows with NA if the result data frame is empty
  gs_sim_and_truth <- cbind(gs_threshold_df, create_na_dataframe(gs_threshold_df, true_threshold_gs_df))
  host_sim_and_truth <- cbind(host_threshold_df, create_na_dataframe(host_threshold_df, true_threshold_host_df))
  
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

# ---------------------------------------------------
# Function to get table of data adaptive results
# ---------------------------------------------------
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

for(i in 1:length(t_list)){
  
  t <- t_list[i]
  
  da_results_tbl[[i]] <- get_data_adaptive_results_table(t, gs_data_adaptive_truth_df, host_data_adaptive_truth_df, gs_results_df, host_results_df)
  results_tbl[[i]] <- get_results_table(t, gs_truth_df, host_truth_df, gs_results_df, host_results_df)
  
}

# GS Table 
idx <- c(1,3)
final_results_gs <- data.frame(`Effect Estimate` = c("$E[Y(1) | d(Z) = 1]$",
                                                     "$E[Y(1) | d(Z) = 1]$",
                                                     "$E[Y(0) | d(Z) = 1]$",
                                                     "$E[Y(0) | d(Z) = 1]$",
                                                     "$E[d(Z) = 1]$",
                                                     "$E[d(Z) = 1]$",
                                                     "$E[Y(1) - Y(0) | d(Z) = 1]$",
                                                     "$E[Y(1) - Y(0) | d(Z) = 1]$",
                                                     "$E[Y(1) - Y(0) | d(Z) = 0]$",
                                                     "$E[Y(1) - Y(0) | d(Z) = 0]$",
                                                     "$E[Y(d) - Y(0)]$",
                                                     "$E[Y(d) - Y(0)]$",
                                                     "$E[Y(1) - Y(0) | d(Z) = 1] - E[Y(1) - Y(0) | d(Z) = 0]$",
                                                     "$E[Y(1) - Y(0) | d(Z) = 1] - E[Y(1) - Y(0) | d(Z) = 0]$"),
                               `Performance Metric` = rep(c("Bias", "95% CI Coverage"), 7))
for(i in idx){
  
  optimal <- results_tbl[[i]]
  optimal$type <- "Optimal"
  da <- da_results_tbl[[i]]
  da$type <- "Data Adaptive"
  results <- rbind(optimal, da)
  
  pivoted_results <- results %>%
    pivot_wider(
      names_from = type, 
      values_from = c(`Gold Standard Rule`, `Host Rule`),
      names_glue = "{.value} {type}"
    )
  
  final_results <- pivoted_results[,c("Effect Estimate", "Performance Metric", "Gold Standard Rule Optimal", "Gold Standard Rule Data Adaptive")]
  colnames(final_results) <- c("Effect Estimate", "Performance Metric", paste0("Optimal: t = ",t_list[i]), paste0("Data Adaptive: t = ",t_list[i]))
  final_results_gs <- cbind(final_results_gs, final_results[,c(paste0("Optimal: t = ",t_list[i]), paste0("Data Adaptive: t = ",t_list[i]))])
}

colnames(final_results_gs) <- c("Effect Estimate", "Performance Metric", "Optimal ", "Data Adaptive ", "Optimal", "Data Adaptive")

table <- kbl(final_results_gs, format = 'html', caption = paste0("Simulation results for comprehensive rule (n = 6692, replicates = 1000)"), booktabs=T, digits=5) %>%
  kable_styling(latex_options = "striped") %>%
  column_spec(1, bold=T, background = NULL) %>%
  collapse_rows(columns = 1:2, latex_hline = "major", row_group_label_position = "first") %>%
  add_header_above(c(" ", " ", "t = 0.05" = 2, "t = 0.25" = 2))

print(table)

# --------------
# Host results

# host Table 
idx <- c(1,2)
final_results_host <- data.frame(`Effect Estimate` = c("$E[Y(1) | d(Z) = 1]$",
                                                     "$E[Y(1) | d(Z) = 1]$",
                                                     "$E[Y(0) | d(Z) = 1]$",
                                                     "$E[Y(0) | d(Z) = 1]$",
                                                     "$E[d(Z) = 1]$",
                                                     "$E[d(Z) = 1]$",
                                                     "$E[Y(1) - Y(0) | d(Z) = 1]$",
                                                     "$E[Y(1) - Y(0) | d(Z) = 1]$",
                                                     "$E[Y(1) - Y(0) | d(Z) = 0]$",
                                                     "$E[Y(1) - Y(0) | d(Z) = 0]$",
                                                     "$E[Y(d) - Y(0)]$",
                                                     "$E[Y(d) - Y(0)]$",
                                                     "$E[Y(1) - Y(0) | d(Z) = 1] - E[Y(1) - Y(0) | d(Z) = 0]$",
                                                     "$E[Y(1) - Y(0) | d(Z) = 1] - E[Y(1) - Y(0) | d(Z) = 0]$"),
                               `Performance Metric` = rep(c("Bias", "95% CI Coverage"), 7))
for(i in idx){
  
  optimal <- results_tbl[[i]]
  optimal$type <- "Optimal"
  da <- da_results_tbl[[i]]
  da$type <- "Data Adaptive"
  results <- rbind(optimal, da)
  
  pivoted_results <- results %>%
    pivot_wider(
      names_from = type, 
      values_from = c(`Gold Standard Rule`, `Host Rule`),
      names_glue = "{.value} {type}"
    )
  
  final_results <- pivoted_results[,c("Effect Estimate", "Performance Metric", "Host Rule Optimal", "Host Rule Data Adaptive")]
  colnames(final_results) <- c("Effect Estimate", "Performance Metric", paste0("Optimal: t = ",t_list[i]), paste0("Data Adaptive: t = ",t_list[i]))
  final_results_host <- cbind(final_results_host, final_results[,c(paste0("Optimal: t = ",t_list[i]), paste0("Data Adaptive: t = ",t_list[i]))])
}

colnames(final_results_host) <- c("Effect Estimate", "Performance Metric", "Optimal ", "Data Adaptive ", "Optimal ", "Data Adaptive ")

table <- kbl(final_results_host, format = 'html', caption = paste0("Simulation results for host rule (n = 6692, replicates = 1000)"), booktabs=T, digits=5) %>%
  kable_styling(latex_options = "striped") %>%
  column_spec(1, bold=T, background = NULL) %>%
  collapse_rows(columns = 1:2, latex_hline = "major", row_group_label_position = "first") %>%
  add_header_above(c(" ", " ", "t = 0.05" = 2, "t = 0.075" = 2))

print(table)


# --------------

# Compare at threshold 0.05

# 'Optimal' difference
optimal_truth <- read.csv(here::here("truth/truth_df_avg_dif.csv"))
compare_results <- read.csv(here::here("results_csv/compare_results.csv"))

compare_results$lower_ci <- compare_results$expected_val_of_comparison - 1.96*sqrt(compare_results$var_of_comparison)
compare_results$upper_ci <- compare_results$expected_val_of_comparison + 1.96*sqrt(compare_results$var_of_comparison)

type <- c("atrt", "atnrt", "atr")
bias <- vector("numeric", length = 3)
coverage <- vector("numeric", length = 3)

for(i in 1:3){
  sub_df <- compare_results[compare_results$type == type[i],]
  
  bias[i] <- mean(compare_results$expected_val_of_comparison - optimal_truth[1,3+i])
  
  coverage_vec <- ifelse(compare_results$lower_ci < optimal_truth[1,3+i] & compare_results$upper_ci > optimal_truth[1,3+i], 1, 0)
  coverage[i] <- mean(coverage_vec)
}

optimal_results_comparison <- data.frame(type = type, bias = bias, coverage = coverage)

# Data Adaptive difference
gs_da_0.05 <- na.omit(gs_data_adaptive_truth_df[gs_data_adaptive_truth_df$threshold == 0.05,] )
host_da_0.05 <- na.omit(host_data_adaptive_truth_df[host_data_adaptive_truth_df$threshold == 0.05,])

da_combined_res <- left_join(gs_da_0.05, host_da_0.05, by = "seed")

da_combined_res$da_atrt_dif <- da_combined_res$subgroup_effect_treated_gs - da_combined_res$subgroup_effect_treated_host
da_combined_res$da_atnrt_dif <- da_combined_res$subgroup_effect_untreated_gs - da_combined_res$subgroup_effect_untreated_host
da_combined_res$da_atr_dif <- da_combined_res$treatment_effect_gs - da_combined_res$treatment_effect_host

bias_atrt_da <- vector("numeric", length = 1000)
bias_atnrt_da <- vector("numeric", length = 1000)
bias_atr_da <- vector("numeric", length = 1000)

coverage_atrt_da <- vector("numeric", length = 1000)
coverage_atnrt_da <- vector("numeric", length = 1000)
coverage_atr_da <- vector("numeric", length = 1000)

for(seed in 1:1000){
  res_seed <- compare_results[compare_results$seed == seed,]
  da_seed <- da_combined_res[da_combined_res$seed == seed,]
  
  bias_atrt_da[seed] <- res_seed$expected_val_of_comparison[res_seed$type == "atrt"] - da_seed$da_atrt_dif
  bias_atnrt_da[seed] <- res_seed$expected_val_of_comparison[res_seed$type == "atnrt"] - da_seed$da_atnrt_dif
  bias_atr_da[seed] <- res_seed$expected_val_of_comparison[res_seed$type == "atr"] - da_seed$da_atr_dif
  
  coverage_atrt_da[seed] <- ifelse(res_seed$lower_ci[res_seed$type == "atrt"] < da_seed$da_atrt_dif 
                                   & res_seed$upper_ci[res_seed$type == "atrt"] > da_seed$da_atrt_dif, 1, 0)
  coverage_atnrt_da[seed] <- ifelse(res_seed$lower_ci[res_seed$type == "atnrt"] < da_seed$da_atnrt_dif 
                                   & res_seed$upper_ci[res_seed$type == "atnrt"] > da_seed$da_atnrt_dif, 1, 0)
  coverage_atr_da[seed] <- ifelse(res_seed$lower_ci[res_seed$type == "atr"] < da_seed$da_atr_dif 
                                   & res_seed$upper_ci[res_seed$type == "atr"] > da_seed$da_atr_dif, 1, 0)
  
}

da_results_comparison <- data.frame(type = type,
                                    bias = c(mean(bias_atrt_da, na.rm=TRUE), mean(bias_atnrt_da, na.rm = TRUE), mean(bias_atr_da, na.rm = TRUE)),
                                    coverage = c(mean(coverage_atrt_da, na.rm = TRUE), mean(coverage_atnrt_da, na.rm = TRUE), mean(coverage_atr_da, na.rm = TRUE)))
