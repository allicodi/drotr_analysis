
here::i_am("histogram.R")

# read in data adaptive truth by seed file
gs_data_adaptive_truth_df <- read.csv("truth/gs_data_adaptive_truth_by_seed.csv")
host_data_adaptive_truth_df <- read.csv("truth/host_data_adaptive_truth_by_seed.csv")

# read simulation results
gs_results_df <- read.csv("results_csv/gold_standard_n_6692.csv")
host_results_df <- read.csv("results_csv/host_n_6692.csv")

threshold_list <- c(0.15, 0.2, 0.25, 0.3, 0.35)

for(t in threshold_list){
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
  
  gs_sim_and_truth$pt_est_minus_da_truth <- gs_sim_and_truth$E_dZ1 - gs_sim_and_truth$true_E_dZ1_gs
  host_sim_and_truth$pt_est_minus_da_truth <- host_sim_and_truth$E_dZ1 - host_sim_and_truth$true_E_dZ1_host
  
  hist(gs_sim_and_truth$pt_est_minus_da_truth, 
       main = paste0("E[d(Z) = 1] point estimate - data adaptive truth \n threshold = ", t, ", gold standard rule"),
       xlab = "Point estimate - data adaptive truth")
  
  hist(host_sim_and_truth$pt_est_minus_da_truth, 
       main = paste0("E[d(Z) = 1] point estimate - data adaptive truth \n threshold = ", t, ", host rule"),
       xlab = "Point estimate - data adaptive truth")
  
}

# average CI width - data adaptive
avg_se_gs_.15_da <- 1.96*mean(gs_data_adaptive_truth_df$se_E_dZ1_gs[gs_data_adaptive_truth_df$threshold == 0.15])
avg_se_gs_.2_da <- 1.96*mean(gs_data_adaptive_truth_df$se_E_dZ1_gs[gs_data_adaptive_truth_df$threshold == 0.2])
avg_se_gs_.25_da <- 1.96*mean(gs_data_adaptive_truth_df$se_E_dZ1_gs[gs_data_adaptive_truth_df$threshold == 0.25])
avg_se_gs_.3_da <- 1.96*mean(gs_data_adaptive_truth_df$se_E_dZ1_gs[gs_data_adaptive_truth_df$threshold == 0.3], na.rm = TRUE)
avg_se_gs_.35_da <- 1.96*mean(gs_data_adaptive_truth_df$se_E_dZ1_gs[gs_data_adaptive_truth_df$threshold == 0.35], na.rm = TRUE)

avg_se_host_.15_da <- 1.96*mean(host_data_adaptive_truth_df$se_E_dZ1_host[host_data_adaptive_truth_df$threshold == 0.15])
avg_se_host_.2_da <- 1.96*mean(host_data_adaptive_truth_df$se_E_dZ1_host[host_data_adaptive_truth_df$threshold == 0.2])
avg_se_host_.25_da <- 1.96*mean(host_data_adaptive_truth_df$se_E_dZ1_host[host_data_adaptive_truth_df$threshold == 0.25])
avg_se_host_.3_da <- 1.96*mean(host_data_adaptive_truth_df$se_E_dZ1_host[host_data_adaptive_truth_df$threshold == 0.3])
avg_se_host_.35_da <- 1.96*mean(host_data_adaptive_truth_df$se_E_dZ1_host[host_data_adaptive_truth_df$threshold == 0.35])

# average CI width - non data adaptive
avg_se_gs_.15 <- 1.96*mean(gs_results_df$se_E_dZ1[gs_results_df$threshold == 0.15])
avg_se_gs_.2 <- 1.96*mean(gs_results_df$se_E_dZ1[gs_results_df$threshold == 0.2])
avg_se_gs_.25 <- 1.96*mean(gs_results_df$se_E_dZ1[gs_results_df$threshold == 0.25])
avg_se_gs_.3 <- 1.96*mean(gs_results_df$se_E_dZ1[gs_results_df$threshold == 0.3])
avg_se_gs_.35 <- 1.96*mean(gs_results_df$se_E_dZ1[gs_results_df$threshold == 0.35])

avg_se_host_.15 <- 1.96*mean(host_results_df$se_E_dZ1[host_results_df$threshold == 0.15], na.rm = TRUE)
avg_se_host_.2 <- 1.96*mean(host_results_df$se_E_dZ1[host_results_df$threshold == 0.2], na.rm = TRUE)
avg_se_host_.25 <- 1.96*mean(host_results_df$se_E_dZ1[host_results_df$threshold == 0.25], na.rm = TRUE)
avg_se_host_.3 <- 1.96*mean(host_results_df$se_E_dZ1[host_results_df$threshold == 0.3], na.rm = TRUE)
avg_se_host_.35 <- 1.96*mean(host_results_df$se_E_dZ1[host_results_df$threshold == 0.35], na.rm = TRUE)
