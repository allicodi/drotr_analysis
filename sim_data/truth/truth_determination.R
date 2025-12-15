here::i_am("sim_data/truth/truth_determination.R")
library(tidyverse)

source("00_simulate_data.R")

truth_df_gs <- data.frame()
truth_df_host <- data.frame()

for(seed in 1:5){
  
  # set seed, repeat 5 times and average results
  set.seed(seed)
  
  # very large abcd dataset
  n <- 1e6
  abcd_data <- generate_abcd(n)
  
  # True CATE GS = E[Qbar(1, W) - Qbar(0, W) | gold-standard Z]
  # pathogen quantity * (beta_pathogen_x_treatment + 
  #                      beta_pathogen_x_treatment_x_lfazscore * lfazscore +
  #                      beta_pathogen_x_treatment_x_lfazscore_x_age * lfazscore * age)
  
  # True CATE Host = E[Qbar(1, W) - Qbar(0, W) | host Z]
  # E[pathogen quantity] * (beta_pathogen_x_treatment + 
  #                      beta_pathogen_x_treatment_x_lfazscore * lfazscore +
  #                      beta_pathogen_x_treatment_x_lfazscore_x_age * lfazscore * age)
  
  true_cate_gs <- 0.1940622*abcd_data$shigella_bin +
    -0.007401625*abcd_data$shigella_bin*abcd_data$lfazscore +
    -0.0025*abcd_data$shigella_bin*abcd_data$agemchild*abcd_data$lfazscore
  
  true_cate_host <- 0.1940622*mean(abcd_data$shigella_bin) +
    -0.007401625*mean(abcd_data$shigella_bin)*abcd_data$lfazscore +
    -0.0025*mean(abcd_data$shigella_bin)*abcd_data$agemchild*abcd_data$lfazscore
  
  t_list_gs <- seq(0.05, 0.35, 0.1)
  t_list_host <- seq(0.025, 0.1, 0.025)
  
  get_truth <- function(true_cate, t_list, seed){
    EY_A1_dZ1 <- vector("numeric", length = length(t_list))
    EY_A0_dZ1 <- vector("numeric", length = length(t_list))
    EY_A1_dZ0 <- vector("numeric", length = length(t_list))
    EY_A0_dZ0 <- vector("numeric", length = length(t_list))
    E_dZ1 <- vector("numeric", length = length(t_list))
    subgroup_effect_treated <- vector("numeric", length = length(t_list))
    subgroup_effect_untreated <- vector("numeric", length = length(t_list))
    treatment_effect <- vector("numeric", length = length(t_list))
    subgroup_difference <- vector("numeric", length = length(t_list))
    
    for(i in 1:length(t_list)){
      t <- t_list[i]
      
      d_Z <- ifelse(true_cate >= t, 1, 0)
      
      abcd_data_treated <- abcd_data[which(d_Z == 1),]
      abcd_data_untreated <- abcd_data[which(d_Z == 0),]
      
      # E[Y(1) | d(Z) = 1]
      EY_A1_dZ1[i] <- mean(abcd_data_treated$lazd90[abcd_data_treated$an_grp_01 == 1], na.rm=TRUE)
      
      # E[Y(0) | d(Z) = 1]
      EY_A0_dZ1[i] <- mean(abcd_data_treated$lazd90[abcd_data_treated$an_grp_01 == 0], na.rm=TRUE)
      
      # E[Y(1) | d(Z) = 0]
      EY_A1_dZ0[i] <- mean(abcd_data_untreated$lazd90[abcd_data_untreated$an_grp_01 == 1], na.rm=TRUE)
      
      # E[Y(0) | d(Z) = 0]
      EY_A0_dZ0[i] <- mean(abcd_data_untreated$lazd90[abcd_data_untreated$an_grp_01 == 0], na.rm=TRUE)
      
      # E[d(Z) = 1]
      E_dZ1[i] <- mean(d_Z)
      
      # E[Y(1) - Y(0) | d(Z) = 1]
      subgroup_effect_treated[i] <- EY_A1_dZ1[i] - EY_A0_dZ1[i]
      
      # E[Y(1) - Y(0) | d(Z) = 0]   
      subgroup_effect_untreated[i] <- EY_A1_dZ0[i] - EY_A0_dZ0[i]
      
      # E[Y(d) - Y(0)] = E[Y(d) - Y(0) | d(Z) = 1] * E[d(Z) = 1]
      treatment_effect[i] <- (EY_A1_dZ1[i] - EY_A0_dZ1[i])*E_dZ1[i]
      
      # E[Y(1) - Y(0) | d(Z) = 1] - E[Y(1) - Y(0) | d(Z) = 0]   
      subgroup_difference[i]<- subgroup_effect_treated[i] - subgroup_effect_untreated[i]
      
    }
    
    truth_df <- cbind(seed,
                      t_list, 
                      EY_A1_dZ1,
                      EY_A0_dZ1,
                      EY_A1_dZ0,
                      EY_A0_dZ0,
                      E_dZ1,
                      subgroup_effect_treated,
                      subgroup_effect_untreated,
                      treatment_effect,
                      subgroup_difference)
    
    return(truth_df)
    
  }
  
    truth_df_gs <- rbind(truth_df_gs, get_truth(true_cate = true_cate_gs, 
                                                t_list = t_list_gs, 
                                                seed = seed))
    truth_df_host <- rbind(truth_df_host, get_truth(true_cate = true_cate_host, 
                                                    t_list = t_list_host, 
                                                    seed = seed))
}
  
# truth dataframe for each seed
write.csv(truth_df_gs, "truth/truth_df_gs.csv")
write.csv(truth_df_host, "truth/truth_df_host.csv")

# average for gold standard
average_truth_gs <- truth_df_gs %>%
  group_by(t_list) %>%
  summarise(across(everything(), mean, na.rm=TRUE))

average_truth_gs <- average_truth_gs[,!(names(average_truth_gs) %in% c("seed"))]

write.csv(average_truth_gs, "truth/average_truth_gs.csv")

# average for host
average_truth_host <- truth_df_host %>%
  group_by(t_list) %>%
  summarise(across(everything(), mean, na.rm=TRUE))

average_truth_host <- average_truth_host[,!(names(average_truth_host) %in% c("seed"))]

write.csv(average_truth_host, "truth/average_truth_host.csv")

# -------------------

# truth for comparison at threshold 0.05

truth_df_gs_0.05 <- truth_df_gs[truth_df_gs$t_list == 0.05,]
truth_df_host_0.05 <- truth_df_host[truth_df_host$t_list == 0.05,]

true_dif_atrt <- vector("numeric", 5)
true_dif_atnrt <- vector("numeric", 5)
true_dif_atr <- vector("numeric", 5)

# seems pretty variable
for(seed in 1:5){
  true_dif_atrt[seed] <- truth_df_gs_0.05$subgroup_effect_treated[seed] - truth_df_host_0.05$subgroup_effect_treated[seed]
  true_dif_atnrt[seed] <- truth_df_gs_0.05$subgroup_effect_untreated[seed] - truth_df_host_0.05$subgroup_effect_untreated[seed]
  true_dif_atr[seed] <- truth_df_gs_0.05$treatment_effect[seed] - truth_df_host_0.05$treatment_effect[seed]
}

true_dif_df <- data.frame(seed = 1:5,
                           threshold = 0.05,
                           true_dif_atrt = true_dif_atrt,
                           true_dif_atnrt = true_dif_atnrt,
                           true_dif_atr = true_dif_atr)

average_dif <- true_dif_df %>%
  summarise(across(everything(), mean, na.rm=TRUE))

write.csv(true_dif_df, "truth/truth_df_dif_0.05.csv")
write.csv(average_dif, "truth/truth_df_avg_dif.csv")  


