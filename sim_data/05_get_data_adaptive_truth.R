library(drotr)
library(data.table)

here::i_am("05_get_data_adaptive_truth.R")

library(drotr)

# get seed to pull corresponding CATE models
seed <- Sys.getenv("SLURM_ARRAY_TASK_ID")

# sample size to generate big dataset for simulation
n <- 1e5

# Simulate large dataset setting seed 12345 so it's the same big dataset for every seed
# (was originally just loading it but thought there may be issues with factors/formatting)
source("00_simulate_data.R")
set.seed(12345)
abcd_data_sim <- generate_abcd(n)

# load very large abcd_data_sim simulation dataset for computing truth
#abcd_data_sim <- fread('/projects/dbenkes/allison/drotr_sim/journal/abcd_data_sim_1e5_seed_12345.csv')
# abcd_data_sim <- data.frame(abcd_data_sim)

# predict function for avgSuperLearner, not sure why it wasn't finding it in drotr utils.R?? just copied over
predict.avgSuperLearner <- function(x, newdata, ...){
  V <- length(x)
  pred_list <- lapply(x, predict, newdata = newdata)
  pred_list_sl <- lapply(pred_list, "[[", 1)
  avg_pred <- as.numeric(
    Reduce("+", pred_list_sl) / V
  )
  return(avg_pred)
}

# ---- Gold Standard ----

# Z_list for gold standard CATE
Z_list_gs <- c("rotavirus_new", "rotavirus_bin", "norovirus_new", "norovirus_bin", "adenovirus_new",
                "adenovirus_bin", "sapovirus_new","sapovirus_bin", "astrovirus_new", "astrovirus_bin",
                "st_etec_new", "st_etec_bin", "shigella_new", "shigella_bin", "campylobacter_new",
                "campylobacter_bin", "tepec_new", "tepec_bin", "v_cholerae_new", "v_cholerae_bin",
                "salmonella_new", "salmonella_bin", "cryptosporidium_new", "cryptosporidium_bin",
                "dy1_scrn_vomitall", "dy1_scrn_lstools", "dy1_scrn_sstools", "dy1_scrn_diardays",
                "dy1_scrn_dehydr", "avemuac", "wfazscore", "lfazscore", "wflzscore",
                "dy1_ant_sex", "agemchild", "an_ses_quintile", "an_tothhlt5")

# dataframe of just columns in gold standard rule in large sim dataset for computing truth
Z_gs <- abcd_data_sim[, Z_list_gs, drop = FALSE]

# thresholds used for decision rule to iterate over 
thresholds <- c(0.05, 0.15, 0.25, 0.35)

# load CATE models for given seed
dir <- "/projects/dbenkes/allison/drotr_sim/journal/results_sim_data/results_object"
results <- readRDS(paste0(dir, "/gold_standard/full_results_n_6692_seed_", seed, ".Rds"))
CATE_models <- results$CATE_models

# empty dataframe to hold truth for each fold
k_truth <- data.frame()

# for each of the k = 10 CATE models:
for(i in 1:length(CATE_models)){
  
  model <- CATE_models[[i]]
  
  # predict using model for a given fold on big dataset
  abcd_data_sim$true_CATE <- stats::predict(model, newdata = Z_gs, type = 'response')
  
  # iterate over each of the five thresholds
  for(t in 1:length(thresholds)){
    threshold <- thresholds[t]

    # check if the given model had non-NA predictions at the given threshold
    k_non_na <- results$results[[t]]$k_non_na
    if(!(i %in% k_non_na)) next;
    
    # get decisions for the kth model using threshold t
    abcd_data_sim$dZ_gs <- ifelse(abcd_data_sim$true_CATE > threshold, 1, 0)
    
    # get dataframe of just treated people
    abcd_data_sim_treated_gs <- abcd_data_sim[which(abcd_data_sim$dZ_gs == 1),]
    
    # get dataframe of just untreated people
    abcd_data_sim_untreated_gs <- abcd_data_sim[which(abcd_data_sim$dZ_gs == 0),]
    
    # E[Y(1) | d(Z) = 1] 
    EY_A1_dZ1_gs <- mean(abcd_data_sim_treated_gs$lazd90[abcd_data_sim_treated_gs$an_grp_01 == 1], na.rm=TRUE) #na.rm = true for extreme cases when everyone or nobody treated
    
    # E[Y(0) | d(Z) = 1] 
    EY_A0_dZ1_gs <- mean(abcd_data_sim_treated_gs$lazd90[abcd_data_sim_treated_gs$an_grp_01 == 0], na.rm=TRUE)
    
    # E[Y(1) | d(Z) = 0] 
    EY_A1_dZ0_gs <- mean(abcd_data_sim_untreated_gs$lazd90[abcd_data_sim_untreated_gs$an_grp_01 == 1], na.rm=TRUE)
    
    # E[Y(0) | d(Z) = 0] 
    EY_A0_dZ0_gs <- mean(abcd_data_sim_untreated_gs$lazd90[abcd_data_sim_untreated_gs$an_grp_01 == 0], na.rm=TRUE)
    
    # E[d(Z) = 1] 
    E_dZ1_gs <- mean(abcd_data_sim$dZ_gs)
    
    # E[Y(1) - Y(0) | d(Z) = 1] 
    subgroup_effect_treated_gs <- EY_A1_dZ1_gs - EY_A0_dZ1_gs
    
    # E[Y(1) - Y(0) | d(Z) = 1] 
    subgroup_effect_untreated_gs <- EY_A1_dZ0_gs - EY_A0_dZ0_gs
    
    # E[Y(d) - Y(0)] = E[Y(d) - Y(0) | d(Z) = 1] * E[d(Z) = 1] 
    treatment_effect_gs <- (EY_A1_dZ1_gs - EY_A0_dZ1_gs)*E_dZ1_gs
    
    # E[Y(1) - Y(0) | d(Z) = 1] - E[Y(1) - Y(0) | d(Z) = 0] 
    subgroup_difference_gs <- subgroup_effect_treated_gs - subgroup_effect_untreated_gs
    
    # rbind truths for given k-th model t-th threshold combo into dataframe
    k_truth <- rbind(k_truth, data.frame(
      threshold = threshold,
      EY_A1_dZ1_gs = EY_A1_dZ1_gs,
      EY_A0_dZ1_gs = EY_A0_dZ1_gs,
      E_dZ1_gs = E_dZ1_gs,
      subgroup_effect_treated_gs = subgroup_effect_treated_gs,
      subgroup_effect_untreated_gs = subgroup_effect_untreated_gs,
      treatment_effect_gs = treatment_effect_gs,
      subgroup_difference_gs = subgroup_difference_gs
    ))
  }
}

# once we've iterated over all the model + threshold combos, get average for each threshold over the ten folds

# dataframe to hold average across folds
mean_over_folds <- data.frame()

# vector to hold the SE we added for testing
se_E_dZ1_gs <- vector(mode = "numeric", length = length(thresholds))

# for each threshold, average the results
for(i in 1:length(thresholds)){
  t <- thresholds[i]
  
  # subset to k=10 results associated with threshold t
  threshold_df <- k_truth[k_truth$threshold == t, ]
  
  # take the average of each column
  mean_over_folds <- rbind(mean_over_folds, colMeans(threshold_df, na.rm=TRUE))
  
  # get standard error to build additional CI for E(d(Z) == 1)
  se_E_dZ1_gs[i] <- sqrt(mean(threshold_df$E_dZ1_gs * (1 - threshold_df$E_dZ1_gs)) / 6692)
}

# add names back to columns, colMeans got rid of names
colnames(mean_over_folds) <- colnames(k_truth)

# add in new SE column
mean_over_folds <- cbind(mean_over_folds, se_E_dZ1_gs)

# this is silly idk why i didn't just add seed to existing mean_over_folds but oh well 
truth_by_seed <- data.frame(
  seed = seed,
  threshold = mean_over_folds['threshold'],
  EY_A1_dZ1_gs = mean_over_folds['EY_A1_dZ1_gs'],
  EY_A0_dZ1_gs = mean_over_folds['EY_A0_dZ1_gs'],
  E_dZ1_gs = mean_over_folds['E_dZ1_gs'],
  E_dZ1_gs_se = mean_over_folds['se_E_dZ1_gs'],
  subgroup_effect_treated_gs = mean_over_folds['subgroup_effect_treated_gs'],
  subgroup_effect_untreated_gs = mean_over_folds['subgroup_effect_untreated_gs'],
  treatment_effect_gs =  mean_over_folds['treatment_effect_gs'],
  subgroup_difference_gs = mean_over_folds['subgroup_difference_gs']
)

# save overall dataframe with truths for given seed
write.csv(truth_by_seed, file=paste0("results_csv/gs_data_adaptive_truth_seed_",seed,".csv"), row.names=FALSE)

# --- Host ---

# same thing which is redundant and should have been a function

# Z_list for host CATE
Z_list_host <- c("avemuac", "wfazscore", "wflzscore", 
                 "lfazscore", "dy1_ant_sex", 
                 "agemchild", "an_ses_quintile", "an_tothhlt5")

# dataframe of just Z in large sim dataset for computing truth
Z_host <- abcd_data_sim[, Z_list_host, drop = FALSE]

# threshold for decision
thresholds <- c(0.025, 0.05, 0.075, 0.10)

dir <- "/projects/dbenkes/allison/drotr_sim/journal/results_sim_data/results_object"

results <- readRDS(paste0(dir, "/host/full_results_n_6692_seed_", seed, ".Rds"))
CATE_models <- results$CATE_models

# empty dataframe to hold truth for each fold
k_truth <- data.frame()

# for each of the k = 10 CATE models:
for(i in 1:length(CATE_models)){
  
  model <- CATE_models[[i]]
  
  # predict using model for a given fold on big dataset
  abcd_data_sim$true_CATE_host <- stats::predict(model, newdata = Z_host, type = 'response')
  
  # iterate over each of the five thresholds
  for(t in 1:length(thresholds)){
    
    threshold <- thresholds[t]
    
    # check if the given model had non-NA predictions at the given threshold
    k_non_na <- results$results[[t]]$k_non_na
    if(!(i %in% k_non_na)) next;
    
    # get decisions for the kth model using threshold t
    abcd_data_sim$dZ_host <- ifelse(abcd_data_sim$true_CATE_host > threshold, 1, 0)
    
    # get dataframe of just treated people
    abcd_data_sim_treated_host <- abcd_data_sim[which(abcd_data_sim$dZ_host == 1),]
    
    # get dataframe of just untreated people
    abcd_data_sim_untreated_host <- abcd_data_sim[which(abcd_data_sim$dZ_host == 0),]
    
    # E[Y(1) | d(Z) = 1] 
    EY_A1_dZ1_host <- mean(abcd_data_sim_treated_host$lazd90[abcd_data_sim_treated_host$an_grp_01 == 1], na.rm=TRUE)
    
    # E[Y(0) | d(Z) = 1] 
    EY_A0_dZ1_host <- mean(abcd_data_sim_treated_host$lazd90[abcd_data_sim_treated_host$an_grp_01 == 0], na.rm=TRUE)
    
    # E[Y(1) | d(Z) = 0] 
    EY_A1_dZ0_host <- mean(abcd_data_sim_untreated_host$lazd90[abcd_data_sim_untreated_host$an_grp_01 == 1], na.rm=TRUE)
    
    # E[Y(0) | d(Z) = 0] 
    EY_A0_dZ0_host <- mean(abcd_data_sim_untreated_host$lazd90[abcd_data_sim_untreated_host$an_grp_01 == 0], na.rm=TRUE)
    
    # E[d(Z) = 1] 
    E_dZ1_host <- mean(abcd_data_sim$dZ_host)
    
    # E[Y(1) - Y(0) | d(Z) = 1] 
    subgroup_effect_treated_host <- EY_A1_dZ1_host - EY_A0_dZ1_host
    
    # E[Y(1) - Y(0) | d(Z) = 1] 
    subgroup_effect_untreated_host <- EY_A1_dZ0_host - EY_A0_dZ0_host
    
    # E[Y(d) - Y(0)] = E[Y(d) - Y(0) | d(Z) = 1] * E[d(Z) = 1] 
    treatment_effect_host <- (EY_A1_dZ1_host - EY_A0_dZ1_host)*E_dZ1_host
    
    # E[Y(1) - Y(0) | d(Z) = 1] - E[Y(1) - Y(0) | d(Z) = 0] 
    subgroup_difference_host <- subgroup_effect_treated_host - subgroup_effect_untreated_host
    
    k_truth <- rbind(k_truth, data.frame(
      threshold = threshold,
      EY_A1_dZ1_host = EY_A1_dZ1_host,
      EY_A0_dZ1_host = EY_A0_dZ1_host,
      E_dZ1_host = E_dZ1_host,
      subgroup_effect_treated_host = subgroup_effect_treated_host,
      subgroup_effect_untreated_host = subgroup_effect_untreated_host,
      treatment_effect_host = treatment_effect_host,
      subgroup_difference_host = subgroup_difference_host
    ))
  }
}

#get average for each threshold over the ten folds
mean_over_folds <- data.frame()
se_E_dZ1_host <- vector(mode = "numeric", length = length(thresholds))

for(i in 1:length(thresholds)){
  t <- thresholds[i]
  threshold_df <- k_truth[k_truth$threshold == t, ]
  se_E_dZ1_host[i] <- sqrt(mean(threshold_df$E_dZ1_host * (1 - threshold_df$E_dZ1_host)) / 6692) #divide by sample size from simulations
  mean_over_folds <- rbind(mean_over_folds, colMeans(k_truth[k_truth$threshold == t, ], na.rm=TRUE))
}

colnames(mean_over_folds) <- colnames(k_truth)
mean_over_folds <- cbind(mean_over_folds, se_E_dZ1_host)

truth_by_seed_host <- data.frame(
  seed = seed,
  threshold = mean_over_folds['threshold'],
  EY_A1_dZ1_host = mean_over_folds['EY_A1_dZ1_host'],
  EY_A0_dZ1_host = mean_over_folds['EY_A0_dZ1_host'],
  E_dZ1_host = mean_over_folds['E_dZ1_host'],
  E_dZ1_host_se = mean_over_folds['se_E_dZ1_host'],
  subgroup_effect_treated_host = mean_over_folds['subgroup_effect_treated_host'],
  subgroup_effect_untreated_host = mean_over_folds['subgroup_effect_untreated_host'],
  treatment_effect_host =  mean_over_folds['treatment_effect_host'],
  subgroup_difference_host = mean_over_folds['subgroup_difference_host']
)


# save overall dataframe with truths for each seed
write.csv(truth_by_seed_host, file=paste0("results_csv/host_data_adaptive_truth_seed_",seed,".csv"), row.names=FALSE)
