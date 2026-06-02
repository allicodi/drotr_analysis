# ------------------------------------------------------------------------------
# Script to run simulation for given seed under simple and complex CATE settings
# -------------------------------------------------------------------------------

options(echo = TRUE)

here::i_am("debug_gs_script.R")

library(drotr)

source("00_simulate_data.R")
source("01_wrappers.R")

# ------------------------------  Set seed ---------------------------------

seed <- Sys.getenv("SLURM_ARRAY_TASK_ID")
set.seed(seed)

# size of ABCD dataset
n <- 6692

# ---------------- Data Generation & Model Specification -------------------

# Generate Simulation Data
abcd_data <- generate_abcd(n)

# Nuisance Model Specification

# Outcome models from analysis plan (excluding SL.glmnet.twoway for computational efficiency)
sl.library.outcome <- c("SL.glm", "SL.ranger", "SL.earth", "SL.glmnet",
                        "SL.xgboost", "SL.outcome.1", "SL.outcome.2", "SL.outcome.3",
                        "SL.outcome.11", "SL.outcome.4", "SL.outcome.5", "SL.outcome.6",
                        "SL.outcome.7", "SL.outcome.8", "SL.outcome.9", "SL.outcome.10")

sl.library.treatment <- c("SL.mean", "SL.treatment")

sl.library.missingness <- c("SL.mean", "SL.missing.1", "SL.missing.2")

W_list <- c("rotavirus_new", "rotavirus_bin", "norovirus_new", "norovirus_bin", "adenovirus_new",
            "adenovirus_bin", "sapovirus_new","sapovirus_bin", "astrovirus_new", "astrovirus_bin",
            "st_etec_new", "st_etec_bin", "shigella_new", "shigella_bin", "campylobacter_new",
            "campylobacter_bin", "tepec_new", "tepec_bin", "v_cholerae_new", "v_cholerae_bin",
            "salmonella_new", "salmonella_bin", "cryptosporidium_new", "cryptosporidium_bin",
            "dy1_scrn_vomitall", "dy1_scrn_lstools", "dy1_scrn_sstools", "dy1_scrn_diardays",
            "dy1_scrn_dehydr", "avemuac", "wfazscore", "lfazscore", "wflzscore", "site",
            "dy1_ant_sex", "agemchild", "an_ses_quintile", "an_tothhlt5", "month_en", "rotaseason")

# -------------------  CATE Model Specification ---------------------

# Uses all available data and is considered the best-informed rule 
# no month, site, rotaseason
Z_list_gold_standard <- c("rotavirus_new", "rotavirus_bin", "norovirus_new", "norovirus_bin", "adenovirus_new",
                          "adenovirus_bin", "sapovirus_new","sapovirus_bin", "astrovirus_new", "astrovirus_bin",
                          "st_etec_new", "st_etec_bin", "shigella_new", "shigella_bin", "campylobacter_new",
                          "campylobacter_bin", "tepec_new", "tepec_bin", "v_cholerae_new", "v_cholerae_bin",
                          "salmonella_new", "salmonella_bin", "cryptosporidium_new", "cryptosporidium_bin",
                          "dy1_scrn_vomitall", "dy1_scrn_lstools", "dy1_scrn_sstools", "dy1_scrn_diardays",
                          "dy1_scrn_dehydr", "avemuac", "wfazscore", "lfazscore", "wflzscore", 
                          "dy1_ant_sex", "agemchild", "an_ses_quintile", "an_tothhlt5")

# ML libraries to try to capture the three-way interaction
CATE_library_gold_standard <- c("SL.earth",
                                "SL.ranger",
                                "SL.glmnet",
                                "SL.glm")

# threshold_list <- c(0.05, 0.15, 0.25, 0.35)
threshold_list <- c(0.05)

# ------------------------- Fit nuisance model and save -------------------------

# nuisance_output <- readRDS(file = paste0("/projects/dbenkes/allison/drotr_sim/journal/results_sim_data/nuisance/nuisance_n_", n, "_seed_", seed, ".Rds"))

nuisance_output <- learn_nuisance(df = abcd_data,
                                  id_name = "pid",
                                  Y_name = "lazd90",
                                  A_name = "an_grp_01",
                                  W_list = W_list,
                                  sl.library.outcome = sl.library.outcome,
                                  sl.library.treatment = sl.library.treatment,
                                  sl.library.missingness = sl.library.missingness,
                                  outcome_type = "gaussian",
                                  k_folds = 10,
                                  ps_trunc_level = 0.01)

nuisance_models <- nuisance_output$nuisance_models
k_fold_assign_and_CATE <- nuisance_output$k_fold_assign_and_CATE
validRows <- nuisance_output$validRows

if(seed %in% 1:10){
  saveRDS(nuisance_output, file = paste0("/projects/dbenkes/allison/drotr_sim/journal/results_sim_data/nuisance/debug_nuisance_n_", n, "_seed_", seed, ".Rds"))
}

# ------------------------- Rule based on all-information ----------------------------

results_gold_standard <- estimate_OTR(df = abcd_data,
                                      Y_name = "lazd90",
                                      A_name = "an_grp_01",
                                      Z_list = Z_list_gold_standard,
                                      W_list= W_list,
                                      id_name = "pid",
                                      sl.library.CATE = CATE_library_gold_standard,
                                      nuisance_models = nuisance_models,
                                      k_fold_assign_and_CATE = k_fold_assign_and_CATE,
                                      validRows = validRows,
                                      threshold = threshold_list,
                                      k_folds = 10,
                                      ps_trunc_level = 0.01,
                                      outcome_type = "gaussian")

print(results_gold_standard)

if(seed %in% 1:10){
  saveRDS(results_gold_standard, file = paste0("/projects/dbenkes/allison/drotr_sim/journal/results_sim_data/results_object/gold_standard/debug_full_results_n_",n,"_seed_", seed, ".Rds"))
}
saveRDS(results_gold_standard$results, file = paste0("/projects/dbenkes/allison/drotr_sim/journal/results_sim_data/results_object/gold_standard/debug_results_n_",n,"_seed_", seed, ".Rds"))

## Data adaptive step to predict on large dataset ------------------------------

set.seed(12345)

abcd_data_sim <- generate_abcd(n)

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

CATE_models <- results_gold_standard$CATE_models

# empty dataframe to hold truth for each fold
k_truth <- data.frame()

thresholds <- c(0.05)

# for each of the k = 10 CATE models:
for(i in 1:length(CATE_models)){
  
  model <- CATE_models[[i]]
  
  # predict using model for a given fold on big dataset
  abcd_data_sim$true_CATE <- stats::predict(model, newdata = Z_gs, type = 'response')
  
  # iterate over each of the five thresholds
  for(t in 1:length(thresholds)){
    threshold <- thresholds[t]
    
    # check if the given model had non-NA predictions at the given threshold
    k_non_na <- results_gold_standard$results[[t]]$k_non_na
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
write.csv(truth_by_seed, file=paste0("results_csv/debug_gs_data_adaptive_truth_seed_",seed,".csv"), row.names=FALSE)

