# ------------------------------------------------------------------------------
# Script to run simulation for given seed under host CATE setting
# -------------------------------------------------------------------------------

here::i_am("02.2_run_simulation_host.R")

library(drotr)

source("00_simulate_data.R")
source("01_wrappers.R")

# ------------------------------  Set seed ---------------------------------

seed <- Sys.getenv("SLURM_ARRAY_TASK_ID")
seed_num <- as.numeric(seed)
set.seed(seed_num)

# size of ABCD dataset
n <- 6692

# outer cross validation folds, default 10
k_folds <- 10

# ---------------- Data Generation & Model Specification -------------------

abcd_data <- generate_abcd(n)

# Nuisance Model Specification

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

Z_list_host <- c("avemuac", "wfazscore", "wflzscore", 
                 "lfazscore", "dy1_ant_sex", 
                 "agemchild", "an_ses_quintile", "an_tothhlt5")

CATE_library_host <- c("SL.earth",
                       "SL.ranger",
                       "SL.glmnet",
                       "SL.glm")

threshold_list <- c(0.025, 0.05, 0.075, 0.10)

# ------------------------- Fit nuisance model and save -------------------------

nuisance_file <- paste0(
  "/projects/dbenkes/allison/drotr_sim/journal/results_sim_data/nuisance/",
  "nuisance_n_", n, "_seed_", seed, ".Rds"
)

if (file.exists(nuisance_file)) {
  
  nuisance_output <- readRDS(nuisance_file)
  
} else {
  
  nuisance_output <- learn_nuisance(
    df = abcd_data,
    id_name = "pid",
    Y_name = "lazd90",
    A_name = "an_grp_01",
    W_list = W_list,
    sl.library.outcome = sl.library.outcome,
    sl.library.treatment = sl.library.treatment,
    sl.library.missingness = sl.library.missingness,
    outcome_type = "gaussian",
    k_folds = k_folds,
    ps_trunc_level = 0.01
  )
  
  # only save nuisance models for the first 10 seeds
  if (seed_num <= 10) {
    saveRDS(nuisance_output, file = nuisance_file)
  }
}

nuisance_models <- nuisance_output$nuisance_models
k_fold_assign_and_CATE <- nuisance_output$k_fold_assign_and_CATE
validRows <- nuisance_output$validRows

# ------------------------- Rule based on host characteristics ----------------------------

results_host <- estimate_OTR(
  df = abcd_data,
  Y_name = "lazd90",
  A_name = "an_grp_01",
  Z_list = Z_list_host,
  W_list = W_list,
  id_name = "pid",
  sl.library.CATE = CATE_library_host,
  nuisance_models = nuisance_models,
  k_fold_assign_and_CATE = k_fold_assign_and_CATE,
  validRows = validRows,
  threshold = threshold_list,
  k_folds = k_folds,
  ps_trunc_level = 0.01,
  outcome_type = "gaussian"
)

print(results_host)

# only save full CATE model object for the first 10 seeds
if (seed_num <= 10) {
  saveRDS(
    results_host,
    file = paste0(
      "/projects/dbenkes/allison/drotr_sim/journal/results_sim_data/results_object/host/",
      "full_results_n_", n, "_seed_", seed, ".Rds"
    )
  )
}

# save results portion for all seeds
saveRDS(
  results_host$results,
  file = paste0(
    "/projects/dbenkes/allison/drotr_sim/journal/results_sim_data/results_object/host/",
    "results_n_", n, "_seed_", seed, ".Rds"
  )
)

# ------------------------- Data adaptive truth generation ---------------------

set.seed(12345)
abcd_data_sim <- generate_abcd(n = 1e5, potential_outcomes = TRUE)

# predict function for avgSuperLearner
predict.avgSuperLearner <- function(x, newdata, ...){
  V <- length(x)
  pred_list <- lapply(x, predict, newdata = newdata)
  pred_list_sl <- lapply(pred_list, "[[", 1)
  avg_pred <- as.numeric(
    Reduce("+", pred_list_sl) / V
  )
  return(avg_pred)
}

# ---- Host ----

Z_host <- abcd_data_sim[, Z_list_host, drop = FALSE]

CATE_models <- results_host$CATE_models

k_truth <- data.frame()

thresholds <- threshold_list

for(i in 1:length(CATE_models)){
  
  model <- CATE_models[[i]]
  
  abcd_data_sim$pred_CATE <- stats::predict(
    model,
    newdata = Z_host,
    type = "response"
  )
  
  for(t in 1:length(thresholds)){
    
    threshold <- thresholds[t]
    
    k_non_na <- results_host$results[[t]]$k_non_na
    if(!(i %in% k_non_na)) next
    
    abcd_data_sim$dZ_host <- ifelse(abcd_data_sim$pred_CATE > threshold, 1, 0)
    
    abcd_data_sim_treated_host <- abcd_data_sim[abcd_data_sim$dZ_host == 1, ]
    abcd_data_sim_untreated_host <- abcd_data_sim[abcd_data_sim$dZ_host == 0, ]
    
    EY_A1_dZ1_host <- mean(abcd_data_sim_treated_host$lazd90_mu1, na.rm = TRUE)
    EY_A0_dZ1_host <- mean(abcd_data_sim_treated_host$lazd90_mu0, na.rm = TRUE)
    
    EY_A1_dZ0_host <- mean(abcd_data_sim_untreated_host$lazd90_mu1, na.rm = TRUE)
    EY_A0_dZ0_host <- mean(abcd_data_sim_untreated_host$lazd90_mu0, na.rm = TRUE)
    
    E_dZ1_host <- mean(abcd_data_sim$dZ_host)
    
    subgroup_effect_treated_host <- EY_A1_dZ1_host - EY_A0_dZ1_host
    subgroup_effect_untreated_host <- EY_A1_dZ0_host - EY_A0_dZ0_host
    
    treatment_effect_host <- subgroup_effect_treated_host * E_dZ1_host
    
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

mean_over_folds <- data.frame()

se_E_dZ1_host <- vector(mode = "numeric", length = length(thresholds))

for(i in 1:length(thresholds)){
  
  threshold <- thresholds[i]
  
  threshold_df <- k_truth[k_truth$threshold == threshold, ]
  
  mean_over_folds <- rbind(
    mean_over_folds,
    colMeans(threshold_df, na.rm = TRUE)
  )
  
  se_E_dZ1_host[i] <- sqrt(
    mean(threshold_df$E_dZ1_host * (1 - threshold_df$E_dZ1_host)) / n
  )
}

colnames(mean_over_folds) <- colnames(k_truth)

mean_over_folds <- cbind(mean_over_folds, se_E_dZ1_host)

truth_by_seed <- data.frame(
  seed = seed_num,
  folds = k_folds,
  threshold = mean_over_folds["threshold"],
  EY_A1_dZ1_host = mean_over_folds["EY_A1_dZ1_host"],
  EY_A0_dZ1_host = mean_over_folds["EY_A0_dZ1_host"],
  E_dZ1_host = mean_over_folds["E_dZ1_host"],
  E_dZ1_host_se = mean_over_folds["se_E_dZ1_host"],
  subgroup_effect_treated_host = mean_over_folds["subgroup_effect_treated_host"],
  subgroup_effect_untreated_host = mean_over_folds["subgroup_effect_untreated_host"],
  treatment_effect_host = mean_over_folds["treatment_effect_host"],
  subgroup_difference_host = mean_over_folds["subgroup_difference_host"]
)

write.csv(
  truth_by_seed,
  file = paste0(
    "results_csv/host_data_adaptive_truth_n_",
    n,
    "_folds_",
    k_folds,
    "_seed_",
    seed,
    ".csv"
  ),
  row.names = FALSE
)
