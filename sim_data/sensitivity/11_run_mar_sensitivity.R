# ---------------------------------------------------------------
# Script to run sensitivity analysis for MAR- dependent on site
# ---------------------------------------------------------------

here::i_am("sensitivity/11_run_mar_sensitivity.R")

library(drotr)

source("00_simulate_data.R")
source("01_wrappers.R")

# ------------------------------  Set seed ---------------------------------

seed <- Sys.getenv("SLURM_ARRAY_TASK_ID")
seed_num <- as.numeric(seed)
set.seed(seed_num)

n <- 6692
k_folds <- 5

# ---------------- Data Generation & Model Specification -------------------

abcd_data <- generate_abcd(n, mar_sens = TRUE)

sl.library.outcome <- c(
  "SL.glm", "SL.ranger", "SL.earth", "SL.glmnet",
  "SL.xgboost", "SL.outcome.1", "SL.outcome.2", "SL.outcome.3",
  "SL.outcome.11", "SL.outcome.4", "SL.outcome.5", "SL.outcome.6",
  "SL.outcome.7", "SL.outcome.8", "SL.outcome.9", "SL.outcome.10"
)

sl.library.treatment <- c("SL.mean", "SL.treatment")
sl.library.missingness <- c("SL.mean", "SL.missing.1", "SL.missing.2")

W_list <- c(
  "rotavirus_new", "rotavirus_bin", "norovirus_new", "norovirus_bin",
  "adenovirus_new", "adenovirus_bin", "sapovirus_new", "sapovirus_bin",
  "astrovirus_new", "astrovirus_bin", "st_etec_new", "st_etec_bin",
  "shigella_new", "shigella_bin", "campylobacter_new", "campylobacter_bin",
  "tepec_new", "tepec_bin", "v_cholerae_new", "v_cholerae_bin",
  "salmonella_new", "salmonella_bin", "cryptosporidium_new",
  "cryptosporidium_bin", "dy1_scrn_vomitall", "dy1_scrn_lstools",
  "dy1_scrn_sstools", "dy1_scrn_diardays", "dy1_scrn_dehydr",
  "avemuac", "wfazscore", "lfazscore", "wflzscore", "site",
  "dy1_ant_sex", "agemchild", "an_ses_quintile", "an_tothhlt5",
  "month_en", "rotaseason"
)

Z_list_gold_standard <- c(
  "rotavirus_new", "rotavirus_bin", "norovirus_new", "norovirus_bin",
  "adenovirus_new", "adenovirus_bin", "sapovirus_new", "sapovirus_bin",
  "astrovirus_new", "astrovirus_bin", "st_etec_new", "st_etec_bin",
  "shigella_new", "shigella_bin", "campylobacter_new", "campylobacter_bin",
  "tepec_new", "tepec_bin", "v_cholerae_new", "v_cholerae_bin",
  "salmonella_new", "salmonella_bin", "cryptosporidium_new",
  "cryptosporidium_bin", "dy1_scrn_vomitall", "dy1_scrn_lstools",
  "dy1_scrn_sstools", "dy1_scrn_diardays", "dy1_scrn_dehydr",
  "avemuac", "wfazscore", "lfazscore", "wflzscore",
  "dy1_ant_sex", "agemchild", "an_ses_quintile", "an_tothhlt5"
)

CATE_library_gold_standard <- c(
  "SL.earth",
  "SL.ranger",
  "SL.glmnet",
  "SL.glm"
)

threshold_list <- c(0.05)

# ------------------------- Fit nuisance model -------------------------

nuisance_path <- paste0(
  "/projects/dbenkes/allison/drotr_sim/journal/results_sim_data/nuisance/",
  "mar_nuisance_n_", n, "_seed_", seed, ".Rds"
)

if (file.exists(nuisance_path)) {
  
  nuisance_output <- readRDS(nuisance_path)
  
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
  
  # only save heavy nuisance objects for first 10 seeds
  if (seed_num <= 10) {
    saveRDS(nuisance_output, file = nuisance_path)
  }
}

nuisance_models <- nuisance_output$nuisance_models
k_fold_assign_and_CATE <- nuisance_output$k_fold_assign_and_CATE
validRows <- nuisance_output$validRows

# ------------------------- Estimate gold-standard rule -------------------------

results_gold_standard <- estimate_OTR(
  df = abcd_data,
  Y_name = "lazd90",
  A_name = "an_grp_01",
  Z_list = Z_list_gold_standard,
  W_list = W_list,
  id_name = "pid",
  sl.library.CATE = CATE_library_gold_standard,
  nuisance_models = nuisance_models,
  k_fold_assign_and_CATE = k_fold_assign_and_CATE,
  validRows = validRows,
  threshold = threshold_list,
  k_folds = k_folds,
  ps_trunc_level = 0.01,
  outcome_type = "gaussian"
)

print(results_gold_standard)

# only save full object, including CATE models, for first 10 seeds
if (seed_num <= 10) {
  saveRDS(
    results_gold_standard,
    file = paste0(
      "/projects/dbenkes/allison/drotr_sim/journal/results_sim_data/results_object/gold_standard/",
      "mar_full_results_n_", n, "_seed_", seed, ".Rds"
    )
  )
}

# save lightweight results for all seeds
saveRDS(
  results_gold_standard$results,
  file = paste0(
    "/projects/dbenkes/allison/drotr_sim/journal/results_sim_data/results_object/gold_standard/",
    "mar_results_n_", n, "_seed_", seed, ".Rds"
  )
)

# ------------------------- Data adaptive truth generation ---------------------

set.seed(12345)

# can still generate same as original sim
abcd_data_sim <- generate_abcd(
  n = 1e5,
  potential_outcomes = TRUE
)

predict.avgSuperLearner <- function(x, newdata, ...) {
  V <- length(x)
  pred_list <- lapply(x, predict, newdata = newdata)
  pred_list_sl <- lapply(pred_list, "[[", 1)
  avg_pred <- as.numeric(Reduce("+", pred_list_sl) / V)
  return(avg_pred)
}

Z_gs <- abcd_data_sim[, Z_list_gold_standard, drop = FALSE]

CATE_models <- results_gold_standard$CATE_models
thresholds <- threshold_list

k_truth <- data.frame()

for (i in seq_along(CATE_models)) {
  
  model <- CATE_models[[i]]
  
  abcd_data_sim$pred_CATE <- stats::predict(
    model,
    newdata = Z_gs,
    type = "response"
  )
  
  for (t in seq_along(thresholds)) {
    
    threshold <- thresholds[t]
    
    k_non_na <- results_gold_standard$results[[t]]$k_non_na
    if (!(i %in% k_non_na)) next
    
    abcd_data_sim$dZ_gs <- ifelse(abcd_data_sim$pred_CATE > threshold, 1, 0)
    
    treated_gs <- abcd_data_sim[abcd_data_sim$dZ_gs == 1, ]
    untreated_gs <- abcd_data_sim[abcd_data_sim$dZ_gs == 0, ]
    
    EY_A1_dZ1_gs <- mean(treated_gs$lazd90_mu1, na.rm = TRUE)
    EY_A0_dZ1_gs <- mean(treated_gs$lazd90_mu0, na.rm = TRUE)
    
    EY_A1_dZ0_gs <- mean(untreated_gs$lazd90_mu1, na.rm = TRUE)
    EY_A0_dZ0_gs <- mean(untreated_gs$lazd90_mu0, na.rm = TRUE)
    
    E_dZ1_gs <- mean(abcd_data_sim$dZ_gs)
    
    subgroup_effect_treated_gs <- EY_A1_dZ1_gs - EY_A0_dZ1_gs
    subgroup_effect_untreated_gs <- EY_A1_dZ0_gs - EY_A0_dZ0_gs
    
    treatment_effect_gs <- subgroup_effect_treated_gs * E_dZ1_gs
    
    subgroup_difference_gs <- subgroup_effect_treated_gs - subgroup_effect_untreated_gs
    
    k_truth <- rbind(
      k_truth,
      data.frame(
        threshold = threshold,
        EY_A1_dZ1_gs = EY_A1_dZ1_gs,
        EY_A0_dZ1_gs = EY_A0_dZ1_gs,
        E_dZ1_gs = E_dZ1_gs,
        subgroup_effect_treated_gs = subgroup_effect_treated_gs,
        subgroup_effect_untreated_gs = subgroup_effect_untreated_gs,
        treatment_effect_gs = treatment_effect_gs,
        subgroup_difference_gs = subgroup_difference_gs
      )
    )
  }
}

mean_over_folds <- data.frame()
se_E_dZ1_gs <- vector(mode = "numeric", length = length(thresholds))

for (i in seq_along(thresholds)) {
  
  threshold <- thresholds[i]
  threshold_df <- k_truth[k_truth$threshold == threshold, ]
  
  mean_over_folds <- rbind(
    mean_over_folds,
    colMeans(threshold_df, na.rm = TRUE)
  )
  
  se_E_dZ1_gs[i] <- sqrt(
    mean(threshold_df$E_dZ1_gs * (1 - threshold_df$E_dZ1_gs)) / n
  )
}

colnames(mean_over_folds) <- colnames(k_truth)

mean_over_folds <- cbind(mean_over_folds, se_E_dZ1_gs)

truth_by_seed <- data.frame(
  seed = seed_num,
  folds = k_folds,
  threshold = mean_over_folds["threshold"],
  EY_A1_dZ1_gs = mean_over_folds["EY_A1_dZ1_gs"],
  EY_A0_dZ1_gs = mean_over_folds["EY_A0_dZ1_gs"],
  E_dZ1_gs = mean_over_folds["E_dZ1_gs"],
  E_dZ1_gs_se = mean_over_folds["se_E_dZ1_gs"],
  subgroup_effect_treated_gs = mean_over_folds["subgroup_effect_treated_gs"],
  subgroup_effect_untreated_gs = mean_over_folds["subgroup_effect_untreated_gs"],
  treatment_effect_gs = mean_over_folds["treatment_effect_gs"],
  subgroup_difference_gs = mean_over_folds["subgroup_difference_gs"]
)

write.csv(
  truth_by_seed,
  file = paste0(
    "results_csv/mar_gs_data_adaptive_truth_n_",
    n,
    "_folds_",
    k_folds,
    "_seed_",
    seed,
    ".csv"
  ),
  row.names = FALSE
)
