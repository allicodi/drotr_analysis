# ---------------------------------------------------------------
# Script to run sensitivity analysis for higher proportion MCAR 
# ---------------------------------------------------------------

here::i_am("sensitivity/10_run_mcar_sensitivity.R")

library(drotr)

source("00_simulate_data.R")
source("01_wrappers.R")

# ------------------------------  Set seed ---------------------------------

seed <- Sys.getenv("SLURM_ARRAY_TASK_ID")
set.seed(seed)

# size of ABCD dataset
n <- 6692

# ---------------- Data Generation & Model Specification -------------------

# Generate Simulation Data -- MCAR, 25% 
abcd_data <- generate_abcd(n, mcar_sens = TRUE, mcar_prop = 0.25)

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

threshold_list <- c(0.05, 0.15, 0.25, 0.35)

# ------------------------- Fit nuisance model and save -------------------------

nuisance_path <- paste0(
  "/projects/dbenkes/allison/drotr_sim/journal/results_sim_data/nuisance/",
  "mcar_nuisance_n_", n, "_seed_", seed, ".Rds"
)

if (file.exists(nuisance_path)) {
  
  message("Loading existing nuisance output: ", nuisance_path)
  nuisance_output <- readRDS(nuisance_path)
  
} else {
  
  message("Nuisance output not found. Fitting nuisance models...")
  
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
    k_folds = 5,
    ps_trunc_level = 0.01
  )
  
  saveRDS(nuisance_output, file = nuisance_path)
  message("Saved nuisance output: ", nuisance_path)
}

nuisance_models <- nuisance_output$nuisance_models
k_fold_assign_and_CATE <- nuisance_output$k_fold_assign_and_CATE
validRows <- nuisance_output$validRows

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
                                      k_folds = 5,
                                      ps_trunc_level = 0.01,
                                      outcome_type = "gaussian")

print(results_gold_standard)

# get rid of nuisance models before saving (need CATE for data adaptive)
results_gold_standard$nuisance_models <- NULL

saveRDS(results_gold_standard, file = paste0("/projects/dbenkes/allison/drotr_sim/journal/results_sim_data/results_object/gold_standard/mcar_full_results_n_",n,"_seed_", seed, ".Rds"))
saveRDS(results_gold_standard$results, file = paste0("/projects/dbenkes/allison/drotr_sim/journal/results_sim_data/results_object/gold_standard/mcar_results_n_",n,"_seed_", seed, ".Rds"))
