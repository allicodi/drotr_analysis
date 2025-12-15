# ------------------------------------------------------------------------------
# Script to run simulation for given seed under simple and complex CATE settings
# -------------------------------------------------------------------------------

here::i_am("02.2_run_simulation_host.R")

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

# Host characteristics CATE 
Z_list_host <- c("avemuac", "wfazscore", "wflzscore", 
                 "lfazscore", "dy1_ant_sex", 
                 "agemchild", "an_ses_quintile", "an_tothhlt5")

# ML libraries to try to capture the three-way interaction
CATE_library_host <- c("SL.earth",
                       "SL.ranger",
                       "SL.glmnet",
                       "SL.glm")

threshold_list <- c(0.025, 0.05, 0.075, 0.10)

# ------------------------- Load nuisance model from gold standard CATE analysis -------------------------
nuisance_output <- readRDS(file = paste0("/projects/dbenkes/allison/drotr_sim/journal/results_sim_data/nuisance/nuisance_n_", n, "_seed_", seed, ".Rds"))

nuisance_models <- nuisance_output$nuisance_models
k_fold_assign_and_CATE <- nuisance_output$k_fold_assign_and_CATE
validRows <- nuisance_output$validRows

# ------------------------- Rule based on host characteristics ----------------------------

results_host <- estimate_OTR(df = abcd_data,
                               Y_name = "lazd90",
                               A_name = "an_grp_01",
                               Z_list = Z_list_host,
                               W_list= W_list,
                               id_name = "pid",
                               sl.library.CATE = CATE_library_host,
                               nuisance_models = nuisance_models,
                               k_fold_assign_and_CATE = k_fold_assign_and_CATE,
                               validRows = validRows,
                               threshold = threshold_list,
                               k_folds = 10,
                               ps_trunc_level = 0.01,
                               outcome_type = "gaussian")

print(results_host)

saveRDS(results_host, file = paste0("/projects/dbenkes/allison/drotr_sim/journal/results_sim_data/results_object/host/full_results_n_",n,"_seed_", seed, ".Rds"))
saveRDS(results_host$results, file = paste0("/projects/dbenkes/allison/drotr_sim/journal/results_sim_data/results_object/host/results_n_",n,"_seed_", seed, ".Rds"))
