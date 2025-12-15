# ------------------------------------------------------------------------------
# Script to run simulation for given seed under simple and complex CATE settings
# -------------------------------------------------------------------------------

here::i_am("02.1_run_analysis_day3diar.R")

library(drotr)

# --------------------- Load data and wrappers -------------------------------

source("00_load_and_prep_data.R")
source("01_wrappers.R")

# ------------------------------  Set seed ---------------------------------

seed <- Sys.getenv("SLURM_ARRAY_TASK_ID")
set.seed(seed)

# ---------------- Model Specification -------------------

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

# nesting smaller rules
CATE_library_gs <- c("SL.earth",
                     "SL.ranger",
                     "SL.glmnet",
                     "SL.glm",
                     "SL.xgboost",
                     "SL.cate.shig.glm",
                     "SL.cate.rota.glm",
                     "SL.cate.path",     # ADDED 11/20/24 realized it was missing :,(
                     "SL.cate.path2",    # ADDED 11/20/24 realized it was missing :,(
                     "SL.cate.clin",
                     "SL.cate.clin2",
                     "SL.cate.pathclin",
                     "SL.cate.pathclin2",
                     "SL.cate.host",
                     "SL.cate.host2",
                     "SL.cate.all",
                     "SL.cate.all2")

# UGGHHHH just realized this does not have SL.cate.path and SL.cate.path2

# Uses all available data and is considered the best-informed rule 
Z_list_host <- c("avemuac", "wfazscore", "wflzscore", 
                 "lfazscore", "dy1_ant_sex", 
                 "agemchild", "an_ses_quintile", "an_tothhlt5")

CATE_library_host <- c("SL.earth",
                       "SL.ranger",
                       "SL.glmnet",
                       "SL.glm",
                       "SL.xgboost",
                       "SL.cate.host",
                       "SL.cate.host2")

Z_list_pathogen_q <- c("rotavirus_new", "rotavirus_bin", "norovirus_new", "norovirus_bin", "adenovirus_new",
                       "adenovirus_bin", "sapovirus_new","sapovirus_bin", "astrovirus_new", "astrovirus_bin",
                       "st_etec_new", "st_etec_bin", "shigella_new", "shigella_bin", "campylobacter_new",
                       "campylobacter_bin", "tepec_new", "tepec_bin", "v_cholerae_new", "v_cholerae_bin",
                       "salmonella_new", "salmonella_bin", "cryptosporidium_new", "cryptosporidium_bin")

CATE_library_pathogen_q <- c("SL.earth",
                             "SL.ranger",
                             "SL.glmnet",
                             "SL.glm",
                             "SL.xgboost",
                             "SL.cate.shig.glm",
                             "SL.cate.rota.glm",
                             "SL.cate.path",
                             "SL.cate.path2")

threshold_list <- c(-0.06, -0.08, -0.10)

# ------------------------- Fit nuisance model and save -------------------------
nuisance_output <- learn_nuisance(df = abcd_data,
                                  id_name = "pid",
                                  Y_name = "day3diar",
                                  A_name = "an_grp_01",
                                  W_list = W_list,
                                  sl.library.outcome = sl.library.outcome,
                                  sl.library.treatment = sl.library.treatment,
                                  sl.library.missingness = sl.library.missingness,
                                  outcome_type = "binomial",
                                  k_folds = 10,
                                  ps_trunc_level = 0.01)

#nuisance_output <- readRDS(file = paste0("/projects/dbenkes/allison/drotr_sim/journal/results_real_data/nuisance/nuisance_day3diar_seed_", seed, ".Rds"))

nuisance_models <- nuisance_output$nuisance_models
k_fold_assign_and_CATE <- nuisance_output$k_fold_assign_and_CATE
validRows <- nuisance_output$validRows

saveRDS(nuisance_output, file = paste0("/projects/dbenkes/allison/drotr_sim/journal/results_real_data/nuisance/nuisance_day3diar_seed_", seed, ".Rds"))

# ------------------------- Rule based on gold standard ----------------------------

results_gold_standard <- estimate_OTR(df = abcd_data,
                                      Y_name = "day3diar",
                                      A_name = "an_grp_01",
                                      Z_list = Z_list_gold_standard,
                                      W_list= W_list,
                                      id_name = "pid",
                                      sl.library.CATE = CATE_library_gs,
                                      nuisance_models = nuisance_models,
                                      k_fold_assign_and_CATE = k_fold_assign_and_CATE,
                                      validRows = validRows,
                                      threshold = threshold_list,
                                      k_folds = 10,
                                      ps_trunc_level = 0.01,
                                      outcome_type = "binomial")

print(results_gold_standard)

saveRDS(results_gold_standard$results, file = paste0("/projects/dbenkes/allison/drotr_sim/journal/results_real_data/results/gold_standard/results_day3diar_gs_seed_", seed, ".Rds"))

# ------------------------- Rule based on host ----------------------------

results_host <- estimate_OTR(df = abcd_data,
                             Y_name = "day3diar",
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
                             outcome_type = "binomial")

print(results_host)

saveRDS(results_host$results, file = paste0("/projects/dbenkes/allison/drotr_sim/journal/results_real_data/results/host/results_day3diar_host_seed_", seed, ".Rds"))

# ------------------------- Rule based on pathogen quantities ----------------------------

results_path <- estimate_OTR(df = abcd_data,
                             Y_name = "day3diar",
                             A_name = "an_grp_01",
                             Z_list = Z_list_pathogen_q,
                             W_list= W_list,
                             id_name = "pid",
                             sl.library.CATE = CATE_library_pathogen_q,
                             nuisance_models = nuisance_models,
                             k_fold_assign_and_CATE = k_fold_assign_and_CATE,
                             validRows = validRows,
                             threshold = threshold_list,
                             k_folds = 10,
                             ps_trunc_level = 0.01,
                             outcome_type = "binomial")

print(results_path)

saveRDS(results_path$results, file = paste0("/projects/dbenkes/allison/drotr_sim/journal/results_real_data/results/pathogen/results_day3diar_pathogen_seed_", seed, ".Rds"))
