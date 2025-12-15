
here::i_am("05_compare_results.R")

library(drotr)

gs_res_seed_1 <- readRDS(here::here("results_csv/results_gs_seed_1.Rds"))
host_res_seed_1 <- readRDS(here::here("results_csv/results_host_seed_1.Rds"))

compare.otr_results(gs_res_seed_1, host_res_seed_1, threshold = 0.06, "se", "se")
compare.otr_results(gs_res_seed_1, host_res_seed_1, threshold = 0.08, "se", "se")
compare.otr_results(gs_res_seed_1, host_res_seed_1, threshold = 0.10, "se", "se")

compare.otr_results(gs_res_seed_1, host_res_seed_1, threshold = 0.06, "se_dZ0", "se_dZ0")
compare.otr_results(gs_res_seed_1, host_res_seed_1, threshold = 0.08, "se_dZ0", "se_dZ0")
compare.otr_results(gs_res_seed_1, host_res_seed_1, threshold = 0.10, "se_dZ0", "se_dZ0")

compare.otr_results(gs_res_seed_1, host_res_seed_1, threshold = 0.06, "te", "te")
compare.otr_results(gs_res_seed_1, host_res_seed_1, threshold = 0.08, "te", "te")
compare.otr_results(gs_res_seed_1, host_res_seed_1, threshold = 0.10, "te", "te")
 
#--------------------------------------------

# Repeat for day3diar

day3diar_gs_res_seed_1 <- readRDS(here::here("results_csv/results_day3diar_gs_seed1.Rds"))
day3diar_host_res_seed_1 <- readRDS(here::here("results_csv/results_day3diar_host_seed1.Rds"))

compare.otr_results(day3diar_gs_res_seed_1, day3diar_host_res_seed_1, threshold = -0.06, "se", "se")
compare.otr_results(day3diar_gs_res_seed_1, day3diar_host_res_seed_1, threshold = -0.08, "se", "se")
compare.otr_results(day3diar_gs_res_seed_1, day3diar_host_res_seed_1, threshold = -0.10, "se", "se")

compare.otr_results(day3diar_gs_res_seed_1, day3diar_host_res_seed_1, threshold = -0.06, "se_dZ0", "se_dZ0")
compare.otr_results(day3diar_gs_res_seed_1, day3diar_host_res_seed_1, threshold = -0.08, "se_dZ0", "se_dZ0")
compare.otr_results(day3diar_gs_res_seed_1, day3diar_host_res_seed_1, threshold = -0.10, "se_dZ0", "se_dZ0")

compare.otr_results(day3diar_gs_res_seed_1, day3diar_host_res_seed_1, threshold = -0.06, "te", "te")
compare.otr_results(day3diar_gs_res_seed_1, day3diar_host_res_seed_1, threshold = -0.08, "te", "te")
compare.otr_results(day3diar_gs_res_seed_1, day3diar_host_res_seed_1, threshold = -0.10, "te", "te")