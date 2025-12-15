here::i_am("03.1_compare_results.R")

library(data.table)
library(stringr)
library(drotr)

dir_gs <- "/projects/dbenkes/allison/drotr_sim/journal/results_sim_data/results_object/gold_standard"
dir_host <-  "/projects/dbenkes/allison/drotr_sim/journal/results_sim_data/results_object/host"

file_list_gs <- list.files(dir_gs, pattern="^results_n_6692_", full.names=TRUE)
file_list_host <- list.files(dir_host, pattern="^results_n_6692_", full.names=TRUE)

t <- 0.05
results <- data.frame()

for(file in file_list_gs){
  if(file.info(file)$mtime > as.POSIXct("2024-09-28")){
    results_gs <- readRDS(file)
    seed <- str_extract(basename(file), "(?<=results_n_6692_seed_)\\d+")
    
    # read host data from same seed
    results_host <- readRDS(paste0(dir_host, "/results_n_6692_seed_", seed,".Rds"))
    
    atrt <- compare.otr_results(results_gs, results_host, 0.05, "se", "se")
    atrt$type <- "atrt"
    atrt$seed <- seed
    atnrt <- compare.otr_results(results_gs, results_host, 0.05, "se_dZ0", "se_dZ0")
    atnrt$type <- "atnrt"
    atnrt$seed <- seed
    atr <- compare.otr_results(results_gs, results_host, 0.05, "te", "te")
    atr$type <- "atr"
    atr$seed <- seed
    
    results <- rbind(results, atrt, atnrt, atr)
  }
}

write.csv(results, file="results_csv/compare_results.csv", row.names=FALSE)
