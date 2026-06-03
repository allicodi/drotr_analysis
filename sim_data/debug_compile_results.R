here::i_am("debug_compile_results.R")

library(data.table)
library(stringr)

dir_gs <- "/projects/dbenkes/allison/drotr_sim/journal/results_sim_data/results_object/gold_standard"

cargs <- commandArgs(TRUE)
folds <- cargs[[1]]

file_list_gs <- list.files(dir_gs, pattern=paste0("^debug_results_n_6692_folds_", folds), full.names=TRUE)

# ----- Gold Standard Results -----

combined_data_gs <- data.table()

for(file in file_list_gs){
  if(file.info(file)$mtime > as.POSIXct("2026-06-02")){
    data <- readRDS(file)
    seed <- str_extract(basename(file), paste0("(?<=debug_results_n_6692_folds_" ,folds, "_seed_)\\d+"))
    
    for(t in 1:(length(data)-1)){
      res_table <- data[[t]]$aggregated_results
      res_table$seed <- as.numeric(seed)
      combined_data_gs <- rbindlist(list(combined_data_gs, res_table))
    }
  }
}

write.csv(combined_data_gs, file=paste0("results_csv/debug_gold_standard_n_6692_folds_", folds, ".csv"), row.names=FALSE)

# ------- Data adaptive truth --------------

library(data.table)

dir <- "results_csv/"

# get names of all the results in the results_csv folder that match the pattern
# there will be one file for every seed 
file_list_gs <- list.files(dir, pattern=paste0("^debug_gs_data_adaptive_truth_n_6692_folds_", folds), full.names=TRUE)

# ----- Gold Standard -----

# make empty data table to save results
combined_data_gs <- data.table()

# for every file in the list, check and make sure timestamp is past a certain point 
# (to make sure im not adding old results if i changed something)
for(file in file_list_gs){
  if(file.info(file)$mtime > as.POSIXct("2026-06-02")){
    
    #read in results and rbind into combined_data_gs which will hold all of the results from every seed
    data <- fread(file)
    combined_data_gs <- rbindlist(list(combined_data_gs, data))
  }
}

# write data adaptive truth into truth folder
write.csv(combined_data_gs, file=paste0("truth/debug_gs_data_adaptive_truth_by_seed_folds_", folds, ".csv"), row.names=FALSE)

