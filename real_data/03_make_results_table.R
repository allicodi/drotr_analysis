here::i_am("03_make_results_table.R")

library(data.table)
library(stringr)

dir_gs <- "/projects/dbenkes/allison/drotr_sim/journal/results_real_data/results/gold_standard"
dir_host <- "/projects/dbenkes/allison/drotr_sim/journal/results_real_data/results/host"

file_list_gs <- list.files(dir_gs, pattern="^results_gs_", full.names=TRUE)
file_list_host <- list.files(dir_host, pattern="^results_host_", full.names=TRUE)

# ----- Gold Standard -----

combined_data_gs <- data.table()

for(file in file_list_gs){
  if(file.info(file)$mtime > as.POSIXct("2024-09-19")){
    data <- readRDS(file)
    seed <- str_extract(basename(file), "(?<=results_gs_seed_)\\d+")
    
    for(t in 1:(length(data)-1)){
      res_table <- data[[t]]$aggregated_results
      res_table$seed <- as.numeric(seed)
      combined_data_gs <- rbindlist(list(combined_data_gs, res_table))
    }
  }
}

write.csv(combined_data_gs, file="results_csv/gold_standard_n_6692.csv", row.names=FALSE)

# ----- Host Rule -----

combined_data_host <- data.table()

for(file in file_list_host){
  if(file.info(file)$mtime > as.POSIXct("2024-09-19")){
    data <- readRDS(file)
    seed <- str_extract(basename(file), "(?<=results_host_seed_)\\d+")
    
    for(t in 1:(length(data)-1)){
      res_table <- data[[t]]$aggregated_results
      res_table$seed <- as.numeric(seed)
      combined_data_host <- rbindlist(list(combined_data_host, res_table))
    }
  }
}

write.csv(combined_data_host, file="results_csv/host_n_6692.csv", row.names=FALSE)
