here::i_am("sim_data/06_make_data_adaptive_truth_table.R")

library(data.table)

dir <- "results_csv/"

# get names of all the results in the results_csv folder that match the pattern
# there will be one file for every seed 
file_list_gs <- list.files(dir, pattern="^gs_data_adaptive_truth_", full.names=TRUE)
file_list_host <- list.files(dir, pattern="^host_data_adaptive_truth_", full.names=TRUE)

# ----- Gold Standard -----

# make empty data table to save results
combined_data_gs <- data.table()

# for every file in the list, check and make sure timestamp is past a certain point 
# (to make sure im not adding old results if i changed something)
for(file in file_list_gs){
  if(file.info(file)$mtime > as.POSIXct("2024-09-19")){
    
    #read in results and rbind into combined_data_gs which will hold all of the results from every seed
    data <- fread(file)
    combined_data_gs <- rbindlist(list(combined_data_gs, data))
  }
}

# write data adaptive truth into truth folder
write.csv(combined_data_gs, file="truth/gs_data_adaptive_truth_by_seed.csv", row.names=FALSE)

# ----- Host -----

combined_data_host <- data.table()

for(file in file_list_host){
  if(file.info(file)$mtime > as.POSIXct("2024-10-02")){
    data <- fread(file)
    combined_data_host <- rbindlist(list(combined_data_host, data))
  }
}

write.csv(combined_data_host, file="truth/host_data_adaptive_truth_by_seed.csv", row.names=FALSE)
