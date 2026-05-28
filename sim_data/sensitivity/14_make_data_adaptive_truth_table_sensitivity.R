here::i_am("sensitivity/14_make_data_adaptive_truth_table_sensitivity.R")

library(data.table)

dir <- "results_csv/"

# get names of all the results in the results_csv folder that match the pattern
# there will be one file for every seed 
file_list_mar <- list.files(dir, pattern="^mar_gs_data_adaptive_truth_", full.names=TRUE)
file_list_mcar <- list.files(dir, pattern="^mcar_gs_data_adaptive_truth_", full.names=TRUE)

# ----- Gold Standard MAR -----

# make empty data table to save results
combined_data_gs_mar <- data.table()

# for every file in the list, check and make sure timestamp is past a certain point 
# (to make sure im not adding old results if i changed something)
for(file in file_list_mar){
  if(file.info(file)$mtime > as.POSIXct("2026-05-27")){
    
    #read in results and rbind into combined_data_gs which will hold all of the results from every seed
    data <- fread(file)
    combined_data_gs_mar <- rbindlist(list(combined_data_gs_mar, data))
  }
}

# write data adaptive truth into truth folder
write.csv(combined_data_gs_mar, file="truth/mar_gs_data_adaptive_truth_by_seed.csv", row.names=FALSE)

# ----- Gold Standard MCAR -----

# make empty data table to save results
combined_data_gs_mcar <- data.table()

# for every file in the list, check and make sure timestamp is past a certain point 
# (to make sure im not adding old results if i changed something)
for(file in file_list_mcar){
  if(file.info(file)$mtime > as.POSIXct("2026-05-27")){
    
    #read in results and rbind into combined_data_gs which will hold all of the results from every seed
    data <- fread(file)
    combined_data_gs_mcar <- rbindlist(list(combined_data_gs_mcar, data))
  }
}

# write data adaptive truth into truth folder
write.csv(combined_data_gs_mcar, file="truth/mcar_gs_data_adaptive_truth_by_seed.csv", row.names=FALSE)
