here::i_am("sensitivity/12_make_results_table.R")

library(data.table)
library(stringr)

dir_gs <- "/projects/dbenkes/allison/drotr_sim/journal/results_sim_data/results_object/gold_standard"

missing_types <- c("mar", "mcar")

for (miss_type in missing_types) {
  
  file_list <- list.files(
    dir_gs,
    pattern = paste0("^", miss_type, "_results_n_6692_"),
    full.names = TRUE
  )
  
  combined_data_gs <- data.table()
  
  for (file in file_list) {
    
    if (file.info(file)$mtime > as.POSIXct("2026-05-29")) {
      
      data <- readRDS(file)
      
      seed <- str_extract(
        basename(file),
        paste0("(?<=", miss_type, "_results_n_6692_seed_)\\d+")
      )
      
      for (t in 1:(length(data) - 1)) {
        res_table <- data[[t]]$aggregated_results
        res_table$seed <- as.numeric(seed)
        res_table$missing_type <- miss_type
        
        combined_data_gs <- rbindlist(
          list(combined_data_gs, res_table),
          fill = TRUE
        )
      }
    }
  }
  
  write.csv(
    combined_data_gs,
    file = paste0("results_csv/gold_standard_", miss_type, "_n_6692.csv"),
    row.names = FALSE
  )
}
