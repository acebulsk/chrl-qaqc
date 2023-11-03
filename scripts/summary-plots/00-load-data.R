raw_data_path <- paste0('data/clean-', cur_stn, '.rds')

qaqc_data_path <- paste0('data/qaqc/qaqc_', cur_stn, '.rds')

# load data

if(!file.exists(raw_data_path)){
  print(paste("Starting data download from server for", cur_stn))
  
  query <-
    paste0("SELECT * FROM clean_",
           cur_stn,
           ";")
  
  wx_raw <- dbGetQuery(conn, query) |> 
    rename(datetime = DateTime)
  
  saveRDS(wx_raw, raw_data_path)
  print(paste("Finished data download from server for", cur_stn))
  
} else{
  print(paste("Raw file already exists for", cur_stn))
  
  wx_raw <- readRDS(raw_data_path) 
}