# raw_data_path <- paste0('data/clean-', cur_stn, '.rds')

# qaqc_data_path <- paste0('data/qaqc/qaqc_', cur_stn, '.rds') #this was qcd by AC

qaqc_data_path <- paste0('data/qaqc_chrl/qaqc_', cur_stn, '.rds') # updated to put qaqc data from sergey and julien into sep folder

# load data

print(paste("Starting data download from server for", cur_stn))
  
  # query <-
  #   paste0("SELECT * FROM clean_",
  #          cur_stn,
  #          ";")
  # 
  # wx_raw <- dbGetQuery(conn, query) |> 
  #   rename(datetime = DateTime)
  
query <-
    paste0("SELECT DateTime, Air_Temp, Snow_Depth, SWE, PC_Raw_Pipe FROM qaqc_",
           cur_stn,
           ";")
  
  tryCatch({
    wx_qc <- dbGetQuery(conn, query) |> 
      rename(datetime = DateTime) 
  },
  error = function(e) print(e)
  )
  
  if(exists('wx_qc')){
    saveRDS(wx_qc, qaqc_data_path)
    print(paste("QC'd data found for", cur_stn))
  } else {
    print(paste("There was no QC'd data found for", cur_stn))
  }
  
print(paste("Finished data download from server for", cur_stn))
  

