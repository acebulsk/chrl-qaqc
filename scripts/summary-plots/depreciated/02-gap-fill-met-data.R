# script to gap fill met data currently just fills small gaps
# TODO build regression for cross station filling 

gap_fill_path <- paste0('data/gap-fill/gap_fill_', cur_stn, '.rds')
qaqc_data_path <- paste0('data/qaqc_chrl_w_ac_pc/qaqc_', cur_stn, '.rds')

at_col <- 2
sd_col <- 3
swe_col <- 4
# pc_col <- 5 PC already filled.... 

max_gap_fill_linear <- 3 # if more than two records are missing linear interpolation is not used
max_gap_fill_sd <- 60*24 # about 2 months to smooth out snowdepth

if(file.exists(qaqc_data_path)){
  qc_data <- readRDS(qaqc_data_path)
  CRHMr::findGaps(qc_data, gapfile = paste0('logs/', cur_stn, '_gaps.csv'), quiet = F)
  
  # fill air temperature
  at_fill <- CRHMr::interpolate(qc_data, 
                                varcols = at_col,
                                methods = 'linear', 
                                maxlength = max_gap_fill_linear,
                                logfile = paste0('logs/', cur_stn, '_at_interpolate.csv'))
  
  at_still_have_gaps <- CRHMr::findGaps(at_fill, paste0('logs/', cur_stn, '_gaps.csv'), quiet = F)
  
  if(!at_still_have_gaps == F){
    warning(paste0(cur_stn, " Air temp still has gaps."))
  }
  
  # check if we have any snowdepth data 
  
  if("Snow_Depth_qaqc" %in% colnames(wx_raw)){
    # fill snow depth
    sd_raw <- wx_raw |> select(datetime, Snow_Depth_qaqc)
    
    if(sum(is.na(sd_raw$Snow_Depth_qaqc) == F) == 0){
      sd_fill <- sd_raw |> rename(Snow_Depth_qaqc_filled = Snow_Depth_qaqc) 
    } else {
      sd_fill <- CRHMr::interpolate(sd_raw, 
                                    varcols = 1,
                                    methods = 'linear', 
                                    maxlength = max_gap_fill_sd,
                                    logfile = paste0('logs/', cur_stn, '_sd_interpolate.csv')) |> 
        rename(Snow_Depth_qaqc_filled = Snow_Depth_qaqc) 
      
      # ggplot(sd_fill, aes(datetime, Snow_Depth_qaqc)) + geom_line()
      # plotly::ggplotly()
      
      sd_still_have_gaps <- CRHMr::findGaps(sd_fill, paste0('logs/', cur_stn, '_gaps.csv'), quiet = F)
      
    }
    
  } else {
    sd_raw <- wx_raw |> select(datetime)
    sd_raw$Snow_Depth_qaqc <- NA
    sd_fill <- sd_raw |> 
      rename(Snow_Depth_qaqc_filled = Snow_Depth_qaqc) 
  }
  
  # Julian has already gap filled snow depth data using 3 hr interpolation so tag
  # this on to this file as well 
  
  
  df_fill <- left_join(at_fill, sd_raw) |> left_join(sd_fill)
  
  saveRDS(df_fill, gap_fill_path)
  
  # TODO fill larger gaps using regressions from other stations
}else{
  warning(paste0(cur_stn, " has no QC data."))
}



