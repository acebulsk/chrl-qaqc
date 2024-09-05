# script to qaqc met data

raw_data_path <- paste0('data/clean-', cur_stn, '.rds')

qaqc_data_path <- paste0('data/qaqc/qaqc_', cur_stn, '.rds')

# setup constants ----

short_list <- c("Air_Temp") # eventually add more col names here we want to qc

# global filters

glob_hi <- 9999
glob_lo <- -9999

# thresholds for each variable

at_hi <- 90
at_lo <- -90

# spike thresholds absolute vals are used in the function

at_spike <- 10 # deg C over 15 min
at_sd <- 25 # number of standard deviation departures from the window mean

flatline_window <- 6 # hrs

# stdev filtering params
window_length <- 24*3 # 3 days 
lead_window <- list(1:window_length) # around 3 days to round out diurnal cycling
lag_window <- list(-1:-window_length)
frac_records_required <- 0.9

# start qaqc ----

# make a continuous time series 
date_seq <- wxlogR::datetime_seq_full(wx_raw$datetime, timestep = 60*60)

missing_records <- length(date_seq) - nrow(wx_raw)

print(paste('The number of records missing is: ', missing_records))

date_seq_df <- data.frame(datetime = date_seq)

df_cont <- date_seq_df |> 
  left_join(wx_raw, by = 'datetime')

## station specific qaqc ---- 

wx_raw_met_long <- wx_raw |> 
  mutate(BP = as.numeric(BP)) |> 
  pivot_longer(c('Air_Temp',
                                            'RH',
                                            'BP',
                                            'Snow_Depth',
                                            'Solar_Rad'))

# wx_raw_met_long |>
#   filter(WatYr == '2023') |>
#   ggplot(aes(datetime, value, colour = name)) +
#   geom_line() +
#   facet_wrap(~name, nrow = 5, scales = 'free_y')
# 
# plotly::ggplotly()

### mount cayley ----

## in 2023 at mt cayley something happened where values defaulted to zero seems
## to occur across BP RH Air Temp Snow Depth Solar Rad

if(cur_stn == 'mountcayley'){
  wx_raw <- wx_raw[wx_raw$datetime > as.POSIXct('2015-07-15 09:00:00', tz = 'UTC'),]
  
  problem_start_date <- as.POSIXct('2022-11-03 17:00', tz = 'UTC')
  problem_end_date <- as.POSIXct('2023-05-11 13:00', tz = 'UTC')
  
  wx_raw$Air_Temp[wx_raw$datetime > problem_start_date &
                    wx_raw$datetime < problem_end_date] <- NA
  wx_raw$RH[wx_raw$datetime > problem_start_date &
              wx_raw$datetime < problem_end_date] <- NA
  wx_raw$BP[wx_raw$datetime > problem_start_date &
              wx_raw$datetime < problem_end_date] <- NA
  wx_raw$Solar_Rad[wx_raw$datetime > problem_start_date &
                     wx_raw$datetime < problem_end_date] <- NA
  wx_raw$Snow_Depth[wx_raw$datetime > problem_start_date &
                      wx_raw$datetime < problem_end_date] <- NA
  
}

### ape lake ----

if(cur_stn == 'apelake'){
  wx_raw <- wx_raw[wx_raw$datetime > as.POSIXct('2016-10-01 00:00', tz = 'UTC'),]
}

## apply global max / min ----

wx_long_glob_qc <- wx_raw |> 
  select(datetime, all_of(short_list)) |> 
  pivot_longer(all_of(short_list)) |> 
  filter(value <= glob_hi,
         value >= glob_lo)

wx_glob_qc <- pivot_wider(wx_long_glob_qc)

## column specific filters----

# define col nums for crhmr function 

col_names <- names(wx_glob_qc)

at_col <- which(col_names == short_list[1]) -1

col_list <- c(at_col)

### air temp ---- 

# wx_glob_qc |>
#   ggplot(aes_string('datetime', short_list[1])) + geom_line()

#### threshold - column specific ----

at_threhsold <- wx_glob_qc |> 
  select(datetime, Air_Temp) |> 
  mutate(Air_Temp = case_when(
    Air_Temp > at_hi ~ NA,
    Air_Temp < at_lo ~ NA,
    TRUE ~ Air_Temp
  )) |> 
  as.data.frame()

#### spike search ----

at_spike_dates <- at_threhsold |> 
  filter(is.na(Air_Temp) == F) |> 
  CRHMr::findSpikes(
    colnum = at_col,
    threshold = -at_spike,
    spike_direction = 'low'
  )

# above returns 0 if no spikes
if(all(at_spike_dates == 0)){
  # i.e. there are no spikes
  at_spike_delete <- at_threhsold
} else {
  # CRHMr::plotFlags(at_threhsold, at_spike_dates, at_col)
  at_spike_delete <- at_threhsold |> 
    filter(is.na(Air_Temp) == F) |> 
    CRHMr::deleteSpikes(
      colnum = at_col,
      threshold = -at_spike,
      spike_direction = 'low',
      logfile = paste0('logs/', cur_stn, '_deletes.csv')
    )
}

#### detect flatlines ---- 

# appears that when snow is covering the T/RH sensor we get a very clear 0 deg.
# C flatline (solar also 0red during this time)

at_flatline_dates <- at_spike_delete |> 
  filter(is.na(Air_Temp) == F) |> 
  CRHMr::findFlatLines(at_col, window_size = flatline_window)

# above returns 0 if no flatlines
if(all(at_flatline_dates == 0)){
  # i.e. there are no spikes
  at_flatline_del <- at_spike_delete
} else {
  # CRHMr::plotFlags(at_spike_delete, at_flatline_dates, at_col)
  # plotly::ggplotly()
  
  at_flatline_del <- at_spike_delete |> 
    filter(is.na(Air_Temp) == F) |> 
    CRHMr::deleteFlatLines(at_col, window_size = flatline_window,
                           logfile = paste0('logs/', cur_stn, '_deletes.csv'))
}



### STDEV check on rolling window WIDE ----

at_spike_delete_nonan <- at_flatline_del |>
  filter(is.na(Air_Temp) == F)

at_spike_dates <- CRHMr::findSpikesStdevWindow(at_spike_delete_nonan,
                                                   min_frac_records = frac_records_required,
                                                   colnum = at_col,
                                                   lead_window = lead_window,
                                                   lag_window = lag_window,
                                                   number_sd = at_sd,
                                                   include_start_end = F
)

# above returns 0 if no spikes
if(all(at_spike_dates == 0)){
  # i.e. there are no spikes
  at_spike_delete <- at_spike_delete_nonan
} else {
  
  # CRHMr::plotFlags(at_spike_delete_nonan, at_spike_dates, at_col)
  # 
  # plotly::ggplotly()
  
  at_spike_delete <- at_spike_delete_nonan |> 
    filter(is.na(Air_Temp) == F) |> 
    CRHMr::deleteSpikesStdevWindow(
      min_frac_records =  frac_records_required,
      colnum = at_col,
      lead_window = lead_window,
      lag_window = lag_window,
      number_sd = at_sd,
      include_start_end = F,
      logfile = paste0('logs/', cur_stn, '_deletes.csv')
    )
}

#### manual removals ----

## Write Out QC Data ----

record_start_date <- min(wx_raw$datetime)
record_finish_date <- max(wx_raw$datetime)

complete_datetime <- seq(record_start_date, record_finish_date, by = 'hour')

qc_data_out <- data.frame(datetime = complete_datetime) |> 
  left_join(at_spike_delete_nonan)

saveRDS(qc_data_out, qaqc_data_path)
