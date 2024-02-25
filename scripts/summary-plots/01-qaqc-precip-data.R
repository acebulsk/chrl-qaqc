# qaqc precip data other met vars have been qc'd by julien and serghey sergey
# applied a qaqc to remove large outliers from the stand pipe data but still
# needs some additional qaqc on it before we run stats which will be added as
# new column in the qaqc_chrl dataframes

wx_qc_path <- paste0('data/qaqc_chrl/qaqc_', cur_stn, '.rds') # updated to put qaqc data from sergey and julien into sep folder

wx_qc_in <- readRDS(wx_qc_path) |> 
  mutate(WatYr = weatherdash::wtr_yr(datetime))

if(all(is.na(wx_qc_in$PC_Raw_Pipe)) == F){
  wx_qc <- wx_qc_in |> 
  select(datetime, PC_Raw_Pipe, WatYr)

# setup constants ----

# select years that dont have major problems 

# if(cur_stn == 'lowercain'){
#   wx_qc <- wx_qc |> filter(WatYr %in% c(2021, 2022))
# }
# 
# 
# if(cur_stn == 'apelake'){
#   wx_qc <- wx_qc[wx_qc$datetime > as.POSIXct('2016-10-01 00:00', tz = 'UTC'),]
#   wx_qc <- wx_qc |> filter(WatYr %in% c(2017, 2020, 2022, 2023, 2024))
# }

# global filters

glob_hi <- 2500
glob_lo <- -2500

empty_amount_threshold <- 501 # this needs to be larger than the spike_loop_th so that spikes are not erroneously added as empties, i.e. we remove pos spikes first 
max_gap_length <- 21*24 # max number of gaps to fill

spike_th <- 25
spike_th_init <- 200
max_interp_tsteps <- 48 
smooth_window <- 15
polynomial_order <- 3
small_drop_th <- 0.001 # this is used to eliminate 


## make a continuous time series ----

date_seq <- wxlogR::datetime_seq_full(wx_qc$datetime, timestep = 60*60)

missing_records <- length(date_seq) - nrow(wx_qc)

print(paste('The number of records missing is: ', missing_records))

date_seq_df <- data.frame(datetime = date_seq)

wx_qc <- date_seq_df |> 
  left_join(wx_qc, by = 'datetime') 

## apply global max / min ----

wx_glob_qc <- wx_qc |> 
  mutate(PC_Raw_Pipe = case_when(
    PC_Raw_Pipe >= glob_hi ~ NA,
    PC_Raw_Pipe <= glob_lo ~ NA,
    PC_Raw_Pipe == 0 ~ NA,
    TRUE ~ PC_Raw_Pipe
  )) |> as.data.frame()

# wx_glob_qc |>
#   # filter(WatYr == '2019') |>
#   ggplot(aes(datetime, PC_Raw_Pipe)) +
#   geom_line() +
#   facet_wrap(~WatYr, scales = 'free')

# plotly::ggplotly()

# TODO try stdev window to rm large high spots 

# rm huge spikes ----

# # try to handle some years that have huge spikes so they don't get erroneously added as empties
# 
# max_iter <- 0
# 
# spike_loop_rm <- wx_glob_qc |> as.data.frame()
# 
# # sd_wide is returned as the despiked df
# while (max_iter < 1000) {
#   
#   cur_df <- spike_loop_rm |> 
#     filter(is.na(value) == F) |> 
#     CRHMr::deleteSpikes(
#       colnum = 3,
#       threshold = 500,
#       spike_direction = 'hi'
#     )
#   
#   if(all(cur_df==0)){
#     break
#   }
#   
#   spike_loop_rm <- cur_df
#   
#   max_iter <- max_iter + 1
#   
# }
# 
# spike_loop_rm |>
#   # filter(WatYr == '2019') |>
#   ggplot(aes(datetime, value)) +
#   geom_line() +
#   facet_wrap(~WatYr, nrow = 6, scales = 'free')
# plotly::ggplotly()

# rm empties ----

pc_empty_amounts <- wx_glob_qc |>
  filter(is.na(PC_Raw_Pipe) == F) |>
  mutate(empty_amount = lag(PC_Raw_Pipe) - PC_Raw_Pipe) |>
  filter(empty_amount > empty_amount_threshold) |>
  arrange(datetime) |>
  select(datetime, empty_amount) |>
  mutate(empty_amount_rolling = cumsum(empty_amount))

pc_empty_correct <- wx_glob_qc |>
  left_join(pc_empty_amounts, by = c('datetime')) |>
  fill(empty_amount_rolling, .direction = 'down') |>
  mutate(
    empty_amount_rolling = ifelse(is.na(empty_amount_rolling) == T,
                                  0, empty_amount_rolling),
    PC_Raw_Pipe = PC_Raw_Pipe + empty_amount_rolling
  ) |> as.data.frame() |>
  select(datetime, WatYr, PC_Raw_Pipe)

# pc_empty_correct |>
#   # filter(WatYr == '2024') |>
#   ggplot(aes(datetime, PC_Raw_Pipe)) +
#   geom_line()
# 
# pc_empty_correct |>
#   # filter(WatYr == '2024') |>
#   ggplot(aes(datetime, PC_Raw_Pipe)) +
#   geom_line() +
#   facet_wrap(~WatYr, scales = 'free')

# plotly::ggplotly()

# find and remove wat yrs missing lots of data

# start qaqc ----

## ---- 1 - apply weighing gauge 1: FILL GAPS ----

findGaps(pc_empty_correct, gapfile = paste0('logs/', cur_stn, '_pc_gaps.csv'))

pc_gap_fill <- weighingGauge1(obs = pc_empty_correct, 
                              precipCol = 2, 
                              maxGapLength = max_gap_length)

gap_list <- pc_gap_fill |> filter(is.na(PC_Raw_Pipe) == T) # remaining gaps we fill below but need ids so we can set these back to NA later as they have longer gaps then we want to interpolate for

# crhmr function doesnt fill gaps at start so need to use alternative function to fill all gaps 

pc_gap_fill <- pc_gap_fill |> 
  mutate(
    PC_Raw_Pipe = 
      imputeTS::na_interpolation(PC_Raw_Pipe, maxgap = 999999))


# pc_gap_fill |>
#   ggplot(aes(datetime, PC_Raw_Pipe)) +
#   geom_line()

# plotly::ggplotly()

## ---- 2 - apply weighing gauge 2: REMOVE SPIKES ----

pc_gap_fill <-  pc_gap_fill |> filter(is.na(PC_Raw_Pipe) == F)

pc_spike_fill <- weighingGauge2(obs = pc_gap_fill, 
                                precipCol = 1, 
                                spikeThreshold = spike_th, 
                                maxSpikeGap = max_gap_length,
                                quiet = F)

if(length(pc_spike_fill) == 1){
  pc_spike_fill <- pc_gap_fill # aka no gaps present
}

# pc_spike_fill |> 
#   ggplot(aes(datetime, PC_Raw_Pipe)) + 
#   geom_line()
# 
# plotly::ggplotly()

## ---- 3 - apply weighing gauge 3 ----

pc_smooth <- weighingGauge3(obs = pc_spike_fill,
                            precipCol = 1,
                            filterLength = smooth_window,
                            polynomial_order = polynomial_order)

# rbind(pc_smooth |> 
#         mutate(group = 'smooth') |> 
#         rename(PC_Raw_Pipe = PC_Raw_Pipe_sg_filtered),
#       pc_spike_fill |> 
#         mutate(group = 'raw')) |> 
#   ggplot(aes(datetime, PC_Raw_Pipe, colour = group)) + 
#   geom_line()
# 
# plotly::ggplotly()

## ---- 4 - apply weighing gauge 4 ----

pc_fltr <- weighingGauge4(pc_smooth,
                          quiet = F, smallDropThreshold = small_drop_th)

# compare data ... 
# 
# eccc_df <- rbind(pc_smooth |>
#         mutate(group = 'eccc_qc_1') |>
#         rename(PC_Raw_Pipe = PC_Raw_Pipe_sg_filtered),
#       pc_spike_fill |>
#         mutate(group = 'raw')) |>
#   rbind(
#     pc_fltr |>
#       mutate(group = 'eccc_qc_2') |>
#       rename(PC_Raw_Pipe = PC_Raw_Pipe_sg_filtered_PcpFiltPosT)
#   )
# 
# eccc_df |>
#   ggplot(aes(datetime, PC_Raw_Pipe, colour = group)) +
#   geom_line()
# 
# plotly::ggplotly()

# fill gaps by year ---- ... this was to fix occasions where we are missing a
# few obs at the start of the water year which broke later calculations this
# occurs frequently where we do the central coast circuit flight a few weeks
# into October. Only fills gaps less than the max_gap_length th

# set large gaps back to nan and then well loop through the years to check for short gaps at the start of the wat yr
pc_fltr$interpolated_flag <- F 
pc_fltr$interpolated_flag[pc_fltr$datetime %in% gap_list$datetime] <- T # if more than 12 consequtive nans were interpolated show a flag
pc_fltr$PC_accumulated <- ifelse(pc_fltr$interpolated_flag, NA, pc_fltr$PC_Raw_Pipe_sg_filtered_PcpFiltPosT)
pc_fltr$WtrYr <- weatherdash::wtr_yr(pc_fltr$datetime)

# pc_fltr |>
#   pivot_longer(c(PC_accumulated, PC_Raw_Pipe_sg_filtered_PcpFiltPosT)) |> 
#   # filter(WatYr == '2019') |>
#   ggplot(aes(datetime, value, colour = name)) +
#   geom_line() +
#   facet_grid(~WtrYr, scales = 'free')
# plotly::ggplotly()

# now set up looping through years to find small gaps again
pc_fltr_zeroed_gps_fld <- data.frame()

yrs <- pc_fltr$WtrYr |> unique()

for(yr in yrs){
  cur_df <- pc_fltr |> filter(WtrYr == yr)
  
  if(all(cur_df$PC_accumulated |> is.na())){
    
    print(paste0('Wat yr ',
                 yr,
                 ' has no data'))
    
  } else {
    # use inputeTS instead of na_interpolation to handle gaps that start from the begining of the df which CRHM ignores
    cur_df_filled <- cur_df |> 
      mutate(
        PC_short_gap_fill = 
               imputeTS::na_interpolation(PC_accumulated, maxgap = max_gap_length),
             short_gap_flag = ifelse((!is.na(PC_short_gap_fill) & is.na(PC_accumulated)), T, F))
    
    print(paste0('Number of short gaps filled at start of wat yr ',
                 yr,
                 ': ',
                 sum(cur_df_filled$short_gap_flag)))
    
    pc_fltr_zeroed_gps_fld <- rbind(pc_fltr_zeroed_gps_fld, cur_df_filled)
  }
}

# rezero oct 1sts ----
pc_fltr_zeroed <- pc_fltr_zeroed_gps_fld |>
  select(datetime,
         WtrYr,
         PC_accumulated = PC_short_gap_fill,
         interpolated_flag,
         short_gap_flag) |>
  mutate(
    PC_incremental = PC_accumulated - lag(PC_accumulated),
    PC_incremental = ifelse(is.na(PC_incremental), 0, PC_incremental)
  ) |>
  group_by(WtrYr) |>
  mutate(PC_accumulated_wtr_yr = cumsum(PC_incremental)) |>
  select(datetime, WtrYr, everything())

# keep the short gaps but set any large gaps as nan
pc_fltr_zeroed$interpolated_flag <- ifelse(pc_fltr_zeroed$interpolated_flag & pc_fltr_zeroed$short_gap_flag, F, pc_fltr_zeroed$interpolated_flag)
pc_fltr_zeroed$PC_accumulated <- ifelse(pc_fltr_zeroed$interpolated_flag, NA, pc_fltr_zeroed$PC_accumulated)
pc_fltr_zeroed$PC_incremental <- ifelse(pc_fltr_zeroed$interpolated_flag, NA, pc_fltr_zeroed$PC_incremental)
pc_fltr_zeroed$PC_accumulated_wtr_yr <- ifelse(pc_fltr_zeroed$interpolated_flag, NA, pc_fltr_zeroed$PC_accumulated_wtr_yr)

# pc_fltr_zeroed |>
#   # filter(WatYr == '2019') |>
#   ggplot(aes(datetime, PC_accumulated_wtr_yr)) +
#   geom_line() +
#   facet_wrap(~WtrYr, scales = 'free')
# plotly::ggplotly()

## ---- 5 - write data out ----

wx_qc_out <- wx_qc_in |> 
  select(-WatYr) |> 
  left_join(pc_fltr_zeroed, by = 'datetime') |> 
  select(
    datetime,
    WtrYr,
    Air_Temp:PC_Raw_Pipe,
    PC_accumulated,
    PC_accumulated_wtr_yr,
    PC_incremental
  )

saveRDS(wx_qc_out, paste0('../../../code/r-pkgs/chrl-graph/data/qaqc_chrl_w_ac_pc/qaqc_', cur_stn, '.rds'))
} else {
  print(paste("NO stand pipe data for ", cur_stn, "filling with NANS"))
  wx_qc_out <- wx_qc_in |> 
    mutate(PC_accumulated = NA,
           PC_incremental = NA, 
           PC_accumulated_wtr_yr = NA,
           interpolated_flag = F)
  
  saveRDS(wx_qc_out, paste0('../../../code/r-pkgs/chrl-graph/data/qaqc_chrl_w_ac_pc/qaqc_', cur_stn, '.rds'))
}

