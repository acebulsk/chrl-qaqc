# qaqc precip data, could eventually combine with met qaqc

gap_fill_data_path <- paste0('data/gap-fill/gap_fill_', cur_stn, '.rds')

# setup constants ----

short_list <- c("PC_Raw_Pipe") # eventually add more col names here we want to qc

# select years that dont have major problems 

if(cur_stn == 'lowercain'){
  wx_raw <- wx_raw |> filter(WatYr %in% c(2021, 2022))
}


if(cur_stn == 'apelake'){
  wx_raw <- wx_raw[wx_raw$datetime > as.POSIXct('2016-10-01 00:00', tz = 'UTC'),]
  wx_raw <- wx_raw |> filter(WatYr %in% c(2017, 2020, 2022, 2023, 2024))
}

# global filters

glob_hi <- 2500
glob_lo <- -2500

empty_amount_threshold <- 501 # this needs to be larger than the spike_loop_th so that spikes are not erroneously added as empties, i.e. we remove pos spikes first 
max_gap_length <- 12 # max number of gaps to fill

spike_th <- 25
spike_th_init <- 200
max_interp_tsteps <- 48 
smooth_window <- 15
polynomial_order <- 3
small_drop_th <- 0.001 # this is used to eliminate 
flatline_window <- 6 # hrs

# stdev filtering params
window_length <- 24 # 
lead_window <- list(1:window_length) # around 3 days to round out diurnal cycling
lag_window <- list(-1:-window_length)
frac_records_required <- 0.9

## make a continuous time series ----

date_seq <- wxlogR::datetime_seq_full(wx_raw$datetime, timestep = 60*60)

missing_records <- length(date_seq) - nrow(wx_raw)

print(paste('The number of records missing is: ', missing_records))

date_seq_df <- data.frame(datetime = date_seq)

wx_raw <- date_seq_df |> 
  left_join(wx_raw, by = 'datetime') 

## apply global max / min ----

wx_raw_met_long <- wx_raw |> 
  select(datetime, WatYr, all_of(short_list)) |> 
  pivot_longer(all_of(short_list))

wx_long_glob_qc <- wx_raw_met_long |> 
  mutate(value = case_when(
    value >= glob_hi ~ NA,
    value <= glob_lo ~ NA,
    value == 0 ~ NA,
    TRUE ~ value
  )) |> as.data.frame()

wx_long_glob_qc |>
  # filter(WatYr == '2019') |>
  ggplot(aes(datetime, value)) +
  geom_line() +
  facet_wrap(~WatYr, nrow = 6, scales = 'free')

# plotly::ggplotly()

# TODO try stdev window to rm large high spots 

# rm huge spikes ----

# # try to handle some years that have huge spikes so they don't get erroneously added as empties
# 
# max_iter <- 0
# 
# spike_loop_rm <- wx_long_glob_qc |> as.data.frame()
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

pc_empty_amounts <- wx_long_glob_qc |>
  filter(is.na(value) == F) |> 
  mutate(empty_amount = lag(value) - value) |> 
  filter(empty_amount > empty_amount_threshold) |>
  arrange(datetime) |>
  select(datetime, empty_amount) |>
  mutate(empty_amount_rolling = cumsum(empty_amount))

pc_empty_correct <- wx_long_glob_qc |>
  left_join(pc_empty_amounts, by = c('datetime')) |>
  fill(empty_amount_rolling, .direction = 'down') |>
  mutate(
    empty_amount_rolling = ifelse(is.na(empty_amount_rolling) == T,
                                  0, empty_amount_rolling),
    value = value + empty_amount_rolling
  ) |> as.data.frame() |> 
  select(datetime, WatYr, value)

pc_empty_correct |>
  # filter(WatYr == '2024') |>
  ggplot(aes(datetime, value)) +
  geom_line() +
  facet_wrap(~WatYr, nrow = 6, scales = 'free')

plotly::ggplotly()

# start qaqc ----

## ---- 1 - apply weighing gauge 1 ----

findGaps(pc_empty_correct, gapfile = paste0('logs/', cur_stn, '_pc_gaps.csv'))

pc_gap_fill <- weighingGauge1(obs = pc_empty_correct, 
                              precipCol = 2, 
                              maxGapLength = max_gap_length)

findGaps(pc_gap_fill, gapfile = paste0('logs/', cur_stn, '_pc_gaps.csv'))

pc_gap_fill |> 
  ggplot(aes(datetime, value)) + 
  geom_line()
plotly::ggplotly()


## ---- 2 - apply weighing gauge 2 ----

pc_spike_fill <- weighingGauge2(obs = pc_gap_fill |> filter(is.na(value) == F), 
                                precipCol = 1, 
                                spikeThreshold = spike_th, 
                                maxSpikeGap = max_gap_length,
                                quiet = F)

pc_spike_fill |> 
  ggplot(aes(datetime, value)) + 
  geom_line()

plotly::ggplotly()

## ---- 3 - apply weighing gauge 3 ----

pc_smooth <- weighingGauge3(obs = pc_spike_fill,
                            precipCol = 1,
                            filterLength = smooth_window,
                            polynomial_order = polynomial_order)

rbind(pc_smooth |> 
        mutate(group = 'smooth') |> 
        rename(value = value_sg_filtered),
      pc_spike_fill |> 
        mutate(group = 'raw')) |> 
  ggplot(aes(datetime, value, colour = group)) + 
  geom_line()

plotly::ggplotly()

## ---- 4 - apply weighing gauge 4 ----

pc_fltr <- weighingGauge4(pc_smooth,
                          quiet = F, smallDropThreshold = small_drop_th)

## ---- 5 - write data out ----

met_df <- readRDS(gap_fill_data_path) |> left_join(pc_fltr) |> 
  rename(PC = value_sg_filtered_PcpFiltPosT)

saveRDS(met_df, gap_fill_data_path)

# compare data ... 
# 
# eccc_df <- rbind(pc_smooth |>
#         mutate(group = 'eccc_qc_1') |>
#         rename(value = value_sg_filtered),
#       pc_spike_fill |>
#         mutate(group = 'raw')) |>
#   rbind(
#     pc_fltr |>
#       mutate(group = 'eccc_qc_2') |>
#       rename(value = value_sg_filtered_PcpFiltPosT)
#   )
# 
# eccc_df |>
#   ggplot(aes(datetime, value, colour = group)) +
#   geom_line()
# 
# plotly::ggplotly()
# 
# # compare sergey too ----
# 
# sg_pc <- read.csv('data/from-sergey/CainLower_presip_qaqc-ed.txt', skip = 1) |> 
#   slice(2:n()) |> 
#   mutate(datetime = as.POSIXct((as.numeric(MatLabTime) - 719529) * 86400, origin = "1970-01-01", tz = "UTC"),
#          datetime = round(datetime, 'hour')) |> 
#   select(datetime, Precip_raw, Precip_filtered) |> 
#   filter(datetime > '2020-10-01',
#          datetime < '2022-10-01',) |> 
#   mutate(sm_raw = as.numeric(Precip_raw) - 3921,
#          sm_filtered = as.numeric(Precip_filtered) - 3853) |> 
#   select(datetime, sm_raw, sm_filtered) |> 
#   pivot_longer(!datetime, names_to = 'group')
# 
# compare_df <- rbind(eccc_df, sg_pc) 
# 
# compare_df |> 
#   ggplot(aes(datetime, value, colour = group)) + 
#   geom_line()
# 
# g <- plotly::ggplotly()
# 
# htmltools::save_html(g, 'data/from-sergey/compare_sergey_eccc_precip_qaqc_interactive_plot.html')
# 
# write.csv(compare_df, 'data/from-sergey/compare_sergey_eccc_precip_qaqc.csv',row.names = F)
  