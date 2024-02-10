# script to calculate monthly means and totals 

library(tidyverse)

qc_vars <- c("Air_Temp", "Snow_Depth", "SWE", "PC_accumulated_wtr_yr")

gap_fill_path <- paste0('data/qaqc_chrl_w_ac_pc/qaqc_', cur_stn, '.rds')

qc_fill_data <- readRDS(gap_fill_path)

# no met data.. do not continue... 
stopifnot("Air_Temp" %in% names(qc_fill_data))

# Check if columns are numeric
qc_vars <- qc_vars[sapply(qc_fill_data[qc_vars], is.numeric)]

max_date <- max(qc_fill_data$datetime) |> as.Date()
min_date <- min(qc_fill_data$datetime) |> as.Date()
max_year <- max(format(qc_fill_data$datetime, '%Y')) |> as.numeric()
min_year <- min(format(qc_fill_data$datetime, '%Y')) |> as.numeric()
year_range <- length(min_year:max_year)
perc_records_fltr <- 0.9

qc_fill_data$month_num <- format(qc_fill_data$datetime, '%m')
qc_fill_data$month_short <- format(qc_fill_data$datetime, '%b')
qc_fill_data$year <- format(qc_fill_data$datetime, '%Y')
qc_fill_data$wtr_year <- weatherdash::wtr_yr(qc_fill_data$datetime)
qc_fill_data$plot_time <- format(qc_fill_data$datetime, "1900-%m-%d %H:%M:%S") # 81 so not a leap year
qc_fill_data$plot_time <- as.POSIXct(qc_fill_data$plot_time, format = "%Y-%m-%d %H:%M:%S", tz = 'UTC')
qc_fill_data$records_filter <- lubridate::days_in_month(qc_fill_data$datetime)*24*perc_records_fltr

# hourly stats on filled snow depth will plot annual no fill data on this
hourly_stats <- qc_fill_data |> 
  filter(year != max_year) |> 
  pivot_longer(qc_vars) |>
  group_by(plot_time, name) |> 
  filter(is.na(value) == F, is.na(plot_time) == F) |> 
  # mutate(n_records = n(),
  #        has_enough_records = n_records > (year_range*perc_records_fltr)) |> 
  # filter(has_enough_records == T) |> 
  summarise(
    mean = mean(value, na.rm = T),
    min = min(value, na.rm = T),
    max = max(value, na.rm = T),
    upper_quantile = quantile(value,0.95, na.rm = T),
    lower_quantile = quantile(value, 0.05, na.rm = T)
  ) |> ungroup()

saveRDS(hourly_stats,
        paste0(
          'data/hourly_stats/',
          cur_stn,
          '_hourly_stats.rds'
        ))
  
# avgs for over all years, but do not include the current year
# glob_avg <- qc_fill_data |> 
#   filter(year != max_year) |> 
#   pivot_longer(qc_vars) |>
#   group_by(month_num, name) |> 
#   filter(is.na(value) == F) |> 
#   mutate(n_records_in_month = n(),
#          has_enough_records = n_records_in_month > records_filter) |> 
#   filter(has_enough_records == T) |> 
#   summarise(
#     Mean = mean(value, na.rm = T),
#     Max = max(value, na.rm = T),
#     Min = min(value, na.rm = T),
#     upper_95th_percentile = quantile(value,0.95, na.rm = T),
#     lower_5th_percentile = quantile(value, 0.05, na.rm = T)
#   ) |> ungroup()
# 
# glob_avg$month_name <- month.abb[as.numeric(glob_avg$month_num)]
# glob_avg$month_name <- ordered(glob_avg$month_name, levels = c(month.abb[10:12], month.abb[1:9]))
# 
# saveRDS(glob_avg,
#         paste0(
#           'data/glob_average/',
#           cur_stn,
#           '_glob_average.rds'
#         ))

# monthly avgs for each year
monthly_summary <- qc_fill_data |> 
  pivot_longer(qc_vars) |>
  group_by(year, wtr_year, month_num, name) |> 
  mutate(n_records_in_month = n(),
         has_enough_records = n_records_in_month > records_filter) |> 
  filter(has_enough_records == T) |> 
  summarise(
    mean_monthly = mean(value, na.rm = T),
    max_monthly = max(value, na.rm = T),
    min_monthly = min(value, na.rm = T),
    upper_quantile = quantile(value,0.95, na.rm = T),
    lower_quantile = quantile(value, 0.05, na.rm = T)
  ) |> ungroup()

monthly_summary$month_name <- month.abb[as.numeric(monthly_summary$month_num)]
monthly_summary$month_name <- ordered(monthly_summary$month_name, levels = c(month.abb[10:12], month.abb[1:9]))

saveRDS(monthly_summary,
          paste0(
            'data/monthly_normals_plot_data/',
            cur_stn,
            '_monthly_normals_data.rds'
          ))

# the max/min/mean of the monthly means

yearly_mean_monthly_summary <- monthly_summary |> 
  group_by(month_num, name) |> 
  filter(is.na(mean_monthly) == F) |> 
  summarise(
    Mean = mean(mean_monthly, na.rm = T),
    Max = max(mean_monthly, na.rm = T),
    Min = min(mean_monthly, na.rm = T),
    upper_95th_percentile = quantile(mean_monthly,0.95, na.rm = T),
    lower_5th_percentile = quantile(mean_monthly, 0.05, na.rm = T)
  ) |> ungroup()

yearly_mean_monthly_summary$month_name <- month.abb[as.numeric(yearly_mean_monthly_summary$month_num)]
yearly_mean_monthly_summary$month_name <- ordered(yearly_mean_monthly_summary$month_name, levels = c(month.abb[10:12], month.abb[1:9]))

saveRDS(yearly_mean_monthly_summary,
        paste0(
          'data/yearly_mean_monthly_summary/',
          cur_stn,
          '_yearly_mean_monthly_summary.rds'
        ))

# output monthly (all years separate) stats to pretty 

# tbl_out_monthly <- monthly_summary |> 
#   mutate(station_name = cur_stn) |> 
#   select(station_name,
#          year,
#          month_num,
#          mean_monthly,
#          max_monthly,
#          min_monthly,
#          lower_5th_percentile = lower_quantile,
#          upper_95th_percentile = upper_quantile,
#          name)
# 
# # aggregate years for overall monthly normals 
# 
# tbl_out_monthly_normals <- glob_avg |> 
#   mutate(station_name = cur_stn) |> 
#   select(station_name,
#          month_num,
#          mean_all_years_value = Mean,
#          max_all_years_value = Max,
#          min_all_years_value = Min,
#          lower_5th_percentile,
#          upper_95th_percentile,
#          name)
# 
# write.csv(tbl_out_monthly,
#           paste0(
#             'data/stat_summary/',
#             cur_stn,
#             '_air_temp_stats_of_each_month_for_each_year.csv'
#           ),
#           row.names = F)
#           
# write.csv(tbl_out_monthly_normals,
#           paste0(
#             'data/stat_summary/',
#             cur_stn,
#             '_air_temp_monthly_normals.csv'
#           ),
#           row.names = F)
