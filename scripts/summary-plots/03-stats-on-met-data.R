# script to calculate monthly means and totals 

library(tidyverse)

gap_fill_path <- paste0('data/gap-fill/gap_fill_', cur_stn, '.rds')

qc_fill_data <- readRDS(gap_fill_path)

max_date <- max(qc_fill_data$datetime) |> as.Date()
min_date <- min(qc_fill_data$datetime) |> as.Date()
max_year <- max(format(qc_fill_data$datetime, '%Y')) |> as.numeric()
min_year <- min(format(qc_fill_data$datetime, '%Y')) |> as.numeric()

perc_records_in_month <- 0.9

qc_fill_data$month_num <- format(qc_fill_data$datetime, '%m')
qc_fill_data$month_short <- format(qc_fill_data$datetime, '%b')
qc_fill_data$year <- format(qc_fill_data$datetime, '%Y')
qc_fill_data$plot_time <- weatherdash::set_yr(qc_fill_data$datetime, 1900)
qc_fill_data$records_filter <- lubridate::days_in_month(qc_fill_data$datetime)*24*perc_records_in_month

# avgs for over all years, but do not include the current year
glob_avg <- qc_fill_data |> 
  filter(year != max_year) |> 
  group_by(month_num) |> 
  mutate(n_records_in_month = n(),
         has_enough_records = n_records_in_month > records_filter) |> 
  filter(has_enough_records == T) |> 
  summarise(
    glob_mean_monthly_temp = mean(Air_Temp, na.rm = T),
    glob_min_monthly_temp = min(Air_Temp, na.rm = T),
    glob_max_monthly_temp = max(Air_Temp, na.rm = T),
    glob_upper_quantile = quantile(Air_Temp,0.95, na.rm = T),
    glob_lower_quantile = quantile(Air_Temp, 0.05, na.rm = T)
  )

# monthly avgs for each year
monthly_air_temp_summary <- qc_fill_data |> 
  group_by(year, month_num) |> 
  mutate(n_records_in_month = n(),
         has_enough_records = n_records_in_month > records_filter) |> 
  filter(has_enough_records == T) |> 
  summarise(
    mean_monthly_temp = mean(Air_Temp, na.rm = T),
    max_monthly_temp = max(Air_Temp, na.rm = T),
    min_monthly_temp = min(Air_Temp, na.rm = T),
    upper_quantile = quantile(Air_Temp,0.95, na.rm = T),
    lower_quantile = quantile(Air_Temp, 0.05, na.rm = T)
  ) |> left_join(
    glob_avg , by = 'month_num' # need the global quantiles for the graphs later
  )

qc_fill_data <- qc_fill_data |> left_join(
  glob_avg , by = 'month_num'
)

# output monthly (all years separate) stats to pretty 

tbl_out_monthly <- monthly_air_temp_summary |> 
  mutate(station_name = cur_stn,
         variable_name = 'air_temp',
         unit = "deg. C") |> 
  select(station_name,
         year,
         month_num,
         mean_monthly_value = mean_monthly_temp,
         max_monthly_value = max_monthly_temp,
         min_monthly_value = min_monthly_temp,
         lower_5th_percentile = lower_quantile,
         upper_95th_percentile = upper_quantile,
         variable_name,
         unit)

# aggregate years for overall monthly normals 

tbl_out_monthly_normals <- glob_avg |> 
  mutate(station_name = cur_stn,
         variable_name = 'air_temp',
         unit = "deg. C") |> 
  select(station_name,
         month_num,
         mean_all_years_value = glob_mean_monthly_temp,
         max_all_years_value = glob_max_monthly_temp,
         min_all_years_value = glob_min_monthly_temp,
         lower_5th_percentile = glob_lower_quantile,
         upper_95th_percentile = glob_upper_quantile,
         variable_name,
         unit)

write.csv(tbl_out_monthly,
          paste0(
            'data/stat_summary/',
            cur_stn,
            '_air_temp_stats_of_each_month_for_each_year.csv'
          ),
          row.names = F)
          
write.csv(tbl_out_monthly_normals,
          paste0(
            'data/stat_summary/',
            cur_stn,
            '_air_temp_monthly_normals.csv'
          ),
          row.names = F)
