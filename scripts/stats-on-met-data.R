# script to calculate monthly means and totals 
library(tidyverse)

perc_records_in_month <- 0.9

qc_fill_data <- readRDS('data/gap-fill/gap_fill_mountcayley.rds')

qc_fill_data$month_num <- format(qc_fill_data$datetime, '%m')
qc_fill_data$month_short <- format(qc_fill_data$datetime, '%b')
qc_fill_data$year <- format(qc_fill_data$datetime, '%Y')
qc_fill_data$records_filter <- lubridate::days_in_month(qc_fill_data$datetime)*24*perc_records_in_month

monthly_air_temp_summary <- qc_fill_data |> 
  group_by(year, month_num) |> 
  mutate(n_records_in_month = n(),
         has_enough_records = n_records_in_month > records_filter) |> 
  filter(has_enough_records == T) |> 
  summarise(
    mean_monthly_temp = mean(Air_Temp, na.rm = T)
  ) 
