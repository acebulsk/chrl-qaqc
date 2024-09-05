sel_var <- "PC_accumulated_wtr_yr"
sel_var <- "Air_Temp"

color_palette <- c(viridisLite::viridis(n = (max_year - min_year),
                                        option = 'D'),'black')

file_path <- paste0('data/qaqc_chrl_w_ac_pc/qaqc_', cur_stn, '.rds')

hourly_df <- readRDS(file_path) |> 
  select(datetime, where(is.numeric)) |> 
  pivot_longer(!datetime) |> 
  filter(name == sel_var)|> 
  mutate(year = format(datetime, '%Y'),
         wtr_year = weatherdash::wtr_yr(datetime))

hourly_df$plot_time <- format(hourly_df$datetime, "1900-%m-%d %H:%M:%S") # 81 so not a leap year
hourly_df$plot_time <- as.POSIXct(hourly_df$plot_time, format = "%Y-%m-%d %H:%M:%S", tz = 'UTC')
hourly_df$plot_time <- if_else(month(hourly_df$plot_time) < 10,
                               weatherdash::set_yr(hourly_df$plot_time, 1901),
                               weatherdash::set_yr(hourly_df$plot_time, 1900))

file_path <- paste0(
  'data/hourly_stats/',
  cur_stn,
  '_hourly_stats.rds'
)
hourly_stats <- readRDS(file_path) |> 
  filter(name == sel_var)# |> 

hourly_stats$plot_time <- if_else(month(hourly_stats$plot_time) < 10,
                                  weatherdash::set_yr(hourly_stats$plot_time, 1901),
                                  weatherdash::set_yr(hourly_stats$plot_time, 1900))


ggplot(hourly_df)  +
  geom_line(aes(
    plot_time, value,
    colour = as.factor(wtr_year),
    group = as.factor(wtr_year),
  ))+
  geom_ribbon(
    data = hourly_stats,
    aes(
      x = plot_time,
      ymin = min,
      ymax = max,
      fill = 'Max to Min Range'
    ),
    alpha = 0.2,
    fill = 'dodgerblue',
    colour = 'blue'
  ) +
  scale_color_viridis_d(name = 'Water Year') +  # Assign colors based on the year
  # scale_linetype_manual(name = 'Line Type', values = c("Mean" = "dashed", "Max" = "dashed", "Min" = "dashed")) +
  scale_x_datetime(labels = scales::date_format("%B")) +
  # scale_fill_manual(name = 'Fill', values = c('5th to 95th\npercentile' = 'grey')) +
  labs(y = y_lab, colour = 'Water Year') +
  theme_bw() +
  theme(axis.title.x = element_blank())

gg_at <- ggplot(qc_fill_data)  +
  geom_line(aes(
    plot_time, PC_accumulated_wtr_yr,
    colour = as.factor(year),
    group = as.factor(year),
  )) +
    geom_ribbon(
      data = hourly_stats_at |> filter(name == sel_var),
      aes(
        x = plot_time,
        ymin = lower_quantile,
        ymax = upper_quantile,
        fill = '5th to 95th\npercentile'
      ),
      alpha = 0.2
    ) + 
  scale_x_datetime(labels = scales::date_format("%B")) +
  labs(x = "Month", y = "Hourly Air Temperature (°C)") +
  theme_bw()

gg_at

if(quant == T){
  gg_at <- gg_at +
  geom_line(data = hourly_stats_at,
            aes(x = plot_time, 
                y = upper_quantile, group = 1, linetype = 'Upper Quantile')) +
    geom_line(data = hourly_stats_at, 
              aes(x = plot_time, 
                  y = lower_quantile, 
                  group = 1, 
                  linetype = 'Lower Quantile')) 
} 

if(means == T){
  gg_at <- gg_at +
    geom_line(data = hourly_stats_at, aes(
      x = plot_time,
      y = max,
      group = 1,
      linetype = 'Max'
    )) +
    geom_line(data = hourly_stats_at, aes(
      x = plot_time,
      y = mean,
      group = 1,
      linetype = 'Mean'
    )) +
    geom_line(data = hourly_stats_at, aes(
      x = plot_time,
      y = min,
      group = 1,
      linetype = 'min'
    )) 
} 
gg_at

plotly::ggplotly()

hourly_stats_sd <- hourly_stats |> filter(name == 'Snow_Depth_qaqc_filled')

ggplot(qc_fill_data)  +
  geom_line(aes(
    plot_time, Snow_Depth_qaqc,
    colour = as.factor(year),
    group = as.factor(year),
  ),
  alpha = ifelse(qc_fill_data$year == max_year, 1, 0.5)) +
  geom_ribbon(
    data = hourly_stats_sd,
    aes(
      x = plot_time,
      ymin = lower_quantile,
      ymax = upper_quantile,
      fill = '5th to 95th\npercentile'
    ),
    alpha = 0.2
  ) + 
  geom_line(data = hourly_stats_sd, aes(x = plot_time, y = upper_quantile, group = 1, linetype = 'Upper Quantile')) +
  geom_line(data = hourly_stats_sd, aes(x = plot_time, y = lower_quantile, group = 1, linetype = 'Lower Quantile')) +
  geom_line(data = hourly_stats_sd, aes(x = plot_time, y = max, group = 1, linetype = 'Max')) +
  geom_line(data = hourly_stats_sd, aes(x = plot_time, y = mean, group = 1, linetype = 'Mean')) +
  geom_line(data = hourly_stats_sd, aes(x = plot_time, y = min, group = 1, linetype = 'min')) +
  # scale_color_manual(name = 'Year', values = color_palette) +  # Assign colors based on the year
  # scale_linetype_manual(name = 'Line Type', values = c("Mean" = "dashed", "Max" = "dashed", "Min" = "dashed")) +
  scale_x_datetime(labels = scales::date_format("%B")) +
  # scale_fill_manual(name = 'Fill', values = c('5th to 95th\npercentile' = 'grey')) +
  labs(x = "Month", y = "Snow Depth (cm)") +
  theme_bw()

plotly::ggplotly()
