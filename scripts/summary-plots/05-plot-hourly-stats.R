
color_palette <- c(viridisLite::viridis(n = (max_year - min_year),
                                        option = 'D'),'black')

# now add in mean and cis
ggplot(qc_fill_data, aes(plot_time, Air_Temp))  +
  geom_line(aes(
    colour = as.factor(year),
    group = as.factor(year),
  ),
  alpha = ifelse(qc_fill_data$year == max_year, 1, 0.5)) +
  # geom_line(data = glob_avg, aes(x = month_num, y = glob_max_monthly_temp, group = 1, linetype = 'Max')) +
  # geom_line(data = glob_avg, aes(x = month_num, y = glob_mean_monthly_temp, group = 1, linetype = 'Mean')) +
  # geom_line(data = glob_avg, aes(x = month_num, y = glob_min_monthly_temp, group = 1, linetype = 'Min')) +
  scale_color_manual(name = 'Year', values = color_palette) +  # Assign colors based on the year
  # scale_linetype_manual(name = 'Line Type', values = c("Mean" = "dashed", "Max" = "dashed", "Min" = "dashed")) +
  scale_x_datetime(labels = scales::date_format("%B")) +
  # scale_fill_manual(name = 'Fill', values = c('5th to 95th\npercentile' = 'grey')) +
  labs(x = "Month", y = "Hourly Temperature (°C)") +
  theme_bw()

plotly::ggplotly()
