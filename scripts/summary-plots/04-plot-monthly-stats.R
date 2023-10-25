# plot statistics

color_palette <- c(viridisLite::viridis(n = (cur_year - min_year),
                     option = 'D'),'black')

monthly_air_temp_summary$is_year <- monthly_air_temp_summary$year == cur_year

# basic without mean and CIs
ggplot(monthly_air_temp_summary, aes(month_num, mean_monthly_temp, colour = as.factor(year), group = as.factor(year)))  +
  geom_line()+
  geom_line(linewidth = ifelse(monthly_air_temp_summary$year == cur_year, 1, 0.5)) +  # Adjust line thickness
  scale_color_manual(name = 'Year', values = color_palette) +  # Assign colors based on the year
  labs(x = "Month", y = "Mean Monthly Temperature (°C)") +
  theme_bw()

ggsave('figs/monthly_mountcayley_air_temp_line_graph.png', width = 6, height = 4)

plotly::ggplotly()

# now add in mean and cis
ggplot(monthly_air_temp_summary, aes(month_num, mean_monthly_temp))  +
  geom_ribbon(
    aes(
      ymin = glob_lower_quantile,
      ymax = glob_upper_quantile,
      fill = '5th to 95th\npercentile',
      group = 1
    ),
    alpha = 0.2
  ) +
  geom_line(aes(
    colour = as.factor(year),
    group = as.factor(year),
  ),
  linewidth = ifelse(monthly_air_temp_summary$year == cur_year, 1, 0.5)) +
  geom_line(data = glob_avg, aes(x = month_num, y = glob_max_monthly_temp, group = 1, linetype = 'Max')) +
  geom_line(data = glob_avg, aes(x = month_num, y = glob_mean_monthly_temp, group = 1, linetype = 'Mean')) +
  geom_line(data = glob_avg, aes(x = month_num, y = glob_min_monthly_temp, group = 1, linetype = 'Min')) +
  scale_color_manual(name = 'Year', values = color_palette) +  # Assign colors based on the year
  scale_linetype_manual(name = 'Line Type', values = c("Mean" = "dashed", "Max" = "dashed", "Min" = "dashed")) +
  scale_fill_manual(name = 'Fill', values = c('5th to 95th\npercentile' = 'grey')) +
  labs(x = "Month", y = "Mean Monthly Temperature (°C)") +
  theme_bw()

# boxplot 

ggplot(monthly_air_temp_summary, aes(month_num, mean_monthly_temp)) + 
  geom_boxplot() +
  geom_point(data = subset(monthly_air_temp_summary, year == cur_year), aes(shape = "Current Year"), color = 'red', size = 3) +
  scale_shape_manual(values = c("Current Year" = 2)) +
  labs(x = "Month", y = "Mean Monthly Temperature (°C)")  +
  theme_bw() +
  theme(legend.title = element_blank())

ggsave('figs/monthly_mountcayley_air_temp_bar_graph.png', width = 6, height = 4)



ggplot(qc_fill_data, aes(month_num, Air_Temp)) + 
  geom_boxplot() +
  geom_point(data = subset(qc_fill_data, year == cur_year), aes(shape = "Current Year"), color = 'red', size = 3) +
  scale_shape_manual(values = c("Current Year" = 2)) +
  labs(x = "Month", y = "Hourly Temperature (°C)")  +
  theme_bw() +
  theme(legend.title = element_blank())
