# plot statistics

cur_year <- format(Sys.Date(), '%Y') |> as.numeric()
min_year <- min(monthly_air_temp_summary$year) |> as.numeric()

color_palette <- c(viridisLite::viridis(n = (cur_year - min_year),
                     option = 'D'),'red')

monthly_air_temp_summary$is_year <- monthly_air_temp_summary$year == cur_year

ggplot(monthly_air_temp_summary, aes(month_num, mean_monthly_temp, colour = as.factor(year), group = as.factor(year)))  +
  geom_line()+
  geom_line(linewidth = ifelse(monthly_air_temp_summary$year == cur_year, 1, 0.5)) +  # Adjust line thickness
  scale_color_manual(name = 'Year', values = color_palette) +  # Assign colors based on the year
  labs(x = "Month", y = "Mean Monthly Temperature (°C)") +
  theme_bw()

ggsave('figs/monthly_mountcayley_air_temp_line_graph.png', width = 6, height = 4)

plotly::ggplotly()

ggplot(monthly_air_temp_summary, aes(month_num, mean_monthly_temp)) + 
  geom_boxplot() +
  geom_point(data = subset(monthly_air_temp_summary, year == cur_year), aes(shape = "Current Year"), color = 'red', size = 3) +
  scale_shape_manual(values = c("Current Year" = 2)) +
  labs(x = "Month", y = "Mean Monthly Temperature (°C)")  +
  theme_bw() +
  theme(legend.title = element_blank())

ggsave('figs/monthly_mountcayley_air_temp_bar_graph.png', width = 6, height = 4)

#TODO add labels 

ggplot(qc_fill_data, aes(month_num, Air_Temp)) + 
  geom_boxplot() +
  geom_point(data = subset(qc_fill_data, year == cur_year), aes(shape = "Current Year"), color = 'red', size = 3) +
  scale_shape_manual(values = c("Current Year" = 2)) +
  labs(x = "Month", y = "Hourly Temperature (°C)")  +
  theme_bw() +
  theme(legend.title = element_blank())
