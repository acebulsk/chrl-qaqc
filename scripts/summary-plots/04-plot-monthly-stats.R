# plot statistics

plot_var <- 'Snow_Depth_qaqc'
y_lab <- 'Mean Monthly Snow Depth (cm)'
select_year <- 2023

monthly_summary <- monthly_summary |> 
  filter(name == plot_var)

color_palette <- c(viridisLite::viridis(n = (max_year - min_year),
                     option = 'D'),'black')

# basic without mean and CIs
ggplot(monthly_summary, aes(month_num, mean_monthly, colour = as.factor(year), group = as.factor(year)))  +
  geom_line()+
  geom_line(linewidth = ifelse(monthly_summary$year == max_year, 1, 0.5)) +  # Adjust line thickness
  scale_color_manual(name = 'Year', values = color_palette) +  # Assign colors based on the year
  labs(x = "Month", y = y_lab) +
  theme_bw()

ggsave(
  paste0(
    'figs/line-graph/monthly_',
    cur_stn,
    '_',
    plot_var,
    '_line_graph.png'
  ), width = 6, height = 4)

# plotly::ggplotly()

# now add in mean and cis
# ggplot(monthly_summary, aes(month_num, mean_monthly))  +
#   geom_ribbon(
#     aes(
#       ymin = glob_lower_quantile,
#       ymax = glob_upper_quantile,
#       fill = '5th to 95th\npercentile',
#       group = 1
#     ),
#     alpha = 0.2
#   ) +
#   geom_line(aes(
#     colour = as.factor(year),
#     group = as.factor(year),
#   ),
#   linewidth = ifelse(monthly_summary$year == max_year, 1, 0.5)) +
#   geom_line(data = glob_avg, aes(x = month_num, y = glob_max_monthly_temp, group = 1, linetype = 'Max')) +
#   geom_line(data = glob_avg, aes(x = month_num, y = glob_mean_monthly_temp, group = 1, linetype = 'Mean')) +
#   geom_line(data = glob_avg, aes(x = month_num, y = glob_min_monthly_temp, group = 1, linetype = 'Min')) +
#   scale_color_manual(name = 'Year', values = color_palette) +  # Assign colors based on the year
#   scale_linetype_manual(name = 'Line Type', values = c("Mean" = "dashed", "Max" = "dashed", "Min" = "dashed")) +
#   scale_fill_manual(name = 'Fill', values = c('5th to 95th\npercentile' = 'grey')) +
#   labs(x = "Month", y = "Mean Monthly Temperature (°C)") +
#   theme_bw()

# ggsave(
#   paste0(
#     'figs/line-graph-busy/monthly_',
#     cur_stn,
#     '_air_temp_line_graph_busy.png'
#   ), width = 6, height = 4)

# boxplot 

ggplot(monthly_summary, aes(month_num, mean_monthly)) + 
  geom_boxplot() +
  geom_point(data = subset(monthly_summary, year == select_year), aes(shape = "Select Year"), color = 'red', size = 3) +
  scale_shape_manual(values = c("Select Year" = 2)) +
  labs(x = "Month", y = y_lab,
       caption = paste0('Period of Record: ', min_date, ' to ', max_date, '. Only includes months with > 90% of data.'))  +
  theme_bw() +
  theme(legend.title = element_blank())

ggsave(paste0(
  'figs/boxplot/monthly_',
  cur_stn,
  '_air_temp_boxplot.png'
), width = 7, height = 4)
