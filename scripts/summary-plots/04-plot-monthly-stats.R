# plot statistics

cur_stn <- 'perseverance'
# plot_var <- 'Snow_Depth'
# plot_var <- 'PC_accumulated_wtr_yr'
plot_var <- 'PC_monthly_total'
y_lab <- 'PC_accumulated_wtr_yr'
select_year <- c(2019, 2020)

col_names <- c('line_group', 'month_name', 'value')

monthly_summary <- readRDS(paste0(
  '../../../code/r-pkgs/chrl-graph/data/monthly_normals_plot_data/',
  cur_stn,
  '_monthly_normals_data.rds'
)) |> 
  filter(name == plot_var) |> 
  select(line_group = WtrYr, month_name, value = mean_monthly) |> 
  filter(line_group %in% select_year)

yearly_stats_df <- readRDS(paste0(
  '../../../code/r-pkgs/chrl-graph/data/yearly_mean_monthly_summary/',
  cur_stn,
  '_yearly_mean_monthly_summary.rds'
)) |> 
  filter(name == plot_var)  |> 
  pivot_longer(c(Max:Min), names_to = 'line_group') |> 
  select(line_group, month_name, value)

month_line_df <- rbind(monthly_summary, yearly_stats_df) |> 
  mutate(line_group = ordered(line_group, levels = c('Min', select_year, 'Max')))

ggplot(month_line_df, aes(month_name, value, colour = line_group, group = line_group))  +
  geom_line() +
  geom_point(size = 0.5)+
  scale_color_viridis_d(name = 'Colour') +  # Assign colors based on the year
  labs(x = "Month", y = y_lab,
       caption = paste0('Only includes months with > 90% of data. Red line shows the selected water year.')) +
  theme_bw(base_size = 14) +
  theme(legend.position = 'bottom')

# if its pc totals show bar plot... 
ggplot(month_line_df, aes(x = month_name, y = value, fill = line_group))  +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d(name = '') +  # Assign colors based on the year
  labs(x = "Month", y = y_lab,
       caption = paste0('Only includes months with > 90% of data. Red line shows the selected water year.')) +
  theme_bw(base_size = 14) +
  theme(legend.position = 'bottom')

# basic without mean and CIs
ggplot(monthly_summary, aes(month_name, mean_monthly))  +
  geom_line(aes(
    colour = as.factor(wtr_year),
    group = as.factor(wtr_year),
  ),
  linetype = ifelse(monthly_summary$wtr_year == select_year, 'dashed', 'solid')
  ) +
  geom_point(aes(colour = as.factor(wtr_year)), size = 0.5)+
  geom_errorbar(data = monthly_summary_select_year, aes(x = month_name, ymin = min_monthly, ymax = max_monthly, colour = as.factor(wtr_year)), width = 0.25)+
  scale_color_manual(name = 'Water Year', values = color_palette) +  # Assign colors based on the year
  labs(x = "Month", y = y_lab,
       caption = paste0('Only includes months with > 90% of data. Dashed red line is the selected water year.')) +
  theme_bw(base_size = 14) +
  theme(legend.position = 'bottom')

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
