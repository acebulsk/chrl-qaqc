# plot statistics

cur_year <- format(Sys.Date(), '%Y')

ggplot(monthly_air_temp_summary, aes(month_num, mean_monthly_temp, colour = year, group = year)) +
  geom_line()

# TODO make cur year more obviously different 

ggplot(monthly_air_temp_summary, aes(month_num, mean_monthly_temp)) + 
  geom_boxplot() +
  geom_point(data = subset(monthly_air_temp_summary, year == cur_year), colour = 'red', shape = 2)

#TODO add labels 