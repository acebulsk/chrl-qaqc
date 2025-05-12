# a script to compare raw vs clean tables 

library(dplyr)
library(tidyr)
library(DBI)
library(ggplot2)

source('config.r')

conn <- do.call(DBI::dbConnect, args)
on.exit(DBI::dbDisconnect(conn))

cur_stn <- 'lowercain' # eventually loop through all stations... 

query <-
  paste0("SELECT * FROM clean_",
         cur_stn,
         ";")
clean_data <- dbGetQuery(conn, query)

query <-
  paste0("SELECT * FROM raw_",
         cur_stn,
         ";")

raw_data <- dbGetQuery(conn, query)

raw_data_slim <- raw_data |> 
  select(DateTime, snow_depth = SDepth) |> 
  mutate(group = 'raw')

clean_data_slim <- clean_data |> 
  select(DateTime, snow_depth = Snow_Depth) |> 
  mutate(group = 'clean')

rbind(raw_data_slim, clean_data_slim) |> 
  ggplot(aes(DateTime, snow_depth, colour = group, group = group)) + 
  geom_line()

plotly::ggplotly()
