# script to run the full summary plot process

# setup ----

library(dplyr)
library(tidyr)
library(DBI)
library(ggplot2)
library(CRHMr)

source('config.r')

options(ggplot2.discrete.colour= c(
  "#000000",
  "#E69F00",
  "#009E73",
  "#CC79A7",
  "#F0E442",
  "#56B4E9"
))

chrl_stations <- c(
  # 'apelake',
  # 'mountcayley',
  # 'claytonfalls',
  # 'lowercain',
  # 'mountarrowsmith',
  # 'tetrahedron',
  # 'machmellkliniklini',
  # 'eastbuxton',
  # 'datlamen', #TODO
  # 'rennellpass', #TODO
  'mountmaya',
  'machmell',
  'klinaklini',
  'plummerhut',
  'homathko',
  'cainridgerun',
  'steph1',
  'steph2',
  'steph3',
  'steph4',
  'steph6',
  'steph7',
  'steph8',
  'upperrussell',
  'russellmain',
  'uppercruickshank',
  'perseverance'
)

conn <- do.call(DBI::dbConnect, args)
on.exit(DBI::dbDisconnect(conn))

for (stn in chrl_stations) {
  
  cur_stn <- stn # eventually loop through all stations...
  
  # # load data ---
  # source('scripts/summary-plots/00-load-data.R')
  # 
  # # qaqc loop ---
  # 
  # print(paste("Starting QAQC for", cur_stn))
  # 
  # # eventually pull from Juliens QAQCd data
  # 
  # source('scripts/summary-plots/01-qaqc-met-data.R')
  # source('scripts/summary-plots/02-gap-fill-met-data.R')
  # 
  # print(paste("Finished QAQC for", cur_stn))

  # stats ----
  
  print(paste("Starting stats for", cur_stn))

  source('scripts/summary-plots/03-stats-on-met-data.R')

  print(paste("Finished stats for", cur_stn))

  # plots ----

  print(paste("Starting plots for", cur_stn))

  source('scripts/summary-plots/04-plot-monthly-stats.R')

  paste("Finished plots for", cur_stn)

}
