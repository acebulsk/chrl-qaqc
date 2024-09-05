# main script to create the stats fuls and output some summary plots
# these stats files are currently used at viu-hydromet-wx.ca for the stats pages

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
  'upperskeena',
  'apelake',
  'mountcayley',
  'claytonfalls',
  'lowercain',
  'mountarrowsmith',
  'tetrahedron',
  'machmellkliniklini',
  'eastbuxton',
  # 'datlamen',no met qc as of Feb 9
  'rennellpass',
  'mountmaya',
  # 'machmell', #no met qc as of Feb 9
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

for (stn in chrl_stations) {
  
  cur_stn <- stn # eventually loop through all stations...
  
  # # load data ---
  conn <- do.call(DBI::dbConnect, args)
  on.exit(DBI::dbDisconnect(conn))
  source('scripts/summary-plots/00-load-data.R')

  # # qaqc loop ---
  # 
  # print(paste("Starting QAQC for", cur_stn))
  # 
  # # source('scripts/summary-plots/01-qaqc-precip-data.R') # this only handles air temp and is depreciated
  # 
  # source('scripts/summary-plots/01-qaqc-precip-data.R')  # this applies a secondary qc to the raw pipe data sergey and julien created, although their qc is good there is still a bit of loss in some of the data and we need to fill gaps for the stats
  # 
  # print(paste("Finished QAQC for", cur_stn))
  # 
  # # stats ----

  # print(paste("Starting stats for", cur_stn))
  # 
  # source('scripts/summary-plots/02-stats-on-met-data.R')
  # 
  # print(paste("Finished stats for", cur_stn))

  # plots ----

  # print(paste("Starting plots for", cur_stn))
  # 
  # source('scripts/summary-plots/04-plot-monthly-stats.R')
  # 
  # paste("Finished plots for", cur_stn)

}

# Move all log files into /logs
files <- list.files(pattern = "*.log", full.names = TRUE, recursive = F)

# Move files to the log directory
file.copy(files, "logs")

# Remove original files
file.remove(files)
