# script to gap fill met data currently just fills small gaps
# TODO build regression for cross station filling 

library(CRHMr)

cur_stn <- 'mountcayley' # eventually loop through all stations... 

max_gap_fill_linear <- 2 # if more than two records are missing linear interpolation is not used

qc_data <- readRDS('data/qaqc/qaqc_mountcayley.rds')

CRHMr::findGaps(qc_data, gapfile = paste0('logs/', cur_stn, '_gaps.csv'), quiet = F)

at_fill <- CRHMr::interpolate(qc_data, 
                             varcols = at_col,
                             methods = 'linear', 
                             maxlength = max_gap_fill_linear)

at_still_have_gaps <- CRHMr::findGaps(at_fill, paste0('logs/', cur_stn, '_gaps.csv'), quiet = F)

if(!at_still_have_gaps == F){
  warning("Air temp still has gaps.")
}

saveRDS(at_fill, paste0('data/gap-fill/gap_fill_', cur_stn, '.rds'))

# TODO fill larger gaps using regressions from other stations
