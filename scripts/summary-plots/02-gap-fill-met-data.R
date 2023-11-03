# script to gap fill met data currently just fills small gaps
# TODO build regression for cross station filling 

gap_fill_path <- paste0('data/gap-fill/gap_fill_', cur_stn, '.rds')
qaqc_data_path <- paste0('data/qaqc/qaqc_', cur_stn, '.rds')


max_gap_fill_linear <- 3 # if more than two records are missing linear interpolation is not used

qc_data <- readRDS(qaqc_data_path)

CRHMr::findGaps(qc_data, gapfile = paste0('logs/', cur_stn, '_gaps.csv'), quiet = F)

at_fill <- CRHMr::interpolate(qc_data, 
                             varcols = at_col,
                             methods = 'linear', 
                             maxlength = max_gap_fill_linear)

at_still_have_gaps <- CRHMr::findGaps(at_fill, paste0('logs/', cur_stn, '_gaps.csv'), quiet = F)

if(!at_still_have_gaps == F){
  warning(paste0(cur_stn, " Air temp still has gaps."))
}

saveRDS(at_fill, gap_fill_path)

# TODO fill larger gaps using regressions from other stations
