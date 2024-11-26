################################################################################

## This script compiles reference times for each cc23 tag into a single .csv
## file. An identical RData file is also produced.
## These ref times are used to convert between tag time and utc time,
## following the functions in the dtag tools matlab scripts. This script will
## allow for similar conversions in other languages. 

## This script only needs to be run once to produce a combined table of ref times

## Written by Eli Strauss Jan 2024

################################################################################

#folder holding the cue tables and reference times
cuetab_folder <- '/Volumes/cc23_eds/cc23_cue_tables_csv/'  
setwd(cuetab_folder)
ref_time_files <- list.files(cuetab_folder, pattern = 'ref_time.csv')

ref_times_list <- list()
for(rf in ref_time_files){
  tag <- substr(gsub(rf, pattern = '..vcues_ref_time.csv', replace = ''), 2, nchar(rf))
  ref_time <- read.csv(rf, head = F)[1,1]
  ref_times_list[[length(ref_times_list)+1]] <- data.frame(tag, ref_time)
  
}
ref_times <- do.call(rbind, ref_times_list)
write.csv(ref_times, file = paste0(cuetab_folder, 'ref_times.csv'), row.names = F)
save(ref_times, file = paste0(cuetab_folder, 'ref_times.RData'))

