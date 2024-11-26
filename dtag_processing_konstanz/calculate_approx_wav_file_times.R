#####################
# This script calculates approximate (not time synched) start and end times
# of wav files to guide pretraining

# runs very slowly because it loads every wav file

library(XML)
library(tuneR)

tags <- list.dirs('/Volumes/cc23_eds/cc23')[-1]
tags <- tags[!grepl(x=tags, pattern = 'failed')]

file_times_list <- list()
for(tag_dir in tags){
  xml_files <- list.files(tag_dir, pattern = '.xml')
  for(xml_file in xml_files){
    file_times_list[[length(file_times_list)+1]] <- 
      file_times_from_xml(xml_file_path = paste(tag_dir, xml_file, sep = '/'))
  }
}
file_times <- do.call(rbind, file_times_list)

write.csv(file_times, file = '/Volumes/cc23_eds/dtag_processing_konstanz/wav_times.csv', row.names = F)

