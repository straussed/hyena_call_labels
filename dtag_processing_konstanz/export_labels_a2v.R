
## Function for exporting parsed labels to csvs for each file
export_labels_a2v <- function(parsed_audits, destination = getwd()){
  for(f in unique(parsed_audits$wav_file)){
    to_write <- parsed_audits[parsed_audits$wav_file == f,]
    write.csv(to_write, file = paste0(destination, gsub(to_write$wav_file[1], pattern = '.wav', replacement = '.csv',)), row.names = F)
  }
  print(paste0('Files exported to ', destination))
}



