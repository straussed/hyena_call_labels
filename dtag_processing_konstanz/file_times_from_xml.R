
file_times_from_xml<- function(xml_file_path){
  
  tag = strsplit(xml_file_path, split = '/')[[1]][5]
  wav_name <- gsub(x = strsplit(xml_file_path, split = '/')[[1]][6],
                   pattern = '.xml', replace = '.wav')
  sound <- readWave(gsub(xml_file_path, pattern = 'xml', replace = 'wav'), header = T)
  duration <- sound$samples/sound$sample.rate
  time_line <- readLines(xml_file_path, 9)[9]
  time_line <- substr(time_line, start = 14, stop = nchar(time_line)-2)
  start_time <- as.POSIXct(time_line, format = '%Y,%m,%d,%H,%M,%S')
  end_time <- start_time + duration
  
  
  start_time_local <- start_time + 60*60*3
  end_time_local <- end_time + 60*60*3
  
  return(data.frame(tag, wav_name, start_time_local, end_time_local))
}


# ## This way doesn't work because file doesn't always contain the end time
# file_times_from_xml <- function(xml_file_path){
# 
#   tag = strsplit(xml_file_path, split = '/')[[1]][5]
#   wav_name <- gsub(x = strsplit(xml_file_path, split = '/')[[1]][6],
#                    pattern = '.xml', replace = '.wav')
# 
#   error_check <- try(xmlParse(xml_file_path), silent = T)
#   if(class(error_check)[1] == 'try-error'){
#     time_line <- readLines(xml_file_path, 9)[9]
#     time_line <- substr(time_line, start = 14, stop = nchar(time_line)-2)
#     start_time <- as.POSIXct(time_line, format = '%Y,%m,%d,%H,%M,%S')
#     end_time <- NA
#   }else{
#     xml_data <- xmlParse(xml_file_path)
#     start_state <- xmlGetAttr(getNodeSet(xml_data, "//EVENT/START[@STATE]")[[1]], 'STATE')
#     events <- getNodeSet(xml_data, "//EVENT[@TIME]")
# 
#     if(start_state == 'NEW'){
#       if (length(events) >= 2) {
#         times <- sapply(events[(length(events)-1):length(events)], function(event) xmlGetAttr(event, "TIME"))
#         times <- as.POSIXct(times, format = '%Y,%m,%d,%H,%M,%S')
#         start_time <- times[1]
#         end_time <- times[2]
#       } else {
#         stop("Not enough <EVENT> elements with a TIME attribute found; start_state = NEW")
#       }
#     }else if (start_state == 'REOPEN'){
#       start_time <- sapply(events[(length(events)-1):length(events)], function(event) xmlGetAttr(event, "TIME"))
#       start_time <- as.POSIXct(start_time, format = '%Y,%m,%d,%H,%M,%S')
#       end_time <- NA
#     }else{
#       print("start state parsing failed")
#     }
#   }
# 
#   start_time_local <- start_time + 60*60*3
#   end_time_local <- end_time + 60*60*3
# 
#   return(data.frame(tag, wav_name, start_time_local, end_time_local))
# }
# 
