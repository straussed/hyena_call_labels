################################################################################

## This function converts from tag time to time relative to a specific file. 
## The purpose of the function is to convert the output from labeling in 
## dtagtools to be used by Julian and Marie. 


## Written by Eli Strauss March 2024

################################################################################

### Arguments
# cue: cue(s) in tag time, such as a number output by dtagaudit_sound labeling
# cuetab: cue table specifying start times for different files


cue2wav <- function(cue, cuetab){
  
  df <- data.frame(tag_time = cue)
  df$wav_num <- dplyr::left_join(df, cuetab, dplyr::join_by(closest(tag_time > V2)))$V1
  df$file_start <- dplyr::left_join(df, cuetab, dplyr::join_by(closest(tag_time > V2)))$V2
  df$wav_time <- df$tag_time - df$file_start
  
  return(df[,c('tag_time', 'wav_time', 'wav_num')])
  
}





