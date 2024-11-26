################################################################################

## This function converts from tag time to UTC time to be used in analyses 
## in combination with the GPS data. This follows the same procedure used in
## cue2utc in the dtagaudit tools, but converts to an R formatted date. It interacts
## with the ref_times which need to be supplied either as an object or a file

#### NOTE: the output of this function contains miliseconds that are not displayed
#### when asked to print but are correctly tracked when doing math with dates. This
#### can sometimes produces 1 sec differences between this time and the time in matlab,
#### but analyses should not be impacted by this 1 sec difference (its only a printing issue)


## Written by Eli Strauss Jan 2024

################################################################################

### Arguments
# tag: tag name(s), in dtagaudit style ('cc23_HYENAID'), for instance 'cc23_BLG'
# cue: cue(s) in tag time, such as a number output by dtagaudit_sound labeling
# ref_times: object containing reference times for each tag. 
# ref_times_file: path to .csv file containing reference times. Not necessary if ref_times supplied

cue2utc <- function(tag, cue, ref_times = NULL, ref_times_file = NULL){
  if(is.null(ref_times) & is.null(ref_times_file)){
    stop('Need to provide reference times as an object or a path to external .csv file')
  }else if(is.null(ref_times) & !is.null(ref_times_file)){
    ref_times <- read.csv(ref_times_file)
  }else{
    ref_times <- ref_times
  }
  
  if(length(tag) > 1 & length(cue) > 1 & length(cue) != length(tag))
    stop('tag and cue inputs must be length 1 or the same length')
  
  rt <- ref_times[match(tag, ref_times$tag),]$ref_time
  date <- as.POSIXct(rt + cue, origin = '1970-01-01 00:00:00', tz = 'UTC')
  return(date)
}
