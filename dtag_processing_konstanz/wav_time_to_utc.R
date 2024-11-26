################################################################################

## This script facilitates the conversion of markers created in audition into
## cue times that can be used in the dtagaudit matlab tools. The output can be
## further converted to UTC time using other functions either in the dtagaudit 
## tools or using another R function. 

## Created Jan 2024 by Ari SP, Eli S, Chi HC, Jana W. 

################################################################################

#------EDIT PARAMS-----
#input label file from audition
aud_input_folder <- '/Volumes/cc23_eds/' #THIS IS THE FOLDER WHERE THE AUDITION LABELS ARE
#folder holding the cue tables
cuetab_folder <- '/Volumes/cc23_eds/cc23_cue_tables_csv/' #THIS IS THE FOLDER WHERE THE CUE TABLES (wavcues) ARE

#AUDITION AUDIT FILE HERE
aud_file <- 'cc23_BLG034.csv' #THIS FILE CONTAINS SOME LABELS IN AUDITION

#------MAIN-----------
#get labels from audition
setwd(aud_input_folder)
aud_labs <- read.csv(aud_file, sep='\t')

#get the appropriate cue table
setwd(cuetab_folder)

#parse the name of the audition label file to get the hyena ID and wav file number 
#NOTE: MUST BE IN THE FORMAT cc23_HYENAID0XX.csv

input_file_basename <- strsplit(aud_file,'\\.')[[1]][1]

#get the file number
wavfile_num <- substring(input_file_basename, nchar(input_file_basename)-2, nchar(input_file_basename))
wavfile_id <- substring(input_file_basename, 1, nchar(input_file_basename)-3)

#open the cue table
cuetab <- read.csv(paste0(cuetab_folder,'/_',wavfile_id,'wavcues.csv'), header=F)
aud_labs$cuetime <- NA

for(i in 1:nrow(aud_labs)){
  row <- which(cuetab$V1 == as.numeric(wavfile_num))[1]
  filestart <- cuetab$V2[row]
  
  #get reference time
  #ref_time <- read.csv(file = paste0(cuetab_folder,'/_',wavfile_id,'wavcues_ref_time.csv'), header=F)$V1[1]
  
  #add together to get cue time
  lab_time <- aud_labs$Start[i]
  hour_min_sec <- as.numeric(as.character(strsplit(lab_time,':')[[1]]))
  if(length(hour_min_sec)==2){
    t <- hour_min_sec[1]*60 + hour_min_sec[2]
  } else{
    t <- hour_min_sec[1]*60*60 + hour_min_sec[2]*60 + hour_min_sec[3]
  }
  
  cuetime <- t + filestart
 
  aud_labs$cuetime[i] <- cuetime 
}



