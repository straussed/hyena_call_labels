################################################################################

## This script converts the prediction files form animal2vec back to cue files 
## that can be read into the matlab labeling and validation tool. The prediction
## files are for each wav file and give the times within the wav file itself.
## This script converts the times to be relative to the start of the collar and
## creates one long file including all predictions for each hyena.


## Written by Marius Faiß January 2025

################################################################################

library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(gtools)
library(stringr)

# to filter for only groan or only feeding predictions etc.
# enter "feeding" or "groan", or "all" to not filter the predictions
filter_calls <- "all"

# filter out low confidence score predictions, set to 0 to get all predictions 
min_confidence <- 0.3

# add confidence score to label
add_conf <- FALSE

# convert call names to label cues
replacement_map <- c(
  "groan" = "grn",
  "feeding" = "fed",
  "whoop" = "whp",
  "squitter" = "str",
  "giggle" = "gig",
  "alarm_rumble" = "rum",
  "squeal" = "sql",
  "growl" = "gwl",
  "cub_whoop" = "whp cub",
  "nf" = "non"
)

################################################################################

# the location of the cue tables containing the relative time of each wav file 
# to the collar start time
cue_tables <- "/Volumes/cc23_4/cc23_cue_tables_csv"

# location of the animal2vec predictions
# a2v_files <- "/Volumes/EAS_shared/hyena/working/analysis/animal2vec/predictions/2025_01_17_round_two/csv"
a2v_files <- "/Users/mfaiss/Documents/Hyena project/hyena_call_labels/a2v_validation/2025_01_17_round_two"

# output directory where the folder with converted cue files will be
out_dir <- "/Users/mfaiss/Documents/Hyena project/hyena_call_labels/a2v_validation/converted_files"

################################################################################

# process predictions for one wav file
wav_preds <- function(file_name, cue_table){
  cat(file_name, "\n")
  
  # load and format prediction file
  wav_df <- read_delim(sprintf("%s/%s", a2v_files, file_name), 
                       delim = "\t", col_types = "cccc", 
                       col_select=c(Start, Duration, Name, Description))
  
  # remove predictions with low confidence score
  wav_df <- wav_df %>% filter(Description >= min_confidence)
  
  # filter specific prediction type
  if (filter_calls != "all") {
    wav_df <- wav_df %>% filter(str_detect(Name, filter_calls))
  }
  
  # convert the call names to cue labels
  wav_df <- wav_df %>%
    mutate(Name = str_replace_all(Name, replacement_map))  %>%
    mutate(Name = if_else(
      !str_detect(Name, "non"), paste0(Name, " foc"), Name
    ))

  if (add_conf) {
    wav_df$Description <- as.character(wav_df$Description)
    wav_df$Description[is.na(wav_df$Description)] <- ""
    
    # combine the predicted label and confidence score into one column
    wav_df <- wav_df %>% mutate(Name = paste(Name, Description)) %>%
      select(-Description)
  } else {
    wav_df$Description <- NULL
  }
  
  
  # convert time to seconds
  wav_df <- wav_df %>%
    mutate(Start = sapply(Start, function(x) {
      parts <- as.numeric(str_split(x, ":")[[1]])
      parts[1] * 3600 + parts[2] * 60 + parts[3]
    }),
    Duration = sapply(Duration, function(x) {
      parts <- as.numeric(str_split(x, ":")[[1]])
      parts[1] * 3600 + parts[2] * 60 + parts[3]
    }))
  
  wav_df$Start <- as.numeric(wav_df$Start)
  wav_df$Duration <- as.numeric(wav_df$Duration)
  
  # get reference time from cue table and add to all start time values
  # extract the wav file number from the file name
  number <- parse_number(str_split(file_name, "_")[[1]][2])
  # find the corresponding rows in the cue table and use the lower one as 
  # the start time relative to the collar, add it to all start times in the 
  # wav file
  cue_values <- cue_table %>% filter(X1 == number)
  start_time <- min(cue_values$X2)
  wav_df <- wav_df %>% mutate(Start = Start + start_time)
  
  return(wav_df)
}

################################################################################

# create output folder
if (min_confidence != 0){
  out_folder <- sprintf("%s/%s_predictions_%s", out_dir, filter_calls, 
                        gsub("\\.", "_", min_confidence))
} else{
  out_folder <- sprintf("%s/%s_predictions", out_dir, filter_calls)
}

if (!file.exists(out_folder)){
  dir.create(out_folder)
}

# list of all prediction files
all_a2v_files <- list.files(a2v_files, full.names = TRUE)

# get a list of all individuals
remove_digits <- function(x) gsub("[[:digit:]]", "", x)
individual_list <- c()

for (file in all_a2v_files) {
  filename <- str_split(basename(file), pattern="_")[[1]][2]
  individual <- str_split(filename, "[.]")[[1]][1]  %>% remove_digits()
  if (!(individual %in% individual_list)) {
    individual_list <- c(individual_list, individual)
  }
}

# gather all files for each individual
individual_files <- list()
for (individual in individual_list) {
  file_list <- c()
  for (file in all_a2v_files) {
    filename <- basename(file)
    if (str_detect(filename, individual)) {
      file_list <- c(file_list, filename)
    }
  }
  individual_files[[individual]] <- file_list
}

# convert all files for each individual
for (individual in names(individual_files)) {
  cat(sprintf("Converting files for: %s\n", individual))
  
  # load cue table for this individual
  cue_table <- read_csv(sprintf("%s/_cc23_%swavcues.csv", cue_tables, 
                                individual), col_names = FALSE)
  
  # iterate through this individual's files, sorted by number
  list_of_files <- mixedsort(individual_files[[individual]])
  
  ##############################################################################
  # filtering for one type of prediction, only results in one large file
  if (filter_calls != "all") {
    
    if (min_confidence != 0) {
      out_name <- sprintf("%s/cc23_%s_%s_predictions_%s.txt", 
                          out_folder, individual, filter_calls, 
                          gsub("\\.", "_", min_confidence))
    }
    else {
      out_name <- sprintf("%s/cc23_%s_%s_predictions.txt", out_folder, 
                          individual, filter_calls)
    }
    
    
    if (!file.exists(out_name)) {
      # empty list to put all wav file predictions into
      df_list <- list()
      
      for (filename in list_of_files) {
        
        # use function to convert file
        wav_df <- wav_preds(filename, cue_table)

        # add converted prediction file to the list
        df_list <- append(df_list, list(wav_df))
      }
      
      # combine all wav predictions into one dataframe and format numbers
      individual_df <- bind_rows(df_list) %>%
        mutate(Start = format(Start, nsmall = 6, scientific=FALSE, trim=TRUE),
               Duration = format(Duration, nsmall = 6, scientific=FALSE, 
                                 trim=TRUE))
      
      # write a text file containing all converted predictions for this hyena
      write_delim(individual_df, out_name, delim = "\t", col_names = FALSE)
    }
  }
  
  ##############################################################################
  # export files individually if all predictions are included
  if (filter_calls == "all"){
    for (filename in list_of_files){
      
      # extract the wav file number from the file name
      number <- str_pad(parse_number(str_split(filename, "_")[[1]][2]), 3, pad="0")
      
      if (min_confidence != 0) {
        out_name <- sprintf("%s/cc23_%s%s_%s_predictions_%s.txt", 
                            out_folder, individual, number, filter_calls, 
                            gsub("\\.", "_", min_confidence))
      }
      else {
        out_name <- sprintf("%s/cc23_%s%s_%s_predictions.txt", 
                            out_folder, individual, number, filter_calls)
      }
      
      if (!file.exists(out_name)) {

        # use function to convert file
        wav_df <- wav_preds(filename, cue_table)
      
        wav_df <- wav_df %>%
          mutate(Start = format(Start, nsmall = 6, scientific=FALSE, trim=TRUE),
                 Duration = format(Duration, nsmall = 6, scientific=FALSE, 
                                   trim=TRUE))
        
        # write a text file containing all converted predictions for this hyena
        write_delim(wav_df, out_name, delim = "\t", col_names = FALSE)
      }
    }
  }
}
