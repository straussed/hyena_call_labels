################################################################################

## This script takes the converted predictions from animal2vec (a2v2cue.R) and 
## creates new sections to label that have not been labeled before. It looks 
## for a certain density of calls in order to find sections that are easy to 
## label.


## Written by Marius Faiß August 2025

################################################################################

library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(gtools)
library(stringr)
library(glue)
library(lubridate)

base_folder <- "/Users/mfaiss/Documents/Hyena project/hyena_call_labels"

setwd(base_folder)

source("dtag_processing_konstanz/cue2utc.R")

# folder with converted a2v predictions
a2v_predictions <- glue("{base_folder}/a2v_validation/converted_files/all_predictions_0_3")

# folder that contains reference times for UTC conversion
ref_time_folder <- "/Volumes/cc23_4/cc23_cue_tables_csv"

# labeling tracking file that contains already labeled sections
# download as .csv from google drive before running the script to have the most 
# up to date version
label_tracking <- read_csv(glue("{base_folder}/a2v_validation/Labeling Effort Tracking Spreadsheet - assignments.csv"),
                           col_select=c("tag", "time_vector", "end_time"), col_types = c("ccc"))

################################################################################
# setting parameters

# how long should the validation sections be?
file_length_min <- 3

# minimum and maximum number of labels of interest per section
# for snores: so far there are between 15 and 42 snores per minute
min_predictions <- 30
max_predictions <- 180

# which labels to focus on
use_labels <- paste(c("grn", "oth"), collapse = "|")

################################################################################

# output directory
out_dir <- glue("{base_folder}/a2v_validation/a2v_easy_validation")
if (!file.exists(out_dir)){dir.create(out_dir)}

# create output folder
out_folder <- glue("{out_dir}/groan_other_snores_{file_length_min}m_{min_predictions}-{max_predictions}lbl")
if (!file.exists(out_folder)){dir.create(out_folder)}

# list of all prediction files
all_a2v_files <- list.files(a2v_predictions, full.names = TRUE)

# get a list of all individuals
remove_digits <- function(x) gsub("[[:digit:]]", "", x)
individual_list <- c()

for (file in all_a2v_files) {
  filename <- str_split(basename(file), pattern="_")[[1]][2]
  individual <- remove_digits(filename)
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
  
  # load the already labeled sections for this individual
  labeled_sections <- label_tracking[label_tracking$tag == glue("cc23_{individual}"),]
  
  labeled_sections <- labeled_sections %>%
    mutate(
      start_datetime = ymd_hms(time_vector),
      end_datetime   = ymd_hms(end_time)
    )
  
  # Create a new column in the tibble with intervals
  labeled_sections <- labeled_sections %>%
    mutate(time_interval = interval(start_datetime, end_datetime))
  
  tag <- glue("cc23_{individual}")
  
  # get the reference time for this individual
  ref_time <- read_csv(sprintf("%s/_%swavcues_ref_time.csv", ref_time_folder, 
                               tag), col_names = FALSE, col_types = "d")
  
  ref_times <- data.frame(tag, ref_time[[1,1]])
  names(ref_times) <- c("tag", "ref_time")
  
  # iterate through this individual's files, sorted by number
  list_of_files <- mixedsort(individual_files[[individual]])
  list_of_tibbles <- list()
  
  for (filename in list_of_files) {
    # load and format prediction file
    wav_df <- read_delim(glue("{a2v_predictions}/{filename}"),
                         col_names = c("start", "length", "label"),
                         col_types = "ddc")
    
    # Add the tibble to the list
    list_of_tibbles[[filename]] <- wav_df
  }
  
  # combine all files together
  combined_tibble <- do.call(rbind, list_of_tibbles)
  
  total_start_sec <- head(combined_tibble$start, n = 1)
  total_end_sec <- tail(combined_tibble$start, n = 1)
  
  # initializing chunk start and end with 20s padding
  start_sec <- max(0, total_start_sec - 20)
  end_sec <- file_length_min * 60
  
  # go through all predictions in 10 min chunks
  stopifnot(end_sec < total_end_sec, is.finite(total_end_sec))
  while (end_sec < total_end_sec + 20) {
  
    # check if out of the wanted call types at least x are present
    # get the predictions in this start and end range
    chunked_tibble <- combined_tibble %>%
      filter(between(start, start_sec, end_sec))
    
    # remove unwanted predictions
    if (nrow(chunked_tibble) >= min_predictions) {
      filtered_tibble <- chunked_tibble %>%
        filter(str_detect(label, use_labels))
      
      # skip if not enough predictions
      n_predictions <- nrow(filtered_tibble)
      if (n_predictions >= min_predictions & n_predictions <= max_predictions) {
        
        ######## OVERLAP CHECKING ########
        # Convert the character vectors to datetime interval
        start_dt <- cue2utc(tag=tag, cue=start_sec, ref_times=ref_times)
        end_dt   <- cue2utc(tag=tag, cue=end_sec, ref_times=ref_times)
        interval_to_check <- interval(start_dt, end_dt)
        
        # Check for any overlap and add the result as a new column
        labeled_sections <- labeled_sections %>%
          mutate(overlaps = int_overlaps(time_interval, interval_to_check))
        
        # To see if there is any overlap at all in the entire tibble
        any_overlap <- any(labeled_sections$overlaps)
        
        if (any_overlap) {
          # cat("\n")
          # cat(glue("overlap: {interval_to_check}"))
        } else {
          # cat("\n")
          # cat(glue("no overlap: {interval_to_check}"))
          
          # add soa and eoa
          soa_row <- tibble(start = start_sec, length = 0.00, label = "soa")
          eoa_row <- tibble(start = end_sec, length = 0.00, label = "eoa")
          final_tibble <- bind_rows(soa_row, chunked_tibble, eoa_row)
          
          # convert interval for file name
          start_str <- format(start_dt, "%Y_%m_%d_%H_%M_%S")
          end_str   <- format(end_dt,   "%Y_%m_%d_%H_%M_%S")
          txt_interval <- paste(start_str, end_str, sep = "-")
          
          # write a text file containing all converted predictions for this hyena
          out_name <- glue("{out_folder}/{tag}_{txt_interval}.txt")
          
          final_tibble <- final_tibble %>%
            mutate(start = format(start, nsmall = 6, scientific=FALSE, trim=TRUE),
                   length = format(length, nsmall = 6, scientific=FALSE, 
                                     trim=TRUE))
          
          write_delim(final_tibble, out_name, delim = "\t", col_names = FALSE)
        }
      }
    }
    # move to the next 10 min chunk and repeat
    start_sec <- end_sec
    end_sec <- end_sec + (file_length_min * 60)
  }
}
