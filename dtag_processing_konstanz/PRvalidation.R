################################################################################

## This script was written for the validation of the a2v predictions after the
## fully trained model has been used to predict calls in the entire dataset. We
## aim to evaluate the validity of the reported metrics on the initial 
## validation set by randomly sampling calls from the entire dataset to gain a 
## representative sample and to mitigate a potential bias that may have been 
## introduced by the selection of the initial validation set.
## The script takes the converted predictions from animal2vec and creates txt
## files with the calls to be validated so that validators can easily skip 
## through predictions and correct them if needed.


## Written by Marius Faiß April 2026

################################################################################

library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(gtools)
library(stringr)
library(glue)
library(lubridate)

# FOLDERS
base_folder <- "/Users/mfaiss/Documents/Hyenaproject/hyena_call_labels"

setwd(base_folder)

source("dtag_processing_konstanz/cue2utc.R")

# folder with converted a2v predictions
a2v_predictions <- glue("{base_folder}/a2v_validation/converted_files/2026_01_01_large_model_final_predictions/all_predictions/confs")

# folder that contains reference times for UTC conversion
ref_time_folder <- "/Users/mfaiss/Documents/Hyenaproject/Hyena_data/cc23_cue_tables_csv"

# folder that contains the a2v labels
a2v_labels_folder <- glue("{base_folder}/hyena_a2v_labels")

# PARAMETERS
# the buffer in minutes before and after the label sections
buffer_min <- 30

################################################################################

# output directory
out_dir <- glue("{base_folder}/a2v_validation/PRvalidation")
if (!file.exists(out_dir)){dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)}

# load all a2v label files
label_list <- list.files(path = a2v_labels_folder, 
                        pattern = "\\.csv$", 
                        full.names = TRUE)
all_labels <- read_csv(label_list)

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
  labeled_sections <- all_labels[all_labels$tag == glue("cc23_{individual}"),]

  # get the start and end of audit timestamps
  
  
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
