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

set.seed(1)

# FOLDERS
base_folder <- "/Users/mfaiss/Documents/Hyenaproject/hyena_call_labels"

setwd(base_folder)

source("dtag_processing_konstanz/cue2utc.R")

# folder with converted a2v predictions
a2v_predictions <- glue("{base_folder}/a2v_validation/converted_files/2026_01_01_large_model_final_predictions/all_predictions/confs")

# folder that contains the a2v labels
a2v_labels_folder <- glue("{base_folder}/hyena_a2v_labels")

# PARAMETERS
# the buffer in minutes before and after the label sections
buffer_min <- 30

# the number of days to use for validation
days <- 20

# the call type labels to use for validation
call_labels <- c("whp", "grn", "gig", "rum", "sql", "gwl", "fed", "str", "oth", "snr")

# number of examples per call type to pick for validation
n_examples <- 20

################################################################################

# output directory
out_dir <- glue("{base_folder}/a2v_validation/PRvalidation")
if (!file.exists(out_dir)){dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)}

# load all a2v label files, filter to only soa and eoa
all_labels <- read_csv(list.files(path = a2v_labels_folder, 
                                  pattern = "\\.csv$", 
                                  full.names = TRUE))
all_labels <- all_labels[all_labels$label %in% c("soa", "eoa"), ]

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
  for (filename in all_a2v_files) {
    if (str_detect(filename, individual)) {
      file_list <- c(file_list, filename)
    }
  }
  individual_files[[individual]] <- file_list
}

# create validation files for each individual
for (individual in names(individual_files)) {
  cat(sprintf("Creating validation files for: %s\n", individual))

  tag <- glue("cc23_{individual}")
  
  # load the already labeled sections for this individual
  labeled_sections <- all_labels[all_labels$tag == tag,]

  # get the start and end of audit timestamps and add buffer around them
  starts <- labeled_sections$tag_time[labeled_sections$label == "soa"] - buffer_min * 60
  ends   <- labeled_sections$tag_time[labeled_sections$label == "eoa"] + buffer_min * 60
  sections <- data.frame(start_time = starts, end_time = ends)

  # read this individuals predictions with confidence scores
  predictions <- read_delim(mixedsort(individual_files[[individual]]), 
                           col_names = FALSE, 
                           delim = "\t")
  predictions <- rename(predictions, start = X1, duration = X2, label = X3, call_conf = X4, foc_conf = X5)
  
  # calculate which days the predictions are from
  predictions$day <- floor(predictions$start / 86400)

  # create an empty dataframe to store the results
  valid_preds <- predictions[0, ]
  
  # iterate over the call types and produce validation files
  for (call_type in call_labels) {
    cat(sprintf("Processing call type: %s\n", call_type))

    # keep track of picked times so they don't cluster
    picked_timestamps <- numeric(0)
    target_n <- n_examples / 2

    # NON-FOCAL CALLS
    # pick n examples for each call type with non-focal calls
    pool_non <- predictions %>% 
      filter(grepl(call_type, label), grepl("non", label)) %>%
      # 1. create confidence bins
      mutate(conf_bin = cut(call_conf, breaks = c(0, seq(0.2, 1.0, by = 0.1)), include.lowest = TRUE)) %>%
      # 2. group by the bin to calculate how many exist
      group_by(conf_bin) %>%
      # 3. save the total population for later, and the probability for now
      mutate(
        bin_total = n(),      # for the evaluation math later
        sample_prob = 1 / n() # for the sample() function
      ) %>%
      ungroup()

    available_days <- unique(pool_non$day)
    available_days <- available_days[available_days <= days - 1]

    if (length(available_days) == 0) {
      cat(sprintf("    Warning: No non-focal data found for %s. Skipping.\n", call_type))
    } else {
      success_count <- 0
      attempts <- 0
      day_queue <- sample(available_days) # shuffle days into a queue
      
      while (success_count < target_n && attempts < 2000) {
        attempts <- attempts + 1
        
        # pop the first day off the queue
        if (length(day_queue) == 0) day_queue <- sample(available_days) # Refill queue if empty
        current_day <- day_queue[1]
        day_queue <- day_queue[-1] 
        
        # get predictions just for this day
        day_preds <- pool_non[pool_non$day == current_day, ]
        
        # pick one random prediction from this day with confidence score weighting
        candidate_row <- day_preds[sample(nrow(day_preds), size = 1, prob = day_preds$sample_prob), ]
        candidate_time <- candidate_row$start
        
        # check if it is within an already labeled section
        in_section <- any(candidate_time >= sections$start_time & candidate_time <= sections$end_time)
        
        # check id it is close to already picked timestamps
        is_too_close <- FALSE
        if (length(picked_timestamps) > 0) {
          is_too_close <- any(abs(candidate_time - picked_timestamps) < buffer_min * 60)
        }
        
        # save the prediction
        if (!in_section && !is_too_close) {
          valid_preds <- rbind(valid_preds, candidate_row)
          picked_timestamps <- c(picked_timestamps, candidate_time)
          success_count <- success_count + 1
        }
      }
      
      if (success_count < target_n) cat(sprintf("    Warning: Only found %d/%d non-focal calls.\n", success_count, target_n))
    }
    
    # FOCAL CALLS
    pool_foc <- predictions %>% 
      filter(grepl(call_type, label), grepl("foc", label)) %>%
      mutate(conf_bin = cut(call_conf, breaks = c(0, seq(0.2, 1.0, by = 0.1)), include.lowest = TRUE)) %>%
      mutate(bin_total = n(), sample_prob = 1 / n()) %>%
      ungroup()

    available_days <- unique(pool_foc$day)
    
    if (length(available_days) == 0) {
      cat(sprintf("    Warning: No focal data found for %s. Skipping.\n", call_type))
    } else {
      success_count <- 0
      attempts <- 0
      day_queue <- sample(available_days)
      
      while (success_count < target_n && attempts < 2000) {
        attempts <- attempts + 1
        
        if (length(day_queue) == 0) day_queue <- sample(available_days)
        current_day <- day_queue[1]
        day_queue <- day_queue[-1]
        
        day_preds <- pool_foc[pool_foc$day == current_day, ]
        
        candidate_row <- day_preds[sample(nrow(day_preds), size = 1, prob = day_preds$sample_prob), ]
        candidate_time <- candidate_row$start
        
        in_section <- any(candidate_time >= sections$start_time & candidate_time <= sections$end_time)
        
        is_too_close <- FALSE
        if (length(picked_timestamps) > 0) {
          is_too_close <- any(abs(candidate_time - picked_timestamps) < buffer_min * 60)
        }
        
        if (!in_section && !is_too_close) {
          valid_preds <- rbind(valid_preds, candidate_row)
          picked_timestamps <- c(picked_timestamps, candidate_time)
          success_count <- success_count + 1
        }
      }
      
      if (success_count < target_n) cat(sprintf("    Warning: Only found %d/%d focal calls.\n", success_count, target_n))
    }
  }

  if (nrow(valid_preds) > 0) {
    # order predictions by start time
    valid_preds <- valid_preds[order(valid_preds$start),]

    # save prediction file with confidence scores and ecaliation weights
    valid_preds <- valid_preds %>%
      group_by(label, conf_bin) %>%
      mutate(
        n_sampled = n(),
        eval_weight = bin_total / n_sampled
      ) %>%
      ungroup()

    write_delim(valid_preds, glue("{base_folder}/a2v_validation/PRvalidation/selected_predictions/cc23_{individual}_predictions_conf.txt"), delim = "\t")

    # remove columns for cue validation file
    cues_to_validate <- valid_preds %>% select(start, duration, label)

    # save validaton file
    write_delim(cues_to_validate, glue("{base_folder}/a2v_validation/PRvalidation/to_validate/cc23_{individual}_validation_cues.txt"), delim = "\t", col_names = FALSE)
  }
}
