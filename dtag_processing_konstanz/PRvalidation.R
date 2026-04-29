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
n_examples <- 10

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
  sections <- data.frame(
    start_time = starts,
    end_time   = ends
  )

  # read this individuals predictions with confidence scores
  predictions <- read_delim(mixedsort(individual_files[[individual]]), 
                           col_names = FALSE, 
                           delim = "\t")
  
  # iterate over the call types and produce validation files
  for (call_type in call_labels) {
    cat(sprintf("Call type: %s\n", call_type))

    # create an empty dataframe with the same columns as the predictions

    # pick n examples for each call type with non-focal calls
    for (i in 1:n_examples/2) {
      # randomly choose a day from the range 0:days (defined above)
        # filter predictions for this day, by multiplying the day with 24 * 60 * 60
          # randomly choose a row from predictions where X3 contains the call_type string and the string "non"
          # example: call_type = "fed", matched row = "fed non"
            # check if X1 falls into any of the start and end periods in labeled_sections
              # if yes, choose another row
              # if not, store the row in the empty dataframe from above
              # also if not, add the X1 timestamp to the labeled_sections dataframe, so that no timestamps close to it are selected
                # start = X1 - buffer_min * 60
                # end = X1 + buffer_min * 60
    }

    # pick n examples for each call type with focal calls
    for (i in 1:n_examples/2) {
      # same as above, but choose a row from predictions where X3 contains the call_type string and the string "foc"
      # example: call_type = "fed", matched row = "fed foc"
    }
  }
}
