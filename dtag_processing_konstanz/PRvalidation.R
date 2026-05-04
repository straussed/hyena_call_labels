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

# ratio between focal and non-focal calls
focal_ratio <- 0.5

# adaptive bin breaks — coarse at low confidence scores where threshold selection 
# is unlikely, finer above 0.7 where it matters
conf_breaks <- c(0, 0.3, 0.5, 0.6, 0.7, 0.75, 0.80, 0.85, 0.90, 0.95, 1.0)

# minimum number of validated examples per confidence bin before a warning is 
# issued. Used to flag bins where precision estimates will be unreliable.
min_per_bin <- 20/(length(conf_breaks)-1)/2

################################################################################
# loading all files for every individual and creating directories

# output directory
out_dir <- glue("{base_folder}/PR_validation")
if (!file.exists(out_dir)){dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)}

dir.create(glue("{out_dir}/selected_predictions"), recursive = TRUE, showWarnings = FALSE)
dir.create(glue("{out_dir}/to_validate"), recursive = TRUE, showWarnings = FALSE)

# load all a2v label files, filter to only soa and eoa
all_labels <- read_csv(list.files(path = a2v_labels_folder, 
                                  pattern = "\\.csv$", 
                                  full.names = TRUE), show_col_types = FALSE)
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


################################################################################
# pass 1 — compute global bin totals across all individuals. bin_total in the 
# output must reflect the full dataset prediction population, not just the 
# individual being sampled, so that eval_weight correctly represents how many 
# real predictions each validated example stands for when computing weighted 
# precision across all individuals combined.

cat("Pass 1: computing global bin totals across all individuals...\n")

global_bin_totals <- data.frame()

for (individual in names(individual_files)) {
  file_list <- mixedsort(individual_files[[individual]])
  file_list <- file_list[file.size(file_list) > 0]

  preds_raw <- read_delim(file_list, col_names = FALSE, delim = "\t", show_col_types = FALSE)
  preds_raw <- rename(preds_raw, start = X1, duration = X2, label = X3, call_conf = X4, foc_conf = X5)

  # restrict to the first n days, consistent with the sampling pool
  preds_raw$day <- floor(preds_raw$start / 86400)
  preds_raw     <- preds_raw[preds_raw$day <= days - 1, ]

  # assign confidence bins using the same breaks as the sampling step
  # so that bin definitions are identical across both passes
  preds_raw <- preds_raw %>%
    mutate(conf_bin = cut(call_conf, breaks = conf_breaks, include.lowest = TRUE))

  # count predictions per label per bin for this individual and accumulate
  individual_counts <- preds_raw %>%
    group_by(label, conf_bin) %>%
    summarise(count = n(), .groups = "drop")

  global_bin_totals <- bind_rows(global_bin_totals, individual_counts)
}

# sum counts across individuals to get the global total per label per bin
global_bin_totals <- global_bin_totals %>%
  group_by(label, conf_bin) %>%
  summarise(global_bin_total = sum(count), .groups = "drop")

cat("Pass 1 complete.\n\n")

################################################################################
# helper function: sample one prediction from a given bin pool using the 
# two-pass proximity check.
# first attempts to find a candidate satisfying both the section
# exclusion and the 30-minute proximity check. If no such candidate is found
# after max_attempts tries, falls back to section exclusion only and warns.

sample_from_bin <- function(bin_pool, sections, picked_timestamps, buffer_min, max_attempts = 200) {

  # shuffle the pool so repeated calls do not always try the same candidates
  bin_pool <- bin_pool[sample(nrow(bin_pool)), ]

  found <- NULL
  used_fallback <- FALSE

  # pass 1: try to satisfy both section exclusion and proximity check
  for (i in seq_len(nrow(bin_pool))) {
    candidate      <- bin_pool[i, ]
    candidate_time <- candidate$start

    in_section <- any(candidate_time >= sections$start_time &
                      candidate_time <= sections$end_time)
    if (in_section) next

    is_too_close <- FALSE
    if (length(picked_timestamps) > 0) {
      is_too_close <- any(abs(candidate_time - picked_timestamps) < buffer_min * 60)
    }
    if (is_too_close) next

    found <- candidate
    break
  }

  # pass 2: fallback — relax proximity check, keep section exclusion as hard constraint
  if (is.null(found)) {
    used_fallback <- TRUE
    for (i in seq_len(nrow(bin_pool))) {
      candidate      <- bin_pool[i, ]
      candidate_time <- candidate$start

      in_section <- any(candidate_time >= sections$start_time &
                        candidate_time <= sections$end_time)
      if (in_section) next

      found <- candidate
      break
    }
  }

  list(row = found, used_fallback = used_fallback)
}


################################################################################
# pass 2: sampling loop, create validation files for each individual
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
  file_list <- mixedsort(individual_files[[individual]])
  file_list <- file_list[file.size(file_list) > 0]
  predictions <- read_delim(file_list, col_names = FALSE, delim = "\t", show_col_types = FALSE)
  predictions <- rename(predictions, start = X1, duration = X2, label = X3, call_conf = X4, foc_conf = X5)
  
  # restrict predictions to the first n days
  predictions$day <- floor(predictions$start / 86400)
  predictions     <- predictions[predictions$day <= days - 1, ]

  # assign confidence bins using the same breaks as pass 1
  predictions <- predictions %>%
    mutate(conf_bin = cut(call_conf, breaks = conf_breaks, include.lowest = TRUE))

  # create an empty dataframe to store the results
  valid_preds <- predictions[0, ]
  
  # iterate over the call types and produce validation files
  for (call_type in call_labels) {
    cat(sprintf("Processing call type: %s\n", call_type))

    # picked_timestamps is shared across focal and non-focal for this call type
    # so that focal and non-focal picks do not cluster around the same times
    picked_timestamps <- numeric(0)

    nf_target <- as.integer(round(n_examples * (1 - focal_ratio)))
    foc_target <- as.integer(round(n_examples * focal_ratio))

    # NON-FOCAL CALLS
    # bin-first sampling — divide the target count equally across
    # available bins and sample directly from each bin across the full pool,
    # rather than using the day-queue mechanism which failed for sparse bins
    pool_non <- predictions %>%
      filter(grepl(call_type, label), grepl("non", label))

    available_bins_non <- unique(pool_non$conf_bin)

    if (length(available_bins_non) == 0) {
      cat(sprintf("    Warning: No non-focal data found for %s. Skipping.\n", call_type))
    } else {
      # divide target count as evenly as possible across available bins
      n_bins_non      <- length(available_bins_non)
      base_per_bin    <- nf_target %/% n_bins_non
      remainder       <- nf_target %% n_bins_non
      # distribute the remainder across the first bins
      targets_per_bin <- rep(base_per_bin, n_bins_non) + c(rep(1, remainder), rep(0, n_bins_non - remainder))
      names(targets_per_bin) <- as.character(available_bins_non)

      newly_added_non <- predictions[0, ]
      success_count   <- 0

      for (bin in available_bins_non) {
        bin_target <- targets_per_bin[as.character(bin)]
        bin_pool   <- pool_non[pool_non$conf_bin == bin, ]
        bin_count  <- 0

        for (k in seq_len(bin_target)) {
          result <- sample_from_bin(bin_pool, sections, picked_timestamps, buffer_min)

          if (!is.null(result$row)) {
            if (result$used_fallback) {
              cat(sprintf("    Warning: proximity fallback triggered for non-focal %s bin %s.\n",
                          call_type, bin))
            }
            valid_preds       <- rbind(valid_preds, result$row)
            newly_added_non   <- rbind(newly_added_non, result$row)
            picked_timestamps <- c(picked_timestamps, result$row$start)
            success_count     <- success_count + 1
            bin_count         <- bin_count + 1

            # remove the picked row from the bin pool so it cannot be picked again
            bin_pool <- bin_pool[bin_pool$start != result$row$start, ]
          } else {
            cat(sprintf("    Warning: No valid non-focal candidate found for %s bin %s.\n",
                        call_type, bin))
          }
        }
      }

      if (success_count < nf_target) {
        cat(sprintf("    Warning: Only found %d/%d non-focal calls for %s.\n",
                    success_count, nf_target, call_type))
      }

      # per-bin insufficiency reporting
      if (nrow(newly_added_non) > 0) {
        bin_summary_non <- newly_added_non %>%
          count(conf_bin, name = "n_sampled") %>%
          right_join(
            pool_non %>% distinct(conf_bin) %>% mutate(conf_bin = as.factor(conf_bin)),
            by = "conf_bin"
          ) %>%
          mutate(n_sampled = replace_na(n_sampled, 0))

        under_non <- bin_summary_non %>% filter(n_sampled < min_per_bin)
        if (nrow(under_non) > 0) {
          cat(sprintf("    Warning: Under-sampled non-focal confidence bins for %s:\n", call_type))
          for (i in seq_len(nrow(under_non))) {
            cat(sprintf("      bin %s: %d sampled\n",
                        under_non$conf_bin[i],
                        under_non$n_sampled[i]))
          }
        }
      }
    }

    # FOCAL CALLS
    pool_foc <- predictions %>%
      filter(grepl(call_type, label), grepl("foc", label), !grepl("non", label))

    available_bins_foc <- unique(pool_foc$conf_bin)

    if (length(available_bins_foc) == 0) {
      cat(sprintf("    Warning: No focal data found for %s. Skipping.\n", call_type))
    } else {

      n_bins_foc      <- length(available_bins_foc)
      base_per_bin    <- foc_target %/% n_bins_foc
      remainder       <- foc_target %% n_bins_foc
      targets_per_bin <- rep(base_per_bin, n_bins_foc) + c(rep(1, remainder), rep(0, n_bins_foc - remainder))
      names(targets_per_bin) <- as.character(available_bins_foc)

      newly_added_foc <- predictions[0, ]
      success_count   <- 0

      for (bin in available_bins_foc) {
        bin_target <- targets_per_bin[as.character(bin)]
        bin_pool   <- pool_foc[pool_foc$conf_bin == bin, ]
        bin_count  <- 0

        for (k in seq_len(bin_target)) {
          result <- sample_from_bin(bin_pool, sections, picked_timestamps, buffer_min)

          if (!is.null(result$row)) {
            if (result$used_fallback) {
              cat(sprintf("    Warning: proximity fallback triggered for focal %s bin %s.\n",
                          call_type, bin))
            }
            valid_preds       <- rbind(valid_preds, result$row)
            newly_added_foc   <- rbind(newly_added_foc, result$row)
            picked_timestamps <- c(picked_timestamps, result$row$start)
            success_count     <- success_count + 1
            bin_count         <- bin_count + 1

            bin_pool <- bin_pool[bin_pool$start != result$row$start, ]
          } else {
            cat(sprintf("    Warning: No valid focal candidate found for %s bin %s.\n",
                        call_type, bin))
          }
        }
      }

      if (success_count < foc_target) {
        cat(sprintf("    Warning: Only found %d/%d focal calls for %s.\n",
                    success_count, foc_target, call_type))
      }

      # per-bin insufficiency reporting
      if (nrow(newly_added_foc) > 0) {
        bin_summary_foc <- newly_added_foc %>%
          count(conf_bin, name = "n_sampled") %>%
          right_join(
            pool_foc %>% distinct(conf_bin) %>% mutate(conf_bin = as.factor(conf_bin)),
            by = "conf_bin"
          ) %>%
          mutate(n_sampled = replace_na(n_sampled, 0))

        under_foc <- bin_summary_foc %>% filter(n_sampled < min_per_bin)
        if (nrow(under_foc) > 0) {
          cat(sprintf("    Warning: Under-sampled focal confidence bins for %s:\n", call_type))
          for (i in seq_len(nrow(under_foc))) {
            cat(sprintf("      bin %s: %d sampled\n",
                        under_foc$conf_bin[i],
                        under_foc$n_sampled[i]))
          }
        }
      }
    }
  }

  if (nrow(valid_preds) > 0) {
    # order predictions by start time
    valid_preds <- valid_preds[order(valid_preds$start),]

    # join against global_bin_totals to get the correct bin_total
    # that reflects the full dataset across all individuals, then recompute
    # eval_weight. This ensures that when computing weighted precision across
    # all individuals combined, each validated example is weighted by its
    # true share of the global prediction population for that label and bin
    valid_preds <- valid_preds %>%
      left_join(global_bin_totals, by = c("label", "conf_bin")) %>%
      group_by(label, conf_bin) %>%
      mutate(
        n_sampled   = n(),
        eval_weight = global_bin_total / n_sampled
      ) %>%
      ungroup()

    write_delim(valid_preds, glue("{base_folder}/PR_validation/selected_predictions/cc23_{individual}_predictions_conf.txt"), delim = "\t")

    # remove columns for cue validation file
    cues_to_validate <- valid_preds %>% select(start, duration, label)

    # save validaton file
    write_delim(cues_to_validate, glue("{base_folder}/PR_validation/to_validate/cc23_{individual}_validation_cues.txt"), delim = "\t", col_names = FALSE)
  }
}
