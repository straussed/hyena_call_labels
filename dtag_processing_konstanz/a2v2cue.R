################################################################################

## This script converts the prediction files form animal2vec back to cue files 
## that can be read into the matlab labeling and validation tool. The prediction
## files are for each wav file and give the times within the wav file itself.
## This script converts the times to be relative to the start of the collar and
## creates one long file including all predictions for each hyena.
## If specified, the script can also produce files that contain the call and 
## focal confidence scores for each prediction in order to calculate PR curves.
## The predictions can either be filtered the original way (all predictions are
## kept and labeled focal or non-focal) or the same way as in a2v2utctable.R
## (only focal predictions are kept, using the thresholds from the threshold
## selection pipeline, from the groans repository 
# https://github.com/mariusfaiss/groans), see the focal_only option below.


## Written by Marius Faiß January 2025

################################################################################

library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(gtools)
library(purrr)


# FILTERING PARAMETERS
# keep only focal predictions, filtered with the per call type class and focal
# thresholds from call_thresholds_{fbeta}.csv, the same way a2v2utctable.R does:
# all "nf" predictions (focal threshold below 0.1, set to "nf" by the inference 
# script) are removed, then each call type is cut on its own focal threshold 
# and on its own class threshold. Everything that survives is focal, so the 
# cues carry the bare call name without a "foc"/"non" label. Unknown
# predictions are removed as well, and squitters are removed by their focal
# threshold of 1.01, since they can only ever be non-focal (infant call)
# set to FALSE for the original filtering: all predictions are kept, the
# hard-coded class_thresholds below are applied and one global focal threshold
# labels each prediction "foc" or "non"
focal_only <- TRUE

# which F-Beta to use, only used when focal_only is TRUE
# "F1" for equal weighting of precision & recall (F1)
# "F15" for lean towards recall (F1.5)
# "F2" for heavier lean towards recall (F2)
# "tiered" for recall lean depending on AUC score (>=0.9 -> F1.5; <0.9 -> F2)
fbeta <- "tiered"

# to filter for only groan or only feeding predictions etc.
# enter "feeding" or "groan", or "all" to not filter the predictions
filter_calls <- "all"

# remove "unknown" predictions, only used when focal_only is FALSE
# they are always removed when focal_only is TRUE, they have no focal score
remove_unk <- TRUE

# filter out low confidence score predictions, set to 0 to get all predictions
# set to NULL to use different thresholds per call type
# has to be NULL when focal_only is TRUE, the thresholds then come from the file
min_confidence <- NULL

if (focal_only & !is.null(min_confidence)) {
  stop("focal_only = TRUE requires min_confidence = NULL, the per call type ",
       "class and focal thresholds are read from the threshold file")
}

# specify minimum confidence score per call type, only used when focal_only is
# FALSE, the "focal" entry is the single global cut for the "foc"/"non" label
if (!focal_only & is.null(min_confidence)) {
  class_thresholds <- c(
    "groan" = 0.397,
    "feeding" = 0.244,
    "whoop" = 0.434,
    "squitter" = 0.594,
    "giggle" = 0.511,
    "alarm_rumble" = 0.487,
    "squeal" = 0.524,
    "growl" = 0.369,
    "snore" = 0.568,
    "oth" = 0.300,
    "focal" = 0.366
  )
}

# OUTPUT FILES
# create validation ground truth files with confidence scores
validation_gt <- FALSE

# create cue files for viewing in MATLAB
cuefiles <- TRUE

if (cuefiles) {
  # add confidence score to label for viewing in MATLAB
  add_conf <- FALSE

  # add focal confidence score to label for viewing in MATLAB
  add_focalconf <- FALSE
} else {
  add_conf <- FALSE
  add_focalconf <- FALSE
}

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
  "nf" = "non",
  "snore" = "snr"
)

################################################################################

# the location of the cue tables containing the relative time of each wav file 
# to the collar start time
cue_tables <- "/Users/mfaiss/Documents/Hyenaproject/Hyena_data/cc23_cue_tables_csv"

# location of the animal2vec predictions
a2v_files <- "/Users/mfaiss/Documents/Hyenaproject/Hyena_data/a2v_predictions/2026_01_01_large_model_final_predictions/csv"

# output directory where the folder with converted cue files will be
out_dir <- "/Users/mfaiss/Documents/Hyenaproject/hyena_call_labels/a2v_validation/converted_files/2026_01_01_large_model_final_predictions"

# minimum confidence score per call type, only used when focal_only is TRUE
# edit this file to change a threshold, the class_note and focal_note columns
# document how each value was selected
thresholds_file <- sprintf(paste0("/Users/mfaiss/Documents/Hyenaproject/groans",
                                  "/audio/a2v_threshold_selection/data",
                                  "/call_thresholds_%s.csv"), fbeta)

if (focal_only) {
  # the file has a "#" commented header block explaining each threshold, skip it
  thresholds <- read_csv(thresholds_file, show_col_types = FALSE, comment = "#",
                         col_types = cols(call_type = col_character(),
                                          class_threshold = col_double(),
                                          focal_threshold = col_double(),
                                          .default = col_character()))

  # named vectors, indexed by call type in wav_preds()
  # an empty cell (NA) means no filtering for that call type
  class_thresholds <- setNames(thresholds$class_threshold, thresholds$call_type)
  focal_thresholds <- setNames(thresholds$focal_threshold, thresholds$call_type)
}

################################################################################

# process predictions for one wav file
wav_preds <- function(file_name, cue_table){
  cat(file_name, "\n")
  
  # load and format prediction file
  wav_df <- read_delim(sprintf("%s/%s", a2v_files, file_name), 
                       delim = "\t", col_types = "cccc", 
                       col_select=c(Start, Duration, Name, Description))
  
  # remove non-focal and unknown predictions, neither carries a focal
  # confidence score, so neither can be verified as focal
  if (focal_only) {
    wav_df <- wav_df[!grepl("nf", wav_df$Name),]
    wav_df <- wav_df[!grepl("unk", wav_df$Name),]
  }
  
  # split the "Description" column into two separate values
  wav_df <- wav_df %>%
    mutate(
      # split the string at spaces
      split_desc = str_split(Description, " ")
    ) %>%
    mutate(
      # take the first element for the "Description" column
      Description = map_chr(split_desc, ~ .x[1]),
      # if there is a second element, assign it to a new column "Description2"
      Description2 = map_chr(split_desc, ~ ifelse(length(.x) > 1, .x[2], NA))
    ) %>%
    mutate(
      # remove the "foc" string from Description2
      Description2 = as.character(gsub("foc", "", Description2))) %>%
    # drop the intermediate list column
    select(-split_desc)
  
  # apply confidence threshold(s) to remove predictions
  if (!is.null(min_confidence)) {
    wav_df <- wav_df %>% filter(Description >= min_confidence)
  } else if (focal_only) {
    # remove predictions below the focal threshold of their call type
    wav_df <- wav_df %>%
      mutate(Description2 = as.numeric(Description2)) %>%
      filter(
        is.na(focal_thresholds[Name]) |
        Description2 >= focal_thresholds[Name]
      ) %>%
      mutate(Description2 = as.character(Description2))

    # remove call predictions with low confidence score
    wav_df <- wav_df %>%
      mutate(Description = as.numeric(Description)) %>%
      filter(
        is.na(class_thresholds[Name]) |
        Description >= class_thresholds[Name]
      ) %>%
      mutate(Description = as.character(Description))
  } else {
    # remove "nf" string from names, remove NA focal confidence scores
    wav_df <- wav_df %>%
      mutate(Description2 = as.numeric(Description2)) %>%
      mutate(Name = gsub(" .*", "", Name),
      Description2 = replace_na(Description2, 0.000))

    # remove call predictions with low confidence score
    wav_df <- wav_df %>%
      mutate(Description = as.numeric(Description)) %>%
      filter(
        is.na(class_thresholds[Name]) |
        Description >= class_thresholds[Name]
      ) %>%
      mutate(Description = as.character(Description))
    
    # assign correct focal label
    wav_df <- wav_df %>%
      mutate(
        Name = ifelse(
          Description2 >= class_thresholds["focal"],
          paste0(Name, " foc"),
          paste0(Name, " non")
        )
      )
  }
  
  # remove "unknown" predictions
  if (remove_unk) {
    wav_df <- wav_df[!grepl("unk", wav_df$Name),]
  }

  # filter specific prediction type
  if (filter_calls != "all") {
    wav_df <- wav_df %>% filter(str_detect(Name, filter_calls))
  }
  
  # convert the call names to cue labels
  wav_df <- wav_df %>%
    mutate(Name = str_replace_all(Name, replacement_map))
  
  # add focal label if no focal threshold is applied
  if (!is.null(min_confidence)) {
  wav_df <- wav_df %>%
    mutate(Name = if_else(
      !str_detect(Name, "non"), paste0(Name, " foc"), Name
    ))
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

  # format confidence scores
  if (add_conf | validation_gt | add_focalconf) {
    # call confidence scores
    wav_df$Description <- as.character(wav_df$Description)
    wav_df$Description[is.na(wav_df$Description)] <- ""
    # focal confidence scores
    wav_df$Description2 <- as.character(wav_df$Description2)
    wav_df$Description2[is.na(wav_df$Description2)] <- ""
  }

  # create validation files with confidence scores
  if (validation_gt) {
    valid_df <- wav_df
    valid_df <- rename(valid_df, call_conf = Description)
    valid_df <- rename(valid_df, focal_conf = Description2)
  } else {
    valid_df <- wav_df[0,]
    valid_df <- rename(valid_df, call_conf = Description)
    valid_df <- rename(valid_df, focal_conf = Description2)
  }

  # add confidence scores to the labels for viewing in MATLAB
  if (add_conf) {
    wav_df <- wav_df %>% mutate(Name = paste(Name, Description)) %>%
      select(-Description)
  } else {
    wav_df$Description <- NULL
  }

  if (add_focalconf) {
    wav_df <- wav_df %>% mutate(Name = paste(Name, Description2)) %>%
      select(-Description2)
  } else {
    wav_df$Description2 <- NULL
  }
  
  return(list(wav_df, valid_df))
}

################################################################################

# tag that records the filtering settings in the output folder and file names,
# so that runs with different settings do not overwrite each other
if (!is.null(min_confidence)) {
  run_tag <- ifelse(min_confidence != 0, gsub("\\.", "_", min_confidence), "")
} else if (focal_only) {
  # focal predictions only, filtered with the thresholds of this F-Beta
  run_tag <- sprintf("%s_focal", fbeta)
} else {
  run_tag <- "PRthres"
}

# appended to the folder and file names, empty when min_confidence is 0
tag <- ifelse(nzchar(run_tag), sprintf("_%s", run_tag), "")

# create output folder
cue_out_folder <- sprintf("%s/%s_predictions%s/cues", out_dir, filter_calls, tag)
conf_out_folder <- sprintf("%s/%s_predictions%s/confs", out_dir, filter_calls, tag)


if (!file.exists(cue_out_folder) & cuefiles){
  dir.create(cue_out_folder, recursive = TRUE, showWarnings = FALSE)
}
if (!file.exists(conf_out_folder) & validation_gt){
  dir.create(conf_out_folder, recursive = TRUE, showWarnings = FALSE)
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
                                individual), col_names = FALSE, 
                                show_col_types = FALSE)
  
  # iterate through this individual's files, sorted by number
  list_of_files <- mixedsort(individual_files[[individual]])
  
  ##############################################################################
  # filtering for one type of prediction, only results in one large file
  if (filter_calls != "all") {
    
    cue_out_name <- sprintf("%s/cc23_%s_%s_predictions_cues%s.txt", 
                        cue_out_folder, individual, filter_calls, tag)
    conf_out_name <- sprintf("%s/cc23_%s_%s_predictions_conf%s.txt", 
                        conf_out_folder, individual, filter_calls, tag)
    
    if (!file.exists(cue_out_name) | !file.exists(conf_out_name)) {
      # empty list to put all wav file predictions into
      df_list <- list()
      valid_list <- list()
      
      for (filename in list_of_files) {
        # use function to convert file
        dataframes <- wav_preds(filename, cue_table)
        wav_df <- dataframes[[1]]
        valid_file <- dataframes[[2]]

        # add converted prediction file to the list
        df_list <- append(df_list, list(wav_df))
        valid_list <- append(valid_list, list(valid_file))
      }
      
      # combine all wav predictions into one dataframe and format numbers
      individual_df <- bind_rows(df_list) %>%
        mutate(Start = format(Start, nsmall = 6, scientific=FALSE, trim=TRUE),
               Duration = format(Duration, nsmall = 6, scientific=FALSE, 
                                 trim=TRUE))
      valid_df <- bind_rows(valid_list) %>%
        mutate(Start = format(Start, nsmall = 6, scientific=FALSE, trim=TRUE),
               Duration = format(Duration, nsmall = 6, scientific=FALSE, 
                                 trim=TRUE))
      
      # write a text file containing all converted predictions for this hyena
      if (cuefiles){write_delim(individual_df, cue_out_name, delim = "\t", col_names = FALSE)}
      if (validation_gt){write_delim(valid_df, conf_out_name, delim = "\t", col_names = FALSE)}
    }
  }
  
  ##############################################################################
  # export files individually if all predictions are included
  if (filter_calls == "all"){
    for (filename in list_of_files){
      
      # extract the wav file number from the file name
      number <- str_pad(parse_number(str_split(filename, "_")[[1]][2]), 3, pad="0")
      
      cue_out_name <- sprintf("%s/cc23_%s%s_%s_predictions_cues%s.txt", 
                            cue_out_folder, individual, number, filter_calls, tag)
      conf_out_name <- sprintf("%s/cc23_%s%s_%s_predictions_conf%s.txt", 
                            conf_out_folder, individual, number, filter_calls, tag)

      if (!file.exists(cue_out_name) | !file.exists(conf_out_name)) {

        # use function to convert file
        dataframes <- wav_preds(filename, cue_table)
        wav_df <- dataframes[[1]]
        valid_file <- dataframes[[2]]
      
        wav_df <- wav_df %>%
          mutate(Start = format(Start, nsmall = 6, scientific=FALSE, trim=TRUE),
                 Duration = format(Duration, nsmall = 6, scientific=FALSE, 
                                   trim=TRUE))
        
        valid_file <- valid_file %>%
          mutate(Start = format(Start, nsmall = 6, scientific=FALSE, trim=TRUE),
                 Duration = format(Duration, nsmall = 6, scientific=FALSE, 
                                   trim=TRUE))
        
        # write a text file containing all converted predictions for this hyena
        if (cuefiles){write_delim(wav_df, cue_out_name, delim = "\t", col_names = FALSE)}
        if (validation_gt){write_delim(valid_file, conf_out_name, delim = "\t", col_names = FALSE)}
      }
    }
  }
}
