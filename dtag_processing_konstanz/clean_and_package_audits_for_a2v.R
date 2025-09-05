################################################################################

## This script parses and saves audits for fine tuning of animal2vec, using some 
## helper functions for converting audit times to wav file times. 


## Written by Eli Strauss March 2024

################################################################################

library(dplyr)
library(intervals)

label_files <- list.files('~/Documents/code/hyena_call_labels/finished_and_checked/', full.names = T)

## SOURCE SOME CRITICAL FUNCTIONS
code_dir <- '~/Documents/code/hyena_call_labels/dtag_processing_konstanz/'
source(paste0(code_dir, 'cue2wav.R'))
source(paste0(code_dir, 'cue2utc.R'))
source(paste0(code_dir, 'parse_audits.R'))
source(paste0(code_dir, 'export_labels_a2v.R'))


#THIS IS THE FOLDER WHERE THE CUE TABLES (wavcues) ARE. THESE ARE EXPORTED FROM /cc23
source(paste0(code_dir, 'export_cue_tables.R'))
cuetab_folder <- '/Volumes/cc23_eds/cc23_cue_tables_csv/' 
julian_labels <- parse_audits(label_files, cuetab_folder)
julian_labels$time <- julian_labels$wav_time


# TIDY UP TYPOS ETC
julian_labels$label <- gsub(x = julian_labels$label, pattern = '  ', replacement = ' ')
julian_labels$label <- gsub(x = julian_labels$label, pattern = ' \\+', replacement = '+')
julian_labels$label <- gsub(x = julian_labels$label, pattern = '\\+ ', replacement = '+')
julian_labels$label <- gsub(x = julian_labels$label, pattern = '\\[ ', replacement = '[')
julian_labels$label <- gsub(x = julian_labels$label, pattern = ' \\]', replacement = ']')



# FILTER TO ONLY CALL TYPES OF INTEREST
labels_to_use <- c('fed', 'sql', 'str', 'grn', 'whp', 'rum', 'gig', 'gwl', 'oth', 'unk', 
                   'skp', 'nnf', 'snr', 'soa', 'eoa')



# visually inspect labels without any of these in them. Resolve typos or confirm that the rest can be deleted
unique(julian_labels$label[!grepl(pattern = paste(labels_to_use, collapse = '|'), x = julian_labels$label)])

# remove labels that dont match - removes 177 labels
nrow(julian_labels) - nrow(julian_labels[grepl(pattern = paste(labels_to_use, collapse = '|'), x = julian_labels$label),])
julian_labels <- julian_labels[grepl(pattern = paste(labels_to_use, collapse = '|'), x = julian_labels$label),]

export_labels_a2v(julian_labels, destination = paste0(code_dir, '../hyena_a2v_labels/'))
