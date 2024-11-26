################################################################################

## This script parses and saves one audit done by Jana to be sent to Julian
## to help set up his analysis pipeline with the hyena data. It's pretty short. 


## Written by Eli Strauss March 2024

################################################################################

library(dplyr)
library(intervals)

label_files <- list.files('~/Dropbox/Documents/Research/Partial_projects/CCAS/Labeling/Label_validaiton/finished_and_checked/', full.names = T)

## SOURCE SOME CRITICAL FUNCTIONS
source('/Volumes/cc23_eds/dtag_processing_konstanz/cue2wav.R')
source('/Volumes/cc23_eds/dtag_processing_konstanz/cue2utc.R')
source('/Volumes/cc23_eds/dtag_processing_konstanz/parse_audits.R')
source('/Volumes/cc23_eds/dtag_processing_konstanz/export_labels_a2v.R')


#THIS IS THE FOLDER WHERE THE CUE TABLES (wavcues) ARE
cuetab_folder <- '/Volumes/cc23_eds/cc23_cue_tables_csv/' 
julian_labels <- parse_audits(label_files, cuetab_folder)
julian_labels$time = julian_labels$wav_time


# TIDY UP TYPOS ETC
julian_labels$label <- gsub(x = julian_labels$label, pattern = '  ', replacement = ' ')
julian_labels$label <- gsub(x = julian_labels$label, pattern = ' \\+', replacement = '+')
julian_labels$label <- gsub(x = julian_labels$label, pattern = '\\+ ', replacement = '+')
julian_labels$label <- gsub(x = julian_labels$label, pattern = '\\[ ', replacement = '[')
julian_labels$label <- gsub(x = julian_labels$label, pattern = ' \\]', replacement = ']')



# FILTER TO ONLY CALL TYPES OF INTEREST
labels_to_use <- c('fed', 'sql', 'str', 'grn', 'whp', 'rum', 'gig', 'gwl', 'oth', 'unk', 
                   'skp', 'nnf', 'soa', 'eoa')



# visually inspect labels without any of these in them. Resolve typos or confirm that the rest can be deleted
unique(julian_labels$label[!grepl(pattern = paste(labels_to_use, collapse = '|'), x = julian_labels$label)])

# remove labels that dont match - removes 147 labels
nrow(julian_labels) - nrow(julian_labels[grepl(pattern = paste(labels_to_use, collapse = '|'), x = julian_labels$label),])
julian_labels <- julian_labels[grepl(pattern = paste(labels_to_use, collapse = '|'), x = julian_labels$label),]


export_labels_a2v(julian_labels, '/Users/straussed/Dropbox/Documents/Research/Partial_projects/CCAS/Labeling/audio_2_vec_labels/fine_tune_labels_Sep_28/')
