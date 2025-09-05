

################################################################################

## This function parses audits, assembling all label data into a single
## dataframe, converting times to utc and wav file times, and 
## incorporating all metadata. 

## The purpose of the function is to convert the output from labeling in 
## dtagtools to be used in downstream analyses, for instance animal2vec 


## Written by Eli Strauss March 2024

################################################################################

### Arguments
# audit_files: path to audit files
# cuetab_folder: folder containing cue tables and the csv of reference times


parse_audits <- function(audit_files, cuetab_folder){
  
  label_info_list <- list()
  for(af in audit_files){
    ## Parse file name for metadata
    file_name <- basename(af)
    file_name_split <- strsplit(file_name, '_')[[1]]
    tag <- paste(file_name_split[1:2], collapse = '_')
    labeler <- file_name_split[4]
    audit_number <- gsub(file_name_split[5], pattern = '.txt', replacement = '')
    
    ## read in file
    labels <- read.delim(af, header = F)
    names(labels) <- c('tag_time', 'duration', 'label')
    
    ##lowercase labels
    labels$label <- tolower(labels$label)
    
    ##trim whitespace
    labels$label <- trimws(labels$label)
    
    ## skip file and complain if soa and eoa are missing or there are too many
    if(sum(labels$label == 'soa') != 1 | sum(labels$label == 'eoa') != 1){
      warning(paste0('Audit ', paste(labeler, audit_number, sep = '_'), ' has soa/eoa issue. Skipping!'))
      next
    }
    
    # ## check for duplicate region numbers, skip file and complain if audit number is already in the table
    # if(audit_number %in% unlist(label_info_list$audit_numbers)){
    #   matching_index <- which(label_info_list$audit_numbers == audit_number)
    #   new_region_duration <- max(labels$tag_time) - min(labels$tag_time)
    #   old_region_duration <- max(label_info_list$labels[[matching_index]]$tag_time) - min(label_info_list$labels[[matching_index]]$tag_time)
    #   warning('Duplicate detected: ', file_name, ' - ', label_info_list$labels[[matching_index]]$audit_file[1], '\n',
    #           '    Duration difference (secs) = ', new_region_duration - old_region_duration, '\nSKIPPING')
    #   next
    # }
    # 
    ## Check for overlap between a new audited region and what exists in the table
  
    audit_interval <- intervals::Intervals(cbind(min(labels$tag_time), max(labels$tag_time)), closed = F)
    same_tag_audits <- which(label_info_list$tag == tag)
    if(length(same_tag_audits)){
      existing_audits <- intervals::Intervals(cbind(
        unlist(label_info_list$aud_start[same_tag_audits]),
        unlist(label_info_list$aud_end[same_tag_audits])
      ), closed = T)
      overlaps <- intervals::interval_overlap(audit_interval, existing_audits)[[1]]
      if(length(overlaps)){
        percent_overlap <- 100 * suppressWarnings(intervals::size(intervals::interval_intersection(existing_audits[overlaps], audit_interval))/intervals::size(intervals::interval_union(existing_audits[overlaps], audit_interval)))
        warning('Overlapping audits: ', label_info_list$file_name[same_tag_audits][[overlaps]], ' - ', file_name, '\n   %overlap = ', percent_overlap)
        #next
      }
    }
    
    ## read in cuetable
    cuetab_name <- paste0('_', tag, 'wavcues.csv')
    cuetab <- read.csv(file = paste0(cuetab_folder, cuetab_name), header = FALSE)
    
    ## read in reference times for start of tag in UTC
    ref_times <- read.csv(file = paste0(cuetab_folder, 'ref_times.csv'))
    
    ## convert times to number of seconds into wav file
    converted_times <- cue2wav(labels$tag_time, cuetab)
    
    ## convert times to utc time
    utc_time <- cue2utc(tag, labels$tag_time, ref_times = ref_times)
    
    ## make wav_num a character with three digits and leading 0s if necessary
    converted_times$wav_file <- paste0(tag, gsub(pattern = '0\\.', replacement = '',
                                                 sprintf(fmt = "%.3f", converted_times$wav_num/1000)), '.wav')
    
    ## create df with all label information
    label_info <- data.frame(
      tag,
      tag_time = labels$tag_time,
      wav_time = converted_times$wav_time,
      utc_time,
      duration = labels$duration,
      label = labels$label,
      audit_number,
      labeler,
      wav_file = converted_times$wav_file,
      audit_file = file_name
    )
    
    ## If label file spans multiple wav files, add in necessary soa and eoa
    ### This assumes all label files have SOA and EOA!
    if(length(unique(label_info$wav_file)) > 1){
      second_half_duration <- NA
      for(wf in unique(label_info$wav_file)){
        wav_num <- as.numeric(substr(wf, start = nchar(wf) - 6, stop = nchar(wf) - 4))
        wav_labs <- label_info[label_info$wav_file == wf,]
        
        ## If file doesnt have start, add it at the beginning of the file
        if(!'soa' %in% wav_labs$label){
          file_start <- cuetab[cuetab$V1 == wav_num,'V2'][1]
          
          ## Copy first row and change the relevant columns
          label_info <- rbind(wav_labs[1,], label_info)
          label_info[1,]$wav_time <- 0
          label_info[1,]$label <- 'soa'
          label_info[1,]$duration <- 0
          label_info[1,]$tag_time <- file_start
          label_info[1,]$utc_time <- cue2utc(label_info$tag[1], cue = file_start, ref_times)
          
          ## If there was a label that spanned across the file break, add in second half of label
          if(!is.na(second_half_duration)){
            ## Copy first row and change the relevant columns
            label_info <- rbind(wav_labs[1,], label_info)
            label_info[1,]$wav_time <- 0.01
            label_info[1,]$label <- second_half_label
            label_info[1,]$duration <- second_half_duration - label_info[1,]$wav_time
            label_info[1,]$tag_time <- file_start + label_info[1,]$wav_time
            label_info[1,]$utc_time <- cue2utc(label_info$tag[1], cue = label_info[1,]$tag_time, ref_times)
            
            
            second_half_duration <- NA
            second_half_label <- NA
          }
        }
        
        if(!'eoa' %in% wav_labs$label){
          ## Need to find the end of the wav file. In cases where there is a gap between files, 
          ## this is listed in cuetab. When file transitions are seamless,
          ## file end = start of next file
          
          if(length(cuetab[cuetab$V1 == wav_num,'V2'])==2){
            file_end <- cuetab[cuetab$V1 == wav_num,'V2'][2]
          }else{
            ## If end of file not listed, set end of file to start of next file - 0.01
            last_lab <- max(wav_labs$tag_time) + wav_labs$duration[length(wav_labs$duration)]
            next_file_start <- cuetab[cuetab$V1 == wav_num+1,'V2'][1]
            file_end <- next_file_start - 0.01
            
            ## Is there a label that crosses the file boundary? If yes, split it into two
            if(last_lab > (next_file_start)){
              warning(paste0('Label crosses file boundary - breaking label into two.\n', wav_labs$audit_file[1], '\n', wav_labs$wav_file[1]))
              first_half_duration <- file_end - wav_labs$wav_time[length(wav_labs$duration)]
              second_half_duration <- wav_labs$duration[length(wav_labs$duration)] - first_half_duration
              second_half_label <- wav_labs$label[length(wav_labs$duration)]
              label_info$duration[max(which(label_info$wav_file == wf))]<- first_half_duration
            }
          }
          
          ## Copy first row and change the relevant columns
          label_info <- rbind(wav_labs[1,], label_info)
          label_info[1,]$tag_time <- file_end
          label_info[1,]$wav_time <- cue2wav(file_end, cuetab)$wav_time
          label_info[1,]$label <- 'eoa'
          label_info[1,]$duration <- 0
          label_info[1,]$utc_time <- cue2utc(label_info$tag[1], cue = file_end, ref_times)
        }
      }
    }
    
    label_info <- label_info[order(label_info$tag, label_info$utc_time),]
    label_info_list$labels[[file_name]] <- label_info
    label_info_list$audit_numbers[[length(label_info_list$audit_numbers)+ 1]] <- audit_number
    label_info_list$tag[[length(label_info_list$tag)+ 1]] <- tag
    label_info_list$aud_start[[length(label_info_list$aud_start)+ 1]] <- min(label_info$tag_time)
    label_info_list$aud_end[[length(label_info_list$aud_end)+ 1]] <- max(label_info$tag_time)
    label_info_list$file_name[[length(label_info_list$file_name) + 1]] <- file_name
  }
  label_info <- do.call(rbind, label_info_list$labels)
  row.names(label_info) <- NULL
  return(label_info)
}
