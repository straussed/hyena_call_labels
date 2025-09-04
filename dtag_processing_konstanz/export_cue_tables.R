# Code to export cue tables and reference times into csv format
# Load required libraries
library(R.matlab)

# Parameters
input_dir <- '/Volumes/cc23_eds/cc23'
output_dir <- '/Volumes/cc23_eds/cc23_cue_tables_csv'

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("Created output directory:", output_dir, "\n")
}

# Set working directory to input directory
setwd(input_dir)

# List files matching the pattern (only one level deep in subdirectories)
files <- list.files(path = input_dir, pattern = "_cc23_.*wavcues\\.mat$", 
                    recursive = FALSE, full.names = TRUE)

# Also check one level deep in subdirectories
subdirs <- list.dirs(path = input_dir, recursive = FALSE, full.names = TRUE)
for (subdir in subdirs) {
  subdir_files <- list.files(path = subdir, pattern = "_cc23_.*wavcues\\.mat$", 
                             recursive = FALSE, full.names = TRUE)
  files <- c(files, subdir_files)
}

# Set up reference times dataframe
ref_times_all <- data.frame(tag = rep(NA, length(files)), ref_time = rep(NA, length(file)))

# Process each file
for (i in 1:length(files)) {
  # Get the directory and filename
  file_dir <- dirname(files[i])
  matname <- basename(files[i])
  
  # Set working directory to the file's directory
  setwd(file_dir)
  
  # Load the MATLAB file
  mat_data <- readMat(files[i])
  
  # Extract cuetab and ref_time from the loaded data
  cuetab <- mat_data$cuetab
  ref_time <- mat_data$ref.time
  
  # Create base name (remove file extension)
  basename <- tools::file_path_sans_ext(matname)
  
  # Create CSV filenames
  csvname <- paste0(basename, '.csv')
  ref_time_name <- paste0(basename, '_ref_time.csv')
  
  # Save reference times in dataframe
  ref_times_all[i,] <- cbind(basename(file_dir), ref_time)
  
  # Change to output directory
  setwd(output_dir)
  
  # Write matrices to CSV files (without row names and column headers to match MATLAB writematrix)
  write.table(cuetab, csvname, row.names = FALSE, col.names = FALSE, sep = ",")
  write.table(ref_time, ref_time_name, row.names = FALSE, col.names = FALSE, sep = ",")
}

# Save reference times
setwd(output_dir)
write.csv(ref_times_all, row.names = F, file = 'ref_times.csv')
save(ref_times_all, file = 'ref_times.RData')

# Create README file with processing information
setwd(output_dir)
readme_content <- paste0(
  "Cue Tables Export Processing Log\n",
  "================================\n\n",
  "Processing Date: ", Sys.Date(), "\n",
  "Processing Time: ", format(Sys.time(), "%H:%M:%S"), "\n\n",
  "Input Directory: ", input_dir, "\n",
  "Output Directory: ", output_dir, "\n\n",
  "Files Processed: ", length(files), "\n\n",
  "Description:\n",
  "This directory contains cue tables and reference times exported from MATLAB .mat files\n",
  "to CSV format. Each original .mat file generates two CSV files:\n",
  "- [basename].csv: Contains the cue table data\n",
  "- [basename]_ref_time.csv: Contains the reference time data\n"
)

writeLines(readme_content, "README.txt")
cat("Processing complete. Generated", length(files) * 2, "CSV files, combined ref_times files, and README.txt\n")
cat("Combined ref_times dataframe contains", nrow(ref_times_all), "entries\n")
