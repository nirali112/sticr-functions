faasr_calibrate_stic <- function() {
  # Step 2: STICr Calibration Function
  # Input: step1-tidy/*.csv (from Step 1)
  # Output: step2-calibrated/*.csv
  
  library(tidyverse)
  library(lubridate)
  library(STICr)
  cat("Libraries loaded \n")
  
  # Step 2: Simple pattern generation based on Step 1 outputs
  
  # Generate patterns for Step 1 outputs: [original_name]_step1_tidy.csv
  # Based on your known file patterns, much more targeted
  known_bases <- c("02M10", "04W02", "04W03", "04W04", "04T02", "20M01", "SFM01", "SFM07", "SFT01", "04SW3")
  types <- c("LS", "HS", "SP", "SW")
  years <- c("2021", "2022", "2023", "2024")
  
  # Generate targeted step1 output patterns
  step1_patterns <- expand.grid(base = known_bases, type = types, year = years)
  stic_step1_files <- paste0("STIC_GP_KNZ_", step1_patterns$base, "_", step1_patterns$type, "_", step1_patterns$year, "_step1_tidy.csv")
  
  # Add raw file step1 outputs
  raw_step1_files <- c("raw_hobo_data_step1_tidy.csv", "hobo_raw_step1_tidy.csv", "raw_stic_data_step1_tidy.csv")
  
  potential_step1_files <- c(stic_step1_files, raw_step1_files)
  cat("Generated", length(potential_step1_files), "targeted Step 1 output patterns\n")
  
  # Find available Step 1 files and check if already processed
  available_step1_files <- c()
  files_to_process <- c()
  
  for(file_name in potential_step1_files) {
    tryCatch({
      # Test if Step 1 file exists
      faasr_get_file(remote_folder = "sticr-workflow/step1-tidy", 
                     remote_file = file_name, 
                     local_file = paste0("test_", file_name))
      
      # If we get here, Step 1 file exists
      available_step1_files <- c(available_step1_files, file_name)
      cat("Found Step 1 output:", file_name, "\n")
      
      # Clean up test file
      if(file.exists(paste0("test_", file_name))) {
        file.remove(paste0("test_", file_name))
      }
      
      # Check if already processed in Step 2
      clean_filename <- gsub("_step1_tidy\\.csv$", "", file_name)
      step2_filename <- paste0(clean_filename, "_step2_calibrated.csv")
      
      # Test if Step 2 output already exists
      already_processed <- tryCatch({
        faasr_get_file(remote_folder = "sticr-workflow/step2-calibrated", 
                       remote_file = step2_filename, 
                       local_file = paste0("test_step2_", step2_filename))
        
        # Clean up test file
        if(file.exists(paste0("test_step2_", step2_filename))) {
          file.remove(paste0("test_step2_", step2_filename))
        }
        
        cat("  ↳ Already processed - SKIPPING:", step2_filename, "\n")
        TRUE  # File exists, already processed
      }, error = function(e) {
        cat("  ↳ Not yet processed - WILL PROCESS\n")
        FALSE  # File doesn't exist, needs processing
      })
      
      # Add to processing queue only if not already processed
      if(!already_processed) {
        files_to_process <- c(files_to_process, file_name)
      }
      
    }, error = function(e) {
      # Step 1 file doesn't exist - skip silently
    })
  }
  
  if(length(available_step1_files) == 0) {
    cat("No Step 1 files found! Run Step 1 first.\n")
    return("No Step 1 files found - run Step 1 first")
  }
  
  if(length(files_to_process) == 0) {
    cat("All Step 1 files already processed! No new files to calibrate.\n")
    return("All files already processed - no new calibration needed")
  }
  
  cat("Found", length(available_step1_files), "Step 1 files,", length(files_to_process), "need processing\n")
  
  # Process only the new/unprocessed files
  processed_files <- 0
  
  for(file_name in files_to_process) {
    tryCatch({
      cat("Processing:", file_name, "\n")
      
      # Download and read Step 1 data
      faasr_get_file(remote_folder = "sticr-workflow/step1-tidy", 
                     remote_file = file_name, 
                     local_file = "current_step1.csv")
      
      stic_data <- read.csv("current_step1.csv", stringsAsFactors = FALSE)
      
      # Ensure datetime is POSIXct
      if (!inherits(stic_data$datetime, "POSIXct")) {
        stic_data$datetime <- as.POSIXct(stic_data$datetime)
      }
      
      # Create example calibration data (replace with real calibration data)
      calibration_data <- data.frame(
        sensor = rep(20946471, 4),
        standard = c(100, 250, 500, 1000),
        condUncal = c(12400, 23422, 46845, 104712)
      )
      
      # Apply STICr calibration
      calibration_params <- STICr::get_calibration(calibration_data)
      calibrated_data <- STICr::apply_calibration(stic_data, calibration_params)
      
      # Ensure SpC column exists
      if (!"SpC" %in% colnames(calibrated_data)) {
        calibrated_data$SpC <- calibrated_data$condUncal
      }
      
      # Prepare output
      output_data <- calibrated_data %>%
        select(datetime, condUncal, tempC, SpC) %>%
        arrange(datetime)
      
      # Generate output filename (remove _step1_tidy.csv and add step2)
      clean_filename <- gsub("_step1_tidy\\.csv$", "", file_name)
      output_filename <- paste0("step2_calibrated_", clean_filename, ".csv")
      
      # Save locally
      write.csv(output_data, output_filename, row.names = FALSE)
      
      # Upload to MinIO
      remote_filename <- paste0(clean_filename, "_step2_calibrated.csv")
      faasr_put_file(local_file = output_filename,
                     remote_folder = "sticr-workflow/step2-calibrated",
                     remote_file = remote_filename)
      
      processed_files <- processed_files + 1
      cat("✓ Calibrated:", clean_filename, "->", nrow(output_data), "rows\n")
      
    }, error = function(e) {
      cat("Failed:", file_name, "-", e$message, "\n")
    })
  }
  
  cat("=== Step 2 Complete ===\n")
  cat("Processed", processed_files, "new files out of", length(available_step1_files), "total Step 1 files\n")
  cat("Skipped", length(available_step1_files) - length(files_to_process), "already processed files\n")
  
  return(paste("Step 2 calibration completed:", processed_files, "new files processed,", 
               length(available_step1_files) - length(files_to_process), "files skipped (already processed)"))
}
