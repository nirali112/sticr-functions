faasr_classify_wetdry <- function() {
  # Step 3: STICr Classification Function - Dynamic Version
  # Input: step2-calibrated/*.csv (from Step 2)
  # Output: step3-classified/*.csv
  # Uses: STICr::classify_wetdry()
  
  library(tidyverse)
  library(lubridate)
  library(STICr)
  cat("Libraries loaded for Step 3: Classification\n")
  
  # Step 3: Simple pattern generation based on Step 2 outputs
  # Only needs to check what Step 2 actually produced
  
  # Generate patterns for Step 2 outputs: [original_name]_step2_calibrated.csv
  # Based on your known file patterns - much more targeted
  known_bases <- c("02M10", "04W02", "04W03", "04W04", "04T02", "20M01", "SFM01", "SFM07", "SFT01", "04SW3")
  types <- c("LS", "HS", "SP", "SW")
  years <- c("2021", "2022", "2023", "2024")
  
  # Generate targeted step2 output patterns (~160 patterns - very fast!)
  step2_patterns <- expand.grid(base = known_bases, type = types, year = years)
  stic_step2_files <- paste0("STIC_GP_KNZ_", step2_patterns$base, "_", step2_patterns$type, "_", step2_patterns$year, "_step2_calibrated.csv")
  
  # Add raw file step2 outputs
  raw_step2_files <- c("raw_hobo_data_step2_calibrated.csv", "hobo_raw_step2_calibrated.csv", "raw_stic_data_step2_calibrated.csv")
  
  potential_step2_files <- c(stic_step2_files, raw_step2_files)
  cat("Generated", length(potential_step2_files), "targeted Step 2 output patterns\n")
  
  # Find available Step 2 files and check if already processed
  available_step2_files <- c()
  files_to_process <- c()
  
  for(file_name in potential_step2_files) {
    tryCatch({
      # Test if Step 2 file exists
      faasr_get_file(remote_folder = "sticr-workflow/step2-calibrated", 
                     remote_file = file_name, 
                     local_file = paste0("test_", file_name))
      
      # If we get here, Step 2 file exists
      available_step2_files <- c(available_step2_files, file_name)
      cat("Found Step 2 output:", file_name, "\n")
      
      # Clean up test file
      if(file.exists(paste0("test_", file_name))) {
        file.remove(paste0("test_", file_name))
      }
      
      # Check if already processed in Step 3
      clean_filename <- gsub("_step2_calibrated\\.csv$", "", file_name)
      step3_filename <- paste0(clean_filename, "_step3_classified.csv")
      
      # Test if Step 3 output already exists
      already_processed <- tryCatch({
        faasr_get_file(remote_folder = "sticr-workflow/step3-classified", 
                       remote_file = step3_filename, 
                       local_file = paste0("test_step3_", step3_filename))
        
        # Clean up test file
        if(file.exists(paste0("test_step3_", step3_filename))) {
          file.remove(paste0("test_step3_", step3_filename))
        }
        
        cat("  ↳ Already processed - SKIPPING:", step3_filename, "\n")
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
      # Step 2 file doesn't exist - skip silently
    })
  }
  
  if(length(available_step2_files) == 0) {
    cat("No Step 2 files found! Run Step 2 first.\n")
    return("No Step 2 files found - run Step 2 first")
  }
  
  if(length(files_to_process) == 0) {
    cat("All Step 2 files already processed! No new files to classify.\n")
    return("All files already processed - no new classification needed")
  }
  
  cat("Found", length(available_step2_files), "Step 2 files,", length(files_to_process), "need processing\n")
  
  # Process only the new/unprocessed files
  processed_files <- 0
  
  for(file_name in files_to_process) {
    tryCatch({
      cat("Processing:", file_name, "\n")
      
      # Download and read Step 2 data
      faasr_get_file(remote_folder = "sticr-workflow/step2-calibrated", 
                     remote_file = file_name, 
                     local_file = "current_step2.csv")
      
      calibrated_data <- read.csv("current_step2.csv", stringsAsFactors = FALSE)
      
      # Ensure datetime is POSIXct
      if (!inherits(calibrated_data$datetime, "POSIXct")) {
        calibrated_data$datetime <- as.POSIXct(calibrated_data$datetime)
      }
      
      # Apply STICr classification using SpC
      # Use absolute threshold method - you can adjust threshold as needed
      classified_data <- STICr::classify_wetdry(
        stic_data = calibrated_data,
        classify_var = "SpC",  # Use specific conductivity for classification
        threshold = 100,       # Threshold in µS/cm (adjust based on your data)
        method = "absolute"    # Options: "absolute", "percent", "y-intercept"
      )
      
      # Ensure wetdry column exists
      if (!"wetdry" %in% colnames(classified_data)) {
        stop("Classification failed - wetdry column not created")
      }
      
      # Prepare output - keep all previous columns plus wetdry
      output_data <- classified_data %>%
        select(datetime, condUncal, tempC, SpC, wetdry) %>%
        arrange(datetime)
      
      # Save output
      clean_filename <- gsub("_step2_calibrated\\.csv$", "", file_name)
      output_filename <- paste0("step3_classified_", clean_filename, ".csv")
      
      write.csv(output_data, output_filename, row.names = FALSE)
      
      # Upload to MinIO
      remote_filename <- paste0(clean_filename, "_step3_classified.csv")
      faasr_put_file(local_file = output_filename,
                     remote_folder = "sticr-workflow/step3-classified",
                     remote_file = remote_filename)
      
      processed_files <- processed_files + 1
      
      # Quick summary of classification results
      wet_count <- sum(output_data$wetdry == "wet", na.rm = TRUE)
      dry_count <- sum(output_data$wetdry == "dry", na.rm = TRUE)
      
      cat("✓ Classified:", clean_filename, "->", nrow(output_data), "rows\n")
      cat("  Wet:", wet_count, "| Dry:", dry_count, "\n")
      
    }, error = function(e) {
      cat("✗ Failed:", file_name, "-", e$message, "\n")
    })
  }
  
  cat("=== Step 3 Complete ===\n")
  cat("Processed", processed_files, "new files out of", length(available_step2_files), "total Step 2 files\n")
  cat("Skipped", length(available_step2_files) - length(files_to_process), "already processed files\n")
  
  return(paste("Step 3 classification completed:", processed_files, "new files processed,", 
               length(available_step2_files) - length(files_to_process), "files skipped (already processed)"))
}
