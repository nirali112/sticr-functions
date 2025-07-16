faasr_classify_wetdry <- function() {
  # Step 3: STICr Classification Function
  # Input: step2-calibrated/*.csv (from Step 2)
  # Output: step3-classified/*.csv
  # Uses: STICr::classify_wetdry()
  
  library(tidyverse)
  library(lubridate)
  library(STICr)
  cat("Libraries loaded for Step 3: Classification\n")
  
  # Auto-discover Step 2 output files
  potential_step2_files <- c(
    "STIC_GP_KNZ_02M10_LS_2022_step2_calibrated.csv",
    "STIC_GP_KNZ_04SW3_SP_2024_step2_calibrated.csv",
    "raw_hobo_data_step2_calibrated.csv",
    "stic_data_step2_calibrated.csv",
    "STIC_data_step2_calibrated.csv"
  )
  
  # Find available Step 2 files
  available_step2_files <- c()
  
  for(file_name in potential_step2_files) {
    tryCatch({
      faasr_get_file(remote_folder = "sticr-workflow/step2-calibrated", 
                     remote_file = file_name, 
                     local_file = paste0("test_", file_name))
      
      available_step2_files <- c(available_step2_files, file_name)
      cat("Found Step 2 output:", file_name, "\n")
      
      if(file.exists(paste0("test_", file_name))) {
        file.remove(paste0("test_", file_name))
      }
      
    }, error = function(e) {
      # File doesn't exist - skip
    })
  }
  
  if(length(available_step2_files) == 0) {
    cat("No Step 2 files found! Run Step 2 first.\n")
    return("No Step 2 files found")
  }
  
  cat("Processing", length(available_step2_files), "files for classification\n")
  
  # Process each Step 2 file
  processed_files <- 0
  
  for(file_name in available_step2_files) {
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
  cat("Classified", processed_files, "of", length(available_step2_files), "files\n")
  
  return(paste("Step 3 classification completed:", processed_files, "files processed"))
}
