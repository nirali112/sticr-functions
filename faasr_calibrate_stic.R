faasr_calibrate_stic <- function() {
  # Step 2: STICr Calibration Function
  # Input: step1-tidy/*.csv (from Step 1)
  # Output: step2-calibrated/*.csv
  
  library(tidyverse)
  library(lubridate)
  library(STICr)
  cat("Libraries loaded for Step 2: Calibration\n")
  
  # Auto-discover Step 1 output files
  potential_step1_files <- c(
    "STIC_GP_KNZ_02M10_LS_2022_step1_tidy.csv",
    "STIC_GP_KNZ_04SW3_SP_2024_step1_tidy.csv",
    "raw_hobo_data_step1_tidy.csv",
    "stic_data_step1_tidy.csv",
    "STIC_data_step1_tidy.csv"
  )
  
  # Find available Step 1 files
  available_step1_files <- c()
  
  for(file_name in potential_step1_files) {
    tryCatch({
      faasr_get_file(remote_folder = "sticr-workflow/step1-tidy", 
                     remote_file = file_name, 
                     local_file = paste0("test_", file_name))
      
      available_step1_files <- c(available_step1_files, file_name)
      cat("Found Step 1 output:", file_name, "\n")
      
      if(file.exists(paste0("test_", file_name))) {
        file.remove(paste0("test_", file_name))
      }
      
    }, error = function(e) {
      # File doesn't exist - skip
    })
  }
  
  if(length(available_step1_files) == 0) {
    cat("No Step 1 files found! Run Step 1 first.\n")
    return("No Step 1 files found")
  }
  
  cat("Processing", length(available_step1_files), "files for calibration\n")
  
  # Process each Step 1 file
  processed_files <- 0
  
  for(file_name in available_step1_files) {
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
      
      # Save output
      clean_filename <- gsub("_step1_tidy\\.csv$", "", file_name)
      output_filename <- paste0("step2_calibrated_", clean_filename, ".csv")
      
      write.csv(output_data, output_filename, row.names = FALSE)
      
      # Upload to MinIO
      remote_filename <- paste0(clean_filename, "_step2_calibrated.csv")
      faasr_put_file(local_file = output_filename,
                     remote_folder = "sticr-workflow/step2-calibrated",
                     remote_file = remote_filename)
      
      processed_files <- processed_files + 1
      cat("✓ Calibrated:", clean_filename, "->", nrow(output_data), "rows\n")
      
    }, error = function(e) {
      cat("✗ Failed:", file_name, "-", e$message, "\n")
    })
  }
  
  cat("=== Step 2 Complete ===\n")
  cat("Calibrated", processed_files, "of", length(available_step1_files), "files\n")
  
  return(paste("Step 2 calibration completed:", processed_files, "files processed"))
}
