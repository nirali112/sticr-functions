faasr_qaqc_stic <- function() {
  # Step 4: STICr QAQC Function
  # Input: step3-classified/*.csv (from Step 3)
  # Output: step4-qaqc/*.csv
  # Uses: STICr::qaqc_stic_data()
  
  library(tidyverse)
  library(lubridate)
  library(STICr)
  cat("Libraries loaded for Step 4: QAQC\n")
  
  # Auto-discover Step 3 output files
  potential_step3_files <- c(
    "STIC_GP_KNZ_02M10_LS_2022_step3_classified.csv",
    "STIC_GP_KNZ_04SW3_SP_2024_step3_classified.csv",
    "raw_hobo_data_step3_classified.csv",
    "stic_data_step3_classified.csv",
    "STIC_data_step3_classified.csv"
  )
  
  # Find available Step 3 files
  available_step3_files <- c()
  
  for(file_name in potential_step3_files) {
    tryCatch({
      faasr_get_file(remote_folder = "sticr-workflow/step3-classified", 
                     remote_file = file_name, 
                     local_file = paste0("test_", file_name))
      
      available_step3_files <- c(available_step3_files, file_name)
      cat("Found Step 3 output:", file_name, "\n")
      
      if(file.exists(paste0("test_", file_name))) {
        file.remove(paste0("test_", file_name))
      }
      
    }, error = function(e) {
      # File doesn't exist - skip
    })
  }
  
  if(length(available_step3_files) == 0) {
    cat("No Step 3 files found! Run Step 3 first.\n")
    return("No Step 3 files found")
  }
  
  cat("Processing", length(available_step3_files), "files for QAQC\n")
  
  # Process each Step 3 file
  processed_files <- 0
  
  for(file_name in available_step3_files) {
    tryCatch({
      cat("Processing:", file_name, "\n")
      
      # Download and read Step 3 data
      faasr_get_file(remote_folder = "sticr-workflow/step3-classified", 
                     remote_file = file_name, 
                     local_file = "current_step3.csv")
      
      classified_data <- read.csv("current_step3.csv", stringsAsFactors = FALSE)
      
      # Ensure datetime is POSIXct
      if (!inherits(classified_data$datetime, "POSIXct")) {
        classified_data$datetime <- as.POSIXct(classified_data$datetime)
      }
      
      # Apply STICr QAQC with multiple checks
      qaqc_data <- STICr::qaqc_stic_data(
        stic_data = classified_data,
        spc_neg_correction = TRUE,    # Correct negative SpC values to 0
        inspect_deviation = TRUE,     # Flag short-term anomalies
        window_size = 6,              # 6 observations around anomaly
        anomaly_size = 1,             # Single point anomalies
        outside_std_range_flag = TRUE # Flag data outside calibration range
      )
      
      # Ensure QAQC column exists
      if (!"QAQC" %in% colnames(qaqc_data)) {
        # If QAQC function didn't add column, create empty one
        qaqc_data$QAQC <- ""
        cat("Note: No QAQC flags generated for", file_name, "\n")
      }
      
      # Prepare output - keep all previous columns plus QAQC
      output_data <- qaqc_data %>%
        select(datetime, condUncal, tempC, SpC, wetdry, QAQC) %>%
        arrange(datetime)
      
      # Save output
      clean_filename <- gsub("_step3_classified\\.csv$", "", file_name)
      output_filename <- paste0("step4_qaqc_", clean_filename, ".csv")
      
      write.csv(output_data, output_filename, row.names = FALSE)
      
      # Upload to MinIO
      remote_filename <- paste0(clean_filename, "_step4_qaqc.csv")
      faasr_put_file(local_file = output_filename,
                     remote_folder = "sticr-workflow/step4-qaqc",
                     remote_file = remote_filename)
      
      processed_files <- processed_files + 1
      
      # Quick summary of QAQC results
      qaqc_flags <- qaqc_data$QAQC[qaqc_data$QAQC != ""]
      flag_count <- length(qaqc_flags)
      
      cat("✓ QAQC processed:", clean_filename, "->", nrow(output_data), "rows\n")
      if(flag_count > 0) {
        cat("  QAQC flags:", flag_count, "issues found\n")
        unique_flags <- unique(qaqc_flags)
        cat("  Flag types:", paste(unique_flags, collapse = ", "), "\n")
      } else {
        cat("  No QAQC issues detected\n")
      }
      
    }, error = function(e) {
      cat("✗ Failed:", file_name, "-", e$message, "\n")
    })
  }
  
  cat("=== Step 4 Complete ===\n")
  cat("QAQC processed", processed_files, "of", length(available_step3_files), "files\n")
  
  return(paste("Step 4 QAQC completed:", processed_files, "files processed"))
}
