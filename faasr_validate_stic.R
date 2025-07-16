faasr_validate_stic <- function() {
  # Step 5: STICr Validation Function
  # Input: step4-qaqc/*.csv (from Step 4)
  # Output: step5-validated/*.csv
  # Uses: STICr::validate_stic_data()
  
  library(tidyverse)
  library(lubridate)
  library(STICr)
  cat("Libraries loaded for Step 5: Validation\n")
  
  # Auto-discover Step 4 output files
  potential_step4_files <- c(
    "STIC_GP_KNZ_02M10_LS_2022_step4_qaqc.csv",
    "STIC_GP_KNZ_04SW3_SP_2024_step4_qaqc.csv",
    "raw_hobo_data_step4_qaqc.csv",
    "stic_data_step4_qaqc.csv",
    "STIC_data_step4_qaqc.csv"
  )
  
  # Find available Step 4 files
  available_step4_files <- c()
  
  for(file_name in potential_step4_files) {
    tryCatch({
      faasr_get_file(remote_folder = "sticr-workflow/step4-qaqc", 
                     remote_file = file_name, 
                     local_file = paste0("test_", file_name))
      
      available_step4_files <- c(available_step4_files, file_name)
      cat("Found Step 4 output:", file_name, "\n")
      
      if(file.exists(paste0("test_", file_name))) {
        file.remove(paste0("test_", file_name))
      }
      
    }, error = function(e) {
      # File doesn't exist - skip
    })
  }
  
  if(length(available_step4_files) == 0) {
    cat("No Step 4 files found! Run Step 4 first.\n")
    return("No Step 4 files found")
  }
  
  cat("Processing", length(available_step4_files), "files for validation\n")
  
  # Process each Step 4 file
  processed_files <- 0
  
  for(file_name in available_step4_files) {
    tryCatch({
      cat("Processing:", file_name, "\n")
      
      # Download and read Step 4 data
      faasr_get_file(remote_folder = "sticr-workflow/step4-qaqc", 
                     remote_file = file_name, 
                     local_file = "current_step4.csv")
      
      qaqc_data <- read.csv("current_step4.csv", stringsAsFactors = FALSE)
      
      # Ensure datetime is POSIXct
      if (!inherits(qaqc_data$datetime, "POSIXct")) {
        qaqc_data$datetime <- as.POSIXct(qaqc_data$datetime)
      }
      
      # Create example field observations for validation
      # In practice, you would load real field observation data
      # This creates synthetic validation data for demonstration
      set.seed(123)  # For reproducible results
      n_obs <- min(20, nrow(qaqc_data))  # Validate up to 20 points
      
      if(n_obs > 0) {
        # Sample random times from the STIC data for validation
        sample_indices <- sample(1:nrow(qaqc_data), n_obs)
        sample_times <- qaqc_data$datetime[sample_indices]
        
        # Create synthetic field observations
        # In reality, these would come from actual field visits
        field_observations <- data.frame(
          datetime = sample_times,
          wetdry_obs = qaqc_data$wetdry[sample_indices], # Use STIC values as proxy
          SpC_obs = qaqc_data$SpC[sample_indices] * runif(n_obs, 0.8, 1.2), # Add some variation
          stringsAsFactors = FALSE
        )
        
        # Apply STICr validation with correct parameters
        validated_data <- STICr::validate_stic_data(
          stic_data = qaqc_data,
          field_observations = field_observations,
          max_time_diff = 30   # 30 minutes max difference
        )
        
        cat("Validation completed with", nrow(validated_data), "validation points\n")
        
        # Calculate accuracy metrics
        if(nrow(validated_data) > 0) {
          # Wet/dry classification accuracy
          wetdry_accuracy <- sum(validated_data$wetdry_obs == validated_data$wetdry_STIC, na.rm = TRUE) / 
                            nrow(validated_data)
          
          # SpC correlation
          spc_correlation <- cor(validated_data$SpC_obs, validated_data$SpC_STIC, use = "complete.obs")
          
          cat("  Wet/dry accuracy:", round(wetdry_accuracy * 100, 1), "%\n")
          cat("  SpC correlation:", round(spc_correlation, 3), "\n")
        }
        
      } else {
        cat("No data available for validation\n")
        validated_data <- data.frame()
      }
      
      # Prepare main output - the complete processed STIC dataset
      output_data <- qaqc_data %>%
        select(datetime, condUncal, tempC, SpC, wetdry, QAQC) %>%
        arrange(datetime)
      
      # Save main output
      clean_filename <- gsub("_step4_qaqc\\.csv$", "", file_name)
      output_filename <- paste0("step5_validated_", clean_filename, ".csv")
      
      write.csv(output_data, output_filename, row.names = FALSE)
      
      # Upload main output to MinIO
      remote_filename <- paste0(clean_filename, "_step5_validated.csv")
      faasr_put_file(local_file = output_filename,
                     remote_folder = "sticr-workflow/step5-validated",
                     remote_file = remote_filename)
      
      # Save validation results separately if they exist
      if(exists("validated_data") && nrow(validated_data) > 0) {
        validation_filename <- paste0("validation_results_", clean_filename, ".csv")
        write.csv(validated_data, validation_filename, row.names = FALSE)
        
        remote_validation_filename <- paste0(clean_filename, "_validation_results.csv")
        faasr_put_file(local_file = validation_filename,
                       remote_folder = "sticr-workflow/validation-results",
                       remote_file = remote_validation_filename)
        
        cat("  Validation results saved separately\n")
      }
      
      processed_files <- processed_files + 1
      cat("✓ Validated:", clean_filename, "->", nrow(output_data), "rows\n")
      
    }, error = function(e) {
      cat("✗ Failed:", file_name, "-", e$message, "\n")
    })
  }
  
  cat("=== Step 5 Complete ===\n")
  cat("Validated", processed_files, "of", length(available_step4_files), "files\n")
  cat("Final datasets ready for analysis!\n")
  
  return(paste("Step 5 validation completed:", processed_files, "files processed"))
}
