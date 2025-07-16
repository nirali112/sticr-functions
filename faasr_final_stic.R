faasr_final_stic <- function() {
  # Step 4: Final STIC Output Function
  # Input: step3-classified/*.csv (from Step 3)
  # Output: step4-final/*.csv (analysis-ready data)
  
  library(tidyverse)
  library(lubridate)
  library(STICr)
  cat("Libraries loaded for Step 4: Final Output\n")
  
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
  
  cat("Processing", length(available_step3_files), "files for final output\n")
  
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
      
      # Apply optional QAQC before creating final output
      qaqc_data <- STICr::qaqc_stic_data(
        stic_data = classified_data,
        spc_neg_correction = TRUE,
        inspect_deviation = TRUE,
        deviation_size = 1,
        window_size = 6
      )
      
      # Ensure QAQC column exists
      if (!"QAQC" %in% colnames(qaqc_data)) {
        qaqc_data$QAQC <- ""
      }
      
      cat("QAQC applied to", file_name, "\n")
      
      # Create final analysis-ready output with QAQC (STICr standard format)
      final_data <- qaqc_data %>%
        select(datetime, condUncal, tempC, SpC, wetdry, QAQC) %>%
        arrange(datetime)
      
      # Save output
      clean_filename <- gsub("_step3_classified\\.csv$", "", file_name)
      output_filename <- paste0("step4_final_", clean_filename, ".csv")
      
      write.csv(final_data, output_filename, row.names = FALSE)
      
      # Upload to MinIO
      remote_filename <- paste0(clean_filename, "_step4_final.csv")
      faasr_put_file(local_file = output_filename,
                     remote_folder = "sticr-workflow/step4-final",
                     remote_file = remote_filename)
      
      processed_files <- processed_files + 1
      
      # Quick summary statistics
      wet_count <- sum(final_data$wetdry == "wet", na.rm = TRUE)
      dry_count <- sum(final_data$wetdry == "dry", na.rm = TRUE)
      wet_percentage <- round((wet_count / nrow(final_data)) * 100, 1)
      
      date_range <- paste(
        format(min(final_data$datetime), "%Y-%m-%d"),
        "to",
        format(max(final_data$datetime), "%Y-%m-%d")
      )
      
      cat("✓ Final dataset:", clean_filename, "->", nrow(final_data), "rows\n")
      cat("  Date range:", date_range, "\n")
      cat("  Wet:", wet_count, "(", wet_percentage, "%) | Dry:", dry_count, "\n")
      cat("  SpC range:", round(min(final_data$SpC, na.rm = TRUE), 1), "-", 
          round(max(final_data$SpC, na.rm = TRUE), 1), "µS/cm\n")
      
    }, error = function(e) {
      cat("✗ Failed:", file_name, "-", e$message, "\n")
    })
  }
  
  cat("=== STICr Workflow Complete ===\n")
  cat("Final datasets created:", processed_files, "of", length(available_step3_files), "files\n")
  cat("Analysis-ready data saved to: sticr-workflow/step4-final/\n")
  cat("🎉 STIC data processing pipeline completed successfully!\n")
  
  return(paste("STICr workflow completed:", processed_files, "analysis-ready datasets created"))
}
