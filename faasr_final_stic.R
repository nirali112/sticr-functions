faasr_final_stic <- function() {
  # Step 4: Final STIC Output Function
  # Input: step3-classified/*.csv (from Step 3)
  # Output: step4-final/*.csv (analysis-ready data)
  
  library(tidyverse)
  library(lubridate)
  library(STICr)
  cat("Libraries loaded for Step 4: Final Output\n")
  

  folder_contents <- faasr_get_folder_list(faasr_prefix = "sticr-workflow/step3-classified")
 
  # Convert list to character vector and filter for CSV files
  all_step3_files <- unlist(folder_contents)
  potential_step3_files <- all_step3_files[grepl("\\.csv$", all_step3_files, ignore.case = TRUE)]
  
  # Remove the folder prefix from filenames for processing
  potential_step3_files <- gsub("^sticr-workflow/step3-classified/", "", potential_step3_files)
  
  # Find available Step 3 files and check if already processed
  available_step3_files <- c()
  files_to_process <- c()
  
  for(file_name in potential_step3_files) {
    tryCatch({
      # Test if Step 3 file exists
      faasr_get_file(remote_folder = "sticr-workflow/step3-classified", 
                     remote_file = file_name, 
                     local_file = paste0("test_", file_name))
      
      # If we get here, Step 3 file exists
      available_step3_files <- c(available_step3_files, file_name)
      cat("Found Step 3 output:", file_name, "\n")
      
      # Clean up test file
      if(file.exists(paste0("test_", file_name))) {
        file.remove(paste0("test_", file_name))
      }
      
      # Check if already processed in Step 4
      clean_filename <- gsub("_step3_classified\\.csv$", "", file_name)
      step4_filename <- paste0(clean_filename, "_step4_final.csv")
      
      # Test if Step 4 output already exists
      already_processed <- tryCatch({
        faasr_get_file(remote_folder = "sticr-workflow/step4-final", 
                       remote_file = step4_filename, 
                       local_file = paste0("test_step4_", step4_filename))
        
        # Clean up test file
        if(file.exists(paste0("test_step4_", step4_filename))) {
          file.remove(paste0("test_step4_", step4_filename))
        }
        
        cat("Already processed - SKIPPING:", step4_filename, "\n")
        TRUE  # File exists, already processed
      }, error = function(e) {
        cat("Not yet processed - WILL PROCESS\n")
        FALSE  # File doesn't exist, needs processing
      })
      
      # Add to processing queue only if not already processed
      if(!already_processed) {
        files_to_process <- c(files_to_process, file_name)
      }
      
    }, error = function(e) {
      # Step 3 file doesn't exist - skip silently
    })
  }
  
  if(length(available_step3_files) == 0) {
    cat("No Step 3 files found! Run Step 3 first.\n")
    return("No Step 3 files found - run Step 3 first")
  }
  
  if(length(files_to_process) == 0) {
    cat("All Step 3 files already processed! No new files to finalize.\n")
    return("All files already processed - no new final outputs needed")
  }
  
  cat("Found", length(available_step3_files), "Step 3 files,", length(files_to_process), "need processing\n")
  
  # Process only the new/unprocessed files
  processed_files <- 0
  
  for(file_name in files_to_process) {
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
      
      # summary statistics
      wet_count <- sum(final_data$wetdry == "wet", na.rm = TRUE)
      dry_count <- sum(final_data$wetdry == "dry", na.rm = TRUE)
      wet_percentage <- round((wet_count / nrow(final_data)) * 100, 1)
      
      date_range <- paste(
        format(min(final_data$datetime), "%Y-%m-%d"),
        "to",
        format(max(final_data$datetime), "%Y-%m-%d")
      )
      
      cat(" Final dataset:", clean_filename, "->", nrow(final_data), "rows\n")
      cat(" Date range:", date_range, "\n")
      cat(" Wet:", wet_count, "(", wet_percentage, "%) | Dry:", dry_count, "\n")
      cat(" SpC range:", round(min(final_data$SpC, na.rm = TRUE), 1), "-", 
          round(max(final_data$SpC, na.rm = TRUE), 1), "µS/cm\n")
      
    }, error = function(e) {
      cat("✗ Failed:", file_name, "-", e$message, "\n")
    })
  }
  
  cat("STICr Workflow Complete\n")
  cat("Processed", processed_files, "new files out of", length(available_step3_files), "total Step 3 files\n")
  cat("Skipped", length(available_step3_files) - length(files_to_process), "already processed files\n")
  cat("Analysis-ready data saved to: sticr-workflow/step4-final/\n")
  
  if(processed_files > 0) {
    cat("STIC data processing pipeline completed successfully!\n")
  } else {
    cat("All files were already processed - pipeline up to date!\n")
  }
  
  return(paste("STICr workflow completed:", processed_files, "new analysis-ready datasets created,", 
               length(available_step3_files) - length(files_to_process), "files skipped (already processed)"))
}
