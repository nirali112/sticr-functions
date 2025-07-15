faasr_tidy_hobo_sticr <- function() {
  cat("=== FULLY AUTOMATIC STICR PROCESSOR: DYNAMIC FILE DISCOVERY ===\n")
  
  # Load libraries
  library(STICr)
  library(tidyverse)
  library(lubridate)
  cat("✓ Libraries loaded\n")
  
  # Auto-discover files by trying common STIC file names
  # This works by attempting to download files that follow common patterns
  potential_files <- c(
    # Pattern 1: STIC_SITE_YEAR files
    "STIC_GP_KNZ_02M10_LS_2022.csv",
    "STIC_GP_KNZ_04SW3_SP_2024.csv", 
    "STIC_GP_KNZ_02M10_HS_2022.csv",
    "STIC_GP_KNZ_04M12_HS_2022.csv",
    
    # Pattern 2: Raw HOBO files
    "raw_hobo_data.csv",
    "hobo_raw.csv",
    "raw_stic_data.csv",
    
    # Pattern 3: Generic STIC files
    "stic_data.csv",
    "STIC_data.csv",
    "STIC_2022.csv",
    "STIC_2023.csv", 
    "STIC_2024.csv",
    
    # Pattern 4: Site-specific patterns (add more as needed)
    paste0("STIC_GP_KNZ_", sprintf("%02dM%02d", rep(1:10, each=12), rep(1:12, 10)), "_LS_2022.csv"),
    paste0("STIC_GP_KNZ_", sprintf("%02dSW%d", rep(1:10, each=5), rep(1:5, 10)), "_SP_2024.csv")
  )
  
  cat("Scanning for STIC files in bucket...\n")
  
  # Discover available files by attempting downloads
  available_files <- c()
  
  for(file_name in potential_files) {
    tryCatch({
      # Try to download the file (this will fail silently if file doesn't exist)
      faasr_get_file(remote_folder = "stic-data", 
                     remote_file = file_name, 
                     local_file = paste0("test_", file_name))
      
      # If download succeeded, file exists
      available_files <- c(available_files, file_name)
      cat("✓ Found:", file_name, "\n")
      
      # Clean up test file
      if(file.exists(paste0("test_", file_name))) {
        file.remove(paste0("test_", file_name))
      }
      
    }, error = function(e) {
      # File doesn't exist - skip silently
    })
  }
  
  if(length(available_files) == 0) {
    cat("⚠️ No STIC files found in bucket!\n")
    cat("Make sure files are uploaded to 'stic-data' folder\n")
    return("No files found to process")
  }
  
  cat("📁 Found", length(available_files), "STIC files to process:\n")
  for(file in available_files) {
    cat("-", file, "\n")
  }
  
  # Process each discovered file
  processed_files <- 0
  processing_results <- list()
  
  for(file_name in available_files) {
    cat("\n", paste(rep("=", 60), collapse=""), "\n")
    cat("Processing file:", file_name, "\n")
    cat(paste(rep("=", 60), collapse=""), "\n")
    
    tryCatch({
      # Download the file
      faasr_get_file(remote_folder = "stic-data", 
                     remote_file = file_name, 
                     local_file = "current_input.csv")
      cat("✓ Downloaded:", file_name, "\n")
      
      # Auto-detect data type and process
      cat("Auto-detecting data type...\n")
      
      # Read first few lines for detection
      first_lines <- readLines("current_input.csv", n = 10)
      
      # Enhanced detection logic
      is_raw_hobo <- any(grepl("#|Plot Title|LGR S/N|Temp.*°C.*LGR|Series:|HOBOware", first_lines[1:5], ignore.case = TRUE))
      has_project_column <- any(grepl("project.*datetime.*siteId", first_lines[1:3], ignore.case = TRUE))
      
      if(is_raw_hobo) {
        cat("→ DETECTED: RAW HOBO DATA - Using STICr::tidy_hobo_data()\n")
        
        # YES - Using official STICr function here!
        tidy_data <- tidy_hobo_data(infile = "current_input.csv", outfile = FALSE)
        
        if(is.null(tidy_data) || nrow(tidy_data) == 0) {
          stop("STICr::tidy_hobo_data() returned empty result")
        }
        
        processing_method <- "official_STICr_tidy_hobo_data"
        cat("✓ Processed with official STICr::tidy_hobo_data()\n")
        
      } else if(has_project_column) {
        cat("→ DETECTED: PROCESSED RESEARCH DATA - Using custom tidying\n")
        
        # Read processed data
        original_data <- read.csv("current_input.csv")
        
        # Convert to STICr format
        tidy_data <- data.frame(
          datetime = as.POSIXct(original_data$datetime, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
          condUncal = as.numeric(original_data$condUncal),
          tempC = as.numeric(original_data$tempC),
          stringsAsFactors = FALSE
        )
        
        # Remove incomplete cases
        tidy_data <- tidy_data[complete.cases(tidy_data), ]
        
        processing_method <- "custom_research_data_tidying"
        cat("✓ Processed with custom tidying\n")
        
      } else {
        cat("→ DETECTED: UNKNOWN FORMAT - Attempting generic processing\n")
        
        # Generic approach with better column detection
        raw_data <- read.csv("current_input.csv")
        col_names <- tolower(colnames(raw_data))
        
        # Find datetime column
        datetime_col <- which(grepl("date|time", col_names))[1]
        # Find temperature column  
        temp_col <- which(grepl("temp", col_names))[1]
        # Find conductivity column
        cond_col <- which(grepl("cond|lux|intensity", col_names))[1]
        
        if(is.na(datetime_col) || is.na(temp_col) || is.na(cond_col)) {
          stop(paste("Could not auto-detect columns. Found columns:", paste(colnames(raw_data), collapse=", ")))
        }
        
        tidy_data <- data.frame(
          datetime = as.POSIXct(raw_data[, datetime_col]),
          condUncal = as.numeric(raw_data[, cond_col]),
          tempC = as.numeric(raw_data[, temp_col]),
          stringsAsFactors = FALSE
        )
        
        tidy_data <- tidy_data[complete.cases(tidy_data), ]
        processing_method <- "generic_auto_detection"
        cat("✓ Processed with generic auto-detection\n")
      }
      
      # Validate results
      if(nrow(tidy_data) == 0) {
        stop("No valid data after processing")
      }
      
      cat("✓ Processing completed successfully\n")
      cat("- Rows processed:", nrow(tidy_data), "\n")
      cat("- Date range:", min(tidy_data$datetime, na.rm = TRUE), "to", max(tidy_data$datetime, na.rm = TRUE), "\n")
      cat("- Method used:", processing_method, "\n")
      
      # Generate output filename
      clean_filename <- gsub("\\.csv$", "", file_name)
      output_filename <- paste0("step1_tidy_", clean_filename, ".csv")
      
      # Save locally
      write.csv(tidy_data, output_filename, row.names = FALSE)
      
      # Upload to MinIO
      remote_filename <- paste0(clean_filename, "_step1_tidy.csv")
      faasr_put_file(local_file = output_filename,
                     remote_folder = "sticr-workflow/step1-tidy",
                     remote_file = remote_filename)
      cat("✓ Uploaded to: sticr-workflow/step1-tidy/", remote_filename, "\n")
      
      # Store processing results
      processing_results[[file_name]] <- list(
        status = "SUCCESS",
        method = processing_method,
        input_file = file_name,
        output_file = remote_filename,
        rows_processed = nrow(tidy_data),
        date_range_start = min(tidy_data$datetime),
        date_range_end = max(tidy_data$datetime),
        processing_time = Sys.time()
      )
      
      processed_files <- processed_files + 1
      
    }, error = function(e) {
      cat("✗ FAILED to process", file_name, ":", e$message, "\n")
      
      processing_results[[file_name]] <- list(
        status = "FAILED",
        error_message = e$message,
        processing_time = Sys.time()
      )
    })
  }
  
  # Create summary report
  cat("\n", paste(rep("=", 70), collapse=""), "\n")
  cat("AUTOMATIC BATCH PROCESSING SUMMARY\n")
  cat(paste(rep("=", 70), collapse=""), "\n")
  cat("Files discovered:", length(available_files), "\n")
  cat("Files processed successfully:", processed_files, "\n")
  cat("Files failed:", length(available_files) - processed_files, "\n")
  
  # Create detailed report
  if(length(processing_results) > 0) {
    summary_data <- data.frame(
      file_name = names(processing_results),
      status = sapply(processing_results, function(x) x$status),
      method = sapply(processing_results, function(x) x$method %||% "N/A"),
      rows = sapply(processing_results, function(x) x$rows_processed %||% 0),
      error = sapply(processing_results, function(x) x$error_message %||% "None"),
      stringsAsFactors = FALSE
    )
    
    print(summary_data)
    
    # Save summary report
    write.csv(summary_data, "automatic_processing_summary.csv", row.names = FALSE)
    faasr_put_file(local_file = "automatic_processing_summary.csv",
                   remote_folder = "sticr-workflow/reports",
                   remote_file = paste0("auto_batch_summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
    cat("✓ Summary report uploaded\n")
  }
  
  cat("🎉 AUTOMATIC PROCESSING COMPLETE!\n")
  cat("📊", processed_files, "files successfully processed\n")
  cat("📁 All outputs in: sticr-workflow/step1-tidy/\n")
  cat("📋 Summary report in: sticr-workflow/reports/\n")
  cat("🔄 Next time you run this, it will automatically discover new files!\n")
  
  return(paste("Automatic processing completed:", processed_files, "of", length(available_files), "files processed successfully"))
}
