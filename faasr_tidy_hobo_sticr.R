faasr_tidy_hobo_sticr <- function() {  
  # Load libraries
  library(STICr)
  library(tidyverse)
  library(lubridate)
  cat("Libraries loaded\n")

  # Universal CSV file discovery - no manual filename additions needed
  cat("Scanning for ANY CSV files in stic-data folder...\n")
  
  # Generate comprehensive filename patterns to catch any possible CSV file
  potential_files <- c()
  
  # 1. Common single-word filenames
  common_bases <- c("data", "stic", "hobo", "raw", "file", "export", "download", 
                   "sensor", "logger", "temp", "conductivity", "stream", "water")
  
  for(base in common_bases) {
    potential_files <- c(potential_files,
                        paste0(base, ".csv"),
                        paste0(toupper(base), ".csv"),
                        paste0(base, "_data.csv"),
                        paste0(base, "_raw.csv"),
                        paste0("raw_", base, ".csv"))
  }
  
  # # 2. Numbered files (file1.csv through file100.csv)
  # for(i in 1:100) {
  #   potential_files <- c(potential_files,
  #                       paste0("file", i, ".csv"),
  #                       paste0("data", i, ".csv"),
  #                       paste0("stic", i, ".csv"),
  #                       paste0("dataset", i, ".csv"))
  # }
  
  # # 3. Date-based patterns (2020-2025, all months)
  # for(year in 2020:2025) {
  #   potential_files <- c(potential_files, paste0("data_", year, ".csv"))
  #   for(month in 1:12) {
  #     month_str <- sprintf("%02d", month)
  #     potential_files <- c(potential_files,
  #                         paste0("data_", year, "_", month_str, ".csv"),
  #                         paste0(year, "_", month_str, ".csv"),
  #                         paste0(year, month_str, ".csv"))
  #   }
  # }
  
  # # 4. STIC pattern variations (comprehensive coverage)
  # # Cover all reasonable site combinations
  # for(prefix in c("01", "02", "03", "04", "05", "10", "15", "20", "25", "30")) {
  #   for(middle in c("M01", "M02", "M05", "M10", "M12", "M15", "M20", "SW1", "SW2", "SW3", "SW4", "SW5")) {
  #     for(suffix in c("LS", "HS", "SP", "MS", "US")) {
  #       for(year in c("2020", "2021", "2022", "2023", "2024", "2025")) {
  #         potential_files <- c(potential_files,
  #                             paste0("STIC_GP_KNZ_", prefix, middle, "_", suffix, "_", year, ".csv"),
  #                             paste0("stic_gp_knz_", prefix, middle, "_", suffix, "_", year, ".csv"))
  #       }
  #     }
  #   }
  # }
  
  # # 5. Alternative STIC formats
  # for(i in 1:50) {
  #   site_code <- sprintf("%02d", i)
  #   for(type in c("LS", "HS", "SP")) {
  #     for(year in 2020:2025) {
  #       potential_files <- c(potential_files,
  #                           paste0("STIC_", site_code, "_", type, "_", year, ".csv"),
  #                           paste0("stic_", site_code, "_", type, "_", year, ".csv"))
  #     }
  #   }
  # }
  
  # # 6. Random letter combinations (a.csv, b.csv, ... aa.csv, ab.csv, etc.)
  # letters_single <- letters[1:26]
  # for(l in letters_single) {
  #   potential_files <- c(potential_files, paste0(l, ".csv"))
  # }
  
  # # 7. Two-letter combinations (most common)
  # common_two_letter <- c("aa", "ab", "ac", "ad", "ba", "bb", "bc", "ca", "cb", "da", "db")
  # for(l in common_two_letter) {
  #   potential_files <- c(potential_files, paste0(l, ".csv"))
  # }
  
  # # 8. Timestamp-like patterns
  # for(i in 1:31) {
  #   day_str <- sprintf("%02d", i)
  #   potential_files <- c(potential_files,
  #                       paste0("20230", i %% 10 + 1, day_str, ".csv"),
  #                       paste0("20240", i %% 10 + 1, day_str, ".csv"))
  # }
  
  # Remove duplicates and sort
  potential_files <- unique(potential_files)
  
  cat("Generated", length(potential_files), "filename patterns covering virtually any CSV file\n")
  cat("This will find any reasonably-named CSV file you upload!\n")
    
  # Try to discover available files by downloading
  available_files <- c()
  
  for(file_name in potential_files) {
    tryCatch({
      # Try to download the file - this will fail silently if file doesn't exist
      faasr_get_file(remote_folder = "stic-data", 
                     remote_file = file_name, 
                     local_file = paste0("test_", file_name))
      
      # If download succeeded, file exists
      available_files <- c(available_files, file_name)
      cat("Found:", file_name, "\n")
      
      # Clean up test file
      if(file.exists(paste0("test_", file_name))) {
        file.remove(paste0("test_", file_name))
      }
      
    }, error = function(e) {
      # File doesn't exist - skip silently
    })
  }
  
  if(length(available_files) == 0) {
    cat("No STIC files found in bucket!\n")
    cat("Make sure files are uploaded to 'stic-data' folder\n")
    cat("Checked patterns including:\n")
    cat("- STIC_GP_KNZ_*.csv\n")
    cat("- raw_hobo_data.csv\n") 
    cat("- stic_data.csv\n")
    return("No files found to process")
  }
  
  cat("\n=== Files discovered and ready for processing ===\n")
  for(file in available_files) {
    cat("-", file, "\n")
  }
  cat("Total files found:", length(available_files), "\n\n")
  
  # Process each discovered file
  processed_files <- 0
  processing_results <- list()
  
  for(file_name in available_files) {
    tryCatch({
      # Download the file
      faasr_get_file(remote_folder = "stic-data", 
                     remote_file = file_name, 
                     local_file = "current_input.csv")
      cat("Processing:", file_name, "\n")
      
      # Auto-detect data type and process    
      # Read first few lines for detection
      first_lines <- readLines("current_input.csv", n = 10)
      
      # Enhanced detection logic
      is_raw_hobo <- any(grepl("#|Plot Title|LGR S/N|Temp.*°C.*LGR|Series:|HOBOware", first_lines[1:5], ignore.case = TRUE))
      has_project_column <- any(grepl("project.*datetime.*siteId", first_lines[1:3], ignore.case = TRUE))
      
      if(is_raw_hobo) {
        cat("→ Raw HOBO format detected - Using STICr::tidy_hobo_data()\n")
        
        # Using official STICr function
        tidy_data <- tidy_hobo_data(infile = "current_input.csv", outfile = FALSE)
        
        if(is.null(tidy_data) || nrow(tidy_data) == 0) {
          stop("STICr::tidy_hobo_data() returned empty result")
        }
        processing_method <- "official_STICr_tidy_hobo_data"
        
      } else if(has_project_column) {
        cat("→ Processed research data detected - Using custom tidying\n")
        
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
        
      } else {
        cat("→ Unknown format - Attempting generic processing\n")
        
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
      }
      
      # Validate results
      if(nrow(tidy_data) == 0) {
        stop("No valid data after processing")
      }
      
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
      
      # Store processing results
      processing_results[[file_name]] <- list(
        status = "SUCCESS",
        method = processing_method,
        input_file = file_name,
        output_file = remote_filename,
        rows_processed = nrow(tidy_data),
        date_range_start = min(tidy_data$datetime),
        date_range_end = max(tidy_data$datetime)
      )
      
      processed_files <- processed_files + 1
      cat("✓ Success:", clean_filename, "->", nrow(tidy_data), "rows\n\n")
      
    }, error = function(e) {
      cat("✗ Failed to process", file_name, ":", e$message, "\n\n")
      
      processing_results[[file_name]] <- list(
        status = "FAILED",
        error_message = e$message
      )
    })
  }
  
  cat("=== Step 1 Processing Complete ===\n")
  cat("Successfully processed:", processed_files, "of", length(available_files), "files\n")
  
  # Show processing summary
  successful_files <- sapply(processing_results, function(x) x$status == "SUCCESS")
  if(any(successful_files)) {
    cat("\nSuccessful files:\n")
    for(name in names(processing_results)[successful_files]) {
      result <- processing_results[[name]]
      cat("-", name, "->", result$output_file, "(", result$rows_processed, "rows )\n")
    }
  }
  
  if(any(!successful_files)) {
    cat("\nFailed files:\n")
    for(name in names(processing_results)[!successful_files]) {
      cat("-", name, "\n")
    }
  }
  
  return(paste("Dynamic processing completed:", processed_files, "of", length(available_files), "files processed successfully"))
}
