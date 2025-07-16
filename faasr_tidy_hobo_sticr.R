faasr_tidy_hobo_sticr <- function() {  
  # Load libraries
  library(STICr)
  library(tidyverse)
  library(lubridate)
  cat("Libraries loaded\n")

  # Get all files from stic-data folder dynamically
  all_files <- faasr_list_files(remote_folder = "stic-data")
  # Filter for CSV files only
  potential_files <- all_files[grepl("\\.csv$", all_files, ignore.case = TRUE)]
    
  # available files by downloads
  available_files <- c()
  
  for(file_name in potential_files) {
    tryCatch({
      # Try to download the file this will fail silently if file doesn't exist
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
      # File doesn't exist - skip
    })
  }
  
  if(length(available_files) == 0) {
    cat("No STIC files found in bucket!\n")
    cat("Make sure files are uploaded to 'stic-data' folder\n")
    return("No files found to process")
  }
  for(file in available_files) {
    cat("-", file, "\n")
  }
  
  # Process each discovered file
  processed_files <- 0
  processing_results <- list()
  
  for(file_name in available_files) {
    tryCatch({
      # Download the file
      faasr_get_file(remote_folder = "stic-data", 
                     remote_file = file_name, 
                     local_file = "current_input.csv")
      cat("Downloaded:", file_name, "\n")
      
      # Auto-detect data type and process    
      # Read first few lines for detection
      first_lines <- readLines("current_input.csv", n = 10)
      
      # Enhanced detection logic
      is_raw_hobo <- any(grepl("#|Plot Title|LGR S/N|Temp.*°C.*LGR|Series:|HOBOware", first_lines[1:5], ignore.case = TRUE))
      has_project_column <- any(grepl("project.*datetime.*siteId", first_lines[1:3], ignore.case = TRUE))
      
      if(is_raw_hobo) {
        # YES - Using STICr function here
        tidy_data <- tidy_hobo_data(infile = "current_input.csv", outfile = FALSE)
        
        if(is.null(tidy_data) || nrow(tidy_data) == 0) {
          stop("STICr::tidy_hobo_data() returned empty result")
        }
        processing_method <- "official_STICr_tidy_hobo_data"
      } else if(has_project_column) {
        cat("DETECTED: PROCESSED RESEARCH DATA - Using custom tidying\n")
        
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
        cat("Processed with custom tidying\n")
        
      } else {
        cat("DETECTED: UNKNOWN FORMAT - Attempting generic processing\n")
        
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
        cat("Processed with generic auto-detection\n")
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
        date_range_end = max(tidy_data$datetime),
        processing_time = Sys.time()
      )
      
      processed_files <- processed_files + 1
      
    }, error = function(e) {
      cat("FAILED to process", file_name, ":", e$message, "\n")
      
      processing_results[[file_name]] <- list(
        status = "FAILED",
        error_message = e$message,
        processing_time = Sys.time()
      )
    })
  }
  return(paste("Automatic processing completed:", processed_files, "of", length(available_files), "files processed successfully"))
}
