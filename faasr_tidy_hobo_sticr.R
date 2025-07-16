faasr_tidy_hobo_sticr <- function() {  
  # Load libraries
  library(STICr)
  library(tidyverse)
  library(lubridate)
  cat("Libraries loaded\n")

  # HydroShare STIC dataset discovery - based on actual South Fork Kings Creek data
  cat("Discovering STIC files from HydroShare dataset...\n")
  
  # Based on the HydroShare resource: South Fork Kings Creek STIC data
  # Focus on the actual naming patterns from that dataset
  potential_files <- c()
  
  # 1. South Fork Kings Creek STIC patterns (from HydroShare)
  # Sites observed in the dataset: 02M10, 04SW3, 20M01, 04W03, 04W04, etc.
  stic_sites <- c(
    "02M10", "04SW3", "20M01", "04W03", "04W04", "01M05", "03SW2", 
    "05M15", "06W01", "07M20", "08SW1", "09W02", "10M01", "11SW4",
    "12M10", "13W03", "14SW2", "15M05", "16W04", "17M15", "18SW1", "19W05", "20SW3"
  )
  
  stic_types <- c("LS", "HS", "SP")
  stic_years <- c("2021", "2022", "2023", "2024")  # Years from the dataset
  
  # Generate STIC_GP_KNZ patterns
  for(site in stic_sites) {
    for(type in stic_types) {
      for(year in stic_years) {
        potential_files <- c(potential_files,
                            paste0("STIC_GP_KNZ_", site, "_", type, "_", year, ".csv"))
      }
    }
  }
  
  # 2. HydroShare raw data files
  hydroshare_files <- c(
    "raw_hobo_data.csv",
    "stic_data.csv",
    "STIC_data.csv", 
    "south_fork_kings_creek.csv",
    "konza_prairie_stic.csv"
  )
  potential_files <- c(potential_files, hydroshare_files)
  
  # 3. Generic data files (for other uploads)
  generic_files <- c(
    "data.csv", "raw_data.csv", "export.csv",
    "file1.csv", "file2.csv", "file3.csv"
  )
  potential_files <- c(potential_files, generic_files)
  
  # Remove duplicates
  potential_files <- unique(potential_files)
  cat("Checking", length(potential_files), "HydroShare STIC patterns...\n")
    
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
