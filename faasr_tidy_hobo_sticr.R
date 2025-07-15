faasr_tidy_hobo_sticr <- function() {
  cat("=== SMART STEP 1: AUTO-DETECT DATA TYPE & APPLY APPROPRIATE TIDYING ===\n")
  
  # Load libraries
  library(STICr)
  library(tidyverse)
  library(lubridate)
  cat("✓ Libraries loaded\n")
  
  # Download file (you can change this to either file)
  file_to_process <- "raw_hobo_data.csv"  # Change to "STIC_GP_KNZ_04SW3_SP_2024.csv" for processed data
  
  faasr_get_file(remote_folder = "stic-data", 
                 remote_file = file_to_process, 
                 local_file = "input_data.csv")
  cat("✓ File downloaded:", file_to_process, "\n")
  
  tryCatch({
    cat("Step 1: Auto-detecting data type...\n")
    
    # Read first few lines to detect format
    first_lines <- readLines("input_data.csv", n = 10)
    cat("First few lines of file:\n")
    for(i in 1:min(5, length(first_lines))) {
      cat("Line", i, ":", first_lines[i], "\n")
    }
    
    # Detection logic
    is_raw_hobo <- any(grepl("#|Plot Title|LGR S/N|Temp.*°C.*LGR|Series:|HOBOware", first_lines[1:5], ignore.case = TRUE))
    has_project_column <- any(grepl("project.*datetime.*siteId", first_lines[1:3], ignore.case = TRUE))
    
    if(is_raw_hobo) {
      cat("→ DETECTED: RAW HOBO DATA - Using STICr::tidy_hobo_data()\n")
      
      # Use official STICr function for raw HOBO data
      tidy_data <- tidy_hobo_data(infile = "input_data.csv", outfile = FALSE)
      
      if(is.null(tidy_data) || nrow(tidy_data) == 0) {
        stop("STICr::tidy_hobo_data() returned empty result")
      }
      
      cat("✓ Raw HOBO data processed with official STICr function\n")
      data_source <- "raw_hobo_official_sticr"
      
    } else if(has_project_column) {
      cat("→ DETECTED: PROCESSED RESEARCH DATA - Using custom tidying\n")
      
      # Read processed research data
      original_data <- read.csv("input_data.csv")
      cat("Original columns:", paste(colnames(original_data), collapse = ", "), "\n")
      
      # Convert to STICr format
      tidy_data <- data.frame(
        datetime = as.POSIXct(original_data$datetime, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
        condUncal = as.numeric(original_data$condUncal),
        tempC = as.numeric(original_data$tempC),
        stringsAsFactors = FALSE
      )
      
      # Remove incomplete cases
      tidy_data <- tidy_data[complete.cases(tidy_data), ]
      
      cat("✓ Processed research data converted to STICr format\n")
      data_source <- "processed_research_custom"
      
    } else {
      cat("→ DETECTED: UNKNOWN FORMAT - Attempting generic CSV read\n")
      
      # Try generic approach
      raw_data <- read.csv("input_data.csv")
      cat("Columns found:", paste(colnames(raw_data), collapse = ", "), "\n")
      
      # Try to find datetime, temperature, and conductivity columns
      datetime_col <- colnames(raw_data)[grepl("date|time", colnames(raw_data), ignore.case = TRUE)][1]
      temp_col <- colnames(raw_data)[grepl("temp", colnames(raw_data), ignore.case = TRUE)][1]
      cond_col <- colnames(raw_data)[grepl("cond|lux|intensity", colnames(raw_data), ignore.case = TRUE)][1]
      
      if(is.na(datetime_col) || is.na(temp_col) || is.na(cond_col)) {
        stop("Could not identify required columns (datetime, temperature, conductivity)")
      }
      
      tidy_data <- data.frame(
        datetime = as.POSIXct(raw_data[[datetime_col]]),
        condUncal = as.numeric(raw_data[[cond_col]]),
        tempC = as.numeric(raw_data[[temp_col]]),
        stringsAsFactors = FALSE
      )
      
      tidy_data <- tidy_data[complete.cases(tidy_data), ]
      data_source <- "generic_auto_detect"
    }
    
    # Validate results
    cat("✓ Data processing completed\n")
    cat("Final tidy data:\n")
    cat("- Columns:", paste(colnames(tidy_data), collapse = ", "), "\n")
    cat("- Rows:", nrow(tidy_data), "\n")
    cat("- Date range:", min(tidy_data$datetime, na.rm = TRUE), "to", max(tidy_data$datetime, na.rm = TRUE), "\n")
    cat("- condUncal range:", min(tidy_data$condUncal, na.rm = TRUE), "to", max(tidy_data$condUncal, na.rm = TRUE), "\n")
    cat("- tempC range:", min(tidy_data$tempC, na.rm = TRUE), "to", max(tidy_data$tempC, na.rm = TRUE), "\n")
    
    # Show sample
    cat("Sample of tidied data:\n")
    print(head(tidy_data, 3))
    
    # Save results with source info
    output_filename <- paste0("step1_tidy_", data_source, ".csv")
    write.csv(tidy_data, output_filename, row.names = FALSE)
    cat("✓ Step 1 output saved as:", output_filename, "\n")
    
    # Upload to MinIO
    remote_filename <- paste0("STIC_", gsub(".csv", "", file_to_process), "_step1_tidy.csv")
    faasr_put_file(local_file = output_filename,
                   remote_folder = "sticr-workflow/step1-tidy",
                   remote_file = remote_filename)
    cat("✓ Uploaded to MinIO as:", remote_filename, "\n")
    
    # Also save processing metadata
    metadata <- data.frame(
      source_file = file_to_process,
      data_type = data_source,
      processing_method = ifelse(is_raw_hobo, "STICr::tidy_hobo_data()", "custom_tidying"),
      output_rows = nrow(tidy_data),
      date_range_start = min(tidy_data$datetime),
      date_range_end = max(tidy_data$datetime),
      processed_timestamp = Sys.time()
    )
    
    write.csv(metadata, "step1_processing_metadata.csv", row.names = FALSE)
    faasr_put_file(local_file = "step1_processing_metadata.csv",
                   remote_folder = "sticr-workflow/metadata",
                   remote_file = paste0("step1_metadata_", gsub(".csv", "", file_to_process), ".csv"))
    cat("✓ Processing metadata saved\n")
    
  }, error = function(e) {
    cat("✗ Step 1 FAILED:", e$message, "\n")
    stop(e)
  })
  
  cat("🎉 SMART STEP 1 COMPLETE!\n")
  cat("📁 Data source:", data_source, "\n")
  cat("📊 Output:", nrow(tidy_data), "rows in STICr format\n")
  cat("📤 Uploaded to: sticr-workflow/step1-tidy/\n")
  cat("🔜 Ready for Step 2: Calibration\n")
  
  return(paste("Step 1 completed:", data_source, "with", nrow(tidy_data), "rows"))
}
