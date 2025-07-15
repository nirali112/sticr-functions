faasr_tidy_hobo_sticr <- function() {
  cat("=== STEP 1: STICR WORKFLOW - TIDYING HOBO DATA ===\n")
  
  # Load libraries
  library(STICr)
  library(tidyverse)
  library(lubridate)
  cat("✓ Libraries loaded\n")
  
  # Download file
  faasr_get_file(remote_folder = "stic-data", 
                 remote_file = "STIC_GP_KNZ_02M10_LS_2022.csv", 
                 local_file = "input_data.csv")
  cat("✓ File downloaded\n")
  
  # Step 1: Detect data type and apply appropriate tidying
  tryCatch({
    cat("Step 1: Detecting data type and tidying...\n")
    
    # Read first few lines to detect data format
    first_lines <- readLines("input_data.csv", n = 10)
    cat("First few lines of file:\n")
    for(i in 1:min(5, length(first_lines))) {
      cat("Line", i, ":", first_lines[i], "\n")
    }
    
    # Check if this is raw HOBO data or already processed
    is_raw_hobo <- any(grepl("Plot Title|LGR S/N|Temp.*°C.*LGR|HOBOware", first_lines[1:5], ignore.case = TRUE))
    
    if(is_raw_hobo) {
      cat("→ Detected: RAW HOBO data - applying tidy_hobo_data()\n")
      
      # Use STICr's tidy_hobo_data function for raw HOBO files
      tidy_data <- tidy_hobo_data(infile = "input_data.csv", outfile = FALSE)
      
      if(is.null(tidy_data) || nrow(tidy_data) == 0) {
        stop("tidy_hobo_data() returned empty result")
      }
      
      cat("✓ Raw HOBO data tidied successfully\n")
      cat("Tidied columns:", paste(colnames(tidy_data), collapse = ", "), "\n")
      cat("Tidied rows:", nrow(tidy_data), "\n")
      
    } else {
      cat("→ Detected: Already processed STIC data - applying custom tidying\n")
      
      # Read the already-processed data
      raw_data <- read.csv("input_data.csv")
      
      # Create tidy format matching STICr's tidy_hobo_data output
      # STICr's tidy_hobo_data produces: datetime, condUncal, tempC
      if(all(c("datetime", "condUncal", "tempC") %in% colnames(raw_data))) {
        # Data already has the right columns
        tidy_data <- raw_data[, c("datetime", "condUncal", "tempC")]
      } else {
        # Try to map columns to STICr format
        tidy_data <- data.frame(
          datetime = raw_data$datetime,
          condUncal = raw_data$condUncal,
          tempC = raw_data$tempC
        )
      }
      
      # Convert datetime to proper format if needed
      if(!inherits(tidy_data$datetime, "POSIXct")) {
        tidy_data$datetime <- as.POSIXct(tidy_data$datetime, 
                                        format = "%Y-%m-%dT%H:%M:%S", 
                                        tz = "UTC")
      }
      
      cat("✓ Already-processed data standardized to STICr format\n")
      cat("Standardized columns:", paste(colnames(tidy_data), collapse = ", "), "\n")
      cat("Standardized rows:", nrow(tidy_data), "\n")
    }
    
    # Validate the tidied data
    cat("Validating tidied data...\n")
    
    # Check required columns
    required_cols <- c("datetime", "condUncal", "tempC")
    missing_cols <- required_cols[!required_cols %in% colnames(tidy_data)]
    if(length(missing_cols) > 0) {
      stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
    }
    
    # Check data ranges
    cat("Data validation:\n")
    cat("- Date range:", min(tidy_data$datetime, na.rm = TRUE), "to", max(tidy_data$datetime, na.rm = TRUE), "\n")
    cat("- condUncal range:", min(tidy_data$condUncal, na.rm = TRUE), "to", max(tidy_data$condUncal, na.rm = TRUE), "\n")
    cat("- tempC range:", min(tidy_data$tempC, na.rm = TRUE), "to", max(tidy_data$tempC, na.rm = TRUE), "\n")
    cat("- NA values: condUncal =", sum(is.na(tidy_data$condUncal)), 
        ", tempC =", sum(is.na(tidy_data$tempC)), "\n")
    
    # Save Step 1 output
    write.csv(tidy_data, "step1_tidy_data.csv", row.names = FALSE)
    cat("✓ Step 1 tidy data saved locally\n")
    
    # Upload Step 1 results
    faasr_put_file(local_file = "step1_tidy_data.csv",
                   remote_folder = "stic-processed/step1-tidy",
                   remote_file = "STIC_GP_KNZ_02M10_LS_2022_step1_tidy.csv")
    cat("✓ Step 1 results uploaded to MinIO\n")
    
    # Show sample of tidied data
    cat("Sample of tidied data:\n")
    print(head(tidy_data, 3))
    
  }, error = function(e) {
    cat("✗ Step 1 FAILED:", e$message, "\n")
    stop(e)
  })
  
  cat("🎉 Step 1 COMPLETE: Data successfully tidied using STICr workflow!\n")
  cat("📁 Output: step1_tidy_data.csv (datetime, condUncal, tempC)\n")
  cat("📤 Uploaded to: stic-processed/step1-tidy/\n")
  cat("🔜 Ready for Step 2: Calibration\n")
  
  return("Step 1: Tidying completed successfully")
}
