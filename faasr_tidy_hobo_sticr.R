faasr_tidy_hobo_sticr <- function() {
  faasr_log("=== STEP 4: STICR PROCESSING TEST ===")
  
  # Step 1: Load libraries
  tryCatch({
    faasr_log("Loading libraries...")
    library(STICr)
    library(tidyverse)
    library(lubridate)
    faasr_log(paste("STICr version:", as.character(packageVersion("STICr"))))
    faasr_log("✓ Step 1: Libraries loaded successfully")
  }, error = function(e) {
    faasr_log(paste("✗ Step 1 FAILED:", e$message))
    stop(e)
  })
  
  # Step 2: Download file
  tryCatch({
    faasr_log("Step 2: Downloading file...")
    input_file <- "STIC_GP_KNZ_02M10_LS_2022.csv"
    
    faasr_get_file(remote_folder = "stic-data", 
                   remote_file = input_file, 
                   local_file = "input_data.csv")
    
    file_info <- file.info("input_data.csv")
    faasr_log(paste("✓ Step 2: File downloaded, size:", file_info$size, "bytes"))
  }, error = function(e) {
    faasr_log(paste("✗ Step 2 FAILED:", e$message))
    stop(e)
  })
  
  # Step 3: Examine input data format
  tryCatch({
    faasr_log("Step 3: Examining input data...")
    
    # Read first few lines to understand structure
    input_data <- read.csv("input_data.csv", nrows = 10)
    faasr_log(paste("Input columns:", paste(colnames(input_data), collapse = ", ")))
    faasr_log(paste("Total rows:", nrow(read.csv("input_data.csv"))))
    faasr_log(paste("Sample first row:", toString(input_data[1,1:min(3, ncol(input_data))])))
    
    faasr_log("✓ Step 3: Input data examined")
  }, error = function(e) {
    faasr_log(paste("✗ Step 3 FAILED:", e$message))
    stop(e)
  })
  
  # Step 4: Check STICr function availability
  tryCatch({
    faasr_log("Step 4: Checking STICr functions...")
    
    # List available STICr functions
    sticr_functions <- ls("package:STICr")
    faasr_log(paste("Available STICr functions:", paste(sticr_functions, collapse = ", ")))
    
    # Check for tidy_hobo_data specifically
    if (!"tidy_hobo_data" %in% sticr_functions) {
      stop("tidy_hobo_data function not found in STICr package")
    }
    
    faasr_log("✓ Step 4: tidy_hobo_data function confirmed available")
  }, error = function(e) {
    faasr_log(paste("✗ Step 4 FAILED:", e$message))
    stop(e)
  })
  
  # Step 5: Process with STICr
  tryCatch({
    faasr_log("Step 5: Running STICr tidy_hobo_data...")
    
    # Try the STICr processing
    tidy_data <- tidy_hobo_data(infile = "input_data.csv", outfile = FALSE)
    
    # Check results
    if (is.null(tidy_data)) {
      stop("STICr returned NULL - check input data format")
    }
    
    if (nrow(tidy_data) == 0) {
      stop("STICr returned empty dataset")
    }
    
    faasr_log(paste("✓ Step 5: STICr processing successful!"))
    faasr_log(paste("Processed rows:", nrow(tidy_data)))
    faasr_log(paste("Output columns:", paste(colnames(tidy_data), collapse = ", ")))
    
    # Show a sample of the processed data
    if (nrow(tidy_data) > 0) {
      faasr_log(paste("Sample processed data:", toString(tidy_data[1,1:min(3, ncol(tidy_data))])))
    }
    
  }, error = function(e) {
    faasr_log(paste("✗ Step 5 FAILED:", e$message))
    faasr_log("STICr processing error - this might be due to:")
    faasr_log("1. Input data format not matching STICr expectations")
    faasr_log("2. Missing required columns")
    faasr_log("3. Date/time format issues")
    stop(e)
  })
  
  # Step 6: Save and upload results
  tryCatch({
    faasr_log("Step 6: Saving and uploading results...")
    
    output_file <- "STIC_GP_KNZ_02M10_LS_2022_sticr_tidy.csv"
    
    # Save processed data
    write.csv(tidy_data, "tidy_output.csv", row.names = FALSE)
    
    # Verify file was created
    if (!file.exists("tidy_output.csv")) {
      stop("Output file was not created")
    }
    
    output_info <- file.info("tidy_output.csv")
    faasr_log(paste("Output file created, size:", output_info$size, "bytes"))
    
    # Upload to MinIO
    faasr_put_file(local_file = "tidy_output.csv",
                   remote_folder = "stic-processed/tidy",
                   remote_file = output_file)
    
    faasr_log("✓ Step 6: Results uploaded successfully")
    faasr_log(paste("✓ Output uploaded to: stic-processed/tidy/", output_file))
    
  }, error = function(e) {
    faasr_log(paste("✗ Step 6 FAILED:", e$message))
    stop(e)
  })
  
  faasr_log("🎉 COMPLETE SUCCESS: STICr workflow finished!")
  faasr_log("✓ Data downloaded, processed with STICr, and uploaded")
  
  return("STICr tidy processing completed successfully")
}
