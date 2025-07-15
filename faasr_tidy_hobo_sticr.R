faasr_tidy_hobo_sticr <- function() {
  faasr_log("=== STEP 4: STICR PROCESSING WITH LOGS ===")
  
  # Step 1: Load libraries
  tryCatch({
    faasr_log("Step 1: Loading libraries...")
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
    faasr_get_file(remote_folder = "stic-data", 
                   remote_file = "STIC_GP_KNZ_02M10_LS_2022.csv", 
                   local_file = "input_data.csv")
    
    if (!file.exists("input_data.csv")) {
      stop("File was not downloaded")
    }
    
    file_info <- file.info("input_data.csv")
    faasr_log(paste("✓ Step 2: File downloaded, size:", file_info$size, "bytes"))
  }, error = function(e) {
    faasr_log(paste("✗ Step 2 FAILED:", e$message))
    stop(e)
  })
  
  # Step 3: Verify file content
  tryCatch({
    faasr_log("Step 3: Verifying file content...")
    
    # Just check if we can read the file header
    file_preview <- readLines("input_data.csv", n = 1)
    faasr_log(paste("File header:", file_preview))
    faasr_log("✓ Step 3: File content verified")
  }, error = function(e) {
    faasr_log(paste("✗ Step 3 FAILED:", e$message))
    stop(e)
  })
  
  # Step 4: STICr processing
  tryCatch({
    faasr_log("Step 4: Running STICr tidy_hobo_data...")
    
    tidy_data <- tidy_hobo_data(infile = "input_data.csv", outfile = FALSE)
    
    if (is.null(tidy_data)) {
      stop("STICr returned NULL")
    }
    
    if (nrow(tidy_data) == 0) {
      stop("STICr returned empty dataset")
    }
    
    faasr_log(paste("✓ Step 4: STICr processing successful!"))
    faasr_log(paste("Processed rows:", nrow(tidy_data)))
    faasr_log(paste("Output columns:", paste(colnames(tidy_data), collapse = ", ")))
    
  }, error = function(e) {
    faasr_log(paste("✗ Step 4 FAILED:", e$message))
    faasr_log("STICr processing failed - possible causes:")
    faasr_log("- Input data format incompatible with STICr")
    faasr_log("- Missing required columns")
    faasr_log("- Date/time format issues")
    stop(e)
  })
  
  faasr_log("🎉 All steps completed successfully!")
  return("STICr processing completed - ready for upload step")
}
