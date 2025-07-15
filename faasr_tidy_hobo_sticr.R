faasr_tidy_hobo_sticr <- function() {
  cat("=== STEP 4: STICR PROCESSING ===\n")
  
  # Step 1: Load libraries
  tryCatch({
    cat("Step 1: Loading libraries...\n")
    library(STICr)
    library(tidyverse)
    library(lubridate)
    cat("✓ Step 1: Libraries loaded successfully\n")
  }, error = function(e) {
    cat("✗ Step 1 FAILED:", e$message, "\n")
    stop(e)
  })
  
  # Step 2: Download file
  tryCatch({
    cat("Step 2: Downloading file...\n")
    faasr_get_file(remote_folder = "stic-data", 
                   remote_file = "STIC_GP_KNZ_02M10_LS_2022.csv", 
                   local_file = "input_data.csv")
    
    if (!file.exists("input_data.csv")) {
      stop("File was not downloaded")
    }
    
    file_info <- file.info("input_data.csv")
    cat("✓ Step 2: File downloaded, size:", file_info$size, "bytes\n")
  }, error = function(e) {
    cat("✗ Step 2 FAILED:", e$message, "\n")
    stop(e)
  })
  
  # Step 3: Verify file content
  tryCatch({
    cat("Step 3: Verifying file content...\n")
    file_preview <- readLines("input_data.csv", n = 1)
    cat("File header:", file_preview, "\n")
    cat("✓ Step 3: File content verified\n")
  }, error = function(e) {
    cat("✗ Step 3 FAILED:", e$message, "\n")
    stop(e)
  })
  
  # Step 4: STICr processing
  tryCatch({
    cat("Step 4: Running STICr tidy_hobo_data...\n")
    
    tidy_data <- tidy_hobo_data(infile = "input_data.csv", outfile = FALSE)
    
    if (is.null(tidy_data)) {
      stop("STICr returned NULL")
    }
    
    if (nrow(tidy_data) == 0) {
      stop("STICr returned empty dataset")
    }
    
    cat("✓ Step 4: STICr processing successful!\n")
    cat("Processed rows:", nrow(tidy_data), "\n")
    cat("Output columns:", paste(colnames(tidy_data), collapse = ", "), "\n")
    
  }, error = function(e) {
    cat("✗ Step 4 FAILED:", e$message, "\n")
    stop(e)
  })
  
  cat("🎉 All steps completed successfully!\n")
  return("STICr processing completed")
}
