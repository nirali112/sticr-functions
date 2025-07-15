faasr_tidy_hobo_sticr <- function() {
  faasr_log("=== STEP 3: FILE DOWNLOAD TEST ===")
  
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
  
  # Step 2: Test file download
  tryCatch({
    faasr_log("Step 2: Testing file download...")
    
    input_file <- "STIC_GP_KNZ_02M10_LS_2022.csv"
    faasr_log(paste("Attempting to download:", input_file, "from stic-data folder"))
    
    # Download file from MinIO
    faasr_get_file(remote_folder = "stic-data", 
                   remote_file = input_file, 
                   local_file = "input_data.csv")
    
    # Check if file was downloaded
    if (!file.exists("input_data.csv")) {
      stop("input_data.csv was not created after download")
    }
    
    # Get file info
    file_info <- file.info("input_data.csv")
    faasr_log(paste("✓ Step 2: File downloaded successfully"))
    faasr_log(paste("File size:", file_info$size, "bytes"))
    
    # Read first few lines to verify content
    tryCatch({
      input_preview <- read.csv("input_data.csv", nrows = 3)
      faasr_log(paste("File columns:", paste(colnames(input_preview), collapse = ", ")))
      faasr_log(paste("Preview rows:", nrow(input_preview)))
      faasr_log("✓ File content verified")
    }, error = function(e) {
      faasr_log(paste("Warning: Could not read file content:", e$message))
    })
    
  }, error = function(e) {
    faasr_log(paste("✗ Step 2 FAILED:", e$message))
    
    # Debug info
    faasr_log("Debugging file download issue...")
    tryCatch({
      # List current directory
      files <- list.files(".", full.names = TRUE)
      faasr_log(paste("Current directory files:", paste(files, collapse = ", ")))
    }, error = function(e2) {
      faasr_log("Could not list directory files")
    })
    
    stop(e)
  })
  
  faasr_log("✓ Step 3 completed - file download successful")
  return("Step 3 completed - libraries loaded and file downloaded")
}
