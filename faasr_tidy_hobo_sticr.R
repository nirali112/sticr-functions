faasr_tidy_hobo_sticr <- function() {
  faasr_log("=== STEP 4: STICR PROCESSING (FIXED) ===")
  
  # Step 1: Load libraries
  tryCatch({
    faasr_log("Step 1: Loading libraries...")
    library(STICr)
    library(tidyverse)
    library(lubridate)
    faasr_log("✓ Step 1: Libraries loaded")
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
    faasr_log("✓ Step 2: File downloaded")
  }, error = function(e) {
    faasr_log(paste("✗ Step 2 FAILED:", e$message))
    stop(e)
  })
  
  # Step 3: Basic file check (memory-safe)
  tryCatch({
    faasr_log("Step 3: Checking file...")
    if (!file.exists("input_data.csv")) {
      stop("File does not exist")
    }
    file_info <- file.info("input_data.csv")
    faasr_log(paste("File size:", file_info$size, "bytes"))
    faasr_log("✓ Step 3: File verified")
  }, error = function(e) {
    faasr_log(paste("✗ Step 3 FAILED:", e$message))
    stop(e)
  })
  
  # Step 4: STICr processing
  tryCatch({
    faasr_log("Step 4: Running STICr...")
    tidy_data <- tidy_hobo_data(infile = "input_data.csv", outfile = FALSE)
    
    if (is.null(tidy_data) || nrow(tidy_data) == 0) {
      stop("STICr returned empty result")
    }
    
    faasr_log(paste("✓ Step 4: STICr success -", nrow(tidy_data), "rows"))
  }, error = function(e) {
    faasr_log(paste("✗ Step 4 FAILED:", e$message))
    stop(e)
  })
  
  # Step 5: Save and upload
  tryCatch({
    faasr_log("Step 5: Uploading results...")
    write.csv(tidy_data, "tidy_output.csv", row.names = FALSE)
    faasr_put_file(local_file = "tidy_output.csv",
                   remote_folder = "stic-processed/tidy",
                   remote_file = "STIC_GP_KNZ_02M10_LS_2022_sticr_tidy.csv")
    faasr_log("✓ Step 5: Upload complete")
  }, error = function(e) {
    faasr_log(paste("✗ Step 5 FAILED:", e$message))
    stop(e)
  })
  
  faasr_log("🎉 SUCCESS: STICr workflow complete!")
  return("STICr processing completed")
}
