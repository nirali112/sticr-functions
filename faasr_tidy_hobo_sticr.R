faasr_tidy_hobo_sticr <- function(input_file, output_file) {
  faasr_log("=== STICR PROCESSING STARTED ===")
  
  # Step 1: Load required packages
  tryCatch({
    faasr_log("Loading required libraries...")
    library(STICr)
    library(tidyverse)
    library(lubridate)
    faasr_log("✓ Libraries loaded successfully")
  }, error = function(e) {
    faasr_log(paste("Error loading libraries:", e$message))
    stop("Library load failed")
  })
  
  # Step 2: Download input file from MinIO
  tryCatch({
    faasr_log(paste("Downloading input file:", input_file))
    faasr_get_file(remote_folder = "stic-data", 
                   remote_file = input_file, 
                   local_file = "input_data.csv")
    faasr_log("✓ Input file downloaded successfully")
  }, error = function(e) {
    faasr_log(paste("Error downloading input file:", e$message))
    stop("File download failed")
  })
  
  # Step 3: Check input file
  tryCatch({
    if (!file.exists("input_data.csv")) {
      stop("Input file not found after download")
    }
    file_info <- file.info("input_data.csv")
    faasr_log(paste("Input file size:", file_info$size, "bytes"))
  }, error = function(e) {
    faasr_log(paste("Error verifying input file:", e$message))
    stop("Input file check failed")
  })
  
  # Step 4: Run STICr processing
  tryCatch({
    faasr_log("Running STICr tidy_hobo_data function...")
    tidy_data <- tidy_hobo_data(infile = "input_data.csv", outfile = FALSE)
    
    if (is.null(tidy_data) || nrow(tidy_data) == 0) {
      stop("tidy_hobo_data returned empty data")
    }
    
    faasr_log(paste("STICr processing complete. Rows:", nrow(tidy_data)))
  }, error = function(e) {
    faasr_log(paste("Error in STICr processing:", e$message))
    stop("STICr processing failed")
  })
  
  # Step 5: Save output and upload back to MinIO
  tryCatch({
    faasr_log(paste("Saving output file:", output_file))
    write.csv(tidy_data, "tidy_output.csv", row.names = FALSE)
    
    faasr_put_file(local_file = "tidy_output.csv",
                   remote_folder = "stic-processed/tidy",
                   remote_file = output_file)
    
    faasr_log("✓ Output file uploaded successfully")
  }, error = function(e) {
    faasr_log(paste("Error saving or uploading output file:", e$message))
    stop("Output file upload failed")
  })
  
  faasr_log("=== STICR PROCESSING COMPLETED SUCCESSFULLY ===")
  return("STICr function execution completed")
}
