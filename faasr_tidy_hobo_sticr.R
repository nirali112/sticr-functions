# Step 14: Create a simplified debug version to identify the issue
debug_function <- 'faasr_tidy_hobo_sticr <- function(input_file, output_file) {
  # Load packages with error checking
  tryCatch({
    library(STICr)
    faasr_log("✓ STICr loaded successfully")
  }, error = function(e) {
    faasr_log(paste("ERROR loading STICr:", e$message))
    stop("STICr load failed")
  })
  
  tryCatch({
    library(tidyverse)
    faasr_log("✓ tidyverse loaded successfully")
  }, error = function(e) {
    faasr_log(paste("ERROR loading tidyverse:", e$message))
    stop("tidyverse load failed")
  })
  
  # Log start
  faasr_log(paste("Starting STICr processing for:", input_file))
  faasr_log(paste("STICr version:", as.character(packageVersion("STICr"))))
  
  # Test file download first
  tryCatch({
    faasr_log("Attempting to download file from MinIO...")
    faasr_get_file(remote_folder = "stic-data", 
                   remote_file = input_file, 
                   local_file = "input_data.csv")
    faasr_log("✓ File download completed")
    
    # Check if file exists and log its properties
    if (file.exists("input_data.csv")) {
      file_size <- file.info("input_data.csv")$size
      faasr_log(paste("✓ Downloaded file size:", file_size, "bytes"))
      
      # Read first few lines to check format
      test_lines <- readLines("input_data.csv", n = 3)
      faasr_log(paste("First line:", test_lines[1]))
      faasr_log(paste("Second line:", test_lines[2]))
    } else {
      stop("Downloaded file not found")
    }
    
  }, error = function(e) {
    faasr_log(paste("ERROR in file download:", e$message))
    stop("File download failed")
  })
  
  # Test STICr function
  tryCatch({
    faasr_log("Testing STICr tidy_hobo_data function...")
    tidy_data <- tidy_hobo_data(infile = "input_data.csv", outfile = FALSE)
    faasr_log(paste("✓ STICr completed:", nrow(tidy_data), "rows"))
    
  }, error = function(e) {
    faasr_log(paste("ERROR in STICr:", e$message))
    stop("STICr processing failed")
  })
  
  faasr_log("=== DEBUG TEST COMPLETED ===")
  return("Debug completed")
}'

# Save debug version
writeLines(debug_function, "faasr_tidy_hobo_sticr_debug.R")

cat("✓ Created debug version: faasr_tidy_hobo_sticr_debug.R\n")
cat("✓ Upload this to GitHub to replace the current function\n")
cat("✓ This will show exactly where the error occurs\n")
