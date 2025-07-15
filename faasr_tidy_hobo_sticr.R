faasr_tidy_hobo_sticr <- function() {
  # FaaSr functions typically don't take arguments - they get config from environment
  faasr_log("=== FUNCTION START ===")
  
  # Hard-code the file names for now (can be made configurable later)
  input_file <- "STIC_GP_KNZ_02M10_LS_2022.csv"
  output_file <- "STIC_GP_KNZ_02M10_LS_2022_sticr_tidy.csv"
  
  faasr_log(paste("Processing file:", input_file, "-> output:", output_file))
  
  # Step 1: Basic setup check
  tryCatch({
    faasr_log("Step 1: Basic setup...")
    faasr_log("✓ Step 1: Setup OK")
  }, error = function(e) {
    faasr_log(paste("✗ Step 1 FAILED:", e$message))
    stop(e)
  })
  
  # Step 2: Load libraries (they should already be installed by FaaSr)
  tryCatch({
    faasr_log("Step 2: Loading libraries...")
    library(STICr)
    library(tidyverse)
    library(lubridate)
    
    faasr_log(paste("STICr version:", as.character(packageVersion("STICr"))))
    faasr_log("✓ Step 2: Libraries loaded")
  }, error = function(e) {
    faasr_log(paste("✗ Step 2 FAILED:", e$message))
    stop(e)
  })
  
  # Step 3: Download file with error handling
  tryCatch({
    faasr_log("Step 3: Downloading input file...")
    faasr_log(paste("Attempting to download:", input_file, "from stic-data folder"))
    
    faasr_get_file(remote_folder = "stic-data", 
                   remote_file = input_file, 
                   local_file = "input_data.csv")
    
    # Check if file was downloaded
    if (!file.exists("input_data.csv")) {
      stop("input_data.csv was not created after download")
    }
    
    file_info <- file.info("input_data.csv")
    faasr_log(paste("✓ Step 3: File downloaded successfully, size:", file_info$size, "bytes"))
  }, error = function(e) {
    faasr_log(paste("✗ Step 3 FAILED:", e$message))
    stop(e)
  })
  
  # Step 4: Check input data format
  tryCatch({
    faasr_log("Step 4: Checking input data format...")
    
    # Read first few lines to understand format
    input_preview <- read.csv("input_data.csv", nrows = 5)
    faasr_log(paste("Input columns:", paste(colnames(input_preview), collapse = ", ")))
    faasr_log(paste("Number of rows in preview:", nrow(input_preview)))
    
    # Check total file size
    total_rows <- nrow(read.csv("input_data.csv"))
    faasr_log(paste("Total rows in file:", total_rows))
    
    faasr_log("✓ Step 4: Input data checked")
  }, error = function(e) {
    faasr_log(paste("✗ Step 4 FAILED:", e$message))
    stop(e)
  })
  
  # Step 5: Check STICr function availability
  tryCatch({
    faasr_log("Step 5: Checking STICr function availability...")
    
    # List all STICr functions
    sticr_functions <- ls("package:STICr")
    faasr_log(paste("Available STICr functions:", paste(sticr_functions, collapse = ", ")))
    
    # Check if tidy_hobo_data function exists
    if (!"tidy_hobo_data" %in% sticr_functions) {
      stop("tidy_hobo_data function not found in STICr package")
    }
    
    faasr_log("✓ Step 5: tidy_hobo_data function found")
  }, error = function(e) {
    faasr_log(paste("✗ Step 5 FAILED:", e$message))
    stop(e)
  })
  
  # Step 6: Process data with STICr
  tryCatch({
    faasr_log("Step 6: Running STICr::tidy_hobo_data...")
    
    tidy_data <- tidy_hobo_data(infile = "input_data.csv", outfile = FALSE)
    
    if (is.null(tidy_data) || nrow(tidy_data) == 0) {
      stop("STICr returned null or empty data")
    }
    
    faasr_log(paste("✓ Step 6: STICr processing completed:", nrow(tidy_data), "rows processed"))
    faasr_log(paste("Output columns:", paste(colnames(tidy_data), collapse = ", ")))
  }, error = function(e) {
    faasr_log(paste("✗ Step 6 FAILED:", e$message))
    stop(e)
  })
  
  # Step 7: Save and upload results
  tryCatch({
    faasr_log("Step 7: Saving and uploading results...")
    
    write.csv(tidy_data, "tidy_output.csv", row.names = FALSE)
    
    if (!file.exists("tidy_output.csv")) {
      stop("tidy_output.csv was not created")
    }
    
    output_file_info <- file.info("tidy_output.csv")
    faasr_log(paste("Output file created, size:", output_file_info$size, "bytes"))
    
    faasr_put_file(local_file = "tidy_output.csv",
                   remote_folder = "stic-processed/tidy",
                   remote_file = output_file)
    
    faasr_log("✓ Step 7: Results uploaded successfully")
  }, error = function(e) {
    faasr_log(paste("✗ Step 7 FAILED:", e$message))
    stop(e)
  })
  
  faasr_log("✓ STICr processing completed successfully")
  faasr_log(paste("✓ Output uploaded to: stic-processed/tidy/", output_file))
  
  return("STICr tidy processing completed")
}
