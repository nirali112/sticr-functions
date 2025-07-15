faasr_tidy_hobo_sticr <- function(input_file, output_file) {
  # Add detailed error handling and logging
  faasr_log("=== FUNCTION START ===")
  faasr_log(paste("Input arguments - input_file:", input_file, "output_file:", output_file))
  
  # Step 1: Check function arguments
  tryCatch({
    faasr_log("Step 1: Checking function arguments...")
    if (is.null(input_file) || input_file == "") {
      stop("input_file is null or empty")
    }
    if (is.null(output_file) || output_file == "") {
      stop("output_file is null or empty")
    }
    faasr_log("✓ Step 1: Arguments OK")
  }, error = function(e) {
    faasr_log(paste("✗ Step 1 FAILED:", e$message))
    stop(e)
  })
  
  # Step 2: Install packages with error handling
  tryCatch({
    faasr_log("Step 2: Installing required packages...")
    
    # Install remotes if not available
    if (!require(remotes, quietly = TRUE)) {
      faasr_log("Installing remotes...")
      install.packages("remotes")
      library(remotes)
    }
    
    # Install STICr from GitHub (already done by FaaSr, but check if it loads)
    if (!require(STICr, quietly = TRUE)) {
      faasr_log("STICr not found, installing from HEAL-KGS/STICr...")
      remotes::install_github("HEAL-KGS/STICr")
    }
    
    # Install CRAN packages
    if (!require(tidyverse, quietly = TRUE)) {
      faasr_log("Installing tidyverse...")
      install.packages("tidyverse")
    }
    if (!require(lubridate, quietly = TRUE)) {
      faasr_log("Installing lubridate...")
      install.packages("lubridate")
    }
    
    faasr_log("✓ Step 2: Packages installed")
  }, error = function(e) {
    faasr_log(paste("✗ Step 2 FAILED:", e$message))
    stop(e)
  })
  
  # Step 3: Load libraries with error handling
  tryCatch({
    faasr_log("Step 3: Loading libraries...")
    library(STICr)
    library(tidyverse)
    library(lubridate)
    
    faasr_log(paste("STICr version:", as.character(packageVersion("STICr"))))
    faasr_log("✓ Step 3: Libraries loaded")
  }, error = function(e) {
    faasr_log(paste("✗ Step 3 FAILED:", e$message))
    stop(e)
  })
  
  # Step 4: Download file with error handling
  tryCatch({
    faasr_log("Step 4: Downloading input file...")
    faasr_log(paste("Attempting to download:", input_file, "from stic-data folder"))
    
    faasr_get_file(remote_folder = "stic-data", 
                   remote_file = input_file, 
                   local_file = "input_data.csv")
    
    # Check if file was downloaded
    if (!file.exists("input_data.csv")) {
      stop("input_data.csv was not created after download")
    }
    
    file_info <- file.info("input_data.csv")
    faasr_log(paste("✓ Step 4: File downloaded successfully, size:", file_info$size, "bytes"))
  }, error = function(e) {
    faasr_log(paste("✗ Step 4 FAILED:", e$message))
    
    # List available files for debugging
    tryCatch({
      faasr_log("Attempting to list files in MinIO bucket for debugging...")
      # This might not work, but worth trying
      files <- list.files(".", full.names = TRUE)
      faasr_log(paste("Local files:", paste(files, collapse = ", ")))
    }, error = function(e2) {
      faasr_log("Could not list local files")
    })
    
    stop(e)
  })
  
  # Step 5: Check STICr function availability
  tryCatch({
    faasr_log("Step 5: Checking STICr function availability...")
    
    # Check if tidy_hobo_data function exists
    if (!exists("tidy_hobo_data")) {
      stop("tidy_hobo_data function not found in STICr package")
    }
    
    faasr_log("✓ Step 5: tidy_hobo_data function found")
  }, error = function(e) {
    faasr_log(paste("✗ Step 5 FAILED:", e$message))
    
    # List available STICr functions for debugging
    tryCatch({
      sticr_functions <- ls("package:STICr")
      faasr_log(paste("Available STICr functions:", paste(sticr_functions, collapse = ", ")))
    }, error = function(e2) {
      faasr_log("Could not list STICr functions")
    })
    
    stop(e)
  })
  
  # Step 6: Process data with STICr
  tryCatch({
    faasr_log("Step 6: Running STICr::tidy_hobo_data...")
    
    # Check input file before processing
    input_data_preview <- read.csv("input_data.csv", nrows = 5)
    faasr_log(paste("Input data preview - columns:", paste(colnames(input_data_preview), collapse = ", ")))
    faasr_log(paste("Input data preview - first few rows:", nrow(input_data_preview)))
    
    tidy_data <- tidy_hobo_data(infile = "input_data.csv", outfile = FALSE)
    
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
