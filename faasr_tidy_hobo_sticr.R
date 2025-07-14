# Step 2: Create the final R function file for your new repository
cat('
faasr_tidy_hobo_sticr <- function(input_file, output_file) {
  # Load STICr package (will be installed from GitHub by FaaSr)
  library(STICr)
  library(tidyverse)
  library(lubridate)
  library(jsonlite)
  
  # Log start with enhanced information
  faasr_log(paste("Starting STICr tidy_hobo_data process for:", input_file))
  faasr_log(paste("STICr version:", as.character(packageVersion("STICr"))))
  faasr_log(paste("Expected output file:", output_file))
  
  # Download input file from Minio
  tryCatch({
    faasr_get_file(remote_folder = "stic-data", 
                   remote_file = input_file, 
                   local_file = "input_data.csv")
    faasr_log(paste("Successfully downloaded", input_file, "from Minio"))
    
    # Check if file was downloaded
    if (file.exists("input_data.csv")) {
      file_size <- file.info("input_data.csv")$size
      faasr_log(paste("Downloaded file size:", file_size, "bytes"))
    } else {
      stop("Downloaded file not found locally")
    }
    
  }, error = function(e) {
    faasr_log(paste("ERROR downloading file:", e$message))
    stop("Failed to download input file")
  })
  
  # Use STICr package tidy_hobo_data function
  tryCatch({
    faasr_log("Running STICr::tidy_hobo_data...")
    
    # STICr tidy_hobo_data expects infile path and outfile (FALSE for return data)
    tidy_data <- tidy_hobo_data(infile = "input_data.csv", outfile = FALSE)
    
    faasr_log(paste("STICr processing completed:", nrow(tidy_data), "rows processed"))
    faasr_log(paste("Output columns:", paste(colnames(tidy_data), collapse = ", ")))
    
  }, error = function(e) {
    faasr_log(paste("ERROR in STICr processing:", e$message))
    stop("STICr tidy_hobo_data failed")
  })
  
  # Save and upload results
  tryCatch({
    write.csv(tidy_data, "tidy_output.csv", row.names = FALSE)
    faasr_log(paste("Saved tidied data locally:", nrow(tidy_data), "rows"))
    
    faasr_put_file(local_file = "tidy_output.csv",
                   remote_folder = "stic-processed/tidy",
                   remote_file = output_file)
    
    faasr_log(paste("✓ STICr tidy process completed successfully"))
    faasr_log(paste("✓ Output uploaded to: stic-processed/tidy/", output_file))
    
  }, error = function(e) {
    faasr_log(paste("ERROR saving/uploading:", e$message))
    stop("Failed to save or upload output")
  })
  
  faasr_log("=== STICr WORKFLOW COMPLETED SUCCESSFULLY ===")
  return("STICr tidy processing completed")
}
', file = "faasr_tidy_hobo_sticr_final.R")

cat("✓ Created faasr_tidy_hobo_sticr_final.R\n")
cat("✓ Upload this file to: nirali112/sticr-functions repository\n")
cat("✓ File name on GitHub: faasr_tidy_hobo_sticr.R\n")
