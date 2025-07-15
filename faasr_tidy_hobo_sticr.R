faasr_tidy_hobo_sticr <- function() {
  cat("=== DEBUGGING STICR COLUMN REQUIREMENTS ===\n")
  
  # Load libraries
  library(STICr)
  library(tidyverse)
  library(lubridate)
  cat("Libraries loaded\n")
  
  # Download and examine file
  faasr_get_file(remote_folder = "stic-data", 
                 remote_file = "STIC_GP_KNZ_02M10_LS_2022.csv", 
                 local_file = "input_data.csv")
  cat("File downloaded\n")
  
  # Read the actual data to see structure
  tryCatch({
    cat("=== EXAMINING INPUT DATA ===\n")
    input_data <- read.csv("input_data.csv")
    cat("Actual columns:", paste(colnames(input_data), collapse = ", "), "\n")
    cat("Number of rows:", nrow(input_data), "\n")
    cat("First few rows:\n")
    print(head(input_data, 3))
    
    # Check STICr documentation or function requirements
    cat("\n=== CHECKING STICR FUNCTION ===\n")
    
    # Let's see what tidy_hobo_data expects by looking at its help
    tryCatch({
      # Try to get function arguments/documentation
      cat("STICr tidy_hobo_data function info:\n")
      print(args(tidy_hobo_data))
    }, error = function(e) {
      cat("Could not get function args\n")
    })
    
    # Try with a different approach - maybe the function needs specific column names
    cat("\n=== TESTING DIFFERENT APPROACHES ===\n")
    
    # Maybe try reading raw without header to see actual format
    raw_lines <- readLines("input_data.csv", n = 5)
    cat("Raw file lines:\n")
    for(i in 1:length(raw_lines)) {
      cat("Line", i, ":", raw_lines[i], "\n")
    }
    
  }, error = function(e) {
    cat("Error examining data:", e$message, "\n")
  })
  
  return("Data examination completed")
}
