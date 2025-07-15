faasr_tidy_hobo_sticr <- function() {
  cat("=== STICR WITH COLUMN MAPPING ===\n")
  
  # Load libraries
  library(STICr)
  library(tidyverse)
  library(lubridate)
  cat("Libraries loaded\n")
  
  # Download file
  faasr_get_file(remote_folder = "stic-data", 
                 remote_file = "STIC_GP_KNZ_02M10_LS_2022.csv", 
                 local_file = "input_data.csv")
  cat("File downloaded\n")
  
  # Read and examine data
  tryCatch({
    cat("Step 1: Reading original data...\n")
    original_data <- read.csv("input_data.csv")
    cat("Original columns:", paste(colnames(original_data), collapse = ", "), "\n")
    
    # STICr typically expects columns like: DateTime, Temp, SpCond, etc.
    # Let's try creating a version with standard HOBO column names
    cat("Step 2: Creating STICr-compatible column names...\n")
    
    # Create a copy with renamed columns for STICr
    sticr_data <- original_data %>%
      rename(
        DateTime = datetime,
        `Temp, °C` = tempC,
        `SpCond, µS/cm` = condUncal
        # Add other column mappings as needed
      )
    
    cat("New columns:", paste(colnames(sticr_data), collapse = ", "), "\n")
    
    # Save the renamed version
    write.csv(sticr_data, "sticr_input.csv", row.names = FALSE)
    cat("Created STICr-compatible file\n")
    
    # Now try STICr processing
    cat("Step 3: Running STICr on renamed data...\n")
    
    tidy_data <- tidy_hobo_data(infile = "sticr_input.csv", outfile = FALSE)
    
    if (!is.null(tidy_data) && nrow(tidy_data) > 0) {
      cat("✓ STICr SUCCESS with renamed columns!\n")
      cat("Processed rows:", nrow(tidy_data), "\n")
      cat("Output columns:", paste(colnames(tidy_data), collapse = ", "), "\n")
      
      # Save and upload results
      write.csv(tidy_data, "tidy_output.csv", row.names = FALSE)
      faasr_put_file(local_file = "tidy_output.csv",
                     remote_folder = "stic-processed/tidy",
                     remote_file = "STIC_GP_KNZ_02M10_LS_2022_sticr_tidy.csv")
      cat("✓ Results uploaded successfully!\n")
      
      return("STICr processing completed successfully")
      
    } else {
      cat("STICr still returned empty/null\n")
      return("STICr processing failed")
    }
    
  }, error = function(e) {
    cat("Error in STICr processing:", e$message, "\n")
    
    # If column mapping fails, let's try the original approach with different STICr functions
    cat("Trying alternative: using original data directly...\n")
    
    tryCatch({
      # Maybe try a different STICr function or approach
      cat("Attempting direct processing with original column names...\n")
      
      # Sometimes STICr works with the original data if we specify parameters differently
      result <- tidy_hobo_data(infile = "input_data.csv", outfile = FALSE, convert_utc = FALSE)
      
      if (!is.null(result) && nrow(result) > 0) {
        cat("✓ Original data worked!\n")
        cat("Rows:", nrow(result), "\n")
        write.csv(result, "tidy_output.csv", row.names = FALSE)
        faasr_put_file(local_file = "tidy_output.csv",
                       remote_folder = "stic-processed/tidy", 
                       remote_file = "STIC_GP_KNZ_02M10_LS_2022_sticr_tidy.csv")
        return("STICr processing completed with original data")
      }
      
    }, error = function(e2) {
      cat("Alternative approach also failed:", e2$message, "\n")
    })
    
    return(paste("STICr processing failed:", e$message))
  })
}
