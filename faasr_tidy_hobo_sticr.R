faasr_tidy_hobo_sticr <- function() {
  cat("=== STICR WITH CORRECT HOBO COLUMN NAMES ===\n")
  
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
  
  tryCatch({
    cat("Creating HOBO-format data for STICr...\n")
    
    # Read original data
    original_data <- read.csv("input_data.csv")
    
    # STICr expects EXACT HOBO logger column names
    # Based on HOBO U24 Conductivity Logger format:
    hobo_data <- data.frame(
      "Date.Time" = original_data$datetime,  # Note: period not space
      "Temp" = original_data$tempC,
      "Conductivity" = original_data$condUncal,
      stringsAsFactors = FALSE
    )
    
    cat("HOBO format columns:", paste(colnames(hobo_data), collapse = ", "), "\n")
    cat("Sample data:\n")
    print(head(hobo_data, 2))
    
    # Save as HOBO format
    write.csv(hobo_data, "hobo_format.csv", row.names = FALSE)
    cat("Created HOBO format file\n")
    
    # Try STICr processing
    cat("Running STICr on HOBO format...\n")
    tidy_data <- tidy_hobo_data(infile = "hobo_format.csv", outfile = FALSE)
    
    if (!is.null(tidy_data) && nrow(tidy_data) > 0) {
      cat("✓ STICr SUCCESS!\n")
      cat("Processed rows:", nrow(tidy_data), "\n")
      cat("Output columns:", paste(colnames(tidy_data), collapse = ", "), "\n")
      
      # Save results
      write.csv(tidy_data, "tidy_output.csv", row.names = FALSE)
      cat("Saved processed data\n")
      
      # Upload to MinIO - make sure folder exists
      cat("Uploading to MinIO...\n")
      faasr_put_file(local_file = "tidy_output.csv",
                     remote_folder = "stic-processed/tidy",
                     remote_file = "STIC_GP_KNZ_02M10_LS_2022_sticr_tidy.csv")
      cat("✓ Upload completed!\n")
      
      return("STICr processing completed successfully")
      
    } else {
      cat("STICr returned empty result\n")
      return("STICr processing returned empty result")
    }
    
  }, error = function(e) {
    cat("Error:", e$message, "\n")
    
    # If still failing, let's try minimal HOBO format
    cat("Trying minimal HOBO format...\n")
    tryCatch({
      minimal_data <- data.frame(
        "Date.Time" = original_data$datetime,
        "Temp" = original_data$tempC
      )
      write.csv(minimal_data, "minimal_hobo.csv", row.names = FALSE)
      
      result <- tidy_hobo_data(infile = "minimal_hobo.csv", outfile = FALSE)
      if (!is.null(result)) {
        cat("Minimal format worked!\n")
        write.csv(result, "tidy_output.csv", row.names = FALSE)
        faasr_put_file(local_file = "tidy_output.csv",
                       remote_folder = "stic-processed/tidy",
                       remote_file = "STIC_GP_KNZ_02M10_LS_2022_minimal.csv")
        return("Minimal STICr processing completed")
      }
    }, error = function(e2) {
      cat("Minimal format also failed:", e2$message, "\n")
    })
    
    return(paste("All STICr attempts failed:", e$message))
  })
}
