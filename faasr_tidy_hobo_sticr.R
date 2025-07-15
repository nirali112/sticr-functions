faasr_tidy_hobo_sticr <- function() {
  cat("=== PROCESSING ALREADY-TIDIED STIC DATA ===\n")
  
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
  
  # Read and examine the already-processed data
  tryCatch({
    cat("Reading already-processed STIC data...\n")
    stic_data <- read.csv("input_data.csv")
    
    cat("Data columns:", paste(colnames(stic_data), collapse = ", "), "\n")
    cat("Number of rows:", nrow(stic_data), "\n")
    cat("Date range:", min(stic_data$datetime), "to", max(stic_data$datetime), "\n")
    
    # Check what we have
    cat("Data summary:\n")
    cat("- condUncal (uncalibrated conductivity):", range(stic_data$condUncal, na.rm = TRUE), "\n")
    cat("- tempC (temperature):", range(stic_data$tempC, na.rm = TRUE), "\n")
    cat("- SpC (specific conductivity):", range(stic_data$SpC, na.rm = TRUE), "\n")
    cat("- wetdry classification:", table(stic_data$wetdry), "\n")
    
    # Since data is already processed, we can perform analysis or validation
    # Step 4: QAQC (optional - data already has QAQC column)
    cat("QAQC flags present:", paste(unique(stic_data$QAQC), collapse = ", "), "\n")
    
    # Step 5: Additional processing or validation could go here
    # For example, we could:
    # - Filter data by quality rating
    # - Analyze temporal patterns
    # - Extract specific time periods
    # - Calculate summary statistics
    
    # Example: Filter for excellent quality data only
    excellent_data <- stic_data[stic_data$qual_rating == "excellent", ]
    cat("Excellent quality data rows:", nrow(excellent_data), "\n")
    
    # Save processed results
    write.csv(excellent_data, "excellent_quality_stic_data.csv", row.names = FALSE)
    cat("Saved excellent quality data\n")
    
    # Upload results
    faasr_put_file(local_file = "excellent_quality_stic_data.csv",
                   remote_folder = "stic-processed/filtered",
                   remote_file = "STIC_GP_KNZ_02M10_LS_2022_excellent_quality.csv")
    cat("✓ Excellent quality data uploaded\n")
    
    # Also upload the full dataset for reference
    write.csv(stic_data, "full_stic_data.csv", row.names = FALSE)
    faasr_put_file(local_file = "full_stic_data.csv",
                   remote_folder = "stic-processed/full",
                   remote_file = "STIC_GP_KNZ_02M10_LS_2022_full_processed.csv")
    cat("✓ Full processed data uploaded\n")
    
  }, error = function(e) {
    cat("Error processing STIC data:", e$message, "\n")
    stop(e)
  })
  
  cat("🎉 SUCCESS: Already-processed STIC data analyzed and organized!\n")
  return("STIC data processing completed")
}
