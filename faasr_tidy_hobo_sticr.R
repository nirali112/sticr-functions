faasr_tidy_hobo_sticr <- function() {
  cat("=== STEP 1: CUSTOM STICR TIDYING WORKFLOW ===\n")
  
  # Load libraries
  library(STICr)
  library(tidyverse)
  library(lubridate)
  cat("✓ Libraries loaded\n")
  
  # Download file
  faasr_get_file(remote_folder = "stic-data", 
                 remote_file = "STIC_GP_KNZ_02M10_LS_2022.csv", 
                 local_file = "input_data.csv")
  cat("✓ File downloaded\n")
  
  tryCatch({
    cat("Step 1: Manual tidying to STICr format...\n")
    
    # Read the original data
    original_data <- read.csv("input_data.csv")
    cat("Original data columns:", paste(colnames(original_data), collapse = ", "), "\n")
    cat("Original data rows:", nrow(original_data), "\n")
    
    # Create STICr-compatible tidy format
    # STICr's tidy_hobo_data() produces: datetime, condUncal, tempC
    tidy_data <- data.frame(
      datetime = as.POSIXct(original_data$datetime, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
      condUncal = as.numeric(original_data$condUncal),
      tempC = as.numeric(original_data$tempC),
      stringsAsFactors = FALSE
    )
    
    # Remove any rows with missing essential data
    tidy_data <- tidy_data[complete.cases(tidy_data), ]
    
    cat("✓ Manual tidying completed\n")
    cat("Tidy data columns:", paste(colnames(tidy_data), collapse = ", "), "\n")
    cat("Tidy data rows:", nrow(tidy_data), "\n")
    
    # Validate the tidied data
    cat("Validating tidied data...\n")
    cat("- Date range:", min(tidy_data$datetime, na.rm = TRUE), "to", max(tidy_data$datetime, na.rm = TRUE), "\n")
    cat("- condUncal range:", min(tidy_data$condUncal, na.rm = TRUE), "to", max(tidy_data$condUncal, na.rm = TRUE), "\n")
    cat("- tempC range:", min(tidy_data$tempC, na.rm = TRUE), "to", max(tidy_data$tempC, na.rm = TRUE), "\n")
    
    # Show sample of tidied data
    cat("Sample of tidied data:\n")
    print(head(tidy_data, 3))
    
    # Save Step 1 output in STICr format
    write.csv(tidy_data, "step1_tidy_sticr_format.csv", row.names = FALSE)
    cat("✓ Step 1 tidy data saved locally\n")
    
    # Upload Step 1 results
    faasr_put_file(local_file = "step1_tidy_sticr_format.csv",
                   remote_folder = "sticr-workflow/step1-tidy",
                   remote_file = "STIC_GP_KNZ_02M10_LS_2022_step1_tidy.csv")
    cat("✓ Step 1 results uploaded to MinIO\n")
    
    # Also save the original data for reference
    faasr_put_file(local_file = "input_data.csv",
                   remote_folder = "sticr-workflow/original",
                   remote_file = "STIC_GP_KNZ_02M10_LS_2022_original.csv")
    cat("✓ Original data also uploaded for reference\n")
    
  }, error = function(e) {
    cat("✗ Step 1 FAILED:", e$message, "\n")
    stop(e)
  })
  
  cat("🎉 STEP 1 COMPLETE: Manual tidying successful!\n")
  cat("📁 Output: step1_tidy_sticr_format.csv (datetime, condUncal, tempC)\n")
  cat("📤 Uploaded to: sticr-workflow/step1-tidy/\n")
  cat("🔜 Ready for Step 2: Calibration\n")
  
  return("Step 1: Manual tidying completed successfully")
}
