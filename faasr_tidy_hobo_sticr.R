faasr_tidy_hobo_sticr <- function() {
  faasr_log("=== STEP 4: STICR PROCESSING ONLY ===")
  
  # Load libraries
  library(STICr)
  library(tidyverse)
  library(lubridate)
  faasr_log("Libraries loaded")
  
  # Download file
  faasr_get_file(remote_folder = "stic-data", 
                 remote_file = "STIC_GP_KNZ_02M10_LS_2022.csv", 
                 local_file = "input_data.csv")
  faasr_log("File downloaded")
  
  # Run STICr
  tidy_data <- tidy_hobo_data(infile = "input_data.csv", outfile = FALSE)
  faasr_log(paste("STICr completed -", nrow(tidy_data), "rows"))
  
  return("Step 4 completed")
}
