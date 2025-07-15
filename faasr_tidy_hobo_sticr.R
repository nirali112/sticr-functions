faasr_tidy_hobo_sticr <- function(input_file, output_file) {
  # Install packages manually (following neon4cast pattern)
  faasr_log("Installing required packages...")
  
  # Install remotes if not available
  if (!require(remotes, quietly = TRUE)) {
    install.packages("remotes")
    library(remotes)
  }
  
  # Install STICr from GitHub (professor's working solution)
  if (!require(STICr, quietly = TRUE)) {
    faasr_log("Installing STICr from HEAL-KGS/STICr...")
    remotes::install_github("HEAL-KGS/STICr")
  }
  
  # Install CRAN packages
  if (!require(tidyverse, quietly = TRUE)) {
    install.packages("tidyverse")
  }
  if (!require(lubridate, quietly = TRUE)) {
    install.packages("lubridate")
  }
  
  # Load libraries
  library(STICr)
  library(tidyverse)
  library(lubridate)
  
  faasr_log(paste("STICr version:", as.character(packageVersion("STICr"))))
  faasr_log(paste("Starting STICr tidy_hobo_data process for:", input_file))
  
  # Download input file from Minio
  faasr_get_file(remote_folder = "stic-data", 
                 remote_file = input_file, 
                 local_file = "input_data.csv")
  
  faasr_log("Downloaded input file successfully")
  
  # Use STICr package tidy_hobo_data function
  faasr_log("Running STICr::tidy_hobo_data...")
  tidy_data <- tidy_hobo_data(infile = "input_data.csv", outfile = FALSE)
  
  faasr_log(paste("STICr processing completed:", nrow(tidy_data), "rows processed"))
  faasr_log(paste("Output columns:", paste(colnames(tidy_data), collapse = ", ")))
  
  # Save and upload results
  write.csv(tidy_data, "tidy_output.csv", row.names = FALSE)
  faasr_put_file(local_file = "tidy_output.csv",
                 remote_folder = "stic-processed/tidy",
                 remote_file = output_file)
  
  faasr_log("✓ STICr processing completed successfully")
  faasr_log(paste("✓ Output uploaded to: stic-processed/tidy/", output_file))
  
  return("STICr tidy processing completed")
}
