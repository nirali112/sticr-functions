faasr_tidy_hobo_sticr <- function() {
  faasr_log("=== STEP 1: LIBRARY LOADING TEST ===")
  
  # Test library loading
  tryCatch({
    faasr_log("Loading STICr...")
    library(STICr)
    faasr_log("✓ STICr loaded successfully")
    
    faasr_log("Loading tidyverse...")
    library(tidyverse)
    faasr_log("✓ tidyverse loaded successfully")
    
    faasr_log("Loading lubridate...")
    library(lubridate)
    faasr_log("✓ lubridate loaded successfully")
    
    faasr_log(paste("STICr version:", as.character(packageVersion("STICr"))))
    
  }, error = function(e) {
    faasr_log(paste("✗ Library loading FAILED:", e$message))
    stop(e)
  })
  
  faasr_log("✓ All libraries loaded successfully")
  return("Step 1 completed - libraries loaded")
}
