faasr_tidy_hobo_sticr <- function() {
  # Use cat() instead of faasr_log() to test
  cat("=== TESTING WITHOUT FAASR_LOG ===\n")
  
  # Test loading STICr
  tryCatch({
    cat("Loading STICr...\n")
    library(STICr)
    cat("STICr loaded successfully\n")
    
    # Simple return
    return("STICr loaded successfully")
    
  }, error = function(e) {
    cat("ERROR loading STICr:", e$message, "\n")
    return(paste("FAILED:", e$message))
  })
}
