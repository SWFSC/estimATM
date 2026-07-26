# =====================================================================
# MVP TO ECHOVIEW ECS PIPELINE
# Ingests raw .m1 casts, computes TEOS-10 physics, and outputs strictly 
# .ecs calibration files by compensating a base ECS template.
# =====================================================================

# Load required packages ----------------------------------------------
library(atm) # For loading functions used to process cast data

# ---------------- USER SETTINGS ----------------
DATA_DIR     <- "Y:\\2606RL\\Cruise Data\\RAW\\MVP\\ToProcess"
OUTPUT_DIR   <- "C:\\SURVEY\\2606RL\\PROCESSED\\EV\\ECS"
ECS_TEMPLATE <- "C:\\SURVEY\\2606RL\\PROCESSED\\EV\\ECS\\_2606RL_Template.ecs"
TX_DEPTH     <- 7.35   
# -----------------------------------------------

dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
m1_files <- list.files(path = DATA_DIR, pattern = "\\.m1$", full.names = TRUE, recursive = TRUE)

cat(sprintf("Starting Pipeline. Found %d MVP profiles...\n", length(m1_files)))

for (file_path in m1_files) {
  cast_id <- tools::file_path_sans_ext(basename(file_path))
  cat(sprintf("\nProcessing MVP Cast: %s\n", cast_id))
  
  processed_data <- atm::mvp_process_file(file_path)
  
  if (is.null(processed_data)) {
    next
  }
  
  # Run the genuine ECS Template Export
  atm::mvp_export_template_ecs(
    raw_descent_df = processed_data$raw_descent, 
    binned_df = processed_data$binned, 
    cast_id = cast_id, 
    output_dir = OUTPUT_DIR,
    template_path = ECS_TEMPLATE,
    tx_depth = TX_DEPTH
  )
}

cat("\nProcessing complete.\n")

