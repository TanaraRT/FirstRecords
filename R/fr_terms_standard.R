##########################################################################
##                                                                      ##
##                       FIRST RECORDS WORKFLOW                         ##
##                    Standardize remaining terms                       ##
##                   -----------------------------                      ##
##                                                                      ##
## T. Renard Truong, H. Seebens                                         ##
## v2.0, October 2025                                                   ##
##########################################################################

fr_terms_standard <- function(dataset = NULL, 
                                  save_to_disk = FALSE,
                                  use_log = FALSE, 
                                  data_dir = NULL) {
 
   if (is.null(dataset) || !is.data.table(dataset)) {
    stop("Error: argument 'dataset' must be a non-null data.table.")
   }  
  
  # --- Open log file ---
  if (use_log == TRUE){
    log_file <- file.path(data_dir, "output", paste0("log_file_", Sys.Date(), ".txt"))
    if (file.exists(log_file)) {
      sink(log_file, append = TRUE)  # open log file for appending
    } else {
      sink(log_file, append = FALSE, type = "message") # create new log file
    }
  }
  cat("\nSTEP 5: Standardize remaining terms") 
  
  # --- 1. degreeOfEstablishment ---
  
  dataset <- standardize_terms(
    dataset = dataset, 
    save_to_disk = FALSE,
    term = "degreeOfEstablishment",
    data_dir = data_dir)
  
  # --- 2. habitat ----
  
  dataset <- standardize_terms(
    dataset = dataset, 
    save_to_disk = FALSE,
    term = "habitat",
    data_dir = data_dir)

  # --- 3. establishmentMeans ---
  
  dataset <- standardize_terms(
    dataset = dataset, 
    save_to_disk = FALSE,
    term = "establishmentMeans",
    data_dir = data_dir)
  
  # --- 4. occurrenceStatus ---
  
  dataset <- standardize_terms(
    dataset = dataset, 
    save_to_disk = FALSE,
    term = "occurrenceStatus",
    data_dir = data_dir)
  ## Assume "present" unless explicitly "absent"
#  dataset[occurrenceStatus != "absent" | is.na(occurrenceStatus), occurrenceStatus := "present"]
  # Clean whitespace 
  dataset[, occurrenceStatus := trimws(occurrenceStatus)]
  dataset[, occurrenceStatus := tolower(occurrenceStatus)]
  dataset[
    is.na(occurrenceStatus) |
      occurrenceStatus %in% c("", "null", "na"),
    occurrenceStatus := "present"
  ]  
  # --- 5. pathway ---
  
  dataset <- standardize_terms(
    dataset = dataset, 
    save_to_disk = FALSE,
    term = "pathway",
    data_dir = data_dir)  
  
  # --- 6. datasetName ---
  
  dataset <- standardize_terms(
    dataset = dataset, 
    save_to_disk = FALSE,
    term = "datasetName",
    data_dir = data_dir) 
  
  # ---7. Save updated main dataset
  
  if (save_to_disk == TRUE){
    filename <- file.path(data_dir, "tmp", "fr_main_dataset_step5.csv")
    fwrite(dataset, filename)
    cat("\n  - Updated dataset available in 'tmp' folder\n ")
  }
  
  cat("\nStep 5 completed: remaining terms have been standardized") 
  
  if (use_log == TRUE){
    sink()
  }
  
  return(dataset)
}