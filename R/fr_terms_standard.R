##########################################################################
##                                                                      ##
##                       FIRST RECORDS WORKFLOW                         ##
##                    Standardize remaining terms                       ##
##                   -----------------------------                      ##
##                                                                      ##
## T. Renard Truong, H. Seebens                                         ##
## v2.0, August 2025                                                    ##
##########################################################################

fr_terms_standard <- function(dataset = NULL, 
                              save_to_disk = FALSE,
                              use_log = FALSE, 
                              data_dir = NULL
                              ){
 
  standard_terms <- fread(file.path(data_dir, "config", "standard_terms.csv"))
  
  stopifnot(is.data.table(dataset), is.data.table(standard_terms))
    
  # --- Open log file ---
  if (use_log == TRUE){
    log_file <- file.path(data_dir, "output", paste0("log_file_", Sys.Date(), ".txt"))
    if (file.exists(log_file)) {
      sink(log_file, append = TRUE)  # Open log file for appending
      } else {
        sink(log_file, append = FALSE) # Create new log file
      }
    }
  cat("\nSTEP 5: Standardize remaining terms") 
    
  unresolved_all <- list()
  
  # --- 1. establishmentMeans ---
  result <- standardize_and_filter_terms(
    dt = dataset, 
    term_col = "establishmentMeans", 
    std_table = standard_terms,
    orig_col = "origTerm_establishmentMeans", 
    std_col = "standardTerm_establishmentMeans"
    )
  dataset <- result$cleaned_data
  unresolved_all[["establishmentMeans"]] <- result$unresolved_terms
  cat("  - establishmentMeans terms have been standardized")
    
  # --- 2. occurrenceStatus ---
  result <- standardize_and_filter_terms(
    dt = dataset, 
    term_col = "occurrenceStatus", 
    std_table = standard_terms,
    orig_col = "origTerm_occurrenceStatus", 
    std_col = "standardTerm_occurrenceStatus"
    )
  dataset <- result$cleaned_data
  unresolved_all[["occurrenceStatus"]] <- result$unresolved_terms
  cat("\n  - occurrenceStatus terms have been standardized")
    
  # Assume "present" unless explicitly "absent"
  dataset[occurrenceStatus != "absent", occurrenceStatus := "present"]
    
  # --- 3. degreeOfEstablishment ---
  result <- standardize_and_filter_terms(
    dt = dataset, 
    term_col = "degreeOfEstablishment", 
    std_table = standard_terms,
    orig_col = "origTerm_degreeOfEstablishment", 
    std_col = "standardTerm_degreeOfEstablishment"
  )
  dataset <- result$cleaned_data
  unresolved_all[["degreeOfEstablishment"]] <- result$unresolved_terms
  cat("\n  - degreeOfEstablishment terms have been standardized")
    
  # --- 4. pathway ---
  result <- standardize_and_filter_terms(
    dt = dataset, 
    term_col = "pathway", 
    std_table = standard_terms,
    orig_col = "origTerm_pathway", 
    std_col = "standardTerm_pathway"
  )
  dataset <- result$cleaned_data
  unresolved_all[["pathway"]] <- result$unresolved_terms
  cat("\n  - pathway terms have been standardized")
    
  # --- 5. habitat ---
  result <- standardize_and_filter_terms(
    dt = dataset, 
    term_col = "habitat", 
    std_table = standard_terms,
    orig_col = "origTerm_habitat", 
    std_col = "standardTerm_habitat"
    )
  dataset <- result$cleaned_data
  unresolved_all[["habitat"]] <- result$unresolved_terms
  cat("\n  - habitat terms have been standardized")
    
  # ---6. datasets ---
  result <- standardize_and_filter_terms(
    dt = dataset, 
    term_col = "datasetName", 
    std_table = standard_terms,
    orig_col = "origTerm_datasetName", 
    std_col = "standardTerm_datasetName", 
    ref_col = "bibliographicCitation",
    std_ref = "bibliographicCitation_dataset"
  )
  dataset <- result$cleaned_data

  # --- Combine unresolved terms ---
  all_unresolved <- rbindlist(unresolved_all, use.names = TRUE, fill = TRUE)
    if (nrow(all_unresolved) > 0) {
      setnames(all_unresolved, names(all_unresolved)[1], "unmatched_term")  # rename first column safely
      filename <- file.path(data_dir, "tmp", "check_unresolved_terms.csv")
      #fwrite(sort(unique(missing)), filename, row.names = FALSE, col.names = FALSE)
      fwrite(unique(all_unresolved), filename)
      cat("\n    ⚠ Warning: Unresolved terms found. See 'check_unresolved_terms.csv' available in the 'tmp' folder\n")
    }    
 
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