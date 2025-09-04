##########################################################################
##                                                                      ##
##                       FIRST RECORDS WORKFLOW                         ##
##                    Standardize remaining terms                       ##
##                   -----------------------------                      ##
##                                                                      ##
## T. Renard Truong, H. Seebens                                         ##
## v2.0, August 2025                                                    ##
##########################################################################

fr_terms_standard <- function(dataset, 
                              use_log = FALSE, 
                              data_dir = NULL
                              # output, 
                              # input, 
                              # tmp, 
                              # config
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
    dataset, "establishmentMeans", standard_terms,
    "origTerm_establishmentMeans", "standardTerm_establishmentMeans"
    )
  dataset <- result$cleaned_data
  unresolved_all[["establishmentMeans"]] <- result$unresolved_terms
  cat("  - establishmentMeans terms have been standardized")
    
  # --- 2. occurrenceStatus ---
  result <- standardize_and_filter_terms(
    dataset, "occurrenceStatus", standard_terms,
    "origTerm_occurrenceStatus", "standardTerm_occurrenceStatus"
    )
  dataset <- result$cleaned_data
  unresolved_all[["occurrenceStatus"]] <- result$unresolved_terms
  cat("\n  - occurrenceStatus terms have been standardized")
    
  # Apply custom rule: assume "present" unless explicitly "absent"
  dataset[occurrenceStatus != "absent", occurrenceStatus := "present"]
    
  # --- 3. degreeOfEstablishment ---
  result <- standardize_and_filter_terms(
    dataset, "degreeOfEstablishment", standard_terms,
    "origTerm_degreeOfEstablishment", "standardTerm_degreeOfEstablishment"
  )
  dataset <- result$cleaned_data
  unresolved_all[["degreeOfEstablishment"]] <- result$unresolved_terms
  cat("\n  - degreeOfEstablishment terms have been standardized")
    
  # --- 4. pathway ---
  result <- standardize_and_filter_terms(
    dataset, "pathway", standard_terms,
    "origTerm_pathway", "standardTerm_pathway"
  )
  dataset <- result$cleaned_data
  unresolved_all[["pathway"]] <- result$unresolved_terms
  cat("\n  - pathway terms have been standardized")
    
  # --- 5. habitat ---
  result <- standardize_and_filter_terms(
    dataset, "habitat", standard_terms,
    "origTerm_habitat", "standardTerm_habitat"
    )
  dataset <- result$cleaned_data
  unresolved_all[["habitat"]] <- result$unresolved_terms
  cat("\n  - habitat terms have been standardized")
    
  # ---6. datasets ---
  result <- standardize_and_filter_terms(
    dataset, "datasetName", standard_terms,
    "origTerm_datasetName", "standardTerm_datasetName", "bibliographicCitation",
    "bibliographicCitation_dataset"
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
    
  # --- Delete duplicates ---
  # Delete rows where "taxon", "verbatimLocation" and "firstRecordEvent" are similar
  dataset <- unique(dataset, by = c("taxon", "verbatimLocation", "firstRecordEvent"))
  # If we have first records for a taxa in the same location, select the one from a paper in priority, or the earliest one
  dataset <- dataset[
    order(!(nzchar(datasetName)), firstRecordEvent),
    .SD[1],
    by = .(taxon, location)
  ]
  #dataset <- dataset[order(firstRecordEvent), .SD[1], by = .(taxon, location)]
     
  # --- Export cleaned dataset ---
  filename <- file.path(data_dir, "output", paste0("fr_main_dataset_final_", Sys.Date(), ".csv"))
  fwrite(dataset, filename)
    
  cat("\n  Final dataset available in output folder\n ")
  if (use_log == TRUE){
    sink()
  }
  return(dataset)
}
