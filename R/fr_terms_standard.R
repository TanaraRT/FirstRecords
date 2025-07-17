##########################################################################
##                                                                      ##
##                       FIRST RECORDS WORKFLOW                         ##
##                    Standardize remaining terms                       ##
##                   -----------------------------                      ##
##                                                                      ##
## H. Seebens, T. Renard Truong                                         ##
## vx.x, 2025                                                           ##
##########################################################################



fr_terms_standard <- function(dataset){
    
    standard_terms <- fread("data/config/standard_terms.csv")
    stopifnot(is.data.table(dataset), is.data.table(standard_terms))
    
    unresolved_all <- list()
    
    # --- 1. establishmentMeans ---
    result <- standardize_and_filter_terms(
      dataset, "establishmentMeans", standard_terms,
      "origTerm_establishmentMeans", "standardTerm_establishmentMeans"
    )
    dataset <- result$cleaned_data
    unresolved_all[["establishmentMeans"]] <- result$unresolved_terms
    
    # --- 2. occurrenceStatus ---
    result <- standardize_and_filter_terms(
      dataset, "occurrenceStatus", standard_terms,
      "origTerm_occurrenceStatus", "standardTerm_occurrenceStatus"
    )
    dataset <- result$cleaned_data
    unresolved_all[["occurrenceStatus"]] <- result$unresolved_terms
    
    # Apply custom rule: assume "present" unless explicitly "absent"
    dataset[occurrenceStatus != "absent", occurrenceStatus := "present"]
    
    # --- 3. degreeOfEstablishment ---
    result <- standardize_and_filter_terms(
      dataset, "degreeOfEstablishment", standard_terms,
      "origTerm_degreeOfEstablishment", "standardTerm_degreeOfEstablishment"
    )
    dataset <- result$cleaned_data
    unresolved_all[["degreeOfEstablishment"]] <- result$unresolved_terms
    
    # --- 4. pathway ---
    result <- standardize_and_filter_terms(
      dataset, "pathway", standard_terms,
      "origTerm_pathway", "standardTerm_pathway"
    )
    dataset <- result$cleaned_data
    unresolved_all[["pathway"]] <- result$unresolved_terms
    
    # --- 5. habitat ---
    result <- standardize_and_filter_terms(
      dataset, "habitat", standard_terms,
      "origTerm_habitat", "standardTerm_habitat"
    )
    dataset <- result$cleaned_data
    unresolved_all[["habitat"]] <- result$unresolved_terms
    
    # --- Combine unresolved terms ---
    all_unresolved <- rbindlist(unresolved_all, use.names = TRUE, fill = TRUE)
#    setnames(all_unresolved, "V1", "unmatched_term")
    if (nrow(all_unresolved) > 0) {
      setnames(all_unresolved, names(all_unresolved)[1], "unmatched_term")  # rename first column safely
      fwrite(unique(all_unresolved), "data/tmp/check_unresolved_terms.csv")
      cat("\n    ⚠ Warning: Unresolved terms found. See data/tmp/check_unresolved_terms.csv\n")
    }    
#    if (nrow(all_unresolved) > 0) {
#      fwrite(unique(all_unresolved), "data/tmp/check_unresolved_terms.csv")
#      cat("\n    ⚠ Warning: Unresolved terms found. See data/tmp/check_unresolved_terms.csv\n")
#    }
    
    # --- Export cleaned dataset ---
    fwrite(dataset, "data/outputs/fr_main_dataset_final.csv")
    
    return(dataset)
  }
