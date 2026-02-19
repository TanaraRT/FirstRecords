##########################################################################
##                                                                      ##
##                       FIRST RECORDS WORKFLOW                         ##
##                    Standardize remaining terms                       ##
##                   -----------------------------                      ##
##                                                                      ##
## T. Renard Truong, H. Seebens                                         ##
## v2.0, October 2025                                                   ##
##########################################################################

standardize_terms <- function(dataset = NULL, 
                              save_to_disk = FALSE, 
                              term = NULL, # term to standardize
                              data_dir = NULL) {
  
  if (is.null(dataset) || !is.data.table(dataset)) {
    stop("Error: argument 'dataset' must be a non-null data.table.")
  }
  
  cat(paste0("\n - Standardize '", term, "' terms"))
  
  # --- Load reference table ---
  standard_table <- fread(file.path(data_dir, paste0("config/standard_", term, ".csv")), header = TRUE, fill=TRUE)
  col_std <- grep("std|standard|term", names(standard_table), value = TRUE)[1]  # standard term
  col_var <- grep("var", names(standard_table), value = TRUE)[1]                # variants
  
  if (is.na(col_std) || is.na(col_var)) {
    stop("Cannot find standard or variant columns in CSV. Check 'standard_", term, ".csv'.")
  }
  
  # --- Expand variants ---
  standard_table_exp <- standard_table[, .(variant = unlist(strsplit(get(col_var), ";"))), by = col_std]
  setnames(standard_table_exp, col_std, "std_value")
  standard_table_exp[, variant := trimws(tolower(variant))]
  
  # --- Prepare dataset ---
  dat <- copy(dataset)
  
  normalize_text <- function(x) {
    x <- tolower(x) # # replaces capital letters
    x <- gsub("[\u00A0]", " ", x) # non-breaking spaces
    x <- gsub(";", ",", x) # replace semicolons by commas in dataset text
    x <- gsub("\\s*[|/,]\\s*", "|", x) # unify remaining separators into '|'
    x <- gsub("\\s+", " ", x) # normalize spaces
    x <- trimws(x) # remove trailing and ending spaces
    return(x)
  }
  
  dat[, term_lower := normalize_text(get(term))] # create temporary clean "term_lower" column
  standard_table_exp[, variant := normalize_text(variant)] # clean the variant terms too
  
  # --- Match standardized values ---
  dat[, matched_value := standard_table_exp$std_value[match(term_lower, standard_table_exp$variant)]]
  
  # --- Replace unmatched with blank ---
  dat[, (term) := fifelse(!is.na(matched_value), matched_value, "NA")]
  
  # --- Identify unresolved (original terms that became blank) ---
  unresolved <- dat[get(term) == "" | is.na(get(term)), .(unmatched_term = term_lower)]
  unresolved <- unique(unresolved[unmatched_term != "" & !is.na(unmatched_term)])
  
  # --- Create unmatched terms table ---
  if (nrow(unresolved) > 0) {
    unresolved[, context := term]
    filename_unres <- file.path(data_dir, "tmp", paste0("fr_check_unresolved_", term, ".csv"))
    fwrite(unresolved, filename_unres)
    
    cat(
      "\nWarning: ", nrow(unresolved), " ", term,
      " terms could not be standardized. See '", filename_unres, "' for details.\n",
      sep = ""
      )
  }
  
  # --- Cleanup ---
  dat[, c("term_lower", "matched_value") := NULL]
  
  # --- Save standardized dataset ---
  if (save_to_disk) {
    filename_out <- file.path(data_dir, "tmp", paste0("fr_main_dataset_", term, "_standardized.csv"))
    fwrite(dat, filename_out)
    cat("\nUpdated dataset saved to '", filename_out, sep = "")
  }
  
  cat("\n - ", term, " standardization completed. Unmatched terms replaced with blanks.", sep = "")
  return(dat)
}
