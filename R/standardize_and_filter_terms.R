##########################################################################
##                                                                      ##
##                       FIRST RECORDS WORKFLOW                         ##
##            Standardize remaining term - general function             ##
##   Check and replace terms using a standard terms reference table     ##
##                   -----------------------------                      ##
##                                                                      ##
## T. Renard Truong                                                     ##
## vx.x, 2025                                                           ##
##########################################################################

standardize_and_filter_terms <- function(dt, term_col, std_table, orig_col, std_col) {
  term_col_sym <- as.name(term_col)
  
  # Clean and lowercase for easy matching
  dt[, (term_col) := trimws(get(term_col))]
  dt[, term_lc := tolower(get(term_col))]
  std_table[, orig_lc := tolower(get(orig_col))]
  std_table[, std_lc := tolower(get(std_col))]
  
  # First-pass match: original/variant terms
  match_idx <- match(dt$term_lc, std_table$orig_lc)
  translated <- std_table[[std_col]][match_idx]
  
  # Replace matched terms
  dt[!is.na(match_idx), (term_col) := translated[!is.na(match_idx)]]
  
  # Collect unresolved terms
  unresolved <- unique(dt[is.na(match_idx), ..term_col])
  unresolved <- unresolved[get(term_col) != ""]
  if (nrow(unresolved) > 0) unresolved[, context := term_col]
  
  # Second pass: strict match to identify and delete unmatched terms
  match_idx2 <- match(tolower(dt[[term_col]]), std_table$std_lc)
  final_translation <- std_table[[std_col]][match_idx2]
  dt[, (term_col) := fifelse(!is.na(match_idx2), final_translation, "")]
  
  # Cleanup
  dt[, term_lc := NULL]
  std_table[, c("orig_lc", "std_lc") := NULL]
  
  return(list(cleaned_data = dt, unresolved_terms = unresolved))
}
