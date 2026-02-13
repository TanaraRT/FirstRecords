##########################################################################
##                                                                      ##
##                       FIRST RECORDS WORKFLOW                         ##
##                       Saving output dataset                          ##
##                   -----------------------------                      ##
##                                                                      ##
## T. Renard Truong, H. Seebens                                         ##
## v2.0, August 2025                                                    ##
##########################################################################

fr_save_output_dataset <- function(dataset, 
                                   use_log = FALSE, 
                                   data_dir = NULL,
                                   identifier = NULL
                                   ){
  
  # --- Open log file ---
  if (use_log == TRUE){
    log_file <- file.path(data_dir, "output", paste0("log_file_", Sys.Date(), ".txt"))
    if (file.exists(log_file)) {
      sink(log_file, append = TRUE)  # Open log file for appending
    } else {
      sink(log_file, append = FALSE) # Create new log file
    }
  }
  cat("\nSTEP 6: Saving final dataset") 
  
  # --- Delete duplicates ---
  
  ## Delete rows where "taxon", "verbatimLocation" and "firstRecordEvent" are similar
  dataset <- unique(dataset, by = c("taxon", "verbatimLocation", "firstRecordEvent"))
  
  ## Select one first record per "location" and "taxon"
  # Prioritization rule: prefer first record from a paper. If multiple, keep the earliest first record
  orig_cols <- names(dataset)  # remember original column order
  
  dataset <- dataset[
    order(!(nzchar(datasetName)), firstRecordEvent),   # 1. Sort: papers first (non-empty datasetName), then by earliest firstRecordEvent
    .SD[1],                                            # 2. Within each group, keep the first row
    by = .(taxon, location),                           # 3. Groups defined by taxon × location
    .SDcols = orig_cols                                # 4. Work only on the original columns
  ][, ..orig_cols                                      # 5. Return only the original columns
  ][order(location, verbatimLocation, taxon)]          # 6. Final ordering for readability
  
  # --- Export taxonomy table ---
  taxonomy_table <- unique(dataset[,c("taxonID", "taxon", "originalNameUsage", "scientificName", "scientificNameAuthorship", 
                                                "GBIFstatus","GBIFstatus_Synonym", "GBIFmatchtype", "GBIFtaxonRank",
                                                "GBIFusageKey","GBIFnote","species","genus","family",
                                                "order","class","phylum","kingdom", "taxaGroup"
  )])
  
  filename <- file.path(data_dir, "output", paste0("taxonomy_table_", identifier, ".csv"))
  fwrite(taxonomy_table, filename)
  
  # --- Export cleaned dataset ---
  filename <- file.path(data_dir, "output", paste0("fr_main_dataset_final_", identifier, ".csv"))
  dataset <- dataset[, c("locationID", "location", "verbatimLocation", "taxonID", "taxon",
                         "habitat",	"firstRecordEvent",	"verbatimFirstRecordEvent", 
                         "confidenceFirstRecordEvent",	"occurrenceStatus",	"establishmentMeans",
                         "degreeOfEstablishment", "pathway",	"datasetName",	"bibliographicCitation",	
                         "accessRights"
  )]
  fwrite(
    dataset,
    file = filename,
    sep = ";",        # use semicolon as separator
    quote = TRUE,     # force all fields to be quoted as text
    row.names = FALSE
  )
  
  cat("\n  Final dataset available in output folder\n ")
  
  if (use_log == TRUE){
    sink()
  }
  
  return(dataset)
  
}