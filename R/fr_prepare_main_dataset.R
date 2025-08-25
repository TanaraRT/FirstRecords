##########################################################################
##                                                                      ##
##                       FIRST RECORDS WORKFLOW                         ##
##                       Prepare master dataset                         ##
##                   -----------------------------                      ##
##                                                                      ##
## T. Renard Truong, H. Seebens                                         ##
## v2.0, 2025                                                           ##
##########################################################################

fr_prepare_main_dataset <- function (dataset = NULL, use_log = FALSE, save_to_disk = FALSE){
  if (is.null(dataset) || !is.data.frame(dataset)) {
    stop("Invalid input: dataset must be a data.frame or data.table")
  }
  
  # --- Open log file ---
  if (use_log == TRUE){
    log_file <- file.path("data","outputs", paste0("log_file_", Sys.Date(),".txt"))
      if (file.exists(log_file)) {
      sink(log_file, append = TRUE)  # Open log file for appending
      } else {
        sink(log_file, append = FALSE) # Create new log file
      }
  }
  
  cat("\n --- FIRST RECORD ", format(Sys.Date(), "%Y-%m-%d"), " ---\n ")
  cat("\nSTEP 1: Prepare main dataset: fr_main_dataset") 
  
  # --- Prepare master dataset ---
  dataset <- dataset[, .(
    locationID = "",
    verbatimLocation = Country,
    locality = Country,
    country = "",
    region = "",
    taxon = "",
    originalNameUsage = "",
    originalNameUsage1 = GenusSpecies,
    originalNameUsage2 = paste(Genus, Species, Author),
    scientificName = "",
    scientificNameAuthorship = Author,
    GBIFstatus = "MISSING",
    GBIFmatchtype = NA,
    GBIFnote = NA,
    GBIFstatus_Synonym = NA,
    species = NA,
    genus = NA,
    family = NA,
    class = NA,
    order = NA,
    phylum = NA,
    kingdom = NA,
    GBIFtaxonRank = NA,
    GBIFusageKey = NA,
    taxaGroup = LifeForm,
    habitat = Habitat,
    FirstRecord1,
    FirstRecord2,
    FirstRecord,
    DateNaturalisation,
    FirstRecord_intentional,
    firstRecordEvent ="",
    verbatimFirstRecordEvent = "",
    confidenceFirstRecordEvent = "",
    occurrenceStatus = PresentStatus,
    establishmentMeans = PresentStatus,
    degreeOfEstablishment = PresentStatus,
    pathway = Pathway,
    datasetName = Source,
    bibliographicCitation = Source,
    accessRights = DataUsage
    )]
   cat("\n  - Loaded relevant columns")

  # --- Basic cleaning ---
  ## replace NA and "NULL", "unknown", "n.d" and "?" with empty strings
  dataset[is.na(dataset)] <- ""
  dataset[, names(dataset) := lapply(.SD, function(x) {
    if (is.character(x)) {
      x <- gsub("(?i)null", "", x, perl = TRUE)
      x <- gsub("\\.", "", x)
      x <- gsub("(?i)unknown", "", x, perl = TRUE)
      x <- gsub("\\?", "", x, perl = TRUE)
      x
    } else {
      x
    }
  })]
  
  cat("\n  - Replaced NA, 'NULL', 'unknown', 'n.d.' and '?' with empty strings")
  
  ## Delete rows where all columns are empty
  dataset <- dataset[
    rowSums(!(is.na(dataset) | dataset == "")) > 0
  ]
  cat("\n  - Deleted rows where all columns are empty")
  
  # --- Prepare first record columns ---
  ## Store original fr values in verbatimFirstRecordEvent
  dataset[, verbatimFirstRecordEvent := FirstRecord] # Initialize verbatimFirstRecordEvent with FirstRecord
  dataset[ # Assign verbatimFirstRecordEvent from FirstRecord1 if it's empty
    verbatimFirstRecordEvent %in% c("", NA) & FirstRecord1 != "",
    verbatimFirstRecordEvent := FirstRecord1
  ]
  dataset[ # Assign verbatimFirstRecordEvent from FirstRecord2 if it's empty
    verbatimFirstRecordEvent %in% c("", NA) & FirstRecord2 != "",
    verbatimFirstRecordEvent := FirstRecord2
  ]
  dataset[ # Assign verbatimFirstRecordEvent from DateNaturalisation if it's empty
    verbatimFirstRecordEvent %in% c("", NA) & DateNaturalisation != "",
    verbatimFirstRecordEvent := DateNaturalisation
  ]
  dataset[ # Assign verbatimFirstRecordEvent from DateNaturalisation if it's empty
    verbatimFirstRecordEvent %in% c("", NA) & FirstRecord_intentional != "",
    verbatimFirstRecordEvent := FirstRecord_intentional
  ]
  dataset[ # Assign verbatimFirstRecordEvent 
    FirstRecord1 != "" & FirstRecord2 != "",
    verbatimFirstRecordEvent := paste(FirstRecord1, FirstRecord2, sep = " - ")
  ]
  
  ## Prepare firstRecordEvent column
  dataset[, firstRecordEvent := verbatimFirstRecordEvent] # Initialize firstRecordEvent with verbatimFirstRecordEvent
  cat("\n  - Stored original first records in verbatimFirstRecord")
  
  ## Prepare confidenceFirstRecordEvent column
  dataset[, confidenceFirstRecordEvent := "low confidence"] # Initialize with low confidence
  cat("\n  - Created confidenceFirstRecordEvent column and initialized it with 'low confidence'")
  
  # --- Prepare species columns ---
  ## Store original species names in originalNameUsage and create new columns
  #dataset[, originalNameUsage := originalNameUsage1] # Initialize originalNameUsage with originalNameUsage1
  #dataset[ # Assign originalNameUsage from originalNameUsage2 if it's empty
   # originalNameUsage %in% c("", NA) & originalNameUsage1 != "",
    #originalNameUsage := originalNameUsage2
  #]
  
  dataset[, originalNameUsage := fifelse(
    !is.na(originalNameUsage1) & originalNameUsage1 != "",
    originalNameUsage1,
    originalNameUsage2
  )]
  cat("\n  - Stored original names in origninalNameUsage")

  
  ## Delete old columns
  dataset$taxon <- dataset$originalNameUsage
  dataset[, c(
    "DateNaturalisation", "FirstRecord", "FirstRecord1", "FirstRecord2",
    "FirstRecord_intentional", "originalNameUsage1", "originalNameUsage2"
  ) := NULL]
  cat("\n  - Deleted rows where all columns are empty")
  
  cat("\nStep 1 completed: main dataset 'fr_main_dataset' ready to be processed\n ") 
  
  if (save_to_disk == TRUE){
    fwrite(dataset, "data/tmp/fr_main_dataset_step1.csv")
    cat("  - 'fr_main_datastep1.csv' is available in 'data/tmp' folder")
  }
  
  if (use_log == TRUE){
    sink()
  }
  return(dataset)
}
