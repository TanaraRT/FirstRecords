##########################################################################
##                                                                      ##
##                       FIRST RECORDS WORKFLOW                         ##
##                       Prepare master dataset                         ##
##                   -----------------------------                      ##
##                                                                      ##
## T. Renard Truong, H. Seebens                                         ##
## v2.0, October 2025                                                   ##
##########################################################################

fr_prepare_main_dataset <- function (dataset = NULL, 
                                     use_log = FALSE, 
                                     save_to_disk = FALSE, 
                                     data_dir = NULL
                                     ){
  if (is.null(dataset) || !is.data.frame(dataset)) {
    stop("Invalid input: dataset must be a data.frame or data.table")
  }
  
  # --- Open log file ---
  if (use_log == TRUE){
    log_file <- file.path(data_dir, "output", paste0("log_file_", Sys.Date(), ".txt"))
      if (file.exists(log_file)) {
      sink(log_file, append = TRUE)  # open log file for appending if log_file already exists
      } else {
        sink(log_file, append = FALSE) # create new log file if log_file does not exist yet
      }
  }
  cat("\n --- FIRST RECORD ", format(Sys.Date(), "%Y-%m-%d"), " ---\n ")
  cat("\nSTEP 1: Prepare main dataset: fr_main_dataset") 
  
  # --- Prepare master dataset ---
  
  # print(names(dataset))

  # replace NA with "" to avoid having "NA" in taxon names
  dataset[, Author:=fifelse(is.na(Author), "", Author)] 

  # merge columns of taxonomic information without introducing NAs
  dataset[, originalNameUsage1 := fifelse(!is.na(GenusSpecies), paste(GenusSpecies, Author), "")] # create temporary column to store species names
  dataset[, originalNameUsage2 := fifelse(!is.na(Genus), paste(Genus, Species, Author), "")] # create temporary column to store species names
  
  dataset <- dataset[, .(
    locationID = "", # create locationID column
    verbatimLocation = Country, # create verbatimLocation and initialize it with "Country" from raw dataset
    location = Country, # create locality and initialize it with "Country" from raw dataset
    country = "", # create country column
    region = "", # create region column
    # taxon = "", # create taxon column; removed as it is introduced with GBIF
    originalNameUsage = "", # create originalNameUsage column
    originalNameUsage1 = originalNameUsage1, # create originalNameUsage1 column
    originalNameUsage2 = originalNameUsage2, # create originalNameUsage2 column
    habitat = Habitat, # create habitat and initialize it with "Habitat" from raw dataset
    FirstRecord1, # create temporary column to store first records from raw dataset
    FirstRecord2, # create temporary column to store first records from raw dataset
    FirstRecord, # create temporary column to store first records from raw dataset
    DateNaturalisation, # create temporary column to store first records from raw dataset
    FirstRecord_intentional, # create temporary column to store first records from raw dataset
    firstRecordEvent ="", #  create firstRecordEvent
    verbatimFirstRecordEvent = "", # create verbatimFirstRecordEvent
    confidenceFirstRecordEvent = "", # create confidenceFirstRecordEvent
    occurrenceStatus = PresentStatus, # create occurrenceStatus and initialize it with "PresentStatus" from raw dataset
    establishmentMeans = PresentStatus, #create establishmentMeansand initialize it with "PresentStatus" from raw dataset
    degreeOfEstablishment = PresentStatus, #create degreeOfEstablishment and initialize it with "PresentStatus" from raw dataset
    pathway = Pathway, #create pathway and initialize it with "Pathway" from raw dataset
    datasetName = Source, #create datasetName and initialize it with "Source" from raw dataset
    bibliographicCitation = Source, #create bibliographicCitation and initialize it with "Source" from raw dataset
    accessRights = DataUsage #create accessRights with "DataUsage" and initialize it from raw dataset
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
      x <- gsub("NA NA", "", x) # introduced through paste() for e.g. originalNameUsage1
      x <- gsub("^NA$", "", x)
      x <- str_squish(x)
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
  dataset[ # Assign verbatimFirstRecordEvent from FirstRecord_intentional if it's empty
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
  
  dataset[, originalNameUsage := fifelse(
    !is.na(originalNameUsage1) & originalNameUsage1 != "",
    originalNameUsage1,
    originalNameUsage2
  )]
  cat("\n  - Stored original names in origninalNameUsage")

  ## Delete temporary columns
  # dataset$taxon <- dataset$originalNameUsage
  dataset[, c(
    "DateNaturalisation", "FirstRecord", "FirstRecord1", "FirstRecord2",
    "FirstRecord_intentional", "originalNameUsage1", "originalNameUsage2"
  ) := NULL]
  cat("\n  - Deleted temporary columns are empty")
  
  if (save_to_disk == TRUE){
    filename <- file.path(data_dir, "tmp", "fr_main_dataset_step1.csv")
    fwrite(dataset, filename)
    cat("  - 'fr_main_datastep1.csv' is available in 'tmp' folder")
  }
  cat("\nStep 1 completed: main dataset 'fr_main_dataset' ready to be processed\n ") 
  
  if (use_log == TRUE){
    sink()
  }
  
  return(dataset)
}