##########################################################################
##                                                                      ##
##                       FIRST RECORDS WORKFLOW                         ##
##                       Prepare master dataset                         ##
##                   -----------------------------                      ##
##                                                                      ##
## H. Seebens, T. Renard Truong                                         ##
## vx.x, 2025                                                           ##
##########################################################################

fr_prepare_main_dataset <- function (dataset = NULL, save_to_disk = FALSE){
  if (is.null(dataset) || !is.data.frame(dataset)) {
    stop("Invalid input: dataset must be a data.frame or data.table")
  }
  # prepare master dataset
  dataset <- dataset[, .(
    locationID = "",
    verbatimLocation = Country,
    locality = Country,
    country = "",
    region = "",
    Taxon = "",
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
    occurenceStatus = PresentStatus,
    establishmentMeans = PresentStatus,
    degreeOfEstablishment = PresentStatus,
    datasetName = Source,
    bibliographicCitation = Source,
    accessRights = DataUsage
    )]

  # replace NA and "NULL", "unknown", "n.d" and "?" with empty strings
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
  
  # delete rows where all columns are empty
  dataset <- dataset[
    rowSums(!(is.na(dataset) | dataset == "")) > 0
  ]
  
  # Store original fr values in verbatimFirstRecordEvent
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
  
  # Prepare firstRecordEvent and confidenceFirstRecordEvent columns
  dataset[, firstRecordEvent := verbatimFirstRecordEvent] # Initialize firstRecordEvent with verbatimFirstRecordEvent
  dataset[, confidenceFirstRecordEvent := "low confidence"] # Initialize with low confidence
  
  # Store original species names in originalNameUsage and create new columns
  dataset[, originalNameUsage := originalNameUsage1] # Initialize originalNameUsage with originalNameUsage1
  dataset[ # Assign originalNameUsage from originalNameUsage2 if it's empty
    originalNameUsage %in% c("", NA) & originalNameUsage1 != "",
    originalNameUsage := originalNameUsage2
  ]
  
  # delete old columns
  dataset$Taxon <- dataset$originalNameUsage
  dataset[, c(
    "DateNaturalisation", "FirstRecord", "FirstRecord1", "FirstRecord2",
    "FirstRecord_intentional", "originalNameUsage1", "originalNameUsage2"
  ) := NULL]
  cat("\nStep 1 completed: main dataset 'fr_main_dataset' ready to be processed\n ") 
  if (save_to_disk == TRUE){
  fwrite(dataset, "tmp/fr_main_dataset_step1.csv")
  }
  return(dataset)
  }