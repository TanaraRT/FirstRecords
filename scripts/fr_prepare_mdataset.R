##########################################################################
##                                                                      ##
##                       FIRST RECORDS WORKFLOW                         ##
##                       Prepare master dataset                         ##
##                   -----------------------------                      ##
##                                                                      ##
## H. Seebens, T. Renard Truong                                         ##
## vx.x, 2025                                                           ##
##########################################################################

# Organization:
#  Main dataset: locality | locationID | taxon| scientificName | taxonID | 
# firstRecord | verbatimFirstRecordEvent | confidenceFirstRecord | occurenceStatus | 
#establishmentMeans | degreeOfEstablishment| datasetName | bibliographicCitation

# Taxonomy: taxonID | scientificName |scientificNameAuthorship| originalNameUsage | taxonRemarks | family | order | class | phylum | habitat |

# Location: location ID| locality | country | continent

fr_prepare_mdataset <- function (fr_raw_data){
  # prepare master dataset
  fr_mdataset <- fr_raw_data[, .(
    locationID = "",
    locality = Country,
    country = "",
    continent = "",
    originalNameUsage = "",
    originalNameUsage1 = GenusSpecies,
    originalNameUsage2 = paste(Genus, Species, Author),
    scientificNameAuthorship = Author,
    taxonRemarks = LifeForm,
    family = Family,
    order = Order,
    class = Class,
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
  fr_mdataset[is.na(fr_mdataset)] <- ""
  fr_mdataset[, names(fr_mdataset) := lapply(.SD, function(x) {
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
  fr_mdataset <- fr_mdataset[
    rowSums(!(is.na(fr_mdataset) | fr_mdataset == "")) > 0
  ]
  
  # Store original fr values in verbatimFirstRecordEvent
  fr_mdataset[, verbatimFirstRecordEvent := FirstRecord] # Initialize verbatimFirstRecordEvent with FirstRecord
  fr_mdataset[ # Assign verbatimFirstRecordEvent from FirstRecord1 if it's empty
    verbatimFirstRecordEvent %in% c("", NA) & FirstRecord1 != "",
    verbatimFirstRecordEvent := FirstRecord1
  ]
  fr_mdataset[ # Assign verbatimFirstRecordEvent from FirstRecord2 if it's empty
    verbatimFirstRecordEvent %in% c("", NA) & FirstRecord2 != "",
    verbatimFirstRecordEvent := FirstRecord2
  ]
  fr_mdataset[ # Assign verbatimFirstRecordEvent from DateNaturalisation if it's empty
    verbatimFirstRecordEvent %in% c("", NA) & DateNaturalisation != "",
    verbatimFirstRecordEvent := DateNaturalisation
  ]
  fr_mdataset[ # Assign verbatimFirstRecordEvent from DateNaturalisation if it's empty
    verbatimFirstRecordEvent %in% c("", NA) & FirstRecord_intentional != "",
    verbatimFirstRecordEvent := FirstRecord_intentional
  ]
  fr_mdataset[ # Assign verbatimFirstRecordEvent 
    FirstRecord1 != "" & FirstRecord2 != "",
    verbatimFirstRecordEvent := paste(FirstRecord1, FirstRecord2, sep = " - ")
  ]
  
  # Prepare firstRecordEvent and confidenceFirstRecordEvent columns
  fr_mdataset[, firstRecordEvent := verbatimFirstRecordEvent] # Initialize firstRecordEvent with verbatimFirstRecordEvent
  fr_mdataset[, confidenceFirstRecordEvent := "low confidence"] # Initialize with low confidence
  
  # Store original species names in originalNameUsage and create new columns
  fr_mdataset[, originalNameUsage := originalNameUsage1] # Initialize originalNameUsage with originalNameUsage1
  fr_mdataset[ # Assign originalNameUsage from originalNameUsage2 if it's empty
    originalNameUsage %in% c("", NA) & originalNameUsage1 != "",
    originalNameUsage := originalNameUsage2
  ]
  
  # delete old columns
  fr_mdataset[, c(
    "DateNaturalisation", "FirstRecord", "FirstRecord1", "FirstRecord2",
    "FirstRecord_intentional", "originalNameUsage1", "originalNameUsage2"
  ) := NULL]
  
  cat("\nStep 1 completed: workspace prepared and data extracted\n ") 
  return(fr_mdataset)
}