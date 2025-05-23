##
## Harmonizing the years for the First Records database
##

## Prepare work space and file ################################################

### Clean work space
graphics.off()
rm(list = ls())

### Call libraries
library(Hmisc)
library(stringr)
library(stringi)
library(data.table)

### Dynamic import of data
directory <- system("pwd", intern = TRUE) # Get the current working directory
raw_data <- file.path(directory, "Raw_data", "IntroData_raw.csv")
raw_data <- read.csv2(raw_data,
                      fileEncoding = "UTF-8",
                      stringsAsFactors = FALSE) |>
  setDT(
  ) # Convert to data.table

### Select relevant columns
fr_years <- raw_data[, .(FirstRecord1,
                         FirstRecord2,
                         FirstRecord,
                         DateNaturalisation)]

### Delete empty rows
fr_years[is.na(fr_years)] <- "" # Replace NA with empty strings
fr_years[, names(fr_years) := lapply(.SD,
                                     function(x) {
                                       if (is.character(x)) {
                                         x <- gsub("(?i)null", "", x, perl = TRUE)
                                         x <- gsub("(?i)unknown", "", x,
                                                   perl = TRUE)
                                       }
                                       return(x)
                                     })
] #Replace "NULL" and "unknown" with empty strings
fr_years <- fr_years[FirstRecord1 != "" | # Delete empty rows
    FirstRecord2 != "" |
    FirstRecord != "" |
    DateNaturalisation != ""
]

### Store original values in verbatimEventDate (vED)
fr_years$verbatimEventDate <- fr_years$FirstRecord
has_range <- fr_years$FirstRecord1 != "" & # Determine if there is a range
  fr_years$FirstRecord2 != "" &
  fr_years$FirstRecord1 != fr_years$FirstRecord2
fr_years[has_range, verbatimEventDate :=
           paste0(FirstRecord1, "-", FirstRecord2)] # Assign range to vED
# Else assign in priority order: FirstRecord1>FirstRecord2>DateNaturalisation
fr_years[verbatimEventDate == "" & FirstRecord1 != "",
         verbatimEventDate := FirstRecord1]
fr_years[verbatimEventDate == "" & FirstRecord2 != "",
         verbatimEventDate := FirstRecord2]
fr_years[verbatimEventDate == "" & DateNaturalisation != "",
         verbatimEventDate := DateNaturalisation]

### Prepare FirstRecord
fr_years[, FirstRecord := verbatimEventDate]

### Remove columns as they are not needed anymore
fr_years[, c("DateNaturalisation", "FirstRecord1", "FirstRecord2") := NULL]

### Write output
output <- file.path(directory, "Years_standard", "FR_years_pre-process.csv")
write.csv(fr_years, output, row.names = FALSE, fileEncoding = "UTF-8")

## Initial data cleaning #######################################
file_path <- file.path(directory, "Years_standard", "initial_cleaning_years.r")
source(file_path)
fr_years <- initial_cleaning_years(fr_years, "FirstRecord")
message("Initial cleaning of the FirstRecord data done")

## First quality control: eventRemarks columns ##############################
file_path <- file.path(directory, "Years_standard", "quality_control.r")
source(file_path)
fr_years <- quality_control(fr_years, "FirstRecord")
message("First quality control of the FirstRecord data done")








output_file <- file.path(directory, "Outputs", "FR_years_test.csv")
write.csv(fr_years, output_file, row.names = FALSE, fileEncoding = "UTF-8")
