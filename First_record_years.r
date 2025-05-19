##
## Harmonizing the years for the First Records database
##

## Prepare work space and file ###################################################
### Clean work space
graphics.off()
rm(list=ls())
### Call libraries
library(Hmisc)
library(stringr)
library(stringi)
library(data.table)
### Import data (dynamic)
directory <- system("pwd", intern = TRUE)
Raw_data <- file.path(directory, "Raw_data", "IntroData_raw.csv")
IntroData <- read.csv2(Raw_data, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
setDT(IntroData) # Convert to data.table
##Select relevant columns
FR_years_0<- IntroData[, .(FirstRecord1, FirstRecord2, FirstRecord, DateNaturalisation)] # 
### Replace NA and "NULL" and "unknown" with empty strings
FR_years_0[is.na(FR_years_0)] <- ""
FR_years_0[, names(FR_years_0) := lapply(.SD, function(x) {
  if (is.character(x)) {
    x <- gsub("(?i)null", "", x, perl = TRUE)
    x <- gsub("(?i)unknown", "", x, perl = TRUE)
    return(x)}
    else {return(x)}})]
### Keep rows where at least one column is not empty
FR_years <- FR_years_0[FirstRecord1 != "" | FirstRecord2 != "" | 
FirstRecord != "" | DateNaturalisation != ""] # Delete empty rows
### Store original valyes in verbatimEventDate
FR_years$verbatimEventDate <- FR_years$FirstRecord 
#### If both FirstRecord1 and FirstRecord2 are present → paste as range
has_range <- FR_years$FirstRecord1 != "" & FR_years$FirstRecord2 != "" & FR_years$FirstRecord1 != FR_years$FirstRecord2
FR_years[has_range, verbatimEventDate := paste0(FirstRecord1, "-", FirstRecord2)]
### Else assign in priority order: FirstRecord1 > FirstRecord2 > DateNaturalisation
FR_years[verbatimEventDate == "" & FirstRecord1 != "", verbatimEventDate := FirstRecord1]
FR_years[verbatimEventDate == "" & FirstRecord2 != "", verbatimEventDate := FirstRecord2]
FR_years[verbatimEventDate == "" & DateNaturalisation != "", verbatimEventDate := DateNaturalisation]
### Final assignment to FirstRecord
FR_years[, FirstRecord := verbatimEventDate]
### Remove columns as they are not needed anymore
FR_years[, c("DateNaturalisation", "FirstRecord1", "FirstRecord2") := NULL]
### Write output
Output_file <- file.path(directory, "Outputs", "FR_years_pre-process.csv") # determine path
write.csv(FR_years, Output_file, row.names = FALSE, fileEncoding = "UTF-8")# generate table

## Basic data cleaning ##############################################################
### Remove spaces, commas and replace capital letters
FR_years$FirstRecord <- gsub(",", "", FR_years$FirstRecord)     # remove commas
FR_years$FirstRecord <- gsub("\\s+", " ", FR_years$FirstRecord) # remove multiple spaces
FR_years$FirstRecord <- trimws(FR_years$FirstRecord)            # trim whitespace
FR_years$FirstRecord <- tolower(FR_years$FirstRecord)           # replaces capital letters
### Normalize known number words 
 word_to_number <- c("half a" = "0.5", "one"    = "1", "two"    = "2", "three"  = "3", "four"   = "4", "five"   = "5", "six" = "6", "seven" = "7", "eight" = "8", "nine" = "9", "ten" = "10", "eleven" = "11", "twelve" = "12", "thirteen" = "13", "fourteen" = "14", "fifteen" = "15", "sixteen" = "16", "seventeen" = "17", "eighteen" = "18", "nineteen" = "19", "twenty" = "20") 
for (word in names(word_to_number)) {
    pattern <- paste0("\\b", gsub(" ", "\\\\s+", word), "\\b")
    FR_years$FirstRecord <- gsub(pattern, word_to_number[[word]], FR_years$FirstRecord, ignore.case = TRUE)} 

## Start quality control ##############################################################
mark_high_confidence <- function(dt) {
year_pattern <- "^\\d{4}$"
dt[grepl(year_pattern, FirstRecord), eventRemarks := "high confidence"]}
FR_years <- mark_high_confidence(FR_years)

## Handing "years/decade/centuries ago" ######################################################
Reference_year <- 2020 # Define reference year
time_units <- list("years? ago" = 1, "decades? ago" = 10, "centur(?:y|ies) ago" = 100) # Define time units and their multipliers (in years)
### Loop through units and process them
for (pattern in names(time_units)) {
    full_pattern <- paste0("([\\d]+(?:\\.\\d+)?)\\s*(?:or more\\s*)?", pattern)
    matches <- str_match(FR_years$FirstRecord, full_pattern)
    has_match <- !is.na(matches[, 1]);
if (any(has_match)) {
    value <- as.numeric(matches[has_match, 2]) * time_units[[pattern]]
    FR_years$FirstRecord[has_match] <- as.character(Reference_year - value)
    FR_years$eventRemarks[has_match & value >= 0 & value <= 10] <- "medium-high confidence"
    FR_years$eventRemarks[has_match & value >= 11 & value <= 30] <- "medium confidence"
    FR_years$eventRemarks[has_match & value > 31] <- "low confidence"
    }}

## Handling "mid", "early" and "late" centuries or decades ###############################
### Assign random years to "mid", "early" and "late" 
convert_mid_decade_random <- function(text) {
  pattern <- regex("(early|mid|late)[- ]?(\\d{3})0s", ignore_case = TRUE)
  matches <- str_match(text, pattern)
  if (!is.na(matches[1,1])) {
    decade_start <- as.numeric(matches[1,3]) * 10
    offset <- switch(matches [1,2], early = sample(1:3, 1), mid   = sample(4:6, 1), late  = sample(7:9, 1), sample(4:6, 1))
    return(decade_start + offset)}
  return(NA)}  # return NA if no match
convert_mid_century_random <- function(text) {
  pattern <- regex("(early|mid|late)[- ]?(\\d{1,2})(?:th|st|nd|rd)? century", ignore_case = TRUE)
  matches <- str_match(text, pattern)
  if (!is.na(matches[1,1])) {
    century_num <- as.numeric(matches[1,3])
    base_year <- (century_num - 1) * 100
    offset <- switch(matches [1,2], early = sample(0:30, 1), mid   = sample(31:70, 1), late  = sample(71:99, 1), sample(31:70, 1))
    return(base_year + offset)}
  return(NA)}
  convert_partial_century <- function(text) {
  pattern <- regex("(early|mid|late)[ -]?(\\d{1,2})(?:th|st|nd|rd)?(?! century)", ignore_case = TRUE)
  matches <- str_match(text, pattern)
  if (!is.na(matches[1,1])) {"^\\d{4}$"
    century_num <- as.numeric(matches[1,3])
    base_year <- (century_num - 1) * 100
    offset <- switch(tolower(matches[1,2]),
                     "early" = sample(0:30, 1),
                     "mid"   = sample(31:70, 1),
                     "late"  = sample(71:99, 1),
                     sample(31:70, 1))
    return(base_year + offset)
  }
  return(NA)
}
### Apply to FirstRecord and add a confidence statement
mid_century_years <- sapply(FR_years$FirstRecord, convert_mid_century_random)
just_mid_century <- which(!is.na(mid_century_years))
FR_years$FirstRecord[just_mid_century] <- as.character(mid_century_years[just_mid_century])
record_age_mid <- Reference_year - mid_century_years[just_mid_century]
FR_years$eventRemarks[just_mid_century[record_age_mid <= 10]] <- "medium-high confidence"
FR_years$eventRemarks[just_mid_century[record_age_mid > 10 & record_age_mid <= 30]] <- "medium confidence"
FR_years$eventRemarks[just_mid_century[record_age_mid > 30]] <- "low confidence"

mid_decade_years <- sapply(FR_years$FirstRecord, convert_mid_decade_random)
just_mid_decade <- which(!is.na(mid_decade_years))
FR_years$FirstRecord[just_mid_decade] <- as.character(mid_decade_years[just_mid_decade])
record_age_decade <- Reference_year - mid_decade_years[just_mid_decade]
FR_years$eventRemarks[just_mid_decade[record_age_decade <= 10]] <- "medium-high confidence"
FR_years$eventRemarks[just_mid_decade[record_age_decade > 10 & record_age_decade <= 30]] <- "medium confidence"
FR_years$eventRemarks[just_mid_decade[record_age_decade > 30]] <- "low confidence"

partial_century_years <- sapply(FR_years$FirstRecord, convert_partial_century)
just_partial_century <- which(!is.na(partial_century_years))
FR_years$FirstRecord[just_partial_century] <- as.character(partial_century_years[just_partial_century])
record_age_partial <- Reference_year - partial_century_years[just_partial_century]
FR_years$eventRemarks[just_partial_century[record_age_partial <= 10]] <- "medium-high confidence"
FR_years$eventRemarks[just_partial_century[record_age_partial > 10 & record_age_partial <= 30]] <- "medium confidence"
FR_years$eventRemarks[just_partial_century[record_age_partial > 30]] <- "low confidence"

## Handling centuries ###########################################################
convert_century_random <- function(text) {
  pattern <- regex("^(\\d{1,2})(?:th|st|nd|rd)? century$", ignore_case = TRUE)
  matches <- str_match(text, pattern)
  if (!is.na(matches[1,1])) {
    century_num <- as.numeric(matches[1,2])
    base_year <- (century_num - 1) * 100
    offset <- sample(0:99, 1)  # random year within the century
    return(base_year + offset)
  }
  return(NA)
}
Norm_centuries <- sapply(FR_years$FirstRecord, convert_century_random) # Apply the function
FR_years$FirstRecord[!is.na(Norm_centuries)] <- as.character(Norm_centuries[!is.na(Norm_centuries)]) # Replace in $FirstRecord

##Handling decades #####################################################
ind <- grep("0s",FR_years$FirstRecord)
for (i in ind){
  FR_years[i, FirstRecord := gsub("0s", sample(0:9, 1), FirstRecord, perl = TRUE)]
  FR_years[i, eventRemarks := "medium-high confidence"]}
FR_years$FirstRecord <- gsub("\t","",FR_years$FirstRecord)
FR_years$FirstRecord <- gsub("\\([0-9]*\\)","",FR_years$FirstRecord,perl=TRUE)

## CHECK 
# Pattern to match:
# - "xxx" or "xxxx" (numbers only)
# - "-xxxx" (negative years)
# - "xxxx - xxxx" or "xxxx-xxxx" (year ranges)
pattern <- "^\\d{3,4}$|^-\\d{4}$|^\\d{4}\\s*-\\s*\\d{4}$"
# Filter rows that DO NOT match this pattern
non_matching_rows <- FR_years[!grepl(pattern, FirstRecord)]
fwrite(non_matching_rows, "FirstRecord_non_matching_formats.csv")


Output_file <- file.path(directory, "Outputs", "FR_years_test.csv")
write.csv(FR_years, Output_file, row.names = FALSE, fileEncoding = "UTF-8")









## Removing all text and special characters, except "-" 
#FR_years$FirstRecord <- gsub("[^0-9\\- ]", " ", FR_years$FirstRecord)

#FR_years[] <- lapply(FR_years, function(x) {
#  if (is.character(x)) {
#    sapply(x, function(cell) {
#      # Count digits in the cell
#      digit_count <- nchar(gsub("[^0-9]", "", cell))
#      if (digit_count == 4 | digit_count == 8| digit_count == 12| digit_count == 0) {
#        gsub("[^0-9\u002D\u2013\u2014]", "", cell, perl = TRUE)  # keep only digits and dashes
#      } else {
#        cell  # leave as is
#      }
#    })
#  } else {
#    x
#  }
#})

### Produce mid-process file
#write.table(FR_years, file.path(directory, "Outputs", "FR_years_mid-process.csv"), sep = ";", row.names = FALSE)
