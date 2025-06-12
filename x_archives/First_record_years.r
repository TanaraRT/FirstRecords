##
## Harmonizing the years for the First Records database
##

### PREPARE WORKSPACE AND FILES ###

# Clean work space
graphics.off()
rm(list = ls())

# Call libraries
library(Hmisc)
library(stringr)
library(stringi)
library(data.table)

# Import data (dynamic)
directory <- system("pwd", intern = TRUE)
Raw_data <- file.path(directory, "data", "raw", "IntroData_raw.csv")
IntroData <- read.csv2(Raw_data, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
setDT(IntroData) # Convert to data.table

# Select relevant columns
FR_years_0 <- IntroData[, .(FirstRecord1, FirstRecord2, FirstRecord, DateNaturalisation)]

# Delete empty rows

## Replace NA and "NULL", "unknown", "n.d" and "?" with empty strings
FR_years_0[is.na(FR_years_0)] <- ""
FR_years_0[, names(FR_years_0) := lapply(.SD, function(x) {
    if (is.character(x)) {
    x <- gsub("(?i)null", "", x, perl = TRUE)
    x <- gsub("\\.", "", x)
    x <- gsub("(?i)unknown", "", x, perl = TRUE)
    x <- gsub("\\?", "", x, perl = TRUE)
    return(x)
    } else {return(x)}})] 

## Keep rows where at least one column is not empty
FR_years <- FR_years_0[FirstRecord1 != "" | FirstRecord2 != "" |
FirstRecord != "" | DateNaturalisation != ""] # Delete empty rows

# Store verbatim values in verbatim_fr
## Initialize verbatim_fr with FirstRecord
FR_years[, verbatim_fr := FirstRecord]
## Update based on priority if verbatim_fr is empty or NA
FR_years[verbatim_fr %in% c("", NA) & FirstRecord1 != "", verbatim_fr := FirstRecord1]
FR_years[verbatim_fr %in% c("", NA) & FirstRecord2 != "", verbatim_fr := FirstRecord2]
FR_years[verbatim_fr %in% c("", NA) & DateNaturalisation != "", verbatim_fr := DateNaturalisation]


# Prepare FirstRecord column

## If both FirstRecord1 and FirstRecord2 are present → paste as range
has_range <- FR_years$FirstRecord1 != "" & FR_years$FirstRecord2 != "" & FR_years$FirstRecord1 != FR_years$FirstRecord2
FR_years[has_range, verbatim_fr := paste0(FirstRecord1, "-", FirstRecord2)]

## FirstRecord1 has 2 years and FirstRecord2 is empty
FR_years[
  grepl("\\b\\d{4}\\b[ /]+\\b\\d{4}\\b", FirstRecord1) | grepl("^\\d{8}$", FirstRecord1),
  FirstRecord := ifelse(
    grepl("^\\d{8}$", FirstRecord1),
    paste0(substr(FirstRecord1, 1, 4), "-", substr(FirstRecord1, 5, 8)),
    gsub("[ /]+", "-", FirstRecord1)
  )
]

## FirstRecord2 has 2 years and FirstRecord1 is empty
FR_years[
  grepl("\\b\\d{4}\\b[ /]+\\b\\d{4}\\b", FirstRecord2) | grepl("^\\d{8}$", FirstRecord2),
  FirstRecord := ifelse(
    grepl("^\\d{8}$", FirstRecord2),
    paste0(substr(FirstRecord2, 1, 4), "-", substr(FirstRecord2, 5, 8)),
    gsub("[ /]+", "-", FirstRecord2)
  )
]

## Else assign in priority order: FirstRecord1 > FirstRecord2 > DateNaturalisation
FR_years[verbatim_fr == "" & FirstRecord1 != "", verbatim_fr := FirstRecord1]
FR_years[verbatim_fr == "" & FirstRecord2 != "", verbatim_fr := FirstRecord2]
FR_years[verbatim_fr == "" & DateNaturalisation != "", verbatim_fr := DateNaturalisation]

## Delete empty rows
FR_years <- FR_years[verbatim_fr != " "]

## Final assignment to FirstRecord
FR_years[, FirstRecord := verbatim_fr]

# Remove columns as they are not needed anymore
FR_years[, c("DateNaturalisation", "FirstRecord1", "FirstRecord2") := NULL]

# Write output
output_file <- file.path(directory, "outputs", "FR_years_pre-process.csv") # determine path
write.csv(FR_years, output_file, row.names = FALSE, fileEncoding = "UTF-8")# generate table

####################################################################################

### BASIC DATA CLEANING ###

# Remove spaces, commas and replace capital letters
FR_years$FirstRecord <- gsub(",", "", FR_years$FirstRecord)     # remove commas
FR_years$FirstRecord <- gsub("\\s+", " ", FR_years$FirstRecord) # remove multiple spaces
FR_years$FirstRecord <- trimws(FR_years$FirstRecord)            # trim whitespace
FR_years$FirstRecord <- tolower(FR_years$FirstRecord)           # replaces capital letters
FR_years$FirstRecord <- str_replace_all(FR_years$FirstRecord, "\\p{Zs}|\\s+", " ") # Normalize whitespace

# Normalize known number words 
 word_to_number <- c("half a" = "0.5", "one"    = "1", "two"    = "2", "three"  = "3", "four"   = "4", "five"   = "5", "six" = "6", "seven" = "7", "eight" = "8", "nine" = "9", "ten" = "10", "eleven" = "11", "twelve" = "12", "thirteen" = "13", "fourteen" = "14", "fifteen" = "15", "sixteen" = "16", "seventeen" = "17", "eighteen" = "18", "nineteen" = "19", "twenty" = "20")
for (word in names(word_to_number)) {
    pattern <- paste0("\\b", gsub(" ", "\\\\s+", word), "\\b")
    FR_years$FirstRecord <- gsub(pattern, word_to_number[[word]], FR_years$FirstRecord, ignore.case = TRUE)} 

# Replace full dates with years
pattern <- paste0(
  "\\b\\d{1,2}(st|nd|rd|th)?\\s+",
  "(january|february|march|april|may|june|july|august|september|october|november|december)",
  "\\s+(\\d{4})\\b"
)
FR_years$FirstRecord <- gsub(pattern, "\\3", FR_years$FirstRecord, ignore.case = TRUE)

# Remove months
remove_months <- c(
  "january" = "", "february" = "", "march" = "", "april" = "", "may" = "",
  "june" = "", "july" = "", "august" = "", "september" = "", "october" = "", "november" = "", "december" = ""
)
for (month in names(remove_months)) {
  pattern <- paste0("\\b", gsub(" ", "\\\\s+", month), "\\b")
  FR_years$FirstRecord <- gsub(pattern, remove_months[[month]], FR_years$FirstRecord, ignore.case = TRUE)
}

# Standardize years with 2 digits

## In year ranges
### Years 1900s
FR_years$FirstRecord <- str_replace_all(
  FR_years$FirstRecord,
  "(\\b19\\d{2})[-–](\\d{2})\\b",
  function(x) {
    parts <- str_match(x, "(19\\d{2})[-–](\\d{2})")
    paste0(parts[2], "-", "19", parts[3])
  }
)
### Years 2000s
FR_years$FirstRecord <- str_replace_all(
  FR_years$FirstRecord,
  "(\\b20\\d{2})[-–](\\d{2})\\b",
  function(x) {
    parts <- str_match(x, "(20\\d{2})[-–](\\d{2})")
    paste0(parts[2], "-", "20", parts[3])
  }
)

## For standalone 2-digit years (avoiding "15th century")
FR_years$FirstRecord <- str_replace_all(
  FR_years$FirstRecord,
  "(?<!\\d)(\\d{2})(?!\\d|\\s*(st|nd|rd|th)\\s+(century|c|cen))",
  function(x) {
    year <- str_match(x, "(\\d{2})")[1, 2]
    paste0("19", year)
    year <- as.integer(str_match(x, "(\\d{2})")[1, 2])
  }
)

# Handle BC years
FR_years$FirstRecord <- str_replace_all(
  FR_years$FirstRecord,
  # Match formats like "500 BC", "BC 500", "500 (BC)", "(BC) 500"
  regex("\\(?\\bBC\\b\\)?\\s*(\\d{3,4})|\\b(\\d{3,4})\\b\\s*\\(?\\bBC\\b\\)?", ignore_case = TRUE),
  function(x) {
    # Extract digits
    year <- as.integer(str_extract(x, "\\d{3,4}"))
    # Return negative year
    paste0("-", year)
  }
)

print ("Basic data cleaning done")

####################################################################################

### QUALITY CONTROL ###

mark_confidence <- function(dt) {
  # High confidence: full year 
dt[grepl("^\\d{4}$", FirstRecord),
   eventRemarks := ifelse(as.numeric(FirstRecord) >= 1500, 
                          "high confidence", "low confidence")]
  # Medium-high confidence: decades 
  dt[grepl("^\\d{4}s$|^\\d{2}s$", FirstRecord), eventRemarks := "medium-high confidence"]
# Identify year ranges using "-" or "or"
range_rows <- grepl("^\\d{4}\\s*[-oO]{1,2}r\\s*\\d{4}$", dt$FirstRecord)
if (any(range_rows)) {
  cleaned <- gsub("(?i)\\s*or\\s*", "-", dt$FirstRecord[range_rows])  # Normalize "or" to "-"
  cleaned <- gsub("\\s+", "", cleaned)  # Remove spaces
  split_result <- tstrsplit(cleaned, "-", fixed = TRUE)
  
  if (length(split_result) == 2) {
    dt[range_rows, `:=`(
      range_start = as.numeric(split_result[[1]]),
      range_end   = as.numeric(split_result[[2]])
    )]
    
    dt[range_rows, range_width := abs(range_end - range_start)]
    
    # Assign confidence levels
    dt[range_rows & range_width <= 9,  eventRemarks := "medium-high confidence"]
    dt[range_rows & range_width >= 10 & range_width <= 15, eventRemarks := "medium confidence"]
    dt[range_rows & range_width >= 16 & range_width <= 20, eventRemarks := "medium-low confidence"]
    dt[range_rows & range_width > 20,  eventRemarks := "low confidence"]
    
    # Clean up
    dt[, c("range_start", "range_end", "range_width") := NULL]
  }
}

  # Remaining missing/empty remarks → low confidence
  dt[is.na(eventRemarks) | eventRemarks == "", eventRemarks := "low confidence"]
  return(dt)
}
FR_years <- mark_confidence(FR_years)

print ("Confidence assigned")

####################################################################################

### Handling decades ###
ind <- grep("0s",FR_years$FirstRecord)
for (i in ind){
  FR_years[i, FirstRecord := gsub("0s", sample(0:9, 1), FirstRecord, perl = TRUE)]
FR_years$FirstRecord <- gsub("\t","",FR_years$FirstRecord)
FR_years$FirstRecord <- gsub("\\([0-9]*\\)","",FR_years$FirstRecord,perl=TRUE)
}
print ("decades handled")
####################################################################################

########################################################################################

### CHECK ### 

# Pattern to match: "xxx" or "xxxx" (numbers only), "-xxxx" (negative years), "xxxx - xxxx" or "xxxx-xxxx" (year ranges)
pattern <- "^\\d{3,4}$|^-\\d{4}$|^\\d{4}\\s*-\\s*\\d{4}$"

# Filter rows that DO NOT match this pattern
non_matching_rows <- FR_years[!grepl(pattern, FirstRecord)]
tmp_file <- file.path(directory, "tmp", "FirstRecord_non_matching_formats1.csv")
fwrite(non_matching_rows, tmp_file)

#######################################################################################

### HANDLING "OR" ###
# Match ranges, including negative years and multiple dashes
range_pattern <- "^\\s*(-?\\d+)\\s*or\\s*(-?\\d+)\\s*$"
range_rows <- grepl(range_pattern, FR_years$FirstRecord)

if (any(range_rows)) {
  # Extract start and end years from pattern
  ranges <- str_match(FR_years$FirstRecord[range_rows], range_pattern)
  start_years <- as.integer(ranges[, 2])
  end_years <- as.integer(ranges[, 3])

  # Ensure correct ordering (start <= end)
  ordered <- mapply(function(start, end) {
    if (start > end) c(end, start) else c(start, end)
  }, start_years, end_years)

  start_years <- as.integer(ordered[1, ])
  end_years <- as.integer(ordered[2, ])

  # Range widths
  range_widths <- end_years - start_years

  # Assign the end year (most recent) instead of random
  FR_years$FirstRecord[range_rows] <- as.character(end_years)

  # Confidence tagging
  confidences <- sapply(range_widths, function(width) {
    if (width <= 9) {
      "medium-high confidence"
    } else if (width <= 15) {
      "medium confidence"
    } else if (width <= 20) {
      "medium-low confidence"
    } else {
      "low confidence"
    }
  })

  # Assign to eventRemarks
  FR_years$eventRemarks[range_rows] <- confidences
}

print(" OR handled")

####################################################################################

### HANDLING "AND" ###
# Match ranges, including negative years and multiple dashes
range_pattern <- "^\\s*(-?\\d+)\\s*(?:and|&)\\s*(-?\\d+)\\s*$"
range_rows <- grepl(range_pattern, FR_years$FirstRecord)

if (any(range_rows)) {
  # Extract start and end years from pattern
  ranges <- str_match(FR_years$FirstRecord[range_rows], range_pattern)
  start_years <- as.integer(ranges[, 2])
  end_years <- as.integer(ranges[, 3])

  # Ensure correct ordering (start <= end)
  ordered <- mapply(function(start, end) {
    if (start > end) c(end, start) else c(start, end)
  }, start_years, end_years)

  start_years <- as.integer(ordered[1, ])
  end_years <- as.integer(ordered[2, ])

  # Range widths
  range_widths <- end_years - start_years

  # Assign the end year (most recent) instead of random
  FR_years$FirstRecord[range_rows] <- as.character(start_years)

  # Confidence tagging
  confidences <- sapply(range_widths, function(width) {
    if (width <= 9) {
      "medium-high confidence"
    } else if (width <= 15) {
      "medium confidence"
    } else if (width <= 20) {
      "medium-low confidence"
    } else {
      "low confidence"
    }
  })

  # Assign to eventRemarks
  FR_years$eventRemarks[range_rows] <- confidences
}

print("AND handled")
####################################################################################

### HANDLING RANGES ###

# Match ranges, including negative years and multiple dashes
range_pattern <- "^.*?(-?\\d{1,4})\\D+(-?\\d{1,4}).*?$"
range_rows <- grepl(range_pattern, FR_years$FirstRecord)

if (any(range_rows)) {
  ranges <- str_match(FR_years$FirstRecord[range_rows], range_pattern)
  start_years <- as.integer(ranges[, 2])
  end_years <- as.integer(ranges[, 3]) # Extract start and end years
  
  ordered <- mapply(function(start, end) {
    if (start > end) c(end, start) else c(start, end)
  }, start_years, end_years) # Order start/end properly

  start_years <- as.integer(ordered[1, ])
  end_years <- as.integer(ordered[2, ])
  
  range_widths <- end_years - start_years
  
  random_years <- mapply(function(start, end) sample(seq(start, end), 1), start_years, end_years)   # Sample years from the range
  
  confidences <- sapply(range_widths, function(width) {
    if (width <= 9) {
      "medium-high confidence"
    } else if (width <= 15) {
      "medium confidence"
    } else if (width <= 20) {
      "medium-low confidence"
    } else {
      "low confidence"
    }
  }) # Apply your confidence rules
  
  
  FR_years$FirstRecord[range_rows] <- as.character(random_years)
  FR_years$eventRemarks[range_rows] <- confidences # Update the data
}

print ("Ranges handled")
####################################################################################

### HANDLING "YEARS/DECADES/CENTURIES AGO" ###
#Reference_year <- 2020 # Define reference year
#time_units <- list("years? ago" = 1, "decades? ago" = 10, "centur(?:y|ies) ago" = 100) # Define time units and their multipliers (in years)

# Loop through units and process them
#for (pattern in names(time_units)) {
#    full_pattern <- paste0("([\\d]+(?:\\.\\d+)?)\\s*(?:or more\\s*)?", pattern)
#    matches <- str_match(FR_years$FirstRecord, full_pattern)
#    has_match <- !is.na(matches[, 1]);
#if (any(has_match)) {
#    value <- as.numeric(matches[has_match, 2]) * time_units[[pattern]]
#    FR_years$FirstRecord[has_match] <- as.character(Reference_year - value)
#    }}

#print ("years ago handled")
####################################################################################

## Handling "mid", "early" and "late" centuries or decades ##

# Assign  years to "mid", "early" and "late" 
convert_mid_decade_random <- function(text) {
  pattern <- regex("(early|mid|middle|late)[- ]?(\\d{3})0s", ignore_case = TRUE)
  matches <- str_match(text, pattern)
  if (!is.na(matches[1,1])) {
    decade_start <- as.numeric(matches[1,3]) * 10
    offset <- switch(matches [1,2], early = 2, mid   = 5, middle = 5, late  = 8, 17)
    return(decade_start + offset)}
  return(NA)}  # return NA if no match


convert_mid_century_random <- function(text) {
  pattern <- regex("(early|mid| middle| late)[- ]?(\\d{1,2})(?:th|st|nd|rd)? century", ignore_case = TRUE)
  matches <- str_match(text, pattern)
  if (!is.na(matches[1,1])) {
    century_num <- as.numeric(matches[1,3])
    base_year <- (century_num - 1) * 100
    offset <- switch(matches [1,2], early = 20, mid   = 50, middle =50, late  = 80, 17)
    return(base_year + offset)}
  return(NA)}
convert_partial_century <- function(text) {
  pattern <- regex("(early|mid|late)[ -]?(\\d{1,2})(?:th|st|nd|rd)?(?!\\s*century)", ignore_case = TRUE)
  matches <- str_match(text, pattern)
  
  if (!is.na(matches[1,1])) {
    century_num <- as.numeric(matches[1,3])
    base_year <- (century_num - 1) * 100
    
    offset <- switch(tolower(matches[1,2]),
                     "early" = 20,
                     "mid"   = 50,
                     "late"  = 80,
                     17)
    return(base_year + offset)
  }
  
  return(NA)
}

# Apply to FirstRecord 
mid_century_years <- sapply(FR_years$FirstRecord, convert_mid_century_random)
inds <- which(!is.na(mid_century_years))
FR_years$FirstRecord[inds] <- as.character(mid_century_years[inds])

mid_decade_years <- sapply(FR_years$FirstRecord, convert_mid_decade_random)
inds <- which(!is.na(mid_decade_years))
FR_years$FirstRecord[inds] <- as.character(mid_decade_years[inds])

partial_century_years <- sapply(FR_years$FirstRecord, convert_partial_century)
inds <- which(!is.na(partial_century_years))
FR_years$FirstRecord[inds] <- as.character(partial_century_years[inds])

print ("early mid-, late handled")
####################################################################################

### Handling centuries ###
convert_century_random <- function(text) {
  pattern <- regex("\\b(?:circa|ca|c\\.?|early|mid|late)?\\s*(\\d{1,2})(?:st|nd|rd|th)?\\s*(century|c|cen)\\b", ignore_case = TRUE)
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

print ("centuries handled")

### Handling "after" ###

# Updated convert_after function with consistent confidence
convert_after <- function(text) {
  pattern <- regex("(?i)(?:after|post)[ -]?(\\d{4})(?:\\s+or\\s+later)?\\b")
  matches <- str_match(text, pattern)
  
  if (!is.na(matches[, 2])) {
    year <- as.numeric(matches[, 2])
    new_year <- year + 2
    confidence <- if (year %% 10 != 0) "medium confidence" else "low confidence"
    return(list(year = as.character(new_year), confidence = confidence))
  }
  
  return(NULL)
}

# Updated convert_circa with confidence handling
# convert_circa <- function(text) {
#   text_norm <- gsub("[~≈∼∾〜]", "~", text, perl = TRUE)
  
#   # Helper to check if year ends with 0
#   ends_with_zero <- function(year) {
#     substr(as.character(year), nchar(as.character(year)), nchar(as.character(year))) == "0"
#   }
  
  # Match circa keywords with year
  #m_circa <- str_match(text_norm, "(?i)\\b(?:circa|approx|around|about|ca)\\s*(\\d{3,4})\\b")
  #if (!is.na(m_circa[1,2])) {
  #  year <- as.numeric(m_circa[1,2])
  #  offset <- sample(-5:5, 1)
  #  new_year <- year + offset
  #  new_year <- max(min(new_year, 2100), 1000)
    
  #  confidence <- if (ends_with_zero(year)) "low confidence" else "medium confidence"
  #  return(list(year = as.character(new_year), confidence = confidence))
  #}
  
  # Match year with tilde after
  #m_tilde <- str_match(text_norm, "\\b(\\d{3,4})\\s*[~*]")
  #if (!is.na(m_tilde[1,2])) {
  #  year <- as.numeric(m_tilde[1,2])
  #  offset <- sample(-5:5, 1)
  #  new_year <- year + offset
  #  new_year <- max(min(new_year, 2100), 1000)
    
  #  confidence <- if (ends_with_zero(year)) "low confidence" else "medium confidence"
  #  return(list(year = as.character(new_year), confidence = confidence))
  #}
  
  #return(NULL) # no match
#}

# Get different random offset per row
#result <- FR_years[, {
#  res <- convert_circa(FirstRecord)
#  if (!is.null(res)) {
#    .(FirstRecord_new = res$year, eventRemarks_new = res$confidence)
#  } else {
#    .(FirstRecord_new = FirstRecord, eventRemarks_new = eventRemarks)
#  }
#}, by = seq_len(nrow(FR_years))]

# Replace columns with new results
#FR_years[, `:=`(FirstRecord = result$FirstRecord_new, eventRemarks = result$eventRemarks_new)]

# Apply convert_after to entire FirstRecord column
#converted_after <- lapply(FR_years$FirstRecord, convert_after)
#new_years_after <- sapply(converted_after, function(x) if (!is.null(x)) x$year else NA)
#confidences_after <- sapply(converted_after, function(x) if (!is.null(x)) x$confidence else NA)

#FR_years$FirstRecord[!is.na(new_years_after)] <- new_years_after[!is.na(new_years_after)]
#FR_years$eventRemarks[!is.na(confidences_after)] <- confidences_after[!is.na(confidences_after)]

# Identify rows with circa-like patterns
# circa_rows <- grepl("(?i)(circa|approx|around|about|ca|~|≈|*)\\s*\\d{3,4}", FR_years$FirstRecord)

# Apply convert_circa to those rows using data.table syntax
# FR_years[circa_rows, c("FirstRecord", "eventRemarks") := {
#   res <- convert_circa(trimws(FirstRecord))
  
#   new_year <- if (!is.null(res)) res$year else FirstRecord
#   new_conf <- if (!is.null(res$confidence)) res$confidence else eventRemarks
  
# list(new_year, new_conf)
# }, by = .I]

print ("after handled")
####################################################################################

### Final cleaning ###
clean_first_record <- function(dt) {
  dt[, FirstRecord := gsub("[^0-9\\-]", "", FirstRecord)]  # Keep only digits and hyphens
  dt[, FirstRecord := gsub("(?<!\\d)(\\d{1,2})(?!\\d)", "", FirstRecord, perl = TRUE)]  # Remove 1–2 digit numbers
  dt[, FirstRecord := gsub("-{2,}", "-", FirstRecord)]  # Replace multiple hyphens with a single one
  dt[, FirstRecord := gsub("-+$", "", FirstRecord)]  # Remove trailing hyphens

}
clean_first_record(FR_years)


########################################################################################

### CHECK ### 

# Pattern to match: "xxx" or "xxxx" (numbers only), "-xxxx" (negative years), "xxxx - xxxx" or "xxxx-xxxx" (year ranges)
pattern <- "^\\d{3,4}$|^-\\d{4}$|^\\d{4}\\s*-\\s*\\d{4}$"

# Filter rows that DO NOT match this pattern
non_matching_rows <- FR_years[!grepl(pattern, FirstRecord)]
tmp_file <- file.path(directory, "tmp", "FirstRecord_non_matching_formats.csv")
fwrite(non_matching_rows, tmp_file)

#######################################################################################

### FINALIZE ###

output_file <- file.path(directory, "outputs", "first_record_years.csv")
write.csv(FR_years, output_file, row.names = FALSE, fileEncoding = "UTF-8")