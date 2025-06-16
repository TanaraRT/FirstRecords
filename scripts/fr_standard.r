##
## STANDARDIZATION OF FIRST RECORDS
##

## 1) PREPARE WORKSPACE AND DATA
cat("\nSTEP 1: PREPARE WORKSPACE AND DATA") 

# Call libraries
graphics.off()
rm(list = ls())

# Call libraries
library(Hmisc)
library(stringr)
library(stringi)
library(data.table)

# Import data
intro_data <- read.csv2(
  'data/raw/IntroData_raw.csv',
  fileEncoding = "UTF-8",
  stringsAsFactors = FALSE
)
setDT(intro_data) # Convert to data.table

# Extract relevant columns
fr_years_0 <- intro_data[, .(
  FirstRecord1,
  FirstRecord2,
  FirstRecord,
  DateNaturalisation,
  FirstRecord_intentional
)]

# Delete empty rows
## Replace NA and "NULL", "unknown", "n.d" and "?" with empty strings
fr_years_0[is.na(fr_years_0)] <- ""
fr_years_0[, names(fr_years_0) := lapply(.SD, function(x) {
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
## Delete rows where all columns are empty
fr_years <- fr_years_0[
  FirstRecord1 != "" |
    FirstRecord2 != "" |
    FirstRecord != "" |
    DateNaturalisation != "" |
    FirstRecord_intentional != ""
]

# Store verbatim values in verbatim_fr and create new columns
fr_years[, verbatim_fr := FirstRecord] # Initialize verbatim_fr with FirstRecord
fr_years[ # Assign verbatim_fr from FirstRecord1 if it's empty
  verbatim_fr %in% c("", NA) & FirstRecord1 != "",
  verbatim_fr := FirstRecord1
]
fr_years[ # Assign verbatim_fr from FirstRecord2 if it's empty
  verbatim_fr %in% c("", NA) & FirstRecord2 != "",
  verbatim_fr := FirstRecord2
]
fr_years[ # Assign verbatim_fr from DateNaturalisation if it's empty
  verbatim_fr %in% c("", NA) & DateNaturalisation != "",
  verbatim_fr := DateNaturalisation
]
fr_years[ # Assign verbatim_fr from DateNaturalisation if it's empty
  verbatim_fr %in% c("", NA) & FirstRecord_intentional != "",
  verbatim_fr := FirstRecord_intentional
]
fr_years[ # Assign verbatim_fr 
  FirstRecord1 != "" & FirstRecord2 != "",
  verbatim_fr := paste(FirstRecord1, FirstRecord2, sep = " - ")
]

# Prepare event_fr and confidence_fr columns, delete old columns
fr_years[, event_fr := verbatim_fr] # Initialize event_fr with verbatim_fr
fr_years[, confidence_fr := "low confidence"] # Initialize with low confidence
fr_years[, c(
  "DateNaturalisation",
  "FirstRecord",
  "FirstRecord1",
  "FirstRecord2",
  "FirstRecord_intentional"
) := NULL]

cat("\nStep 1 completed: workspace prepared and data extracted\n ") 

# Save pre-processed data
output <- file.path("tmp", "fr_years_pre-process.csv")
write.csv(fr_years, output, row.names = FALSE, fileEncoding = "UTF-8")

## 2) BASIC DATA CLEANING
cat("\nSTEP 2: BASIC DATA CLEANING") 

# Clean text
fr_years$event_fr <- gsub(",", "", fr_years$event_fr) # Remove commas
fr_years$event_fr <- gsub("['’]", "", fr_years$event_fr) # Remove apostrophes
fr_years$event_fr <- gsub("\\s+", " ", fr_years$event_fr) # Remove spaces
fr_years$event_fr <- trimws(fr_years$event_fr) # Trim whitespace
fr_years$event_fr <- tolower(fr_years$event_fr) # Replaces capital letters
fr_years$event_fr <- str_replace_all( # Normalize whitespace
  fr_years$event_fr,
  "\\p{Zs}|\\s+",
  " "
)

# Replace words with numbers
word_to_number <- c(
  "half a" = "0.5",
  "one" = "1",
  "two" = "2",
  "three" = "3",
  "four" = "4",
  "five" = "5",
  "six" = "6",
  "seven" = "7",
  "eight" = "8",
  "nine" = "9",
  "ten" = "10",
  "eleven" = "11",
  "twelve" = "12",
  "thirteen" = "13",
  "fourteen" = "14",
  "fifteen" = "15",
  "sixteen" = "16",
  "seventeen" = "17",
  "eighteen" = "18",
  "nineteen" = "19",
  "twenty" = "20"
)
for (word in names(word_to_number)) {
  pattern <- paste0("\\b", gsub(" ", "\\\\s+", word), "\\b")
  fr_years$event_fr <- gsub(
    pattern,
    word_to_number[[word]],
    fr_years$event_fr,
    ignore.case = TRUE
  )
}

# Replace full dates with years
pattern <- paste0(
  "\\b\\d{1,2}(st|nd|rd|th)?\\s+",
  "(january|
  february|
  march|
  april|
  may|
  june|
  july|
  august|
  september|
  october|
  november|
  december)",
  "\\s+(\\d{4})\\b"
)
fr_years$event_fr <- gsub(pattern, "\\3", fr_years$event_fr, ignore.case = TRUE)

# Remove months
remove_months <- c(
  "january" = "",
  "february" = "",
  "march" = "",
  "april" = "",
  "may" = "",
  "june" = "",
  "july" = "",
  "august" = "",
  "september" = "",
  "october" = "",
  "november" = "",
  "december" = ""
)
for (month in names(remove_months)) {
  pattern <- paste0("\\b", gsub(" ", "\\\\s+", month), "\\b")
  fr_years$event_fr <- gsub(pattern,
    remove_months[[month]],
    fr_years$event_fr,
    ignore.case = TRUE
  )
}

# Standardize years with 2 digits
## Ranges - in the 20th century
fr_years$event_fr <- str_replace_all(
  fr_years$event_fr,
  "(\\b19\\d{2})[-–|,](\\d{2})\\b",
  function(x) {
    parts <- str_match(x, "(19\\d{2})[-–](\\d{2})")
    paste0(parts[2], "-", "19", parts[3])
  }
)
## Ranges - in the 21st century
fr_years$event_fr <- str_replace_all(
  fr_years$event_fr,
  "(\\b20\\d{2})[-–](\\d{2})\\b",
  function(x) {
    parts <- str_match(x, "(20\\d{2})[-–](\\d{2})")
    paste0(parts[2], "-", "20", parts[3])
  }
)
## For standalone 2-digit years (avoiding centuries), ex: 78 -> 1978
fr_years$event_fr <- str_replace_all(
  fr_years$event_fr,
  "(?<!\\d)(\\d{2})(?!\\d|\\s*(st|nd|rd|th)\\s+(century|c|cen))",
  function(x) {
    year <- str_match(x, "(\\d{2})")[1, 2]
    paste0("19", year)
    year <- as.integer(str_match(x, "(\\d{2})")[1, 2])
  }
)

# Handle BC years, ex: 500 BC -> -500
fr_years$event_fr <- str_replace_all(
  fr_years$event_fr,
  regex("\\(?\\bBC\\b\\)?\\s*(\\d{3,4})|
  \\b(\\d{3,4})\\b\\s*\\(?\\bBC\\b\\)?",
    ignore_case = TRUE
  ),
  function(x) {
    year <- as.integer(str_extract(x, "\\d{3,4}"))
    paste0("-", year)
  }
)

cat("\nStep 2 completed: cleaned text and standardized years.\n ") 

## 3) CONFIDENCE ASSIGNMENT
cat("\nSTEP 3: CONFIDENCE ASSIGNMENT\
    Full years above 1500: high confidence\
    Decades above 1500: medium-high confidence\
     - Ranges <9: medium-high confidence\
     - 9 < Ranges <=15: medium confidence\
     - 15 < Ranges <=20: medium-low confidence\
     - 20 < Ranges : low confidence\
     Other: low confidence") 

mark_confidence <- function(dt) {
  # 1. Full years: high or low confidence based on 1500 threshold (1991 -> high confidence; 1491 -> low confidence)
  dt[grepl("^\\d{4}$", event_fr) & as.numeric(event_fr) >= 1500,
     confidence_fr := "high confidence"]

  # 2. Decades: low to medium-high confidence (1990s -> medium-high confidence; 1490s -> low confidence)
  dt[grepl("^\\d{4}s$|^\\d{2}s$", event_fr), confidence_fr := {
    decade_num <- as.numeric(gsub("s", "", event_fr))
    ifelse(!is.na(decade_num) & decade_num >= 1500 & decade_num %% 100 != 0,
           "medium-high confidence",
           "low confidence")
  }]

  # 3. Two-year ranges or ranges with "or"/"and": assign based on range width
  range_rows <- which(grepl("(?i)^\\s*-?\\d+\\s*([-–]|or|and)\\s*-?\\d+\\s*$",
    dt$event_fr
  ))
  if (length(range_rows) > 0) {
    # Clean and standardize separators to '-'
    cleaned <- gsub("\\s+", "", dt$event_fr[range_rows])
    cleaned <- gsub("(?i)\\s*(?:or|and|[-–])\\s*", "-", cleaned)
    # Split into start and end years
    split_result <- tstrsplit(cleaned, "-", fixed = TRUE)
    valid_pairs <- suppressWarnings(!is.na(as.integer(split_result[[1]])) &
        !is.na(as.integer(split_result[[2]]))
    )
    if (any(valid_pairs)) {
      start_years <- as.integer(split_result[[1]][valid_pairs])
      end_years   <- as.integer(split_result[[2]][valid_pairs])
      selected_rows <- range_rows[valid_pairs]

      # Ensure chronological order
      ordered <- mapply(function(start, end) {
        if (start > end) c(end, start) else c(start, end)
      }, start_years, end_years)

      start_years <- as.integer(ordered[1, ])
      end_years   <- as.integer(ordered[2, ])
      range_widths <- end_years - start_years

      # Assign confidence by width
      dt[selected_rows[range_widths <= 9],
         confidence_fr := "medium-high confidence"]
      dt[selected_rows[range_widths > 9  & range_widths <= 15],
         confidence_fr := "medium confidence"]
      dt[selected_rows[range_widths > 15 & range_widths <= 20],
         confidence_fr := "medium-low confidence"]
      dt[selected_rows[range_widths > 20], confidence_fr := "low confidence"]
    }
  }

  # 4. Default low confidence for all unmatched rows
  dt[is.na(confidence_fr), confidence_fr := "low confidence"]

  dt
}

fr_years <- mark_confidence(fr_years)

cat("\nStep 3 completed: confidence assigned\n ")

## 4) STANDARDIZE YEARS
cat("\nSTEP4: STANDARDIZE YEARS") 

# DECADES
ind <- grep("0s", fr_years$event_fr)
for (i in ind){
  fr_years[i, event_fr := gsub("0s", sample(0:9, 1), event_fr, perl = TRUE)]
}
cat("\n  Decades handled: randomized last digit of decades")

# CENTURIES, e.g., 18th century -> 1750
convert_century_to_year <- function(text) {
  pattern <- regex("\\b(\\d{1,2})(st|nd|rd|th)?\\s+(century|c|cen)\\b",
    ignore_case = TRUE
  )
  matches <- str_match(text, pattern)
  if (!is.na(matches[1, 1])) {
    century_num <- as.integer(matches[1, 2])
    year <- (century_num - 1) * 100 + 50
    return(as.character(year))
  }

  NA
}
century_years <- sapply(fr_years$event_fr, convert_century_to_year)
inds <- which(!is.na(century_years))
fr_years$event_fr[inds] <- century_years[inds]

cat("\n  Centuries handled")

# TWO POSSIBLE YEARS ("OR" and "AND")
range_pattern <- "^\\s*(-?\\d+)\\s*(or|and)\\s*(-?\\d+)\\s*$" # pattern
range_rows <- grepl(range_pattern, fr_years$event_fr) # find rows
if (any(range_rows)) { # Extract start and end years from pattern
  ranges <- str_match(fr_years$event_fr[range_rows], range_pattern)
  start_years <- as.integer(ranges[, 2])
  end_years <- as.integer(ranges[, 4])
  ordered <- mapply(function(start, end) { # Ensure correct ordering
    if (start > end) c(end, start) else c(start, end)
  }, start_years, end_years)
  start_years <- as.integer(ordered[1, ])
  end_years <- as.integer(ordered[2, ])
  range_widths <- end_years - start_years
  fr_years$event_fr[range_rows] <- as.character(end_years) # Assign most recent
}

cat("\n  Two possible years handled (and and or)")

# RANGES
range_pattern <- "^.*?(-?\\d{1,4})\\D+(-?\\d{1,4}).*?$"
range_rows <- grepl(range_pattern, fr_years$event_fr)
if (any(range_rows)) {
  ranges <- str_match(fr_years$event_fr[range_rows], range_pattern)
  start_years <- as.integer(ranges[, 2])
  end_years <- as.integer(ranges[, 3]) # Extract start and end years
  ordered <- mapply(function(start, end) {
    if (start > end) c(end, start) else c(start, end)
  }, start_years, end_years) # Order start/end properly

  start_years <- as.integer(ordered[1, ])
  end_years <- as.integer(ordered[2, ])
  range_widths <- end_years - start_years
  collapsed_years <- mapply(function(start, end) {
    if (start == end) {
      start #if range is a single year, return it
    } else {
      sample(seq(start, end), 1) #else return a random year from the range
    }
  }, start_years, end_years)

  # Update the event_fr field
  fr_years$event_fr[range_rows] <- as.character(collapsed_years)
}

cat("\n  Ranges handled")


### HANDLING "YEARS/DECADES/CENTURIES AGO" ###
reference_year <- 2000 # Define reference year
time_units <- list("years? ago" = 1, "decades? ago" = 10, "centur(?:y|ies) ago" = 100) # Define time units and their multipliers (in years)

# Loop through units and process them
for (pattern in names(time_units)) {
    full_pattern <- paste0("([\\d]+(?:\\.\\d+)?)\\s*(?:or more\\s*)?", pattern)
    matches <- str_match(fr_years$event_fr, full_pattern)
    has_match <- !is.na(matches[, 1]);
if (any(has_match)) {
    value <- as.numeric(matches[has_match, 2]) * time_units[[pattern]]
    fr_years$event_fr[has_match] <- as.character(reference_year - value)
    }}

cat ("\n  Years ago handled")

# "EARLY", "MID", "LATE" DECADES AND CENTURIES

# For decades
convert_mid_decade_random <- function(text) {
  pattern <- regex("(early|mid|middle|late)[- ]?(\\d{3})0s", ignore_case = TRUE)
  matches <- str_match(text, pattern)
  if (!is.na(matches[1, 1])) {
    decade_start <- as.numeric(matches[1, 3]) * 10
    offset <- switch(matches [1, 2],
      early = 2,
      mid   = 5,
      middle = 5,
      late  = 8,
      17
    )
    return(decade_start + offset)
  }
  NA
}  # return NA if no match

# For centuries
convert_mid_century_random <- function(text) {
  pattern <- regex(
    "(early|mid| middle| late)[- ]?(\\d{1,2})(?:th|st|nd|rd)? century",
    ignore_case = TRUE
  )
  matches <- str_match(text, pattern)
  if (!is.na(matches[1, 1])) {
    century_num <- as.numeric(matches[1, 3])
    base_year <- (century_num - 1) * 100
    offset <- switch(matches [1, 2],
      early = 20,
      mid   = 50,
      middle = 50,
      late  = 80,
      17
    )
    return(base_year + offset)
  }
  NA
}

# For partial centuries
convert_partial_century <- function(text) {
  pattern <- regex(
    "(early|mid|late)[ -]?(\\d{1,2})(?:th|st|nd|rd)?(?!\\s*century)",
    ignore_case = TRUE
  )
  matches <- str_match(text, pattern)
  if (!is.na(matches[1, 1])) {
    century_num <- as.numeric(matches[1, 3])
    base_year <- (century_num - 1) * 100
    offset <- switch(tolower(matches[1, 2]),
                     "early" = 20,
                     "mid"   = 50,
                     "late"  = 80,
                     17)
    return(base_year + offset)
  }
  NA
}

# Apply to fr_years
mid_century_years <- sapply(fr_years$event_fr, convert_mid_century_random)
inds <- which(!is.na(mid_century_years))
fr_years$event_fr[inds] <- as.character(mid_century_years[inds])

mid_decade_years <- sapply(fr_years$event_fr, convert_mid_decade_random)
inds <- which(!is.na(mid_decade_years))
fr_years$event_fr[inds] <- as.character(mid_decade_years[inds])

partial_century_years <- sapply(fr_years$event_fr, convert_partial_century)
inds <- which(!is.na(partial_century_years))
fr_years$event_fr[inds] <- as.character(partial_century_years[inds])

cat("\n  Early, mid-, late decades and centuries handled")

# AFTER/POST YEARS
convert_after <- function(text) {
  pattern <- regex("(?i)(?:after|post)[ -]?(\\d{4})(?:\\s+or\\s+later)?\\b")
  matches <- str_match(text, pattern)
  if (!is.na(matches[, 2])) {
    year <- as.numeric(matches[, 2])
    new_year <- year + 2
    confidence <- if (year %% 10 != 0) "medium confidence" else "low confidence"
    return(c(year = as.character(new_year), confidence = confidence))
  }
  c(year = NA, confidence = NA)
}
# Apply to all rows
after_results <- t(sapply(fr_years$event_fr, convert_after))

# Add new values only where a match was found
matched_inds <- which(!is.na(after_results[, "year"]))
fr_years$event_fr[matched_inds] <- after_results[matched_inds, "year"]
fr_years$confidence_fr[matched_inds] <-
  after_results[matched_inds, "confidence"]

cat("\n  After and post years handled")

# CHECK FOR NON-MATCHING FORMATS 1
pattern <- "^\\d{3,4}$|^-\\d{4}$|^\\d{4}\\s*-\\s*\\d{4}$"
# Filter rows that DO NOT match this pattern
non_matching_rows <- fr_years[!grepl(pattern, event_fr)]
tmp_file <- file.path("tmp", "fr_non_matching_formats1.csv")
fwrite(non_matching_rows, tmp_file)

cat("\nStep 4 completed: years standardized\n ")

## 5) FINAL CLEANING
cat("\nSTEP 5: FINAL CLEANING")

clean_first_record <- function(dt) {
  dt[, event_fr := gsub("[^0-9\\-]", "", event_fr)]  # Keep only digits and -
  dt[, event_fr := gsub("(?<!\\d)(\\d{1,2})(?!\\d)",
      "", event_fr, perl = TRUE
    )
  ]  # Remove 1–2 digit numbers
  dt[, event_fr := gsub("-{2,}", "-", event_fr)] # Replace multiple - by one
  dt[, event_fr := gsub("-+$", "", event_fr)]  # Remove trailing hyphens

}
clean_first_record(fr_years)
# Remove empty event_fr values
# Extract rows that we will delete (empty event_fr, but not empry verbatim_fr)
fr_years <- fr_years[event_fr != ""]

# CHECK FOR NON-MATCHING FORMATS 2
pattern <- "^\\d{3,4}$|^-\\d{4}$|^\\d{4}\\s*-\\s*\\d{4}$"
# Filter rows that DO NOT match this pattern
non_matching_rows <- fr_years[!grepl(pattern, event_fr)]
tmp_file <- file.path("tmp", "fr_non_matching_formats2.csv")
fwrite(non_matching_rows, tmp_file)

cat("\nStep 5 completed: final cleaning done")

# Save processed data
output <- file.path("outputs", "fr_years_processed.csv")
write.csv(fr_years, output, row.names = FALSE, fileEncoding = "UTF-8")

cat("\nYears have been processed. Output file saved.\n ")
