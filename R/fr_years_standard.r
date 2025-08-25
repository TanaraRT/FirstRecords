##########################################################################
##                                                                      ##
##                       FIRST RECORDS WORKFLOW                         ##
##                          Standardize years                           ##
##                   -----------------------------                      ##
##                                                                      ##
## T. Renard Truong, H. Seebens                                         ##
## v2.0, August 2025                                                    ##
##########################################################################

fr_years_standard <- function(dataset = NULL, fr_column_name = NULL, use_log = FALSE, save_to_disk = FALSE){
  
  stopifnot(!is.null(dataset) && is.data.table(dataset))
  
  # --- Open log file ---
  if (use_log == TRUE){
    log_file <- file.path(outputs, paste0("log_file_", Sys.Date(), ".txt"))
    if (file.exists(log_file)) {
      sink(log_file, append = TRUE)  # Open log file for appending
    } else {
      sink(log_file, append = FALSE) # Create new log file
    }
  }
  cat("\nSTEP 3: Standardize years") 
  
  # --- Check if FirstRecordEvent exist ---
  if (!fr_column_name %in% colnames(dataset)) {
    stop("No first record column provided. Please specify in fr_column_name")
  }
  colnames(dataset)[colnames(dataset)==fr_column_name] <- "firstRecordEvent"
  cat("\n   - Renamed 'fr_column_name' to 'firstRecordEvent'") 
  
  # --- Create verbatim copy ---
  if (!"verbatimFirstRecordEvent" %in% colnames(dataset)) {
    dataset[, verbatimFirstRecordEvent := firstRecordEvent]
  }
  
  cat("\n   - If it doesn't already exsit, created 'verbatimFirstRecordEvent'") 
  
  # --- Initialize confidence column ---
  if (!"confidenceFirstRecordEvent" %in% colnames(dataset)) {
    dataset[, confidenceFirstRecordEvent := ""]
  }
  
  cat("\n   - If it doesn't already exsit, created 'confidenceFirstRecordEvent'") 
  
  ## 3A) BASIC DATA CLEANING
  
  # --- Clean text ---
  dataset$firstRecordEvent <- gsub(",", "", dataset$firstRecordEvent) # Remove commas
  dataset$firstRecordEvent <- gsub("['’]", "", dataset$firstRecordEvent) # Remove apostrophes
  dataset$firstRecordEvent <- gsub("\\s+", " ", dataset$firstRecordEvent) # Remove spaces
  dataset$firstRecordEvent <- trimws(dataset$firstRecordEvent) # Trim whitespace
  dataset$firstRecordEvent <- tolower(dataset$firstRecordEvent) # Replaces capital letters
  dataset$firstRecordEvent <- str_replace_all( # Normalize whitespace
    dataset$firstRecordEvent, "\\p{Zs}|\\s+"," ")
  dataset$firstRecordEvent <- gsub(" to ", " - ", dataset$firstRecordEvent) # Replace "to" by "-"
  dataset$firstRecordEvent <- gsub(" & ", " - ", dataset$firstRecordEvent) # Replace "&" by "-"
  dataset$firstRecordEvent <- gsub(";", "-", dataset$firstRecordEvent) # Replace ";" by "-"
  dataset$firstRecordEvent <- gsub("�", "", dataset$firstRecordEvent, fixed = TRUE)# Delete "�"
  
  # --- Replace words with numbers ---
  word_to_number <- c(
    "one" = "1", "two" = "2", "three" = "3", "four" = "4", "five" = "5",
    "six" = "6", "seven" = "7", "eight" = "8", "nine" = "9", "ten" = "10",
    "eleven" = "11", "twelve" = "12", "thirteen" = "13", "fourteen" = "14",
    "fifteen" = "15", "sixteen" = "16", "seventeen" = "17", "eighteen" = "18",
    "nineteen" = "19", "twenty" = "20"
  )
  for (word in names(word_to_number)) {
    pattern <- paste0("\\b", gsub(" ", "\\\\s+", word), "\\b")
    dataset$firstRecordEvent <- gsub(
      pattern,
      word_to_number[[word]],
      dataset$firstRecordEvent,
      ignore.case = TRUE
    )
  }
  
  # --- Replace full dates with years ---
  pattern <- paste0(
    "\\b\\d{1,2}(st|nd|rd|th)?\\s+",
    "(january|february|march|april|may|june|july|august|september|october|november|december)",
    "\\s+(\\d{4})\\b"
  )
  dataset$firstRecordEvent <- gsub(pattern, "\\3", dataset$firstRecordEvent, ignore.case = TRUE)
  
  # --- Remove months ---
  remove_months <- c(
    "january" = "", "february" = "", "march" = "", "april" = "", "may" = "",
    "june" = "", "july" = "", "august" = "", "september" = "", "october" = "",
    "november" = "", "december" = ""
  )
  for (month in names(remove_months)) {
    pattern <- paste0("\\b", gsub(" ", "\\\\s+", month), "\\b")
    dataset$firstRecordEvent <- gsub(pattern,
                                     remove_months[[month]],
                                     dataset$firstRecordEvent,
                                     ignore.case = TRUE
    )
  }
  
  # --- Standardize years with 2 digits ---
  
  ## Ranges in the 19th century
  # Example: 1817-19 -> 1817-1819
  dataset$firstRecordEvent <- str_replace_all(
    dataset$firstRecordEvent,
    "(\\b18\\d{2})[-–|,](\\d{2})\\b",
    function(x) {
      parts <- str_match(x, "(18\\d{2})[-–](\\d{2})")
      paste0(parts[2], "-", "18", parts[3])
    }
  )
  
  ## Ranges in the 20th century
  # Example: 1927-30 -> 1927-1930
  dataset$firstRecordEvent <- str_replace_all(
    dataset$firstRecordEvent,
    "(\\b19\\d{2})[-–|,](\\d{2})\\b",
    function(x) {
      parts <- str_match(x, "(19\\d{2})[-–](\\d{2})")
      paste0(parts[2], "-", "19", parts[3])
    }
  )
  
  ## Ranges in the 21st century
  # Example: 20012-14 -> 2012- 2014
  dataset$firstRecordEvent <- str_replace_all(
    dataset$firstRecordEvent,
    "(\\b20\\d{2})[-–](\\d{2})\\b",
    function(x) {
      parts <- str_match(x, "(20\\d{2})[-–](\\d{2})")
      paste0(parts[2], "-", "20", parts[3])
    }
  )
  
  ## For standalone 2-digit years (avoiding centuries)
  # Example: 78 -> 1978
  dataset$firstRecordEvent <- str_replace_all(
    dataset$firstRecordEvent,
    "(?<!\\d)(\\d{2})(?!\\d|\\s*(st|nd|rd|th)\\s+(century|c|cen))",
    function(x) {
      year <- str_match(x, "(\\d{2})")[1, 2]
      paste0("19", year)
      year <- as.integer(str_match(x, "(\\d{2})")[1, 2])
    }
  )
  
  # --- Handle BC years ---
  # example: 500 BC -> -500
  dataset$firstRecordEvent <- str_replace_all(
    dataset$firstRecordEvent,
    regex("\\(?\\s*(?:BC\\.?E?\\.?\\s*(\\d{1,4})|(\\d{1,4})\\s*\\(?(?:BC\\.?E?\\.?)\\)?)\\s*\\)?", ignore_case = TRUE),
    function(x) {
      year <- as.integer(str_extract(x, "\\d{3,4}"))
      paste0("-", year)
    }
  )
  
  # --- Replace two dates separated by a space by a range ---
  # example: 1917 1920 -> 1917-1920
  pattern_years <- regex("(\\b\\d{4})\\s*(\\d{4}\\b)", ignore_case = TRUE)
  dataset$firstRecordEvent <- gsub(pattern_years, "\\1-\\2", dataset$firstRecordEvent, ignore.case = TRUE)
  
  # --- Replace two dates separated by a / by a range ---
  # example: 1917/1920 -> 1917 - 1920
  pattern_years <- regex("(\\b\\d{4})\\s*/\\s*(\\d{4}\\b)", ignore_case = TRUE)
  dataset$firstRecordEvent <- gsub(pattern_years, "\\1-\\2", dataset$firstRecordEvent, ignore.case = TRUE)
  
  # --- Delete references ---
  # example: 1817 (text) -> 1817 
  pattern_ref <- regex("\\b(\\d{4})\\s+(\\(.*\\))$", ignore_case = TRUE)
  dataset$firstRecordEvent <- gsub(pattern_ref, "\\1", dataset$firstRecordEvent, ignore.case = TRUE)
  dataset$firstRecordEvent <- trimws(dataset$firstRecordEvent) # Trim whitespace
  
  cat("\nStep3a completed: basic data cleaning") 
  cat("\n  - Removed or standardized spaces and special characters") 
  cat("\n  - Replaced number words with digits")
  cat("\n  - Converted full (d, m, y) and partial dates (m, y) to years")
  cat("\n  - Normalized two-digit and BC years to four-digit format")
  cat("\n  - Replaced slashes/spaces between years with hyphens")
  cat("\n  - Deleted references")
  
  ## 3B) CONFIDENCE ASSIGNMENT
  
  mark_confidence <- function(dt) {
    # --- 1. Full years: high or low confidence based on 1500 threshold ---
    # example: 1991 -> high confidence; 1491 -> low confidence
    dt[grepl("^\\d{4}$", firstRecordEvent), 
       confidenceFirstRecordEvent := ifelse(as.numeric(firstRecordEvent) >= 1500, "high confidence", confidenceFirstRecordEvent)]
    
    # --- 2. Decades: low to medium-high confidence ---
    # example: 1990s -> medium-high confidence; 1490s -> low confidence
    dt[grepl("^\\d{4}s$|^\\d{2}s$", firstRecordEvent), confidenceFirstRecordEvent := {
      decade_num <- as.numeric(gsub("s", "", firstRecordEvent))
      ifelse(!is.na(decade_num) & decade_num >= 1500 & decade_num %% 100 != 0,
             "medium-high confidence",
             "low confidence")
    }
    ]
    
    # --- 3. Two-year ranges or ranges with "or"/"and": assign based on range width ---
    # example: 1922-1927 -> high confidence; 1922-1976 -> low confidence 
    range_rows <- which(grepl("(?i)^\\s*-?\\d+\\s*([-–]|or|and)\\s*-?\\d+\\s*$",
                              dt$firstRecordEvent
    ))
    if (length(range_rows) > 0) {# Clean and standardize separators to '-'
      cleaned <- gsub("\\s+", "", dt$firstRecordEvent[range_rows])
      cleaned <- gsub("(?i)\\s*(?:or|and|[-–])\\s*", "-", cleaned)
      split_result <- tstrsplit(cleaned, "-", fixed = TRUE) # Split into start and end years
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
           confidenceFirstRecordEvent := "medium-high confidence"]
        dt[selected_rows[range_widths > 9  & range_widths <= 15],
           confidenceFirstRecordEvent := "medium confidence"]
        dt[selected_rows[range_widths > 15 & range_widths <= 20],
           confidenceFirstRecordEvent := "medium-low confidence"]
        dt[selected_rows[range_widths > 20], confidenceFirstRecordEvent := "low confidence"]
      }
    }
    
    # --- 4. Default low confidence for all unmatched rows ---
    dt[is.na(confidenceFirstRecordEvent), confidenceFirstRecordEvent := "low confidence"]
    dt
  }
  dataset <- mark_confidence(dataset)
  
  cat("\nStep 3b completed: Confidence terms assigned\
  - Full years above 1500: high confidence\
  - Decades above 1500: medium-high confidence\
  - Ranges <9: medium-high confidence\
  - 9 < Ranges <=15: medium confidence\
  - 15 < Ranges <=20: medium-low confidence\
  - 20 < Ranges : low confidence\
  Other: low confidence") 
  
  ## 3C) STANDARDIZE YEARS
  
  # --- Handling decades ---
  convert_decade_expression_to_year <- function(text) {
    pattern_specific <- regex("(early|mid|middle|late)[- ]?(\\d{3})0s", ignore_case = TRUE)
    match_specific <- str_match(text, pattern_specific)
    
    ## 1. Specific: early/mid/late 1970s
    # example : early 1970s -> 1972
    if (!is.na(match_specific[1, 1])) {
      decade_start <- as.numeric(match_specific[1, 3]) * 10
      offset <- switch(trimws(tolower(match_specific[1, 2])),
                       early = 2,
                       mid = 5,
                       middle = 5,
                       late = 8,
                       5)  # default fallback
      return(as.character(decade_start + offset))
    }
    
    ## 2. Randomize last digit of a decade
    # example : 1970s -> 1974
    pattern_generic <- regex("\\b(\\d{3})0s\\b", ignore_case = TRUE)
    match_generic <- str_match(text, pattern_generic)
    
    if (!is.na(match_generic[1, 1])) {
      decade_start <- as.numeric(match_generic[1, 2]) * 10
      random_offset <- sample(0:9, 1)
      return(as.character(decade_start + random_offset))
    }
    # No match
    NA
  }
  
  decade_years <- sapply(dataset$firstRecordEvent, convert_decade_expression_to_year)
  inds <- which(!is.na(decade_years))
  dataset$firstRecordEvent[inds] <- decade_years[inds]
  
  # --- Handling early/mid/late centuries ---
  convert_century_expression_to_year <- function(text) {
    pattern_specific <- regex(
      "(early|mid| middle|late)[- ]?(\\d{1,2})(?:th|st|nd|rd)?\\s*century",
      ignore_case = TRUE
    )
    match_specific <- str_match(text, pattern_specific)
    
    if (!is.na(match_specific[1, 1])) {
      century_num <- as.numeric(match_specific[1, 3])
      base_year <- (century_num - 1) * 100
      offset <- switch(trimws(tolower(match_specific[1, 2])),
                       early = 20,
                       mid = 50,
                       middle = 50,
                       late = 80,
                       50)  # default fallback
      return(as.character(base_year + offset))
    }
    
    # If no specific match, try the generic "19th century"
    pattern_generic <- regex(
      "\\b(\\d{1,2})(st|nd|rd|th)?\\s+(century|c|cen)\\b",
      ignore_case = TRUE
    )
    match_generic <- str_match(text, pattern_generic)
    
    if (!is.na(match_generic[1, 1])) {
      century_num <- as.integer(match_generic[1, 2])
      year <- (century_num - 1) * 100 + 50
      return(as.character(year))
    }
    NA # If no match at all
  }
  century_years <- sapply(dataset$firstRecordEvent, convert_century_expression_to_year)
  inds <- which(!is.na(century_years))
  dataset$firstRecordEvent[inds] <- century_years[inds]
  
  # --- Handling two possible years ("or" and "and")
  range_pattern <- "^\\s*(-?\\d+)\\s*(or|and)\\s*(-?\\d+)\\s*$" # pattern
  range_rows <- grepl(range_pattern, dataset$firstRecordEvent) # find rows
  if (any(range_rows)) { # Extract start and end years from pattern
    ranges <- str_match(dataset$firstRecordEvent[range_rows], range_pattern)
    start_years <- as.integer(ranges[, 2])
    end_years <- as.integer(ranges[, 4])
    ordered <- mapply(function(start, end) { # Ensure correct ordering
      if (start > end) c(end, start) else c(start, end)
    }, start_years, end_years)
    start_years <- as.integer(ordered[1, ])
    end_years <- as.integer(ordered[2, ])
    range_widths <- end_years - start_years
    dataset$firstRecordEvent[range_rows] <- as.character(end_years) # Assign most recent
  }
  
  # --- Handling ranges ---
  # example: 1917-1920 -> 1918
  range_pattern <- "^\\s*(-?\\d{1,4})\\s*[-–]\\s*(-?\\d{1,4})\\s*$"
  range_rows <- grepl(range_pattern, dataset$firstRecordEvent)
  if (any(range_rows)) {
    ranges <- str_match(dataset$firstRecordEvent[range_rows], range_pattern)
    start_years <- as.integer(ranges[, 2])
    end_years <- as.integer(ranges[, 3]) # Extract start and end years
    # Chronologically order start and end years (oldest to most recent)
    is_later <- start_years > end_years
    temp <- start_years
    start_years[is_later] <- end_years[is_later]
    end_years[is_later] <- temp[is_later]
    range_widths <- end_years - start_years
    
    collapsed_years <- mapply(function(start, end) {
      if (start == end) {
        start
      } else {
        sample(seq(start, end), 1)
      }
    }, start_years, end_years)
    dataset$firstRecordEvent[range_rows] <- as.character(collapsed_years)
  }
  
  # --- Handling years, decades, centuries ago ---
  # example: 20 years ago -> 1980
  reference_year <- 2000 # Define reference year
  time_units <- list("years? ago" = 1, "decades? ago" = 10, "centur(?:y|ies) ago" = 100) # Define time units and their multipliers (in years)
  
  # Loop through units and process them
  for (pattern in names(time_units)) {
    full_pattern <- paste0("([\\d]+(?:\\.\\d+)?)\\s*(?:or more\\s*)?", pattern)
    matches <- str_match(dataset$firstRecordEvent, full_pattern)
    has_match <- !is.na(matches[, 1]);
    if (any(has_match)) {
      value <- as.numeric(matches[has_match, 2]) * time_units[[pattern]]
      dataset$firstRecordEvent[has_match] <- as.character(reference_year - value)
    }
  }
  
  # --- Handling after- and post- years
  # example: after 1917 -> 1919
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
  after_results <- t(sapply(dataset$firstRecordEvent, convert_after))
  
  # Add new values only where a match was found
  matched_inds <- which(!is.na(after_results[, "year"]))
  dataset$firstRecordEvent[matched_inds] <- after_results[matched_inds, "year"]
  dataset$confidenceFirstRecordEvent[matched_inds] <-after_results[matched_inds, "confidence"]
  
  # --- Check non-matching formats (1) ---
  pattern <- "^(-?(?:[0-9]{1,3}|1[0-9]{3}|20[0-1][0-9]|202[0-5]))$|^(-?(?:[0-9]{1,3}|1[0-9]{3}|20[0-1][0-9]|202[0-5])\\s*-\\s*(-?(?:[0-9]{1,3}|1[0-9]{3}|20[0-1][0-9]|202[0-5])))$"
  # Filter rows that DO NOT match this pattern
  non_matching_rows <- dataset[!grepl(pattern, firstRecordEvent)]
  filename <- file.path(tmp, "fr_check_missing_years1.csv")
  fwrite(non_matching_rows, filename)
  cat("\n  - Non-processed rows pre-final cleaning \"fr_check_missing_years1.csv\" available in tmp folder")
  
  cat("\nStep 3c: years have been standardized") 
  cat("\n  - Decades handled: randomized or interpreted early/mid/late decades")
  cat("\n  - Centuries handled: +50 or interpreted early/mid/late decades")
  cat("\n  - Two possible years handled (and and or)")
  cat("\n  - Ranges handled")
  cat ("\n  - Years ago handled")
  cat("\n  - After and post years handled")

  ## 3D) FINAL CLEANING
  
  clean_first_record <- function(dt) {
    dt[, firstRecordEvent := gsub("[^0-9\\-]", "", firstRecordEvent)]  # Keep only digits and -
    dt[, firstRecordEvent := gsub("(?<!\\d)(\\d{1,2})(?!\\d)", "", firstRecordEvent, perl = TRUE)]  # Remove 1–2 digit numbers
    dt[, firstRecordEvent := gsub("-{2,}", "-", firstRecordEvent)] # Replace multiple - by one
    dt[, firstRecordEvent := gsub("-+$", "", firstRecordEvent)]  # Remove trailing hyphens
  }
  clean_first_record(dataset)
  
  # Remove empty firstRecordEvent values
  # Extract rows that we will delete (empty firstRecordEvent, but not empty verbatim_fr)
  dataset <- dataset[firstRecordEvent != ""]
  
  # Extract non-processed/non-matching years (e.g., ranges)
  pattern <- "^\\d{3,4}$|^-\\d{3,4}$|^\\d{3,4}\\s*-\\s*\\d{3,4}$"
  # Filter rows that DO NOT match this pattern
  non_matching_rows <- dataset[!grepl(pattern, firstRecordEvent)]
  filename <- file.path(tmp, "fr_check_missing_years2.csv")
  fwrite(non_matching_rows, filename)
  
  # Remove non-processed/non-matching years and save processed data
  pattern <- "^-?\\d{3,4}$"
  # Filter rows that DO NOT match this pattern
  fr_main_dataset_step3 <- as.data.table(dataset[grepl(pattern, dataset$firstRecordEvent), ])
  # Remove rows where years are >= 2025
  fr_main_dataset_step3 <- fr_main_dataset_step3[firstRecordEvent <= 2025 | is.na(firstRecordEvent)]
  
  cat("Step 3d completed: final cleaning, text has been erased")
  cat("\n  - Non-processed rows post-final cleaning \"fr_check_missing_years2.csv\" available in tmp folder")
  
  if (save_to_disk == TRUE){
    filename <- file.path(tmp, "fr_main_dataset_step3.csv")
    fwrite(fr_main_dataset_step3, filename)
    cat("\n  - Updated dataset available in 'tmp' folder\n ")
  }
  
  cat("\nStep3 completed: first records (years) have been standardized in 'fr_main_dataset_3'. Years that couldn't be standardized are available in the 'tmp' folder\n ")

  if (use_log == TRUE){
    sink()
  }
  return(fr_main_dataset_step3)
}