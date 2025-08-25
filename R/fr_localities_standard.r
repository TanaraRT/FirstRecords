##########################################################################
##                                                                      ##
##                       FIRST RECORDS WORKFLOW                         ##
##                       Standardize localities                         ##
##                   -----------------------------                      ##
##                                                                      ##
## M. Gomez Suarez, H. Seebens, T. Renard Truong                        ##
## v2.0, August 2025                                                    ##
##########################################################################


fr_localities_standard <- function(dat, use_log = FALSE, save_to_disk = FALSE){
  
  stopifnot(!is.null(dat) && is.data.table(dat))
  
  # --- Open log file ---
  if (use_log == TRUE){
    log_file <- file.path(outputs, paste0("log_file_", Sys.Date(), ".txt"))
    if (file.exists(log_file)) {
      sink(log_file, append = TRUE)  # Open log file for appending
    } else {
      sink(log_file, append = FALSE) # Create new log file
    }
  }
  cat("\nSTEP 4: Standardize localities") 

  ## STEP 4A: Prepare reference tables and dataset
  # --- 1. Load reference location table ---
  regions <- read.xlsx("data/config/AllLocations.xlsx", sheet = "location", na.strings = "")
  regions <- regions[, c("locationID", "location", "location_var", "sinas_region")]
  regions$location_var <- tolower(regions$location_var)  # Set all to lowercase for matching
  regions$location_lower <- tolower(regions$location)  # Set all to lowercase for matching
  
  subregions <- read.xlsx("data/config/AllLocations.xlsx", sheet = "stateProvince", na.strings = "")
  subregions <- subregions[, c("locationID", "location", "location_var", "gadm1_name", "gadm1_var", "sinas_region")]
  subregions$gadm1_var <- tolower(subregions$gadm1_var)  # Set all to lowercase for matching
  subregions$Gadm1_lower <- tolower(subregions$gadm1_name)  # Set all to lowercase for matching

  # --- 2. Get duplicated names of subregions ---
  dup <- unique(gsub("\\s*\\(.*?\\)", "", subregions$gadm1_name)[duplicated(gsub("\\s*\\(.*?\\)", "", subregions$gadm1_name))])
  
  # --- 3. Prepare dataset for processing ---
  dat <- rename(dat, "location_orig" = "locality") #standardize column name
  dat <- select(dat, -locationID) #remove existing locationID column
  
  # --- 4. Prepare for matching with regions ---
  dat_match1 <- dat ## use another dat set for region matching to keep the original names
  dat_match1$order <- 1:nrow(dat_match1)
  dat_match1$location_orig <- gsub("\\xa0|\\xc2", " ", dat_match1$location_orig)  # Replace special spaces
  dat_match1$location_orig <- gsub("^\\s+|\\s+$", "", dat_match1$location_orig)  # Trim leading/trailing whitespace
  dat_match1$location_orig <- gsub("  ", " ", dat_match1$location_orig)  # Replace double spaces
  dat_match1$location_orig <- gsub(" \\(the\\)", "", dat_match1$location_orig)  # Remove " (the)"
  dat_match1$location_lower <- tolower(dat_match1$location_orig)  # Lowercase for matching
  
  cat("\nStep 4a completed: reference table and dataset ready for process")
  
  ## STEP 4B: Exact location matching
  
  # --- 1: Match names of 'dat' with region names of 'regions'--- 
  dat_match_regions <- merge(dat_match1, regions, by.x = "location_lower", by.y = "location_lower", all.x = TRUE)
  
  # --- 2: Match names of 'dat' with region names of 'subregions' --- 
  dat_match_subregions <- merge(dat_match1, subregions, by.x = "location_lower", by.y = "Gadm1_lower", all.x = TRUE)
  
  cat("\nStep 4b completed: exact region and subregion names matched")
  
  ## STEP 4C: Keyword matching
  
  # --- 1: Match based on keywords in 'regions' - after 'location_var' column ---
  ind_keys_regions <- which(!is.na(regions$location_var))
  for (j in ind_keys_regions) {  # loop over rows with multiple country name variations
    location_var <- unlist(strsplit(regions$location_var[j], "; ")) # check if multiple country name variations provided
    for (k in location_var) {
      ind_match <- which(dat_match_regions$location_lower == k)
      if (length(unique(regions$location[j])) > 1) 
        cat(paste0("Warning: ", k, " matches multiple location names. Refine location_var!"))
      
      dat_match_regions$location[ind_match] <- regions$location[j]
      dat_match_regions$locationID[ind_match] <- regions$locationID[j]
    }
  }
  
  # --- 2: Match based on keywords in 'subregions' - after 'gadm1_var' column ---
  ind_keys_subregions <- which(!is.na(subregions$gadm1_var))
  for (j in ind_keys_subregions) {  # loop over rows with multiple subregion name variations
    gadm1_var <- unlist(strsplit(subregions$gadm1_var[j], "; ")) # check if multiple subregion name variations provided
    for (k in gadm1_var) {
      ind_match <- which(dat_match_subregions$location_lower == k)
      if (length(unique(subregions$gadm1_name[j])) > 1) 
        cat(paste0("Warning: ", k, " matches multiple location names. Refine gadm1_var!"))
      
      dat_match_subregions$gadm1_name[ind_match] <- subregions$gadm1_name[j]
      dat_match_subregions$location[ind_match] <- subregions$location[j]
      dat_match_subregions$locationID[ind_match] <- subregions$locationID[j]
    }
  }
  
  # --- 3: Merge both data frames with standardized locations names ('region' and 'subregion') ---
  dat_match1 <- full_join(dat_match_subregions, 
                          dat_match_regions |> select(order, locationID, location), 
                          by="order") |> 
    mutate(locationID = coalesce(locationID.x, locationID.y),
           location = coalesce(location.x, location.y))|> 
    select(-locationID.x, -locationID.y, -location.x, -location.y, -location_var, -gadm1_var)
  
  
  # --- 4: Final merging of both data sets with standardized region names to original data ---
  dat_match1 <- dat_match1[order(dat_match1$order),]
  if (!identical(dat_match1$taxon_orig,dat$taxon_orig)) stop("Data sets not sorted equally!")
  
  dat$locationID <- dat_match1$locationID
  dat$location <- dat_match1$location
  dat$stateProvince <- dat_match1$gadm1_name
  
  cat("\nStep 4c completed: variant region and subregion names matched")
  
  ## STEP 4D: Warnings for duplication
  
  # --- 1: Check if any locations in the original dataframe correspond to duplicated locations in the world ---
  if (any(dat$location_orig %in% dup)) {
    # Extract the matching names from dat$location_orig
    matching_names <- unique(dat$location_orig[dat$location_orig %in% dup])
    warning(paste(
      "\n    Warning: Unresolved terms in file.The following location name(s) correspond to multiple subregions in the world:",
      paste(matching_names, collapse = ", "),
      ". Please modify the original location name(s) by including the country name in parentheses(), and try again (e.g: Amazonas (Colombia)) \n"
    ))
  }
  dat_regnames <- dat
  
  
  cat("\nStep 4d completed: warnings will be issued for unresolved terms")
  
  ## STEP 4E: Outputs
  
  # --- 1: Remove duplicated entries ---
  dat_regnames <- dat_regnames[!duplicated(dat_regnames), ]
  write_regnames <- dat_regnames
  
  # --- 2: Clean locations and locationID 
  write_regnames <- write_regnames |> 
    select(-c(stateProvince, locationID)) |> 
    left_join(regions |> select(location, locationID, sinas_region), by = "location")
  
  # --- 3: Save fr_main_dataset_step4 with standardized location names ---
  fr_main_dataset_step4 <- write_regnames[, c("locationID", "location", "taxonID", "taxon",
                                       "habitat",	"firstRecordEvent",	"verbatimFirstRecordEvent", 
                                       "confidenceFirstRecordEvent",	"occurrenceStatus",	"establishmentMeans",
                                       "degreeOfEstablishment", "pathway",	"datasetName",	"bibliographicCitation",	
                                       "accessRights"
  )]
  
  if (save_to_disk == TRUE){
    fr_main_dataset_step4 <- as.data.table(fr_main_dataset_step4)
    filename <- file.path(tmp, "fr_main_dataset_step4.csv")
    fwrite(fr_main_dataset_step4, filename)
    cat("\n  - Updated dataset available in 'tmp' folder\n ")
  }
  
  # --- 4: Check and save missing location names ---
  missing <- dat_regnames$location_orig[is.na(dat_regnames$locationID)]
  if (length(missing) > 0) {
    filename <- file.path(tmp, "check_missing_locations.csv")
    write.table(sort(unique(missing)), filename, row.names = FALSE, col.names = FALSE)
    cat("Missing locations written to:", filename, "\n")
    }
  
  # ---5: Save location table ---
  location_table <- write_regnames[, .(locationID, location, verbatimLocation, sinas_region)]
  location_table[, verbatimLocation_lower := tolower(verbatimLocation)]
  location_table <- unique(location_table, by = "verbatimLocation_lower")
  location_table[, verbatimLocation_lower := NULL]  # drop helper column
  setorder(location_table, locationID)
  location_table <- location_table[locationID != "" & !is.na(locationID)]
  names(location_table)[names(location_table) == "sinas_region"] <- "region"
  filename <- file.path(outputs, "location_table.csv")
  fwrite(location_table, filename)
  
  cat("\nStep 4 completed: locations have been standardized and the location table is available in the 'outputs' folder\n ") 
 
   if (use_log == TRUE){
    sink()
  }
  return(fr_main_dataset_step4)
}