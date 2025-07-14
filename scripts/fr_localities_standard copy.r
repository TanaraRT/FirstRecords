
##########################################################################
##                                                                      ##
##                        FIRST RECORDS WORKFLOW                        ##
##               Step 4: Standardisation of location names              ##
##             using a translation table "AllLocations.xlsx"            ##
##                                                                      ##
## M. Gómez-Suárez, H. Seebens, T. Renard Truong                        ##
## vx.x, 2025                                                           ##
##########################################################################


fr_localities_standard <- function(dataset){

  if (is.null(dataset) || !is.data.frame(dataset)) {
    stop("Invalid input: dataset must be a data.frame or data.table")
  }
  
  ## load location tables
  # load table with countries
  regions <- read.xlsx("data/external/AllLocations.xlsx") # Sheet with first aggregation level (i.e. countries)
  regions <- regions[, c("locationID", "location", "location_var")]
  regions$location_var <- tolower(regions$location_var)  # Set all to lowercase for matching
  regions$location <- tolower(regions$location)  # Set all to lowercase for matching

  # load table with state, provinces, departments, etc...
  subregions <- read.xlsx("data/external/AllLocations.xlsx") # Sheet with second aggregation level (i.e. states, provinces...)
  subregions <- subregions[, c("locationID", "location", "location_var", "gadm1_name", "gadm1_var")]
  subregions$gadm1_var <- tolower(subregions$gadm1_var)  # Set all to lowercase for matching
  subregions$Gadm1_lower <- tolower(subregions$gadm1_name)  # Set all to lowercase for matching
  
  setDT(regions)
  setDT(subregions)

  # Get duplicated names of subregions  
  dup <- unique(gsub("\\s*\\(.*?\\)", "", subregions$gadm1_name)[duplicated(gsub("\\s*\\(.*?\\)", "", subregions$gadm1_name))])
  
  # Prepare dataset for matching with regions
  dat_match1 <- dataset # use another dataset for region matching to keep the original names
  dat_match1$order <- 1:nrow(dat_match1)
  dat_match1$locality <- tolower(dat_match1$locality)  # Lowercase for matching
  
  ## Step 1: Match localities of 'dataset' with locations of 'regions' and 'subregions' 
  regions_dedup <- regions[!duplicated(location)]
  dat_match_regions <- merge(dat_match1, regions_dedup, by.x = "locality", by.y = "location", all.x = TRUE)
  
  dat_match_regions <- rename(dat_match_regions, locationID = locationID.y)
  
  dat_match_regions$locationID.x <- NULL
  dat_match_regions <- dat_match_regions[, c("locationID", "locality", "location_var","verbatimLocation", "country", "continent", "taxonID", "Taxon",
                                             "habitat",	"firstRecordEvent",	"verbatimFirstRecordEvent", 
                                             "confidenceFirstRecordEvent",	"occurenceStatus",	"establishmentMeans",
                                             "degreeOfEstablishment",	"datasetName",	"bibliographicCitation",	
                                             "accessRights", "order"
  )]

  subregions_dedup <- subregions[!duplicated(Gadm1_lower)]
  dat_match_subregions <- merge(dat_match1, subregions_dedup, by.x = "locality", by.y = "Gadm1_lower", all.x = TRUE)
  dat_match_subregions$locationID.x <- dat_match_subregions$locationID.y
  dat_match_subregions <- rename(dat_match_subregions, locationID = locationID.x)
  
  dat_match_subregions$locationID.y <- NULL
  dat_match_subregions <- dat_match_subregions[, c("locationID", "locality", "location_var","verbatimLocation", "country", "continent", "taxonID", "Taxon",
                                                   "habitat",	"firstRecordEvent",	"verbatimFirstRecordEvent", 
                                                   "confidenceFirstRecordEvent",	"occurenceStatus",	"establishmentMeans",
                                                   "degreeOfEstablishment",	"datasetName",	"bibliographicCitation",	
                                                   "accessRights", "order"
  )]
  
  ## Step 2a: Match based on keywords in 'regions' - after "location_var" column
  
  # Check no duplicated locationID in 'regions' (important for unique IDs)
  stopifnot(!anyDuplicated(regions$locationID))
  print("TEST2")
  # Indices of rows in 'regions' with non-missing 'location_var'
  ind_keys_regions <- which(!is.na(regions$location_var))
  
  for (j in ind_keys_regions) {
    current_locID <- regions$locationID[j]
    current_loc <- regions$location[j]
    
    # Safety check: must be scalar
    if (length(current_locID) != 1 || length(current_loc) != 1) {
      warning(sprintf("Skipping region row %d - ambiguous locationID or location", j))
      next
    }
    
    # Split the semicolon-separated keywords and trim whitespace
    location_vars <- unlist(strsplit(regions$location_var[j], ";\\s*"))
    
    for (k in location_vars) {
      ind_match <- which(dat_match_regions$locality == k)
      
      if (length(ind_match) > 0) {
        # Use data.table's set() to assign by reference and avoid recycling errors
        set(dat_match_regions, i = ind_match, j = "locality", value = rep(current_loc, length(ind_match)))
        set(dat_match_regions, i = ind_match, j = "locationID", value = rep(current_locID, length(ind_match)))
      }
    }
  }
  
  ## Step 2b: Match based on keywords in 'subregions' - after "gadm1_var" column
  
  # Identify rows with non-missing gadm1_var
  ind_keys_subregions <- which(!is.na(subregions$gadm1_var))
  
  # Safety check: locationID should be unique
  stopifnot(!anyDuplicated(subregions$locationID))
  print("TEST3")
  # Loop through relevant subregions
  for (j in ind_keys_subregions) {
    current_locID <- subregions$locationID[j]
    current_loc <- subregions$location[j]
    current_gadm1 <- subregions$gadm1_name[j]
    
    # Make sure values are scalar
    if (length(current_locID) != 1 || length(current_loc) != 1 || length(current_gadm1) != 1) {
      warning(sprintf("Skipping subregion row %d - ambiguous data in locationID, location, or gadm1_name", j))
      next
    }
    
    # Split gadm1_var variations
    gadm1_vars <- unlist(strsplit(subregions$gadm1_var[j], ";\\s*"))
    
    for (k in gadm1_vars) {
      ind_match <- which(dat_match_subregions$locality == k)
      
      if (length(ind_match) > 0) {
        # Safe assignment with rep()
        set(dat_match_subregions, i = ind_match, j = "gadm1_name", value = rep(current_gadm1, length(ind_match)))
        set(dat_match_subregions, i = ind_match, j = "locality", value = rep(current_loc, length(ind_match)))
        set(dat_match_subregions, i = ind_match, j = "locationID", value = rep(current_locID, length(ind_match)))
      }
    }
  }
  
  
  ## Merge both data frames with standardized locations names ('region' and 'subregion')
  dat_match1 <- full_join(dat_match_subregions, 
                          dat_match_regions |> select(order, locationID, locality), 
                          by="order") |> 
    mutate(
      locationID = coalesce(locationID.x, locationID.y),
      Location   = coalesce(locality.x, locality.y)
    )
  
  print("TEST4")
  ## final merging of both data sets with standardized region names to original data
  dat_match1 <- dat_match1[order(dat_match1$order),]
  if (!identical(dat_match1$Taxon,dataset$Taxon)) stop("Data sets not sorted equally!")
  dat_match1 <- dat_match1[, c("locationID.y", "locality.x", "location_var","verbatimLocation", "country", "continent", "taxonID", "Taxon",
                               "habitat",	"firstRecordEvent",	"verbatimFirstRecordEvent", 
                               "confidenceFirstRecordEvent",	"occurenceStatus",	"establishmentMeans",
                               "degreeOfEstablishment",	"datasetName",	"bibliographicCitation",	
                               "accessRights", "order"
  )]
  print(names(dat_match1))
  
  fwrite (dat_match1, "dat_match1.csv")
  dataset$locationID <- dat_match1$locationID
  dataset$locality <- dat_match1$locality
  dataset$country <- dat_match1$gadm1_name
  ## Check if any locations in the original dataframe correspond to duplicated locations in the world. 
  if (any(dataset$verbatimLocation %in% dup)) {
    # Extract the matching names from dataset$verbatimLocation
    matching_names <- unique(dataset$verbatimLocation[dataset$verbatimLocation %in% dup])
    warning(paste(
      "\n    Warning: Unresolved terms in. The following location name(s) correspond to multiple subregions in the world:",
      paste(matching_names, collapse = ", "),
      ". Please modify the original location name(s) by including the country name in parentheses(), and try again (e.g: Amazonas (Colombia). ) \n"
    ))
  }
  
  #dat_regnames <- merge(dataset,regions[,c("locationID","location")],by.x="region",by.y="location",all.x=T)
  dat_regnames <- dataset
  ## Remove duplicated entries
  dat_regnames <- dat_regnames[!duplicated(dat_regnames), ]
  write_regnames <- dat_regnames
  
  fwrite(write_regnames, "tmp/hereherehere.csv")
  
  # Clean locations and locationID 
  
  #    write_regnames <- write_regnames |> 
  #      select(-c(country, locationID)) |> 
  #      left_join(regions |> select(location, locationID), by = c("verbatimLocation" = "location"))
  
  
  #    write_regnames <- write_regnames |> 
  #      left_join(regions |> select(locationID, location), by = "locationID", suffix = c("", "_region"))
  
  
  
  print(names(write_regnames))
  print(names(regions))
  # Clean locations and locationID 
  #   write_regnames <- write_regnames |> 
  #      select(-c(locationID)) |> 
  #      left_join(regions |> select(location, locationID), by = "locationID")
  write_regnames <- write_regnames |> 
    select(-c(locationID)) |> 
    left_join(regions |> select(location, locationID), by = c("verbatimLocation" = "location"))
  
  fwrite(write_regnames, "tmp/regnames.csv")


## output ###############################################################################

# Output: Save the file with standardized location names
#    write.table(write_regnames,file.path("Output","Intermediate",paste0("Step3_StandardLocationNames_",dataset[i,"Dataset_brief_name"],".csv")),row.names=F)
fwrite(write_regnames, "tmp/fr_main_dataset_step4.csv")

# Check and export missing locations
missing <- dat_regnames$verbatimLocation[is.na(dat_regnames$locationID)]
if (length(missing) > 0) {
  missing <- sort(unique(missing))
  fwrite(data.table(verbatimLocation = missing), "tmp/fr_check_missing_locations.csv")
}
#      fwrite(missing, "tmp/fr_check_missing_locations.csv")
#      write.table(sort(unique(missing)),file.path("Output","Check",paste0("Missing_Locations_",dataset[i,"Dataset_brief_name"],".csv")),row.names = F,col.names=F)


## Post-processing: Aggregate and export changed location names
if (nrow(dat_regnames)>0){ # avoid step when no region names have changed
  reg_names <- dataset[, c("Location", "verbatimLocation")]
  reg_names <- reg_names[reg_names$Location != reg_names$verbatimLocation, ]
  reg_names <- unique(reg_names[order(reg_names$Location), ])
}
#    reg_names <- vector()
#   for (i in 1:length(inputfiles)){
#    dataset <- read.table(file.path("Output","Intermediate",paste0("Step3_StandardLocationNames_",dataset,".csv")),stringsAsFactors = F,header=T)
#      reg_names <- rbind(reg_names,cbind(dataset[,c("Location","verbatimLocation")],dataset[i,1]))
#    }
#    reg_names <- reg_names[reg_names$Location!=reg_names$verbatimLocation,] # export only region names deviating from the original
#    reg_names <- unique(reg_names[order(reg_names$Location),])
#    colnames(reg_names) <- c("Location","verbatimLocation","origDB")

# Clean locations and locationID 
reg_names  <- reg_names |> left_join(regions |> select(location, locationID), by = c("Location" = "location"))
fwrite(reg_names, "tmp/fr_translated_locationNames.csv")
#    write.table(reg_names,file.path("Output","Translated_LocationNames.csv"),row.names=F)
return(dataset)
 }


