
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
  
  ## load location tables #################################################
  # load table with countries
  regions <- read.xlsx("data/external/AllLocations.xlsx") # Sheet with first aggregation level (i.e. countries)
  regions <- regions[, c("locationID", "location", "location_var")]
  regions$location_var <- tolower(regions$location_var)  # Set all to lowercase for matching
  regions$Location_lower <- tolower(regions$location)  # Set all to lowercase for matching
  
  # load table with state, provinces, departments, etc...
  subregions <- read.xlsx("data/external/AllLocations.xlsx") # Sheet with second aggregation level (i.e. states, provinces...)
  subregions <- subregions[, c("locationID", "location", "location_var", "gadm1_name", "gadm1_var")]
  subregions$gadm1_var <- tolower(subregions$gadm1_var)  # Set all to lowercase for matching
  subregions$Gadm1_lower <- tolower(subregions$gadm1_name)  # Set all to lowercase for matching
  
  # Get duplicated names of subregions  
  dup <- unique(gsub("\\s*\\(.*?\\)", "", subregions$gadm1_name)[duplicated(gsub("\\s*\\(.*?\\)", "", subregions$gadm1_name))])
  
  ## loop over all data set ############################################
#  dat <- read.table(file.path("Output","Intermediate",paste0(inputfiles[i])),header=T,stringsAsFactors = F)
    
    # dat <- dat[dat$GBIFstatus!="Missing",]
    #   print(inputfiles[i])
    #   print(dim(dat))
    #   print(length(unique(dat$Species_name_orig)))
    #   print(length(unique(dat$verbatimLocation)))
    # }
    
    # Prepare dataset for matching with regions
    dat_match1 <- dataset ## use another dat set for region matching to keep the original names
    dat_match1$order <- 1:nrow(dat_match1)
    dat_match1$verbatimLocation <- gsub("\\xa0|\\xc2", " ", dat_match1$verbatimLocation)  # Replace special spaces
    dat_match1$verbatimLocation <- gsub("^\\s+|\\s+$", "", dat_match1$verbatimLocation)  # Trim leading/trailing whitespace
    dat_match1$verbatimLocation <- gsub("  ", " ", dat_match1$verbatimLocation)  # Replace double spaces
    dat_match1$verbatimLocation <- gsub(" \\(the\\)", "", dat_match1$verbatimLocation)  # Remove " (the)"
    dat_match1$Location_lower <- tolower(dat_match1$verbatimLocation)  # Lowercase for matching
    
    ## Step 1: Match names of 'dataset' with region names of 'regions' and 'subregions' 
    dat_match_regions <- merge(dat_match1, regions, by.x = "Location_lower", by.y = "Location_lower", all.x = TRUE)
#    dat_match_subregions <- merge(dat_match1, subregions, by.x = "Location_lower", by.y = "Gadm1_lower", all.x = TRUE)
    dat_match_subregions <- merge(dat_match1, subregions, by.x = "locality", by.y = "Gadm1_lower", all.x = TRUE)
    
    ## Step 2a: Match based on keywords in 'regions' - after "location_var" column
    ind_keys_regions <- which(!is.na(regions$location_var))
    for (j in ind_keys_regions) {  # loop over rows with multiple country name variations
      location_var <- unlist(strsplit(regions$location_var[j], "; ")) # check if multiple country name variations provided
      for (k in location_var) {
        ind_match <- which(dat_match_regions$Location_lower == k)
        if (length(unique(regions$location[j])) > 1) 
          cat(paste0("Warning: ", k, " matches multiple location names. Refine location_var!"))
        
        dat_match_regions$location[ind_match] <- regions$location[j]
        dat_match_regions$locationID[ind_match] <- regions$locationID[j]
      }
    }
    
    ## Step 2b: Match based on keywords in 'subregions' - after "gadm1_var" column
    ind_keys_subregions <- which(!is.na(subregions$gadm1_var))
    for (j in ind_keys_subregions) {  # loop over rows with multiple subregion name variations
      gadm1_var <- unlist(strsplit(subregions$gadm1_var[j], "; ")) # check if multiple subregion name variations provided
      for (k in gadm1_var) {
        ind_match <- which(dat_match_subregions$Location_lower == k)
        if (length(unique(subregions$gadm1_name[j])) > 1) 
          cat(paste0("Warning: ", k, " matches multiple location names. Refine gadm1_var!"))
        
        dat_match_subregions$gadm1_name[ind_match] <- subregions$gadm1_name[j]
        dat_match_subregions$location[ind_match] <- subregions$location[j]
        dat_match_subregions$locationID[ind_match] <- subregions$locationID[j]
      }
    }
    
    ## Merge both data frames with standardized locations names ('region' and 'subregion')
    dat_match1 <- full_join(dat_match_subregions, 
                            dat_match_regions |> select(order, locationID, location), 
                            by="order") |> 
    #mutate(locationID = coalesce(as.character(locationID.x), as.character(locationID.y)),
    #         Location = coalesce(location.x, location.y))
    #mutate(locationID = coalesce(locationID.x, locationID.y),
    #         Location = coalesce(location.x, location.y))|> 
    mutate(locationID = coalesce(as.character(locationID.x.x), as.character(locationID.y)),
           Location = coalesce(location.y, location.x))  # location.y seems more complete
    
    # Check column names
    print(names(dat_match1))
    
#      select(-locationID.x, -locationID.y, -location.x, -location.y, -location_var, -gadm1_var)
    

    
    ## final merging of both data sets with standardized region names to original data
    dat_match1 <- dat_match1[order(dat_match1$order),]
    if (!identical(dat_match1$Taxon,dataset$Taxon)) stop("Data sets not sorted equally!")
    
    dataset$locationID <- dat_match1$locationID
    dataset$Location <- dat_match1$Location
    dataset$stateProvince <- dat_match1$gadm1_name
    
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
    
    # Clean locations and locationID 
    write_regnames <- write_regnames |> 
      select(-c(stateProvince, locationID)) |> 
      left_join(regions |> select(location, locationID), by = c("Location" = "location"))

    
    ## output ###############################################################################
    
    # Output: Save the file with standardized location names
#    write.table(write_regnames,file.path("Output","Intermediate",paste0("Step3_StandardLocationNames_",dataset[i,"Dataset_brief_name"],".csv")),row.names=F)
    fwrite(write_regnames, "tmp/fr_main_dataset_step4.csv")
    
    # Check and export missing locations
    missing <- dat_regnames$verbatimLocation[is.na(dat_regnames$locationID)]
    if (length(missing) > 0) {
      missing <- sort(unique(missing))
      fwrite(missing, "tmp/fr_check_missing_locations.csv")
#      write.table(sort(unique(missing)),file.path("Output","Check",paste0("Missing_Locations_",dataset[i,"Dataset_brief_name"],".csv")),row.names = F,col.names=F)
    }
  
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
#  }
}

