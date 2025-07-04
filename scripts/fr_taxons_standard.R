##########################################################################
##                                                                      ##
##                       FIRST RECORDS WORKFLOW                         ##
##                        Standardize taxonomy                          ##
##                   -----------------------------                      ##
##                                                                      ##
## H. Seebens, T. Renard Truong                                         ##
## vx.x, 2025                                                           ##
##########################################################################

fr_taxons_standard <- function (dataset = NULL){
  if (is.null(dataset) || !is.data.frame(dataset)) {
    stop("Invalid input: dataset must be a data.frame or data.table")
  }
  # Basic data cleaning
  dataset[, Taxon := str_replace_all(
    tolower(trimws(Taxon)), "\\p{Zs}|\\s+", " ")]
  dataset <- dataset[Taxon != "" & !is.na(Taxon)]

  # Apply GBIF function
  gbif_result <- check_GBIF_taxa(taxon_names=dataset, 
               column_name_taxa="Taxon")
  taxon_dataset <- gbif_result[[1]]
  #str(taxon_dataset)
  mismatches <- gbif_result[[2]]
 # str(mismatches)
  
  # Clean taxon_dataset and mismatches
  taxon_dataset <- unique(taxon_dataset) # remove duplicates
  names(taxon_dataset)
  
  taxon_dataset$GBIFstatus[is.na(taxon_dataset$GBIFstatus)] <- "NoMatch"
  #taxon_dataset <- taxon_dataset[,!names(taxon_dataset)%in%c("GBIFstatus",
  #                                                              "GBIFmatchtype",
  #                                                              "GBIFtaxonRank",
  #                                                              "GBIFusageKey",
   #                                                             "GBIFnote",
    #                                                            "GBIFstatus_Synonym",
     #                                                           "species",
      #                                                          "genus","family",
       #                                                        "phylum","kingdom"
        #                                                        )]
  
  oo <- order(mismatches$Taxon)
  mismatches <- unique(mismatches[oo,])
  
  # Export full species list
  fullspeclist <- unique(taxon_dataset)
  
    unique(taxon_dataset[,c("originalNameUsage","Taxon", "scientificName","GBIFstatus",
                            "GBIFstatus_Synonym", "GBIFmatchtype", "GBIFtaxonRank",
                            "GBIFusageKey","GBIFnote","species","genus","family",
                            "order","class","phylum","kingdom"
    )])
  
  oo <- order(fullspeclist$kingdom,fullspeclist$phylum,fullspeclist$class,fullspeclist$order,fullspeclist$Taxon)
  fullspeclist <- unique(fullspeclist[oo,])

  # Save original column order before merging
  fullspeclist[, sequence := .I]
  original_order <- names(fullspeclist)

  ## assign taxon ID unique to individual taxa #############
  ## identify unique taxa (obtained from GBIF)
  fullspeclist[, sequence := .I]  # Assigns row number to a new column "sequence"
  uni_taxa <- unique(na.omit(fullspeclist[, .(scientificName)]))  # Keep unique, non-NA scientific names
  uni_taxa[, taxonID := .I]  # Assign a unique taxonID to each row
  # Merge taxonID into fullspeclist
  fullspeclist_2 <- merge(fullspeclist, uni_taxa, by = "scientificName", all = TRUE)
  fullspeclist_2[is.na(taxonID), taxonID := max(taxonID, na.rm = TRUE) + .I]
  setorder(fullspeclist_2, sequence)
  fullspeclist_2[, sequence := NULL]
  
  # Fix column order: remove 'sequence' before setting column order
  original_order <- setdiff(original_order, "sequence")
  setcolorder(fullspeclist_2, c(original_order, "taxonID"))
  
  # Add taxon ID back to dataset
  taxon_id <- unique(fullspeclist_2[, .(taxonID, Taxon)])
  dataset <- merge(dataset, taxon_id, by = "Taxon", all.x = TRUE)
  fwrite(dataset, "tmp/fr_main_dataset1_with_taxon_id.csv")
  
  # Output final result
  fwrite(fullspeclist_2, "tmp/full_taxa_list_2a.csv")
  fwrite(taxon_dataset, "tmp/fr_main_dataset_step2a.csv")
  fwrite(mismatches[order(mismatches$Taxon), ], "tmp/fr_check_missing_taxa_2a.csv")

  cat("Step 2a completed: species names have been standardized\n") 
  
#return (fullspeclist_2)
  return(list(
    taxon_dataset = taxon_dataset,
    mismatches = mismatches,
    fullspeclist = fullspeclist_2
  ))
  }