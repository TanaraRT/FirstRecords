##########################################################################
##                                                                      ##
##                       FIRST RECORDS WORKFLOW                         ##
##                        Standardize taxonomy                          ##
##                   -----------------------------                      ##
##                                                                      ##
## H. Seebens, T. Renard Truong                                         ##
## vx.x, 2025                                                           ##
##########################################################################

fr_taxons_standard <- function (fr_mdataset){
  fr_mdataset <- data.table::fread("tmp/fr_mdataset_step1.csv")
  
  # Basic data cleaning
  fr_mdataset[, Taxon := str_replace_all(
    tolower(trimws(Taxon)), "\\p{Zs}|\\s+", " ")]
  fr_mdataset <- fr_mdataset[Taxon != "" & !is.na(Taxon)]

  # Apply GBIF function
  gbif_result <- CheckGBIFTax(taxon_names=fr_mdataset, 
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
  
  # Add taxon ID back to dataset
  taxon_id <- unique(fullspeclist_2[, .(taxonID, Taxon)])
  fr_mdataset <- merge(fr_mdataset, taxon_id, by = "Taxon", all.x = TRUE)
  fwrite(fr_mdataset, "tmp/fr_mdataset_step2.csv")
  
  # Output final result
  fwrite(fullspeclist_2, "tmp/fr_full_taxa_list.csv")
  fwrite(taxon_dataset, "tmp/fr_taxon_standards_intermediate.csv")
  fwrite(mismatches[order(mismatches$Taxon), ], "tmp/fr_check_missing_taxa.csv")

  cat("\nStep 3a completed: species names have been standardized\n" ) 
  

  return(list(
    taxon_dataset = taxon_dataset,
    mismatches = mismatches,
    fullspeclist = fullspeclist_2
  ))
  }