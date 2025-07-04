
####### SInAS workflow: Integration and standardisation of alien species data ###########
##
## Step 2c: Standardisation of taxon names
##
## Replacing taxon names by user-defined list "UserDefinedTaxonNames.xlsx"
##
## Hanno Seebens, Gießen, 17.06.2025
#########################################################################################

##########################################################################
##                                                                      ##
##                       FIRST RECORDS WORKFLOW                         ##
##                       Standardize taxonomy 2                         ##
##                   -----------------------------                      ##
##                                                                      ##
## H. Seebens, T. Renard Truong                                         ##
## vx.x, 2025                                                           ##
##########################################################################


OverwriteTaxonNames <- function (dataset, fullspeclist, missing){
  new_names <- read.xlsx("data/external/UserDefinedTaxonNames.xlsx")
  ## replace taxonomic information for each provided new taxon name 
  for (j in 1:nrow(new_names)){
    ## overwrite taxononomic information in individual database files
    dataset[dataset$originalNameUsage==new_names$originalNameUsage[j],]$Taxon <- new_names$New_Taxon[j]
    if (!is.na(new_names$New_scientificName[j])) dataset[dataset$Taxon==new_names$originalNameUsage[j],]$scientificName <- new_names$New_scientificName[j]
      
    ## overwrite taxononomic information in full taxon list
    fullspeclist[fullspeclist$originalNameUsage==new_names$originalNameUsage[j],]$Taxon <- new_names$New_Taxon[j]
    fullspeclist[fullspeclist$originalNameUsage==new_names$originalNameUsage[j],]$GBIFstatus <- ""
    if (!is.na(new_names$New_scientificName[j])) fullspeclist[fullspeclist$originalNameUsage==new_names$originalNameUsage[j],]$scientificName <- new_names$New_scientificName[j]
    if (!is.na(new_names$family[j])) fullspeclist[fullspeclist$originalNameUsage==new_names$originalNameUsage[j],]$family <- new_names$family[j]
    if (!is.na(new_names$order[j])) fullspeclist[fullspeclist$originalNameUsage==new_names$originalNameUsage[j],]$order <- new_names$order[j]
    if (!is.na(new_names$class[j])) fullspeclist[fullspeclist$originalNameUsage==new_names$originalNameUsage[j],]$class <- new_names$class[j]
    if (!is.na(new_names$phylum[j])) fullspeclist[fullspeclist$originalNameUsage==new_names$originalNameUsage[j],]$phylum <- new_names$phylum[j]
    if (!is.na(new_names$kingdom[j])) fullspeclist[fullspeclist$originalNameUsage==new_names$originalNameUsage[j],]$kingdom <- new_names$kingdom[j]
      
    ## remove taxon name from list of missing taxon names
    if (any(new_names$originalNameUsage[j]%in%missing)) missing <- missing[!missing%in%new_names$originalNameUsage[j]]
  }
  fwrite(missing, "tmp/fr_check_tmissing_taxa2b.csv")
  fwrite(dataset, "tmp/fr_main_dataset_step2b.csv")
  
  ## define major taxonomic groups ###################
  fullspeclist$taxaGroup <- NA
  fullspeclist$taxaGroup[fullspeclist$scientificName%in%subset(fullspeclist,class=="Mammalia")$scientificName] <- "Mammals"
  fullspeclist$taxaGroup[fullspeclist$scientificName%in%subset(fullspeclist,class=="Aves")$scientificName] <- "Birds"
  fullspeclist$taxaGroup[fullspeclist$scientificName%in%subset(fullspeclist,class%in%c("Cephalaspidomorphi","Actinopterygii","Elasmobranchii","Sarcopterygii", "Petromyzonti"))$scientificName] <- "Fishes"
  fullspeclist$taxaGroup[fullspeclist$scientificName%in%subset(fullspeclist,order%in%c("Polypteriformes", "Acipenseriformes", "Lepisosteiformes", "Amiiformes", "Osteoglossiformes", "Hiodontiformes", 
                                                                         "Elopiformes", "Albuliformes", "Notacanthiformes", "Anguilliformes", "Saccopharyngiformes", "Clupeiformes", "Ceratodontiformes",
                                                                         "Gonorynchiformes", "Cypriniformes", "Characiformes", "Gymnotiformes", "Siluriformes", "Salmoniformes", "Esociformes", 
                                                                         "Osmeriformes", "Ateleopodiformes", "Stomiiformes", "Aulopiformes", "Myctophiformes", "Lampriformes", "Polymixiiformes", 
                                                                         "Percopsiformes", "Batrachoidiformes", "Lophiiformes", "Gadiformes", "Ophidiiformes", "Mugiliformes", "Atheriniformes", 
                                                                         "Beloniformes", "Cetomimiformes", "Cyprinodontiformes", "Stephanoberyciformes", "Beryciformes", "Zeiformes", 
                                                                         "Gobiesociformes", "Gasterosteiformes", "Syngnathiformes", "Synbranchiformes", "Tetraodontiformes", "Pleuronectiformes", 
                                                                         "Scorpaeniformes", "Perciformes"))$scientificName] <- "Fishes"
  fullspeclist$taxaGroup[fullspeclist$scientificName%in%subset(fullspeclist,class%in%c("Reptilia", "Testudines","Squamata", "Crocodylia"))$scientificName] <- "Reptiles"
  fullspeclist$taxaGroup[fullspeclist$scientificName%in%subset(fullspeclist,class=="Amphibia")$scientificName] <- "Amphibians"
  fullspeclist$taxaGroup[fullspeclist$scientificName%in%subset(fullspeclist, class%in%c("Insecta"))$scientificName] <- "Insects"
  fullspeclist$taxaGroup[fullspeclist$scientificName%in%subset(fullspeclist, class%in%c("Arachnida", "Pycnogonida"))$scientificName] <- "Arachnids"
  fullspeclist$taxaGroup[fullspeclist$scientificName%in%subset(fullspeclist, class%in%c("Collembola", "Chilopoda", "Diplopoda", "Diplura", "Merostomata", "Pauropoda", "Protura", "Symphyla"))$scientificName] <- "Other arthropods"
  fullspeclist$taxaGroup[fullspeclist$scientific%in%subset(fullspeclist,class%in%c("Branchiopoda","Hexanauplia","Maxillopoda","Ostracoda","Malacostraca", "Copepoda"))$scientificName] <- "Crustaceans"
  fullspeclist$taxaGroup[fullspeclist$scientific%in%subset(fullspeclist,family%in%c("Elminiidae"))$scientificName] <- "Crustaceans"
  fullspeclist$taxaGroup[fullspeclist$scientific%in%subset(fullspeclist,phylum=="Mollusca")$scientificName] <- "Molluscs"
  fullspeclist$taxaGroup[fullspeclist$scientificName %in% subset(fullspeclist, phylum %in% "Tracheophyta")$scientificName] <- "Vascular plants"
  fullspeclist$taxaGroup[fullspeclist$scientificName%in%subset(fullspeclist,phylum%in%c("Bryophyta","Anthocerotophyta", "Marchantiophyta"))$scientificName] <- "Bryophytes"
  fullspeclist$taxaGroup[fullspeclist$scientific%in%subset(fullspeclist,phylum%in%c("Rhodophyta","Chlorophyta","Charophyta","Cryptophyta","Haptophyta"))$scientificName] <- "Algae"
  fullspeclist$taxaGroup[fullspeclist$scientific%in%subset(fullspeclist,phylum%in%c("Ascomycota", "Dothideomycetes", "Sordariomycetes", "Chytridiomycota","Basidiomycota","Microsporidia","Zygomycota", "Entomophthoromycota"))$scientificName] <- "Fungi"
  fullspeclist$taxaGroup[fullspeclist$scientific%in%subset(fullspeclist,phylum%in%c("Actinobacteria","Chlamydiae","Cyanobacteria","Firmicutes","Proteobacteria"))$scientificName |
                          fullspeclist$class == "Ichthyosporea"] <- "Bacteria and protozoans"
  fullspeclist$taxaGroup[fullspeclist$scientific%in%subset(fullspeclist,kingdom%in%c("Bacteria", "Protozoa","Euglenozoa"))$scientificName] <- "Bacteria and protozoans"
  fullspeclist$taxaGroup[fullspeclist$scientific%in%subset(fullspeclist,kingdom%in%c("Viruses"))$scientificName] <- "Viruses"
  fullspeclist$taxaGroup[fullspeclist$scientific%in%subset(fullspeclist,phylum%in%c("Annelida", "Nematoda", "Platyhelminthes", "Sipuncula", "Nemertea", "Onychophora", "Acanthocephala"))$scientificName] <- "Annelids, nematodes, platyhelminthes, and other worms"
  fullspeclist$taxaGroup[fullspeclist$scientific%in%subset(fullspeclist,phylum%in%c("Bryozoa", "Entoprocta", "Chaetognatha", "Cnidaria", "Ctenophora", "Echinodermata", "Phoronida", "Porifera", "Rotifera", "Xenacoelomorpha","Brachiopoda"))$scientificName] <- "Other aquatic animals"
  fullspeclist$taxaGroup[fullspeclist$scientific%in%subset(fullspeclist,class=="Ascidiacea")$scientificName] <- "Other aquatic animals"
  fullspeclist$taxaGroup[fullspeclist$scientificName%in%subset(fullspeclist, phylum%in%c("Foraminifera","Cercozoa","Ciliophora","Ochrophyta","Oomycota","Myzozoa","Peronosporea", "Bigyra"))$scientificName] <- "SAR"
  fullspeclist$taxaGroup[fullspeclist$scientificName%in%subset(fullspeclist, genus%in%c("Plasmodium"))$scientificName] <- "SAR"
  
  # write output
  fwrite(fullspeclist, "tmp/fr_fulltaxalist_2b.csv")
  cat("Step 2b completed: wrong species names have been overwritten\n" ) 
  return(dataset)
}