
## load CheckGBIFTax.R first before running this script.


library(rgbif)
source("scripts/CheckGBIFTax.r")

## example with a data.frame as input ##########################################
all_names <- data.frame(Taxon=c("Urtica dioica", 
                                         "Pterois volitans", 
                                         "Bimastos parvus", 
                                         "Nelima semproni Szalay, 1951", 
                                         "Chloris chloris", 
                                         "Abutilon aurantiacum"))

standardised_names <- CheckGBIFTax(taxon_names=all_names, 
                                   column_name_taxa="Taxon")
standardised_names


## example with a vector as input ##############################################
all_names <- c("Urtica dioica", 
               "Pterois volitans", 
               "Bimastos parvus", 
               "Nelima semproni Szalay, 1951", 
               "Chloris chloris", 
               "Abutilon aurantiacum")

standardised_names <- CheckGBIFTax(taxon_names=all_names, 
                                   column_name_taxa="Taxon")
standardised_names