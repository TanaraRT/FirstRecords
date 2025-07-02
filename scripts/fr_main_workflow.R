##########################################################################
##                                                                      ##
##                        FIRST RECORDS WORKFLOW                        ##
##                            Main workflow                             ##
##                   -----------------------------                      ##
##                                                                      ##
## H. Seebens, T. Renard Truong                                         ##
## v2.0, 2025                                                           ##
##########################################################################

## 1) PREPARE WORKSPACE AND IMPORT DATA ##################################

cat("\nSTEP 1: Prepare workspace and import data") 

# clean workspace
graphics.off()
rm(list = ls())

# call libraries
library(Hmisc)
library(stringr)
library(stringi)
library(data.table)
library(rgbif)
library(worrms)
library(openxlsx)

# import data
fr_raw_data <- read.csv2('data/raw/IntroData_raw.csv', fileEncoding = "UTF-8",
  stringsAsFactors = FALSE
)

# convert to data.table
setDT(fr_raw_data) 

# load functions
source("scripts/fr_prepare_mdataset.r")
source("scripts/fr_taxons_standard.r")
source("scripts/checkGBIFTax.r")
source("scripts/StandardiseLocationNames.r")

cat("\nStep 1 completed: library, functions and raw data loaded\n ") 


## 2) PREPARATION OF DATASET #############################################
cat("\nSTEP 2: Prepare main dataset: fr_mdataset") 
fr_mdataset <- fr_prepare_mdataset(fr_raw_data)

## 3) STANDARDIZATION OF TAXA ############################################
cat("\nSTEP 3: Standardize taxa\n") 
fr_mdataset_step3a <- fr_taxons_standard (fr_mdataset)
fr_mdataset_step3b <-fr_mdataset_step3a$taxon_dataset

# Step 4
cat("\nSTEP 4: Standardize localities\n") 
fr_mdataset_step4 <- StandardiseLocationNames(fr_mdataset)





## 4) STANDARDIZATION OF LOCALITIES
#cat("\nSTEP 3: Standardize taxa\n") 
#fr_mdataset_step4 <-StandardiseLocationNames (taxon_dataset)




