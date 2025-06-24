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

# import data
fr_raw_data <- read.csv2('data/raw/IntroData_raw.csv', fileEncoding = "UTF-8",
  stringsAsFactors = FALSE
)

# convert to data.table
setDT(fr_raw_data) 

# load functions
source("scripts/fr_prepare_mdataset.r")
source("scripts/fr_taxons_standard.r")
source("scripts/checkGBIFtaxa.r")

cat("\nStep 1 completed: library, functions and raw data loaded") 


## 2) PREPARATION OF DATASET #############################################
cat("\nSTEP 2: Prepare main dataset: fr_mdataset") 
fr_mdataset <- fr_prepare_mdataset(fr_raw_data)

## 3) STANDARDIZATION OF TAXA ############################################
cat("\nSTEP 3: Standardize taxa") 
fr_mdataset_step1 <- fr_taxons_standard (fr_mdataset)

