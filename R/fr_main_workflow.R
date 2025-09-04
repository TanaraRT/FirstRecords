##########################################################################
##                                                                      ##
##                        FIRST RECORDS WORKFLOW                        ##
##                            Main workflow                             ##
##                   -----------------------------                      ##
##                                                                      ##
## T. Renard Truong, H. Seebens                                         ##
## v2.0, August 2025                                                    ##
##########################################################################

## PREPARE WORKSPACE AND IMPORT DATA #####################################

cat("\n Initialization") 

# --- Clean workspace ---
graphics.off()
rm(list = ls())


# please define "data_dir" folder ("data" by default)
data_dir = "data"




# --- Workflow start --- #######################################################


# --- Prepare workspace and import data ---
source(file.path("R", "fr_initialization.r"))

# --- Check folders and load packages and scripts --- #
init <- fr_initialization(data_dir = data_dir) # HANNO: I think that an init file is not necessary any more as I removed the folder paths.

cat("\nIntialization completed\n ") 
## 1) PREPARATION OF DATASET #############################################
cat("\nSTEP 1: Prepare main dataset") 

fr_main_dataset_1 <- fr_prepare_main_dataset(dataset = init$fr_input_data,
                                             use_log = TRUE, # TRUE to record progress in log file in 'output' folder
                                             save_to_disk = TRUE, # TRUE to save fr_main_dataset_1 in 'tmp' folder
                                             data_dir = data_dir)
cat("\nStep 1 completed: main dataset 'fr_main_dataset' ready to be processed\n ") 

## 2) STANDARDIZATION OF TAXA ############################################
cat("\nSTEP 2: Standardize taxa") 
fr_main_dataset_2 <- fr_taxons_standard(dataset = fr_main_dataset_1,
                                        use_log = TRUE, # TRUE to record progress in log file in 'output' folder
                                        save_to_disk = TRUE, # TRUE to save fr_main_dataset_2 in 'tmp' folder
                                        data_dir = data_dir) 
cat("\nStep 2 completed: taxa have been standardized in 'fr_main_dataset_2'. Unmatched taxa are available in the 'tmp' folder and a taxonomy table is available in the 'output' folder\n ")

## 3) STANDARDIZATION OF YEARS############################################
cat("\nSTEP 3: Standardize years") 
fr_main_dataset_3 <- fr_years_standard(dataset = fr_main_dataset_2, 
                                       fr_column_name = "firstRecordEvent", 
                                       use_log = TRUE, # TRUE to record progress in log file in 'output' folder
                                       save_to_disk = TRUE, # TRUE to save fr_main_dataset_2 in 'tmp' folder
                                       data_dir = data_dir) 
cat("\nStep3 completed: first records (years) have been standardized in 'fr_main_dataset_3'. Years that couldn't be standardized are available in the 'tmp' folder\n ")

## 4) STANDARDIZATION OF LOCALITIES #######################################
cat("\nSTEP 4: Standardize localities") 
fr_main_dataset_4 <- fr_localities_standard(fr_main_dataset_3, 
                                            use_log = TRUE, # TRUE to record progress in log file in 'output' folder
                                            save_to_disk = TRUE, # TRUE to save fr_main_dataset_2 in 'tmp' folder
                                            data_dir = data_dir) 
cat("\nStep 4 completed: locations have been standardized and the location table is available in the 'output' folder\n ") 

## 5) STANDARDIZATION OF REMAINING TERMS ################################
fr_final_dataset <- fr_terms_standard(fr_main_dataset_4, 
                                      use_log = TRUE, # TRUE to record progress in log file in 'output' folder
                                      data_dir = data_dir) 
cat("\n  Final dataset available in the 'output' folder\n ")
