# set up directories and load libraries for us_atlas_v1


# libraries
library(stats) # LJ add, predict function is used for predicting main driver and ownlease
library(data.table)
library(tidyverse)
library(dplyr)
library(apollo)
library(tictoc)
library(parallel)
library(doParallel)
library(foreach)




# directories:
# powertrain calibration data dir:
power_ctrl_dir <- file.path(inputdir, 'Calibration_Data')

# dir for submodel specific functions
submodel_func_dir <- file.path(codedir,'atlas_v1_code')

# dir for submodel specific coefficients
submodel_coef_dir <- file.path(codedir,'atlas_v1_coefs')

# dir for auxiliary variable inputs (e.g. location attributes, variables other than synthetic population)
auxiliary_input_dir <- file.path(inputdir, 'Auxiliary_Data')

# dir for synthetic households and persons
synthpop_input_dir <- file.path(inputdir,  'Synthetic_Population',paste0("State_", select.state.fips))


# source the global parameters
source('parameters_us_atlas_v1.R')

# source functions and load coefs.

# 1 PTR_HH_Mileage ####
source(paste(c(submodel_func_dir,'/1_Mileage_prediction.R'),collapse=''))

load(file.path(submodel_coef_dir, "coef1.rda"))
names(coef1)[1] <- "const"

coefs_name_mile <- names(coef1)
coefs_mile <- unname(coef1)

# 2 MNL_Num_of_Alternatives ####
source(paste(c(submodel_func_dir,'/2_Number_of_alternatives.R'),collapse=''));

load(file.path(submodel_coef_dir, "coef2.rda"))
num_alt <- 6

coef_veh <- coef2_prepare(coef2, num_alt)

coef_names_veh  <- coef_veh[[1]]
coef_values_veh <- coef_veh[[2]]

# 3 MDCEV_Vehicle_fleet ####
load(file.path(submodel_coef_dir, "coef3_full.rda"))
source(paste0(submodel_func_dir, '/3_Vehicle_fleet_mix_apollo_full.R'))

# 4. HMR
source(paste(c(submodel_func_dir,'/4_Heuristic_mileage_reallocation.R'),collapse=''));

# 5. number of bodytypes
source(paste(c(submodel_func_dir,'/5_Number_of_bodytypes.R'),collapse=''));

load(file.path(submodel_coef_dir, "coef5.rda"))
num_type <- 5

coef_type <- coef5_prepare(coef5, num_type)
coef_names_type  <- coef_type[[1]]
coef_values_type <- coef_type[[2]]

# 6. car count
source(paste(c(submodel_func_dir,'/6.1_Car_count.R'),collapse=''));

load(file.path(submodel_coef_dir, "coef6_car.rda"))

coef_names_car <- names(coef6_car)
coef_names_car[1] <- "const"
coefs_car <- unname(coef6_car)

# 7. van count
source(paste(c(submodel_func_dir,'/6.2_Van_count.R'),collapse=''));

load(file.path(submodel_coef_dir, "coef6_van.rda"))
coef_names_van <- names(coef6_van)
coef_names_van[1] <- "const"
coefs_van <- unname(coef6_van)

# 8. suv count
source(paste(c(submodel_func_dir,'/6.3_SUV_count.R'),collapse=''));

load(file.path(submodel_coef_dir, "coef6_suv.rda"))
coef_names_suv <- names(coef6_suv)
coef_names_suv[1] <- "const"
coefs_suv <- unname(coef6_suv)

# 9. pickup count
source(paste(c(submodel_func_dir,'/6.4_Pickup_count.R'),collapse=''));

load(file.path(submodel_coef_dir, "coef6_pickup.rda"))
coef_names_pick <- names(coef6_pickup)
coef_names_pick[1] <- "const"
coefs_pick <- unname(coef6_pickup)

# 10. power train
source(paste(c(submodel_func_dir,'/7_Powertrain_distribution.R'),collapse=''));

load(file.path(submodel_coef_dir, "coef7.rda"))


coef_names_power  <- coef7_prepare(coef7)[[1]]
coef_values_power <- coef7_prepare(coef7)[[2]]

# 11. source data prep function
source(file.path(submodel_func_dir,"data_prep_us_atlas_v1.R"))

# 12. source the model application function
source(file.path(submodel_func_dir,"run_func_us_atlas_v1.R"))

