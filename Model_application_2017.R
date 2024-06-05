###################################
###Codes for running application###
###################################
hh.masterdat = households; rm(households)
pp.masterdat = persons; rm(persons)

## import functions for all steps
source(paste0('Model_application_functions_',diryear, '.R'))

## pre-set values
num_alt <- 6  # number of alternatives for step 2 estimation (6+)
num_type <- 5 # number of bodytypes for step 5 estimation (5+)
num_power <- 4 # number of powertrain for step 7 estimation (5+)
controls <- list() # MDCEV controls
controls[[1]] <- 5   # UNO - One (Column Index)
controls[[2]] <- 6   # SERO - Zero (Column Index)
controls[[3]] <- 7   # Case ID (Column Index)
controls[[4]] <- 25  # Number of Replications
controls[[5]] <- 1   # Number of Outside Goods
controls[[6]] <- 4   # Configuration (1,4,5,6, or 7)

Tolerance <- 0.035 # 0.035; # Tolerance limit to match predicted bodytype distribution
max_iteration <- 10 #5; # Maximum number of iterations for the mileage re-allocation algorithm
nc <- 14;

N_fail <- 0 # initialize number of non-convergence to record during run time

## load coefficients and process them [Don't need to modify]
# load(file.path(coefdir, "coefs_apollo.RData"))
load(paste0('coefs_', diryear, '.RData'))

# 1
names(coef1)[1] <- "const"
coefs_name_mile <- names(coef1)
coefs_mile <- unname(coef1)
# 2
coef_names_veh <- coef2_prepare(coef2)[[1]]
coef_values_veh <- coef2_prepare(coef2)[[2]]
# 5
coef_names_type <- coef5_prepare(coef5)[[1]]
coef_values_type <- coef5_prepare(coef5)[[2]]
# 6
coef_names_car <- names(coef6_1)
coef_names_car[1] <- "const"
coefs_car <- unname(coef6_1)
coef_names_van <- names(coef6_2)
coef_names_van[1] <- "const"
coefs_van <- unname(coef6_2)
coef_names_suv <- names(coef6_3)
coef_names_suv[1] <- "const"
coefs_suv <- unname(coef6_3)
coef_names_pick <- names(coef6_4)
coef_names_pick[1] <- "const"
coefs_pick <- unname(coef6_4)
# 7
coef_names_power <- coef7_prepare(coef7)[[1]]
coef_values_power <- coef7_prepare(coef7)[[2]]

