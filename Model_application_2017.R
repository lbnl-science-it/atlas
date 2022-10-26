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
controls <- list()
controls[[1]] <- 5   # UNO - One (Column Index)
controls[[2]] <- 6   # SERO - Zero (Column Index)
controls[[3]] <- 7   # Case ID (Column Index)
controls[[4]] <- 25  # Number of Replications
controls[[5]] <- 1   # Number of Outside Goods
controls[[6]] <- 4   # Configuration (1,4,5,6, or 7)

Tolerance <- 0.035 # 0.035; # Tolerance limit to match predicted bodytype distribution
max_iteration <- 10 #5; # Maximum number of iterations for the mileage re-allocation algorithm
nc <- 14;

N_fail <- 0 # number of non-convergence


## apollo global control variables
apollo_control <- list(
  modelName  ="nhts2017",
  modelDescr ="MDCEV model on vehicle mileage data, alpha-gamma profile with outside good and socio-demographics",
  indivID    ="household_id"
)

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta <- c(alpha_base    = 0,
                  gamma_car1    = 1,
                  gamma_car2    = 1,
                  gamma_car3    = 1,
                  gamma_van1    = 1,
                  gamma_van2    = 1,
                  gamma_van3    = 1,
                  gamma_suv1    = 1,
                  gamma_suv2    = 1,
                  gamma_suv3    = 1,
                  gamma_pickup1    = 1,
                  gamma_pickup2    = 1,
                  gamma_pickup3    = 1,
                  gamma_motorbike    = 1,
                  delta_car1    = 0,
                  # delta_car1_income1 = 0,
                  # delta_car1_income4 = 0,
                  # delta_car1_NUMCHILD = 0,
                  # delta_car1_work3 = 0,
                  delta_car2    = 0,
                  # delta_car2_income2   = 0,
                  # delta_car2_work2    = 0,
                  delta_car3    = 0,
                  # delta_car3_income1    = 0,
                  # delta_car3_retired    = 0,
                  delta_van1    = 0,
                  # delta_van1_work2    = 0,
                  # delta_van1_NUMCHILD    = 0,
                  delta_van2    = 0,
                  # delta_van2_income2    = 0,
                  # delta_van2_NUMCHILD    = 0,
                  delta_van3    = 0,
                  # delta_van3_income1    = 0,
                  # delta_van3_HHSIZE1    = 0,
                  delta_suv1    = 0,
                  # delta_suv1_income1    = 0,
                  # delta_suv1_work2    = 0,
                  # delta_suv1_retired    = 0,
                  delta_suv2    = 0,
                  # delta_suv2_income3    = 0,
                  # delta_suv2_HHSIZE4    = 0,
                  # delta_suv2_hhown    = 0,
                  delta_suv3    = 0,
                  # delta_suv3_income4    = 0,
                  # delta_suv3_child    = 0,
                  delta_pickup1    = 0,
                  # delta_pickup1_urban_cbsa    = 0,
                  # delta_pickup1_HHSIZE1    = 0,
                  # delta_pickup1_income5    = 0,
                  delta_pickup2    = 0,
                  # delta_pickup2_child    = 0,
                  # delta_pickup2_income1    = 0,
                  delta_pickup3    = 0,
                  # delta_pickup3_income2    = 0,
                  # delta_pickup3_child    = 0,
                  delta_motorbike  = 0,
                  # delta_motorbike_HHSIZE1  = 0,
                  # delta_motorbike_urban_cbsa  = 0,
                  sigma              = 1)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed <- c("alpha_base", "sigma")


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

