rm(list = ls())
gc()


# Set county to predict
county <- "Alameda"
select.county.fips <- '001'
select.state.fips <- '06'

# power calibration threshold
power.threshold <- 0.001 # 0.001 rmse error threshold for powertrain calibration

# powertrain calibration data dir:
power_ctrl_dir <- '/Users/lingjin/Library/CloudStorage/GoogleDrive-ljin@lbl.gov/Shared drives/Shared_ATLAS/Naomi/Project_Deliverable/Data/Validation_Data/Vehicle_Registration/Experian_US/Clean/'



# set location of the model data folder in Google Drive (either relative or absolute path)
data_folder <- paste0("/Users/lingjin/Dropbox/Research/SmartGrid_Behavioral/TransportationInitiative/ATLAS/ATLAS-National/Rscripts/Replicate_Naomi_CA/")
setwd(data_folder)

# 0 Model Initialization ####
source("code/init.R")

# Load the data set
source("code/data_prep.R")

# 1 PTR_HH_Mileage ####
print("Running mileage prediction model .......")

tt1 <- mileage_model(data, coefs_name_mile, coefs_mile)
data_temp <- cbind(data, tt1)
rm(tt1)


# 2 MNL_Num_of_Alternatives ####
print("Running MNL model for the number of alternatives .......");

tt1 <- NumAlternative_model(data_temp, coef_names_veh, coef_values_veh);
data_temp <<- cbind(data_temp, tt1);

# generate variable isVehicle (indication if the household has any vehicle or not)
data_temp <- data_temp %>% 
  mutate(isVehicle = case_when(pred_own == 0 ~ 0, TRUE ~ 1))
rm(tt1);

# 3 MDCEV_Vehicle_fleet ####

library(apollo)

print("Running MDCEV forecasting code.......");

# data_temp <- data_temp %>% dplyr::mutate(annual_nonmotor_new=0, car1=0, car2=0, car3=0, van1=0,
#                                          van2=0, van3=0, suv1=0, suv2=0, suv3=0,
#                                          pickup1=0, pickup2=0, pickup3=0, motorbike=0)
# 
# if (!("budget" %in% colnames(data_temp))) {
#   database <- data_temp %>% mutate(budget = 
#                                       car1 + car2 + car3 + van1 + van2 +
#                                       van3 + suv1 + suv2 + suv3 + pickup1 +
#                                       pickup2 + pickup3 + motorbike +
#                                       annual_nonmotor_new)
# } else {
#   database <- data_temp
# }
# 
# database <- database %>% rename(outside = annual_nonmotor_new)

# LJ: move the database part to the function in 3_Vehicle_fleet_mix_apollo_full.R
data_temp <- data_temp %>% dplyr::mutate(annual_nonmotor_new=0, car1=0, car2=0, car3=0, van1=0,
                                         van2=0, van3=0, suv1=0, suv2=0, suv3=0,
                                         pickup1=0, pickup2=0, pickup3=0, motorbike=0)




mdcev_predicted <- VehicleFleet_model(data_temp, model3_mdcev_full_est);

melted_preds <- melt(
  setDT(mdcev_predicted),
  id.vars = c("ID", "Observation"),
  variable.name = "outcome"
  )

melted_preds[, c("outcome", "parameter", "measure") := tstrsplit(outcome, "_")]
cast_preds <- dcast(melted_preds, ID + Observation + outcome ~ parameter + measure)

mdcev <- dcast(cast_preds, ID ~ outcome, value.var = "cont_mean")

mdcev[, AVG := 1]
setcolorder(mdcev, c("ID", "AVG", "outside", "car1","car2","car3","van1","van2","van3",
                     "suv1","suv2","suv3","pickup1","pickup2","pickup3","motorbike"))

# mdcev <- mdcev_predicted[,c(1,3:16)]
# names(mdcev) <- c("ID", "outside", "car1","car2","car3","van1","van2","van3","suv1","suv2","suv3","pickup1","pickup2","pickup3","motorbike")
# mdcev <- mdcev %>% mutate(.after="ID", AVG = 1)

mdcev <- as.data.frame(mdcev)

# data_temp <- data_temp[,1:68]
remove <- c(
  "isVehicle", "annual_nonmotor_new",
  "car1", "car2", "car3",
  "van1", "van2", "van3",
  "suv1", "suv2", "suv3",
  "pickup1", "pickup2", "pickup3",
  "motorbike"
)
data_temp <- data_temp %>% select(-all_of(remove))
rm(remove)

# 4 Heuristic_Mileage_reallocation ####
# Heuristic mileage reallocation algorithm
# Need to be changed based on different specifications

print("Running heuristic mileage reallocation algorithm....");

# 5 MNL_Num_of_Vehicle_body_types ####
# MNL model for number of vehicle body types
# Calibrated coefficients
print("Checking tolerance limits .......");

indfor <- NULL;
tt3 <- NULL;

iter <<- 1
diff_body <<- c(1.0, 1.0, 1.0, 1.0, 1.0); # LJ change 9/13/2021: initialize diff_body here.

while ((iter <= max_iteration) & (diff_body[1] > Tolerance | 
                                  diff_body[2] > Tolerance | 
                                  diff_body[3] > Tolerance | 
                                  diff_body[4] > Tolerance | 
                                  diff_body[5] > Tolerance)) {
  print(paste("Iteration", iter));
  
  tt2 <- Heuristic_mileage(data_temp, mdcev);
  indfor <- tt2[[1]];
  pnumpercent <- tt2[[2]];
  pavgmileage <- tt2[[3]];
  tt3 <- NumBodytypes(data_temp, indfor, pnumpercent, pavgmileage, coef_names_type, coef_values_type);
  names(tt3) <- c("NumBodyType_HMR","NumBodyType_MNL");
  
  print(diff_body);
  iter <- iter+1;
}

if(iter > max_iteration){
  # N_fail <<- N_fail + 1
  print("Consider adjusting the tolerance criteria and/or re-calibrate MDCEV model");
}

#Sys.sleep(3); # LJ change 9/13/2021 remove sleep time

indfor$ID <- NULL;
# names(indfor) <- c("NONMOTOR_HU","CAR1_HU","CAR2_HU","CAR3_HU","VAN1_HU","VAN2_HU","VAN3_HU","SUV1_HU","SUV2_HU","SUV3_HU","PICK1_HU",
#                    "PICK2_HU","PICK3_HU","MOTER_HU");
# data_temp <- cbind(data_temp,indfor);
data_temp <- cbind(data_temp, tt3);
rm(tt3);
names(indfor) <- c("nonmotor","car1","car2","car3","van1","van2","van3","suv1","suv2","suv3","pickup1",
                   "pickup2","pickup3","motor");

data_temp$ycar <- indfor$car1 + indfor$car2 + indfor$car3 #ycar is indication that (predicted) car exist
data_temp$ycar[data_temp$ycar > 0] <- 1
data_temp$yvan <- indfor$van1 + indfor$van2 + indfor$van3
data_temp$yvan[data_temp$yvan > 0] <- 1
data_temp$ysuv <- indfor$suv1 + indfor$suv2 + indfor$suv3
data_temp$ysuv[data_temp$ysuv > 0] <- 1
data_temp$ypickup <- indfor$pickup1 + indfor$pickup2 + indfor$pickup3
data_temp$ypickup[data_temp$ypickup > 0] <- 1

data_temp$ycar1 <- indfor$car1 #ycar1 is indication that (predicted) car 0-5 years exist
data_temp$ycar1[data_temp$ycar1 > 0] <- 1
data_temp$yvan1 <- indfor$van1
data_temp$yvan1[data_temp$yvan1 > 0] <- 1
data_temp$ysuv1 <- indfor$suv1
data_temp$ysuv1[data_temp$ysuv1 > 0] <- 1
data_temp$ypickup1 <- indfor$pickup1
data_temp$ypickup1[data_temp$ypickup1 > 0] <- 1

data_temp$ycar2 <- indfor$car2 #ycar2 is indication that (predicted) car 6-11 years exist
data_temp$ycar2[data_temp$ycar2 > 0] <- 1
data_temp$yvan2 <- indfor$van2
data_temp$yvan2[data_temp$yvan2 > 0] <- 1
data_temp$ysuv2 <- indfor$suv2
data_temp$ysuv2[data_temp$ysuv2 > 0] <- 1
data_temp$ypickup2 <- indfor$pickup2
data_temp$ypickup2[data_temp$ypickup2 > 0] <- 1

data_temp$ycar3 <- indfor$car3 #ycar3 is indication that (predicted) car 12+ years exist
data_temp$ycar3[data_temp$ycar3 > 0] <- 1
data_temp$yvan3 <- indfor$van3
data_temp$yvan3[data_temp$yvan3 > 0] <- 1
data_temp$ysuv3 <- indfor$suv3
data_temp$ysuv3[data_temp$ysuv3 > 0] <- 1
data_temp$ypickup3 <- indfor$pickup3
data_temp$ypickup3[data_temp$ypickup3 > 0] <- 1



# 6.1 OP_Car_count ####
# Ordered probit Model for car count
# Calibrated coefficients

print("Running count models for CAR .......");

tt1 <- car_count_model(data_temp, indfor, coef_names_car, coefs_car);
indfor <- cbind(indfor,tt1);
rm(tt1);



# 6.2 OP_Van_count ####
# Ordered probit model for van count
# Calibrated coefficients

print("Running count models for VAN .......");

tt1 <- van_count_model(data_temp, indfor, coef_names_van, coefs_van);
indfor <- cbind(indfor,tt1);
rm(tt1);  



# 6.3 OP_SUV_count ####
# Ordered probit model for SUV count
# Calibrated coefficients

print("Running count models for SUV .......");

tt1 <- SUV_count_model(data_temp, indfor, coef_names_suv, coefs_suv);
indfor <- cbind(indfor,tt1);
rm(tt1);



# 6.4 OP_Pick-up_count ####
# Ordered probit model for pick-up count
# Calibrated coefficients

print("Running count models for PICKUP .......");

tt1 <- pickup_count_model(data_temp, indfor, coef_names_pick, coefs_pick);
indfor <- cbind(indfor,tt1);
rm(tt1);
data_temp <- cbind(data_temp, indfor) # column bind prediction of mileage and number of vehicle count



# 7. Powertrain ####

vehicles <- generate_vehicles(data_temp)

print('now predicting power train')


tt1 <- powertrain_model(vehicles, coef_names_power, coef_values_power);

# load calibration data for powertrain frctions for this county. 
power_ctrl <- fread(paste0(power_ctrl_dir, "/power_control_bycounty.csv"))

pred.tmp <- tt1 %>%
  group_by(pred_power) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(pred_frac = count / sum(count)) %>%
  rename(power = pred_power)

mod.pred <- power_ctrl %>%
  dplyr::filter(state_fips == as.integer(select.state.fips) & COUNTY_CODE == as.integer(select.county.fips)) 

# only recalibrate if there exist registration data for the county
if(nrow(mod.pred) >0){
  mod.pred <- mod.pred %>%
    dplyr::rename(ctrl_frac = frac_power) %>%
    left_join(pred.tmp, by = "power") %>%
    mutate(across(everything(), ~replace_na(.x, 0))) # replace NA with 0 as it means no such powertrain predicted
  mod.pred = as.data.frame(mod.pred)  
  # get the rmse of powertrain prediction error
  err.metric.power = sqrt(mean(as.matrix(as.data.table((mod.pred[,'ctrl_frac'] - mod.pred[,'pred_frac'])^2))))
  print(paste('predicted power distribution vs control has rmse of', err.metric.power))
  
  it.number = 1
  while(err.metric.power > power.threshold & it.number <10){ # log adjust the constant until the predicted power matches the DMV control fractions
    # note the coef_names_power , coef_values_power are in the following order
    # coefs_name_aev, coefs_name_phev, coefs_name_hybrid , coefs_name_ice
    # coefs_aev, coefs_phev, coefs_hybrid, coefs_ice
    print('recalibrate powertrain prediction')
    # avoid dividing by 0 or log(0) adding a small number 0.00001
     coef_values_power[[1]][1] = coef_values_power[[1]][1] + log(mod.pred$ctrl_frac[mod.pred$power=='AEV'] / (mod.pred$pred_frac[mod.pred$power=='AEV']+0.00001) + 0.00001) 
     coef_values_power[[2]][1] = coef_values_power[[2]][1] + log(mod.pred$ctrl_frac[mod.pred$power=='PHEV'] / (mod.pred$pred_frac[mod.pred$power=='PHEV']+0.00001) + 0.00001) 
     coef_values_power[[3]][1] = coef_values_power[[3]][1] + log(mod.pred$ctrl_frac[mod.pred$power=='Hybrid'] / (mod.pred$pred_frac[mod.pred$power=='Hybrid']+0.00001) + 0.00001) 
     
     tt1 <- powertrain_model(vehicles, coef_names_power, coef_values_power);
    
     pred.tmp <- tt1 %>%
       group_by(pred_power) %>%
       summarise(count = n()) %>%
       ungroup() %>%
       mutate(pred_frac = count / sum(count)) %>%
       rename(power = pred_power)
     
       mod.pred <- mod.pred %>%
         dplyr::select(-pred_frac) %>%
         left_join(pred.tmp, by = "power") %>%
         mutate(across(everything(), ~replace_na(.x, 0))) # replace NA with 0 as it means no such powertrain predicted
        # get the rmse of powertrain prediction error
       err.metric.power = sqrt(mean(as.matrix(as.data.table((mod.pred[,'ctrl_frac'] - mod.pred[,'pred_frac'])^2))))
       print(paste('predicted power distribution vs control has rmse of', err.metric.power))
       it.number = it.number + 1
  }
  
}


vehicles <- cbind(vehicles,tt1);
rm(tt1);

# Results reformat ####
vehicles$annual_mileage <- 0
vehicles$annual_mileage[vehicles$ycar1==1] <- vehicles$car1[vehicles$ycar1==1]/vehicles$ncar1[vehicles$ycar1==1]
vehicles$annual_mileage[vehicles$ycar2==1] <- vehicles$car2[vehicles$ycar2==1]/vehicles$ncar2[vehicles$ycar2==1]
vehicles$annual_mileage[vehicles$ycar3==1] <- vehicles$car3[vehicles$ycar3==1]/vehicles$ncar3[vehicles$ycar3==1]
vehicles$annual_mileage[vehicles$yvan1==1] <- vehicles$van1[vehicles$yvan1==1]/vehicles$nvan1[vehicles$yvan1==1]
vehicles$annual_mileage[vehicles$yvan2==1] <- vehicles$van2[vehicles$yvan2==1]/vehicles$nvan2[vehicles$yvan2==1]
vehicles$annual_mileage[vehicles$yvan3==1] <- vehicles$van3[vehicles$yvan3==1]/vehicles$nvan3[vehicles$yvan3==1]
vehicles$annual_mileage[vehicles$ysuv1==1] <- vehicles$suv1[vehicles$ysuv1==1]/vehicles$nsuv1[vehicles$ysuv1==1]
vehicles$annual_mileage[vehicles$ysuv2==1] <- vehicles$suv2[vehicles$ysuv2==1]/vehicles$nsuv2[vehicles$ysuv2==1]
vehicles$annual_mileage[vehicles$ysuv3==1] <- vehicles$suv3[vehicles$ysuv3==1]/vehicles$nsuv3[vehicles$ysuv3==1]
vehicles$annual_mileage[vehicles$ypickup1==1] <- vehicles$pickup1[vehicles$ypickup1==1]/vehicles$npickup1[vehicles$ypickup1==1]
vehicles$annual_mileage[vehicles$ypickup2==1] <- vehicles$pickup2[vehicles$ypickup2==1]/vehicles$npickup2[vehicles$ypickup2==1]
vehicles$annual_mileage[vehicles$ypickup3==1] <- vehicles$pickup3[vehicles$ypickup3==1]/vehicles$npickup3[vehicles$ypickup3==1]


# now reformat the results and write out tables, note budget is no longer written out
print('Reformat results into vehicle and household tables and write out')
households_output <- vehicles %>%
  group_by(HOUSEID) %>%
  summarise(nvehicles = n(), .groups = "drop")

households_output <- data_temp %>%
  select(HOUSEID) %>%  # Keep only HOUSEID before merging
  merge(households_output, by = "HOUSEID", all.x = TRUE) %>%
  select(HOUSEID, nvehicles)

households_output$nvehicles[is.na(households_output$nvehicles)==T] <- 0

vehicles <- vehicles %>% mutate(VEHAGE0=case_when(VEHAGE==2.5~1, TRUE~0),
                                VEHAGE1=case_when(VEHAGE==8.5~1, TRUE~0),
                                VEHAGE2=case_when(VEHAGE==14.5~1, TRUE~0),
                                bev=case_when(pred_power=="AEV"~1, TRUE~0),
                                hybrid=case_when(pred_power=="Hybrid"~1, TRUE~0),
                                phev=case_when(pred_power=="PHEV"~1, TRUE~0))

vehicles_output <- vehicles %>% 
  # select(HOUSEID, VEHID, VEHAGE:pred_own) %>%
  mutate(bodytype=case_when(car==1~"car", van==1~"van", suv==1~"suv", pickup==1~"pickup", T~"others"),
         vintage_category=case_when(VEHAGE0==1~"0~5 years", VEHAGE1==1~"6~11 years", VEHAGE2==1~"12+ years")) %>% 
  select(HOUSEID, VEHID, bodytype, vintage_category,
         annual_mileage, pred_power,
         VEHAGE0, VEHAGE1, VEHAGE2)

# distribute the vintage prediction to actual model year
# vintage 3: age assignment
vintage3_exp <- 0.200526701  # currently hard coded exponential distribution parameter derived from NHTS 2017 data
outputyear <- 2017
nrow1 <- vehicles_output %>% filter(VEHAGE0==1) %>% nrow()
vehicles_output$modelyear[vehicles_output$VEHAGE0==1] <- outputyear-floor(runif(nrow1, min = 0, max = 4.99999))

nrow2 <- vehicles_output %>% filter(VEHAGE1==1) %>% nrow()
vehicles_output$modelyear[vehicles_output$VEHAGE1==1] <- outputyear-floor(runif(nrow2, min = 5, max = 10.99999))

nrow3 <- vehicles_output %>% filter(VEHAGE2==1) %>% nrow()
vehicles_output$modelyear[vehicles_output$VEHAGE2==1] <- outputyear-floor(rexp(nrow3, rate = vintage3_exp))-11

vehicles_output = vehicles_output %>% select(-VEHAGE0, -VEHAGE1,-VEHAGE2)

# print('map to adopt vehicle type and powertrain type')
# vehicles_output = vehicles_output %>% mutate(adopt_veh = recode(bodytype, 'van'='minvan', 'pickup' = 'truck'), 
#                                              adopt_fuel = recode(pred_power, "AEV"="ev", "Hybrid"="hybrid","ICE"="conv","PHEV"="phev"),
#                                              acquire_year = NA)



# Save final results ####

fwrite(data_temp, file = paste0(tabdir, "prediction_results.csv"), row.names=FALSE, sep=",");
fwrite(households_output, file = paste0(tabdir, "atlas_hh.csv"), row.names=FALSE, sep=",");
fwrite(vehicles_output, file = paste0(tabdir, "atlas_veh.csv"), row.names=FALSE, sep=",");

print("Successfully saved!")



# Generate plots ####
source("code/final_visualization_LJ.R")

print("Plots are generated!")


