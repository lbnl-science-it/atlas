# main script for atlas_v1
# note that we will always intitiate from 2017
# runs will conducted by county basis in parallel mode



# ##### for testing only
# inputdir <- '/Users/lingjin/Dropbox/Research/SmartGrid_Behavioral/TransportationInitiative/ATLAS/ATLAS-National/Rscripts/Software/atlas_input'
# outputdir <- '/Users/lingjin/Dropbox/Research/SmartGrid_Behavioral/TransportationInitiative/ATLAS/ATLAS-National/Rscripts/Software/atlas_output'
# codedir <- '/Users/lingjin/Dropbox/Research/SmartGrid_Behavioral/TransportationInitiative/ATLAS/ATLAS-National/Rscripts/Software/code_inside_container'
# setwd(codedir)
# outputyear = 2017
# select.county.fips <- '001'
# select.state.fips <- '06'
# Npe <-2
# #### end testing only

# set up directories and load libraries
source('setup_us_atlas_v1.R')

# get the list of counties to run
synthpop.files <- list.files(
  path = synthpop_input_dir,
  pattern = paste0("^synthetic_households_.*\\.csv$"),
  full.names = FALSE
)
all.countyfips <- substr(synthpop.files, nchar('synthetic_households_06_')+1, nchar('synthetic_households_06_001'))

# 4. model run

registerDoParallel(cores = Npe) 

  tic()
  res <- foreach(i= 1: length(all.countyfips), 
                 .combine=rbind,
                 .packages = c('dplyr','tidyr')
  )  %dopar% {
    
    select.county.fips = all.countyfips[i]
    print(paste0('Processing county fips: ', select.county.fips))
    
    # prep data
    data1 <- estimation_data_prep(select.state.fips, select.county.fips)
    data1 <- as.data.frame(data1) 
    
    # data1 <- data1 %>% # reduce size to test data
    #   sample_n(200)
    
    # run the whole process
    model_application(select.state.fips, select.county.fips, data1, coefs_name_mile, coefs_mile , coef_names_veh, coef_values_veh, 
                      coef_names_type, coef_values_type, coef_names_car, coefs_car,
                      coef_names_van, coefs_van, coef_names_suv, coefs_suv, coef_names_pick, coefs_pick,
                      coef_names_power, coef_values_power)
  }
  toc()
  


stopImplicitCluster()


# # LJ add: map to adopt vehicle category here, # note it is minvan, not minivan
## this is now in the application function
# print('map to adopt vehicle type and powertrain type')
# vehicles_output = vehicles_output %>% mutate(adopt_veh = recode(bodytype, 'van'='minvan', 'pickup' = 'truck'), 
#                                              adopt_fuel = recode(pred_power, "AEV"="ev", "Hybrid"="hybrid","ICE"="conv","PHEV"="phev"),
#                                              acquire_year = NA)



# comment out for now, because each county is now saved sepearately 
# # 5. write out the results # we may need to add some post processing code here after clarifying the variables with Qianmiao
# 
# write.csv(vehicles_output, file = file.path(outputdir, paste0('vehicles_',outputyear,'.csv')), row.names = F) # vehicle level prediction
# write.csv(households_output, file = file.path(outputdir, paste0('householdv_',outputyear,'.csv')),row.names = F) # houshold level prediction
# 

