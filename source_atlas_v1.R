# main script for atlas_v1

# Note currently the static model predicts output year directly, 
#no dependencies on  previous year
if(outputyear <2015){diryear = 2010}else{diryear = 2017} # 2010-2014 use 2010 data and model, 2015- use 2017 mode

# 2. generate variables --------------#

source(paste0('data_clean_',diryear, '.R')) 

# 3. prepare coefs and control variables

source(paste0('Model_application_',diryear, '.R')) 


# 4. model run


registerDoParallel(cores = Npe) 

Nloop.max = floor(nrow(hh.masterdat)/10000)
print(paste('pop max loop',Nloop.max))


# determine sample of households to process
if(nsample > nrow(hh.masterdat)){stop('requested number of households exceeds the max number of existing households')}

if(nsample == 0){
  print('processing the full population')
  Nloop = Nloop.max # full sample
  hh.dat = hh.masterdat
  #  print('max loop',Nloop)
}else{ # if 0 < nsample < max hh number
  print(paste('processing subsample',nsample,'hh'))
  Nloop = floor(nsample/10000)
  print(paste('actual Nloop',Nloop))
  set.seed(4847384) # so that it is reproducible
  hh.dat = hh.masterdat %>% sample_n(nsample)
}

# hh.dat = hh.masterdat[]
if(Nloop == 0){ # nsample less than 10000 hh, direct compute
  print('less than 10000 households, using a serial run')
  data1 = hh.dat
  pp.tmp <- data1 %>% dplyr::select(household_id) %>% merge(pp.masterdat, by="household_id") # note here, the name cannot be persons otherwise apollo will report error, persons obj in both global and local environment
  res = model_application(persons=pp.tmp, data1, coefs_name_mile, coefs_mile , coef_names_veh, coef_values_veh, 
                          coef_names_type, coef_values_type, coef_names_car, coefs_car,
                          coef_names_van, coefs_van, coef_names_suv, coefs_suv, coef_names_pick, coefs_pick,
                          coef_names_power, coef_values_power,loopi = 1)
}else { # more than 10000
  tic()
  res <- foreach(i=1:Nloop, 
                 .combine=rbind,
                 .packages = c('dplyr','tidyr')
  )  %dopar% {
    
    if(i<Nloop){
      data1 <- hh.dat[(1+(i-1)*10000):(i*10000),]
    }else{
      data1 <- hh.dat[(1+(Nloop-1)*10000):nrow(hh.dat),]
    }
    #rm(households)
    print(paste('loop',i))
    pp.tmp <- data1 %>% dplyr::select(household_id) %>% merge(pp.masterdat, by="household_id")
    model_application(pp.tmp, data1, coefs_name_mile, coefs_mile , coef_names_veh, coef_values_veh, 
                      coef_names_type, coef_values_type, coef_names_car, coefs_car,
                      coef_names_van, coefs_van, coef_names_suv, coefs_suv, coef_names_pick, coefs_pick,
                      coef_names_power, coef_values_power, loopi = i)
  }
  toc()
  
}

stopImplicitCluster()

# now reformat the results and write out tables, note budget is no longer written out
print('reformat results into vehicle and household tables and write out')
households_output <- res %>% select(household_id, budget) %>% group_by(household_id) %>% summarise(nvehicles=n(), budget=mean(budget))
households_output <- hh.dat %>% merge(households_output, by="household_id", all.x = T) %>% select(household_id, nvehicles)
households_output$nvehicles[is.na(households_output$nvehicles)==T] <- 0

vehicles_output <- res %>% select(household_id, vehicle_id, VEHAGE:pred_own) %>%
  mutate(bodytype=case_when(car==1~"car", van==1~"van", suv==1~"suv", pickup==1~"pickup", T~"others"),
         vintage_category=case_when(VEHAGE0==1~"0~5 years", VEHAGE1==1~"6~11 years", VEHAGE2==1~"12+ years"),
         ownlease=case_when(pred_own==1~"own", T~"lease")) %>% select(household_id, vehicle_id, bodytype, vintage_category,
                                                                      maindriver_id, annual_mileage, pred_power, ownlease,
                                                                      VEHAGE0, VEHAGE1,VEHAGE2)
# distribute the vintage prediction to actual model year
# vintage 3: age assignment
vintage3_exp <- 0.200526701  # currently hard coded exponetional distribution parameter derived from NHTS 2017 data

nrow1 <- vehicles_output %>% filter(VEHAGE0==1) %>% nrow()
vehicles_output$modelyear[vehicles_output$VEHAGE0==1] <- outputyear-floor(runif(nrow1, min = 0, max = 4.99999))

nrow2 <- vehicles_output %>% filter(VEHAGE1==1) %>% nrow()
vehicles_output$modelyear[vehicles_output$VEHAGE1==1] <- outputyear-floor(runif(nrow2, min = 5, max = 10.99999))

nrow3 <- vehicles_output %>% filter(VEHAGE2==1) %>% nrow()
vehicles_output$modelyear[vehicles_output$VEHAGE2==1] <- outputyear-floor(rexp(nrow3, rate = vintage3_exp))-11

vehicles_output = vehicles_output %>% select(-VEHAGE0, -VEHAGE1,-VEHAGE2)

# LJ add: map to adopt vehicle category here, # note it is minvan, not minivan
print('map to adopt vehicle type and powertrain type')
vehicles_output = vehicles_output %>% mutate(adopt_veh = recode(bodytype, 'van'='minvan', 'pickup' = 'truck'), 
                                             adopt_fuel = recode(pred_power, "AEV"="ev", "Hybrid"="hybrid","ICE"="conv","PHEV"="phev"))



# 5. write out the results # we may need to add some post processing code here after clarifying the variables with Qianmiao

write.csv(vehicles_output, file = file.path(outputdir, paste0('vehicles_',outputyear,'.csv')), row.names = F) # vehicle level prediction
write.csv(households_output, file = file.path(outputdir, paste0('householdv_',outputyear,'.csv')),row.names = F) # houshold level prediction


