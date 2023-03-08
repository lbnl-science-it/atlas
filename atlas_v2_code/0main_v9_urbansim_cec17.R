# This is the main file for DEMOS 2017 run
# v9: add main driver prediction for middle year (i.e. 2018)
# v5: integrate and optimize the codes
# v4: (1) use the vehicle choice coeffs from 2017 data (2) change the order of steps
# v3: based on v2_LJ add with following adjustments:
# 1. change link variable from interview to headpid
# 2. use the raw data with income info combined

## clean the 2010 data from initialization
rm(list=ls())
mywd <- "I:/Shared drives/Shared_ATLAS/Backup_by_Qianmiao/Demos/20172020"
setwd(mywd)

# folders
demosdata <- "./data/cleaned"
atlasdata <- "./data/atlas_v1"
codes <- "./atlas_v2"
garphdir <- "./graph"
coefdir <- "./coefs"
output <- "./outputdata/V8/nourban/"
inputdir <- "./input"

options(dplyr.summarise.inform = FALSE)

# initialization of the clustering
library(stats)
library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(tictoc)

# numiter <- 1
yearstep <- 2
# set year
simyear = 2015 + yearstep*1
# evolution year, the output year
evoyear <- simyear + yearstep-1 #middle year
evoyear <- simyear + yearstep
yreffect <- 2017
numalt <- 3 # three status: dispose, replace, keep(default)

# load household data and person covariates data
load(file=file.path(demosdata, paste0("households", simyear, ".Rdata")))
load(file=file.path(demosdata, paste0("persons", simyear, ".Rdata")))
load(file=file.path(demosdata, paste0("persons", evoyear, ".Rdata")))

# load the initialization data
load(file=file.path(atlasdata, paste0("households", simyear, "_fromv1.Rdata")))
load(file=file.path(atlasdata, paste0("vehicles", simyear, ".Rdata")))

households_thisyear <- households0 %>% merge(households0_fromv1, by="headpid") %>% drop_na()
households_thisyear$urban_cbsa <- 0 # for now we zero out this variable to improve prediction
vehicles_thisyear <- households_thisyear %>% select(-newhhflag) %>% merge(vehicles0, by=c("headpid")) #4461502 vehicles
persons_thisyear <- persons0 %>% merge(households_thisyear %>% select(headpid), by="headpid")
hhv0 <- households0_fromv1
rm(households0_fromv1)


#=====================================Preload needed variables=========================================
#============= step 1: apply the replace/dispose/keep model
load(file=file.path(coefdir, "vehicle_transaction_model1.RData"))
load(file=file.path(coefdir, "vehicle_transaction_model2.RData"))

#add geolocation coefficients

# single veh hh coefs
geocoeff1 <- setNames(c(0.315, 0.065, 0.084, -0.014, 0.00002, -0.00001), c("urban_cbsa:disp", "urban_cbsa:replace",  
                                                                           "emp_zscore:disp", "emp_zscore:replace",
                                                                           "pop_density:disp", "pop_density:replace"))
# multi veh hh coefs
geocoeff2 <- setNames(c(0.064, -0.015, 0.063, -0.021, 0.00003, -0.00000), c("urban_cbsa:disp", "urban_cbsa:replace",  
                                                                            "emp_zscore:disp", "emp_zscore:replace",
                                                                            "pop_density:disp", "pop_density:replace"))

source(paste0(codes,'/1dispose_or_replace.R'))
#============= step 2: apply the addtion model
load(file=file.path(coefdir, "nvehChange_orderedLogit.RData"))
load(file=file.path(coefdir, "nvehChange_orderedLogit2.RData"))

geocoeff_add1 <- setNames(c(-0.241, -0.100, -0.00005), c("urban_cbsa", "emp_zscore", "pop_density"))
geocoeff_add2 <- setNames(c(-0.266, -0.067, -0.00003), c("urban_cbsa", "emp_zscore", "pop_density"))
geocoeff_add3 <- setNames(c(-0.172, -0.070, -0.00004), c("urban_cbsa", "emp_zscore", "pop_density"))

source(paste0(codes,'/2addition_v2.R'))
#============= step 3: disposed vehicles: scrappaged or go back to market
# define the categories
# scrap_car <- c("car", "suv", "van")
# scrap_truck <- c("pickup")
# LJ 10/18/2022: change to ADOPT definition for survival curve
scrap_car <- c("car", "suv", "van")
scrap_truck <- c("pickup","suv", "van", "minivan")


source(paste0(codes,'/3scrappage.R'))
# scrappage rates
carA=0.000298; carB=-0.01336; carC=0.142273; carD=-1.18379; carE=94.94659;
truckA=0.000486; truckB=-0.02414; truckC=0.382124; truckD=-3.31743; truckE=104.3316;

#============= step4: predict the new or used vehciels
set.seed(375874638)
local.us.ratio = 0.023  # ratio of sale totals between local, i.e. SFB, and U.S. this number is derived based on SFB vs US total vehicles
veh.contHH.ratio = 0.87 # fraction of vehicles represented by the continuing hh
adopt.fnm = 'adopt_raw.csv'
threshold <- 0.001
load(file.path(coefdir, 'newused_coefs.RData')) 
source(paste0(codes,'/4new_used.R'))

#============= step 5: apply the vehicle choice model to new vehicles
# load adopt new sale data 2018-2019
adopt.vehfuel = 'adopt_biannual_values.csv'
adopt.us.new = fread(file.path(inputdir, adopt.vehfuel))
adopt.us.new$bodytype[adopt.us.new$bodytype=="suv"] <- "SUV"
names(adopt.us.new)[1:3] <- c("adopt_fuel", "adopt_veh", "price")
local.sale.new = cbind(adopt.us.new, floor(adopt.us.new$total_sales * local.us.ratio * veh.contHH.ratio))
names(local.sale.new)[11] <- "sales"
# load original coeffs
coef3 <- fread(file.path(coefdir, 'final_coefficients_cec2017_v6.csv'))
source(paste0(codes,'/5new_mode_choice.R'))

#============= step 6: apply the vehicle choice model to used vehicles
# load the attribute data
attribute <- read.csv(file.path(inputdir, 'adopt_epa_used_vehicles_2018.csv'))
attribute <- attribute %>% rename(adopt_veh=bodytype, adopt_fuel=fueltype)

source(paste0(codes,'/6used_mode_choice.R'))

#============= step 7: own or lease prediction
load(file=file.path(coefdir, "ownlease_dynamic_estimates.Rdata"))
#============= step 8: predict main driver for new hh and newly acquired vehicles
load(file=file.path(coefdir, "main_driver.rda"))
source(paste0(codes,'/8main_driver.R'))
source(paste0(codes,'/8main_driver_middle.R'))

#============= step 10: match new hh to the hh with known veh prediction in the evolution year
load(file.path(demosdata, "households2019.Rdata"))
source(paste0(codes,'/10new_hh.R'))

# 2. determine if simyear is a mid year or an evolution year
if(((evoyear - simyear)%/%2)*2 - (evoyear - simyear) == 0){
  # evolution year
  evoTF = T
  baseyear = evoyear - 2 # current wave should be 2 years ago
}else{
  evoTF = F
  baseyear = evoyear - 1 # current wave should be 1 year ago
}

#=====================================Start vehicle pool change process=========================================
print(paste0("Running for ", evoyear))

if(!evoTF){
  # if it is the simyear is the mid year
  
  # copy the base year outputs to continuing hhs in simyear
  
  cur_hhids = intersect(households0$headpid,households0.5$headpid)
  
  cont.hhv = hhv0[hhv0$headpid %in% cur_hhids,] # copy the hh level predictions over 
  cont.veh = vehicles0[vehicles0$headpid %in% cur_hhids,] # copy the veh level predictions over 
  
  # match the new hhs to existing hhs
  demodat = households0.5 %>% # use the demo data from the mid year to do the matching
    select(matches(c('headpid','tract_geoid',hhmatch_varnames)))  
  
  demodat = demodat %>% drop_na() # get complete cases
  
  new_hhids = demodat$headpid[!(demodat$headpid %in% cur_hhids)]
  
  matched_ids <- matchinghhid(demodat) 
  
  # copy hhv over
  new_hhv = matched_ids %>% 
    rename(headpid = matched_id) %>%
    left_join(cont.hhv) %>%
    select(new_hhid, nvehicles)%>%
    rename(headpid = new_hhid)
  
  # copy vehicle-level prediction over
  new_veh = matched_ids %>% 
    rename(headpid = matched_id) %>%
    left_join(cont.veh) %>%
    select(-headpid)%>%
    rename(headpid = new_hhid)
  
  # combine the continuing hh with new hh
  
  hhv0.5 = bind_rows(cont.hhv, new_hhv)
  vehicles0.5 = bind_rows(cont.veh, new_veh)
  
  # as the persons may have changed, need to redo main driver prediction here
  # redo main driver prediction
  
  vehicles0.5 <- maindriver0.5(as.data.table(vehicles0.5), as.data.table(persons0.5))
  
  save(hhv0.5, vehicles0.5, file=file.path(output, paste0(evoyear, ".Rdata")))
  
}else{

  # set parameters
  initialyear <- (simyear==2017)
  
  #============= step1: apply the replace/dispose/keep model
  vehicles_thisyear <- dipose_clean(vehicles_thisyear, initialyear, yreffect)
  tmp <- dispose_apply(vehicles_thisyear)
  table(tmp$nextwave_status)/nrow(tmp)
  
  # dispose      keep   replace 
  # 0.1953017 0.6081376 0.1965607 
  
  #============= step2: apply the addtion model
  households_thisyear <- addition_clean(vehicles_thisyear, households_thisyear)
  
  tmp2 <- addition(households_thisyear)
  table(tmp2$addition)/nrow(tmp2)
  
  # addition 1  addition 2  decrease 1  decrease 2 no addition 
  # 0.119505420 0.009272607 0.221495383 0.037608140 0.612118450 
  
  # first adjustment: random sample the number of households with disposed vehicles
  hh3 <- nrow(tmp2[ncar_thiswave==3 & (addition=="decrease 2" | addition=="decrease 1")])
  hh4 <- nrow(tmp2[ncar_thiswave>=4 & (addition=="decrease 2" | addition=="decrease 1")])
  veh3 <- unique(tmp[ncar_thiswave==3&nextwave_status=="dispose"], by="headpid")[,c("headpid")]
  veh4 <- unique(tmp[ncar_thiswave>=4&nextwave_status=="dispose"], by="headpid")[,c("headpid")]
  
  if (nrow(veh3)>=hh3){
    veh3 <- veh3[sample(.N,nrow(veh3)-hh3)]
  }
  if (nrow(veh4)>=hh4){
    veh4 <- veh4[sample(.N,nrow(veh4)-hh4)]  
  }
  
  tmp <- (tmp %>% merge(rbind(veh3, veh4)[,m:=1], by="headpid", all.x = T))[m==1, nextwave_status:="keep"][,-c("m")]
  tmp <- merge(tmp[,-c("Ndisp_veh")], tmp[nextwave_status=="dispose"][, .(Ndisp_veh=.N), by="headpid"][,c("headpid", "Ndisp_veh")], 
               by="headpid", all.x=T)[is.na(Ndisp_veh)==T, Ndisp_veh:=0]
  
  # second adjustmetn the overprediction of disposal decision
  disp_adjust <- merge(tmp2[, .(headpid, Ndisp_hh, nvehicles)], unique(tmp, by = c("headpid", "Ndisp_veh")), by="headpid")[
    Ndisp_veh>Ndisp_hh & Ndisp_hh!=0 & nvehicles>=3][,gap:=Ndisp_veh-Ndisp_hh][,.(headpid, gap)]
  
  tmp <- merge(tmp, disp_adjust, by="headpid", all.x = T)
  tmp <- tmp[is.na(gap), gap:=0]
  
  # only adjust for households having 3 or 4+ vehicles
  tmp0 <- tmp[gap>0 & nextwave_status=="dispose" & ncar_thiswave>=3] %>% 
    group_split(headpid) %>%
    map2_df(disp_adjust$gap, ~sample_n(.x, size = .y)) %>%
    rbind(tmp[gap==0 | nextwave_status!="dispose" | ncar_thiswave<3])
  
  tmp <- as.data.table(setdiff(tmp, tmp0))[,nextwave_status:="keep"] %>% rbind(tmp0)
  table(tmp$nextwave_status)/nrow(tmp)
  
  # dispose      keep   replace 
  # 0.1515261 0.6567116 0.1917623
  
  # save the replaced vehicles
  vehicles_dispose_rep <- merge(tmp[nextwave_status=="replace"], vehicles_thisyear, by=c("headpid", "vehicle_id"))[
    ,.(headpid, vehicle_id, vehtype, pred_power, ownlease, deltayear)][,vehicle_tag:="replaced"]
  
  #============= step3: save the dispose and scrappage vehicles
  vehicles_dispose <- merge(merge(vehicles_thisyear, tmp, by=c("headpid", "vehicle_id"))[nextwave_status=="dispose"],
                            tmp, by=c("headpid", "vehicle_id"))[,.(headpid, vehicle_id, vehtype,pred_power, ownlease, deltayear)][
                              , vehicle_tag:="disposed"]
  
  vehicles_dispose <- vehicles_dispose %>% rbind(vehicles_dispose_rep)
  vehicles_dispose_scrape <- scrappage(vehicles_thisyear, vehicles_dispose)[[1]]
  
  vehicles_dispose_return <- merge(merge(fsetdiff(vehicles_dispose[,.(headpid, vehicle_id)], 
                                                  vehicles_dispose_scrape[,.(headpid, vehicle_id)], all = TRUE),
                                         vehicles_dispose[,.(headpid, vehicle_id)], by=c("headpid", "vehicle_id")), vehicles_thisyear, by=c("headpid", "vehicle_id"))[
                                           ,.(headpid, vehicle_id, vehtype, vintage_category, annual_mileage, pred_power, ownlease, deltayear)]
  
  # save the intermedium datasets
  save(vehicles_dispose, file=file.path(output, paste0("vehdispose_", simyear, "_", evoyear, ".Rdata")))
  save(vehicles_dispose_return, file=file.path(output, paste0("vehinvent_", simyear, "_", evoyear, ".Rdata")))
  save(vehicles_dispose_scrape, file=file.path(output, paste0("scrappage_", simyear, "_", evoyear, ".Rdata")))
  
  # get the used vehicle inventory
  vehicles_dispose_return <- vehicles_dispose_return[,adopt_veh:=vehtype][,adopt_fuel:=pred_power][
    adopt_veh=="van", adopt_veh:="minvan"][adopt_veh=="pickup", adopt_veh:="truck"][
      adopt_fuel=="AEV", adopt_fuel:="ev"][adopt_fuel=="Hybrid", adopt_fuel:="hybrid"][
        adopt_fuel=="ICE", adopt_fuel:="conv"][adopt_fuel=="PHEV", adopt_fuel:="phev"]
  
  used_invent <- vehicles_dispose_return[,keyby=.(adopt_veh, adopt_fuel, vintage_category), 
                                         .(n=.N, model_year=median(deltayear))]
  
  #============= step4: predict the new or used vehciels
  vehmodepredict <- vehmode_clean(tmp, tmp2)
  N.occassion = dim(vehmodepredict)[1]
  newused <- new_used(vehmodepredict)
  vehmodepredict.new <- newused[[1]]
  vehmodepredict.used <- newused[[2]]
  
  #============= step5: apply the vehicle choice model to new vehicle pool
  N.occassion = nrow(vehmodepredict.new)
  # choose the new vehicle choice set
  lookup <- vehmodeset_clean(adopt.us.new)
  
  # prepare the new vehicle data for mode choice
  vehmodepredict.new <- vehmodepredict_newfunc(vehmodepredict.new)
  
  # tune coef3 and predict mode
  vehmodepredict.new <- vehmodechoice_new(vehmodepredict.new)
  
  # add model year
  vehmodepredict.new <- vehmodepredict.new[,deltayear:=fcase(random<0.5, evoyear-1, random>=0.5, evoyear)]
  
  save(vehmodepredict.new, file=file.path(output, paste0("vehmodepredict.new", simyear, "_", evoyear, ".Rdata")))
  
  #============= step 6: apply the vehicle choice model to old vehicle pool
  N.occassion <- nrow(vehmodepredict.used)
  
  # readjust the coefficients, by adding two more vintage constants to be calibrated
  coef3 <- fread(file.path(coefdir, 'final_coefficients_cec2017_v6.csv'))
  coef3[5,1] <- "veh_age_bins7-12yr"
  coef3[6,1] <- "veh_age_bins>=13yr"
  coef3[5,c(3,4,5)] <- 0
  coef3[6,c(3,4,5)] <- 0
  
  # choose the used vehicle choice set
  attribute <- as.data.table(attribute %>% merge(used_invent, by=c("adopt_veh", "adopt_fuel", "model_year")))
  lookup <- vehmodeset_clean_used(attribute)
  
  # prepare the used vehicle data for mode choice
  vehmodepredict.used <- vehmodepredict_usedfunc(vehmodepredict.used)
  gc()
  # tune coef3 and predict mode
  vehmodepredict.used <- vehmodechoice_used(vehmodepredict.used)
  
  # get the exponential rate of vintage category 3
  vint3 <- get_vint3exp(vehicles_dispose_return)
  
  # add model year
  n1 <- nrow(vehmodepredict.used[vintage_category=="0~5 years"])
  n2 <- nrow(vehmodepredict.used[vintage_category=="6~11 years"])
  n3 <- nrow(vehmodepredict.used[vintage_category=="12+ years"])
  
  vehmodepredict.used$deltayear[vehmodepredict.used$vintage_category=="0~5 years"] <- evoyear-
    floor(runif(n1, min=2, max=6.99999))
  vehmodepredict.used$deltayear[vehmodepredict.used$vintage_category=="6~11 years"] <- evoyear-
    floor(runif(n2, min=7, max=12.99999))
  vehmodepredict.used$deltayear[vehmodepredict.used$vintage_category=="12+ years"] <- evoyear-
    floor(rexp(n3, rate = -coef(vint3)[2]))-13
  
  
  save(vehmodepredict.used, file=file.path(output, paste0("vehmodepredict.used", simyear, "_", evoyear, ".Rdata")))
  
  vehmodepredict <- rbind(vehmodepredict.used[,!c("id", "random")], vehmodepredict.new[,!c("id", "random")][
    ,vintage_category:="new"])[,tempid:= as.numeric(row.names(.SD))]
  
  vehmodepredict <- merge(vehmodepredict[adopt_veh=="SUV", adopt_veh:="suv"], households_thisyear, by="headpid")
  
  # fill in the old vehicle type for next run
  vehmodepredict <- vehmodepredict[adopt_veh=="suv", vehtype:="suv"][adopt_veh=="truck", vehtype:="pickup"]
  
  #============= step 7: own or lease prediction
  source(paste0(codes,'/7own_or_lease_v2.R'))
  
  #============= step 8: predict main driver for new hh and newly acquired vehicles
  vehmodepredict <- maindriver(vehmodepredict, persons_thisyear)
  
  #============= step 9: clean the vehicle data and household data for next wave
  source(paste0(codes,'/9output_data.R'))
  
  #============= step 10: match new hh to the hh with known veh prediction in the evolution year
  demodat = households1
  rm(households1)
  
  demodat = demodat %>%
    select(matches(c('headpid','tract_geoid',hhmatch_varnames)))
  
  demodat = demodat %>% drop_na() # get complete cases
  
  cur_hhids = unique(households_nextyear$headpid)
  new_hhids = demodat$headpid[!(demodat$headpid %in% cur_hhids)]
  
  matched_ids <- matchinghhid(demodat)
  
  newhh_nextyear <- households_nextyear %>% rename(matched_id=headpid)
  newhh_nextyear <- setnames(merge(newhh_nextyear, matched_ids, by="matched_id"), c("new_hhid"), c("headpid"))[
    ,!c("matched_id")][, newhhflag:=1]
  
  newhh_veh_nextyear <- vehicles_nextyear %>% rename(matched_id=headpid)
  newhh_veh_nextyear <- setnames(merge(newhh_veh_nextyear, matched_ids, by="matched_id"), c("new_hhid"), c("headpid"))[
    ,!c("matched_id")][, newhhflag:=1]
  
  households <- rbind(households_nextyear, newhh_nextyear, fill=TRUE)
  vehicles <- rbind(vehicles_nextyear, newhh_veh_nextyear, fill=TRUE)
  
  save(households, vehicles, file=file.path(output, paste0(evoyear, ".Rdata")))
  
}





