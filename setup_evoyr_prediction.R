# setup coefs and functions for mid year prediction

options(dplyr.summarise.inform = FALSE)

library(stats)
library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(tictoc)
library(fastDummies)


# folders
adoptdir <- file.path(inputdir,'adopt',adscen) # LJ 3/28/2023, read from scenario subfolder
rdatdir <- file.path(outputdir, 'rdata') # to store intermediate results
if(!file.exists(rdatdir)){dir.create(rdatdir)}

v2codedir <- file.path('atlas_v2_code')
v2coefdir <- file.path('atlas_v2_coefs')



#==============Start: modeling domain specific parameter====================================#
#  for SFB, it will changeg for austin
# local.us.ratio = 0.023  # ratio of sale totals between local, i.e. SFB, and U.S. this number is derived based on SFB vs US total vehicles
# ladj.factor = 0.83 # lifetime adjustment factor: the ratio between actual sales (carb derived sale) and the scaled adopt sale from national to SFB. 

# LJ 3/28/2023: now adopt sales are already scaled to sfb, no adjustment factors are needed here
local.us.ratio = 1
ladj.factor = 1

# scrappage rate parameters
carA=0.000298; carB=-0.01336; carC=0.142273; carD=-1.18379; carE=94.94659;
truckA=0.000486; truckB=-0.02414; truckC=0.382124; truckD=-3.31743; truckE=104.3316;

# correction factor for adopt fuel share, using multiplier for ev and phev
# calibrated factor using 2018 and 2019 sales data compared to carb derived
local.factor = F # whether to apply a CARB data derived correction factor (cuurrently only work for 2019)

# LJ 2/5/2023. now use the long term example adopt data processed by Connor
adopt.fnm = paste0('new_vehicles_biannual_values_', baseyear, '.csv')
adopt.us = fread(file.path(adoptdir, adopt.fnm)) 
Nsale = sum(adopt.us$total_sales)

# carb 2year derived sales share in 2019 in SFB
if(local.factor){
  ev.carb = 0.111; phev.carb = 0.039
  ev.share.factor = ev.carb/adoptfuelshare$prob.hat[adoptfuelshare$fueltype == 'ev'] # 11.38
  phev.share.factor = phev.carb/adoptfuelshare$prob.hat[adoptfuelshare$fueltype == 'phev'] # 3.65

}


# incentive related literature values

#============== End: modeling domain specific parameter====================================#

#============== start: modeling steps ====================================#
# load the baseyear output Rdata
load(file=file.path(inputdir, paste0('year',baseyear),"vehicles_output.RData"))
load(file=file.path(inputdir, paste0('year',baseyear),"households_output.RData"))

# LJ 2/14/2023: add acquire_year variable to initial year outputs and use NA for maindriver and never predict it in evoyears
if(baseyear==iniyear){vehicles_output$acquire_year = NA; vehicles_output$maindriver_id = NA}

hhv0 = households_output%>%dplyr::select(headpid, nvehicles); rm(households_output) # LJ 2/13/2023: the newhhflag is the baseyear relative to its own previous timestep hence not useful in evoyear prediction.
vehicles0 = vehicles_output%>%
  dplyr::select(headpid, vehicle_id, vehtype, pred_power, 
         ownlease, deltayear, adopt_fuel,
         adopt_veh, acquire_year, maindriver_id, vintage_category)
rm(vehicles_output)

households_thisyear <- households0 %>% merge(hhv0, by="headpid") %>% drop_na() # baseyear data used for prediction
households_thisyear$urban_cbsa <- 0 # for now we zero out this variable to improve prediction
vehicles_thisyear <- households_thisyear  %>% merge(vehicles0, by=c("headpid")) # predictors for vehicle level transaction
persons_thisyear <- persons0 %>% merge(households_thisyear %>% dplyr::select(headpid), by="headpid")
#persons_nextwave <- persons1 %>% merge(households1)
# LJ 10/18/2022, headpid that appeared in both baseyear and evoyear
cur_hhids = intersect(households0$headpid,households1$headpid) # hhids that in both years (baseyear and evoyear)
cont.veh = vehicles0[vehicles0$headpid %in% cur_hhids,] # vehicles in the continuing hh

print(paste('continuing hh is',length(cur_hhids)/length(households1$headpid),'of the evoyear hh'))
print(paste('continuing hh is',length(cur_hhids)/length(households0$headpid),'of the baseyear hh'))


#=====================================Preload needed variables=========================================
#============= step 1: apply the replace/dispose/keep model
yreffect <- 2017
numalt <- 3 # three status: dispose, replace, keep(default)
load(file=file.path(v2coefdir, "vehicle_transaction_model1.RData"))
load(file=file.path(v2coefdir, "vehicle_transaction_model2.RData"))

#add geolocation coefficients

# single veh hh coefs
geocoeff1 <- setNames(c(0.315, 0.065, 0.084, -0.014, 0.00002, -0.00001), c("urban_cbsa:disp", "urban_cbsa:replace",  
                                                                           "emp_zscore:disp", "emp_zscore:replace",
                                                                           "pop_density:disp", "pop_density:replace"))
# multi veh hh coefs
geocoeff2 <- setNames(c(0.064, -0.015, 0.063, -0.021, 0.00003, -0.00000), c("urban_cbsa:disp", "urban_cbsa:replace",  
                                                                            "emp_zscore:disp", "emp_zscore:replace",
                                                                            "pop_density:disp", "pop_density:replace"))

source(paste0(v2codedir,'/1dispose_or_replace.R'))
#============= step 2: apply the addtion model
load(file=file.path(v2coefdir, "nvehChange_orderedLogit.RData"))
load(file=file.path(v2coefdir, "nvehChange_orderedLogit2.RData"))

geocoeff_add1 <- setNames(c(-0.241, -0.100, -0.00005), c("urban_cbsa", "emp_zscore", "pop_density"))
geocoeff_add2 <- setNames(c(-0.266, -0.067, -0.00003), c("urban_cbsa", "emp_zscore", "pop_density"))
geocoeff_add3 <- setNames(c(-0.172, -0.070, -0.00004), c("urban_cbsa", "emp_zscore", "pop_density"))

source(paste0(v2codedir,'/2addition_v2.R'))
#============= step 3: disposed vehicles: scrappaged or go back to market
# define the categories
# scrap_car <- c("car", "suv", "van")
# scrap_truck <- c("pickup")
# LJ 10/18/2022: change to ADOPT definition for survival curve
scrap_car <- c("car")
scrap_truck <- c("pickup","suv", "van") # note we are using aggregated atlas vehicle types here

# fraction of vehicles represented by the continuing hh
veh.contHH.ratio = nrow(cont.veh)/nrow(vehicles0) # 0.9337 # 2/9/2023: 0.8854646 # 2/13/2023 year 2021: 0.86

print(paste('fraction of vehicles represented by continuing hh',veh.contHH.ratio))

# car and truck type continuing fraction to scale the scrappage
# fraction of car represented by the continuing hh # 0.9339 # 2/9/2023: 0.8857164 # 2/13/2023 year 2021: 0.86
car.contHH.ratio = nrow(cont.veh%>%filter(vehtype %in% scrap_car))/nrow(vehicles0%>%filter(vehtype %in% scrap_car)) 
# fraction of truck (vehicles other than car bodytype) represented by the continuing hh #0.9341 # 0.8849684
truck.contHH.ratio = nrow(cont.veh%>%filter(vehtype %in% scrap_truck))/nrow(vehicles0%>%filter(vehtype %in% scrap_truck)) 

print(paste('fraction of cars represented by continuing hh',car.contHH.ratio))
print(paste('fraction of trucks (non-car) represented by continuing hh',truck.contHH.ratio))


source(paste0(v2codedir,'/3scrappage.R'))

#============= step4: predict the new or used vehciels
set.seed(375874638)

# # previous testing code for evolv 1 step
# adopt.fnm = 'adopt_raw.csv'
# threshold <- 0.001
# adopt.us = fread(file.path(adoptdir, adopt.fnm)) %>%
#   filter(year %in% (baseyear+1):evoyear)

# adopt.fnm = paste0('new_vehicles_biannual_values_', baseyear, '.csv')
# adopt.us = fread(file.path(adoptdir, adopt.fnm)) 
# Nsale = sum(adopt.us$total_sales)
# adoptfuelshare = adopt.us[, by=fueltype, .(prob.hat=sum(total_sales)/Nsale)]

threshold <- 0.005 # 0.001. 4/5/2023 LJ: now relax the error threshold to increase the speed
# local sale totals of new vehicles
local.sale = floor(Nsale * local.us.ratio * veh.contHH.ratio *ladj.factor ) # LJ 10/22/2022: further adjuustment by lifetime factor

load(file.path(v2coefdir, 'newused_coefs.RData')) 
source(paste0(v2codedir,'/4new_used.R'))

#============= load incentive functions and data for step 5 and 6 ================#
source('./incentives/setup_incentive.R') # load the global data for incentive functions
source('./incentives/incentives_functions.R')
#============= step 5: apply the vehicle choice model to new vehicles ================#
# load adopt new sale data 2018-2019

# # previous testing 1 step
# adopt.vehfuel = 'adopt_biannual_values.csv'
# adopt.us.new = fread(file.path(adoptdir, adopt.vehfuel))
# adopt.us.new$bodytype[adopt.us.new$bodytype=="suv"] <- "SUV" # to match coefs
# names(adopt.us.new)[1:3] <- c("adopt_fuel", "adopt_veh", "price")
# local.sale.new = adopt.us.new%>%
#   mutate(sales = floor(adopt.us.new$total_sales * local.us.ratio * veh.contHH.ratio*ladj.factor)) # LJ 10/22/2022: further adjuustment by lifetime factor


# LJ 2/5/2023
adopt.us.new = adopt.us
#names(adopt.us.new)[1:3] <- c("adopt_fuel", "adopt_veh", "price")
adopt.us.new = adopt.us.new %>% dplyr::rename(adopt_fuel = fueltype, adopt_veh = bodytype)
if('year' %in% names(adopt.us.new)){adopt.us.new = adopt.us.new %>% dplyr::select(-year)}

local.sale.new = adopt.us.new%>%
  mutate(sales = floor(adopt.us.new$total_sales * local.us.ratio * veh.contHH.ratio*ladj.factor)) # LJ 10/22/2022: further adjuustment by lifetime factor

localNsales = sum(local.sale.new$sales)
adoptfuelshare = local.sale.new[, by=adopt_fuel, .(prob.hat=sum(sales)/localNsales)]
adoptvehshare = local.sale.new[, by=adopt_veh, .(prob.hat=sum(sales)/localNsales)]

# load original coeffs
coef3 <- fread(file.path(v2coefdir, 'final_coefficients_cec2017_v6.csv'))
source(paste0(v2codedir,'/5new_mode_choice.R'))

#============= step 6: apply the vehicle choice model to used vehicles ================#
# load the attribute data

# # previous testing 1 step
# attribute <- read.csv(file.path(adoptdir, 'adopt_epa_used_vehicles_2018.csv')) # read the mid year used vehicle attributes

attribute <- read.csv(file.path(adoptdir, paste0('used_vehicles_',baseyear,'.csv'))) # read the mid year used vehicle attributes
attribute <- attribute %>% dplyr::rename(adopt_veh=bodytype, adopt_fuel=fueltype)
#attribute <- attribute  #%>% mutate(adopt_veh = recode(adopt_veh, 'minvan' = 'minivan'))
source(paste0(v2codedir,'/6used_mode_choice.R'))

#============= step 7: own or lease prediction ================#
load(file=file.path(v2coefdir, "ownlease_dynamic_estimates.RData"))
#============= step 8: predict main driver for new hh and newly acquired vehicles
load(file=file.path(v2coefdir, "main_driver.rda"))
source(paste0(v2codedir,'/8main_driver_unified.R'))

#============= step 10: match new hh to the hh with known veh prediction in the evolution year ================#
source(paste0(v2codedir,'/10new_hh.R'))
