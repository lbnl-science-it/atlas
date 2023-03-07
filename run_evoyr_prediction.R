# run evolution year prediction

initialyear <- (baseyear == iniyear) # whether the base year is the initial year (iniyear = 2017), if so, we do not have the vehicle acquirement year variable "acquire_year"
library(tictoc)
#============= step1: apply the replace/dispose/keep model
# note transaction models use psid veh types to predict.
print('prepare vehicle related predictors')
tic()
vehicles_thisyear <- dipose_clean(vehicles_thisyear, initialyear, yreffect)
toc()
print('predicting vehicle level transaction outcome')
tic()
tmp <- dispose_apply(vehicles_thisyear)
toc()
table(tmp$nextwave_status)/nrow(tmp)

#tmp.sav = tmp; 
# dispose      keep   replace 
# 0.1953017 0.6081376 0.1965607 

# dispose      keep   replace 
# 0.1819109 0.6195328 0.1985563 

# 2/9/2023, evoyear = 2019
# dispose      keep   replace 
# 0.1879682 0.6098392 0.2021926 

#2/13/2023: for evoyear = 2021
# dispose      keep   replace 
# 0.1518401 0.6107714 0.2373885 

#============= step2: apply the addtion model

print('predicting household level transaction outcomes')
tic()
households_thisyear <- addition_clean(vehicles_thisyear, households_thisyear)

tmp2 <- addition(households_thisyear)
toc()

table(tmp2$addition)/nrow(tmp2)
# LJ comment: no addition means no change in the level of vehicles
# addition 1  addition 2  decrease 1  decrease 2 no addition 
# 0.119505420 0.009272607 0.221495383 0.037608140 0.612118450 

# addition 1  addition 2  decrease 1  decrease 2 no addition 
# 0.12617134  0.00993667  0.20894579  0.02862427  0.62632193 

# 2/9/2023
# addition 1  addition 2  decrease 1  decrease 2 no addition 
# 0.13897260  0.01561615  0.20973953  0.03749719  0.59817452 

# 2/13/2023, evoyear = 2021
# addition 1  addition 2  decrease 1  decrease 2 no addition 
# 0.14163915  0.01646359  0.21498245  0.02836667  0.59854813 

print('adjust vehicle level disposal decision by hh level vehicle ')
# first adjustment: random sample the number of households with disposed vehicles
# adjust the number of hh with vehicle level disposal decision to match  total number of hh derived from hh-level disposal decision
tic()
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

# change the status back to keep
tmp <- (tmp %>% merge(rbind(veh3, veh4)[,m:=1], by="headpid", all.x = T))[m==1, nextwave_status:="keep"][,-c("m")]
tmp <- merge(tmp[,-c("Ndisp_veh")], tmp[nextwave_status=="dispose"][, .(Ndisp_veh=.N), by="headpid"][,c("headpid", "Ndisp_veh")], 
             by="headpid", all.x=T)[is.na(Ndisp_veh)==T, Ndisp_veh:=0]

# second adjustmetn the overprediction of vehicle level disposal decision
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
toc()
table(tmp$nextwave_status)/nrow(tmp)

# dispose      keep   replace 
# 0.1515261 0.6567116 0.1917623

# dispose      keep   replace 
# 0.1395729 0.6684214 0.1920057 

# 2/9/2023
# dispose      keep   replace 
# 0.1440777 0.6589099 0.1970124 

# 2/13: evoyear = 2021
# dispose       keep    replace 
# 0.08203513 0.70249776 0.21546711 


print('save the transaction outcomes to rdatdir')
veh_trans_decision = tmp
hh_trans_decision = tmp2
tic()
save(veh_trans_decision,hh_trans_decision, vehicles_thisyear, file = file.path(rdatdir,paste0('transact.outcome.',baseyear,'-',evoyear,'.RData')))
toc()



#============= step3: determine the scrappage vehicles and return to market used inventory
print('vehicle survial model and used vehicle inventory')
# 
tic()
vehicles_dispose_rep <- merge(tmp[nextwave_status=="replace"], vehicles_thisyear, by=c("headpid", "vehicle_id"))[
  ,.(headpid, vehicle_id, vehtype, pred_power, ownlease, deltayear,adopt_fuel, adopt_veh)][,vehicle_tag:="replaced"]
vehicles_dispose <- merge(merge(vehicles_thisyear, tmp, by=c("headpid", "vehicle_id"))[nextwave_status=="dispose"],
                          tmp, by=c("headpid", "vehicle_id"))[,.(headpid, vehicle_id, vehtype,pred_power, ownlease, deltayear, adopt_fuel, adopt_veh)][
                            , vehicle_tag:="disposed"]

# vehicles disposed (replaced or without replacement)
vehicles_dispose <- vehicles_dispose %>% rbind(vehicles_dispose_rep)
res.tmp = scrappage(vehicles_thisyear, vehicles_dispose)
vehicles_dispose_scrape <- res.tmp[[1]]

scraptotals = list(car = res.tmp[[2]], truck = res.tmp[[3]])
# scraptotals = list(car= veh_num_car, truck = veh_num_truck)
vehicles_dispose_return <- merge(merge(fsetdiff(vehicles_dispose[,.(headpid, vehicle_id)], 
                                                vehicles_dispose_scrape[,.(headpid, vehicle_id)], all = TRUE),
                                       vehicles_dispose[,.(headpid, vehicle_id)], by=c("headpid", "vehicle_id")), vehicles_thisyear, by=c("headpid", "vehicle_id"))[
                                         ,.(headpid, vehicle_id, vehtype, vintage_category, pred_power, ownlease, deltayear, adopt_fuel, adopt_veh)]
vehicles_dispose_return <- vehicles_dispose_return %>% mutate(adopt_veh = recode(adopt_veh, 'minivan' = 'minvan'))
toc()

# get the used vehicle inventory

# LJ 10/22/2022: now that we have added adopt_veh and adopt_fuel columns from the beginning, no longer need the followign code
# vehicles_dispose_return <- vehicles_dispose_return[,adopt_veh:=vehtype][,adopt_fuel:=pred_power][
#   adopt_veh=="van", adopt_veh:="minvan"][adopt_veh=="pickup", adopt_veh:="truck"][
#     adopt_fuel=="AEV", adopt_fuel:="ev"][adopt_fuel=="Hybrid", adopt_fuel:="hybrid"][
#       adopt_fuel=="ICE", adopt_fuel:="conv"][adopt_fuel=="PHEV", adopt_fuel:="phev"]

used_invent <- vehicles_dispose_return[,keyby=.(adopt_veh, adopt_fuel, vintage_category), 
                                       .(n=.N, model_year=median(deltayear))]  # summarize used inv by adopt veh fuel type, and vintage category

# need to double check this
used_invent <- used_invent %>% mutate(adopt_veh = recode(adopt_veh, 'minivan' = 'minvan'))
# save the intermedium datasets
save(vehicles_dispose, file=file.path(rdatdir, paste0("vehdispose_", baseyear, "_", evoyear, ".Rdata")))
save(vehicles_dispose_return, file=file.path(rdatdir, paste0("usedvehinvent_", baseyear, "_", evoyear, ".Rdata")))
save(vehicles_dispose_scrape, file=file.path(rdatdir, paste0("scrappage_", baseyear, "_", evoyear, ".Rdata")))
save(scraptotals, file=file.path(rdatdir, paste0("scrapcontrol_", baseyear, "_", evoyear, ".Rdata")))
save(used_invent, file=file.path(rdatdir, paste0("usedinventory_", baseyear, "_", evoyear, ".Rdata")))

#load(file=file.path(rdatdir, paste0("usedinventory_", baseyear, "_", evoyear, ".Rdata")))

#============= step4: predict the new or used vehciels
print('predicting new or used choice')
tic()
vehmodepredict <- vehmode_clean(tmp, tmp2)
N.occassion = dim(vehmodepredict)[1]
newused <- new_used(vehmodepredict)
vehmodepredict.new <- newused[[1]]
vehmodepredict.used <- newused[[2]]
toc()

save(vehmodepredict.new, vehmodepredict.used, file=file.path(rdatdir, paste0("newusedchoice_", baseyear, "_", evoyear, ".Rdata")))

#load(file=file.path(rdatdir, paste0("newusedchoice_", baseyear, "_", evoyear, ".Rdata")))


#============= step5: apply the vehicle choice model to new vehicle pool
print('Now in step of predicting choices for new veh choice occassions')
N.occassion = nrow(vehmodepredict.new)
# choose the new vehicle choice set
print('preparing choice set')

# LJ 2/14/2023: as coef3 will be tuned separately for new vs used veh choices, copy coef3 to a new name
coef3new = coef3
lookup <- vehmodeset_clean(adopt.us.new,rebate.input = 0, tax_credit.input = 0) # note in this version, the incentives are set to 0

# prepare the new vehicle data for mode choice
print('preparing predictors')
tic()
vehmodepredict.new <- vehmodepredict_newfunc(vehmodepredict.new)
toc()

# tune coef3 and predict mode
print('predicting new vehicle choices')
if(local.factor){print('calibrate based on adopt data further scaled by local factor')}
tic()
res.tmp <- vehmodechoice_new(vehmodepredict.new, local.factor =local.factor) # local.factor = T in setup_evoyr, --> adopt ev/phev sales scaled further by carb data
toc()
vehmodepredict.new = res.tmp[[1]]
coef3new.tuned = res.tmp[[2]]

# assign continous model year, half to mid year and half to evoyear.
vehmodepredict.new <- vehmodepredict.new[,deltayear:=fcase(random<0.5, as.numeric(evoyear-1), random>=0.5, as.numeric(evoyear))]

save(vehmodepredict.new, coef3new.tuned, file=file.path(rdatdir, paste0("vehmodepredict.new", baseyear, "_", evoyear, ".Rdata")))
save(coef3new.tuned, file=file.path(rdatdir, paste0("coef3new.tuned", evoyear,".Rdata")))

#============= step 6: apply the vehicle choice model to old vehicle pool
print('Now in step of predicting used vehicle choices')
tic()
N.occassion <- nrow(vehmodepredict.used)

# readjust the coefficients, by adding two more vintage constants to be calibrated
print('preparing used vehicle attributes')
coef3 <- fread(file.path(v2coefdir, 'final_coefficients_cec2017_v6.csv'))
coef3[5,1] <- "veh_age_bins7-12yr" # note we use the mid year to get veh attributes, therefore it is 1 year older than the vintage category from baseyear.
coef3[6,1] <- "veh_age_bins>=13yr"
coef3[5,c(3,4,5)] <- 0 # initialize these coefs
coef3[6,c(3,4,5)] <- 0

# LJ 2/14/2023: as coef3 will be tuned separately for new vs used veh choices, copy coef3 to a new name
coef3used = coef3


# choose the used vehicle choice set
print('preparing predictors')
attribute <- as.data.table(attribute %>% merge(used_invent, by=c("adopt_veh", "adopt_fuel", "model_year")))
lookup <- vehmodeset_clean_used(attribute,rebate.input = 0, tax_credit.input = 0)

# prepare the used vehicle data for mode choice
vehmodepredict.used <- vehmodepredict_usedfunc(vehmodepredict.used)
gc()
# tune coef3 and predict mode
print('predicting used vehicle choice and calibrate constants to match used inventory')
used.tmpres <- vehmodechoice_used(vehmodepredict.used)
vehmodepredict.used = used.tmpres$predicted
coef3used.tuned = used.tmpres$coef3used.tuned
rm(used.tmpres)


# get the exponential rate of vintage category 3
print('assign continous model year to predicted vintage bin')
vint3 <- get_vint3exp(vehicles_dispose_return)

# add model year (note the vintage categories are still currently relative to baseyear)
n1 <- nrow(vehmodepredict.used[vintage_category=="0~5 years"])
n2 <- nrow(vehmodepredict.used[vintage_category=="6~11 years"])
n3 <- nrow(vehmodepredict.used[vintage_category=="12+ years"])

# LJ 2/13/2023,  update vintage here, because these categories are by now still defined relative to base year
vehmodepredict.used$deltayear[vehmodepredict.used$vintage_category=="0~5 years"] <- evoyear-
  floor(runif(n1, min=2, max=6.99999))
vehmodepredict.used$deltayear[vehmodepredict.used$vintage_category=="6~11 years"] <- evoyear-
  floor(runif(n2, min=7, max=12.99999))
vehmodepredict.used$deltayear[vehmodepredict.used$vintage_category=="12+ years"] <- evoyear-
  floor(rexp(n3, rate = -coef(vint3)[2]))-13

toc()


save(vehmodepredict.used, coef3used.tuned, file=file.path(rdatdir, paste0("vehmodepredict.used", baseyear, "_", evoyear, ".Rdata")))
save(coef3used.tuned, file=file.path(rdatdir, paste0("coef3used.tuned", evoyear,".Rdata")))

vehmodepredict <- rbind(vehmodepredict.used[,!c("id", "random")], vehmodepredict.new[,!c("id", "random")][
  ,vintage_category:="new"])[,tempid:= as.numeric(row.names(.SD))][adopt_veh=="SUV", adopt_veh:="suv"]

# map to atlas vehicle type from adopt types
vehmodepredict <- vehmodepredict %>%
  mutate(vehtype = recode(adopt_veh,  'truck' = 'pickup',  'minivan' = 'van','minvan' = 'van'),
         pred_power = recode(adopt_fuel, 'ev' = 'AEV', 'conv' = 'ICE', 'hybrid' = 'Hybrid','phev' = 'PHEV', 'fuelcell' = 'AEV', 'cng' = 'ICE'))

# add acquirement year, set to mid year
vehmodepredict <- vehmodepredict %>%
  mutate(acquire_year = evoyear-1)
# merge the prediction of added and replacement vehicles 
vehmodepredict <- merge(vehmodepredict, households_thisyear, by="headpid")

save(vehmodepredict, file=file.path(rdatdir, paste0("vehmodepredict", baseyear, "_", evoyear, ".Rdata")))

#============= step 7: own or lease prediction
print('predict own/lease')
tic()
source(paste0(v2codedir,'/7own_or_lease_v2.R'))
toc()

#============= LJ move to last step as the persons mmay have changed: step 8: predict main driver for new hh and newly acquired vehicles
#vehmodepredict <- maindriver(vehmodepredict, persons_thisyear) # currently skip this step
vehmodepredict$maindriver_id = NA

#============= step 9: clean the vehicle data and household data for next wave
print('combining output data for continuing hh ')
source(paste0(v2codedir,'/9output_data.R'))

#============= step 10: match new hh to the hh with known veh prediction in the evolution year
print('match the new hh with existing hh')

demodat = households1
#rm(households1)

demodat = demodat %>%
  select(matches(c('headpid','tract_geoid',hhmatch_varnames)))

demodat = demodat %>% drop_na() # get complete cases

cur_hhids = unique(households_nextyear$headpid)
new_hhids = demodat$headpid[!(demodat$headpid %in% cur_hhids)]

# # parallel code sometimes give errors
# tic()
# matched_ids <- matchinghhid_parallel(demodat, Npe = Npe)
# toc()

tic()
matched_ids <- matchinghhid(demodat)
toc()


newhh_nextyear <- households_nextyear %>% rename(matched_id=headpid)
newhh_nextyear <- setnames(merge(newhh_nextyear, matched_ids, by="matched_id"), c("new_hhid"), c("headpid"))[
  ,!c("matched_id")][, newhhflag:=1]

newhh_veh_nextyear <- vehicles_nextyear %>% rename(matched_id=headpid)
newhh_veh_nextyear <- setnames(merge(newhh_veh_nextyear, matched_ids, by="matched_id"), c("new_hhid"), c("headpid"))[
  ,!c("matched_id")][, newhhflag:=1]

#### output data
households_output <- rbind(households_nextyear, newhh_nextyear, fill=TRUE)
vehicles_output <- rbind(vehicles_nextyear, newhh_veh_nextyear, fill=TRUE)


# #============= LJ move to last step as the persons mmay have changed: step 8: predict main driver for new hh and newly acquired vehicles
# 
# print('redo main driver prediction as the persons may have changed')
# tic()
# vehicles_output <- p_maindriver(as.data.table(vehicles_output), as.data.table(persons1), Npe = Npe)
# toc()

print('currently skip main driver prediction to speed up')
vehicles_output$maindriver_id = NA 

# recode vintage category based on current year age
vehicles_output = vehicles_output %>%
  mutate(vintage_category = case_when(
    outputyear - deltayear <=5 ~ '0~5 years',
    outputyear - deltayear >=12 ~ '12+ years',
    TRUE ~ '6~11 years'
  ))


save(vehicles_output, file=file.path(inputdir, paste0('year',evoyear),"vehicles_output.RData"))
save(households_output, file=file.path(inputdir, paste0('year',evoyear),"households_output.RData"))

# load(file=file.path(inputdir, paste0('year',evoyear),"vehicles_output.RData"))
# load(file=file.path(inputdir, paste0('year',evoyear),"households_output.RData"))

