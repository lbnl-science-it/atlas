#============= data combination for next wave
if (initialyear==T){
  vehicles_thisyear_tmp <- merge(merge(vehicles_thisyear, veh_trans_decision, by=c("headpid", "vehicle_id"))[nextwave_status=="keep"],
                                 veh_trans_decision, by=c("headpid", "vehicle_id"))[,.(headpid, vehicle_id, vehtype, 
                                                                        pred_power, ownlease, deltayear, adopt_fuel, adopt_veh)][
                                                                          ,vehicle_tag:="old"]
  vehicles_thisyear_tmp <- vehicles_thisyear_tmp %>% mutate(acquire_year = NA)
  vehicles_thisyear_tmp = vehicles_thisyear_tmp%>% mutate(ownlease = recode(ownlease, '1' = 'own', '0' ='lease'))
  
}else{
  vehicles_thisyear_tmp <- merge(merge(vehicles_thisyear, veh_trans_decision, by=c("headpid", "vehicle_id"))[nextwave_status=="keep"],
                                 veh_trans_decision, by=c("headpid", "vehicle_id"))[,.(headpid, vehicle_id, vehtype, pred_power, 
                                                                        ownlease, deltayear, adopt_fuel, adopt_veh, acquire_year)][
                                                                          ,vehicle_tag:="old"]
  vehicles_thisyear_tmp = vehicles_thisyear_tmp%>% mutate(ownlease = recode(pred_own, '1' = 'own', '0' ='lease'))
  
}
#vehmodepredict.sav = vehmodepredict
vehmodepredict <- vehmodepredict[,.(headpid, vehtype, pred_power, adopt_veh, adopt_fuel, ownlease, deltayear,acquire_year)][
  ,vehicle_tag:="new"]
# vehmodepredict <- setnames(vehmodepredict, c("pred_own"), c("ownlease"))[
#   ,pred_power:=fcase(adopt_fuel %in% c("ev",'fuelcell'), "AEV", adopt_fuel %in% c("conv",  "cng"), "ICE",
#                      adopt_fuel %in% c("hybrid"), "Hybrid", adopt_fuel %in% c("phev"), "PHEV")][
#                        ,vehtype:=fcase(adopt_veh=="car", "car", adopt_veh=="van", "van",
#                                        adopt_veh=="pickup", "truck", 
#                                        adopt_veh %in% c("minvan", "minivan", "van"), "van")][,acquire_year:= evoyear]

vehicles_nextyear <- rbind(vehicles_thisyear_tmp, vehmodepredict, fill=TRUE)[, by=headpid, vehicle_id:=sequence(.N)]
rm(vehicles_thisyear_tmp)

households_nextyear <- merge(vehicles_nextyear[,by=headpid, .(nvehicles=.N)], households_thisyear[,.(headpid)], by="headpid", all.y=T) 
households_nextyear <- households_nextyear[is.na(nvehicles)==T, nvehicles:=0]

# Add year
households_nextyear <- households_nextyear[,year:=evoyear]
vehicles_nextyear <- vehicles_nextyear[,year:=evoyear]

