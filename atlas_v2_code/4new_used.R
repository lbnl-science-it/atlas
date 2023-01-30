# This step is to tune the new and used vehicle classification coefficients and split the vehicle pool

# First: prepare the dataset for mode choice (new and used mixed)
vehmode_clean <- function(data1, data2){
  # prepare the vehicle data
  replace <- merge(data1[nextwave_status=="replace"], vehicles_thisyear, by=c("headpid", "vehicle_id"))[
    ,vehicle_tag:="acquire_rep"]

  addition <- data2[addition!="no addition" & addition!="decrease 1" & addition!="decrease 2"][
    ,addition:=fcase(addition=="addition 1", 1, default = 2)][, vehicle_tag:="acquire_add"]

  addition <- as.data.table(lapply(addition, rep, addition$addition))
  
  vehmodepredict <- rbindlist(list(replace, addition), fill=TRUE)
  
  vehmodepredict <- merge(vehmodepredict[,.(headpid, nextwave_status, addition, vehicle_tag)], 
                          households_thisyear, by="headpid")[,has_car:=has_carTRUE][,has_suv:=has_suvTRUE][
                            ,has_pickup:=has_pickupTRUE][,has_van:=has_vanTRUE]
  
  vehmodepredict <- vehmodepredict[, random:=runif(nrow(vehmodepredict))]
  
  veh_power <- vehicles_thisyear[,ev_experience:=fcase(pred_power %in% c("AEV", "PHEV", "Hybrid"), 1, default = 0)][
    , by=headpid, .(ev_experience=sum(ev_experience))][, ev_experience:=fcase(ev_experience>0, 1, default = 0)]
  
  vehmodepredict <- merge(vehmodepredict, veh_power, by="headpid", all.x=T)
  vehmodepredict <- vehmodepredict[is.na(ev_experience)==T, ev_experience:=0]
  
  return(vehmodepredict)
}


# Second: tune the constants and then split vehicles
new_used <- function(data){
  data <- data[,cons:= 1][,logincome:= fcase(income_fu > 0, log(income_fu+0.001), income_fu <= 0, log(0.001))]
#  data <- data[,cons:= 1][,logincome:= log(income_fu+1)] # LJ 10/22/2022: changge to be consistent with psid regression definition
  
  coefs.tune <- as.matrix(newused.coefs$coefficients)
  
  # initialize the prediction and error metric
  yhat =as.matrix(data[,.(cons,logincome)]) %*% coefs.tune
  data$prob.hat = exp(yhat) / (1 + exp(yhat))  
  
  share.pred = sum(data$prob.hat)/N.occassion
  share.target = local.sale/N.occassion
  
  # error metric is the RMSE root mean squred errors, 
  # note here we have one outcome class, for multi-class outcome, this will be useful
  err.metric = sqrt(mean(share.pred-share.target)^2) # initialize the error metric
  
  while(err.metric > threshold){
    print('calibrating new/used choice constant')
    coefs.tune["(Intercept)",1]<-coefs.tune["(Intercept)",1] + log(local.sale/sum(data$prob.hat))
    yhat =as.matrix(data[,c('cons','logincome')]) %*% coefs.tune
    data$prob.hat = exp(yhat) / (1 + exp(yhat))  
    share.pred = sum(data$prob.hat)/N.occassion
    share.target = local.sale/N.occassion
    err.metric = sqrt(mean(share.pred-share.target)^2) 
    print(err.metric)
  }
  
  #coefs.tune
  data$randnumber = runif(N.occassion)
  data$choose.new = data$prob.hat > data$randnumber
  
  sum(data$choose.new)
  
  data.new = data[choose.new == 1]
  data.new <- data.new[,!c('cons','logincome','prob.hat','randnumber')]
  
  data.used = data[choose.new == 0]
  data.used <- data.used[,!c('cons','logincome','prob.hat','randnumber')]
  
  returnlist <- list(data.new, data.used)
  return(returnlist)
}
