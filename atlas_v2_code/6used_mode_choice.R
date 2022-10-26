# This step is to tune the used vehicle choice model and made the prediction

# First: lookup clean
vehmodeset_clean_used <- function(datainput,rebate.input = 0, tax_credit.input = 0){
  require(tidyr)
  require(dplyr)
  require(fastDummies)
  # prepare the vehicle mode choice set data
  data <- datainput
  # veh type dummies
  data <- fastDummies::dummy_cols(data, select_columns = "adopt_veh") %>%
    rename(adopt_vehminvan=adopt_veh_minvan, adopt_vehSUV=adopt_veh_suv,
           adopt_vehtruck=adopt_veh_truck, adopt_vehcar=adopt_veh_car) 
  
  if ("adopt_veh_van" %in% names(data)){
    data <- data %>% rename(adopt_vehvan=adopt_veh_van)
  }else{
    data <- data %>% mutate(adopt_vehvan=0)
  }
  
  # veh age dummies
  data <- data %>% mutate(`veh_age_bins7-12yr`=case_when(vintage_category=="6~11 years"~1, T~0),
                          `veh_age_bins>=13yr`=case_when(vintage_category=="12+ years"~1, T~0))
  
  # veh fueltype dummies
  data <- fastDummies::dummy_cols(data, select_columns = "adopt_fuel") %>%
    rename(adopt_fuelev=adopt_fuel_ev,
           adopt_fuelhybrid=adopt_fuel_hybrid,
           adopt_fuelphev=adopt_fuel_phev)
  if ("adopt_fuel_fuelcell" %in% names(data)){
    data <- data %>% rename(adopt_fuelfuelcell=adopt_fuel_fuelcell)
  }else{
    data <- data %>% mutate(adopt_fuelfuelcell=0)
  }
  if ("adopt_fuel_cng" %in% names(data)){
    data <- data %>% rename(adopt_fuelcng=adopt_fuel_cng)
  }else{
    data <- data %>% mutate(adopt_fuelcng=0)
  }
  
  # other veh charateristics variables
  data <- data %>% mutate(`log(range)` = log(range))
  
  # incentives
  data <- data %>% mutate(rebate = rebate.input, tax_credit = tax_credit.input)
  
  return(data)
}

# Second: prepare vehmodepredict for used vehicles
vehmodepredict_usedfunc <- function(data){
  data <- data[,m:=1][,id:= as.numeric(row.names(.SD))]
  data = merge(data, lookup%>%mutate(m=1), by="m", allow.cartesian=TRUE)
  
  data <- data[,has_kid:=(Nkid_4+Nkid_5_11+Nkid_12_15)>0][, kids_vehminvan:=adopt_vehminvan*has_kid][
    ,kids_vehSUV:=adopt_vehSUV*has_kid][,kids_vehtruck:=adopt_vehtruck*has_kid][
      ,kids_vehvan:=adopt_vehvan*has_kid][,pickup_pickupowerTRUE:=adopt_vehtruck*has_pickup][
        ,elec_owner_ADOPTvehev:=ev_experience*adopt_fuelev][, elec_owner_ADOPTvehfuelcell:=ev_experience*adopt_fuelfuelcell][
          ,elec_owner_ADOPTvehhybrid:=ev_experience*adopt_fuelhybrid][
            ,elec_owner_ADOPTvehphev:=ev_experience*adopt_fuelphev][
              ,`price:log(income_bin_mid)`:=fcase(income_fu>2, price * log(income_fu), income_fu<=2, price * log(2))]
  
  data <- data[,`replace_veh_matchTRUE`:= fcase(nextwave_status=="replace"&has_car==1&adopt_vehcar==1, 1,
                                                nextwave_status=="replace"&has_car==1&adopt_vehminvan==1, 1, # for now has_car with minivan
                                                nextwave_status=="replace"&has_suv==1&adopt_vehSUV==1, 1,
                                                nextwave_status=="replace"&has_van==1&adopt_vehvan==1, 1,
                                                nextwave_status=="replace"&has_pickup==1&adopt_vehtruck==1, 1, default = 0)]
  
  coefnames <- coef3$coef
  coefnames <- c(coefnames, "id", "ncar_thiswave", "adopt_veh", "adopt_fuel", "vintage_category", "headpid")
  data = data[, ..coefnames]
  return(data)
}

# Third: tune the coefficients
vehmodechoice_used <- function(data){
  vehmodepredict.used1 <- data[ncar_thiswave<=1]
  vehmodepredict.used2 <- data[ncar_thiswave==2]
  vehmodepredict.used3 <- data[ncar_thiswave>=3]
  
  coefs.tune1 <- as.matrix(coef3$HH_1)
  coefs.tune2 <- as.matrix(coef3$HH_2)
  coefs.tune3 <- as.matrix(coef3$HH_3)
  
  # initialize the prediction and error metric
  yhat1 =as.matrix(vehmodepredict.used1[,c(1:31)]) %*% coefs.tune1
  vehmodepredict.used1$yhat <- yhat1
  
  yhat2 =as.matrix(vehmodepredict.used2[,c(1:31)]) %*% coefs.tune2
  vehmodepredict.used2$yhat <- yhat2
  
  yhat3 =as.matrix(vehmodepredict.used3[,c(1:31)]) %*% coefs.tune3
  vehmodepredict.used3$yhat <- yhat3
  rm(yhat1, yhat2, yhat3)
  
  data <- rbind(vehmodepredict.used1, vehmodepredict.used2, vehmodepredict.used3)
  data <- data[, expect_op:=sum(exp(yhat)), by=id][,id:=mean(id), by=id][,prob.hat:=exp(yhat)/expect_op]
  
  share.pred.veh = data[, by=adopt_veh, .(prob.hat=sum(prob.hat)/N.occassion)]
  share.target.veh = attribute[, by=adopt_veh, .(prob.hat=sum(n)/sum(attribute$n))]
  share.pred.fuel = data[,by=adopt_fuel, .(prob.hat=sum(prob.hat)/N.occassion)]
  share.target.fuel = attribute[, by=adopt_fuel, .(prob.hat=sum(n)/sum(attribute$n))]
  share.pred.age = data[,by=vintage_category, .(prob.hat=sum(prob.hat)/N.occassion)]
  share.target.age = attribute[, by=vintage_category, .(prob.hat=sum(n)/sum(attribute$n))]
  
  err.metric.veh = sqrt(colMeans(abs((share.pred.veh[,2]-share.target.veh[,2])[,1]))^2)
  err.metric.fuel = sqrt(colMeans(abs((share.pred.fuel[,2]-share.target.fuel[,2])[,1]))^2)
  err.metric.age = sqrt(colMeans(abs((share.pred.age[,2]-share.target.age[,2])[,1]))^2)
  
  # test: loop 10 times succeed!
  i=1
  while((err.metric.veh > threshold)|(err.metric.fuel > threshold)|(err.metric.age > threshold)){
    print(paste0("Running ", i))
    # tune vehicle type first
    coefs.tune1[1:3,1] <- coefs.tune1[1:3,1] + as.matrix(log(share.target.veh[2:4,2]/share.pred.veh[2:4,2]))
    coefs.tune2[1:3,1] <- coefs.tune2[1:3,1] + as.matrix(log(share.target.veh[2:4,2]/share.pred.veh[2:4,2]))
    coefs.tune3[1:3,1] <- coefs.tune3[1:3,1] + as.matrix(log(share.target.veh[2:4,2]/share.pred.veh[2:4,2]))
    
    yhat1 =as.matrix(vehmodepredict.used1[,c(1:31)]) %*% coefs.tune1
    vehmodepredict.used1$yhat <- yhat1
    
    yhat2 =as.matrix(vehmodepredict.used2[,c(1:31)]) %*% coefs.tune2
    vehmodepredict.used2$yhat <- yhat2
    
    yhat3 =as.matrix(vehmodepredict.used3[,c(1:31)]) %*% coefs.tune3
    vehmodepredict.used3$yhat <- yhat3
    rm(yhat1, yhat2, yhat3)
    
    data <- rbind(vehmodepredict.used1, vehmodepredict.used2, vehmodepredict.used3)
    data <- data[, expect_op:=sum(exp(yhat)), by=id][,id:=mean(id), by=id][,prob.hat:=exp(yhat)/expect_op]
    
    # tune fuel type second
    share.pred.fuel = data[,by=adopt_fuel, .(prob.hat=sum(prob.hat)/N.occassion)]
    
    coefs.tune1[c(19,21,22),1] <- coefs.tune1[c(19,21,22),1] + as.matrix(log(share.target.fuel[c(2:4),2]/share.pred.fuel[c(2:4),2]))
    coefs.tune2[c(19,21,22),1] <- coefs.tune2[c(19,21,22),1] + as.matrix(log(share.target.fuel[c(2:4),2]/share.pred.fuel[c(2:4),2]))
    coefs.tune3[c(19,21,22),1] <- coefs.tune3[c(19,21,22),1] + as.matrix(log(share.target.fuel[c(2:4),2]/share.pred.fuel[c(2:4),2]))
    
    # get the prediction after two-step tunes
    yhat1 =as.matrix(vehmodepredict.used1[,c(1:31)]) %*% coefs.tune1
    vehmodepredict.used1$yhat <- yhat1
    
    yhat2 =as.matrix(vehmodepredict.used2[,c(1:31)]) %*% coefs.tune2
    vehmodepredict.used2$yhat <- yhat2
    
    yhat3 =as.matrix(vehmodepredict.used3[,c(1:31)]) %*% coefs.tune3
    vehmodepredict.used3$yhat <- yhat3
    rm(yhat1, yhat2, yhat3)
    
    data <- rbind(vehmodepredict.used1, vehmodepredict.used2, vehmodepredict.used3)
    data <- data[, expect_op:=sum(exp(yhat)), by=id][,id:=mean(id), by=id][,prob.hat:=exp(yhat)/expect_op]
    
    # tune vintage category third
    share.pred.age = data[,by=vintage_category, .(prob.hat=sum(prob.hat)/N.occassion)]
    
    coefs.tune1[c(5:6),1] <- coefs.tune1[c(5:6),1] + as.matrix(log(share.target.age[c(2,1),2]/share.pred.age[c(2,1),2]))
    coefs.tune2[c(5:6),1] <- coefs.tune2[c(5:6),1] + as.matrix(log(share.target.age[c(2,1),2]/share.pred.age[c(2,1),2]))
    coefs.tune3[c(5:6),1] <- coefs.tune3[c(5:6),1] + as.matrix(log(share.target.age[c(2,1),2]/share.pred.age[c(2,1),2]))
    
    yhat1 =as.matrix(vehmodepredict.used1[,c(1:31)]) %*% coefs.tune1
    vehmodepredict.used1$yhat <- yhat1
    
    yhat2 =as.matrix(vehmodepredict.used2[,c(1:31)]) %*% coefs.tune2
    vehmodepredict.used2$yhat <- yhat2
    
    yhat3 =as.matrix(vehmodepredict.used3[,c(1:31)]) %*% coefs.tune3
    vehmodepredict.used3$yhat <- yhat3
    rm(yhat1, yhat2, yhat3)
    
    data <- rbind(vehmodepredict.used1, vehmodepredict.used2, vehmodepredict.used3)
    data <- data[, expect_op:=sum(exp(yhat)), by=id][,id:=mean(id), by=id][,prob.hat:=exp(yhat)/expect_op]
    
    share.pred.veh = data[, by=adopt_veh, .(prob.hat=sum(prob.hat)/N.occassion)]
    share.pred.fuel = data[,by=adopt_fuel, .(prob.hat=sum(prob.hat)/N.occassion)]
    share.pred.age = data[,by=vintage_category, .(prob.hat=sum(prob.hat)/N.occassion)]

    err.metric.veh = sqrt(colMeans(abs((share.pred.veh[,2]-share.target.veh[,2])[,1]))^2)
    err.metric.fuel = sqrt(colMeans(abs((share.pred.fuel[,2]-share.target.fuel[,2])[,1]))^2)
    err.metric.age = sqrt(colMeans(abs((share.pred.age[,2]-share.target.age[,2])[,1]))^2)
    
    print(paste("Error of veh", err.metric.veh))
    print(paste("Error of fuel", err.metric.fuel))
    print(paste("Error of age", err.metric.age))
    
    i <- i+1
    gc()
  }
  data <- data[,by=id, cumutility:=cumsum(prob.hat)][,by=id, random:=runif(1)][
    ,by=id, whichone:=cumsum((cumutility>=random))][whichone==1][,.(headpid, id, adopt_veh, adopt_fuel, vintage_category, random)]
}

# Forth: get the exponential rate
get_vint3exp <- function(data){
  data <- data[deltayear<=(evoyear-13)][,vintage:=evoyear-deltayear]
  data <- data[, by=vintage, .(n=.N)]
  vint3 <- lm(log(n)~vintage, data=data)
  return(vint3)
}
