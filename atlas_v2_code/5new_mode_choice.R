# This step is to tune the new vehicle choice model and made the prediction

# First: lookup clean # LJ: note that we do not have incentives in the data yet, so here we manually set them to zero
vehmodeset_clean <- function(datainput, rebate.input = NA, tax_credit.input = NA){
  require(tidyr)
  require(dplyr)
  require(fastDummies)
  # prepare the vehicle mode choice set data
  data <- datainput
  # veh type dummies
  data <- fastDummies::dummy_cols(data, select_columns = "adopt_veh") %>%
    rename(adopt_vehminvan=adopt_veh_minvan, adopt_vehSUV=adopt_veh_SUV,
           adopt_vehtruck=adopt_veh_truck, adopt_vehvan=adopt_veh_van, adopt_vehcar=adopt_veh_car)
  
  # veh age dummies
  data <- data %>% mutate(`veh_age_bins1-2yr`=0,
                          `veh_age_bins>=3yr`=0)
  
  # veh fueltype dummies
  data <- fastDummies::dummy_cols(data, select_columns = "adopt_fuel") %>%
    rename(adopt_fuelcng=adopt_fuel_cng, adopt_fuelev=adopt_fuel_ev,
           adopt_fuelfuelcell=adopt_fuel_fuelcell, adopt_fuelhybrid=adopt_fuel_hybrid,
           adopt_fuelphev=adopt_fuel_phev)
  
  # other veh charateristics variables
  data <- data %>% mutate(`log(range)` = log(range))
  
  # incentives if non-na values are given, otherwide there should be rebate and tax_credit in the attributes
  if(!is.na(rebate.input)){data <- data %>% mutate(rebate = rebate.input)}
  if(!is.na(tax_credit.input)){data <- data %>% mutate(tax_credit = tax_credit.input)}

  return(data)
}

# Second: prepare vehmodepredict for new vehicles
vehmodepredict_newfunc <- function(data){
  data = merge(data[,m:=1][,id:= as.numeric(row.names(.SD))], lookup%>%mutate(m=1), by="m", allow.cartesian=TRUE)
  
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
  coefnames <- c(coefnames, "id", "ncar_thiswave", "adopt_veh", "adopt_fuel", "headpid")
  data = data[, ..coefnames]
  return(data)
}

# Third: tune the coefficients
vehmodechoice_new <- function(data, local.factor = T){ # LJ 10/22/2022: if local.factor == T, apply a calibrated correction factor
  vehmodepredict.new1 <- data[ncar_thiswave<=1]
  vehmodepredict.new2 <- data[ncar_thiswave==2]
  vehmodepredict.new3 <- data[ncar_thiswave>=3]
  
  coefs.tune1 <- as.matrix(coef3$HH_1)
  coefs.tune2 <- as.matrix(coef3$HH_2)
  coefs.tune3 <- as.matrix(coef3$HH_3)
  
  # initialize the prediction and error metric
  # LJ comment: this needs to be recoded with variable names
  yhat1 =as.matrix(vehmodepredict.new1[,c(1:31)]) %*% coefs.tune1
  vehmodepredict.new1$yhat <- yhat1
  
  yhat2 =as.matrix(vehmodepredict.new2[,c(1:31)]) %*% coefs.tune2
  vehmodepredict.new2$yhat <- yhat2
  
  yhat3 =as.matrix(vehmodepredict.new3[,c(1:31)]) %*% coefs.tune3
  vehmodepredict.new3$yhat <- yhat3
  rm(yhat1, yhat2, yhat3)
  
  data <- rbind(vehmodepredict.new1, vehmodepredict.new2, vehmodepredict.new3)
  data <- data[, expect_op:=sum(exp(yhat)), by=id][,id:=mean(id), by=id][,prob.hat:=exp(yhat)/expect_op]
  
  Nsale = sum(local.sale.new$sales) # note the global variable Nsale was national totals, so here we need to recalc local totals 
  share.pred.veh = data[, by=adopt_veh, .(prob.hat=sum(prob.hat)/N.occassion)]
  share.target.veh = local.sale.new[, by=adopt_veh, .(prob.hat=sum(sales)/Nsale)] # LJ 10/22/2022: change N.occassion to Nsale
  share.pred.fuel = data[,by=adopt_fuel, .(prob.hat=sum(prob.hat)/N.occassion)]
  share.target.fuel = local.sale.new[, by=adopt_fuel, .(prob.hat=sum(sales)/Nsale)]
  
  if(local.factor){
    # if need to apply local correction factor for ev and phev sales shares
    share.target.fuel$prob.hat[share.target.fuel$adopt_fuel == 'ev'] = share.target.fuel$prob.hat[share.target.fuel$adopt_fuel == 'ev'] * ev.share.factor 
    share.target.fuel$prob.hat[share.target.fuel$adopt_fuel == 'phev'] = share.target.fuel$prob.hat[share.target.fuel$adopt_fuel == 'phev'] * phev.share.factor 
    tmp.sum = sum(share.target.fuel$prob.hat[!(share.target.fuel$adopt_fuel %in% c('phev','ev'))])
    
    tmp.sum2 = sum(share.target.fuel$prob.hat[(share.target.fuel$adopt_fuel %in% c('phev','ev'))])
    
    if(tmp.sum2 >1){
      # if after adjustment more than 100%, scale to 100%
      share.target.fuel$prob.hat[(share.target.fuel$adopt_fuel %in% c('phev','ev'))] = 
        share.target.fuel$prob.hat[(share.target.fuel$adopt_fuel %in% c('phev','ev'))] /tmp.sum2
      tmp.sum2 = 1
    }
    if(tmp.sum>0){ # rescale other fuel type only when they have nonzero shares
      share.target.fuel$prob.hat[!(share.target.fuel$adopt_fuel %in% c('phev','ev'))] = 
        share.target.fuel$prob.hat[!(share.target.fuel$adopt_fuel %in% c('phev','ev'))] * (1-tmp.sum2)/tmp.sum
      tmp.sum = sum(share.target.fuel$prob.hat[!(share.target.fuel$adopt_fuel %in% c('phev','ev'))])
      
      }
    
    }# if local factor = T

  err.metric.veh = sqrt(colMeans(abs((share.pred.veh[,2]-share.target.veh[,2])[,1]))^2)
  err.metric.fuel = sqrt(colMeans(abs((share.pred.fuel[,2]-share.target.fuel[,2])[,1]))^2)
  
  # test: loop 10 times succeed!
  i=1
  while(isTRUE(err.metric.veh > threshold)|isTRUE(err.metric.fuel > threshold)){
    print(paste0("calibrate new vehice choice: Running ", i))
     # tune vehicle type first
    coefs.tune1[1:4,1] <- coefs.tune1[1:4,1] + as.matrix(log(share.target.veh[2:5,2]/share.pred.veh[2:5,2]))
    coefs.tune2[1:4,1] <- coefs.tune2[1:4,1] + as.matrix(log(share.target.veh[2:5,2]/share.pred.veh[2:5,2]))
    coefs.tune3[1:4,1] <- coefs.tune3[1:4,1] + as.matrix(log(share.target.veh[2:5,2]/share.pred.veh[2:5,2]))
    
    yhat1 =as.matrix(vehmodepredict.new1[,c(1:31)]) %*% coefs.tune1
    vehmodepredict.new1$yhat <- yhat1
    
    yhat2 =as.matrix(vehmodepredict.new2[,c(1:31)]) %*% coefs.tune2
    vehmodepredict.new2$yhat <- yhat2
    
    yhat3 =as.matrix(vehmodepredict.new3[,c(1:31)]) %*% coefs.tune3
    vehmodepredict.new3$yhat <- yhat3
    rm(yhat1, yhat2, yhat3)
    
    data <- rbind(vehmodepredict.new1, vehmodepredict.new2, vehmodepredict.new3)
    data <- data[, expect_op:=sum(exp(yhat)), by=id][,id:=mean(id), by=id][,prob.hat:=exp(yhat)/expect_op]
    
    
    # tune fuel type second
    share.pred.fuel = data[,by=adopt_fuel, .(prob.hat=sum(prob.hat)/N.occassion)]
    coefs.tune1[18:22,1] <- coefs.tune1[18:22,1] + as.matrix(log(share.target.fuel[c(1,3:6),2]/share.pred.fuel[c(1,3:6),2]))
    coefs.tune2[18:22,1] <- coefs.tune2[18:22,1] + as.matrix(log(share.target.fuel[c(1,3:6),2]/share.pred.fuel[c(1,3:6),2]))
    coefs.tune3[18:22,1] <- coefs.tune3[18:22,1] + as.matrix(log(share.target.fuel[c(1,3:6),2]/share.pred.fuel[c(1,3:6),2]))
    
    # get the prediction after two-step tunes
    yhat1 =as.matrix(vehmodepredict.new1[,c(1:31)]) %*% coefs.tune1
    vehmodepredict.new1$yhat <- yhat1
    
    yhat2 =as.matrix(vehmodepredict.new2[,c(1:31)]) %*% coefs.tune2
    vehmodepredict.new2$yhat <- yhat2
    
    yhat3 =as.matrix(vehmodepredict.new3[,c(1:31)]) %*% coefs.tune3
    vehmodepredict.new3$yhat <- yhat3
    rm(yhat1, yhat2, yhat3)
    
    data <- rbind(vehmodepredict.new1, vehmodepredict.new2, vehmodepredict.new3)
    data <- data[, expect_op:=sum(exp(yhat)), by=id][,id:=mean(id), by=id][,prob.hat:=exp(yhat)/expect_op]
    
    # LJ 10/22/2022: deleted the recalculation of target shares.
    share.pred.veh = data[, by=adopt_veh, .(prob.hat=sum(prob.hat)/N.occassion)]
    share.pred.fuel = data[,by=adopt_fuel, .(prob.hat=sum(prob.hat)/N.occassion)]
     
    err.metric.veh = sqrt(colMeans(abs((share.pred.veh[,2]-share.target.veh[,2])[,1]))^2)
    err.metric.fuel = sqrt(colMeans(abs((share.pred.fuel[,2]-share.target.fuel[,2])[,1]))^2)
    
    print(paste("Error of veh", err.metric.veh))
    print(paste("Error of fuel", err.metric.fuel))
    
    i <- i+1
    #gc()
  }
  
  data <- data[,by=id, cumutility:=cumsum(prob.hat)][,by=id, random:=runif(1)][
    ,by=id, whichone:=cumsum((cumutility>=random))][whichone==1][,.(headpid, id, adopt_veh, adopt_fuel, random)]
  
  coef3$HH_1 = as.vector(coefs.tune1)
  coef3$HH_2 = as.vector(coefs.tune2)
  coef3$HH_3 = as.vector(coefs.tune3)
  
  return(list(predicted = data, coef3.tuned = coef3 ))
}
