# This step is to tune the used vehicle choice model and made the prediction
vehnm.map <- data.frame(coef.nms = c('adopt_vehminvan','adopt_vehSUV','adopt_vehtruck','adopt_vehvan','adopt_vehcar'),
                        dummy.nms = c('adopt_veh_minvan','adopt_veh_suv','adopt_veh_truck','adopt_veh_van','adopt_veh_car'))

pwrnm.map<- data.frame(coef.nms = c('adopt_fuelcng','adopt_fuelev','adopt_fuelfuelcell','adopt_fuelhybrid','adopt_fuelphev'),
                       dummy.nms = c('adopt_fuel_cng','adopt_fuel_ev','adopt_fuel_fuelcell','adopt_fuel_hybrid','adopt_fuel_phev'))

tunepwr.map <- data.frame(adopt = c('cng','ev','fuelcell','hybrid','phev'),
                          coef = c('adopt_fuelcng','adopt_fuelev','adopt_fuelfuelcell','adopt_fuelhybrid','adopt_fuelphev'))

tunepveh.map <- data.frame(adopt= c('minvan','suv','truck','van'),
                           coef = c('adopt_vehminvan','adopt_vehSUV','adopt_vehtruck','adopt_vehvan'))

tuneage.map <- data.frame(adopt= c('12+ years','6~11 years'),
                          coef = c('veh_age_bins>=13yr','veh_age_bins7-12yr'))


# First: lookup clean
#vehmodeset_clean_used <- function(datainput,rebate.input = 0, tax_credit.input = 0){
# LJ 4/5/2023: in this function, we set the rebate and tax_credit to 0, because in adopt inputs, connor put in some default values using outdated functions
vehmodeset_clean_used <- function(datainput){
  require(tidyr)
  require(dplyr)
  require(fastDummies)
  # prepare the vehicle mode choice set data
  data <- datainput
  # veh type dummies and rename to coef variables
  data <- fastDummies::dummy_cols(data, select_columns = "adopt_veh") 
  tmp.swap <- names(data %>% dplyr::select(contains('adopt_veh_')))
  renm <- vehnm.map%>%filter(dummy.nms %in% tmp.swap) # to get the vehicle types appeared in attribute
  dropnm <- vehnm.map%>%filter(!(dummy.nms %in% tmp.swap))
  setnames(data, renm$dummy.nms, renm$coef.nms)  
  
  if(dim(dropnm)[1]>0){    # if there are nonexisting veh types
    # set the nonexisting vehicle dummy to 0 in the attribute so that we can still create interaction terms
    data[,dropnm$coef.nms]<-0
    # drop the  constant of non-existing vheicle type from coefs
    coef3used<<- coef3used %>% filter(!(coef%in%dropnm$coef.nms))
  }
  rm(renm)
  
  # veh age dummies
  data <- data %>% mutate(`veh_age_bins7-12yr`=case_when(vintage_category=="6~11 years"~1, T~0),
                          `veh_age_bins>=13yr`=case_when(vintage_category=="12+ years"~1, T~0))
  # if some vintage doesn't exist, drop the corresponding coef
  tmp = colSums(data[,c('veh_age_bins7-12yr','veh_age_bins>=13yr')])
  if(tmp['veh_age_bins7-12yr']==0){coef3used<<- coef3used %>% filter(!(coef%in%c('veh_age_bins7-12yr')))}
  if(tmp['veh_age_bins>=13yr']==0){coef3used<<- coef3used %>% filter(!(coef%in%c('veh_age_bins>=13yr')))}
  
  # veh fueltype dummies
  
  data <- fastDummies::dummy_cols(data, select_columns = "adopt_fuel")
  
  tmp.swap <- names(data %>% dplyr::select(contains('adopt_fuel_')))
  renm <- pwrnm.map%>%filter(dummy.nms %in% tmp.swap) # to get the vehicle types appeared in attribute
  dropnm <- pwrnm.map%>%filter(!(dummy.nms %in% tmp.swap))
  setnames(data, renm$dummy.nms, renm$coef.nms)  
  if(dim(dropnm)[1]>0){    # if there are nonexisting veh types
    # set the nonexisting vehicle dummy to 0 in the attribute so that we can still create interaction terms
    data[,dropnm$coef.nms]<-0
    # drop the  constant of non-existing vheicle type from coefs
    coef3used <<- coef3used %>% filter(!(coef %in% dropnm$coef.nms))
  }
  rm(renm)
  
  # other veh charateristics variables
  data <- data %>% mutate(`log(range)` = log(range))
  
  data <- data %>% mutate(rebate = 0,  # only to initialize the incentive variables without computing them yet
                          tax_credit = 0)
  
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

  print('computing incentives based on hh attributes and vehicle attributes')
  tic()
  data$tax_credit <- apply_used_credits(data) * taxfactor
  data$rebate <- apply_used_rebates(data) * rebfactor
  toc()
    
  coefnames <- coef3used$coef
  coefnames <- c(coefnames, "id", "ncar_thiswave", "adopt_veh", "adopt_fuel", "vintage_category", "headpid")
  data = data[, ..coefnames] # here we dropped the columns of alternative specific constants that are not in coefs
  return(data)
}

# Third: tune the coefficients
vehmodechoice_used <- function(data){
  vehmodepredict.used1 <- data[ncar_thiswave<=1]
  vehmodepredict.used2 <- data[ncar_thiswave==2]
  vehmodepredict.used3 <- data[ncar_thiswave>=3]
  
  coefs.tune1 <- as.matrix(coef3used$HH_1)
  coefs.tune2 <- as.matrix(coef3used$HH_2)
  coefs.tune3 <- as.matrix(coef3used$HH_3)
  
  coef.names = coef3used$coef
  rownames(coefs.tune1)<-coef.names
  rownames(coefs.tune2)<-coef.names
  rownames(coefs.tune3)<-coef.names
  
  
  # initialize the prediction and error metric
  yhat1 =as.matrix(vehmodepredict.used1[,..coef.names]) %*% coefs.tune1
  vehmodepredict.used1$yhat <- yhat1
  
  yhat2 =as.matrix(vehmodepredict.used2[,..coef.names]) %*% coefs.tune2
  vehmodepredict.used2$yhat <- yhat2
  
  yhat3 =as.matrix(vehmodepredict.used3[,..coef.names]) %*% coefs.tune3
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
  
  # use matrix
  comp.veh = as.matrix(data.frame(target = share.target.veh$prob.hat, pred = share.pred.veh$prob.hat))
  rownames(comp.veh) = share.target.veh$adopt_veh
  
  comp.fuel = as.matrix(data.frame(target = share.target.fuel$prob.hat, pred = share.pred.fuel$prob.hat))
  rownames(comp.fuel) = share.target.fuel$adopt_fuel
  
  comp.age = as.matrix(data.frame(target = share.target.age$prob.hat, pred = share.pred.age$prob.hat))
  rownames(comp.age) = share.target.age$vintage_category
  
  err.metric.veh = sqrt(mean((comp.veh[,'target'] - comp.veh[,'pred'])^2))
  err.metric.fuel = sqrt(mean((comp.fuel[,'target'] - comp.fuel[,'pred'])^2))
  err.metric.age = sqrt(mean((comp.age[,'target'] - comp.age[,'pred'])^2))
  print('initial error metrics:')
  print(paste("Error of veh", err.metric.veh))
  print(paste("Error of fuel", err.metric.fuel))
  print(paste("Error of age", err.metric.age))
  
  ####### Tune the coefs (remove reference level) #################
  
  i=1
  while((err.metric.veh > threshold)|(err.metric.fuel > threshold)|(err.metric.age > threshold)){

    print(paste0("Running ", i))
    # tune vehicle type first
    # LJ 2/14/2023: 
    #this code is customized to only include minvan, suv, and truck, not including van for first evo step
    # this doesn't work for subsequent years when van is also available
    # LJ 2/27/2023: update to use variable names to tune
 
    a.nms = intersect(share.target.veh$adopt_veh,tunepveh.map$adopt) # adopt names
    coef.to.tune = tunepveh.map%>%filter(adopt %in% a.nms)
    coefs.tune1[coef.to.tune$coef,1] <- coefs.tune1[coef.to.tune$coef,1] + as.matrix(log(comp.veh[coef.to.tune$adopt,'target']/comp.veh[coef.to.tune$adopt,'pred']))
    coefs.tune2[coef.to.tune$coef,1] <- coefs.tune2[coef.to.tune$coef,1] + as.matrix(log(comp.veh[coef.to.tune$adopt,'target']/comp.veh[coef.to.tune$adopt,'pred']))
    coefs.tune3[coef.to.tune$coef,1] <- coefs.tune3[coef.to.tune$coef,1] + as.matrix(log(comp.veh[coef.to.tune$adopt,'target']/comp.veh[coef.to.tune$adopt,'pred']))
    
    
    yhat1 =as.matrix(vehmodepredict.used1[,..coef.names]) %*% coefs.tune1
    vehmodepredict.used1$yhat <- yhat1
    
    yhat2 =as.matrix(vehmodepredict.used2[,..coef.names]) %*% coefs.tune2
    vehmodepredict.used2$yhat <- yhat2
    
    yhat3 =as.matrix(vehmodepredict.used3[,..coef.names]) %*% coefs.tune3
    vehmodepredict.used3$yhat <- yhat3
    rm(yhat1, yhat2, yhat3)
    
    data <- rbind(vehmodepredict.used1, vehmodepredict.used2, vehmodepredict.used3)
    data <- data[, expect_op:=sum(exp(yhat)), by=id][,id:=mean(id), by=id][,prob.hat:=exp(yhat)/expect_op]
    
    # tune fuel type second
    share.pred.fuel = data[,by=adopt_fuel, .(prob.hat=sum(prob.hat)/N.occassion)]
    comp.fuel[,'pred']= share.pred.fuel$prob.hat
    
    a.nms = intersect(share.target.fuel$adopt_fuel,tunepwr.map$adopt) # adopt names
    coef.to.tune = tunepwr.map%>%filter(adopt %in% a.nms)
    
    coefs.tune1[coef.to.tune$coef,1] <- coefs.tune1[coef.to.tune$coef,1] + as.matrix(log(comp.fuel[coef.to.tune$adopt,'target']/comp.fuel[coef.to.tune$adopt,'pred']))
    coefs.tune2[coef.to.tune$coef,1] <- coefs.tune2[coef.to.tune$coef,1] + as.matrix(log(comp.fuel[coef.to.tune$adopt,'target']/comp.fuel[coef.to.tune$adopt,'pred']))
    coefs.tune3[coef.to.tune$coef,1] <- coefs.tune3[coef.to.tune$coef,1] + as.matrix(log(comp.fuel[coef.to.tune$adopt,'target']/comp.fuel[coef.to.tune$adopt,'pred']))
    
    # get the prediction after two-step tunes
    yhat1 =as.matrix(vehmodepredict.used1[,..coef.names]) %*% coefs.tune1
    vehmodepredict.used1$yhat <- yhat1
    
    yhat2 =as.matrix(vehmodepredict.used2[,..coef.names]) %*% coefs.tune2
    vehmodepredict.used2$yhat <- yhat2
    
    yhat3 =as.matrix(vehmodepredict.used3[,..coef.names]) %*% coefs.tune3
    vehmodepredict.used3$yhat <- yhat3
    rm(yhat1, yhat2, yhat3)
    
    data <- rbind(vehmodepredict.used1, vehmodepredict.used2, vehmodepredict.used3)
    data <- data[, expect_op:=sum(exp(yhat)), by=id][,id:=mean(id), by=id][,prob.hat:=exp(yhat)/expect_op]
    
    # tune vintage category third
 
    share.pred.age = data[,by=vintage_category, .(prob.hat=sum(prob.hat)/N.occassion)]
    comp.age[,'pred']= share.pred.age$prob.hat
    
    a.nms = intersect(share.target.age$vintage_category,tuneage.map$adopt) # adopt names
    coef.to.tune = tuneage.map%>%filter(adopt %in% a.nms)
    
    coefs.tune1[coef.to.tune$coef,1] <- coefs.tune1[coef.to.tune$coef,1] + as.matrix(log(comp.age[coef.to.tune$adopt,'target']/comp.age[coef.to.tune$adopt,'pred']))
    coefs.tune2[coef.to.tune$coef,1] <- coefs.tune2[coef.to.tune$coef,1] + as.matrix(log(comp.age[coef.to.tune$adopt,'target']/comp.age[coef.to.tune$adopt,'pred']))
    coefs.tune3[coef.to.tune$coef,1] <- coefs.tune3[coef.to.tune$coef,1] + as.matrix(log(comp.age[coef.to.tune$adopt,'target']/comp.age[coef.to.tune$adopt,'pred']))
    
    yhat1 =as.matrix(vehmodepredict.used1[,..coef.names]) %*% coefs.tune1
    vehmodepredict.used1$yhat <- yhat1
    
    yhat2 =as.matrix(vehmodepredict.used2[,..coef.names]) %*% coefs.tune2
    vehmodepredict.used2$yhat <- yhat2
    
    yhat3 =as.matrix(vehmodepredict.used3[,..coef.names]) %*% coefs.tune3
    vehmodepredict.used3$yhat <- yhat3
    rm(yhat1, yhat2, yhat3)
    
    data <- rbind(vehmodepredict.used1, vehmodepredict.used2, vehmodepredict.used3)
    data <- data[, expect_op:=sum(exp(yhat)), by=id][,id:=mean(id), by=id][,prob.hat:=exp(yhat)/expect_op]
    
    share.pred.veh = data[, by=adopt_veh, .(prob.hat=sum(prob.hat)/N.occassion)]
    share.pred.fuel = data[,by=adopt_fuel, .(prob.hat=sum(prob.hat)/N.occassion)]
    share.pred.age = data[,by=vintage_category, .(prob.hat=sum(prob.hat)/N.occassion)]

    comp.fuel[,'pred']= share.pred.fuel$prob.hat
    comp.veh[,'pred']= share.pred.veh$prob.hat
    comp.age[,'pred']= share.pred.age$prob.hat
    
    err.metric.veh = sqrt(mean((comp.veh[,'target'] - comp.veh[,'pred'])^2))
    err.metric.fuel = sqrt(mean((comp.fuel[,'target'] - comp.fuel[,'pred'])^2))
    err.metric.age = sqrt(mean((comp.age[,'target'] - comp.age[,'pred'])^2))
    
    print(paste("Error of veh", err.metric.veh))
    print(paste("Error of fuel", err.metric.fuel))
    print(paste("Error of age", err.metric.age))
    
    i <- i+1
    gc()
  }
  data <- data[,by=id, cumutility:=cumsum(prob.hat)][,by=id, random:=runif(1)][
    ,by=id, whichone:=cumsum((cumutility>=random))][whichone==1][,.(headpid, id, adopt_veh, adopt_fuel, vintage_category, random)]
  
  coef3used$HH_1 = as.vector(coefs.tune1)
  coef3used$HH_2 = as.vector(coefs.tune2)
  coef3used$HH_3 = as.vector(coefs.tune3)
  
  return(list(predicted = data, coef3used.tuned = coef3used ))
  }

# Forth: get the exponential rate
get_vint3exp <- function(data){
  data <- data[deltayear<=(evoyear-13)][,vintage:=evoyear-deltayear]
  data <- data[, by=vintage, .(n=.N)]
  vint3 <- lm(log(n)~vintage, data=data)
  return(vint3)
}
