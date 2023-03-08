#============= last step: predict main driver for new hh and newly acquired vehicles for middle year or evolution year
maindriver <- function(vehicle, person){  
  require(data.table)
  require(stats)
  
  vehicle_person <- merge(person[age >= 18], vehicle[,.(headpid, vehicle_id, vehtype, pred_power)], by = "headpid", allow.cartesian=TRUE)
  vehicle_person <- vehicle_person[,R_SEX_IMP:=as.numeric(gender==2)][, high:=as.numeric(edu=="high")][
    ,college:=as.numeric(edu=="college" & edu=="some_college")][,graduate:=as.numeric(edu=="more_than_college")][
      , school:=as.numeric(age<=25 & employ=="not_employed")][
        ,work:=as.numeric(employ=="employed")][, retired_person:=as.numeric(employ=="retired")][
          ,black:=as.numeric(hd_race=="black")][, asian:=as.numeric(hd_race=="asian")][
            ,race_other:=as.numeric(black==0&asian==0)][, van:=as.numeric(vehtype=="van")][
              , suv:=as.numeric(vehtype=="suv")][, pickup:=as.numeric(vehtype=="pickup")][
                ,power_ev:=as.numeric(pred_power=="AEV" |pred_power=="PHEV")]
  
  vehicle_person <- setnames(vehicle_person, c("age"), c("R_AGE_IMP"))
  vehicle_person <- vehicle_person[,power_age:=power_ev*R_AGE_IMP][, van_age:=van*R_AGE_IMP][, suv_age:=suv*R_AGE_IMP][, pickup_age:=pickup*R_AGE_IMP][
    ,power_sex:=power_ev*R_SEX_IMP][, van_sex:=van*R_SEX_IMP][, suv_sex:=suv*R_SEX_IMP][, pickup_sex:=pickup*R_SEX_IMP][
      ,power_high:=power_ev*high][, van_high:=van*high][, suv_high:=suv*high][, pickup_high:=pickup*high][
        ,power_coll:=power_ev*college][, van_coll:=van*college][, suv_coll:=suv*college][, pickup_coll:=pickup*college][
          ,power_grad:=power_ev*graduate][, van_grad:=van*graduate][, suv_grad:=suv*graduate][, pickup_grad:=pickup*graduate][
            ,power_school:=power_ev*school][, van_school:=van*school][, suv_school:=suv*school][, pickup_school:=pickup*school][
              ,power_retired:=power_ev*retired_person][, van_retired:=van*retired_person][, suv_retired:=suv*retired_person][
                , pickup_retired:=pickup*retired_person][,power_full:=power_ev*work][, van_full:=van*work][, suv_full:=suv*work][
                  , pickup_full:=pickup*work][,power_black:=power_ev*black][, van_black:=van*black][, suv_black:=suv*black][
                    , pickup_black:=pickup*black][,power_asian:=power_ev*asian][, van_asian:=van*asian][, suv_asian:=suv*asian][
                      , pickup_asian:=pickup*asian][,power_rother:=power_ev*race_other][, van_rother:=van*race_other][
                        , suv_rother:=suv*race_other][, pickup_rother:=pickup*race_other]
  
  vehicle_person <- cbind(vehicle_person, stats::predict(maindriver_logit3, vehicle_person, type='response'))
  names(vehicle_person)[ncol(vehicle_person)] <- "pred_driver"
  vehicle_person <- vehicle_person[, .SD[which.max(pred_driver)], keyby = c("headpid", "vehicle_id")] 
  vehicle_person <- setnames(vehicle_person[, .(headpid,vehicle_id,pid)], c("pid"), c("maindriver_id"))
  vehicle <- merge(vehicle, vehicle_person, by=c("headpid", "vehicle_id"), all.x=T)
  return(vehicle)
}



p_maindriver <- function(vehicle, person, Npe = Npe){ # LJ 10/18/2022: parallel version
  require(data.table)
  require(stats)
  library(tidyverse)
  library(dplyr)
  library(apollo)
  library(tictoc)
  library(parallel)
  library(doParallel)
  library(foreach)
  
  registerDoParallel(cores = Npe) 
  tic()
  #res <- foreach(i=1:nrow(vehicle), 
  
  tmpi = seq(0, nrow(vehicle), floor(nrow(vehicle)/Npe))
  res <- foreach(i=1:(length(tmpi)-1), 
                                .combine=rbind,
                 .packages = c('dplyr','tidyr','data.table')
  )  %dopar% {
    print(paste('Loop',i, 'out of', Npe))
    
    tmpveh = as.data.table(vehicle[(tmpi[i]+1):(tmpi[i+1])])
    
    maindriver(tmpveh, person=person)
  }
  
  stopImplicitCluster()

  
  
  return(res)
}
