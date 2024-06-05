# step1: this script process the vehicle data set and apply the dispose model to the dataset

# step1.1: clean the data for model prediction
dipose_clean <- function(vehicles, initialyear, yreffect){
  if (initialyear==T){
    vehicles <- vehicles[,yrs_inFu_nextwave:=NA]
  }else{
    vehicles <- vehicles[,yrs_inFu_nextwave:=evoyear-acquire_year]
  }
  
  for (yr in seq(2005, 2017, by = 2)){ # this fake year effect part can be changed
    if (yreffect==yr){
      vehicles[ ,paste0("year", yr)] <- 1
    }else{
      vehicles[ ,paste0("year", yr)] <- 0
    }
  }

 
  vehicles <- setnames(vehicles, c("nvehicles"), c("ncar_thiswave"))[
    , vintage_nextwave:= evoyear - deltayear][
    , carown_bins := fcase(is.na(ncar_thiswave) | is.na(N_driving+ch_Ndriving), NA_character_,
                           ncar_thiswave ==0, 'no_veh', ncar_thiswave >0 & ncar_thiswave < (N_driving+ch_Ndriving), 'veh_deficient',
                           ncar_thiswave >= (N_driving+ch_Ndriving), 'veh_sufficient')]
  
  vehicles <- vehicles[, vintage_nxt_bins:= fcase(
    vintage_nextwave <=2, 1,
    vintage_nextwave >=3 & vintage_nextwave <= 6, 2,
    vintage_nextwave >=7 & vintage_nextwave <= 11,3,
    vintage_nextwave >=12, 4, default = NA_real_)][
      , vintage_nxt_bins_2:=as.numeric(vintage_nxt_bins==2)][
        , vintage_nxt_bins_3:=as.numeric(vintage_nxt_bins==3)][
          , vintage_nxt_bins_4:=as.numeric(vintage_nxt_bins==4)]
  
  vehicles <- vehicles[, hd_raceblack:= as.numeric(hd_race=="black")][
    , hd_raceother:= as.numeric(hd_race=="other")][
      , hd_racewhite:= as.numeric(hd_race=="white")][
        , `hh_edu>clg`:= as.numeric(hh_edu==">clg")][
          , `hh_educlg`:= as.numeric(hh_edu=="clg")][
            , `hh_edusome clg`:= as.numeric(hh_edu=="some clg")][
              , `hh_eduhi`:= as.numeric(hh_edu=="hi")]
  
  vehicles <- vehicles[, vehtypecar:= as.numeric(vehtype == 'car')][
    , vehtypeutility:= as.numeric(vehtype == 'suv')][
      , vehtypevan:= as.numeric(vehtype == 'van')][
        , vehtypepickup:= as.numeric(vehtype == 'pickup')][,const:= 1][
          , ownlease:= as.numeric(ownlease=="own")][
            , vintage_nextwave_3_5:= as.numeric(vintage_nextwave >=3 & vintage_nextwave <=5)][
              , lease_vintage_nextwave_3_5:= vintage_nextwave_3_5 * (ownlease == 0)]
  
  vehicles <- vehicles[, keyby=.(year, headpid), .(
    N_car = sum(vehtype == 'car', na.rm =T),
    N_suv = sum(vehtype == 'suv', na.rm =T),
    N_pickup = sum(vehtype == 'pickup', na.rm =T),
    N_van = sum(vehtype == 'van', na.rm =T),
    minyrs_withfu = fcase (  # how many years since ever acquired a vehicle
      sum(!is.na(yrs_inFu_nextwave))>0, min(yrs_inFu_nextwave, na.rm =T),
      default =  NA_real_))][, has_car:= !is.na(N_car) & N_car>0][
                           ,has_suv:= !is.na(N_suv) & N_suv>0][
                           ,has_pickup:= !is.na(N_pickup) & N_pickup>0][
                           ,has_van:= !is.na(N_van) & N_van >0] %>% 
    merge(vehicles, by=c("headpid", "year"))
  
  return(vehicles)
}

# step1.2: prepare for the coefficients
dispose_prepare <- function(coef){
  cons <- unname(coef[1:numalt-1])
  coefs_name_disp <- c('const', names(coef)[seq(from = numalt, to = length(coef), by = numalt-1)])
  coefs_name_disp <- str_replace_all(coefs_name_disp, ":disp", "")
  coefs_disp <- c(cons[1], unname(coef)[seq(from = numalt, to = length(coef), by = numalt-1)])
  
  coefs_name_replace <- c('const', names(coef)[seq(from = numalt+1, to = length(coef), by = numalt-1)])
  coefs_name_replace <- str_replace_all(coefs_name_replace, ":replace", "")
  coefs_replace <- c(cons[2], unname(coef)[seq(from = numalt+1, to = length(coef), by = numalt-1)])
  
  coefs_name_keep <- c('const')
  coefs_keep <- c(0)
  
  names <- list(coefs_name_disp, coefs_name_replace, coefs_name_keep);
  values <- list(coefs_disp, coefs_replace, coefs_keep)
  coef_dispose <- list(names, values)
  return(coef_dispose);
}


# step1.3: prediction
utility <- function(names, coefs, data) {
  util_veh <- as.matrix(setcolorder(data, names)[, ..names]) %*% coefs
  return (exp(util_veh));
}

Dispose_model <- function(data, coefs_names, coefs_vehs) {
  
  coefs_name_disp <- c(coefs_names[[1]]);
  coefs_name_replace <- c(coefs_names[[2]]);
  coefs_name_keep <- c(coefs_names[[3]]);
  
  coefs_disp <- c(coefs_vehs[[1]]);
  coefs_replace <- c(coefs_vehs[[2]]);
  coefs_keep <- c(coefs_vehs[[3]]);
  
  ### Utility computation 
  util_disp <- utility(coefs_name_disp, coefs_disp, data);
  util_replace <- utility(coefs_name_replace, coefs_replace, data);
  util_keep <- utility(coefs_name_keep, coefs_keep, data);
  
  ### Probability computation
  total_util <- util_disp + util_replace + util_keep ;
  prob_disp <- (util_disp/total_util);
  prob_replace <- (util_replace/total_util);
  prob_keep <- (util_keep/total_util);
  
  
  ### Cumulative probability computation
  cum_prob_disp <- prob_disp;
  cum_prob_replace <- cum_prob_disp +  prob_replace;
  cum_prob_keep <- cum_prob_replace +  prob_keep;
  
  
  ### Predicted choices
  NumHousehold <- nrow(data);
  rnum <- runif(NumHousehold);
  rnum[rnum < cum_prob_disp] <- "dispose";
  rnum[rnum < cum_prob_replace] <- "replace";
  rnum[rnum!="dispose"&rnum!="replace"] <- "keep";
  rnum[is.na(total_util)] <- NA;
  data <- data[,nextwave_status:=rnum];
  
  tmp <- data[,.(headpid, vehicle_id, nextwave_status, ncar_thiswave)];
  return(tmp);
}

dispose_apply <- function(data){
  data1 <- data[is.na(yrs_inFu_nextwave)==T & ncar_thiswave==1] #
  coef_names_dispose <- dispose_prepare(c(reslist2$oneCarFU[,1], geocoeff1))[[1]]
  coef_dispose <- dispose_prepare(c(reslist2$oneCarFU[,1], geocoeff1))[[2]]
  tmp1 <- Dispose_model(data1, coef_names_dispose, coef_dispose)
  
  data3 <- data[is.na(yrs_inFu_nextwave)==T & ncar_thiswave>1]
  coef_names_dispose <- dispose_prepare(c(reslist2$moreCarFU[,1], geocoeff2))[[1]]
  coef_dispose <- dispose_prepare(c(reslist2$moreCarFU[,1], geocoeff2))[[2]]
  tmp3 <- Dispose_model(data3, coef_names_dispose, coef_dispose)

  data2 <- data[is.na(yrs_inFu_nextwave)==F & ncar_thiswave==1]
  coef_names_dispose <- dispose_prepare(c(reslist$oneCarFU[,1], geocoeff1))[[1]]
  coef_dispose <- dispose_prepare(c(reslist$oneCarFU[,1], geocoeff1))[[2]]
  tmp2 <- Dispose_model(data2, coef_names_dispose, coef_dispose)

  data4 <- data[is.na(yrs_inFu_nextwave)==F & ncar_thiswave>1]
  coef_names_dispose <- dispose_prepare(c(reslist$moreCarFU[,1], geocoeff2))[[1]]
  coef_dispose <- dispose_prepare(c(reslist$moreCarFU[,1], geocoeff2))[[2]]
  tmp4 <- Dispose_model(data4, coef_names_dispose, coef_dispose)

  tmp <- rbind(tmp1, tmp2, tmp3, tmp4) #
  tmp <- tmp[, by=headpid, .(Ndisp_veh=sum(nextwave_status=="dispose"))] %>% 
    merge(tmp, by="headpid")
  return(tmp)
}

