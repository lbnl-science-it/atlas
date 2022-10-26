# step2: this script process the vehicle data set and apply the addtion model to the dataset
# v2: decrease two groups: decrease1 and decrease2

# 2.1: data cleaning
addition_clean <- function(vehdata, housedata){
  if (initialyear==T){
    housedata <- housedata[,yrs_inFu_nextwave:=NA]
  }
  
  vehdata <- vehdata[, keyby=.(year, headpid), .(
    N_car = sum(vehtype == 'car', na.rm =T),
    N_suv = sum(vehtype == 'suv', na.rm =T),
    N_pickup = sum(vehtype == 'pickup', na.rm =T),
    N_van = sum(vehtype == 'van', na.rm =T),
    minyrs_withfu = fcase (  # how many years since ever acquired a vehicle
      sum(!is.na(yrs_inFu_nextwave))>0, min(yrs_inFu_nextwave, na.rm =T),
      default =  NA_real_))][, has_carTRUE:= !is.na(N_car) & N_car>0][
        ,has_suvTRUE:= !is.na(N_suv) & N_suv>0][
          ,has_pickupTRUE:= !is.na(N_pickup) & N_pickup>0][
            ,has_vanTRUE:= !is.na(N_van) & N_van >0]
  
  housedata <- merge(housedata, vehdata, by=c("year", "headpid"), all.x=T)[
    , carown_bins_veh_sufficient:=as.numeric(nvehicles>0&nvehicles>=N_driving)][
      , N_car:= coalesce(N_car, 0)][, N_suv:= coalesce(N_suv, 0)][,N_pickup:= coalesce(N_pickup, 0)][
        , N_van:= coalesce(N_van, 0)][,has_suvTRUE:= coalesce(has_suvTRUE, 0)][
          , has_pickupTRUE:= coalesce(has_pickupTRUE, 0)][,has_vanTRUE:= coalesce(has_vanTRUE, 0)][
            , has_carTRUE:= coalesce(has_carTRUE, 0)][,const:= 1][,hd_raceblack:= (hd_race=="black")][
              , hd_raceother:= (hd_race=="other")][,hd_racewhite:=(hd_race=="white")][,ncar_thiswave:= nvehicles]

  
  housedata <- setnames(housedata, c("hh_edu_hi", "hh_edu_>clg", "hh_edu_clg", "hh_edu_some clg"), 
                        c("hh_eduhi", "hh_edu>clg", "hh_educlg", "hh_edusome clg"))
}

# 2.2： prediction preprocess
addition_veh0 <- function(const, expect_op) {
  p1 <- 1/(1+exp(expect_op));
  p2 <- const;
  rnum <- runif(length(const));
  rnum[rnum < p1] <- "no addition";
  rnum[rnum < (p1 + 2*(1-sqrt(p1))*sqrt(p1))] <- "addition 1";
  rnum[rnum!="no addition"&rnum!="addition 1"] <- "addition 2";
  rnum[is.na(p1)==T] <- NA
  return (rnum);
}

addition_veh <- function(const, expect_op, thresh) {
  p1 <- 1/(1+exp(expect_op));
  p2 <- 1/(1+exp(thresh));
  p3 <- const;
  rnum <- runif(length(const));
  # v2: added
  rnum[rnum < (1-sqrt(1-p1))^2] <- "decrease 2"
  rnum[rnum < p1] <- "decrease 1"
  rnum[rnum < p2] <- "no addition";
  rnum[rnum < (p2 + 2*(1-sqrt(p2))*sqrt(p2))] <- "addition 1";
  rnum[rnum!="decrease 2"&rnum!="decrease 1"&rnum!="no addition"&rnum!="addition 1"] <- "addition 2";
  rnum[is.na(p1)==T] <- NA
  return (rnum);
}

# 2.3： coefficients process + prediction
addition <- function(data){
  # numcar <- c("carlessFU", "oneCarFU", "moreCarFU")
  addition_tmp <- data.table(matrix(ncol = (ncol(data)+1), nrow = 0))
  names(addition_tmp) <- c(names(data), "addition")
  for (i in c(TRUE, FALSE)){
    for (j in 1:3){
      if (i==FALSE){
        coef_names <- c(names(coef(nvehchange[[j]])), names(get(paste0("geocoeff_add", j))))
        coef_names[1] <- "const"
        coef_values <- c(unname(coef(nvehchange[[j]])), unname(get(paste0("geocoeff_add", j))))
      }else{
        coef_names <- c(names(coef(nvehchange2[[j]])), names(get(paste0("geocoeff_add", j))))
        coef_names[1] <- "const"
        coef_values <- c(unname(coef(nvehchange2[[j]])), unname(get(paste0("geocoeff_add", j))))
      }
      if (j<=2){
        tmpdata <- data[is.na(minyrs_withfu)==i & nvehicles==(j-1)]
      }else{
        tmpdata <- data[is.na(minyrs_withfu)==i & nvehicles>=(j-1)]
      }
      
      if (j==1){
        tmpdata <- tmpdata[, const:=-1]
        expect_op <- as.matrix(setcolorder(tmpdata, coef_names)[, ..coef_names]) %*% coef_values;
        tmpdata <- tmpdata[, const:=1]
        tmpdata$addition <- addition_veh0(tmpdata$const, expect_op)
      }else{
        tmpdata <- tmpdata[, const:=-1]
        tmpdata <- tmpdata[, `no_change|increase`:=-1]
        thresh  <- as.matrix(setcolorder(tmpdata, coef_names)[, ..coef_names]) %*% coef_values -
          as.matrix(tmpdata[,const]) %*% coef_values[1];
        expect_op <- thresh + as.matrix(tmpdata[,const]) %*% coef_values[1]- 
          as.matrix(tmpdata[,`no_change|increase`]) %*% coef_values[2]
        tmpdata <- tmpdata[, const:=1][,!"no_change|increase"]
        tmpdata$addition <- addition_veh(tmpdata$const, expect_op, thresh)
      }
      addition_tmp <- rbind(addition_tmp, tmpdata)
    }
  }
  addition_tmp <- addition_tmp[ncar_thiswave>=3 & addition=="addition 2", addition:="addition 1"][
    ncar_thiswave<=2 & addition=="decrease 2", addition:="decrease 1"][
                                                 , Ndisp_hh:=fcase(addition=="decrease 2", 2,
                                                                   addition=="decrease 1", 1,
                                                                   default=0)]
  return(addition_tmp)
}

