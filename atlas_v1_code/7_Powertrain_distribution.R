# This code predicts the vehicle count based on the powertrain

num_power <- 4 # number of powertrain for step 7 estimation (5+)

coef7_prepare <- function(coef7){
  cons <- unname(coef7[1:num_power-1])
  coefs_name_aev <- c('const', names(coef7)[seq(from = num_power, to = length(coef7), by = num_power-1)])
  coefs_name_aev <- str_replace_all(coefs_name_aev , ":1", "")
  coefs_aev <- c(cons[1], unname(coef7)[seq(from = num_power, to = length(coef7), by = num_power-1)])
  
  coefs_name_phev <- c('const', names(coef7)[seq(from = num_power+1, to = length(coef7), by = num_power-1)])
  coefs_name_phev <- str_replace_all(coefs_name_phev, ":2", "")
  coefs_phev <- c(cons[2], unname(coef7)[seq(from = num_power+1, to = length(coef7), by = num_power-1)])
  
  coefs_name_hybrid <- c('const', names(coef7)[seq(from = num_power+2, to = length(coef7), by = num_power-1)])
  coefs_name_hybrid <- str_replace_all(coefs_name_hybrid , ":3", "")
  coefs_hybrid <- c(cons[3], unname(coef7)[seq(from = num_power+2, to = length(coef7), by = num_power-1)])
  
  coefs_name_ice <- c('const')
  coefs_ice <- c(0.0)
  
  names <- list(coefs_name_aev, coefs_name_phev, coefs_name_hybrid , coefs_name_ice);
  values <- list(coefs_aev, coefs_phev, coefs_hybrid, coefs_ice)
  coef_power <- list(names, values)
  return(coef_power) # LJ add
}

# 7
utility_power <- function(names, coefs, data) {
  # Extract numeric vectors
  util_power <- data[[names[1]]] * coefs[1]
  
  if (length(names) >= 2) {
    for (i in 2:length(names)) {
      util_power <- util_power + data[[names[i]]] * coefs[i]
    }
  }
  
  # Remove invalid extraction of 'const', assuming it's already in names
  util_power <- as.numeric(util_power)  
  
  return(exp(util_power))
}

generate_vehicles <- function(data_temp) {
  if(sum(data_temp$ncar1)>0) car1 <-  as.data.frame(lapply(data_temp, rep, data_temp$ncar1)) %>% 
      dplyr::mutate(VEHAGE=2.5, car=1, van=0, suv=0, pickup=0,
                    ycar1=1, ycar2=0, ycar3=0, yvan1=0, yvan2=0, yvan3=0,
                    ysuv1=0, ysuv2=0, ysuv3=0, ypickup1=0, ypickup2=0, ypickup3=0) else car1<-NULL

  if(sum(data_temp$ncar2)>0) car2 <-  as.data.frame(lapply(data_temp, rep, data_temp$ncar2)) %>% dplyr::mutate(VEHAGE=8.5, car=1, van=0, suv=0, pickup=0,
                                                                                                               ycar1=0, ycar2=1, ycar3=0, yvan1=0, yvan2=0, yvan3=0,
                                                                                                               ysuv1=0, ysuv2=0, ysuv3=0, ypickup1=0, ypickup2=0, ypickup3=0) else car2<-NULL
  if(sum(data_temp$ncar3)>0) car3 <- as.data.frame(lapply(data_temp, rep, data_temp$ncar3)) %>% dplyr::mutate(VEHAGE=14.5, car=1, van=0, suv=0, pickup=0,
                                                                                                              ycar1=0, ycar2=0, ycar3=1, yvan1=0, yvan2=0, yvan3=0,
                                                                                                              ysuv1=0, ysuv2=0, ysuv3=0, ypickup1=0, ypickup2=0, ypickup3=0) else car3<-NULL
  if(sum(data_temp$nvan1>0)) van1 <- as.data.frame(lapply(data_temp, rep, data_temp$nvan1)) %>% dplyr::mutate(VEHAGE=2.5, car=0, van=1, suv=0, pickup=0,
                                                                                                              ycar1=0, ycar2=0, ycar3=0, yvan1=1, yvan2=0, yvan3=0,
                                                                                                              ysuv1=0, ysuv2=0, ysuv3=0, ypickup1=0, ypickup2=0, ypickup3=0) else van1<-NULL
  if(sum(data_temp$nvan2>0)) van2 <- as.data.frame(lapply(data_temp, rep, data_temp$nvan2)) %>% dplyr::mutate(VEHAGE=8.5, car=0, van=1, suv=0, pickup=0,
                                                                                                              ycar1=0, ycar2=0, ycar3=0, yvan1=0, yvan2=1, yvan3=0,
                                                                                                              ysuv1=0, ysuv2=0, ysuv3=0, ypickup1=0, ypickup2=0, ypickup3=0) else van2<-NULL
  if(sum(data_temp$nvan3>0)) van3 <- as.data.frame(lapply(data_temp, rep, data_temp$nvan3)) %>% dplyr::mutate(VEHAGE=14.5, car=0, van=1, suv=0, pickup=0,
                                                                                                              ycar1=0, ycar2=0, ycar3=0, yvan1=0, yvan2=0, yvan3=1,
                                                                                                              ysuv1=0, ysuv2=0, ysuv3=0, ypickup1=0, ypickup2=0, ypickup3=0) else van3<-NULL
  if(sum(data_temp$nsuv1>0)) suv1 <- as.data.frame(lapply(data_temp, rep, data_temp$nsuv1)) %>% dplyr::mutate(VEHAGE=2.5, car=0, van=0, suv=1, pickup=0,
                                                                                                              ycar1=0, ycar2=0, ycar3=0, yvan1=0, yvan2=0, yvan3=0,
                                                                                                              ysuv1=1, ysuv2=0, ysuv3=0, ypickup1=0, ypickup2=0, ypickup3=0) else suv1<-NULL
  if(sum(data_temp$nsuv2>0)) suv2 <- as.data.frame(lapply(data_temp, rep, data_temp$nsuv2)) %>% dplyr::mutate(VEHAGE=8.5, car=0, van=0, suv=1, pickup=0,
                                                                                                              ycar1=0, ycar2=0, ycar3=0, yvan1=0, yvan2=0, yvan3=0,
                                                                                                              ysuv1=0, ysuv2=1, ysuv3=0, ypickup1=0, ypickup2=0, ypickup3=0) else suv2<-NULL
  if(sum(data_temp$nsuv3>0)) suv3 <- as.data.frame(lapply(data_temp, rep, data_temp$nsuv3)) %>% dplyr::mutate(VEHAGE=14.5, car=0, van=0, suv=1, pickup=0,
                                                                                                              ycar1=0, ycar2=0, ycar3=0, yvan1=0, yvan2=0, yvan3=0,
                                                                                                              ysuv1=0, ysuv2=0, ysuv3=1, ypickup1=0, ypickup2=0, ypickup3=0) else suv3<-NULL
  if(sum(data_temp$npickup1>0)) pickup1 <- as.data.frame(lapply(data_temp, rep, data_temp$npickup1)) %>% dplyr::mutate(VEHAGE=2.5, car=0, van=0, suv=0, pickup=1,
                                                                                                                       ycar1=0, ycar2=0, ycar3=0, yvan1=0, yvan2=0, yvan3=0,
                                                                                                                       ysuv1=0, ysuv2=0, ysuv3=0, ypickup1=1, ypickup2=0, ypickup3=0) else pickup1<-NULL
  if(sum(data_temp$npickup2>0)) pickup2 <- as.data.frame(lapply(data_temp, rep, data_temp$npickup2)) %>% dplyr::mutate(VEHAGE=8.5, car=0, van=0, suv=0, pickup=1,
                                                                                                                       ycar1=0, ycar2=0, ycar3=0, yvan1=0, yvan2=0, yvan3=0,
                                                                                                                       ysuv1=0, ysuv2=0, ysuv3=0, ypickup1=0, ypickup2=1, ypickup3=0) else pickup2<-NULL
  if(sum(data_temp$npickup3>0)) pickup3 <- as.data.frame(lapply(data_temp, rep, data_temp$npickup3)) %>% dplyr::mutate(VEHAGE=14.5, car=0, van=0, suv=0, pickup=1,
                                                                                                                       ycar1=0, ycar2=0, ycar3=0, yvan1=0, yvan2=0, yvan3=0,
                                                                                                                       ysuv1=0, ysuv2=0, ysuv3=0, ypickup1=0, ypickup2=0, ypickup3=1) else pickup3<-NULL
  print('combining vehicles')
  vehicles <- rbind(car1, car2, car3, van1, van2, van3, suv1, suv2, suv3, pickup1, pickup2, pickup3)
  vehicles <- vehicles %>% group_by(HOUSEID) %>% dplyr::mutate(VEHID= 1:n()) %>% ungroup()
  
  return(vehicles);
}

powertrain_model <- function(data, coefs_names, coefs_vehs) {
  
  coefs_name_aev <- c(coefs_names[[1]]);
  coefs_name_phev <- c(coefs_names[[2]]);
  coefs_name_hybrid <- c(coefs_names[[3]]);
  coefs_name_ice <- c(coefs_names[[4]]);
  
  coefs_aev <- c(coefs_vehs[[1]]);
  coefs_phev <- c(coefs_vehs[[2]]);
  coefs_hybrid <- c(coefs_vehs[[3]]);
  coefs_ice <- c(coefs_vehs[[4]]);
  
  ### Utility computation
  util_aev <- utility_power(coefs_name_aev, coefs_aev, data);
  util_phev <- utility_power(coefs_name_phev, coefs_phev, data);
  util_hybrid <- utility_power(coefs_name_hybrid, coefs_hybrid, data);
  util_ice <- utility_power(coefs_name_ice, coefs_ice, data);
  
  
  ### Probability computation
  total_util <- util_aev + util_phev + util_hybrid + util_ice ;
  prob_aev <- (util_aev/total_util);
  prob_phev <- (util_phev/total_util);
  prob_hybrid <- (util_hybrid/total_util);
  prob_ice <- (util_ice/total_util);
  
  
  ### Cumulative probability computation
  cum_prob_aev <- prob_aev
  cum_prob_phev <- cum_prob_aev +  prob_phev
  cum_prob_hybrid <- cum_prob_phev +  prob_hybrid;
  cum_prob_ice <- cum_prob_hybrid +  prob_ice;
  
  
  ### Predicted choices
  Numvehicle <- nrow(data);
  rnum <- runif(Numvehicle);
  rnum[rnum < cum_prob_aev] <- 2;
  rnum[rnum < cum_prob_phev] <- 3;
  rnum[rnum < cum_prob_hybrid] <- 4;
  rnum[rnum <= 1] <- 5;
  data$pred_power <- rnum-1
  
  data$pred_power[data$pred_power==1] <- "AEV"
  data$pred_power[data$pred_power==2] <- "PHEV"
  data$pred_power[data$pred_power==3] <- "Hybrid"
  data$pred_power[data$pred_power==4] <- "ICE"
  
  tmp <- data[c("pred_power")];
  return(tmp);
}
