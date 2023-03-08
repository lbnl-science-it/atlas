#####################################################
###Codes for running application related functions###
#####################################################

## Process coefs
# 2
coef2_prepare <- function(coef2){
  cons <- unname(coef2[1:num_alt-1])
  coefs_name_0veh <- c('const', names(coef2)[seq(from = num_alt, to = length(coef2), by = num_alt-1)])
  coefs_name_0veh <- str_replace_all(coefs_name_0veh, ":1", "")
  coefs_0veh <- c(cons[1], unname(coef2)[seq(from = num_alt, to = length(coef2), by = num_alt-1)])
  
  coefs_name_1veh <- c('const', names(coef2)[seq(from = num_alt+1, to = length(coef2), by = num_alt-1)])
  coefs_name_1veh <- str_replace_all(coefs_name_0veh, ":2", "")
  coefs_1veh <- c(cons[2], unname(coef2)[seq(from = num_alt+1, to = length(coef2), by = num_alt-1)])
  
  coefs_name_2veh <- c('const', names(coef2)[seq(from = num_alt+2, to = length(coef2), by = num_alt-1)])
  coefs_name_2veh <- str_replace_all(coefs_name_0veh, ":3", "")
  coefs_2veh <- c(cons[3], unname(coef2)[seq(from = num_alt+2, to = length(coef2), by = num_alt-1)])
  
  coefs_name_3veh <- c('const', names(coef2)[seq(from = num_alt+3, to = length(coef2), by = num_alt-1)])
  coefs_name_3veh <- str_replace_all(coefs_name_0veh, ":4", "")
  coefs_3veh <- c(cons[4], unname(coef2)[seq(from = num_alt+3, to = length(coef2), by = num_alt-1)])
  
  coefs_name_4veh <- c('const', names(coef2)[seq(from = num_alt+4, to = length(coef2), by = num_alt-1)])
  coefs_name_4veh <- str_replace_all(coefs_name_0veh, ":5", "")
  coefs_4veh <- c(cons[5], unname(coef2)[seq(from = num_alt+4, to = length(coef2), by = num_alt-1)])
  
  coefs_name_5veh <- c('const')
  coefs_5veh <- c(0.0)
  
  names <- list(coefs_name_0veh, coefs_name_1veh, coefs_name_2veh , coefs_name_3veh, coefs_name_4veh, coefs_name_5veh);
  values <- list(coefs_0veh, coefs_1veh, coefs_2veh, coefs_3veh, coefs_4veh, coefs_5veh)
  coef_veh <- list(names, values)
  return(coef_veh);
}

# 5
coef5_prepare <- function(coef5){
  cons <- unname(coef5[1:num_type-1])
  coefs_name_0type <- c('const', names(coef5)[seq(from = num_type, to = length(coef5), by = num_type-1)])
  coefs_name_0type <- str_replace_all(coefs_name_0type, ":1", "")
  coefs_0type <- c(cons[1], unname(coef5)[seq(from = num_type, to = length(coef5), by = num_type-1)])
  coefs_name_1type <- c('const', names(coef5)[seq(from = num_type+1, to = length(coef5), by = num_type-1)])
  coefs_name_1type <- str_replace_all(coefs_name_1type, ":2", "")
  coefs_1type <- c(cons[2], unname(coef5)[seq(from = num_type+1, to = length(coef5), by = num_type-1)])
  coefs_name_2type <- c('const', names(coef5)[seq(from = num_type+2, to = length(coef5), by = num_type-1)])
  coefs_name_2type <- str_replace_all(coefs_name_2type, ":3", "")
  coefs_2type <- c(cons[3], unname(coef5)[seq(from = num_type+2, to = length(coef5), by = num_type-1)])
  coefs_name_3type <- c('const', names(coef5)[seq(from = num_type+3, to = length(coef5), by = num_type-1)])
  coefs_name_3type <- str_replace_all(coefs_name_3type, ":4", "")
  coefs_3type <- c(cons[4], unname(coef5)[seq(from = num_type+3, to = length(coef5), by = num_type-1)])
  coefs_name_4type <- c('const');
  coefs_4type <- c(0.0);
  
  names <- list(coefs_name_0type, coefs_name_1type, coefs_name_2type , coefs_name_3type, coefs_name_4type);
  values <- list(coefs_0type, coefs_1type, coefs_2type, coefs_3type, coefs_4type);
  coef_type <- list(names, values)
  return(coef_type) # LJ add
  
}
# 7
# coef7_prepare <- function(coef7){
#   cons <- unname(coef7[1:num_power-1])
#   coefs_name_hybrid <- c('const', names(coef7)[seq(from = num_power, to = length(coef7), by = num_power-1)])
#   coefs_name_hybrid <- str_replace_all(coefs_name_hybrid , ":2", "")
#   coefs_hybrid <- c(cons[1], unname(coef7)[seq(from = num_power, to = length(coef7), by = num_power-1)])
# 
#   coefs_name_ice <- c('const')
#   coefs_ice <- c(0.0)
# 
#   names <- list(coefs_name_hybrid , coefs_name_ice);
#   values <- list(coefs_hybrid, coefs_ice)
#   coef_power <- list(names, values)
# }

coef7_prepare <- function(coef7){
  cons <- unname(coef7[1:num_power-1])
  coefs_name_bev <- c('const', names(coef7)[seq(from = num_power, to = length(coef7), by = num_power-1)])
  coefs_name_bev <- str_replace_all(coefs_name_bev , ":1", "")
  coefs_bev <- c(cons[1], unname(coef7)[seq(from = num_power, to = length(coef7), by = num_power-1)])
  
  coefs_name_phev <- c('const', names(coef7)[seq(from = num_power+1, to = length(coef7), by = num_power-1)])
  coefs_name_phev <- str_replace_all(coefs_name_phev, ":2", "")
  coefs_phev <- c(cons[2], unname(coef7)[seq(from = num_power+1, to = length(coef7), by = num_power-1)])
  
  coefs_name_hybrid <- c('const', names(coef7)[seq(from = num_power+2, to = length(coef7), by = num_power-1)])
  coefs_name_hybrid <- str_replace_all(coefs_name_hybrid , ":3", "")
  coefs_hybrid <- c(cons[3], unname(coef7)[seq(from = num_power+2, to = length(coef7), by = num_power-1)])
  
  coefs_name_ice <- c('const')
  coefs_ice <- c(0.0)
  
  names <- list(coefs_name_bev, coefs_name_phev, coefs_name_hybrid , coefs_name_ice);
  values <- list(coefs_bev, coefs_phev, coefs_hybrid, coefs_ice)
  coef_power <- list(names, values)
  return(coef_power) # LJ add
  
}


# Model application inside mini functions
# 2
utility <- function(names, coefs, data1) {
  util_veh <- data1[names[1]] * coefs[1];
  if (length(names) >= 2) {
    for (i in 2:(length(names))) {
      util_veh <- util_veh + data1[names[i]] * coefs[i];
    }
  }
  util_veh <- as.numeric(util_veh$const);
  return (exp(util_veh));
}


# 6.1
count_car <- function(const, expect_op, thresh, ncar) {
  
  p1 <- 1/(1+exp(expect_op));
  p2 <- 1/(1+exp(thresh));
  p3 <- const;
  
  isCAR <- ncar;
  isCAR[isCAR > 0] <- 1;
  
  
  rnum <- runif(length(const));
  
  
  rnum[rnum < p1] <- 1;
  rnum[rnum < p2] <- 2;
  rnum[rnum < 1] <- 3;
  rnum <- rnum*isCAR;
  
  return (rnum);
}

# 6.2
count_veh <- function(const, expect_op, nveh) {
  
  p1 <- 1/(1+exp(expect_op));
  p2 <- const;
  
  isVEH <- nveh;
  isVEH[isVEH > 0] <- 1;
  
  
  rnum <- runif(length(const));
  
  
  rnum[rnum < p1] <- 1;
  rnum[rnum < 1] <- 2;
  rnum <- rnum*isVEH;
  
  return (rnum);
}

# 7
utility_power <- function(names, coefs, data) {
  
  
  
  util_power <- data[names[1]] * coefs[1];
  if (length(names) >= 2) {
    for (i in 2:(length(names))) {
      
      util_power <- util_power + data[names[i]] * coefs[i];
      
    }
  }
  util_power <- as.numeric(util_power$const);
  return (exp(util_power));
}



# Model application main function
model_application <- function(persons, data1, coefs_name_mile, coefs_mile , coefs_names, coefs_vehs, 
                              coef_names_type, coef_values_type, coef_names_car, coefs_car,
                              coef_names_van, coefs_van, coef_names_suv, coefs_suv, coef_names_pick, coefs_pick,
                              coef_names_power, coef_values_power, loopi){
  require(tidyr)
  require(dplyr)
  require(apollo)
  require(stats)
  
  # 1. Mileage prediction
  
  mileage_model <- function(data, coefs_name_mile, coefs_mile) {
    
    data$const <- 1;
    # #how to decide the sd
    # set.seed(1234) # change LJ 9/27/2021 do not set the same seed for all parallel processes.
    error_term <- rnorm(length(data$const), mean=0, sd=3.75); # LJ change 9/14/2021 changed from 6 to 4.
    expect_op <- data[coefs_name_mile[1]] * coefs_mile[1];
    for (i in 2:(length(coefs_name_mile))) {
      expect_op <- expect_op + data[coefs_name_mile[i]] * coefs_mile[i];
    }
    expect_op <- as.numeric(expect_op$const);
    expect_op <- (expect_op + error_term);
    expect_op[expect_op < 0.0] <- 0
    
    # log transformation: this needs to be changed
    expect_op <- expect_op^(1/0.3);
    
    isVehicle <- data$cars;
    isVehicle[isVehicle > 0] <- 1;
    data$budget <- (expect_op * isVehicle) + data$HHSIZE * 0.5 * 365;
    
    tmp <- data[c("const","budget")];
    return (tmp);
  } 
  tt1 <- mileage_model(data1, coefs_name_mile, coefs_mile)
  data_temp <- cbind(data1,tt1)
  rm(tt1)
  
  
  # 2. Number of alternatives
  NumAlternative_model <- function(data, coefs_names, coefs_vehs) {
    
    coefs_name_0veh <- c(coefs_names[[1]]);
    coefs_name_1veh <- c(coefs_names[[2]]);
    coefs_name_2veh <- c(coefs_names[[3]]);
    coefs_name_3veh <- c(coefs_names[[4]]);
    coefs_name_4veh <- c(coefs_names[[5]]);
    coefs_name_5veh <- c(coefs_names[[6]]);
    
    coefs_0veh <- c(coefs_vehs[[1]]);
    coefs_1veh <- c(coefs_vehs[[2]]);
    coefs_2veh <- c(coefs_vehs[[3]]);
    coefs_3veh <- c(coefs_vehs[[4]]);
    coefs_4veh <- c(coefs_vehs[[5]]);
    coefs_5veh <- c(coefs_vehs[[6]]);
    
    num_alt <- 6;
    
    
    ### Utility computation 
    util_one_veh <- utility(coefs_name_0veh, coefs_0veh, data);
    util_two_veh <- utility(coefs_name_1veh, coefs_1veh, data);
    util_three_veh <- utility(coefs_name_2veh, coefs_2veh, data);
    util_four_veh <- utility(coefs_name_3veh, coefs_3veh, data);
    util_five_veh <- utility(coefs_name_4veh, coefs_4veh, data);
    util_six_veh <- utility(coefs_name_5veh, coefs_5veh, data);
    
    
    ### Probability computation
    total_util <- util_one_veh + util_two_veh + util_three_veh + util_four_veh + util_five_veh + util_six_veh ;
    prob_one_veh <- (util_one_veh/total_util);
    prob_two_veh <- (util_two_veh/total_util);
    prob_three_veh <- (util_three_veh/total_util);
    prob_four_veh <- (util_four_veh/total_util);
    prob_five_veh <- (util_five_veh/total_util);
    prob_six_veh <- (util_six_veh/total_util);
    
    
    ### Cumulative probability computation
    cum_prob_one_veh <- prob_one_veh;
    cum_prob_two_veh <- cum_prob_one_veh +  prob_two_veh;
    cum_prob_three_veh <- cum_prob_two_veh +  prob_three_veh;
    cum_prob_four_veh <- cum_prob_three_veh +  prob_four_veh;
    cum_prob_five_veh <- cum_prob_four_veh +  prob_five_veh;
    cum_prob_six_veh <- cum_prob_five_veh +  prob_six_veh;
    
    
    ### Predicted choices
    NumHousehold <- nrow(data);
    rnum <- runif(NumHousehold);
    rnum[rnum < cum_prob_one_veh] <- 2;
    rnum[rnum < cum_prob_two_veh] <- 3;
    rnum[rnum < cum_prob_three_veh] <- 4;
    rnum[rnum < cum_prob_four_veh] <- 5;
    rnum[rnum < cum_prob_five_veh] <- 6;
    rnum[rnum <= 1] <- 7;
    data$pred_own <- rnum - 2;
    
    tmp <- data[c("pred_own")];
    return(tmp);
  }
  tt1 <- NumAlternative_model(data_temp, coef_names_veh, coef_values_veh);
  data_temp <- cbind(data_temp,tt1);
  rm(tt1);
  
  # 3. MDECV
  VehicleFleet_model <- function(data1){ # LJ change: change data to data1 
    
    database1 <- data_temp %>% mutate(outside=0, car1=0, car2=0, car3=0, van1=0, van2=0, van3=0, suv1=0, suv2=0, suv3=0,
                                      pickup1=0, pickup2=0, pickup3=0, motorbike=0) # LJ change to a local variable
    
    ###### Apollo function
    ### Initialise code
    apollo_initialise()
    
    ### Set core controls # LJ change, move to the main function
    
    # ################################################################# #
    #### DEFINE MODEL PARAMETERS                                     ####
    # ################################################################# #
    
    
    
    # ################################################################# #
    #### GROUP AND VALIDATE INPUTS                                   ####
    # ################################################################# #
    
    apollo_inputs <- apollo_validateInputs(database = database1) # LJ change to <- from <<-; and specify database = database1
    
    # ################################################################# #
    #### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
    # ################################################################# #
    
    apollo_probabilities<-function(apollo_beta = apollo_beta, apollo_inputs = apollo_inputs, functionality="estimate"){ # LJ change to explicit argument assignment
      
      ### Attach inputs and detach after function exit 
      apollo_attach(apollo_beta, apollo_inputs)
      on.exit(apollo_detach(apollo_beta, apollo_inputs))
      
      ### Create list of probabilities P
      P = list()
      
      ### Define individual alternatives
      alternatives  = c("outside", "car1", "car2", "car3",
                        "van1", "van2", "van3",
                        "suv1", "suv2", "suv3",
                        "pickup1", "pickup2", "pickup3",
                        "motorbike")
      
      ### Define availabilities
      avail = list(outside  = 1, car1=1, car2=1, car3=1, 
                   van1=1, van2=1, van3=1,  
                   suv1=1, suv2=1, suv3=1,  
                   pickup1=1, pickup2=1, pickup3=1,  
                   motorbike=1)
      
      ### Define continuous consumption for individual alternatives
      continuousChoice = list(outside=outside, car1=car1, car2=car2, car3=car3, 
                              van1=van1, van2=van2, van3=van3,  
                              suv1=suv1, suv2=suv2, suv3=suv3,  
                              pickup1=pickup1, pickup2=pickup2, pickup3=pickup3,  
                              motorbike=motorbike)
      
      ### Define utilities for individual alternatives
      V = list()
      V[["outside"]]  = 0
      
      # 2017 v4
      # V[["car1"]]     = delta_car1  + delta_car1_income1*income1 + delta_car1_income4*income4 + delta_car1_NUMCHILD*NUMCHILD #+ delta_car1_sonoma*sonoma + delta_car1_marin*marin + delta_car1_napa*napa + delta_car1_sf*sf #+ delta_car1_income1*income1 + delta_car1_income4*income4 + delta_car1_NUMCHILD*NUMCHILD
      # V[["car2"]]   = delta_car2 + delta_car2_work2*work2 #+ delta_car2_sc*sc#   # + delta_car2_solano*solano + delta_car2_sonoma*sonoma + delta_car2_cc*cc + delta_car2_sf*sf + delta_car2_sf*sf#+ delta_car2_income2*income2 
      # V[["car3"]] = delta_car3  + delta_car3_income1*income1 + delta_car3_retired*retired #+ delta_car3_sf*sf # #+ delta_car3_cc*cc + delta_car3_napa*napa + delta_car3_income1*income1 + delta_car3_retired*retired
      # V[["van1"]]     = delta_van1 + delta_van1_NUMCHILD*NUMCHILD #+ delta_van1_marin*marin + delta_van1_napa*napa + delta_van1_solano*solano + delta_van1_sonoma*sonoma #delta_van1_sf*sf #+ delta_van1_cc*cc #+ delta_van1_NUMCHILD*NUMCHILD
      # V[["van2"]]   = delta_van2 + delta_van2_NUMCHILD*NUMCHILD #+ delta_van2_cc*cc + delta_van2_marin*marin + delta_van2_napa*napa #+ delta_van2_cc*cc #+ delta_van2_NUMCHILD*NUMCHILD 
      # V[["van3"]] = delta_van3 #+ delta_van3_marin*marin #delta_van3_sf*sf #+ delta_van3_cc*cc #+ delta_van3_income1*income1 + delta_van3_HHSIZE1*HHSIZE1
      # V[["suv1"]]     = delta_suv1 +  delta_suv1_sf*sf #+ delta_suv1_sm*sm #+ delta_suv1_income1*income1 + delta_suv1_work2*work2 #+ delta_suv1_sonoma*sonoma + delta_suv1_napa*napa + delta_suv1_cc*cc #+ delta_suv1_income1*income1 + delta_suv1_work2*work2
      # V[["suv2"]]   = delta_suv2 # +  delta_suv2_sf*sf #+ delta_suv2_HHSIZE4*HHSIZE4 #+ delta_suv2_sonoma*sonoma + delta_suv2_cc*cc + delta_suv2_marin*marin + delta_suv2_income3*income3 + delta_suv2_HHSIZE4*HHSIZE4 
      # V[["suv3"]] = delta_suv3 #+ delta_suv3_income4*income4#+ delta_suv3_cc*cc + delta_suv3_marin*marin +  delta_suv3_sf*sf + delta_suv3_sc*sc + delta_suv3_solano*solano #+ delta_suv3_income4*income4 + delta_suv3_child *child 
      # V[["pickup1"]]     = delta_pickup1 # + delta_pickup1_sm*sm + delta_pickup1_sonoma*sonoma # + delta_pickup1_marin*marin + delta_pickup1_solano*solano #delta_pickup1_sf*sf #+ delta_pickup1_cc*cc #+ delta_pickup1_HHSIZE1*HHSIZE1 + delta_pickup1_income5*income5 
      # V[["pickup2"]]   = delta_pickup2  + delta_pickup2_solano*solano + delta_pickup2_sonoma*sonoma #+ delta_pickup2_child*child #+ delta_pickup2_marin*marin + delta_pickup2_napa*napa delta_pickup2_sf*sf #+ delta_pickup2_cc*cc #+ delta_pickup2_child*child + delta_pickup2_income1*income1
      # V[["pickup3"]] = delta_pickup3 + delta_pickup3_solano*solano + delta_pickup3_sonoma*sonoma #+ delta_pickup3_income2*income2 + delta_pickup3_child*child # + delta_pickup3_napa*napa + delta_pickup3_sm*sm delta_pickup3_sf*sf #+ delta_pickup3_cc*cc #+ delta_pickup3_income2*income2 + delta_pickup3_child*child
      # V[["motorbike"]] = delta_motorbike #+ delta_motorbike_cc*cc #+ delta_motorbike_sf*sf + delta_motorbike_HHSIZE1*HHSIZE1
      
      # V[["car1"]]     = delta_car1 #+ delta_car1_income1*income1 + delta_car1_income4*income4 + delta_car1_NUMCHILD*NUMCHILD
      # V[["car2"]]   = delta_car2 #+ delta_car2_income2*income2 + delta_car2_work2*work2
      # V[["car3"]] = delta_car3 #+ delta_car3_income1*income1 + delta_car3_retired*retired
      # V[["van1"]]     = delta_van1 #+ delta_van1_NUMCHILD*NUMCHILD
      # V[["van2"]]   = delta_van2 #+ delta_van2_NUMCHILD*NUMCHILD
      # V[["van3"]] = delta_van3 #+ delta_van3_income1*income1 + delta_van3_HHSIZE1*HHSIZE1
      # V[["suv1"]]     = delta_suv1 #+ delta_suv1_income1*income1 + delta_suv1_work2*work2
      # V[["suv2"]]   = delta_suv2 #+ delta_suv2_income3*income3 + delta_suv2_HHSIZE4*HHSIZE4
      # V[["suv3"]] = delta_suv3 #+ delta_suv3_income4*income4 + delta_suv3_child *child
      # V[["pickup1"]]     = delta_pickup1 #+ delta_pickup1_HHSIZE1*HHSIZE1 + delta_pickup1_income5*income5
      # V[["pickup2"]]   = delta_pickup2 #+ delta_pickup2_child*child + delta_pickup2_income1*income1
      # V[["pickup3"]] = delta_pickup3 #+ delta_pickup3_income2*income2 + delta_pickup3_child*child
      # V[["motorbike"]] = delta_motorbike #+ delta_motorbike_HHSIZE1*HHSIZE1
      
      # V[["car1"]]     = delta_car1 #+ delta_car1_sf*sf #+ delta_car1_income1*income1 + delta_car1_income4*income4 + delta_car1_NUMCHILD*NUMCHILD
      # V[["car2"]]   = delta_car2 #+ delta_car2_sf*sf#+ delta_car2_income2*income2 + delta_car2_work2*work2
      # V[["car3"]] = delta_car3 #+ delta_car3_sf*sf #+ delta_car3_income1*income1 + delta_car3_retired*retired
      # V[["van1"]]     = delta_van1 + delta_van1_sf*sf #+ delta_van1_NUMCHILD*NUMCHILD
      # V[["van2"]]   = delta_van2 + delta_van2_sf*sf #+ delta_van2_NUMCHILD*NUMCHILD 
      # V[["van3"]] = delta_van3 + delta_van3_sf*sf #+ delta_van3_income1*income1 + delta_van3_HHSIZE1*HHSIZE1
      # V[["suv1"]]     = delta_suv1 + delta_suv1_sf*sf #+ delta_suv1_income1*income1 + delta_suv1_work2*work2
      # V[["suv2"]]   = delta_suv2 + delta_suv2_sf*sf #+ delta_suv2_income3*income3 + delta_suv2_HHSIZE4*HHSIZE4 
      # V[["suv3"]] = delta_suv3 + delta_suv3_sf*sf #+ delta_suv3_income4*income4 + delta_suv3_child *child 
      # V[["pickup1"]]     = delta_pickup1 + delta_pickup1_sf*sf #+ delta_pickup1_HHSIZE1*HHSIZE1 + delta_pickup1_income5*income5 
      # V[["pickup2"]]   = delta_pickup2 + delta_pickup2_sf*sf #+ delta_pickup2_child*child + delta_pickup2_income1*income1
      # V[["pickup3"]] = delta_pickup3 + delta_pickup3_sf*sf #+ delta_pickup3_income2*income2 + delta_pickup3_child*child
      # V[["motorbike"]] = delta_motorbike #+ delta_motorbike_sf*sf + delta_motorbike_HHSIZE1*HHSIZE1
      
      # 2009 v3
      V[["car1"]]     = delta_car1  + delta_car1_income1*income1 + delta_car1_income4*income4 + delta_car1_NUMCHILD*NUMCHILD #+ delta_car1_sonoma*sonoma + delta_car1_marin*marin + delta_car1_napa*napa + delta_car1_sf*sf #+ delta_car1_income1*income1 + delta_car1_income4*income4 + delta_car1_NUMCHILD*NUMCHILD
      V[["car2"]]   = delta_car2  + delta_car2_sc*sc + delta_car2_work2*work2  # + delta_car2_solano*solano + delta_car2_sonoma*sonoma + delta_car2_cc*cc + delta_car2_sf*sf + delta_car2_sf*sf#+ delta_car2_income2*income2
      V[["car3"]] = delta_car3  + delta_car3_sf*sf + delta_car3_income1*income1 + delta_car3_retired*retired #+ delta_car3_cc*cc + delta_car3_napa*napa + delta_car3_income1*income1 + delta_car3_retired*retired
      V[["van1"]]     = delta_van1 + delta_van1_NUMCHILD*NUMCHILD #+ delta_van1_marin*marin + delta_van1_napa*napa + delta_van1_solano*solano + delta_van1_sonoma*sonoma #delta_van1_sf*sf #+ delta_van1_cc*cc #+ delta_van1_NUMCHILD*NUMCHILD
      V[["van2"]]   = delta_van2 + delta_van2_NUMCHILD*NUMCHILD #+ delta_van2_cc*cc + delta_van2_marin*marin + delta_van2_napa*napa #+ delta_van2_cc*cc #+ delta_van2_NUMCHILD*NUMCHILD
      V[["van3"]] = delta_van3 #+ delta_van3_marin*marin #delta_van3_sf*sf #+ delta_van3_cc*cc #+ delta_van3_income1*income1 + delta_van3_HHSIZE1*HHSIZE1
      V[["suv1"]]     = delta_suv1 +  delta_suv1_sf*sf + delta_suv1_sm*sm + delta_suv1_income1*income1 + delta_suv1_work2*work2 #+ delta_suv1_sonoma*sonoma + delta_suv1_napa*napa + delta_suv1_cc*cc #+ delta_suv1_income1*income1 + delta_suv1_work2*work2
      V[["suv2"]]   = delta_suv2  +  delta_suv2_sf*sf + delta_suv2_HHSIZE4*HHSIZE4 #+ delta_suv2_sonoma*sonoma + delta_suv2_cc*cc + delta_suv2_marin*marin + delta_suv2_income3*income3 + delta_suv2_HHSIZE4*HHSIZE4
      V[["suv3"]] = delta_suv3 + delta_suv3_income4*income4#+ delta_suv3_cc*cc + delta_suv3_marin*marin +  delta_suv3_sf*sf + delta_suv3_sc*sc + delta_suv3_solano*solano #+ delta_suv3_income4*income4 + delta_suv3_child *child
      V[["pickup1"]]     = delta_pickup1 + delta_pickup1_sm*sm + delta_pickup1_sonoma*sonoma # + delta_pickup1_marin*marin + delta_pickup1_solano*solano #delta_pickup1_sf*sf #+ delta_pickup1_cc*cc #+ delta_pickup1_HHSIZE1*HHSIZE1 + delta_pickup1_income5*income5
      V[["pickup2"]]   = delta_pickup2  + delta_pickup2_solano*solano + delta_pickup2_sonoma*sonoma + delta_pickup2_child*child #+ delta_pickup2_marin*marin + delta_pickup2_napa*napa delta_pickup2_sf*sf #+ delta_pickup2_cc*cc #+ delta_pickup2_child*child + delta_pickup2_income1*income1
      V[["pickup3"]] = delta_pickup3 + delta_pickup3_solano*solano + delta_pickup3_sonoma*sonoma + delta_pickup3_income2*income2 + delta_pickup3_child*child # + delta_pickup3_napa*napa + delta_pickup3_sm*sm delta_pickup3_sf*sf #+ delta_pickup3_cc*cc #+ delta_pickup3_income2*income2 + delta_pickup3_child*child
      V[["motorbike"]] = delta_motorbike #+ delta_motorbike_cc*cc #+ delta_motorbike_sf*sf + delta_motorbike_HHSIZE1*HHSIZE1
      
      # Without location fixed effects
      # V[["car1"]]     = delta_car1 + delta_car1_income1*income1 + delta_car1_income4*income4 + delta_car1_NUMCHILD*NUMCHILD
      # V[["car2"]]   = delta_car2 + delta_car2_income2*income2 + delta_car2_work2*work2
      # V[["car3"]] = delta_car3 + delta_car3_income1*income1 + delta_car3_retired*retired
      # V[["van1"]]     = delta_van1 + delta_van1_NUMCHILD*NUMCHILD
      # V[["van2"]]   = delta_van2 + delta_van2_NUMCHILD*NUMCHILD 
      # V[["van3"]] = delta_van3 + delta_van3_income1*income1 + delta_van3_HHSIZE1*HHSIZE1
      # V[["suv1"]]     = delta_suv1 + delta_suv1_income1*income1 + delta_suv1_work2*work2
      # V[["suv2"]]   = delta_suv2 + delta_suv2_income3*income3 + delta_suv2_HHSIZE4*HHSIZE4 
      # V[["suv3"]] = delta_suv3 + delta_suv3_income4*income4 + delta_suv3_child *child 
      # V[["pickup1"]]     = delta_pickup1 + delta_pickup1_HHSIZE1*HHSIZE1 + delta_pickup1_income5*income5 
      # V[["pickup2"]]   = delta_pickup2 + delta_pickup2_child*child + delta_pickup2_income1*income1
      # V[["pickup3"]] = delta_pickup3 + delta_pickup3_income2*income2 + delta_pickup3_child*child
      # V[["motorbike"]] = delta_motorbike + delta_motorbike_HHSIZE1*HHSIZE1
      
      
      ### Define alpha parameters
      alpha = list(
        outside=alpha_base,
        car1=alpha_base,
        car2=alpha_base,
        car3=alpha_base,
        van1=alpha_base,
        van2=alpha_base,
        van3=alpha_base,
        suv1=alpha_base,
        suv2=alpha_base,
        suv3=alpha_base,
        pickup1=alpha_base,
        pickup2=alpha_base,
        pickup3=alpha_base,
        motorbike=alpha_base)
      
      ### Define gamma parameters
      gamma = list( car1=gamma_car1,
                    car2=gamma_car2,
                    car3=gamma_car3,
                    van1=gamma_van1,
                    van2=gamma_van2,
                    van3=gamma_van3,
                    suv1=gamma_suv1,
                    suv2=gamma_suv2,
                    suv3=gamma_suv3,
                    pickup1=gamma_pickup1,
                    pickup2=gamma_pickup2,
                    pickup3=gamma_pickup3,
                    motorbike=gamma_motorbike)
      
      ### Define costs for individual alternatives
      cost = list(outside  = 1,
                  car1=1,
                  car2=1,
                  car3=1,
                  van1=1,
                  van2=1,
                  van3=1,
                  suv1=1,
                  suv2=1,
                  suv3=1,
                  pickup1=1,
                  pickup2=1,
                  pickup3=1,
                  motorbike=1)
      
      ### Define budget
      budget = budget
      
      ### Define settings for MDCEV model
      mdcev_settings <- list(alternatives      = alternatives,
                             avail             = avail,
                             continuousChoice = continuousChoice,
                             V                 = V,
                             alpha             = alpha,
                             gamma             = gamma, 
                             sigma             = sigma, 
                             cost              = cost,
                             budget            = budget)
      
      ### Compute probabilities using MDCEV model
      P[["model"]] = apollo_mdcev(mdcev_settings, functionality)
      
      ### Take product across observation for same individual
      # P = apollo_panelProd(P, apollo_inputs, functionality)
      
      ### Prepare and return outputs of function
      P = apollo_prepareProb(P, apollo_inputs, functionality)
      return(P)
    }
    
    mdcev = apollo_prediction(model = coef3_model,
                              apollo_probabilities = apollo_probabilities,
                              apollo_inputs = apollo_inputs)
    mdcev <- mdcev[,c(1,3:16)]
    names(mdcev) <- c("ID", "NONMOTOR", "car1","car2","car3","van1","van2","van3","suv1","suv2","suv3","pickup1","pickup2","pickup3","MOTOR")
    mdcev$AVG <- 1
    mdcev <- mdcev %>% dplyr::relocate(AVG, .after=all_of("ID"))
    
    NumHousehold <- nrow(data1);
    pnumbyalter <- mdcev;
    pnumbyalter[pnumbyalter > 0] <- 1;
    pnumbyalter <- pnumbyalter[,3:(ncol(pnumbyalter))];
    
    pavgbyalter <- mdcev;
    pavgbyalter <- colSums(pavgbyalter[,3:(ncol(pavgbyalter))])
    pavgmileage <- pavgbyalter / NumHousehold;
    
    return(mdcev);
  }
  mdcev <- VehicleFleet_model(data1);
  
  # 4. Reallocation
  Heuristic_mileage <- function(data, mdcev) {
    mdcev$num_obs <- data$pred_own;
    cum_mdcev <- mdcev[,4:(ncol(mdcev)-1)];
    fpick <- cum_mdcev;
    fpick[fpick >= 0] <- 1;
    
    for (i in 1:5) {
      cum_mdcev <- mdcev[,4:(ncol(mdcev)-1)] * fpick;
      for(j in 2:ncol(cum_mdcev)) {
        cum_mdcev[,j] <- rowSums(cum_mdcev[,(j-1):j]);
      }
      
      total <- cum_mdcev[,(ncol(cum_mdcev))];
      cum_mdcev <- cum_mdcev / total;
      # add this (Michelle)
      cum_mdcev[is.na(cum_mdcev)] <- 0
      
      NumHousehold <- nrow(data);
      pick_mdcev <- cum_mdcev;
      
      # random number generator
      rand <- runif(NumHousehold);
      
      for (j in 1:(nc-1)) {
        if (j == 1) {
          pick_mdcev[(pick_mdcev[,j] > rand & 0 <= rand & mdcev[,length(mdcev)] >= i), j] <- 2;
        }
        else {
          pick_mdcev[(pick_mdcev[,j] > rand & pick_mdcev[,j-1] <= rand & mdcev[,length(mdcev)] >= i), j] <- 2;
        } 
      }
      
      pick_mdcev[pick_mdcev < 2] <- 0;
      pick_mdcev[pick_mdcev == 0] <- 1;
      pick_mdcev[pick_mdcev == 2] <- 0;
      
      fpick <- fpick * pick_mdcev;
      
    }
    avg_mdcev <- mdcev[,4:(ncol(mdcev)-1)];
    budget <- rowSums(avg_mdcev);
    fpick[fpick == 0] <- 2;
    fpick[fpick == 1] <- 0;
    fpick[fpick == 2] <- 1;
    pick_miles <- fpick * avg_mdcev;
    sum_pick_miles <- rowSums(pick_miles);
    indfor <- mdcev[,1:(ncol(mdcev)-1)];
    indfor$AVG <- NULL;
    indfor[,3:(ncol(indfor))] <- budget * pick_miles/sum_pick_miles;
    indfor[indfor == "NaN"] <- 0.0;
    
    
    pnumbyalter <- indfor;
    pnumbyalter[pnumbyalter > 0] <- 1;
    pnumbyalter <- pnumbyalter[,2:(ncol(pnumbyalter))];
    pnumpercent <- colSums(pnumbyalter) / nrow(pnumbyalter);
    
    pavgbyalter <- indfor;
    pavgbyalter <- colSums(pavgbyalter[,2:(ncol(pavgbyalter))]);
    pavgmileage <- pavgbyalter / colSums(pnumbyalter);
    
    return(list(indfor,pnumpercent,pavgmileage));
  }
  
  # 5. Number of body types
  NumBodytypes <- function(data, indfor, bothnum, bothmile, coefs_names, coefs_type) {
    
    # pst <- proc.time() # LJ comment out
    util_internal <- function(names, coefs) {
      
      util_veh <- data[names[1]] * coefs[1];
      if (length(names) >= 2) {
        for (i in 2:(length(names))) {
          util_veh <- util_veh + data[names[i]] * coefs[i];
        }
      }
      util_veh <- as.numeric(util_veh$const);
      return (exp(util_veh));
    }
    
    
    coefs_name_0type <- c(coefs_names[[1]]);
    coefs_name_1type <- c(coefs_names[[2]]);
    coefs_name_2type <- c(coefs_names[[3]]);
    coefs_name_3type <- c(coefs_names[[4]]);
    coefs_name_4type <- c(coefs_names[[5]]);
    
    coefs_0type <- c(coefs_type[[1]]);
    coefs_1type <- c(coefs_type[[2]]);
    coefs_2type <- c(coefs_type[[3]]);
    coefs_3type <- c(coefs_type[[4]]);
    coefs_4type <- c(coefs_type[[5]]);
    
    #data$const <- 1;
    
    num_body <- 4;
    
    ### Utility Computation
    util_zero_body <- util_internal(coefs_name_0type, coefs_0type);
    util_one_body <- util_internal(coefs_name_1type, coefs_1type);
    util_two_body <- util_internal(coefs_name_2type, coefs_2type);
    util_three_body <- util_internal(coefs_name_3type, coefs_3type);
    util_four_body <- util_internal(coefs_name_4type, coefs_4type);
    
    
    ### Probability computation
    total_util <- util_zero_body + util_one_body + util_two_body + util_three_body + util_four_body;
    prob_zero_body <- (util_zero_body/total_util);
    prob_one_body <- (util_one_body/total_util);
    prob_two_body <- (util_two_body/total_util);
    prob_three_body <- (util_three_body/total_util);
    prob_four_body <- (util_four_body/total_util);
    
    
    ### Cumulative probability computation
    cum_prob_zero_body <- prob_zero_body;
    cum_prob_one_body <- cum_prob_zero_body +  prob_one_body;
    cum_prob_two_body <- cum_prob_one_body +  prob_two_body;
    cum_prob_three_body <- cum_prob_two_body +  prob_three_body;
    cum_prob_four_body <- cum_prob_three_body +  prob_four_body; 
    
    
    NumHousehold <- nrow(data);
    # LJ note random number generated here
    rnum <- runif(NumHousehold);
    rnum[rnum < cum_prob_zero_body] <- 2;
    rnum[rnum < cum_prob_one_body] <- 3;
    rnum[rnum < cum_prob_two_body] <- 4;
    rnum[rnum < cum_prob_three_body] <- 5;
    rnum[rnum <= 1] <- 6;
    data$pred_body_MNL <- rnum - 2;
    
    
    pnumbybody <- indfor;
    pnumbybody[pnumbybody > 0] <- 1;
    pnumbybody <- pnumbybody[,3:(ncol(pnumbybody))];
    
    
    isCAR <- rowSums(pnumbybody[,1:3]);
    isVAN <- rowSums(pnumbybody[,4:6]);
    isSUV <- rowSums(pnumbybody[,7:9]);
    isPICK <- rowSums(pnumbybody[,10:12]);
    isMOTOR <- pnumbybody[,13];
    isCAR[isCAR > 0] <- 1;
    isVAN[isVAN > 0] <- 1;
    isSUV[isSUV > 0] <- 1;
    isPICK[isPICK > 0] <- 1;
    data$pred_body_HMR <- isCAR + isVAN + isSUV + isPICK + isMOTOR;
    data$pred_body_HMR[data$pred_body_HMR > 4] <- 4;
    
    i <- 1;
    while (i <= 5) {
      diff_body[i] <<- abs((sum(data$pred_body_HMR == (i-1)) - sum(data$pred_body_MNL == (i-1)))/NumHousehold);
      i <- i+1;
    }
    return(data[c("pred_body_HMR","pred_body_MNL")]);
  }
  infor <- NULL;
  tt3 <-NULL;
  iter <- 1
  
  diff_body <- c(1.0, 1.0, 1.0, 1.0, 1.0); # LJ change 9/13/2021: initialize diff_body here.
  
  while ((iter <= max_iteration) & (diff_body[1] > Tolerance | diff_body[2] > Tolerance | diff_body[3] > Tolerance | diff_body[4] > Tolerance | diff_body[5] > Tolerance)) {
    
    print(paste("Iteration", iter));
    
    
    tt2 <- Heuristic_mileage(data_temp, mdcev);
    indfor <- tt2[[1]];
    pnumpercent <- tt2[[2]];
    pavgmileage <- tt2[[3]];
    tt3 <- NumBodytypes(data_temp, indfor, pnumpercent, pavgmileage, coef_names_type, coef_values_type);
    names(tt3) <- c("NumBodyType_HMR","NumBodyType_MNL");
    
    print(diff_body);
    iter <- iter+1;
  }
  
  if(iter > max_iteration){
    N_fail <<- N_fail + 1
    print("Consider adjusting the tolerance criteria and/or re-calibrate MDCEV model");
  }
  
  #Sys.sleep(3); # LJ change 9/13/2021 remove sleep time
  
  indfor$ID <- NULL;
  names(indfor) <- c("NONMOTOR_HU","CAR1_HU","CAR2_HU","CAR3_HU","VAN1_HU","VAN2_HU","VAN3_HU","SUV1_HU","SUV2_HU","SUV3_HU","PICK1_HU",
                     "PICK2_HU","PICK3_HU","MOTER_HU");
  data_temp <- cbind(data_temp,indfor);
  data_temp <- cbind(data_temp, tt3);
  rm(tt3);
  names(indfor) <- c("nonmotor","car1","car2","car3","van1","van2","van3","suv1","suv2","suv3","pickup1",
                     "pickup2","pickup3","motor");
  
  # 6.1 Number of cars
  # update 11.1
  data_temp$ycar <- indfor$car1 + indfor$car2 + indfor$car3
  data_temp$ycar[data_temp$ycar > 0] <- 1
  data_temp$yvan <- indfor$van1 + indfor$van2 + indfor$van3
  data_temp$yvan[data_temp$yvan > 0] <- 1
  data_temp$ysuv <- indfor$suv1 + indfor$suv2 + indfor$suv3
  data_temp$ysuv[data_temp$ysuv > 0] <- 1
  data_temp$ypickup <- indfor$pickup1 + indfor$pickup2 + indfor$pickup3
  data_temp$ypickup[data_temp$ypickup > 0] <- 1
  
  
  car_count_model <- function(data, indfor, coef_names_car, coefs_car) {
    
    
    ### Dummy variables for Ordered logit model 
    data$ycar0_5 <- indfor$car1
    data$ycar0_5 [data$ycar0_5 > 0] <- 1
    data$ycar6_11 <- indfor$car2
    data$ycar6_11 [data$ycar6_11 > 0] <- 1
    data$ycar_12 <- indfor$car3
    data$ycar_12 [data$ycar_12 > 0] <- 1
    
    ### Compute total predicted mileage for car in the level of household
    data$carmile1 <- (indfor$car1 + indfor$car2 + indfor$car3)/10000;
    
    # ### Observed car counts
    # data$ncar_sum1 <- data$ncar
    # data$ncar_sum1 [data$ncar >= 4] <- 3
    # 
    # data$obs_ncar <- data$ncar_sum1 
    # 
    # 
    ### Ordered logit utility computation for car
    data$const <- 1
    data$cons <- 1*coefs_car[1]*data$const
    # car1
    data$vintage2 <- 0
    data$vintage3 <- 0
    # expect_op1 <- -data[coef_names_car[1]] * coefs_car[2]
    expect_op1 <- -data[coef_names_car[1]] * coefs_car[1] #update
    for (i in 3:length(coef_names_car)){
      expect_op1 <- expect_op1 + data[coef_names_car[i]] * coefs_car[i]
    }
    
    thresh1 <- expect_op1 + data$cons - 1*coefs_car[2]*data$const
    expect_op1 <- as.numeric(expect_op1$const)
    thresh1 <- as.numeric(thresh1$const)
    # car2
    data$vintage2 <- 1
    data$vintage3 <- 0
    # expect_op2 <- -data[coef_names_car[1]] * coefs_car[2]
    expect_op2 <- -data[coef_names_car[1]] * coefs_car[1] #update
    for (i in 3:length(coef_names_car)){
      expect_op2 <- expect_op2 + data[coef_names_car[i]] * coefs_car[i]
    }
    
    thresh2 <- expect_op2 + data$cons - 1*coefs_car[2]*data$const
    expect_op2 <- as.numeric(expect_op2$const)
    thresh2 <- as.numeric(thresh2$const)
    # car3
    data$vintage2 <- 0
    data$vintage3 <- 1
    # expect_op3 <- -data[coef_names_car[1]] * coefs_car[2]
    expect_op3 <- -data[coef_names_car[1]] * coefs_car[1] #update
    for (i in 3:length(coef_names_car)){
      expect_op3 <- expect_op3 + data[coef_names_car[i]] * coefs_car[i]
    }
    
    thresh3 <- expect_op3 + data$cons - 1*coefs_car[2]*data$const
    expect_op3 <- as.numeric(expect_op3$const)
    thresh3 <- as.numeric(thresh3$const)
    
    ### Monte Carlo simulation for each age category
    indfor$ncar1 <- count_car(data$const, expect_op1, thresh1, indfor$car1);
    indfor$ncar2 <- count_car(data$const, expect_op2, thresh2, indfor$car2);
    indfor$ncar3 <- count_car(data$const, expect_op3, thresh3, indfor$car3);
    
    
    # data$pred_ncar <- indfor$ncar1 + indfor$ncar2 + indfor$ncar3;
    # data$pred_ncar[data$pred_ncar > 3] <- 3;
    # comp_obs_pred <- list(data$obs_ncar, data$pred_ncar)
    
    
    # ### Chart showing observed vs. predicted car counts
    # png(file.path(tabdir, '6.1_Car_count_update.png'))
    # 
    # myplot <- ggplot(data=data, aes(x=pred_ncar)) + 
    #   geom_bar(aes(y = (..count..)/sum(..count..)), fill="steelblue") + 
    #   theme_minimal() +
    #   ggtitle("Car Count") +
    #   theme(plot.title = element_text(hjust = 0.5)) +
    #   labs(y="Percent of Households", x = "Number of Cars in Household")
    # print(myplot)
    # dev.off()
    return(indfor[c("ncar1","ncar2","ncar3")])
  }
  
  tt1 <- car_count_model(data_temp, indfor, coef_names_car, coefs_car);
  indfor <- cbind(indfor,tt1);
  rm(tt1);
  
  # 6.2 Number of vans
  van_count_model <- function(data, indfor, coef_names_van, coefs_van) {
    
    
    ### Dummy variables for Ordered Probit model 
    data$yvan0_5 <- indfor$van1;
    data$yvan0_5 [data$yvan0_5 > 0] <- 1;
    data$yvan6_11 <- indfor$van2;
    data$yvan6_11 [data$yvan6_11 > 0] <- 1;
    data$yvan_12 <- indfor$van3;
    data$yvan_12 [data$yvan_12 > 0] <- 1;
    data$vanmile1 <- (indfor$van1 + indfor$van2 + indfor$van3)/10000;
    
    # ### Observed van counts
    # data$nvan_sum1 <- data$nvan;
    # data$nvan_sum1 [data$nvan >= 3] <- 2;
    # data$obs_nvan <- data$nvan_sum1;
    
    
    ### Ordered probit utility computation for van
    data$cons <- coefs_van[1]*data$const
    # van1
    data$vintage2 <- 0
    data$vintage3 <- 0
    expect_op1 <- -data[coef_names_van[1]] * coefs_van[1];
    for (i in 2:(length(coef_names_van))) {
      expect_op1 <- expect_op1 + data[coef_names_van[i]] * coefs_van[i];
    }
    expect_op1 <- as.numeric(expect_op1$const);
    
    # van2
    data$vintage2 <- 1
    data$vintage3 <- 0
    expect_op2 <- -data[coef_names_van[1]] * coefs_van[1];
    for (i in 2:(length(coef_names_van))) {
      expect_op2 <- expect_op2 + data[coef_names_van[i]] * coefs_van[i];
    }
    expect_op2 <- as.numeric(expect_op2$const);
    
    # van3
    data$vintage2 <- 0
    data$vintage3 <- 1
    expect_op3 <- -data[coef_names_van[1]] * coefs_van[1];
    for (i in 2:(length(coef_names_van))) {
      expect_op3 <- expect_op3 + data[coef_names_van[i]] * coefs_van[i];
    }
    expect_op3 <- as.numeric(expect_op3$const);
    
    
    ### Monte Carlo simulation for each age category
    indfor$nvan1 <- count_veh(data$const, expect_op1, indfor$van1);
    indfor$nvan2 <- count_veh(data$const, expect_op2, indfor$van2);
    indfor$nvan3 <- count_veh(data$const, expect_op3, indfor$van3);
    
    
    data$pred_nvan <- indfor$nvan1 + indfor$nvan2 + indfor$nvan3;
    data$pred_nvan[data$pred_nvan > 2] <- 2;
    # comp_obs_pred <- list(data$obs_nvan, data$pred_nvan)
    
    
    # ### Chart showing observed vs. predicted van counts
    # png(file.path(tabdir, '6.2_Van_count_update.png'))
    # myplot <- ggplot(data=data, aes(x=pred_nvan)) + 
    #   geom_bar(aes(y = (..count..)/sum(..count..)), fill="steelblue") + 
    #   theme_minimal() +
    #   ggtitle("Van Count") +
    #   theme(plot.title = element_text(hjust = 0.5)) +
    #   labs(y="Percent of Households", x = "Number of Vans in Household")
    # print(myplot)
    # dev.off()
    return(indfor[c("nvan1","nvan2","nvan3")]);
  }
  
  tt1 <- van_count_model(data_temp, indfor, coef_names_van, coefs_van);
  indfor <- cbind(indfor,tt1);
  rm(tt1);
  
  # 6.3 Number of SUVs
  SUV_count_model <- function(data, indfor, coef_names_suv, coefs_suv) {
    
    ### Dummy variables for Ordered Probit model 
    data$ysuv0_5 <- indfor$suv1;
    data$ysuv0_5 [data$ysuv0_5 > 0] <- 1;
    data$ysuv6_11 <- indfor$suv2;
    data$ysuv6_11 [data$ysuv6_11 > 0] <- 1;
    data$ysuv_12 <- indfor$suv3;
    data$ysuv_12 [data$ysuv_12 > 0] <- 1;
    data$suvmile1 <- (indfor$suv1 + indfor$suv2 + indfor$suv3)/10000
    
    # ### Observed SUV counts
    # data$nsuv_sum1 <- data$nsuv;
    # data$nsuv_sum1 [data$nsuv >= 3] <- 2;
    # data$obs_nsuv <- data$nsuv_sum1;
    
    
    ### Ordered probit utility computation for SUV
    data$cons <- coefs_suv[1]*data$const
    # suv1
    data$vintage2 <- 0
    data$vintage3 <- 0
    expect_op1 <- -data[coef_names_suv[1]] * coefs_suv[1];
    for (i in 2:(length(coef_names_suv))) {
      expect_op1 <- expect_op1 + data[coef_names_suv[i]] * coefs_suv[i];
    }
    expect_op1 <- as.numeric(expect_op1$const);
    
    # suv2
    data$vintage2 <- 1
    data$vintage3 <- 0
    expect_op2 <- -data[coef_names_suv[1]] * coefs_suv[1];
    for (i in 2:(length(coef_names_suv))) {
      expect_op2 <- expect_op2 + data[coef_names_suv[i]] * coefs_suv[i];
    }
    expect_op2 <- as.numeric(expect_op2$const);
    
    # suv3
    data$vintage2 <- 0
    data$vintage3 <- 1
    expect_op3 <- -data[coef_names_suv[1]] * coefs_suv[1];
    for (i in 2:(length(coef_names_suv))) {
      expect_op3 <- expect_op3 + data[coef_names_suv[i]] * coefs_suv[i];
    }
    expect_op3 <- as.numeric(expect_op3$const);
    
    
    ### Monte Carlo simulation for each age category
    indfor$nsuv1 <- count_veh(data$const, expect_op1, indfor$suv1);
    indfor$nsuv2 <- count_veh(data$const, expect_op2, indfor$suv2);
    indfor$nsuv3 <- count_veh(data$const, expect_op3, indfor$suv3);
    
    
    data$pred_nsuv <- indfor$nsuv1 + indfor$nsuv2 + indfor$nsuv3;
    data$pred_nsuv[data$pred_nsuv > 2] <- 2;
    # comp_obs_pred <- list(data$obs_nsuv, data$pred_nsuv);
    
    # ### Chart showing observed vs. predicted suv counts
    # png(file.path(tabdir, '6.3_Suv_count_update.png'))
    # myplot <- ggplot(data=data, aes(x=pred_nsuv)) + 
    #   geom_bar(aes(y = (..count..)/sum(..count..)), fill="steelblue") + 
    #   theme_minimal() +
    #   ggtitle("SUV Count") +
    #   theme(plot.title = element_text(hjust = 0.5)) +
    #   labs(y="Percent of Households", x = "Number of SUVs in Household")
    # print(myplot)
    # dev.off()
    
    return (indfor[c("nsuv1","nsuv2","nsuv3")]);
  }
  
  tt1 <- SUV_count_model(data_temp, indfor, coef_names_suv, coefs_suv);
  indfor <- cbind(indfor,tt1);
  rm(tt1);
  
  # 6.4 Number of pickups
  pickup_count_model <- function(data, indfor, coef_names_pick, coefs_pick) {
    
    ### Dummy variables for Ordered Probit model 
    data$ypickup0_5 <- indfor$pickup1
    data$ypickup0_5 [data$ypickup0_5 > 0] <- 1
    data$ypickup6_11 <- indfor$pickup2
    data$ypickup6_11 [data$ypickup6_11 > 0] <- 1
    data$ypickup_12 <- indfor$pickup3
    data$ypickup_12 [data$ypickup_12 > 0] <- 1
    data$pickupmile1 <- (indfor$pickup1 + indfor$pickup2 + indfor$pickup3)/10000
    
    
    # ### Observed Pick-up counts
    # data$npickup_sum1 <- data$npickup
    # data$npickup_sum1 [data$npickup >= 3] <- 2
    # data$obs_npick <- data$npickup_sum1
    
    
    ### Ordered probit utility computation for Pick-up
    #data$const <- 1
    data$cons <- coefs_pick[1]*data$const
    # suv1
    data$vintage2 <- 0
    data$vintage3 <- 0
    expect_op1 <- -data[coef_names_pick[1]] * coefs_pick[1];
    for (i in 2:(length(coef_names_pick))) {
      expect_op1 <- expect_op1 + data[coef_names_pick[i]] * coefs_pick[i];
    }
    expect_op1 <- as.numeric(expect_op1$const);
    
    # suv2
    data$vintage2 <- 1
    data$vintage3 <- 0
    expect_op2 <- -data[coef_names_pick[1]] * coefs_pick[1];
    for (i in 2:(length(coef_names_pick))) {
      expect_op2 <- expect_op2 + data[coef_names_pick[i]] * coefs_pick[i];
    }
    expect_op2 <- as.numeric(expect_op2$const);
    
    # suv3
    data$vintage2 <- 0
    data$vintage3 <- 1
    expect_op3 <- -data[coef_names_pick[1]] * coefs_pick[1];
    for (i in 2:(length(coef_names_pick))) {
      expect_op3 <- expect_op3 + data[coef_names_pick[i]] * coefs_pick[i];
    }
    expect_op3 <- as.numeric(expect_op3$const);
    
    
    ### Monte Carlo simulation for each age category
    indfor$npickup1 <- count_veh(data$const, expect_op1, indfor$pickup1);
    indfor$npickup2 <- count_veh(data$const, expect_op2, indfor$pickup2);
    indfor$npickup3 <- count_veh(data$const, expect_op3, indfor$pickup3);
    
    
    data$pred_npick <- indfor$npickup1 + indfor$npickup2 + indfor$npickup3;
    data$pred_npick[data$pred_npick > 2] <- 2;
    # comp_obs_pred <- list(data$obs_npick, data$pred_npick)
    
    # ### Chart showing observed vs. predicted SUV counts
    # png(file.path(tabdir, '6.4_Pickup_count_update.png'))
    # myplot <- ggplot(data=data, aes(x=pred_npick)) + 
    #   geom_bar(aes(y = (..count..)/sum(..count..)), fill="steelblue") + 
    #   theme_minimal() +
    #   ggtitle("Pickup Count") +
    #   theme(plot.title = element_text(hjust = 0.5)) +
    #   labs(y="Percent of Households", x = "Number of Pickups in Household")
    # print(myplot)
    # dev.off()
    ### Final result of Vehicle Fleet Composition model 
    #write.table(indfor, file = paste(c(current_folder, output_file),collapse=''), row.names=FALSE, sep=",");
    
    return (indfor[c("npickup1","npickup2","npickup3")])
    
  }
  
  tt1 <- pickup_count_model(data_temp, indfor, coef_names_pick, coefs_pick);
  indfor <- cbind(indfor,tt1);
  rm(tt1);
  
  data_temp <- cbind(data_temp, indfor)
  data_temp[, match("NONMOTOR_HU",names(data_temp)):match("MOTER_HU",names(data_temp))] <- list(NULL)
  
  # 7. Powertrain
  if(sum(data_temp$ncar1)>0) car1 <-  as.data.frame(lapply(data_temp, rep, data_temp$ncar1)) %>% dplyr::mutate(VEHAGE=2.5, car=1, van=0, suv=0, pickup=0,
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
  # vehicles <- vehicles %>% group_by(household_id) %>% mutate(vehicle_id= sequence(n()))
  # LJ add 9/14/2021 error handling when there is no vehicles (except for motor bike, which is not included in here)
  if(is.null(vehicles)){ # if none of the vehicles exists, return a NULL dataframe
    return(NULL)
  }else{
    vehicles <- vehicles %>% group_by(household_id) %>% dplyr::mutate(vehicle_id= 1:n()) %>% ungroup()
    
    print(paste('now predicting power train at loop',loopi))
    powertrain_model <- function(data, coefs_names, coefs_vehs) {
      
      coefs_name_bev <- c(coefs_names[[1]]);
      coefs_name_phev <- c(coefs_names[[2]]);
      coefs_name_hybrid <- c(coefs_names[[3]]);
      coefs_name_ice <- c(coefs_names[[4]]);
      
      coefs_bev <- c(coefs_vehs[[1]]);
      coefs_phev <- c(coefs_vehs[[2]]);
      coefs_hybrid <- c(coefs_vehs[[3]]);
      coefs_ice <- c(coefs_vehs[[4]]);
      
      ### Utility computation
      util_bev <- utility_power(coefs_name_bev, coefs_bev, data);
      util_phev <- utility_power(coefs_name_phev, coefs_phev, data);
      util_hybrid <- utility_power(coefs_name_hybrid, coefs_hybrid, data);
      util_ice <- utility_power(coefs_name_ice, coefs_ice, data);
      
      
      ### Probability computation
      total_util <- util_bev + util_phev + util_hybrid + util_ice ;
      prob_bev <- (util_bev/total_util);
      prob_phev <- (util_phev/total_util);
      prob_hybrid <- (util_hybrid/total_util);
      prob_ice <- (util_ice/total_util);
      
      
      ### Cumulative probability computation
      cum_prob_bev <- prob_bev
      cum_prob_phev <- cum_prob_bev +  prob_phev
      cum_prob_hybrid <- cum_prob_phev +  prob_hybrid;
      cum_prob_ice <- cum_prob_hybrid +  prob_ice;
      
      
      ### Predicted choices
      Numvehicle <- nrow(data);
      rnum <- runif(Numvehicle);
      rnum[rnum < cum_prob_bev] <- 2;
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
    # if(is.null(vehicles)){ # if none of the vehicles exists, return a NULL dataframe
    #   return(NULL)
    # }else{
    #   vehicles <- vehicles %>% group_by(household_id) %>% dplyr::mutate(vehicle_id= 1:n()) %>% ungroup()
    #   
    #   print('now predicting power train')
    #   powertrain_model <- function(data, coefs_names, coefs_vehs) {
    #     
    #     coefs_name_hybrid <- c(coefs_names[[1]]);
    #     coefs_name_ice <- c(coefs_names[[2]]);
    #     
    #     coefs_hybrid <- c(coefs_vehs[[1]]);
    #     coefs_ice <- c(coefs_vehs[[2]]);
    #     
    #     ### Utility computation 
    #     util_hybrid <- utility_power(coefs_name_hybrid, coefs_hybrid, data);
    #     util_ice <- utility_power(coefs_name_ice, coefs_ice, data);
    #     
    #     
    #     ### Probability computation
    #     total_util <- util_hybrid + util_ice ;
    #     prob_hybrid <- (util_hybrid/total_util);
    #     prob_ice <- (util_ice/total_util);
    #     
    #     
    #     ### Cumulative probability computation
    #     cum_prob_hybrid <- prob_hybrid;
    #     cum_prob_ice <- cum_prob_hybrid +  prob_ice;
    #     
    #     
    #     ### Predicted choices
    #     Numvehicle <- nrow(data);
    #     rnum <- runif(Numvehicle);
    #     rnum[rnum < cum_prob_hybrid] <- 2;
    #     rnum[rnum < 1] <- 1;
    #     data$pred_power <- rnum
    #     
    #     data$pred_power[data$pred_power==1] <- "Hybrid"
    #     data$pred_power[data$pred_power==2] <- "ICE"
    #     
    #     tmp <- data[c("pred_power")];
    #     return(tmp);
    #   }
    tt1 <- powertrain_model(vehicles, coef_names_power, coef_values_power);
    vehicles <- cbind(vehicles,tt1);
    rm(tt1);
    
    # Updated by Qianmiao 2022.1.12
    # 8. Main driver
    print(paste('now predicting maindriver for loop',loopi))
    
    vehicles$power_ev <-  0
    vehicles$power_ev[vehicles$pred_power!="ICE"] <- 1
    vehicle_person <- vehicles %>% merge(persons, by = "household_id")
    
    ## Generate Interaction Term
    vehicle_person <- vehicle_person %>% mutate(R_AGE=R_AGE_IMP, R_SEX=R_SEX_IMP, van_age=van*R_AGE_IMP, suv_age=suv*R_AGE_IMP, pickup_age=pickup*R_AGE_IMP,
                                                van_sex=van*R_SEX_IMP, suv_sex=suv*R_SEX_IMP, pickup_sex=pickup*R_SEX_IMP,
                                                van_high=van*high, suv_high=suv*high, pickup_high=pickup*high,
                                                van_coll=van*college, suv_coll=suv*college, pickup_coll=pickup*college,
                                                van_grad=van*graduate, suv_grad=suv*graduate, pickup_grad=pickup*graduate,
                                                van_school=van*school, suv_school=suv*school, pickup_school=pickup*school,
                                                van_retired=van*retired_person, suv_retired=suv*retired_person, pickup_retired=pickup*retired_person,
                                                van_full=van*work, suv_full=suv*work, pickup_full=pickup*work,
                                                van_black=van*black, suv_black=suv*black, pickup_black=pickup*black,
                                                van_asian=van*asian, suv_asian=suv*asian, pickup_asian=pickup*asian,
                                                van_rother=van*race_other, suv_rother=suv*race_other, pickup_rother=pickup*race_other)
    
    
    #### Predict who is the main driver
    print('use predict func for main driver prob')
    vehicle_person <- cbind(vehicle_person, stats::predict(maindriver_logit3, vehicle_person, type='response')) # LJ add stats::
    names(vehicle_person)[ncol(vehicle_person)] <- "pred_driver"
    
    print('use find main driver')
    
    vehicle_person <- vehicle_person %>% group_by(household_id, vehicle_id) %>% dplyr::slice(which.max(pred_driver)) # LJ change added dplyr::
    vehicle_person <- vehicle_person[, c("household_id", "vehicle_id", "person_id")]
    names(vehicle_person)[match("person_id", names(vehicle_person))] <- "maindriver_id"
    # 
    # ### Result of Vehicle Level Data
    vehicles <- vehicles %>% merge(vehicle_person, by=c("household_id", "vehicle_id"), all.x=TRUE)
    vehicles$annual_mileage <- 0
    vehicles$annual_mileage[vehicles$ycar1==1] <- vehicles$car1[vehicles$ycar1==1]/vehicles$ncar1[vehicles$ycar1==1]
    vehicles$annual_mileage[vehicles$ycar2==1] <- vehicles$car2[vehicles$ycar2==1]/vehicles$ncar2[vehicles$ycar2==1]
    vehicles$annual_mileage[vehicles$ycar3==1] <- vehicles$car3[vehicles$ycar3==1]/vehicles$ncar3[vehicles$ycar3==1]
    vehicles$annual_mileage[vehicles$yvan1==1] <- vehicles$van1[vehicles$yvan1==1]/vehicles$nvan1[vehicles$yvan1==1]
    vehicles$annual_mileage[vehicles$yvan2==1] <- vehicles$van2[vehicles$yvan2==1]/vehicles$nvan2[vehicles$yvan2==1]
    vehicles$annual_mileage[vehicles$yvan3==1] <- vehicles$van3[vehicles$yvan3==1]/vehicles$nvan3[vehicles$yvan3==1]
    vehicles$annual_mileage[vehicles$ysuv1==1] <- vehicles$suv1[vehicles$ysuv1==1]/vehicles$nsuv1[vehicles$ysuv1==1]
    vehicles$annual_mileage[vehicles$ysuv2==1] <- vehicles$suv2[vehicles$ysuv2==1]/vehicles$nsuv2[vehicles$ysuv2==1]
    vehicles$annual_mileage[vehicles$ysuv3==1] <- vehicles$suv3[vehicles$ysuv3==1]/vehicles$nsuv3[vehicles$ysuv3==1]
    vehicles$annual_mileage[vehicles$ypickup1==1] <- vehicles$pickup1[vehicles$ypickup1==1]/vehicles$npickup1[vehicles$ypickup1==1]
    vehicles$annual_mileage[vehicles$ypickup2==1] <- vehicles$pickup2[vehicles$ypickup2==1]/vehicles$npickup2[vehicles$ypickup2==1]
    vehicles$annual_mileage[vehicles$ypickup3==1] <- vehicles$pickup3[vehicles$ypickup3==1]/vehicles$npickup3[vehicles$ypickup3==1]
    
    # 9. Own or Lease
    # Generate indicators
    print('now predicting own lease')
    
    vehicles <- vehicles %>% mutate(VEHAGE0=case_when(VEHAGE==2.5~1, TRUE~0),
                                    VEHAGE1=case_when(VEHAGE==8.5~1, TRUE~0),
                                    VEHAGE2=case_when(VEHAGE==14.5~1, TRUE~0),
                                    hybrid=case_when(pred_power=="Hybrid"~1, TRUE~0),
                                    bev=case_when(pred_power=="AEV"~1, TRUE~0),
                                    hybrid=case_when(pred_power=="Hybrid"~1, TRUE~0),
                                    phev=case_when(pred_power=="PHEV"~1, TRUE~0))
    
    #### Predict own or lease
    vehicles <- cbind(vehicles, stats::predict(ownlease.static, vehicles, type='response')) # LJ add stats::
    names(vehicles)[ncol(vehicles)] <- "pred_ownlease"
    rnum <- runif(nrow(vehicles))
    rnum[rnum < vehicles$pred_ownlease] <- 1
    rnum[rnum !=1 ] <- 0
    vehicles <- cbind(vehicles, rnum)
    vehicles["pred_own"] <- NULL
    names(vehicles)[ncol(vehicles)] <- "pred_own"
    
    print('return results')
    return(vehicles)
    
    
    # # Add: 2022.1.13 by Qianmiao
    # # Generate two data sets
    # households_output <- vehicles %>% select(household_id, budget) %>% group_by(household_id) %>% summarise(nvehicles=n(), budget=mean(budget))
    # households_output <- data1 %>% merge(households_output, by="household_id", all.x = T) %>% select(household_id, nvehicles, budget)
    # households_output$nvehicles[is.na(households_output$nvehicles)==T] <- 0
    # households_output$budget[is.na(households_output$budget)==T] <- 0
    # # households_output <- households_output %>% mutate(nvehicles_cat=case_when(nvehicles==0~"None", nvehicles==1~"One",
    # #                                                                           nvehicles==2~"Two", nvehicles==3~"Three", T~"Four or more"))
    # 
    # vehicles_output <- vehicles %>% select(household_id, vehicle_id, VEHAGE:pred_own) %>%
    #   mutate(bodytype=case_when(car==1~"car", van==1~"van", suv==1~"suv", pickup==1~"pickup", T~"others"),
    #          vintage_category=case_when(VEHAGE0==1~"0~5 years", VEHAGE1==1~"6~11 years", VEHAGE2==1~"12+ years"),
    #          ownlease=case_when(pred_own==1~"own", T~"lease")) %>% select(household_id, vehicle_id, bodytype, vintage_category,
    #                                                                       maindriver_id, annual_mileage, pred_power, ownlease)
    # 
    # output <- list()
    # output[[1]] <- households_output
    # output[[2]] <- vehicles_output
    # 
    # print('return results')
    # return(output)
  }
}
