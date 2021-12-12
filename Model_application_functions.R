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
# 3
coef3_prepare <- function(coef3, coef_mdcev){
  # Car 0 - 5 years old
  ivmt2 <- coef_mdcev[[1]]
  ivmt2_leng <- length(ivmt2)
  # Car 6 - 11 years old
  ivmt3 <- coef_mdcev[[2]]
  ivmt3_leng <- ivmt2_leng + length(ivmt3)
  # Car 12 years or older
  ivmt4 <- coef_mdcev[[3]]
  ivmt4_leng <- ivmt3_leng + length(ivmt4)
  # Van 0 - 5 years old
  ivmt5 <- coef_mdcev[[4]]
  ivmt5_leng <- ivmt4_leng + length(ivmt5)
  # Van 6-11 years old
  ivmt6 <- coef_mdcev[[5]]
  ivmt6_leng <- ivmt5_leng + length(ivmt6)
  # Van 12 years or older
  ivmt7 <- coef_mdcev[[6]]
  ivmt7_leng <- ivmt6_leng + length(ivmt7)
  # SUV 0 - 5 years old
  ivmt8 <- coef_mdcev[[7]]
  ivmt8_leng <- ivmt7_leng + length(ivmt8)
  # SUV 6-11 years old
  ivmt9 <- coef_mdcev[[8]]
  ivmt9_leng <- ivmt8_leng + length(ivmt9)
  # SUV 12 years or older
  ivmt10 <- coef_mdcev[[9]]
  ivmt10_leng <- ivmt9_leng + length(ivmt10)
  # Pick-up 0-5 years old
  ivmt11 <- coef_mdcev[[10]]
  ivmt11_leng <- ivmt10_leng + length(ivmt11)
  # Pick-up 6-11 years old
  ivmt12 <- coef_mdcev[[11]]
  ivmt12_leng <- ivmt11_leng + length(ivmt12)
  # Pick-up 12 years or older
  ivmt13 <- coef_mdcev[[12]]
  ivmt13_leng <- ivmt12_leng + length(ivmt13)
  # Motorbike
  ivmt14 <- coef_mdcev[[13]]
  ivmt14_leng <- ivmt13_leng + length(ivmt14)
  
  ivmts <- list(ivmt2,ivmt3,ivmt4,ivmt5,ivmt6,ivmt7,ivmt8,ivmt9,ivmt10,ivmt11,ivmt12,ivmt13,ivmt14);
  
  #### Translation parameters
  # Car 0 - 5 years old
  ivgt2 <- c("uno")
  # Car 6 - 11 years old
  ivgt3 <- c("uno")
  # Car 12 years or older
  ivgt4 <- c("uno")
  # Van 0 - 5 years old
  ivgt5 <- c("uno")
  # Van 6-11 years old
  ivgt6 <- c("uno")
  # Van 12 years or older
  ivgt7 <- c("uno")
  # SUV 0 - 5 years old
  ivgt8 <- c("uno")
  # SUV 6-11 years old
  ivgt9 <- c("uno")
  # SUV 12 years or older
  ivgt10 <- c("uno")
  # Pick-up 0-5 years old
  ivgt11 <- c("uno")
  # Pick-up 6-11 years old
  ivgt12 <- c("uno")
  # Pick-up 12 years or older
  ivgt13 <- c("uno")
  # Motorbike
  ivgt14 <- c("uno")
  ivgts <- list(ivgt2,ivgt3,ivgt4,ivgt5,ivgt6,ivgt7,ivgt8,ivgt9,ivgt10,ivgt11,ivgt12,ivgt13,ivgt14)
  
  ivs <- list(ivmts, ivgts, ivmt14_leng)
  return(ivs);
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

# 3
mdcev_ind <- function(ivmt, ind, nc, numcoefficients) {
  i <- 1;
  while (i <= (nc-1)) {
    j <- 1;
    while (j <= numcoefficients[i]) {
      if (i > ind) {
        ivmt <- c(ivmt, "sero");
      }
      if (i < ind) {
        ivmt <- c("sero", ivmt);
      }
      j <- j+1;
    }
    i <- i+1;
  }
  return (ivmt);
}

mdcev_ivgt_ind <- function(ivgt, ind, nc, numgamma) {
  ivgt <- c("sero", ivgt);
  i <- 1;
  while (i <= (nc-1)) {
    j <- 1;
    while (j <= numgamma[i]) {
      if (i > ind) {
        ivgt <- c(ivgt, "sero");
      }
      if (i < ind) {
        ivgt <- c("sero", ivgt);
      }
      j <- j+1;
    }
    i <- i+1;
  }
  return (ivgt);
}


# 6.1
count_car <- function(const, expect_op, thresh, ncar) {
  
  p1 <- 1/(1+exp(expect_op));
  p2 <- 1/(1+exp(thresh));
  p3 <- const;
  
  isCAR <- ncar;
  isCAR[isCAR > 0] <- 1;
  
  # a=as.numeric(Sys.time()) # LJ add
  # set.seed(a)
  
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
  
  # a=as.numeric(Sys.time()) # LJ add
  # set.seed(a)
  
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
# LJ add persons into the argument
model_application <- function(persons, data1, coefs_name_mile, coefs_mile , coefs_names, coefs_vehs, 
                              ivmts, ivgts, coef_names_type, coef_values_type, coef_names_car, coefs_car,
                              coef_names_van, coefs_van, coef_names_suv, coefs_suv, coef_names_pick, coefs_pick,
                              coef_names_power, coef_values_power){
  
  require(tidyr)
  require(dplyr)
  # 1. Mileage prediction
  
  mileage_model <- function(data, coefs_name_mile, coefs_mile) {
    
    data$const <- 1;
    #how to decide the sd
    
    error_term <- rnorm(length(data$const), mean=0, sd=4); # LJ change 9/14/2021 changed from 6 to 4.
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
    
    # a=as.numeric(Sys.time()) # LJ add
    # set.seed(a)
    # 
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
  VehicleFleet_model <- function(data, ivmts, ivgts) {
    
    uno <- controls[[1]] #"uno"; # Column of ones
    sero <- controls[[2]] #"sero"; # Column of zeros
    
    po <- controls[[3]];   #7 # Column number of the unique identifier created for a household (this column is created by the user)
    nrep <- controls[[4]]; #25 # Number of replications of the MDCEV model to be carried out
    
    numout <- controls[[5]];  #1
    config <- controls[[6]];  #4    #Utility specification configuration, possible values: 1,4,5,6,7
    alp0to1 <- 1;    #1 if you want the Alpha values to be constrained between 0 and 1, 0 otherwise
    price <- 0;       #1 if there is price variation across goods, 0 otherwise
    alt_names <- c("NONMOTOR","car1","car2","car3","van1","van2","van3","suv1","suv2","suv3","pickup1","pickup2","pickup3","MOTOR");
    
    gumbel <- 1;
    
    if (gumbel == 0) {
      nrep <- 1;
    }
    avg <- 1;
    budget_index <- grep('budget', colnames(data));
    f <- c(budget_index, sero, sero, sero, sero, sero, sero, sero, sero, sero, sero,  sero, sero, sero);
    
    #fp <- c(5,  5,  5,  5,  5,	5,	5,	5,	5,	5,	5,	5,	5,	5); 
    fp <- c(rep(uno,nc));
    # This is subject to change
    ivmt1 <- rep("sero", ivmt14_length)
    ivmt2 <- c(ivmts[[1]]);
    ivmt3 <- c(ivmts[[2]]);
    ivmt4 <- c(ivmts[[3]]);
    ivmt5 <- c(ivmts[[4]]);
    ivmt6 <- c(ivmts[[5]]);
    ivmt7 <- c(ivmts[[6]]);
    ivmt8 <- c(ivmts[[7]]);
    ivmt9 <- c(ivmts[[8]]);
    ivmt10 <- c(ivmts[[9]]);
    ivmt11 <- c(ivmts[[10]]);
    ivmt12 <- c(ivmts[[11]]);
    ivmt13 <- c(ivmts[[12]]);  
    ivmt14 <- c(ivmts[[13]]);
    
    numcoefficients <- c(length(ivmt2), length(ivmt3), length(ivmt4), length(ivmt5), length(ivmt6), length(ivmt7), length(ivmt8),
                          length(ivmt9), length(ivmt10), length(ivmt11), length(ivmt12), length(ivmt13), length(ivmt14));
    
    ivmt2 <- mdcev_ind(ivmt2, 1, nc, numcoefficients);
    ivmt3 <- mdcev_ind(ivmt3, 2, nc, numcoefficients);
    ivmt4 <- mdcev_ind(ivmt4, 3, nc, numcoefficients);
    ivmt5 <- mdcev_ind(ivmt5, 4, nc, numcoefficients);
    ivmt6 <- mdcev_ind(ivmt6, 5, nc, numcoefficients);
    ivmt7 <- mdcev_ind(ivmt7, 6, nc, numcoefficients);
    ivmt8 <- mdcev_ind(ivmt8, 7, nc, numcoefficients);
    ivmt9 <- mdcev_ind(ivmt9, 8, nc, numcoefficients);
    ivmt10 <- mdcev_ind(ivmt10, 9, nc, numcoefficients);
    ivmt11 <- mdcev_ind(ivmt11, 10, nc, numcoefficients);
    ivmt12 <- mdcev_ind(ivmt12, 11, nc, numcoefficients);
    ivmt13 <- mdcev_ind(ivmt13, 12, nc, numcoefficients);
    ivmt14 <- mdcev_ind(ivmt14, 13, nc, numcoefficients);
    
    #First good is outside good "Annual Non_auto mileage"
    ivdt1 <-  c(uno,	sero,	sero,	sero,	sero,	sero,	sero,	sero,	sero,	sero,	sero,	sero,	sero,	sero);
    ivdt2	<-	c(sero,	uno,	sero, sero,	sero,	sero,	sero,	sero,	sero,  sero,	sero,	sero,	sero,	sero);
    ivdt3	<-	c(sero,	sero,	uno,	sero,	sero,	sero,	sero,	sero,	sero,  sero,	sero,	sero,	sero,	sero);
    ivdt4	<-	c(sero,	sero,	sero,	uno,	sero,	sero,	sero,	sero,	sero,  sero,	sero,	sero,	sero,	sero);
    ivdt5	<-	c(sero,	sero,	sero,	sero,	uno,	sero,	sero,	sero,	sero,  sero,	sero,	sero,	sero,	sero);
    ivdt6	<-	c(sero,	sero,	sero,	sero,	sero,	uno,	sero,	sero,	sero,  sero,	sero,	sero,	sero,	sero);
    ivdt7	<-	c(sero,	sero,	sero,	sero,	sero,	sero,	uno,	sero,	sero,  sero,	sero,	sero,	sero,	sero);
    ivdt8	<-	c(sero,	sero,	sero,	sero,	sero,	sero,	sero,	uno,	sero,	sero,	sero,	sero,	sero,	sero);
    ivdt9	<-	c(sero,	sero,	sero,	sero,	sero,	sero,	sero,	sero,	uno,	sero,	sero,	sero,	sero,	sero);
    ivdt10 <-	c(sero,  sero,	sero,	sero,	sero,	sero,	sero,	sero,	sero,	uno,	sero,	sero,	sero,	sero);
    ivdt11 <-	c(sero,  sero,	sero,	sero,	sero,	sero,	sero,	sero,	sero,	sero,	uno,	sero,	sero,	sero);
    ivdt12 <-	c(sero,  sero,	sero,	sero,	sero,	sero,	sero,	sero,	sero,	sero,	sero,	uno,	sero,	sero);
    ivdt13 <-	c(sero,  sero,	sero,	sero,	sero,	sero,	sero,	sero,	sero,	sero,	sero,	sero,	uno,	sero);
    ivdt14 <-	c(sero,  sero,	sero,	sero,	sero,	sero,	sero,	sero,	sero,	sero,	sero,	sero,	sero,	uno);
    
    #First good is outside good "home epoch" and therefore gamma = 0 for ivgt1
    ###This is subject to change
    ivgt1 <- c("sero",  "sero",  "sero",  "sero",	"sero",	"sero",	"sero",	"sero",	"sero",	"sero",	"sero",	"sero",	"sero",	"sero");
    ivgt2 <- c(ivgts[[1]]);
    ivgt3 <- c(ivgts[[2]]);
    ivgt4 <- c(ivgts[[3]]);
    ivgt5 <- c(ivgts[[4]]);
    ivgt6 <- c(ivgts[[5]]);
    ivgt7 <- c(ivgts[[6]]);
    ivgt8 <- c(ivgts[[7]]);
    ivgt9 <- c(ivgts[[8]]);
    ivgt10 <- c(ivgts[[9]]);
    ivgt11 <- c(ivgts[[10]]);
    ivgt12 <- c(ivgts[[11]]);
    ivgt13 <- c(ivgts[[12]]);  
    ivgt14 <- c(ivgts[[13]]);
    
    numgamma <- c(length(ivgt2), length(ivgt3), length(ivgt4), length(ivgt5), length(ivgt6), length(ivgt7), length(ivgt8),
                   length(ivgt9), length(ivgt10), length(ivgt11), length(ivgt12), length(ivgt13), length(ivgt14));
    
    ivgt2 <- mdcev_ivgt_ind(ivgt2, 1, nc, numgamma);
    ivgt3 <- mdcev_ivgt_ind(ivgt3, 2, nc, numgamma);
    ivgt4 <- mdcev_ivgt_ind(ivgt4, 3, nc, numgamma);
    ivgt5 <- mdcev_ivgt_ind(ivgt5, 4, nc, numgamma);
    ivgt6 <- mdcev_ivgt_ind(ivgt6, 5, nc, numgamma);
    ivgt7 <- mdcev_ivgt_ind(ivgt7, 6, nc, numgamma);
    ivgt8 <- mdcev_ivgt_ind(ivgt8, 7, nc, numgamma);
    ivgt9 <- mdcev_ivgt_ind(ivgt9, 8, nc, numgamma);
    ivgt10 <- mdcev_ivgt_ind(ivgt10, 9, nc, numgamma);
    ivgt11 <- mdcev_ivgt_ind(ivgt11, 10, nc, numgamma);
    ivgt12 <- mdcev_ivgt_ind(ivgt12, 11, nc, numgamma);
    ivgt13 <- mdcev_ivgt_ind(ivgt13, 12, nc, numgamma);
    ivgt14 <- mdcev_ivgt_ind(ivgt14, 13, nc, numgamma);
    
    ivm <- c(ivmt1, ivmt2, ivmt3, ivmt4, ivmt5, ivmt6, ivmt7, ivmt8, ivmt9, ivmt10, ivmt11, ivmt12, ivmt13, ivmt14);
    ivd <- c(ivdt1, ivdt2, ivdt3, ivdt4, ivdt5, ivdt6, ivdt7, ivdt8, ivdt9, ivdt10, ivdt11, ivdt12, ivdt13, ivdt14);
    ivg <- c(ivgt1, ivgt2, ivgt3, ivgt4, ivgt5, ivgt6, ivgt7, ivgt8, ivgt9, ivgt10, ivgt11, ivgt12, ivgt13, ivgt14);
    
    
    nvarm <- length(ivmt1);    #number of variables in baseline utility   = number of columns in ivm1, do not modify this
    nvardel <- length(ivdt1);  #number of variables in satiation          = number of columns in ivd1, do not modify this
    nvargam <- length(ivgt1);  #number of variables in translation        = number of columns in ivg1, do not modify this
    
    
    #Associating columns with variable names
    flagchm <- f;        
    flagavm <- c(rep(uno,nc));
    flagprcm <- fp;
    
    
    #Do not modify the line below####################### 
    if (config == 1) {
      eqmatdel <- diag(nvardel);
      eqmatgam <- rbind(matrix(1,nrow=1,ncol=numout),matrix(0,nrow=1,ncol=numout));
      eqmatgam <- cbind(eqmatgam, rbind(matrix(0,nrow=1,ncol=(nc-numout)),matrix(1,nrow=1,ncol=(nc-numout))));
    }
    if (config == 4) {
      eqmatdel <- matrix(1,nrow=1,ncol=nc);
      eqmatgam <- diag(nvargam);
    }
    if (config == 5) {
      eqmatdel <- matrix(1,nrow=1,ncol=nvardel);
      eqmatgam <- diag(nvargam);
    }
    if (config == 6) {
      eqmatdel <- rbind(matrix(1,nrow=1,ncol=numout),matrix(0,nrow=1,ncol=numout));
      eqmatdel <- cbind(eqmatdel, rbind(matrix(0,nrow=1,ncol=(nc-numout)),matrix(1,nrow=1,ncol=(nc-numout))));
      eqmatgam <- diag(nc);
    }
    if (config == 7) {
      eqmatdel <- matrix(1,nrow=1,ncol=nc);
      eqmatgam <- diag(nc);
    }

    bmdcev <- coef3;
    
    lpr <- function(x) {
      
      ptm1 <- proc.time();
      
      e1 <- nrow(data);
      popass <- as.matrix(data[,po]);
      
      xdel <- t(eqmatdel)%*%x[(nvarm+1):(nvarm+nrow(eqmatdel)),];
      xgam <- t(eqmatgam)%*%x[(nvarm+nrow(eqmatdel)+1):(nvarm+nrow(eqmatdel)+nrow(eqmatgam)),];
      xsigm <- x[(nvarm+nrow(eqmatdel)+nrow(eqmatgam)+1),];
      
      a <- matrix(c(rep(1,nc)), ncol=1)%x%x[1:nvarm,];
      b <- matrix(c(rep(1,nc)), ncol=1)%x%xdel;
      c <- matrix(c(rep(1,nc)), ncol=1)%x%xgam;
      
      v2 <- sweep(data[,ivm],MARGIN=2,t(a),'*');
      w2 <- sweep(data[,ivd],MARGIN=2,t(b),'*');
      u2 <- sweep(data[,ivg],MARGIN=2,t(c),'*');
      rm(a, b, c);

      v <- NULL;
      w <- NULL;
      u <- NULL;
      alts <- matrix(0,nrow=1,ncol=nc);
      for (j in 1:nc) {
        #v <- cbind(v, as.matrix(apply(v2[,((j-1)*nvarm+1):(j*nvarm)],1,sum),ncol=1));
        #w <- cbind(w, as.matrix(apply(w2[,((j-1)*nvardel+1):(j*nvardel)],1,sum),ncol=1));
        #u <- cbind(u, as.matrix(apply(u2[,((j-1)*nvargam+1):(j*nvargam)],1,sum),ncol=1));
        
        v <- cbind(v, as.matrix(rowSums(v2[,((j-1)*nvarm+1):(j*nvarm)]),ncol=1));
        w <- cbind(w, as.matrix(rowSums(w2[,((j-1)*nvardel+1):(j*nvardel)]),ncol=1));
        u <- cbind(u, as.matrix(rowSums(u2[,((j-1)*nvargam+1):(j*nvargam)]),ncol=1));
        
        alts[1,j] <- j;
      }
      u[,1:numout] <- -1000*matrix(1,nrow=e1,ncol=numout);
      rm(v2, w2);
      v <- exp(v);
      v <- kronecker(v,matrix(1,nrow=nrep,ncol=1));
      
      if (gumbel == 1) {
        #as <- halton(nrep*e1, dim = nc, init = TRUE, normal = FALSE, usetime = FALSE)
        # as <- read.table(Halton_File, header=F, sep=","); # LJ 9/9/2021, change halton sequence to a global variable to be read here
        as <- HaltonSequence; # LJ change 9/7/2021
        as <- as.matrix(as[22:(e1*nrep+21),1:nc]);
        as <- -log(-log(as))*xsigm;
      }
      else {
        as <- matrix(0,nrow=(nrep*e1),ncol=nc);
      }

      v <- v*exp(as);
      tmp <- kronecker(as.matrix(data[,flagprcm]),matrix(1,nrow=nrep,ncol=1));
      v <- v/tmp ;   # Price-normalized Baseline Utilities
      rm(tmp);
      a <- 1-(1/(1+exp(w))) ;        # Alphas
      prices <- as.matrix(data[,flagprcm]);
      f1 <- exp(u);                # Gammas  

      for (i in 1:e1) {
        for (j in 1:nrep) {
          fc <- matrix(0,nrow=1,ncol=nc);
          vqr <- rbind(alts,rbind(v[((i-1)*nrep+j),],rbind(prices[i,],f1[i,])));
          vqr1 <- vqr[,1:numout];
          vqr2 <- vqr[,(numout+1):nc];
          vqr <- cbind(vqr1,vqr2[,order(vqr2[2,],decreasing = TRUE)]);
          
          m <- numout;
          k <- -1;
          N <- rowSums(t(prices[i,1:numout]*((vqr[2,1:numout])^(1/(matrix(1,nrow=1,ncol=numout)-a[1,1:numout])))));
          D <- colSums(t(data[i,flagchm]));
          
          lambda <- (N/D)^(1-a[1,1]);     
          
          if (vqr[2,(numout+1)] < lambda) {
            fc[1,1:numout] <- ((vqr[2,1:numout]/lambda)^(1/(matrix(1,nrow=1,ncol=numout) - a[1,1:numout])));
            fc[1,(numout+1):nc] <- matrix(0,nrow=1,ncol=(nc-1));
          }
          else{
            while (m != k){
              m <- m+1;
              if (m == nc) {            
                fc[1,1:numout] <- ((vqr[2,1:numout]/lambda)^(1/(1-a[1,1:numout])));
                fc[1,(numout+1):nc] <- (((vqr[2,(numout+1):nc]/lambda)^(1/(1-a[1,(numout+1):nc])))-matrix(1,nrow=1,ncol=(nc-numout)))*(vqr[4,(numout+1):nc]);
                fc[1,1:nc] <- colSums(t(data[i,flagchm]))*fc[1,1:nc]/rowSums(t(fc[1,1:nc]));
                k <- m;
              }
              else if (m < nc){
                N <- N + (vqr[4,m]*vqr[3,m]*(vqr[2,m]^(1-a[1,1])));
                D <- D + (vqr[4,m]*vqr[3,m]);
                lambda <- (N/D)^(1-a[1,1]);
                
                if (vqr[2,m+1] < lambda){
                  fc[1,1:numout] <- ((vqr[2,1:numout]/lambda)^(1/(1-a[1,1:numout])));
                  fc[1,(numout+1):m] <- (((vqr[2,(numout+1):m]/lambda)^(1/(1-a[1,(numout+1):m])))-matrix(1,nrow=1,ncol=(m-numout)))*(vqr[4,(numout+1):m]);
                  fc[1,(m+1):nc] <- matrix(0,nrow=1,ncol=(nc-m));
                  k <- m;
                }
              }
            }
          }
          vqr[2,1:nc] <- fc;
          vqr <- vqr[,order(vqr[1,])];
          v[((i-1)*nrep+j),1:nc] <- vqr[2,];
        }
      }
      nreps = NULL;
      for (i in 1:nrep) {
        nreps = rbind(nreps, i);
      }
      z1 <- kronecker(popass,matrix(1,nrow=nrep,ncol=1));
      z2 <- kronecker(matrix(1,nrow=nrow(popass),ncol=1),nreps);
      z <- cbind(z1,z2);
      z <- cbind(z,v);
      rm(z1,z2);
      return(z);
    }
    
    Forecasts <- lpr(bmdcev);
    nobs <- nrow(data);
    if (avg == 1) {
      
      mdcev <- NULL
      for (ii in 0:(nobs-1)) {
        mdcev <- rbind(mdcev,colSums(Forecasts[(ii*nrep+1):(ii*nrep+nrep),]/nrep)) 
      }
      mdcev <- as.data.frame(mdcev); 
      names(mdcev) <- c("ID","AVG",alt_names);
      print("Done!")
    }
    if (avg == 0) {
      print("Done!")
    }
    rm(Forecasts);
    
    NumHousehold <- nrow(data);
    pnumbyalter <- mdcev;
    pnumbyalter[pnumbyalter > 0] <- 1;
    pnumbyalter <- pnumbyalter[,3:(ncol(pnumbyalter))];
    
    pavgbyalter <- mdcev;
    pavgbyalter <- colSums(pavgbyalter[,3:(ncol(pavgbyalter))])
    pavgmileage <- pavgbyalter / NumHousehold;
    
    return(mdcev);
  }
 
   mdcev <- VehicleFleet_model(data_temp, ivmts, ivgts);
  
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
      
      # a=as.numeric(Sys.time()) # LJ add
      # set.seed(a)
      # 
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
    
    pst <- proc.time()
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
    
    num_body <- 4; # LJ this variable is not used.
    
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
     # a=as.numeric(Sys.time()) # LJ add
     # set.seed(a)

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
    data$pred_body_HMR[data$pred_body_HMR > 4] <- 4;  # LJ Comment?? preset max 4 bodytypes per hh.
    
    i <- 1;
    while (i <= 5) { # 9/10/2021 by LJ change to <<-  # Note here, when we only have 1 hh, the bodytype counts need to match exactly.
      diff_body[i] <<- abs((sum(data$pred_body_HMR == (i-1)) - sum(data$pred_body_MNL == (i-1)))/NumHousehold);
      i <- i+1;
    }
    return(data[c("pred_body_HMR","pred_body_MNL")]);
  }
  
  infor <- NULL;
  tt3 <-NULL;
  iter <- 1;
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
    N_fail <<- N_fail + 1 # count the number of hh that fails to converge
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
  car_count_model <- function(data, indfor, coef_names, coef_values) {
    
    ### Dummy variables for Ordered logit model 
    data$ycar0_5 <- indfor$car1
    data$ycar0_5 [data$ycar0_5 > 0] <- 1
    data$ycar6_11 <- indfor$car2
    data$ycar6_11 [data$ycar6_11 > 0] <- 1
    data$ycar_12 <- indfor$car3
    data$ycar_12 [data$ycar_12 > 0] <- 1
    
    ### Compute total predicted mileage for car in the level of household
    data$carmile1 <- (indfor$car1 + indfor$car2 + indfor$car3)/10000;
    
    ### Ordered logit utility computation for car
    data$const <- 1
    data$cons <- 1*coef_values[1]*data$const
    
    expect_op <- -data[coef_names[1]] * coef_values[1]
    for (i in 3:length(coef_names)){
      expect_op <- expect_op + data[coef_names[i]] * coef_values[i]
    }
    
    thresh <- expect_op + data$cons - 1*coef_values[2]*data$const
    expect_op <- as.numeric(expect_op$const)
    thresh <- as.numeric(thresh$const)
    
    ### Monte Carlo simulation for each age category
    indfor$ncar1 <- count_car(data$const, expect_op, thresh, indfor$car1);
    indfor$ncar2 <- count_car(data$const, expect_op, thresh, indfor$car2);
    indfor$ncar3 <- count_car(data$const, expect_op, thresh, indfor$car3);
    
    data$pred_ncar <- indfor$ncar1 + indfor$ncar2 + indfor$ncar3;
    data$pred_ncar[data$pred_ncar > 3] <- 3;
    
    return(indfor[c("ncar1","ncar2","ncar3")])
  }
  tt1 <- car_count_model(data_temp, indfor, coef_names_car, coefs_car);
  indfor <- cbind(indfor,tt1);
  rm(tt1);
  
  # 6.2 Number of vans
  van_count_model <- function(data, indfor, coef_names, coef_values) {
    
    ### Dummy variables for Ordered Probit model 
    data$yvan0_5 <- indfor$van1;
    data$yvan0_5 [data$yvan0_5 > 0] <- 1;
    data$yvan6_11 <- indfor$van2;
    data$yvan6_11 [data$yvan6_11 > 0] <- 1;
    data$yvan_12 <- indfor$van3;
    data$yvan_12 [data$yvan_12 > 0] <- 1;
    data$vanmile1 <- (indfor$van1 + indfor$van2 + indfor$van3)/10000;
    
    ### Ordered probit utility computation for van
    #data$const <- 1;
    data$cons <- coef_values[1]*data$const;
    
    expect_op <- -data[coef_names[1]] * coef_values[1];
    for (i in 2:(length(coef_names))) {
      expect_op <- expect_op + data[coef_names[i]] * coef_values[i];
    }
    expect_op <- as.numeric(expect_op$const);
    
    ### Monte Carlo simulation for each age category
    indfor$nvan1 <- count_veh(data$const, expect_op, indfor$van1);
    indfor$nvan2 <- count_veh(data$const, expect_op, indfor$van2);
    indfor$nvan3 <- count_veh(data$const, expect_op, indfor$van3);
    
    data$pred_nvan <- indfor$nvan1 + indfor$nvan2 + indfor$nvan3;
    data$pred_nvan[data$pred_nvan > 2] <- 2;
    return(indfor[c("nvan1","nvan2","nvan3")]);
  }
  
  tt1 <- van_count_model(data_temp, indfor, coef_names_van, coefs_van);
  indfor <- cbind(indfor,tt1);
  rm(tt1);
  
  # 6.3 Number of SUVs
  SUV_count_model <- function(data, indfor, coef_names, coef_values) {
    
    ### Dummy variables for Ordered Probit model 
    data$ysuv0_5 <- indfor$suv1;
    data$ysuv0_5 [data$ysuv0_5 > 0] <- 1;
    data$ysuv6_11 <- indfor$suv2;
    data$ysuv6_11 [data$ysuv6_11 > 0] <- 1;
    data$ysuv_12 <- indfor$suv3;
    data$ysuv_12 [data$ysuv_12 > 0] <- 1;
    data$suvmile1 <- (indfor$suv1 + indfor$suv2 + indfor$suv3)/10000
    
    ### Ordered probit utility computation for SUV
    #data$const <- 1;
    data$cons <- coef_values[1]*data$const;
    
    expect_op <- -data[coef_names[1]] * coef_values[1];
    for (i in 2:(length(coef_names))) {
      expect_op <- expect_op + data[coef_names[i]] * coef_values[i];
    }
    expect_op <- as.numeric(expect_op$const);
    
    
    ### Monte Carlo simulation for each age category
    indfor$nsuv1 <- count_veh(data$const, expect_op, indfor$suv1);
    indfor$nsuv2 <- count_veh(data$const, expect_op, indfor$suv2);
    indfor$nsuv3 <- count_veh(data$const, expect_op, indfor$suv3);
    
    
    data$pred_nsuv <- indfor$nsuv1 + indfor$nsuv2 + indfor$nsuv3;
    data$pred_nsuv[data$pred_nsuv > 2] <- 2;
    
    return (indfor[c("nsuv1","nsuv2","nsuv3")]);
  }
  
  tt1 <- SUV_count_model(data_temp, indfor, coef_names_suv, coefs_suv);
  indfor <- cbind(indfor,tt1);
  rm(tt1);
  
  # 6.4 Number of pickups
  pickup_count_model <- function(data, indfor, coef_names, coef_values) {
    
    ### Dummy variables for Ordered Probit model 
    data$ypickup0_5 <- indfor$pickup1
    data$ypickup0_5 [data$ypickup0_5 > 0] <- 1
    data$ypickup6_11 <- indfor$pickup2
    data$ypickup6_11 [data$ypickup6_11 > 0] <- 1
    data$ypickup_12 <- indfor$pickup3
    data$ypickup_12 [data$ypickup_12 > 0] <- 1
    data$pickupmile1 <- (indfor$pickup1 + indfor$pickup2 + indfor$pickup3)/10000
    
    ### Ordered probit utility computation for Pick-up
    #data$const <- 1
    data$cons <- coef_values[1]*data$const
    expect_op <- -data[coef_names[1]] * coef_values[1]
    for (i in 2:(length(coef_names))) {
      expect_op <- expect_op + data[coef_names[i]] * coef_values[i]
    }
    expect_op <- as.numeric(expect_op$const)
    
    ### Monte Carlo simulation for each age category
    indfor$npickup1 <- count_veh(data$const, expect_op, indfor$pickup1);
    indfor$npickup2 <- count_veh(data$const, expect_op, indfor$pickup2);
    indfor$npickup3 <- count_veh(data$const, expect_op, indfor$pickup3);
    
    data$pred_npick <- indfor$npickup1 + indfor$npickup2 + indfor$npickup3;
    data$pred_npick[data$pred_npick > 2] <- 2;
    
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
  
  vehicles <- rbind(car1, car2, car3, van1, van2, van3, suv1, suv2, suv3, pickup1, pickup2, pickup3)
  
  # LJ add 9/14/2021 error handling when there is no vehicles (except for motor bike, which is not included in here)
  if(is.null(vehicles)){ # if none of the vehicles exists, return a NULL dataframe
    return(NULL)
  }else{
    vehicles <- vehicles %>% group_by(household_id) %>% dplyr::mutate(vehicle_id= 1:n()) %>% ungroup()
    
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
      
      # a=as.numeric(Sys.time()) # LJ add
      # set.seed(a)
      # 
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
    tt1 <- powertrain_model(vehicles, coef_names_power, coef_values_power);
    vehicles <- cbind(vehicles,tt1);
    rm(tt1);
    
    # 8. Main driver
    vehicles$power_ev <-  0
    vehicles$power_ev[vehicles$pred_power!="ICE"] <- 1
    vehicle_person <- vehicles %>% merge(persons, by = "household_id")
    
    ## Generate Interaction Term
    vehicle_person <- vehicle_person %>% dplyr::mutate(power_age=power_ev*R_AGE_IMP, van_age=van*R_AGE_IMP, suv_age=suv*R_AGE_IMP, pickup_age=pickup*R_AGE_IMP,
                                                power_sex=power_ev*R_SEX_IMP, van_sex=van*R_SEX_IMP, suv_sex=suv*R_SEX_IMP, pickup_sex=pickup*R_SEX_IMP,
                                                power_high=power_ev*high, van_high=van*high, suv_high=suv*high, pickup_high=pickup*high,
                                                power_coll=power_ev*college, van_coll=van*college, suv_coll=suv*college, pickup_coll=pickup*college,
                                                power_grad=power_ev*graduate, van_grad=van*graduate, suv_grad=suv*graduate, pickup_grad=pickup*graduate,
                                                power_school=power_ev*school, van_school=van*school, suv_school=suv*school, pickup_school=pickup*school,
                                                power_retired=power_ev*retired_person, van_retired=van*retired_person, suv_retired=suv*retired_person, pickup_retired=pickup*retired_person,
                                                power_full=power_ev*work, van_full=van*work, suv_full=suv*work, pickup_full=pickup*work,
                                                power_black=power_ev*black, van_black=van*black, suv_black=suv*black, pickup_black=pickup*black,
                                                power_asian=power_ev*asian, van_asian=van*asian, suv_asian=suv*asian, pickup_asian=pickup*asian,
                                                power_rother=power_ev*race_other, van_rother=van*race_other, suv_rother=suv*race_other, pickup_rother=pickup*race_other)
    
  
    #### Predict who is the main driver
    vehicle_person <- cbind(vehicle_person, predict(maindriver_logit3, vehicle_person, type='response'))
    names(vehicle_person)[ncol(vehicle_person)] <- "pred_driver"
    
    vehicle_person <- vehicle_person %>% group_by(household_id, vehicle_id) %>% dplyr::slice(which.max(pred_driver))
    vehicle_person <- vehicle_person[, c("household_id", "vehicle_id", "person_id")]
    names(vehicle_person)[match("person_id", names(vehicle_person))] <- "maindriver_id"
  
    ### Result of Vehicle Level Data
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
    vehicles <- vehicles %>% dplyr::mutate(VEHAGE0=case_when(VEHAGE==2.5~1, TRUE~0),
                                    VEHAGE1=case_when(VEHAGE==8.5~1, TRUE~0),
                                    VEHAGE2=case_when(VEHAGE==14.5~1, TRUE~0),
                                    bev=case_when(pred_power=="AEV"~1, TRUE~0),
                                    hybrid=case_when(pred_power=="Hybrid"~1, TRUE~0),
                                    phev=case_when(pred_power=="PHEV"~1, TRUE~0))
    #### Predict own or lease
    vehicles <- cbind(vehicles, predict(ownlease.static, vehicles, type='response'))
    names(vehicles)[ncol(vehicles)] <- "pred_ownlease"
    
    # a=as.numeric(Sys.time()) # LJ add
    # set.seed(a)
    
    rnum <- runif(nrow(vehicles))
    rnum[rnum < vehicles$pred_ownlease] <- 1
    rnum[rnum !=1 ] <- 0
    vehicles <- cbind(vehicles, rnum)
    vehicles["pred_own"] <- NULL
    names(vehicles)[ncol(vehicles)] <- "pred_own"
    
    return(vehicles)
  }
}
