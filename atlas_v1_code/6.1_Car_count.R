# This code predicts the count of cars for each bodytype-age category in a household 
# This code takes input from '4_Heuristic_mileage_reallocation.r'
# Output of code is written to 'VFC_output.csv'


### Function for Monte Carlo Simulation 
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
  
  return(indfor[c("ncar1","ncar2","ncar3")])
}

