# This code predicts the annual milage budget for each household in the dataset
# For base year, this is compared with the observed annual mileage distribution
# Output (annual milage budget) from this code goes is in as input to '3_Vehicle_fleet_mix.r'

mile_category <- function(x, act_value, cat_value) {
  
  for (i in x)
  {
    
    if(act_value[i] <= 10000){
      cat_value[i] <- 0;
    } else{
      if(act_value[i] <= 20000){
        cat_value[i] <- 1;  
      }
      else{
        if(act_value[i] <= 30000){
          cat_value[i] <- 2;
        }
        else{
          if(act_value[i] <= 40000){
            cat_value[i] <- 3;
          }
          else{
            if(act_value[i] <= 50000){
              cat_value[i] <- 4;
            }
            else{
              if(act_value[i] <= 60000){
                cat_value[i] <- 5;
              }
              else{
                if(act_value[i] <= 70000){
                  cat_value[i] <- 6;
                }
                else{
                  if(act_value[i] <= 80000){
                    cat_value[i] <- 7;
                  }
                  else{
                    if(act_value[i] <= 90000){
                      cat_value[i] <- 8;
                    }
                    else{
                      if(act_value[i] <= 100000){
                        cat_value[i] <- 9;
                      }
                      else{
                        cat_value[i] <- 10
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  } 
  
  return (cat_value);
}

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
  
  # isVehicle <- data$ncar + data$nvan + data$nsuv + data$npickup + data$nmotorbike;
  # isVehicle[isVehicle > 0] <- 1;
  isVehicle <- 1
  data$budget <- (expect_op * isVehicle) + data$HHSIZE * 0.5 * 365;
  
  tmp <- data[c("const","budget")];
  return (tmp);
}
