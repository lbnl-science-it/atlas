# This code re-allocates mileages to vehicles in the household based on a 'choice occasion based approach'
# This code takes inputs from
# a. '2_Number_of_alternatives.R'
# b. '3_Vehicle_fleet_mix.R'
# Output form this code is checked for 
# 1. How well it compares with observed average mileage distributions of the 13+1 (outside good) alternative system of bodytype-age combinations
# 2. Frequency distribution of 13+1 (outside good) alternative system 
# 3. How well it compares with predicted body type distribution by the MNL model (for base year or any horizon year).
# This code is controlled by a tolerance limit or a maximum of 5 iterations both of which can be set by the user in the control file
# If the predicted fleet composition is not satisfactory, MDCEV model is re-calibrated and the whole process is repeated again
# Output from this code serves as input to '5_Number_of_bodytypes.r'
# Output of this code (if tolerance criteria are satisfied) serves as input to all the count models 
# Households having non-zero mileages are selected by the respective models
# '6.1_Car_count.r'
# '6.2_Van_count.r'
# '6.3_SUV_count.r'
# '6.4_Pickup_count.r'

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
