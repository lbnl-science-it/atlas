# This code predicts the number of different body types owned by a household  for any given horizon year 
# and compares it to the bodytype distribution provided by the hueristic mileage reallocation algorithm
# This code takes input from '4_Heuristic_mileage_reallocation.r' 
# and checks it against a predicted body type distribution for user set tolerance limits

# Coef 5 preparation
coef5_prepare <- function(coef5, num_type){
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

NumBodytypes <- function(data, indfor, bothnum, bothmile, coefs_names, coefs_type, diff_body) {
  
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
  
  data$const <- 1;
  
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
  while (i <= 5) { # change diff_body to explicit argument
    diff_body[i] <- abs((sum(data$pred_body_HMR == (i-1)) - sum(data$pred_body_MNL == (i-1)))/NumHousehold);
    i <- i+1;
  }
  return(list(data[c("pred_body_HMR","pred_body_MNL")], diff_body));
}
