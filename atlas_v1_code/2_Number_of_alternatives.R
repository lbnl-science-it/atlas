# This codes predicts the number of vehicle alternatives owned by each household
# Output (number of vehicle alternatives) of this code goes in as input to '4_Heuristic_mileage_reallocation.r'

# Coef 2 preparation
coef2_prepare <- function(coef2, num_alt){
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
  values <- list(coefs_0veh, coefs_1veh, coefs_2veh, coefs_3veh, coefs_4veh, coefs_5veh);
  
  coef_veh <- list(names, values)
  return(coef_veh);
}


##### Utility computation function #####
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

NumAlternative_model <- function(data1, coefs_names, coefs_vehs) {
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
  
  data1$lif_cyc_1 <- 0;
  data1$lif_cyc_1[data1$LIF_CYC == 4] <- 1;
  data1$lif_cyc_2 <- 0;
  data1$lif_cyc_2[data1$LIF_CYC == 8] <- 1;
  
  num_alt <- 6;
  
  ### Utility computation 
  util_one_veh <- utility(coefs_name_0veh, coefs_0veh, data1);
  util_two_veh <- utility(coefs_name_1veh, coefs_1veh, data1);
  util_three_veh <- utility(coefs_name_2veh, coefs_2veh, data1);
  util_four_veh <- utility(coefs_name_3veh, coefs_3veh, data1);
  util_five_veh <- utility(coefs_name_4veh, coefs_4veh, data1);
  util_six_veh <- utility(coefs_name_5veh, coefs_5veh, data1);
  
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
  NumHousehold <- nrow(data1);
  rnum <- runif(NumHousehold);
  rnum[rnum < cum_prob_one_veh] <- 2;
  rnum[rnum < cum_prob_two_veh] <- 3;
  rnum[rnum < cum_prob_three_veh] <- 4;
  rnum[rnum < cum_prob_four_veh] <- 5;
  rnum[rnum < cum_prob_five_veh] <- 6;
  rnum[rnum <= 1] <- 7;
  data1$pred_own <- rnum - 2;
  
  tmp <- data1[c("pred_own")];
  return(tmp);
}





