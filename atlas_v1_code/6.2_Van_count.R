# This code predicts the count of vans for each bodytype-age category in a household 
# This code takes inputs from '4_Heuristic_mileage_reallocation.r'
# Output of code is written to 'VFC_output.csv'


### Function for Monte Carlo Simulation 
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
