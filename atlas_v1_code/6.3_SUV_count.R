# This code predicts the count of SUVs for each bodytype-age category in a household 
# This code takes inputs from '4_Heuristic_mileage_reallocation.r'
# Output of code is written to 'VFC_output.csv'

### Monte Carlo Simulation code used from '6.2_Van_count.r'

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

