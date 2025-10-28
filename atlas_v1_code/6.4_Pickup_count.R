# This code predicts the count of Pick-ups for each bodytype-age category in a household 
# This code takes inputs from '4_Heuristic_mileage_reallocation.r'
# Output of code is written to 'VFC_output.csv'

### Monte Carlo Simulation code used from '6.2_Van_count.r' 

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
