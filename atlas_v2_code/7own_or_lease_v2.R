# step4: own or lease prediction

# step 4.1: prepare the data

vehmodepredict$vintage = evoyear - vehmodepredict$deltayear

vehmodepredict <- vehmodepredict[,`acquire_agebin`:=fcase(vintage>=2, ">=2yrs", default = "<=1yrs")][, hybrid:=as.numeric(adopt_fuel=="hybrid")][
  ,phev:=as.numeric(adopt_fuel=="phev")][, `log(income_fu)`:=fcase(is.na(income_fu)==F & income_fu>2, log(income_fu),
                                                                   is.na(income_fu)==T, log(2), default=log(2))][
                                                                     ,hhage_65:=as.numeric((hh_age>=65))]
vehmodepredict <- vehmodepredict[,car:=as.numeric(vehtype=="car")][,suv:=as.numeric(vehtype=="suv")][
  ,van:=as.numeric(vehtype=="van")][,pickup:=as.numeric(vehtype=="pickup")]                                           

vehmodepredict$house_type <- as.numeric(vehmodepredict$house_type)
# step 4.2: predict own or lease
names(ownlease.dynamic$coefficients)[2] <- "acquire_agebin"
vehmodepredict <- cbind(vehmodepredict, stats::predict(ownlease.dynamic, vehmodepredict, type='response')) # LJ add stats::

names(vehmodepredict)[ncol(vehmodepredict)] <- "pred_ownlease"
rnum <- runif(nrow(vehmodepredict))
rnum[rnum < vehmodepredict$pred_ownlease] <- 1
rnum[rnum !=1 ] <- 0
vehmodepredict <- cbind(vehmodepredict, rnum)
vehmodepredict <- vehmodepredict %>% select(-pred_ownlease)
names(vehmodepredict)[ncol(vehmodepredict)] <- "pred_own"
vehmodepredict = vehmodepredict%>% mutate(ownlease = recode(pred_own, '1' = 'own', '0' ='lease'))


