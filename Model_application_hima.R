###################################
###Codes for running application###
###################################
rm(list = ls())

#mywd <- "/Users/lingjin/Dropbox/Research/SmartGrid_Behavioral/TransportationInitiative/ATLAS/Software_Development/urbansim"
mywd <- "/global/data/transportation/ATLAS/static/urbansim/"
#mywd <- "C:/Users/ljin/Dropbox/Research/SmartGrid_Behavioral/TransportationInitiative/ATLAS/Rscripts"
setwd(mywd)

## load packages
library('pacman')
p_load(utils, foreign, pastecs, mlogit, graphics, VGAM, aod, plotrix, Zelig, Zelig, 
       vctrs, maxLik, plyr, MASS, ordinal, mltest, haven, stargazer, stringr, tidyverse)
p_load(gWidgets2, gWidgets2tcltk, miscTools, lmtest, dplyr, BiocManager)
p_load(ggplot2, scales)


## set working folders
current_folder <<- "./model_application/"
Halton_File <<- "./model_application/Halton/nhts2017.csv"
tabdir <- './model_application/output2017bay'
output_file <- paste(c(current_folder, 'output2017bay/VFC_output.csv'),collapse='')
coefdir <- './model_application/coef2017bay'

## load data and global varialbe
load("data_clean/households.RData")
hh.masterdat = households; rm(households)
# load person data for maindriver prediction
load("data_clean/persons.RData")
pp.masterdat = persons; rm(persons)


# # random numbers
# a=as.numeric(Sys.time()) # LJ add
# set.seed(a)
# 
# rseeds = floor(runif(10^7)*10^7)

HaltonSequence = read.table(Halton_File, header=F, sep=",") # LJ change here, make it a global variable.

## import functions for all steps
source(paste(c(current_folder,'Model_application_functions.R'),collapse=''))

## pre-set values
num_alt <- 6  # number of alternatives for step 2 estimation (6+)
num_type <- 5 # number of bodytypes for step 5 estimation (5+)
num_power <- 4 # number of powertrain for step 7 estimation (5+)
controls <- list()
controls[[1]] <- 5   # UNO - One (Column Index)
controls[[2]] <- 6   # SERO - Zero (Column Index)
controls[[3]] <- 7   # Case ID (Column Index)
controls[[4]] <- 25  # Number of Replications
controls[[5]] <- 1   # Number of Outside Goods
controls[[6]] <- 4   # Configuration (1,4,5,6, or 7)

Tolerance <<- 0.07 # 0.035; # Tolerance limit to match predicted bodytype distribution
max_iteration <<- 100 #5; # Maximum number of iterations for the mileage re-allocation algorithm
N_fail <- 0 # number of hh failed converge HMR.
nc <<- 14;

#diff_body <<- c(1.0, 1.0, 1.0, 1.0, 1.0) # LJ change 9/13/2021, this should be initialized within the application function.

## load coefficients and process them [Don't need to modify]
load(file.path(coefdir, "coefs.RData"))

# 1
names(coef1)[1] <- "const"
coefs_name_mile <- names(coef1)
coefs_mile <- unname(coef1)
# 2
coef_names_veh <- coef2_prepare(coef2)[[1]]
coef_values_veh <- coef2_prepare(coef2)[[2]]
# 3
ivmts <- coef3_prepare(coef3, coef_mdcev)[[1]]
ivgts <- coef3_prepare(coef3, coef_mdcev)[[2]]
ivmt14_length <- coef3_prepare(coef3, coef_mdcev)[[3]]
# 5
coef_names_type <- coef5_prepare(coef5)[[1]]
coef_values_type <- coef5_prepare(coef5)[[2]]
# 6
coef_names_car <- names(coef6_1)
coef_names_car[1] <- "const"
coefs_car <- unname(coef6_1)
coef_names_van <- names(coef6_2)
coef_names_van[1] <- "const"
coefs_van <- unname(coef6_2)
coef_names_suv <- names(coef6_3)
coef_names_suv[1] <- "const"
coefs_suv <- unname(coef6_3)
coef_names_pick <- names(coef6_4)
coef_names_pick[1] <- "const"
coefs_pick <- unname(coef6_4)
# 7
coef_names_power <- coef7_prepare(coef7)[[1]]
coef_values_power <- coef7_prepare(coef7)[[2]]



# # test a single line
# data1 <- hh.masterdat[1,]
# #rm(households)
# persons <- data1 %>% dplyr::select(household_id) %>% merge(pp.masterdat, by="household_id")
# 
# tic()
# ## Main application
# vehicles <- model_application(persons, data1, coefs_name_mile, coefs_mile , coefs_names, coefs_vehs, 
#                               ivmts, ivgts, coef_names_type, coef_values_type, coef_names_car, coefs_car,
#                               coef_names_van, coefs_van, coef_names_suv, coefs_suv, coef_names_pick, coefs_pick,
#                               coef_names_power, coef_values_power)
# 
# toc()
# 
# 

# parallel run set up
p_load(snow, foreach, parallel, doParallel, tictoc)
# library(snow)
# library(foreach)
# library(parallel)
# library(doParallel)
# library(tictoc)


# no.cores <- (detectCores() - 1)
# cl <- makeCluster(no.cores, type = "SOCK", outfile="")
# registerDoSNOW(cl)


# registerDoParallel(cores=detectCores(all.tests=TRUE))
# R=100
# 
# tic()
# res <- foreach(i=1:100, 
#                .combine=rbind,
#                .packages = c('dplyr','tidyr'),
#                .export = c(ls()))  %dopar% {
#   data1 <- hh.masterdat[i,]
#   #rm(households)
#   persons <- data1 %>% dplyr::select(household_id) %>% merge(pp.masterdat, by="household_id")
#   vehicles <- model_application(persons, data1, coefs_name_mile, coefs_mile , coefs_names, coefs_vehs, 
#                                 ivmts, ivgts, coef_names_type, coef_values_type, coef_names_car, coefs_car,
#                                 coef_names_van, coefs_van, coef_names_suv, coefs_suv, coef_names_pick, coefs_pick,
#                                 coef_names_power, coef_values_power)
# }
# toc()


## export none
#registerDoParallel(cores=detectCores(all.tests=TRUE)
registerDoParallel(cores=4)


tic()
#res <- foreach(i=1:(floor(dim(hh.masterdat)[1]/10000)+1), 
res <- foreach(i=1:260,
               .combine=rbind,
               .packages = c('dplyr','tidyr')
               )  %dopar% {
                 data1 <- hh.masterdat[(1+(i-1)*20000):min((i*20000),dim(hh.masterdat)[1]),]
                 #rm(households)
                 persons <- data1 %>% dplyr::select(household_id) %>% merge(pp.masterdat, by="household_id")
#                 vehicles <- model_application(persons, data1, coefs_name_mile, coefs_mile , coefs_names, coefs_vehs, 
#                                               ivmts, ivgts, coef_names_type, coef_values_type, coef_names_car, coefs_car,
#                                               coef_names_van, coefs_van, coef_names_suv, coefs_suv, coef_names_pick, coefs_pick,
#                                               coef_names_power, coef_values_power)

                 model_application(persons, data1, coefs_name_mile, coefs_mile , coefs_names, coefs_vehs,
                                               ivmts, ivgts, coef_names_type, coef_values_type, coef_names_car, coefs_car,
                                               coef_names_van, coefs_van, coef_names_suv, coefs_suv, coef_names_pick, coefs_pick,
                                               coef_names_power, coef_values_power)
               }
toc()

save(res, file = './testparallelres.RData')

stopImplicitCluster()


# tic()
# for(i in 1:100){
#   
#   print(i)
# 
# tic()
# data1 <- hh.masterdat[9000:10000,]
# #rm(households)
# persons <- data1 %>% dplyr::select(household_id) %>% merge(pp.masterdat, by="household_id")
# vehicles <- model_application(persons, data1, coefs_name_mile, coefs_mile , coefs_names, coefs_vehs, 
#                               ivmts, ivgts, coef_names_type, coef_values_type, coef_names_car, coefs_car,
#                               coef_names_van, coefs_van, coef_names_suv, coefs_suv, coef_names_pick, coefs_pick,
#                               coef_names_power, coef_values_power)
# toc()
# 
# }
# 
# toc()
# 
# head(hh.masterdat)
# head(pp.masterdat)
# 
# kk = NULL
# i=0
# 
# a=as.numeric(Sys.time()) # LJ add
# set.seed(a)
# seeds = floor(runif(10000)*10000)
# while(i<10000){
#   i = i+1
# set.seed(seeds[i])
# 
# kk = c(kk,runif(1))
# }
# 
# df1 = data.frame(x = kk)
# ggplot(df1, aes(x = x)) + geom_histogram()
#        
# 
# df2 = data.frame(x = runif(10000))  
# ggplot(df2, aes(x = x)) + geom_histogram()
# 
# 
# set.seed(1)
# runif(1)
# rnorm(1)
# 
# # test parallel
# 
# # parallel run set up
# library(snow)
# require(snow)
# require(boot)
# library(foreach)
# library(parallel)
# library(doParallel)
# 
# registerDoParallel(cores=detectCores(all.tests=TRUE))
# R=100
# 
# res <- foreach(i=1:R, 
#                .combine=rbind,
#                .packages = c('dplyr','tidyr'),
#                .export = c(ls())) %dopar% {
#   df=data.frame(x = runif(1), y = runif(1))
# }
