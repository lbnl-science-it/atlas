#!/usr/bin/Rscript


###########################
# the main code
###########################


# the following code has been tested on atlas-test instance in the R container environment

# Tin, please note I used the following before testing the code 
#docker pull     ghcr.io/lbnl-science-it/atlas:main

# docker run -v /home/ubuntu/AWS/PILATES/pilates/atlas:/mnt -it --entrypoint=bash  ghcr.io/lbnl-science-it/atlas:main


# command line parse to setup the i/o dir and year to run --------------#

# Note that preceding this docker call, the preprocessing py code already took .h5 output from urbansim 
# and extracted various data tables into .csv file

#useparser = F  # currently we do not use the parser for directory and year input
useparser = T  # we do want to use cmd argument parsing now -Tin

if(useparser){  
  suppressPackageStartupMessages(library("optparse"))
  
  option_list <- list( 
    make_option(c("--indir"),  dest="inputdirPath",  action="store", help="path to input  data", default="/atlas_input" ),
    make_option(c("--outdir"), dest="outputdirPath", action="store", help="path to output data", default="/atlas_output" ),
    make_option(c("--basedir"), dest="basedirPath", action="store", help="dir where pilates/orchestrator is located", default="/" ),
    make_option(c("--codedir"), dest="codedirPath", action="store", help="base dir where R code is located", default="/" ),
    make_option(c("--outyear"), dest="outputyear", action="store", help="output year", default="2017" ),
    make_option(c("--freq"), dest="freq", action="store", help="simulation interval", default="1" ),
    make_option(c("--nsample"), dest="nsample", action="store", help="subsample of hh to process, 0 if all hh", default= "0" ),
    make_option(c("--npe"), dest="npe", action="store", help="number of cores for parallel computing", default="9" ) # number of cores to use in parallel run
    
    
  )
  # input year is previous year in urbansim output, output year is the current year in urbansim output
  # note, static model directly predicts output year without relying on previous year (i.e. inputyear)
  
  opt <- parse_args(OptionParser(option_list=option_list))
  
  showDebug = 1
  if( showDebug ) {
    print( "Hello World from R! (rel:2022.0118.2125)" )
    print( "** input  directory specified as, and content:") 
    print( opt$inputdirPath )
    print( list.files( opt$inputdirPath, recursive=TRUE ) )
    print( "** output directory specified as, and content:")
    print( opt$outputdirPath )
    print( list.files( opt$outputdirPath, recursive=TRUE ) )
    print( "** codedir  directory specified as:") 
    print( opt$codedirPath )
    print( "** basedir  directory specified as:") 
    print( opt$basedirPath )
    print( "** outputyear, freq specified as:") 
    print( opt$outputyear )
    print( opt$freq )
    print( "** number of clusters for parallel computing")
    
    print( "sample of households to process")
    if(opt$nsample == 0){ print('full sample')}else{print(opt$nsample)}
  }
	  
  # read out the global variables so that subsequent programs can all use them
  
  basedir = opt$basedirPath
  codedir = opt$codedirPath
  inputdir = opt$inputdirPath  # the mounting point
  outputdir = opt$outputdirPath # the mounting point
  outputyear    = strtoi(opt$outputyear, base=10)
#  inputyear = outputyear - freq  # this variable will be used for next version of atlas
#  freq  = strtoi(opt$freq, base=10) # this variable will be used for next version of atlas
  nsample = strtoi(opt$nsample, base=10) # number of households to subsample
  Npe = strtoi(opt$npe, base=10) # number of processors to use

  print( "basedir and codedir parsed as:") 
  print( basedir )
  print( codedir )
  
}

######################################################################################################
#-------- Below is Ling's code with inputdir, outputdir, inputyear, outputyear manually defined -----#
# can be modified 
if(!useparser){ # if not using parser, define things here for debuging process
#  codedir = '~/Dropbox/Research/SmartGrid_Behavioral/TransportationInitiative/ATLAS/Software_Development/AWS/PILATES/pilates/atlas/code_inside_container'  # Note that R is launched from the "code_inside_container" folder
 codedir = '/mnt/code_inside_container'  # Note that R is launched from the "code_inside_container" folder
 codedir = '/'
  # Global dir and variables
  # these are best set as command line arguments to main.R via the optparse above
#  basedir = '~/Dropbox/Research/SmartGrid_Behavioral/TransportationInitiative/ATLAS/Software_Development/AWS/PILATES/pilates/atlas'
  basedir = '/mnt/'
  inputdir <- file.path(basedir,'atlas_input')
  outputdir <- file.path(basedir, 'atlas_output')
  
  
  outputyear <- 2010
  nsample = 20000
  Npe = 2
  
}
###########################################
#     DO NOT MODIFY BELOW !
###########################################

# 1. set up 

setwd(codedir)

library(stats) # LJ add, predict function is used for predicting main driver and ownlease
library(tidyverse)
library(dplyr)
library(apollo)
library(tictoc)


# Note currently the static model predicts output year directly, 
#no dependencies on input year (i.e. previous year)
if(outputyear <2015){diryear = 2010}else{diryear = 2017} # 2010-2014 use 2010 data and model, 2015- use 2017 mode

# 2. generate variables --------------#

source(paste0('data_clean_',diryear, '.R')) 

# 3. prepare coefs and control variables

source(paste0('Model_application_',diryear, '.R')) 


# 4. model run

library(parallel)
library(doParallel)
library(foreach)

registerDoParallel(cores = Npe) 

Nloop.max = floor(nrow(hh.masterdat)/10000)
print(paste('pop max loop',Nloop.max))


# determine sample of households to process
if(nsample > nrow(hh.masterdat)){stop('requested number of households exceeds the max number of existing households')}

if(nsample == 0){
  print('processing the full population')
  Nloop = Nloop.max # full sample
  hh.dat = hh.masterdat
}else{ # if 0 < nsample < max hh number
  print(paste('processing subsample',nsample,'hh'))
  Nloop = floor(nsample/10000)
  print(paste('actual Nloop',Nloop))
  set.seed(4847384) # so that it is reproducible
  hh.dat = hh.masterdat %>% sample_n(nsample)
}


if(Nloop == 0){ # nsample less than 10000 hh, direct compute
  print('less than 10000 households, using a serial run')
  data1 = hh.dat
  persons <- data1 %>% dplyr::select(household_id) %>% merge(pp.masterdat, by="household_id")
  res = model_application(persons, data1, coefs_name_mile, coefs_mile , coef_names_veh, coef_values_veh, 
                    coef_names_type, coef_values_type, coef_names_car, coefs_car,
                    coef_names_van, coefs_van, coef_names_suv, coefs_suv, coef_names_pick, coefs_pick,
                    coef_names_power, coef_values_power)
}else { # more than 10000
  tic()
  res <- foreach(i=1:Nloop, 
                 .combine=rbind,
                 .packages = c('dplyr','tidyr')
  )  %dopar% {
    
    if(i<Nloop){
      data1 <- hh.dat[(1+(i-1)*10000):(i*10000),]
    }else{
      data1 <- hh.dat[(1+(Nloop-1)*10000):nrow(hh.dat),]
    }
    #rm(households)
    print(paste('loop',i))
    pp.tmp <- data1 %>% dplyr::select(household_id) %>% merge(pp.masterdat, by="household_id")
    model_application(pp.tmp, data1, coefs_name_mile, coefs_mile , coef_names_veh, coef_values_veh, 
                      coef_names_type, coef_values_type, coef_names_car, coefs_car,
                      coef_names_van, coefs_van, coef_names_suv, coefs_suv, coef_names_pick, coefs_pick,
                      coef_names_power, coef_values_power)
  }
  toc()
  
}

stopImplicitCluster()

# now reformat the results and write out tables
print('reformat results into vehicle and household tables and write out')
households_output <- res %>% select(household_id, budget) %>% group_by(household_id) %>% summarise(nvehicles=n(), budget=mean(budget))
households_output <- hh.dat %>% merge(households_output, by="household_id", all.x = T) %>% select(household_id, nvehicles, budget)
households_output$nvehicles[is.na(households_output$nvehicles)==T] <- 0
households_output$budget[is.na(households_output$budget)==T] <- 0

vehicles_output <- res %>% select(household_id, vehicle_id, VEHAGE:pred_own) %>%
  mutate(bodytype=case_when(car==1~"car", van==1~"van", suv==1~"suv", pickup==1~"pickup", T~"others"),
         vintage_category=case_when(VEHAGE0==1~"0~5 years", VEHAGE1==1~"6~11 years", VEHAGE2==1~"12+ years"),
         ownlease=case_when(pred_own==1~"own", T~"lease")) %>% select(household_id, vehicle_id, bodytype, vintage_category,
                                                                      maindriver_id, annual_mileage, pred_power, ownlease)


write.csv(vehicles_output, file = file.path(outputdir, paste0('vehicles_',outputyear,'.csv')), row.names = F) # vehicle level prediction
write.csv(households_output, file = file.path(outputdir, paste0('householdv_',outputyear,'.csv')),row.names = F) # houshold level prediction


