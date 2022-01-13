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

useparser = F  # currently we do not use the parser for directory and year input

if(useparser){  
  suppressPackageStartupMessages(library("optparse"))
  
  option_list <- list( 
    make_option(c("--indir"),  dest="inputdir",  action="store", help="path to input  data", default="tbd" ),
    make_option(c("--outdir"), dest="outputdir", action="store", help="path to output data", default="tbd" ),
    make_option(c("--outyear"), dest="outputyear", action="store", help="output year", default="tbd" ),
    make_option(c("--freq"), dest="freq", action="store", help="simulation interval", default="tbd" )
    
  )
  # input year is previous year in urbansim output, output year is the current year in urbansim output
  # note, static model directly predicts output year without relying on previous year (i.e. inputyear)
  
  opt <- parse_args(OptionParser(option_list=option_list))
  
  
  print( "input  directory specified as:") 
  print( opt$inputdir )
  print( "--" )
  print( "output directory specified as:")
  print( opt$outputdir )
  print( "--" )
  
  # read out the global variables so that subsequent programs can all use them
  
  inputdir = opt$inputdir
  outputdir = opt$outputdir
  outputdir = opt$outputyear
  freq = opt$freq
  inputyear = outputyear - freq
  
}

######################################################################################################
#-------- Below is Ling's code with inputdir, outputdir, inputyear, outputyear manually defined -----#
# can be modified 

# 1. set up 
#codedir = '~/Dropbox/Research/SmartGrid_Behavioral/TransportationInitiative/ATLAS/Software_Development/AWS/PILATES/pilates/atlas/code_inside_container'  # Note that R is launched from the "code_inside_container" folder
codedir = '/mnt/code_inside_container'  # Note that R is launched from the "code_inside_container" folder
setwd(codedir)


# Global dir and variables
#basedir = '~/Dropbox/Research/SmartGrid_Behavioral/TransportationInitiative/ATLAS/Software_Development/AWS/PILATES' 
basedir = '/mnt/' 
inputdir <- file.path(basedir,'atlas_input')
outputdir <- file.path(basedir, 'atlas_output')


outputyear <- 2017
freq <- 1
inputyear <- outputyear - freq

###########################################
#     DO NOT MODIFY BELOW !
###########################################

library(stats) # LJ add, predict function is used for predicting main driver and ownlease
library(tidyverse)
library(dplyr)
library(apollo)
library(tictoc)


# Note currently the static model predicts output year directly, 
#no dependencies on input year (i.e. previous year)
if(outputyear <2015){diryear = 2010}else{diryear = 2017} # 2010-2014 use 2010 data and model, 2015- use 2017 mode

# 2. generate variables --------------#

tic()
source(paste0('data_clean_',diryear, '.R')) 
toc('variable generation')

# 3. prepare coefs and control variables

source(paste0('Model_application_',diryear, '.R')) 


# 4. model run

library(parallel)
library(doParallel)
library(foreach)

registerDoParallel(cores=9)  # for now the number of cores is hardwired. this can be made into a yml setting variable later.

Nloop = floor(nrow(hh.masterdat)/10000)

tic()
res <- foreach(i=1:Nloop, 
               .combine=rbind,
               .packages = c('dplyr','tidyr')
)  %dopar% {
  
  if(i<Nloop){
    data1 <- hh.masterdat[(1+(i-1)*10000):(i*10000),]
  }else{
    data1 <- hh.masterdat[(1+(Nloop-1)*10000):nrow(hh.masterdat),]
  }
  #rm(households)
  print(paste('loop',i))
  persons <- data1 %>% dplyr::select(household_id) %>% merge(pp.masterdat, by="household_id")
  model_application(persons, data1, coefs_name_mile, coefs_mile , coef_names_veh, coef_values_veh, 
                    coef_names_type, coef_values_type, coef_names_car, coefs_car,
                    coef_names_van, coefs_van, coef_names_suv, coefs_suv, coef_names_pick, coefs_pick,
                    coef_names_power, coef_values_power)
}
toc()


stopImplicitCluster()


# 5. write out the results # we may need to add some post processing code here after clarifying the variables with Qianmiao

write.csv(vehicles, file = file.path(outputdir, paste0('vehicles_',outputyear,'.csv')))

