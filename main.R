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
    make_option(c("--basedir"), dest="basedirPath", action="store", help="dir where pilates/orchestrator is located", default="~/Dropbox/Research/SmartGrid_Behavioral/TransportationInitiative/ATLAS/Software_Development/AWS/PILATES" ),
    make_option(c("--codedir"), dest="codedirPath", action="store", help="base dir where R code is located", default="/opt/gitrepo/atlas" ),
    make_option(c("--outyear"), dest="outputyear", action="store", help="output year", default="2017" ),
    make_option(c("--freq"), dest="freq", action="store", help="simulation interval", default="1" ), # only yearly runs are enabled currently
    make_option(c("--nsample"), dest="nsample", action="store", help="subsample of hh to process, 0 if all hh", default= "0" ), # for atlas_v2, all hhs needs to be processed
    make_option(c("--npe"), dest="npe", action="store", help="number of cores for parallel computing", default="9" ), # number of cores to use in parallel run
    
    # LJ add 5/31/2022, option to read zscore of job accessibility by transit from beam, 1- true, 0- false, will read from the observed data
    make_option(c("--beamac"), dest="beamac", action="store",
                help="indicator of whether to read from beam derived zscore of job accessibility by transit", default="0" ) ,
    
    # LJ add 9/21/2022, option to run static or dynamic LJ4/26/2024 change default to 2
    make_option(c("--mod"), dest="mod", action="store", help="static (1) or dynamic (2) ", default="2" ),
    
    # LJ add 3/28/2023, option to select from adopt scenario folder to read the adopt inputs
    # current available options: "baseline", "ess_cons", "zev_mandate", "example"
    make_option(c("--adscen"),  dest="adscen",  action="store", help="path to subfolder of adopt for scenario inputs", default= "baseline" ),
    
    # LJ add 3/28/2023, option to select a multiplier separately for rebate and tax_credit for sensitivity analysis
    # default value is 0, i.e. no purchasing incentives
    make_option(c("--rebfactor"), dest="rebfactor", action="store", help="multiplier for cash rebate", default="0" ),
    make_option(c("--taxfactor"), dest="taxfactor", action="store", help="multiplier for tax credit", default="0" ),
    
    # LJ add 6/20/2023. option to discount the incentive dollars
    make_option(c("--discIncent"), dest="discIncent", action="store", help="whether to discount incentive dollars (1=T, 0=F)", default="0" ),
    
    # LJ add 4/26/2024. option to output intermediate output for diagnosis
    make_option(c("--midout"), dest="midout", action="store", help="whether to output intermediate outputs (1=T, 0=F)", default="0" )
    
    
  )
  # output year is the current year in urbansim output, for atlas_v2, prediction of output year will use vehicle predictions from previous years
  # note, static model directly predicts output year without relying on previous years
  
  opt <- parse_args(OptionParser(option_list=option_list))
  
  
  print( "input  directory specified as:")
  print( opt$inputdirPath )
  print( "--" )
  print( "output directory specified as:")
  print( opt$outputdirPath )
  print( "--" )
  print( "codedir  directory specified as:")
  print( opt$codedirPath )
  print( "--" )
  print( "basedir  directory specified as:")
  print( opt$basedirPath )
  print( "--" )
  print( "outputyear, freq specified as:")
  print( opt$outputyear )
  print( opt$freq ) # atlas can run frequency of 1 or 2. and this option is not used in the code yet as usim is running yearly.
  print( "number of clusters for parallel computing")
  print( opt$npe )
  print ( "ATLAS running mode selection: 1-static or 2-dynamic")
  print( opt$mod )
  
  print( "ADOPT scenario folder used is ")
  print(opt$adscen)
  
  print("multiplier applied to cash rebate incentive to vehicle purchasing")
  print(opt$rebfactor)
  
  print("multiplier applied to tax credit incentive to vehicle purchasing")
  print(opt$taxfactor)
  
  print('whether the incentives are discounted to the 2019 dollars')
  print(opt$discIncent)
  
  print('whether to output intermediate outputs')
  print(opt$midout)
  
  
  print( "sample of households to process. \nCaution: only works for static version, dynamic version will always use full population")
  if(opt$nsample == 0){ print('full sample')}else{print(opt$nsample)}
  
  # read out the global variables so that subsequent programs can all use them
  
  basedir = opt$basedirPath
  codedir = opt$codedirPath
  inputdir = opt$inputdirPath  # the mounting point
  outputdir = opt$outputdirPath # the mounting point
  outputyear    = strtoi(opt$outputyear, base=10)
  nsample = strtoi(opt$nsample, base=10) # number of households to subsample
  Npe = strtoi(opt$npe, base=10) # number of processors to use
  
  adscen = opt$adscen # adopt scenario folder
  rebfactor = strtoi(opt$rebfactor, base = 10) # multiplier to cash rebate
  taxfactor = strtoi(opt$taxfactor, base = 10) # multiplier to tax credit
  discIncent = strtoi(opt$discIncent, base = 10) # 0/1 value to determine whether discount the incentive dollars
  midout = strtoi(opt$midout, base = 10) # 0/1 value to determine whether to output intermidiate files
  
  # read option of whether read from beam derived accessibility, 0, 1
  beamac = strtoi(opt$beamac, base=10)
  
  atlas_runmod = strtoi(opt$mod, base=10) # 'static 1' or 'dynamic 2'
  
  # warmTF = strtoi(opt$warmstart, base=10) # 1=warmstart run, 0=evolution run
  
  
  
  
  print( "basedir and codedir parsed as:")
  print( basedir )
  print( codedir )
  
  setwd(codedir)
  
  
  if(atlas_runmod == 2 & nsample !=0){
    stop('Error - atlas run mode 2: dynamic evolution should only run on full sample')
  }
  
  # region specific constants here
  iniyear = 2017 # default initialization year for SF
  # Austin need to change this to 2018
  
}

######################################################################################################
#-------- Below is Ling's code with inputdir, outputdir, inputyear, outputyear manually defined -----#
# can be modified
if(!useparser){ # if not using parser, define things here for debuging process
  codedir = '/mnt/data2/ljin/sfb_atlas_v2/code_inside_container' # if on gems instance
  # codedir = '/mnt/code_inside_container'  # Note that R is launched from the "code_inside_container" folder
  # codedir = '/'
  # Global dir and variables
  # these are best set as command line arguments to main.R via the optparse above
  #  basedir = '/Volumes/GoogleDrive/Shared drives/Shared_ATLAS/fromLing/SoftwareDevelopment/sfb_atlas_v2'
  basedir = '/mnt/data2/ljin/sfb_atlas_v2'
  #  basedir = '/mnt/'
  inputdir <- file.path(basedir,'atlas_input')
  outputdir <- file.path(basedir, 'atlas_output')
  
  
  outputyear <- 2021 # only place to change
  simyear = outputyear
  
  nsample = 0 # 0- full sample, >0 subsample
  Npe = 40 # number of processors
  
  beamac = 0 # read from observed job accessibility
  
  
  atlas_runmod = 2 # '1-static' or '2-dynamic'
  
  setwd(codedir)
  
  iniyear = 2017 # default initialization year 
  # Austin need to change this to 2018
  
  if(atlas_runmod == 2 & nsample !=0){
    stop('Error - atlas run mode 2: dynamic evolution should only run on full sample')
  }
  
  adscen = "baseline" # adopt scenario folder
  rebfactor = 1 # multiplier to cash rebate
  taxfactor = 1 # multiplier to tax credit
  
  
}
###########################################
#     DO NOT MODIFY BELOW !
###########################################

debugTF = F
if(midout == 0){midout = F}else{midout = T} # whether output intermediate files

if(atlas_runmod == 1) {
  
  # 1. set up
  library(stats) # LJ add, predict function is used for predicting main driver and ownlease
  library(tidyverse)
  library(dplyr)
  library(apollo)
  library(tictoc)
  library(parallel)
  library(doParallel)
  library(foreach)
  library(data.table)
  
  source('source_atlas_v1.R')
  
} # static version of atlas, always run atlas_v1

if(atlas_runmod == 2){
  
  
  # # ---- global variables, will be moved to 'global_vars.R' after testing ------------- #
  # # note that I changed some notation
  # iniyear = 2017 # default initialization year
  
  if(outputyear <= iniyear){ # for <=2017, use static initialization
    library(stats) # LJ add, predict function is used for predicting main driver and ownlease
    library(tidyverse)
    library(dplyr)
    library(apollo)
    library(tictoc)
    library(parallel)
    library(doParallel)
    library(foreach)
    library(data.table)
    
    source('source_atlas_v1.R')
    
    if(outputyear == iniyear){ # for the initial year (2017) of dynamic evolution,
      # map variable name back to names used subsequently by dynamic prediction
      # save to input folder (as these will be used as inputs for vehicle evolution)
      
      source('clean_dynamic_data_fromv1.R')
      
    } # additional cleaning for initial year outputs and inputs (for SFB it is 2017)
  }else{ # for years >iniyear
    simyear = outputyear
    
    if(((simyear - iniyear)%/%2)*2 - (simyear - iniyear) == 0){
      # evolution year
      evoTF = T
      baseyear = simyear - 2 # current wave should be 2 years ago
    }else{
      evoTF = F
      baseyear = simyear - 1 # current wave should be 1 year ago
    }# mid year
    
    if(!evoTF){
      # if it is the simyear is the mid year
      print(paste('year', simyear, 'is a mid year, not a evolution year.'))
      print('proceed with non-evolution prediction.')
      
      # 1. clean data
      print('cleaning the mid year demo data')
      source('clean_for_mid_prediction.R')
      
      
      # 2. setup: loading global variables and setup function
      
      print('setup mid year prediction')
      source('setup_midyr_prediction.R')
      
      # 3. run prediction for mid year
      print('run mid year prediction')
      source('run_midyr_prediction.R')
      
      # 4. reset colnames and save csv
      print('output csvs')
      source('reset_name_save_csv.R')
      
    }else{ # else if evolution year, i.e. need to predict from baseyear = simyear -2
      
      print(paste('year', simyear, 'is an evolution year, evolve the fleet in this time step'))
      evoyear = simyear
      
      # 1. clean data
      print('clean data for vehicle evolution')
      source('clean_for_evo_prediction.R')
      
      # for debugging purpose, we skip above cleaning step and load the data directly
      if(debugTF){
        load(file = file.path(inputdir, paste0('year',evoyear),'households0.RData')) #households0,
        load(file = file.path(inputdir, paste0('year',evoyear),'households1.RData')) # households1
        load(file = file.path(inputdir, paste0('year',evoyear),'persons1.RData')) # persons1,
        load(file = file.path(inputdir, paste0('year',evoyear),'persons0.RData')) #persons0,
      }
      
      
      
      
      # 2. setup: loading global variables and setup function
      print('setup evolution year parameters and functions')
      source('setup_evoyr_prediction.R')
      
      # 3. run prediction for evolution year
      print('run evolution year prediction')
      tic()
      source('run_evoyr_prediction.R')
      toc()
      
      #remember to map the adopt vehicle predictions back to atlas vehicle bodytypes so that next round we can use it for vehicle transaction prediction
      
      # 4. reset colnames and save csv
      print('output csvs')
      source('reset_name_save_csv.R')
      
      
      
    }# else if evolution year, i.e. need to predict from baseyear = simyear -2
    
  }# dynamic prediction for year > iniyear
  
  
}# run atlas_v2: for SFB, year <=2017 run static version, after that, dynamic evolve

if(!(atlas_runmod %in% c(1,2))){
  stop('specify atlas run mode: 1 - static, 2 - dynamic')
}









