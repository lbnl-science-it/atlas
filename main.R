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

useparser = F  # currently we do not use the parser for directory and year input
# useparser = T  # we do want to use cmd argument parsing now -Tin

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

    # LJ add 9/21/2022, option to run static or dynamic
    make_option(c("--mod"), dest="mod", action="store", help="static (1) or dynamic (2) ", default="1" )


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

  # read option of whether read from beam derived accessibility, 0, 1
  beamac = strtoi(opt$beamac, base=10)

  atlas_runmod = strtoi(opt$mod, base=10) # 'static 1' or 'dynamic 2'

  warmTF = strtoi(opt$warmstart, base=10) # 1=warmstart run, 0=evolution run




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
  # codedir = '/Volumes/GoogleDrive/Shared drives/Shared_ATLAS/fromLing/SoftwareDevelopment/sfb_atlas_v2/code_inside_container'  # Note that R is launched from the "code_inside_container" folder
  # codedir = '/mnt/data2/ljin/sfb_atlas_v2/code_inside_container' # if on gems instance
  # codedir = '/mnt/code_inside_container'  # Note that R is launched from the "code_inside_container" folder
  # codedir = '/'
  # Global dir and variables
  # these are best set as command line arguments to main.R via the optparse above
  # basedir = '/Volumes/GoogleDrive/Shared drives/Shared_ATLAS/fromLing/SoftwareDevelopment/sfb_atlas_v2'
  # basedir = '/mnt/data2/ljin/sfb_atlas_v2'
  #  basedir = '/mnt/'
  basedir <- getwd()
  codedir <- "."
  inputdir <- file.path(basedir,'atlas_input')
  outputdir <- file.path(basedir, 'atlas_output')


  outputyear <- 2017
  simyear = outputyear

  nsample = 0 # 0- full sample, >0 subsample
  Npe = 4 # number of processors

  beamac = 0 # read from observed job accessibility


  atlas_runmod = 2 # '1-static' or '2-dynamic'

  setwd(codedir)

  iniyear = 2017 # default initialization year
                 # Austin need to change this to 2018

  if(atlas_runmod == 2 & nsample !=0){
    stop('Error - atlas run mode 2: dynamic evolution should only run on full sample')
  }

}
###########################################
#     DO NOT MODIFY BELOW !
###########################################

debugTF = F

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




