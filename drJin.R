#!/usr/bin/env R

print("Hello World from R! (ref:0906)")


# ########## OPTIONAL: CONFIGURE WORKSPACE ##########

# # set working directory to this script's locations
# source("building_dynamics_functions.R")
# this.dir <- dirname(GetScriptFilepath())
# setwd(this.dir)

# #clear workspace
# rm(list=ls())

#-----------------------------------------------------------------------------#

########### LOAD LIBRARIES ############
packages <- c("raster", "sp", "rgeos", "geosphere", "doParallel", "iterators", "foreach", "rgdal", "plyr", "doSNOW", "openxlsx")

## install librarie that may not already be installed
##? if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
##?   install.packages(setdiff(packages, rownames(installed.packages())))
##? }

require(raster)
require(sp)
require(rgeos)
require(geosphere)
require(doParallel)
require(iterators)
require(foreach)
require(rgdal)
require(plyr)
require(doSNOW)
require(openxlsx)

############ SOURCE CORE FUNCTIONS ############ 
# source("building_dynamics_functions.R")

############ LOAD & CLEAN SUPPLEMENTARY DATA ############

# set name of data file with sheets containing required tables and input data
##input.datafile <- "./supp_data/building_dynamics_supp_data.xlsx"
input.datafile <- "/mnt/supp_data/building_dynamics_supp_data.xlsx"

##// LoadSuppData(input.datafile)

########## INITIALIZE SPREADSHEET FOR SUMMARY OUTPUTS ##########

##//output.filename <- "subtype_summary_tables"
##// output.filepath <- CreateOutputWorkbook(output.filename)

############ LOAD & PROCESS INPUT SHAPEFILES IN PARALLEL ############

# set up processing que from shapefiles in input_shapefiles dir
dir.files <- list.files("../input_shapefiles/")
##//in.shapes <- dir.files[CheckExt(dir.files)]
in.shapes <- dir.files[      (dir.files)]

# initialize parallel backend
no.cores <- (detectCores() - 1)
cl <- makeCluster(no.cores, type = "SOCK", outfile="")
registerDoSNOW(cl)

# iterate over input shapefiles
output.metalist <-
  foreach(shapefile = in.shapes,
          .packages = c("plyr", "rgdal", "sp", "raster",  "rgeos", "openxlsx"),
          .export = c(ls())) %dopar% {
            
            ##try(ProcessCountyParcels(shapefile))
            try(print("processing sharefile"))
          }

# cancel parallel backend
stopCluster(cl)

# write output to excel spreadsheet
##// ExportSummaryTables(output.metalist, output.filename, output.filepath)

print("Goodbye World from R! (ref:0906)")
