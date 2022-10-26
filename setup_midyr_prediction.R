# setup coefs and functions for mid year prediction

options(dplyr.summarise.inform = FALSE)

# initialization of the clustering
library(stats)
library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(tictoc)

# load main driver coefs
print('load prediction coefs and functions')
load(file=file.path('altas_v2_coefs', "main_driver.rda"))
source(file=file.path('atlas_v2_code','8main_driver_unified.R'))
source(file=file.path('atlas_v2_code','10new_hh.R'))

# load baseyear prediction

print('load base year predictions')

load(file=file.path(inputdir, paste0('year',baseyear),"vehicles_output.RData"))

load(file=file.path(inputdir, paste0('year',baseyear),"households_output.RData"))

hhv0 = households_output; rm(households_output)
vehicles0 = vehicles_output; rm(vehicles_output)

