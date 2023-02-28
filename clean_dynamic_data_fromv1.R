# Rscripts to generate RData for dynamic evolution from the atlas_v1 outputs
options(dplyr.summarise.inform = FALSE)

# initialization of the clustering
library(stats)
library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(tictoc)

vehicles_output <- vehicles_output %>% rename(headpid=household_id) # headpid is the dynammic model hh identifier
households_output <- households_output %>% rename(headpid=household_id)

# rename the vehicle variables so that they are consistent with Qianmiao's code
setnames(vehicles_output, old = c('bodytype','modelyear'), new = c('vehtype','deltayear'))

save(vehicles_output, file=file.path(inputdir, paste0('year',outputyear),"vehicles_output.RData"))

save(households_output, file=file.path(inputdir, paste0('year',outputyear),"households_output.RData"))



