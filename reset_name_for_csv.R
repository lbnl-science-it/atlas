# reset names back to atlas output names from the RData used in simiulation

vehicles_output <- vehicles_output %>% rename(household_id = headpid) # headpid is the dynammic model hh identifier
households_output <- households_output %>% rename(household_id = headpid)

# rename the vehicle variables so that they are consistent with Qianmiao's code
setnames(vehicles_output, old = c('vehtype','deltayear'), new = c('bodytype','modelyear'))

write.csv(vehicles_output, file = file.path(outputdir, paste0('vehicles_',outputyear,'.csv')), row.names = F) # vehicle level prediction
write.csv(households_output, file = file.path(outputdir, paste0('householdv_',outputyear,'.csv')),row.names = F) # houshold level prediction
