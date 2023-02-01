# rscript to run predictions for mid year


cur_hhids = intersect(households0$headpid,households0.5$headpid)

print('copying continuing hh vehicles over to the next year')
cont.hhv = hhv0[hhv0$headpid %in% cur_hhids,] # copy the hh level predictions over 
cont.veh = vehicles0[vehicles0$headpid %in% cur_hhids,] # copy the veh level predictions over 

# match the new hhs to existing hhs
print('match the new hh to existing hh')
demodat = households0.5 %>% # use the demo data from the mid year to do the matching
  select(matches(c('headpid','tract_geoid',hhmatch_varnames)))  

demodat = demodat %>% drop_na() # get complete cases

new_hhids = demodat$headpid[!(demodat$headpid %in% cur_hhids)]

# tic()
# 
# matched_ids <- matchinghhid(demodat) # serial mode run
# toc()

# parallel process is faster LJ: 10/18/2022, improved the stablility by doing shuffling in earlier step of matchfamily2 function
tic()
matched_ids <- matchinghhid_parallel(demodat, Npe = Npe)
toc()


print('copy the matched continuing hh vehicles to new hh')
# copy hhv over
new_hhv = matched_ids %>% 
  rename(headpid = matched_id) %>%
  left_join(cont.hhv) %>%
  select(new_hhid, nvehicles)%>%
  rename(headpid = new_hhid)

# copy vehicle-level prediction over
new_veh = matched_ids %>% 
  rename(headpid = matched_id) %>%
  left_join(cont.veh) %>%
  select(-headpid)%>%
  rename(headpid = new_hhid)

# combine the continuing hh with new hh

hhv0.5 = bind_rows(cont.hhv, new_hhv)
vehicles0.5 = bind_rows(cont.veh, new_veh)

# as the persons may have changed, need to redo main driver prediction here
# redo main driver prediction

# print('redo main driver prediction as the persons may have changed')
# tic()
# vehicles0.5 <- maindriver(as.data.table(vehicles0.5), as.data.table(persons0.5))
# toc()


print('redo main driver prediction as the persons may have changed')
tic()
vehicles0.5 <- p_maindriver(as.data.table(vehicles0.5), as.data.table(persons0.5), Npe = Npe)
toc()


# 
# > head(hhv0.5)
# headpid nvehicles     budget
# 1       0         1 18536.2744
# 2       1         2  3862.4185
# 3       2         1  6814.5567
# 4       3         2   920.9202
# 5       4         1  5096.2765
# 6       5         3  8449.8414
# > head(vehicles0.5)
# headpid vehicle_id vehtype vintage_category annual_mileage pred_power
# 1:       0          1     car        0~5 years     18536.2744        AEV
# 2:       1          1     car       6~11 years      5952.7650        ICE
# 3:       1          2     suv        12+ years      1772.0720        ICE
# 4:       2          1     car        12+ years      6814.5567        ICE
# 5:       3          1     car        12+ years       920.9202        ICE
# 6:       3          2     car        12+ years       920.9202        ICE
# ownlease deltayear maindriver_id
# 1:      own      2013           417
# 2:      own      2012          2562
# 3:      own      1998          2562
# 4:      own      2005          2143
# 5:      own      2006           858
# 6:      own      2005           858

vehicles_output = vehicles0.5 %>% select(-annual_mileage) # currently, we do not predict mileage
households_output = hhv0.5 # %>% select(-budget) # currently, we do not predict mileage

# recode vintage category based on cuurrent year vintage
vehicles_output = vehicles_output %>%
  mutate(vintage_category = case_when(
    outputyear - deltayear <=5 ~ '0~5 years',
    outputyear - deltayear >=12 ~ '12+ years',
    TRUE ~ '6~11 years'
  ))

# save RData to input for predicting future years if needed.
save(vehicles_output, file=file.path(inputdir, paste0('year',outputyear),"vehicles_output.RData"))
save(households_output, file=file.path(inputdir, paste0('year',outputyear),"households_output.RData"))



