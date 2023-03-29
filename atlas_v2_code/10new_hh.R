# functions for atlas v2 software
hhmatch_varnames = c('Nemp_1','Nemp_2','Nemp_3',
                     'hh_edu_<hi',                
                     'hh_edu_>clg',
                     'hh_edu_clg',                
                     'hh_edu_hi',
                     'fu_size',
                     'house_tenure',
                     'house_type',
                     'kid_4_TF',
                     'kid_5_11_TF',
                     'kid_12_15_TF',
                     'inc_acsbins')


match_family2 <- function(from_tract, new_hhids,cur_hhids, hhmatch_varnames,demodat){
  # function to find the most similar hh in the cur_hhids to the new_hhids in a given geoid
  # from_tract: a single tract id of new hh
  # new_hhids:  new hh ids , we use headpid here, that are new to the dataset
  # cur_hhids: unique current hh ids that we already have hh fleet predicted
  # varnames: attributes in the demodat that are used to compute similarity between the new_hh to cur_hh
  # demodat: is the demosgraphic household level dataset of a given year -- should be the evoyear, not the baseyear
  #           Note: tract_geoid is one of the variable in the demodat
  require(dplyr)
  require(tidyr)
  varnames = hhmatch_varnames
  fromdat = demodat%>%  # new hh
    select(matches(c('headpid','tract_geoid',varnames))) %>%
    filter(headpid %in% new_hhids, tract_geoid == from_tract)
  
  fromvec = t(as.matrix(fromdat[,c(-1,-2)]))
  
  tmpnewids = unique(fromdat$headpid)
  
  # if missing data in varnames in the new family, the new family should be removed
  # because all the variables used should be present for subsequent vehicle evolution modeling
  # return NA match
  
  if(sum(is.na(fromdat))>0){
    return(NA)
  }
  
  todat = demodat%>% # existing hh
    select(matches(c('headpid','tract_geoid',varnames)))%>%
    filter(headpid %in% cur_hhids, tract_geoid == from_tract)
  
  if(dim(todat)[1]<3){
    # if tract id has some error
    # use randomm selected hh for matching
    todat = demodat%>% 
      select(matches(c('headpid','tract_geoid',varnames)))%>%
      filter(headpid %in% cur_hhids)%>%
      sample_n(5000)
  }
  
  # LJ 10/2022: shuffle the existing hh so that the pick of first min hh is random, this increase the speed and stability
  todat = todat[sample(1:nrow(todat)),]
  
  tovec = t(as.matrix(todat[,c(-1,-2)]))
  
  toids = todat$headpid
  

  
  # compute distance
  dis = apply(fromvec, 2, function(center) {
    colSums((tovec - center)^2)})
  
  
  # # return random hhid of a min distance: LJ 10/18/2022, no longer needed, now we have shuffle done earlier
  # minidx<- function(x){
  #   minx = min(x,na.rm = T)
  #   
  #   if(length(x[x == minx])==1){
  #     return(toids[x == minx])
  #   }else{
  #     # if more than one best match, random select
  #     return(sample(toids[x==minx],1))
  #   }
  # }
  
  #kk = data.frame(new_hhid = tmpnewids, matched_id = apply(dis, 2, minidx))
  
 # return(data.frame(new_hhid = tmpnewids, matched_id = apply(dis, 2, minidx)))
  
  return(data.frame(new_hhid = tmpnewids, matched_id = toids[apply(dis, 2, which.min)]))
  
  }



# now loop through new hh racts
matchinghhid <- function(demodat){
  # now loop through new hh racts
  from_tracts = (unique(demodat[demodat$headpid %in% new_hhids,'tract_geoid']))$tract_geoid
  
  # this can be 
  tic()
  res = do.call(rbind,lapply(from_tracts,match_family2,
                             new_hhids=new_hhids, 
                             cur_hhids=cur_hhids, 
                             hhmatch_varnames=hhmatch_varnames,
                             demodat=demodat))
  toc()
  

  return(res)
}


matchinghhid_parallel <- function(demodat, Npe){
  library(tidyverse)
  library(dplyr)
  library(tictoc)
  library(parallel)
  library(doParallel)
  library(foreach)
  # now loop through new hh racts
  from_tracts = (unique(demodat[demodat$headpid %in% new_hhids,'tract_geoid']))$tract_geoid
  #  print(paste('from_tracts total #',length(from_tracts)))
  registerDoParallel(cores = Npe) 
  tic()
  res <- foreach(i=1:length(from_tracts), 
                 .combine=rbind,
                 .packages = c('dplyr','tidyr')
  )  %dopar% {
    # for(i in 1:length(from_tracts)){
      print(paste('Loop',i, 'out of',length(from_tracts)))

    match_family2(from_tract = from_tracts[i], new_hhids=new_hhids, 
                 cur_hhids=cur_hhids, 
                 hhmatch_varnames=hhmatch_varnames,
                 demodat=demodat)
    
    
    
    
    
    
    
  }
  
  stopImplicitCluster()
  return(res)
}



