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
  require(data.table)
  varnames = hhmatch_varnames
  demodat <- as.data.frame(demodat) # demodat is a data.table object, to avoid run error, force it to data.frame here.
  fromdat = demodat%>%  # new hh
    dplyr::select(matches(c('headpid','tract_geoid',varnames))) %>%
    dplyr::filter(headpid %in% new_hhids, tract_geoid == from_tract)
  
  fromvec = t(as.matrix(fromdat[,varnames]))
  
  tmpnewids = unique(fromdat$headpid)
  
  # if missing data in varnames in the new family, the new family should be removed
  # because all the variables used should be present for subsequent vehicle evolution modeling
  # return NA match
  
  if(sum(is.na(fromdat))>0){
    return(NA)
  }
  
  todat = demodat%>% # existing hh
    #dplyr::select(matches(c('headpid','tract_geoid',varnames)))%>%
    dplyr::filter(headpid %in% cur_hhids, tract_geoid == from_tract)
  
  if(dim(todat)[1]<3){
    # if tract id has some error
    # first try using 2000 hh from othe same county
    todat <- demodat%>% 
      dplyr::filter(headpid %in% cur_hhids, county_id == floor(from_tract/1000000))%>%
      dplyr::select(matches(c('headpid','tract_geoid',varnames)))
      
      
    if(dim(todat)[1]<1000){ # if very few hh in the county, use random select sample from full pop to match
      # use randomm selected hh for matching
      todat <- demodat%>% 
        # select(matches(c('headpid','tract_geoid',varnames)))%>%
        filter(headpid %in% cur_hhids)%>%
        sample_n(1000) 
    }else{ # if enough pop in the county
      todat <- todat%>% sample_n(1000)
    }
      
  }else{ # if more than 3 hh
     if(dim(todat)[1]>1000){
       # if more than 1000 hh, random select 1000
       todat <- todat%>% sample_n(1000)
     }
   }
  
  # LJ 10/2022: shuffle the existing hh so that the pick of first min hh is random, this increase the speed and stability
  todat = todat[sample(1:nrow(todat)),]
  
  tovec = t(as.matrix(todat[,varnames]))
  
  toids = todat$headpid
  
  # compute distance
  dis = apply(fromvec, 2, function(center) {
    colSums((tovec - center)^2)})
  
  return(data.frame(new_hhid = tmpnewids, matched_id = toids[apply(dis, 2, which.min)]))
  
}


# original code
match_family2.orig <- function(from_tract, new_hhids,cur_hhids, hhmatch_varnames,demodat){
  # function to find the most similar hh in the cur_hhids to the new_hhids in a given geoid
  # from_tract: a single tract id of new hh
  # new_hhids:  new hh ids , we use headpid here, that are new to the dataset
  # cur_hhids: unique current hh ids that we already have hh fleet predicted
  # varnames: attributes in the demodat that are used to compute similarity between the new_hh to cur_hh
  # demodat: is the demosgraphic household level dataset of a given year -- should be the evoyear, not the baseyear
  #           Note: tract_geoid is one of the variable in the demodat
  require(dplyr)
  require(tidyr)
  require(data.table)
  varnames = hhmatch_varnames
  demodat <- as.data.frame(demodat) # demodat is a data.table object, to avoid run error, force it to data.frame here.
  fromdat = demodat%>%  # new hh
    dplyr::select(matches(c('headpid','tract_geoid',varnames))) %>%
    dplyr::filter(headpid %in% new_hhids, tract_geoid == from_tract)
  
  fromvec = t(as.matrix(fromdat[,c(-1,-2)]))
  
  tmpnewids = unique(fromdat$headpid)
  
  # if missing data in varnames in the new family, the new family should be removed
  # because all the variables used should be present for subsequent vehicle evolution modeling
  # return NA match
  
  if(sum(is.na(fromdat))>0){
    return(NA)
  }
  
  todat = demodat%>% # existing hh
    dplyr::select(matches(c('headpid','tract_geoid',varnames)))%>%
    dplyr::filter(headpid %in% cur_hhids, tract_geoid == from_tract)
  
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
  
  return(data.frame(new_hhid = tmpnewids, matched_id = toids[apply(dis, 2, which.min)]))
  
}


match_family2.experiment2 <- function(from_tract, new_hhids,cur_hhids, hhmatch_varnames,demodat){
  # function to find the most similar hh in the cur_hhids to the new_hhids in a given geoid
  # from_tract: a single tract id of new hh
  # new_hhids:  new hh ids , we use headpid here, that are new to the dataset
  # cur_hhids: unique current hh ids that we already have hh fleet predicted
  # varnames: attributes in the demodat that are used to compute similarity between the new_hh to cur_hh
  # demodat: is the demosgraphic household level dataset of a given year -- should be the evoyear, not the baseyear
  #           Note: tract_geoid is one of the variable in the demodat
  require(dplyr)
  require(tidyr)
  require(data.table)
  varnames <- c('headpid','tract_geoid',hhmatch_varnames)
  # new hh
  fromdat = demodat[, ..varnames][headpid %in% new_hhids,][tract_geoid == from_tract,]
  
  fromvec = t(as.matrix(fromdat[,c(-1,-2)]))

    tmpnewids = unique(fromdat$headpid)
  
  # if missing data in varnames in the new family, the new family should be removed
  # because all the variables used should be present for subsequent vehicle evolution modeling
  # return NA match
  
  if(sum(is.na(fromdat))>0){
    return(NA)
  }
  
  #existing hh
  todat <- demodat[, ..varnames][headpid %in% cur_hhids,][tract_geoid == from_tract,]
  
  if(dim(todat)[1]<100){
    # print('existing tract has not enough sample to match')
    # if tract id has some error
    # use randomm selected hh from the same county for matching
    todat <- demodat[, ..varnames][headpid %in% cur_hhids,][county_id == floor(from_tract/1000000),]
    if(dim(todat)[1]<100){ # if the county has not enough data, random select from all data
    todat <- demodat[, ..varnames][headpid %in% cur_hhids,][sample(.N,1000)]
    }else{
      if(dim(todat)[1]>1000){ # if more than 1000
      # random select 1000
      todat <- todat[sample(.N,1000)]
      }
    }
  }
  
  # LJ 10/2022: shuffle the existing hh so that the pick of first min hh is random, this increase the speed and stability
  todat <- todat[sample(.N, .N)]
  
  tovec <- t(as.matrix(todat[,c(-1,-2)]))
  
  toids <- todat$headpid
  
  # LJ 4/25/2024: alternative way to compute distance matrix of rows between two matrixs: https://stackoverflow.com/questions/59679046/speed-challenge-any-faster-method-to-calculate-distance-matrix-between-rows-of

  distfunc <- function(x,y){
    sqrt(outer(rowSums(x^2), rowSums(y^2), '+') - tcrossprod(x, 2 * y))
  }
  
  fromvec <- t(fromvec)
  tovec <- t(tovec)
  dis<- distfunc(tovec,fromvec) # number of tovec x number of fromvec

  return(data.frame(new_hhid = tmpnewids, matched_id = toids[apply(dis, 2, which.min)]))
  
  }

match_family2.experiment1 <- function(from_tract, new_hhids,cur_hhids, hhmatch_varnames,demodat){
  # function to find the most similar hh in the cur_hhids to the new_hhids in a given geoid
  # from_tract: a single tract id of new hh
  # new_hhids:  new hh ids , we use headpid here, that are new to the dataset
  # cur_hhids: unique current hh ids that we already have hh fleet predicted
  # varnames: attributes in the demodat that are used to compute similarity between the new_hh to cur_hh
  # demodat: is the demosgraphic household level dataset of a given year -- should be the evoyear, not the baseyear
  #           Note: tract_geoid is one of the variable in the demodat
  require(dplyr)
  require(tidyr)
  require(data.table)
  varnames <- c('headpid','tract_geoid',hhmatch_varnames)
  # new hh
  fromdat = demodat[, ..varnames][headpid %in% new_hhids,][tract_geoid == from_tract,]
  
  fromvec = t(as.matrix(fromdat[,c(-1,-2)]))
  
  #  fromvec = as.data.table(t(as.matrix(fromdat[,c(-1,-2)]))) # use data.table opertation
  
  tmpnewids = unique(fromdat$headpid)
  
  # if missing data in varnames in the new family, the new family should be removed
  # because all the variables used should be present for subsequent vehicle evolution modeling
  # return NA match
  
  if(sum(is.na(fromdat))>0){
    return(NA)
  }
  
  #existing hh
  todat <- demodat[, ..varnames][headpid %in% cur_hhids,][tract_geoid == from_tract,]
  
  if(dim(todat)[1]<3){
    print('existing tract has not enough sample to match')
    # if tract id has some error
    # use randomm selected hh for matching
    todat <- demodat[, ..varnames][headpid %in% cur_hhids,][sample(.N,5000)]
  }
  
  # LJ 10/2022: shuffle the existing hh so that the pick of first min hh is random, this increase the speed and stability
  # todat <- todat[sample(1:nrow(todat)),]
  todat <- todat[sample(.N, .N)]
  
  tovec <- t(as.matrix(todat[,c(-1,-2)]))
  
  toids <- todat$headpid
  
  # # # compute distance
  # dis <- apply(fromvec, 2, function(center) {
  #   colSums((tovec - center)^2)})
  # 
  # 
  # LJ 4/25/2024: alternative way to compute distance matrix of rows between two matrixs: https://stackoverflow.com/questions/59679046/speed-challenge-any-faster-method-to-calculate-distance-matrix-between-rows-of
  
  distfunc <- function(x,y){
    sqrt(outer(rowSums(x^2), rowSums(y^2), '+') - tcrossprod(x, 2 * y))
  }
  
  fromvec <- t(fromvec)
  tovec <- t(tovec)
  dis<- distfunc(tovec,fromvec) # number of tovec x number of fromvec
  #  dis <- fromvec[, lapply(seq_along(.SD),function(center)(tovec - .SD[x])]
  
  return(data.frame(new_hhid = tmpnewids, matched_id = toids[apply(dis, 2, which.min)]))
  
}




# now loop through new hh racts
matchinghhid <- function(demodat){
  # now loop through new hh racts
  #profvis({
  require(dplyr)
  require(tidyr)
  
  from_tracts = (unique(demodat[demodat$headpid %in% new_hhids,'tract_geoid']))$tract_geoid
  
  # this can be 
  tic()
  res <- do.call(rbind,lapply(from_tracts,match_family2,
                             new_hhids=new_hhids, 
                             cur_hhids=cur_hhids, 
                             hhmatch_varnames=hhmatch_varnames,
                             demodat=demodat))
  toc()
  #})

  return(res)
}


matchinghhid_parallel <- function(demodat, Npe){
  library(tidyverse)
  library(dplyr)
  require(data.table)
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
    #  print(paste('Loop',i, 'out of',length(from_tracts)))

    match_family2(from_tract = from_tracts[i], new_hhids=new_hhids, 
                 cur_hhids=cur_hhids, 
                 hhmatch_varnames=hhmatch_varnames,
                 demodat=demodat)
    
    
    
    
    
    
    
  }
  
  stopImplicitCluster()
  return(res)
}





# now loop through new hh racts
matchinghhid.bkup <- function(demodat){
  # now loop through new hh racts
  
  require(dplyr)
  require(tidyr)
  
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


matchinghhid_parallel.bkup <- function(demodat, Npe){
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


