# prepare data for prediction of evolution year (i.e. baseyear + 2)
# the data cleaning involves both the base year and the evolution year

# initialization of the clustering
library(stats)
library(tidyverse)
library(dplyr)
library(data.table)


CPIbase <- 2010 # demos data are 2010 dollars, need to scale it to 2019 dollars that atlas uses

# income bins define
inc <- c(10000, 15000, 25000, 35000, 50000, 75000, 100000, 150000, 200000)

#===============No change below ===============================================#

# 1. load data -------------------------#
print('load demos data')
# demos data for base year
households0 <- fread(file.path(inputdir, paste0('year',baseyear),"households.csv"))
blocks0 <- fread(file.path(inputdir, paste0('year',baseyear),"blocks.csv")) 
persons0 <- fread(file.path(inputdir, paste0('year',baseyear),"persons.csv")) 
names(persons0)[1] <- "person_id"

# demos data for evolution year
households1 <- fread(file.path(inputdir, paste0('year',evoyear),"households.csv"))
grave1 <- fread(file.path(inputdir, paste0('year',evoyear),"grave.csv"))
persons1 <- fread(file.path(inputdir, paste0('year',evoyear),"persons.csv"))
names(persons1)[1] <- "person_id"



# Accessbility data
if(beamac>0){ # if read from beam, read the accessibility computed from beam by preprocess.py, which was saved in the inputdir/yearXXXX
  job <- read.table(file.path(inputdir, paste0('year',outputyear),paste0("accessibility_",outputyear,"_tract.csv")), header=T, sep=",")
}else{# else read from observed data
  load(file.path(inputdir, "accessbility_2015.RData"))
}

cpi <- read_csv(file.path(inputdir,"cpi.csv")) # ratio between 2019 and 2010 to convert demos 2010 dollars to 2019 used in psid estiamtes
load(file.path(inputdir,"psid_names.Rdat"))


# 2. clean data -------------------------#

# demos is 2010 dollar based, LJ add: convert to 2019 dollars to match the ceofs estimated from psid
cpi <- as.numeric((cpi %>% filter(Year==CPIbase))[1,3])

households0 <- households0[,income_fu:=income*cpi]
households1 <- households1[,income_fu:=income*cpi]

#################Clean the data from Urbansim: household ###############

# households variables: size_fu, hd_race
households0 <- households0[, size_fu:=persons]
households0 <- households0[, hd_race:=fcase(
  hh_race_of_head=="white", "white", 
  hh_race_of_head=="black", "black", 
  default = "other")]

# process from person data: spouseTF
households0_sup <- persons0[, spouseTF:=relate==1][
  , by = household_id, .(spouseTF = sum(spouseTF))][
    , spouseTF:=spouseTF>0]

households0 <- merge(households0, households0_sup, by="household_id")

# process from person data: hh_edu
households0_sup <- persons0[, edu:=fcase(is.na(edu)==T, 0, is.na(edu)==F, edu)][
  relate<=1][, by = household_id, .(edu = max(edu))][, hh_edu:=fcase(
    edu>=16 & edu<=17, "hi", edu>=18 & edu<=19, "some clg",
    edu>=20 & edu<=21, "clg", edu>=22, ">clg", default = "<hi")][, edu:= NULL]

households0 <- merge(households0, households0_sup, by="household_id", all.x = T)

households0 <- households0[,`hh_edu_<hi`:=hh_edu=="<hi"][, `hh_edu_hi`:=(hh_edu=="hi")][, 
                                                                                        `hh_edu_some clg`:=(hh_edu=="some_clg")][,
                                                                                                                                 `hh_edu_clg`:=(hh_edu=="clg")][, `hh_edu_>clg`:=(hh_edu==">clg")] 

# households variables: house_tenure, house_type, moved.thiswave
households0 <- households0[,house_tenure:=(tenure==1)][,house_type:=(hh_type==1)][
  ,moved.thiswave:=(recent_mover==1)]

# households variables [change status]: moved.nextwave
households1_sup <- households1[,moved.nextwave:=(recent_mover==1)][
  , .(household_id, moved.nextwave)]
households0 <-  merge(households0, households1_sup, by="household_id", all.x = T)

# households variables [change status]: hd_marriage
households1_sup <- persons1[,spouseTF:=(relate==1)][
  ,by=household_id,.(spouseTF = sum(spouseTF))][, spouseTF.nextwave:=(spouseTF>0)][
    ,.(household_id, spouseTF.nextwave)]

households0 <- merge(households0, households1_sup, by="household_id", all.x = T)[
  , ch_hd_marriage:=spouseTF.nextwave-spouseTF]

# households variables [change status]: ch_hhsize
households1_sup <- households1[,size_fu.nextwave:=persons][
  , .(household_id, size_fu.nextwave)]
households0 <- merge(households0, households1_sup, by="household_id", all.x = T)[
  ,ch_hhsize:=size_fu.nextwave - size_fu]

# households variables [change status]: ch_income
households1_sup <- households1[,income_fu.nextwave:=income*cpi][,.(household_id, income_fu.nextwave)] 

households0 <- merge(households0, households1_sup, by="household_id", all.x = T)[
  ,ch_income:=(income_fu.nextwave-income_fu)/1000]

# households variables [change status]: ch_EduTF
households1_sup <- persons1[,edu:=fcase(is.na(edu)==T, 0, is.na(edu)==F, edu)][
  relate==0][,by=household_id, .(edu=max(edu))][, hd_edu.next:=fcase(edu>=16 & edu<=17, "hi", edu>=18 & edu<=19, "some clg",
                                                                     edu>=20 & edu<=21, "clg", edu>=22, ">clg", default = "<hi")][
                                                                       ,edu:=NULL]

households1_sup2 <- persons1[,edu:=fcase(is.na(edu)==T, 0, is.na(edu)==F, edu)][
  relate==1][,by=household_id, .(edu=max(edu))][, sp_edu.next:=fcase(edu>=16 & edu<=17, "hi", edu>=18 & edu<=19, "some clg",
                                                                     edu>=20 & edu<=21, "clg", edu>=22, ">clg", default = "<hi")][
                                                                       ,edu:=NULL]

households0_sup <- persons0[,edu:=fcase(is.na(edu)==T, 0, is.na(edu)==F, edu)][
  relate==0][,by=household_id, .(edu=max(edu))][, hd_edu:=fcase(edu>=16 & edu<=17, "hi", edu>=18 & edu<=19, "some clg",
                                                                edu>=20 & edu<=21, "clg", edu>=22, ">clg", default = "<hi")][
                                                                  ,edu:=NULL]

households0_sup2 <- persons0[,edu:=fcase(is.na(edu)==T, 0, is.na(edu)==F, edu)][
  relate==1][,by=household_id, .(edu=max(edu))][, sp_edu:=fcase(edu>=16 & edu<=17, "hi", edu>=18 & edu<=19, "some clg",
                                                                edu>=20 & edu<=21, "clg", edu>=22, ">clg", default = "<hi")][
                                                                  ,edu:=NULL]

households0_sup <- merge(households0_sup, households0_sup2, by="household_id", all.x=T) %>%
  merge(households1_sup, by="household_id", all.x=T) %>%
  merge(households1_sup2, by="household_id", all.x=T)

households0_sup <- households0_sup[,ch_hdEduTF:= hd_edu.next > hd_edu][
  ,ch_spEduTF:= sp_edu.next > sp_edu][,.(household_id, ch_hdEduTF, ch_spEduTF)]

households0 <- merge(households0, households0_sup, by="household_id", all.x = T)[
  ,ch_EduTF:=fcase(is.na(ch_hdEduTF)==F & is.na(ch_spEduTF)==F, ch_hdEduTF | ch_spEduTF,
                   is.na(ch_hdEduTF)==T & is.na(ch_spEduTF)==F, ch_spEduTF,
                   is.na(ch_hdEduTF)==F & is.na(ch_spEduTF)==T, ch_hdEduTF)] 


# households variables: inc_5bins
households0 <- households0[,inc_acsbins:=fcase(income_fu <inc[1], 1,
                                               income_fu >=inc[1] & income_fu<inc[2], 2,
                                               income_fu >=inc[2] & income_fu <inc[3], 3,
                                               income_fu >=inc[3] & income_fu <inc[4], 4,
                                               income_fu >=inc[4] & income_fu <inc[5], 5,
                                               income_fu >=inc[5] & income_fu <inc[6], 6,
                                               income_fu >=inc[6] & income_fu <inc[7], 7,
                                               income_fu >= inc[7] & income_fu <inc[8], 8,
                                               income_fu >= inc[8] & income_fu <inc[9], 9,
                                               income_fu >=inc[9], 10)]

households0 <- households0[, inc_5bins:=fcase(inc_acsbins %in% c(1,2,3), 1,
                                              inc_acsbins %in% c(4,5), 2,
                                              inc_acsbins %in% c(6), 3,
                                              inc_acsbins %in% c(7,8), 4,
                                              inc_acsbins %in% c(9,10), 5)]
households0 <- households0[,inc_5bins_1:=(inc_5bins==1)][, inc_5bins_2:=(inc_5bins==2)][
  , inc_5bins_3:=(inc_5bins==3)][,inc_5bins_4:=(inc_5bins==4)][, inc_5bins_5:=(inc_5bins==5)]  


# households variables: hh_age
households0_sup <- persons0[relate==1][,by=household_id,.(sp_age=max(age))][
  , .(household_id, sp_age)]
households0 <- merge(households0, households0_sup, by="household_id", all.x = T)
households0_sup <- households0[,by="household_id", .(hh_age= pmax(age_of_head, sp_age, na.rm = T))]

households0 <- merge(households0, households0_sup, by="household_id", all.x = T)[,hh_age_60:= hh_age >=60][
  ,hh_age_70:= hh_age >=70][,hh_age_bins:=fcase(
    hh_age <27, 1, hh_age >=27 & hh_age <=35, 2, hh_age >35, 3)][
      ,hh_age_bins_1:=(hh_age_bins==1)][,hh_age_bins_2:=(hh_age_bins==2)][
        ,hh_age_bins_3:=(hh_age_bins==3)][,hd_age_bins:=fcase(age_of_head <27, 1,
                                                              age_of_head >=27 & age_of_head <=35, 2,
                                                              age_of_head >35, 3)]

# households variables: N_driving, N_kids_16, kid_4_TF, kid_5_11_TF, kid_12_15_TF
# Nkid_4, Nkid_5_11, Nkid_12_15, N_emp, N_birth
households0_sup <- persons0[,by=household_id, .(N_driving = sum(age >=16, na.rm = T),
                                                N_kids_16 = sum(age <16, na.rm = T),
                                                kid_4_TF = sum(age <5, na.rm = T) >0,
                                                kid_5_11_TF= sum(age >=5 & age<12, na.rm = T)>0,
                                                kid_12_15_TF = sum(age >=12 & age <16, na.rm =T)>0,
                                                
                                                Nkid_4 = sum(age <5, na.rm = T),
                                                Nkid_5_11= sum(age >=5 & age<12, na.rm = T),
                                                Nkid_12_15 = sum(age >=12 & age <16, na.rm =T),
                                                
                                                N_emp = sum(worker == 1,na.rm = T),
                                                N_birth = sum(age <=2, na.rm = T),
                                                N_grownupkid = sum(relate %in% c(2, 3, 4, 7) & !is.na(age) & age >=18 ,na.rm = T), # including (step, grand/great-grant) son/daughter, 
                                                N_youngkid = sum(relate %in% c(2, 3, 4, 7) & !is.na(age) & age <16 ,na.rm = T), # non-driving (step, grand/great-grant) son/daughter
                                                N_retired = sum(worker == 0 & age>60 ,na.rm = T))]

households0 <- merge(households0, households0_sup, by="household_id", all.x = T)

# households variables [change status]: kids_moveoutTF, youngkids_moveoutTF, 
# youngkids_moveinTF, retireTF, ch_Nemp, ch_Ndriving
households1_sup <- persons1[,by="household_id", .(N_driving.nextwave = sum(age >=16, na.rm = T),
                                                  N_emp.nextwave = sum(worker == 1,na.rm = T),
                                                  N_birth.nextwave = sum(age <=2, na.rm = T),
                                                  N_grownupkid.nextwave = sum(relate %in% c(2, 3, 4, 7) & !is.na(age) & age >=18 ,na.rm = T), # including (step, grand/great-grant) son/daughter, 
                                                  N_youngkid.nextwave = sum(relate %in% c(2, 3, 4, 7) & !is.na(age) & age <16 ,na.rm = T), # non-driving (step, grand/great-grant) son/daughter
                                                  N_retired.nextwave = sum(worker == 0 & age>60 ,na.rm = T))]

households0_sup <- merge(households0_sup, households1_sup, by="household_id", all.x = T)[
  ,`:=` (kids_moveoutTF = N_grownupkid.nextwave < N_grownupkid,
         youngkids_moveoutTF = N_youngkid.nextwave < N_youngkid,
         youngkids_moveinTF = N_youngkid.nextwave > N_youngkid,
         retireTF = N_retired.nextwave > N_retired,
         ch_Nemp = N_emp.nextwave - N_emp,
         ch_Ndriving = N_driving.nextwave - N_driving)][,.(household_id, kids_moveoutTF, youngkids_moveoutTF, youngkids_moveinTF, retireTF,
                                                           ch_Nemp, ch_Ndriving, N_birth.nextwave)]

households0 <- households0 %>% merge(households0_sup, by="household_id", all.x = T)

# households variables: interactions
households0 <- households0[, `:=` (moved=(moved.nextwave == T | moved.thiswave == T),
                                   youngkidXhd_age1 = kid_4_TF * hh_age_bins_1,
                                   youngkidXhd_age2 = kid_4_TF * hh_age_bins_2,
                                   youngkidXhd_age3 = kid_4_TF * hh_age_bins_3,
                                   
                                   midkidXhd_age1 = kid_5_11_TF * hh_age_bins_1,
                                   midkidXhd_age2 = kid_5_11_TF * hh_age_bins_2,
                                   midkidXhd_age3 = kid_5_11_TF * hh_age_bins_3,
                                   
                                   # birth interacting with hh_age bins
                                   birth = N_birth.nextwave >0)]

households0 <- households0[, `:=`  (birthXhd_age1= birth * hh_age_bins_1,
                                    birthXhd_age2 = birth * hh_age_bins_2,
                                    birthXhd_age3 = birth * hh_age_bins_3,
                                    
                                    
                                    # income change interacting with income bins
                                    ch_inc_earner1 = ch_income * inc_5bins_1,
                                    ch_inc_earner2 = ch_income * inc_5bins_2,
                                    ch_inc_earner3 = ch_income * inc_5bins_3,
                                    ch_inc_earner4 = ch_income * inc_5bins_4,
                                    ch_inc_earner5 = ch_income * inc_5bins_5,
                                    
                                    # low_income bins interacting with largeHH (hhsize>=4)
                                    largeHH = (size_fu>=4),
                                    smallerHH = (size_fu<=3))]

# low_income bins interacting with smaller hh (hhsize<=3)
# inc_bin1XsmallerHH = inc_5bins_1 * smallerHH,
# inc_bin2XsmallerHH = inc_5bins_2 * smallerHH,

households0 <- households0[,`:=`  (
  inc_bin1XlargeHH = inc_5bins_1 * largeHH,
  inc_bin2XlargeHH = inc_5bins_2 * largeHH)]

# rename identifier variables
households0 <- setnames(households0, "household_id", "headpid")
households0 <- households0[,`:=`(year=2017,California=1,tract_geoid= as.numeric(substr(block_id, 1, 10)))] 

# drop ununsed variables
dropcol <- setdiff(names(households0), psid_names)
households0 <- households0[, !..dropcol]

# add death data: deathTF
# pretend the data are available now
households0_sup <- grave1[,by=household_id, .(deathTF=.N>0)]
households0_sup <- setnames(households0_sup, "household_id", "headpid")
households0 <- merge(households0, households0_sup, by="headpid", all.x = T)[,newhhflag:=0]
households0$deathTF[is.na(households0$deathTF)==T] <- F

# add geolocation variables from base year
names(job)[1] <- "tract_geoid"
households0 <- households0 %>% merge(job, by="tract_geoid", all.x=TRUE)
households0$access_zscore[is.na(households0$access_zscore)] <- 0
households0$urban_cbsa[is.na(households0$urban_cbsa)] <- 0
names(households0)[names(households0) == "access_zscore"] <- "emp_zscore"
households0 <- households0[,.SD, .SDcols = !c('access_sum')]

blocks0 <- blocks0 %>% rename(tract_geoid=tract_id)
tracts <- blocks0[,by="tract_geoid", .(square_meters=sum(square_meters_land))][,square_km:=(square_meters/10^6)]
tracts_pop <- households0[ ,by=tract_geoid, .(population=sum(size_fu))]
tracts$tract_geoid <- as.numeric(tracts$tract_geoid)
tracts <- tracts %>% merge(tracts_pop, by="tract_geoid")

# pop density population per square km
tracts <- tracts[, pop_density:=population/square_km][,.SD, .SDcols = c('tract_geoid', 'pop_density')]
households0 <- households0 %>% merge(tracts, by="tract_geoid", all.x = T)
households0$pop_density[is.na(households0$pop_density)] <- 0

# households$log_pop_density <- log(households$pop_density+1)

cols <- which(sapply(households0, is.logical))
setDT(households0)
for(j in cols){
  set(households0, i=NULL, j=j, value= as.numeric(households0[[j]]))
}

save(households0, file = file.path(inputdir, paste0('year',evoyear),'households0.RData'))

#################Clean the  evoyear data for new hh matching#####################
# process from person data: N_emp "kid_4_TF"      "kid_5_11_TF"   "kid_12_15_TF"

print('prepare variables for new hh matching')
households1_sup <- persons1[,by=household_id,.(N_emp = sum(worker == 1,na.rm = T),
                                               kid_4_TF = sum(age <5, na.rm = T) >0,
                                               kid_5_11_TF= sum(age >=5 & age<12, na.rm = T)>0,
                                               kid_12_15_TF = sum(age >=12 & age <16, na.rm =T)>0)]
households1_sup <- households1_sup[, Nemp_1:=N_emp ==1][,Nemp_2:=N_emp == 2][,Nemp_3:= N_emp >=3][,!c("N_emp")]
households1 <- merge(households1, households1_sup, by="household_id")

# process from person data: spouseTF
households1_sup <- persons1[,spouseTF:=(relate==1)][,by=household_id,.(spouseTF=sum(spouseTF))][,spouseTF:=(spouseTF>0)]
households1 <- merge(households1, households1_sup, by="household_id")

# process from person data: hh_edu "hh_edu_<hi"    "hh_edu_>clg"   "hh_edu_clg"    "hh_edu_hi"
households1_sup <- persons1[,edu:=fcase(is.na(edu)==T, 0, is.na(edu)==F, edu)][
  relate<=1][,by=household_id, .(edu=max(edu, na.rm = TRUE))][, hh_edu:=fcase(edu>=16 & edu<=17, "hi", edu>=18 & edu<=19, "some clg",
                                                                              edu>=20 & edu<=21, "clg", edu>=22, ">clg", default = "<hi")][
                                                                                ,edu:=NULL]

persons1_0 <- persons1[!(persons1$household_id %in% households1_sup$household_id),]
households1_sup <- rbind(persons1_0[,edu:=fcase(is.na(edu)==T, 0, is.na(edu)==F, edu)][
  , by=household_id, .(edu=max(edu, na.rm = TRUE))][, hh_edu:=fcase(edu>=16 & edu<=17, "hi", edu>=18 & edu<=19, "some clg",
                                                                    edu>=20 & edu<=21, "clg", edu>=22, ">clg", default = "<hi")][
                                                                      ,edu:=NULL], households1_sup)

households1 <- merge(households1, households1_sup, by="household_id", all.x = T)
households1 <- households1[,`hh_edu_<hi`:=hh_edu=="<hi"][, `hh_edu_hi`:=(hh_edu=="hi")][
  , `hh_edu_some clg`:=(hh_edu=="some_clg")][,`hh_edu_clg`:=(hh_edu=="clg")][, `hh_edu_>clg`:=(hh_edu==">clg")]

# households variables: house_tenure, house_type, moved.thiswave
households1 <- households1[,house_tenure:=(tenure==1)][,house_type:=(hh_type==1)][,moved.thiswave:=(recent_mover==1)]

# households variables: hh_age "hh_age_bins_1" "hh_age_bins_2" "hh_age_bins_3" "hh_age_60"
households1_sup <- persons1[relate==1][,by=household_id,.(sp_age=max(age))][,.(household_id, sp_age)]
households1 <- merge(households1, households1_sup, by="household_id", all.x = T)
households1_sup <- households1[,by="household_id", .(hh_age= pmax(age_of_head, sp_age, na.rm = T))]
households1 <- merge(households1, households1_sup, by="household_id", all.x = T)[
  ,hh_age_60:= hh_age >=60][
    ,hh_age_70:= hh_age >=70][,hh_age_bins:=fcase(
      hh_age <27, 1, hh_age >=27 & hh_age <=35, 2, hh_age >35, 3)][
        ,hh_age_bins_1:=(hh_age_bins==1)][,hh_age_bins_2:=(hh_age_bins==2)][
          ,hh_age_bins_3:=(hh_age_bins==3)][,hd_age_bins:=fcase(age_of_head <27, 1,
                                                                age_of_head >=27 & age_of_head <=35, 2,
                                                                age_of_head >35, 3)]

households1 <- households1[,inc_acsbins:=fcase(income_fu <inc[1], 1,
                                               income_fu >=inc[1] & income_fu<inc[2], 2,
                                               income_fu >=inc[2] & income_fu <inc[3], 3,
                                               income_fu >=inc[3] & income_fu <inc[4], 4,
                                               income_fu >=inc[4] & income_fu <inc[5], 5,
                                               income_fu >=inc[5] & income_fu <inc[6], 6,
                                               income_fu >=inc[6] & income_fu <inc[7], 7,
                                               income_fu >= inc[7] & income_fu <inc[8], 8,
                                               income_fu >= inc[8] & income_fu <inc[9], 9,
                                               income_fu >=inc[9], 10)]

households1 <- households1[, inc_5bins:=fcase(inc_acsbins %in% c(1,2,3), 1,
                                              inc_acsbins %in% c(4,5), 2,
                                              inc_acsbins %in% c(6), 3,
                                              inc_acsbins %in% c(7,8), 4,
                                              inc_acsbins %in% c(9,10), 5)]

households1 <- households1[,inc_5bins_1:=(inc_5bins==1)][, inc_5bins_2:=(inc_5bins==2)][
  , inc_5bins_3:=(inc_5bins==3)][,inc_5bins_4:=(inc_5bins==4)][, inc_5bins_5:=(inc_5bins==5)]

# add tract id
households1 <- households1[,`:=`(tract_geoid= as.numeric(substr(block_id, 1, 10)))] 

households1 <- setnames(households1, "household_id", "headpid")

# add numeric household size
households1 <- merge(households1, persons1[,by="household_id", .(fu_size=.N)][
  ,headpid:=household_id][,!c("household_id")],by="headpid")


save(households1, file = file.path(inputdir, paste0('year',evoyear),'households1.RData'))


# clean person data for evoyear main driver analysis
print('clean evolution year person data for main driver prediction')
persons1 <- persons1[,.(household_id, age, sex, edu, worker, race, person_id)]
persons1 <- setnames(persons1, c("household_id", "sex", "race", "person_id"), 
                       c("headpid", "gender", "hd_race", "pid"))
persons1 <- persons1[, edu:=fcase(edu>=16 & edu<=17, "high", edu>=18 & edu<=19, "some_college",
                                      edu>=20 & edu<=21, "college", edu>=22, "more_than_college", 
                                      default = "<hi")][,senior:=(edu>=65)][,
                                                                            employ:=fcase(worker==0&senior==0, "not_employed", worker==0&senior==1, "retired", 
                                                                                          default = "employed")]
save(persons1, file = file.path(inputdir, paste0('year',evoyear),'persons1.RData'))

# save person0 just in case for post analysis for now
persons0 <- persons0[,.(household_id, age, sex, edu, worker, race, person_id)]
persons0 <- setnames(persons0, c("household_id", "sex", "race", "person_id"), 
                     c("headpid", "gender", "hd_race", "pid"))
persons0 <- persons0[, edu:=fcase(edu>=16 & edu<=17, "high", edu>=18 & edu<=19, "some_college",
                                  edu>=20 & edu<=21, "college", edu>=22, "more_than_college", 
                                  default = "<hi")][,senior:=(edu>=65)][,
                                                                        employ:=fcase(worker==0&senior==0, "not_employed", worker==0&senior==1, "retired", 
                                                                                      default = "employed")]
save(persons0, file = file.path(inputdir, paste0('year',evoyear),'persons0.RData'))
