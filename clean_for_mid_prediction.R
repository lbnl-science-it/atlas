# clean demos data for mid year prediction


# initialization of the clustering
library(stats)
library(tidyverse)
library(dplyr)
library(data.table)


# simyear <- 2017
# yearstep <- 2
# evoyear <- simyear + yearstep
CPIbase <- 2010 # demos data are 2010 dollars, need to scale it to 2019 dollars that atlas uses

# income bins define
inc <- c(10000, 15000, 25000, 35000, 50000, 75000, 100000, 150000, 200000)

#===============No change below ===============================================#

# 1. load data -------------------------#

households0 <- fread(file.path(inputdir, paste0('year',baseyear),"households.csv"))
households0.5 <- fread(file.path(inputdir, paste0('year',simyear),"households.csv"))

persons0.5 <- fread(file.path(inputdir, paste0('year',simyear),"persons.csv")) 

names(persons0.5)[1] <- "person_id"

cpi <- read_csv(file.path(inputdir,"cpi.csv")) # ratio between 2019 and 2010 to convert demos 2010 dollars to 2019 used in psid estiamtes
load(file.path(inputdir,"psid_names.Rdat"))


# 2. clean data -------------------------#

# demos is 2010 dollar based, LJ add: convert to 2019 dollars to match the ceofs estimated from psid
cpi <- as.numeric((cpi %>% filter(Year==CPIbase))[1,3])

households0.5 <- households0.5[,income_fu:=income*cpi]


# process from person data: N_emp "kid_4_TF"      "kid_5_11_TF"   "kid_12_15_TF"
households0.5_sup <- persons0.5[,by=household_id,.(N_emp = sum(worker == 1,na.rm = T),
                                                   kid_4_TF = sum(age <5, na.rm = T) >0,
                                                   kid_5_11_TF= sum(age >=5 & age<12, na.rm = T)>0,
                                                   kid_12_15_TF = sum(age >=12 & age <16, na.rm =T)>0)]
households0.5_sup <- households0.5_sup[, Nemp_1:=N_emp ==1][,Nemp_2:=N_emp == 2][,Nemp_3:= N_emp >=3][,!c("N_emp")]
households0.5 <- merge(households0.5, households0.5_sup, by="household_id")

# process from person data: spouseTF
households0.5_sup <- persons0.5[,spouseTF:=(relate==1)][,by=household_id,.(spouseTF=sum(spouseTF))][,spouseTF:=(spouseTF>0)]
households0.5 <- merge(households0.5, households0.5_sup, by="household_id")

# process from person data: hh_edu "hh_edu_<hi"    "hh_edu_>clg"   "hh_edu_clg"    "hh_edu_hi"
households0.5_sup <- persons0.5[,edu:=fcase(is.na(edu)==T, 0, is.na(edu)==F, edu)][
  relate<=1][,by=household_id, .(edu=max(edu, na.rm = TRUE))][, hh_edu:=fcase(edu>=16 & edu<=17, "hi", edu>=18 & edu<=19, "some clg",
                                                                              edu>=20 & edu<=21, "clg", edu>=22, ">clg", default = "<hi")][
                                                                                ,edu:=NULL]

persons0.5_0 <- persons0.5[!(persons0.5$household_id %in% households0.5_sup$household_id),] # persons in 2018 but not in 2017
households0.5_sup <- rbind(persons0.5_0[,edu:=fcase(is.na(edu)==T, 0, is.na(edu)==F, edu)][
  , by=household_id, .(edu=max(edu, na.rm = TRUE))][, hh_edu:=fcase(edu>=16 & edu<=17, "hi", edu>=18 & edu<=19, "some clg",
                                                                    edu>=20 & edu<=21, "clg", edu>=22, ">clg", default = "<hi")][
                                                                      ,edu:=NULL], households0.5_sup)

households0.5 <- merge(households0.5, households0.5_sup, by="household_id", all.x = T)
households0.5 <- households0.5[,`hh_edu_<hi`:=hh_edu=="<hi"][, `hh_edu_hi`:=(hh_edu=="hi")][
  , `hh_edu_some clg`:=(hh_edu=="some_clg")][,`hh_edu_clg`:=(hh_edu=="clg")][, `hh_edu_>clg`:=(hh_edu==">clg")]

# households variables: house_tenure, house_type, moved.thiswave
households0.5 <- households0.5[,house_tenure:=(tenure==1)][,house_type:=(hh_type==1)][,moved.thiswave:=(recent_mover==1)]

# households variables: hh_age "hh_age_bins_1" "hh_age_bins_2" "hh_age_bins_3" "hh_age_60"
households0.5_sup <- persons0.5[relate==1][,by=household_id,.(sp_age=max(age))][,.(household_id, sp_age)]
households0.5 <- merge(households0.5, households0.5_sup, by="household_id", all.x = T)
households0.5_sup <- households0.5[,by="household_id", .(hh_age= pmax(age_of_head, sp_age, na.rm = T))]
households0.5 <- merge(households0.5, households0.5_sup, by="household_id", all.x = T)[
  ,hh_age_60:= hh_age >=60][
    ,hh_age_70:= hh_age >=70][,hh_age_bins:=fcase(
      hh_age <27, 1, hh_age >=27 & hh_age <=35, 2, hh_age >35, 3)][
        ,hh_age_bins_1:=(hh_age_bins==1)][,hh_age_bins_2:=(hh_age_bins==2)][
          ,hh_age_bins_3:=(hh_age_bins==3)][,hd_age_bins:=fcase(age_of_head <27, 1,
                                                                age_of_head >=27 & age_of_head <=35, 2,
                                                                age_of_head >35, 3)]

households0.5 <- households0.5[,inc_acsbins:=fcase(income_fu <inc[1], 1,
                                                   income_fu >=inc[1] & income_fu<inc[2], 2,
                                                   income_fu >=inc[2] & income_fu <inc[3], 3,
                                                   income_fu >=inc[3] & income_fu <inc[4], 4,
                                                   income_fu >=inc[4] & income_fu <inc[5], 5,
                                                   income_fu >=inc[5] & income_fu <inc[6], 6,
                                                   income_fu >=inc[6] & income_fu <inc[7], 7,
                                                   income_fu >= inc[7] & income_fu <inc[8], 8,
                                                   income_fu >= inc[8] & income_fu <inc[9], 9,
                                                   income_fu >=inc[9], 10)]

households0.5 <- households0.5[, inc_5bins:=fcase(inc_acsbins %in% c(1,2,3), 1,
                                                  inc_acsbins %in% c(4,5), 2,
                                                  inc_acsbins %in% c(6), 3,
                                                  inc_acsbins %in% c(7,8), 4,
                                                  inc_acsbins %in% c(9,10), 5)]

households0.5 <- households0.5[,inc_5bins_1:=(inc_5bins==1)][, inc_5bins_2:=(inc_5bins==2)][
  , inc_5bins_3:=(inc_5bins==3)][,inc_5bins_4:=(inc_5bins==4)][, inc_5bins_5:=(inc_5bins==5)]



households0.5 <- setnames(households0.5, "household_id", "headpid")

# add numeric household size
households0.5 <- merge(households0.5, persons0.5[,by="household_id", .(fu_size=.N)][
  ,headpid:=household_id][,!c("household_id")],by="headpid")

# add tract number
households0.5 <- households0.5[,`:=`(tract_geoid= as.numeric(substr(block_id, 1, 10)))] 

save(households0.5, file = file.path(inputdir, paste0('year',simyear),'households0.5.RData'))

# get the household_id from baseyear for copying data over
households0 = households0 %>% select(household_id) %>% rename(headpid = household_id)
save(households0, file = file.path(inputdir, paste0('year',simyear),'households0.RData'))


# clean person data for middle year main driver analysis
persons0.5 <- persons0.5[,.(household_id, age, sex, edu, worker, race, person_id)]
persons0.5 <- setnames(persons0.5, c("household_id", "sex", "race", "person_id"), 
                       c("headpid", "gender", "hd_race", "pid"))
persons0.5 <- persons0.5[, edu:=fcase(edu>=16 & edu<=17, "high", edu>=18 & edu<=19, "some_college",
                                      edu>=20 & edu<=21, "college", edu>=22, "more_than_college", 
                                      default = "<hi")][,senior:=(edu>=65)][,
                                                                            employ:=fcase(worker==0&senior==0, "not_employed", worker==0&senior==1, "retired", 
                                                                                          default = "employed")]
save(persons0.5, file = file.path(inputdir, paste0('year',simyear),'persons0.5.RData'))




