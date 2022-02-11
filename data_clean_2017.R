#################Clean the data from Urbansim ###############
#Read the original data
households <- read.table(file.path(inputdir, paste0('year',outputyear),"households.csv"), header=T, sep=",")
blocks <- read.table(file.path(inputdir,paste0('year',outputyear),"blocks.csv"), header=T, sep=",")
persons <- read.table(file.path(inputdir, paste0('year',outputyear),"persons.csv"), header=T, sep=",")
residential <- read.table(file.path(inputdir, paste0('year',outputyear),"residential.csv"), header=T, sep=",")
jobs <- read.table(file.path(inputdir, paste0('year',outputyear),"jobs.csv"), header=T, sep=",")

# correct the error of person_id column name
names(persons)[1] <- "person_id"

# Accessbility data
load(file.path(inputdir, "accessbility_2015.RData"))

# availability of transit/bus by tract, only used for 2010 parameters
if(diryear == 2010){tract_access <- read_csv(file.path(inputdir, "modeaccessibility.csv"))}

#Create new variables
#	INCOME1		    - Lowest income household (< $25,000)
#	INCOME2		    - Low income household (25,000 <= & < $50,000)
#	INCOME3		    - Middle income household (50,000 <= & < $75,000)
#	INCOME4		    - High income household (75,000 <= & < $100,000)
#	INCOME5		    - Highest income household (>= $100,000)
households <- households %>% mutate(income1 = case_when(income < 25000 ~ 1, TRUE~0),
                                income2 = case_when(income >= 25000 & income < 50000 ~ 1, TRUE~0),
                                income3 = case_when(income >= 50000 & income < 75000 ~ 1, TRUE~0),
                                income4 = case_when(income >= 75000 & income < 100000 ~ 1, TRUE~0),
                                income5 = case_when(income >= 100000 ~ 1, TRUE~0))
#	HSIZE1		    - Household size = 1
#	HSIZE2		    - Household size = 2
#	HSIZE3		    - Household size = 3
#	HSIZE4		    - Household size = 4 or more
households <- households %>% mutate(HHSIZE1 = case_when(persons == 1 ~ 1, TRUE~0),
                                    HHSIZE2 = case_when(persons == 2 ~ 1, TRUE~0),
                                    HHSIZE3 = case_when(persons == 3 ~ 1, TRUE~0),
                                    HHSIZE4 = case_when(persons >= 4 ~ 1, TRUE~0))
households$HHSIZE <- households$persons
#	Own		        - Household own the house (homeown)
households <- households %>% mutate(hhown = case_when(tenure == 1 ~ 1, TRUE~0))
#	WORK0		      - Zero worker household
#	WORK1		      - One worker household
#	WORK2		      - Two worker household
#	WORK3		      - Three or more worker household
households <- households %>% mutate(work0 = case_when(workers == 0 ~ 1, TRUE~0),
                                work1 = case_when(workers == 1 ~ 1, TRUE~0),
                                work2 = case_when(workers == 2 ~ 1, TRUE~0),
                                work3 = case_when(workers >= 3 ~ 1, TRUE~0))
# Household Race
households <- households %>% mutate(hhwhite = case_when(race_of_head== 1 ~ 1, TRUE~0),
                                hhblack = case_when(race_of_head == 2 ~ 1, TRUE~0),
                                hhasian = case_when(race_of_head == 6 ~ 1, TRUE~0),
                                hhothers = case_when(race_of_head != 1 & race_of_head != 2 & race_of_head != 6 ~ 1, TRUE~0))
# Number of adults
children <- persons %>% group_by(household_id) %>% summarise(NUMCHILD = sum(age < 18))
households <- households %>% merge(children, by="household_id")
households <- households %>% mutate(NUMADLT = persons - NUMCHILD)
households <- households %>% mutate(child = case_when(NUMCHILD>0 ~ 1, TRUE~0))

# Life cycle
# LIF_CYC1: 01=one adult, no children
households <- households %>% mutate(LIF_CYC1 = case_when(NUMADLT - seniors == 1 & NUMCHILD ==0 ~ 1, TRUE~0))
# LIF_CYC2: 02=2+ adults, no children
households <- households %>% mutate(LIF_CYC2 = case_when(NUMADLT - seniors >= 2 & NUMCHILD ==0 ~ 1, TRUE~0))
# youngest person
youngest <- persons %>% group_by(household_id) %>% summarise(minage = min(age))
households <- households %>% merge(youngest, by="household_id")
rm(youngest, children)
# LIF_CYC3: 03=one adult, youngest child 0-5
households <- households %>% mutate(LIF_CYC3 = case_when(NUMADLT - seniors == 1 & minage <= 5 ~ 1, TRUE~0))
# LIF_CYC4: 04=2+ adults, youngest child 0-5
households <- households %>% mutate(LIF_CYC4 = case_when(NUMADLT - seniors >= 2 & minage <= 5 ~ 1, TRUE~0))
# LIF_CYC5: 05=one adult, youngest child 6-15
households <- households %>% mutate(LIF_CYC5 = case_when(NUMADLT - seniors == 1 & minage <= 15 & minage >= 6 ~ 1, TRUE~0))
# LIF_CYC6: 06=2+ adults, youngest child 6-15
households <- households %>% mutate(LIF_CYC6 = case_when(NUMADLT - seniors >= 2 & minage <= 15 & minage >= 6 ~ 1, TRUE~0))
# LIF_CYC7: 03=one adult, youngest child 16-21
households <- households %>% mutate(LIF_CYC7 = case_when(NUMADLT - seniors == 1 & minage <= 21 & minage >= 16 ~ 1, TRUE~0))
# LIF_CYC8: 04=2+ adults, youngest child 16-21
households <- households %>% mutate(LIF_CYC8 = case_when(NUMADLT - seniors >= 2 & minage <= 21 & minage >= 16 ~ 1, TRUE~0))
# LIF_CYC9: 09=one adult, retired, no children
households <- households %>% mutate(LIF_CYC9 = case_when(seniors == 1 & NUMCHILD ==0 ~ 1, TRUE~0))
# LIF_CYC10: 10=2+ adult, retired, no children
households <- households %>% mutate(LIF_CYC10 = case_when(seniors >= 2 & NUMCHILD ==0 ~ 1, TRUE~0))
households$minage <- NULL

# Retired
households <- households %>% mutate(retired = case_when(LIF_CYC9 == 1 | LIF_CYC10 == 1 ~ 1, TRUE~0))

# Tract id
households <- households %>% mutate(tract_id= as.numeric(substr(block_id, 1, 10)))

# Geolocation data
# rent percentage by tract
residential <- residential %>% mutate(tract_id = as.numeric(substr(block_group_id, 1, 10)))
# update 11.15, building_type_id == 1,2 --> single family owned, multifamily owned
perrent <- residential %>% group_by(tract_id) %>% summarise(totalhouse = sum(unit_id >0), 
                                                            renthouse = sum(unit_id>0 & building_type_id!=1 & building_type_id!=2)) %>% mutate(perrent=renthouse/totalhouse * 100)
# perrent: precentage of rental housing is less than 25%
perrent <- perrent %>% mutate(perrent1 = case_when(perrent < 25 ~ 1, TRUE~0),
                              perrent2 = case_when(perrent < 45 & perrent >=25 ~ 1, TRUE~0),
                              perrent3 = case_when(perrent >= 45  ~ 1, TRUE~0))
perrent[, c("totalhouse", "renthouse")] <- list(NULL)
households <- households %>% merge(perrent, by="tract_id")
rm(perrent)

# Accessbility data
# 
names(job)[1] <- "tract_id"
households <- households %>% merge(job, by="tract_id", all.x=TRUE)
households$access_zscore[is.na(households$access_zscore)] <- 0
households$urban_cbsa[is.na(households$urban_cbsa)] <- 0
households[, "access_sum"] <- list(NULL)
names(households)[names(households) == "access_zscore"] <- "emp_zscore"

# pop_density/job_density
# regionaldata <- read.table("D:/Box Sync/lbl intern/VFC_NHTS2017/data cleaning/microtypes_inputs.csv", header=T, sep="," , fill = TRUE)
# names(regionaldata)[1] <- "tract_id"
# households <- households %>% merge(regionaldata, by="tract_id")
tracts <- blocks %>% group_by(tract_id) %>% summarise(square_meters=sum(square_meters_land))
tracts <- tracts %>% mutate(square_km=(square_meters/10^6))  #*0.386102
tracts_pop <- persons %>% merge(households, by="household_id") %>% group_by(tract_id) %>% summarise(population=n_distinct(person_id))
tracts <- tracts %>% merge(tracts_pop, by="tract_id")
tracts_job <- jobs %>% mutate(tract_id=floor(block_id/10000)) %>% group_by(tract_id) %>% summarise(jobs=n_distinct(job_id))
tracts <- tracts %>% merge(tracts_job, by="tract_id")
rm(tracts_pop, tracts_job)
tracts <- tracts %>% mutate(job_density=jobs/square_km, pop_density=population/square_km)
households <- households %>% merge(tracts, by="tract_id")
households$log_job_density <- log(households$job_density+1)
households$log_pop_density <- log(households$pop_density+1)

# age of head categories
households <- households %>% mutate(hhage_34=case_when(age_of_head<=34~1, TRUE~0),
                                  hhage_35_64=case_when(age_of_head<=64 & age_of_head>=35~1, TRUE~0),
                                  hhage_65=case_when(age_of_head>=65~1, TRUE~0))

##### generate some variable for later estimation ######
# LJ flag: this is only to move the uno and sero and id number for later model estimation usage
# is there a reason why this needs to be here? --> no, but still leave it here for now
# used for estimation used in old apollo code
households$uno <- 1
households$sero <- 0
column5 <- names(households[5])
households <- households %>% relocate(uno, .before=all_of(column5))
households <- households %>% relocate(sero, .after=uno)
households <- households %>% mutate(id = row_number())
households <- households %>% relocate(id, .after=sero)



#save(households, file = "data_clean17/households.RData")

############################ person level data clean ###################################
# Age
persons <- persons %>% filter(age >= 18)
# LJ flag: this will cause problem when the order of variables in person table change
#names(persons)[2] <- "R_AGE_IMP"
persons = persons %>% rename(R_AGE_IMP = age)
# Education
persons <- persons %>% mutate(below_high = case_when(edu <= 15 ~ 1, TRUE~0),
                                              high = case_when(edu ==16 | edu == 17 ~ 1, TRUE~0),
                                              college = case_when(edu ==18 | edu == 19 | edu ==20 | edu ==21 ~ 1, TRUE~0),
                                              graduate = case_when(edu >=22 ~ 1, TRUE~0))
# female=1
persons$R_SEX_IMP <- persons$sex-1

# race categories
persons <- persons %>% mutate(white = case_when(race == "white" ~ 1, TRUE~0),
                                              black = case_when(race == "black" ~ 1, TRUE~0),
                                              asian = case_when(race == "asian" ~ 1, TRUE~0),
                                              race_other = case_when(race == "other" ~ 1, TRUE~0))
# Work status
persons <- persons %>% mutate(work = case_when(worker == 1 ~ 1, TRUE~0),
                                              school = case_when(student == 1 ~ 1, TRUE~0),
                                              retired_person = case_when(R_AGE_IMP >= 65 ~ 1, TRUE~0))

#save(persons, file = "data_clean17/persons.RData")
if(diryear == 2010){
  rm(list = c('tract_access', 'jobs','residential','blocks'))
}else{
  rm(list = c('jobs','residential','blocks'))
}


