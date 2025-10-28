# ATLAS Vehicle Fleet Composition 
# Data cleaning
# Estimation data

# Restricted-version of National Household Travel Survey (NHTS) 2017 - National coverage
# NHTS 2017 is available at https://nhts.ornl.gov/

# Naomi Panjaitan -- naomifp@lbl.gov
# Adapted from code for NHTS Bay Area data by Qianmiao (Michelle) Chen and NHTS Austin area by Connor P. Jackson

# May be run only in LBNL databucket with restricted-version NHTS 2017 for matching the census tractid ID with household ID


estimation_data_prep <- function(select.state.fips, select.county.fips) {
# the county and state fips are character codes, e.g. '001' for county and '06' for state (California)
  # select.county.fips <- '001'
  # select.state.fips <- '06'
  
  library(bit64)
  
#  estimation_data <- "G:/.shortcut-targets-by-id/1DEYFdNx9JRN3XaRK69_BmKY_2P19zxb1/Naomi/Project_Deliverable/Data/Estimation_Data/"
  
  # Define folders
  fips_folder  <- file.path(auxiliary_input_dir,'FIPS')
  perrent_folder <- file.path(auxiliary_input_dir,"ACS")
  access_folder  <- file.path(auxiliary_input_dir,"Accessibility_UMN")
  density_folder <- file.path(auxiliary_input_dir,"Jobs_Population_density")
  cnt_folder  <- file.path(auxiliary_input_dir,"CNT")
  sld_folder  <- file.path(auxiliary_input_dir,"Smart_Location_EPA")
  experian_folder <- file.path(auxiliary_input_dir,"Experian_2016")
  
  # load synthetic popualtion data
  #Load the synthetic population data (csv files)
  households <- fread(file = file.path(synthpop_input_dir, paste0("synthetic_households_", select.state.fips, "_", select.county.fips,"_",outputyear ,".csv")))
  persons <- fread(file = file.path(synthpop_input_dir, paste0("synthetic_persons_", select.state.fips, "_", select.county.fips,"_",outputyear ,".csv")))
  
  # Load datasets
  # households <- population_data[[1]]
  # persons    <- population_data[[2]]
  county_fips <- fread(file.path(fips_folder, "Clean/county_fips_name.csv"))
  perrent <- fread(file.path(perrent_folder, "Clean/percentage_rental_housing_2017.csv"))
  gems <- fread(file.path(density_folder, "Clean/microtypes_imputed.csv"))
  job <- fread(file.path(access_folder, "Clean/job_access.csv"))
  cnt <- fread(file.path(cnt_folder, "Clean/cnt_us.csv"))
  sld <- fread(file.path(sld_folder, "Clean/smart_location.csv"))
  experian <- fread(file.path(experian_folder, "Clean/Experian_US_2016_ZEV_pct.csv"))
  
  fips <- as.integer(paste0(select.state.fips, select.county.fips)) # 6001
  county <-  county_fips %>%
    filter(county_fips == fips) %>%
    pull(county_name) # county name
    
  # Prepare household data
  households <- as.data.frame(households)
  
  ## Create new variables
  
  # Tract ID
  households <- households %>% 
    rename(HOUSEID = household_id) %>% 
    mutate(
      .after=HOUSEID,
      state_fips = 6,
      state_name = "California",
      county_fips = fips,
      county_name = county
    ) %>% 
    mutate(
      .after=HOUSEID,
      tractid = as.integer64(county_fips*1000000 + as.integer(TRACT))
    )
  
  # INCOME
  #	INCOME1		    - Lowest income household (< $25,000)
  #	INCOME2		    - Low income household (25,000 <= & < $50,000)
  #	INCOME3		    - Middle income household (50,000 <= & < $75,000)
  #	INCOME4		    - High income household (75,000 <= & < $100,000)
  #	INCOME5		    - Highest income household (>= $100,000)
  households <- households %>% mutate(income1 = case_when(HINCP < 25000 ~ 1, TRUE~0),
                                      income2 = case_when(HINCP >= 25000 & HINCP < 50000 ~ 1, TRUE~0),
                                      income3 = case_when(HINCP >= 50000 & HINCP < 75000 ~ 1, TRUE~0),
                                      income4 = case_when(HINCP >= 75000 & HINCP < 100000 ~ 1, TRUE~0),
                                      income5 = case_when(HINCP >= 100000 ~ 1, TRUE~0))
  
  # HHSIZE
  #	HHSIZE1		    - Household size = 1
  #	HHSIZE2		    - Household size = 2
  #	HHSIZE3		    - Household size = 3
  #	HHSIZE4		    - Household size = 4 or more
  households <- households %>% mutate(HHSIZE = NUM_ADULTS + NOC) %>% 
    mutate(HHSIZE1 = case_when(HHSIZE == 1 ~ 1, TRUE~0),
           HHSIZE2 = case_when(HHSIZE == 2 ~ 1, TRUE~0),
           HHSIZE3 = case_when(HHSIZE == 3 ~ 1, TRUE~0),
           HHSIZE4 = case_when(HHSIZE >= 4 ~ 1, TRUE~0))
  
  #	Own		        - Household own the house (homeown)
  households <- households %>% mutate(hhown = case_when(TEN == 3 ~ 0, TRUE ~ 1))
  
  #	WORK0		      - Zero worker household
  #	WORK1		      - One worker household
  #	WORK2		      - Two worker household
  #	WORK3		      - Three or more worker household
  households <- households %>% mutate(work0 = case_when(NUM_WORKERS == 0 ~ 1, TRUE~0),
                                      work1 = case_when(NUM_WORKERS == 1 ~ 1, TRUE~0),
                                      work2 = case_when(NUM_WORKERS == 2 ~ 1, TRUE~0),
                                      work3 = case_when(NUM_WORKERS >= 3 ~ 1, TRUE~0))
  
  # Household Race
  households <- households %>% mutate(hhwhite  = case_when(HEAD_RACE == 1 ~ 1, TRUE~0),
                                      hhblack  = case_when(HEAD_RACE == 2 ~ 1, TRUE~0),
                                      hhasian  = case_when(HEAD_RACE == 6 ~ 1, TRUE~0),
                                      hhothers = case_when(HEAD_RACE != 1 & HEAD_RACE != 2 & HEAD_RACE != 6 ~ 1, TRUE~0))
  
  # Number of adults and children
  households <- households %>% 
    rename(NUMADLT  = NUM_ADULTS,
           NUMCHILD = NOC) %>% 
    mutate(.before = NUMCHILD, child = case_when(NUMCHILD > 0 ~ 1, TRUE ~ 0))
  
  # Life cycle
  households <- persons %>% 
    rename(HOUSEID = household_id) %>% 
    group_by(HOUSEID) %>% 
    summarize(minage = min(AGEP)) %>% 
    right_join(households, by = 'HOUSEID')
  
  households <- persons %>% 
    rename(HOUSEID = household_id) %>% 
    group_by(HOUSEID) %>% 
    summarize(seniors = sum(AGEP>=65)) %>% 
    right_join(households, by = 'HOUSEID')
  
  households <- households %>%
    mutate(
      LIF_CYC1 = as.integer(NUMADLT - seniors == 1 & NUMCHILD == 0),
      LIF_CYC2 = as.integer(NUMADLT - seniors >= 2 & NUMCHILD == 0),
      LIF_CYC3 = as.integer(NUMADLT - seniors == 1 & minage <= 5),
      LIF_CYC4 = as.integer(NUMADLT - seniors >= 2 & minage <= 5),
      LIF_CYC5 = as.integer(NUMADLT - seniors == 1 & minage >= 6 & minage <= 15),
      LIF_CYC6 = as.integer(NUMADLT - seniors >= 2 & minage >= 6 & minage <= 15),
      LIF_CYC7 = as.integer(NUMADLT - seniors == 1 & minage >= 16 & minage <= 21),
      LIF_CYC8 = as.integer(NUMADLT - seniors >= 2 & minage >= 16 & minage <= 21),
      LIF_CYC9 = as.integer(seniors == 1 & NUMCHILD == 0),
      LIF_CYC10 = as.integer(seniors >= 2 & NUMCHILD == 0)
    )
  
  # Retired
  households <- households %>% mutate(retired = case_when(LIF_CYC9 == 1 | LIF_CYC10 == 1 ~ 1, TRUE~0))
  
  
  # REMOVE COLUMNS
  households <- households %>% select(-c(seniors, minage, PUMA, TRACT, NP, HINCP, ADJINC, TEN, HEAD_RACE, BLD))
  
  
  # Merge perrent (precentage of rental housing in the tract)
  data <- households %>% 
    left_join(perrent, by = "tractid") %>% 
    mutate(perrent1 = case_when(perrent < 25 ~ 1, TRUE~0),
           perrent2 = case_when(perrent < 45 & perrent >=25 ~ 1, TRUE~0),
           perrent3 = case_when(perrent >= 45  ~ 1, TRUE~0))
  
  # Merge job accessibility
  data <- data %>% 
    left_join(job, by = "tractid") %>%
    mutate(urban_cbsa = ifelse(is.na(tractmean), 0, 1)) %>%
    # handle NAs in variables related to job access; 0 because they belong to tracts without transit service / not in urban area
    replace_na(list(tractmean = 0, emp_zscore = 0))
  
  # Merge GEMS data set
  data <- gems %>% 
    select(c("tractid", "pop_density", "job_density", "pct_ag_land", "pct_water")) %>% 
    right_join(data, by="tractid") %>% 
    relocate(c(job_density, pop_density, pct_ag_land, pct_water), .after = everything()) 
  data <- data %>% 
    mutate(.after = pop_density,
           log_job_density = log(data$job_density+1),
           log_pop_density = log(pop_density+1),
           log_pct_agland  = log(pct_ag_land+0.0000001),
           log_pct_water   = log(pct_water  +0.0000001))
  data <- data %>% 
    mutate(.after = log_pop_density,
           log_pop_below3 = ifelse(log_pop_density<3, 1, 0),
           log_pop_above9 = ifelse(log_pop_density>9, 1, 0),
           log_job_below4 = ifelse(log_job_density<4, 1, 0),
           log_job_above8 = ifelse(log_job_density>8, 1, 0))
  
  # Merge CNT data set
  data <- cnt %>% 
    select(c("tractid", "tas_acres", "tci", "compact_ndx", "transit_performance_score")) %>% 
    right_join(data, by="tractid") %>% 
    relocate(c(tas_acres, tci, compact_ndx, transit_performance_score), .after = everything()) %>% 
    # handle NAs in transit-related variables; 0 because they belong to tracts without transit service
    replace_na(list(tas_acres = 0,
                    tci = 0,
                    compact_ndx = 0,
                    transit_performance_score = 0)) %>% 
    mutate(hi_tps = ifelse(transit_performance_score>=8, 1, 0))
  
  # Merge SLD data set
  data <- sld %>%
    select(tractid, walkndx, res_density) %>% 
    right_join(data, by="tractid") %>% 
    relocate(c(walkndx, res_density), .after = everything()) 
  data <- data %>%
    mutate(
      .after = res_density,
      log_res_density = log(res_density + 0.000001)
    )
  
  # Merge %ZEV in the area from Experian data set
  data <- experian %>%
    right_join(data, by = c("state_name", "county_name")) %>%
    relocate(lastyear_zev_pct, .after = everything())
  data <- data %>%
    mutate(
      .after = lastyear_zev_pct,
      log_lastyear_zevpct = log(lastyear_zev_pct + 0.000001)
    )
  
  
  # Save household data set
  fwrite(data, file = file.path(outputdir,paste0('processed_synthpop',select.state.fips, "_", select.county.fips,"_",outputyear ,".csv")))
  
  return(data)
}



