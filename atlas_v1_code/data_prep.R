# ATLAS Vehicle Fleet Composition 
# Data cleaning
# Estimation data

# Restricted-version of National Household Travel Survey (NHTS) 2017 - National coverage
# NHTS 2017 is available at https://nhts.ornl.gov/

# Naomi Panjaitan -- naomifp@lbl.gov
# Adapted from code for NHTS Bay Area data by Qianmiao (Michelle) Chen and NHTS Austin area by Connor P. Jackson

# May be run only in LBNL databucket with restricted-version NHTS 2017 for matching the census tractid ID with household ID


# LJ 10/18/2025, move the dir path outside of the functions
estimation_data <- "/Users/lingjin/Library/CloudStorage/GoogleDrive-ljin@lbl.gov/Shared drives/Shared_ATLAS/Naomi/Project_Deliverable/Data/Estimation_Data/"

estimation_data_prep <- function(population_data, county) {
  library(bit64)
  
#  estimation_data <- "G:/.shortcut-targets-by-id/1DEYFdNx9JRN3XaRK69_BmKY_2P19zxb1/Naomi/Project_Deliverable/Data/Estimation_Data/"
  
  # Define folders
  fips_folder  <- paste0(estimation_data, "Auxiliary_Data/FIPS/")
  perrent_folder <- paste0(estimation_data, "Auxiliary_Data/ACS/")
  access_folder  <- paste0(estimation_data, "Auxiliary_Data/Accessibility_UMN/")
  density_folder <- paste0(estimation_data, "Auxiliary_Data/Jobs_Population_density/")
  cnt_folder  <- paste0(estimation_data, "Auxiliary_Data/CNT/")
  sld_folder  <- paste0(estimation_data, "Auxiliary_Data/Smart_Location_EPA/")
  experian_folder <- paste0(estimation_data, "Survey_Data/Experian_2016/")
  
  # Load datasets
  households <<- population_data[[1]]
  persons    <<- population_data[[2]]
  county_fips <- fread(paste0(fips_folder, "Clean/county_fips_name.csv"))
  perrent <- fread(paste0(perrent_folder, "Clean/percentage_rental_housing_2017.csv"))
  gems <- fread(paste0(density_folder, "Clean/microtypes_imputed.csv"))
  job <- fread(paste0(access_folder, "Clean/job_access.csv"))
  cnt <- fread(paste0(cnt_folder, "Clean/cnt_us.csv"))
  sld <- fread(paste0(sld_folder, "Clean/Smart_location.csv"))
  experian <- fread(paste0(experian_folder, "Clean/Experian_US_2016_ZEV_pct.csv"))
  
  fips <<- county_fips[county_name == county, county_fips]
  
  # Prepare household data
  source("code/data_clean.R")
  
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
  fwrite(data, file = paste0("data/synthetic_population_", county, ".csv"))
  
  return(data)
}


#Load the synthetic population data (csv files)
csv_files <- list.files(path = paste0(data_folder, "data/"), pattern = "\\.csv$", full.names = TRUE)
population_data <- lapply(csv_files, fread)

# Generate the data  
data <- estimation_data_prep(population_data, county)
data <- as.data.frame(data)
rm(households)
rm(persons)

