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
