## Functions to apply the financial incentives based on vehicle and HH attributes


## ---- preliminary-steps ----

# these libs are already loaded in the previous code, LJ 3/29/2023


federal_tax_liability <- function(income, spouseTF, std_deduction=12950) {
  # use marginal tax brackets, standard deduction and household income to 
  # estimate federal tax liability. Standard deduction is in real dollars
  income <- income - std_deduction
  income[income < 0] <- 0
  data <- data.table(roll_income=income, income=income, spouseTF=spouseTF)
  
  # for joint filing households, the marginal brackets are twice as high. For ease 
  # of joining, instead make the joining income half as large
  data[spouseTF == 1, roll_income := roll_income / 2]
  setindex(data, roll_income)
  liability <- 
    federal_tax[data, ((accumulated_tax + (roll_income - income_bracket) * marginal_rate) * 
                         ifelse(spouseTF == 1, 2, 1)), roll=TRUE, on="roll_income"]
  liability[liability < 0] <- 0
  return(round(liability, 0))
}

federal_poverty <- function(hhsize) {
  return(fed_poverty[year == 2019, first_person] + fed_poverty[year == 2019, addl_person] * hhsize)
}

old_federal_credit <- function(data) {
  # BEV, PHEV: min battery 5 kWh
  # new vehicles
  # $417 * (kWh - 5), max 7500
  if (simyear > 2022) return(0)
  
  credit <- (data$adopt_fuel %in% c("phev", "ev")) * (data$bev_energy >= 5)
  credit <- credit * pmin(2.5 + 0.417*(data$bev_energy - 5), 7.5)
  
  # Phase out credit for cars and SUVs because Tesla
  credit <- credit * (1 - (data$adopt_veh %in% c("car", "suv")) * (data$year >= 2019))
  
  if (deflate_incentives) {
    # Convert nominal credit amount to 2019 (real) dollars
    credit <- cpi[Year == simyear, ratio_2019] * credit
  }
  return(credit)
}

ira_fed_new_credit_income <- function(income, spouse, single_inc_thresh=150000,
                                      joint_inc_thresh=300000) {
  # deflate income caps that are fixed in nominal dollars
  cpi_coef <- ifelse(deflate_incentives, cpi[Year == simyear, ratio_2019], 1)
  return(income <= (cpi_coef * ifelse(spouse == 0, single_inc_thresh, joint_inc_thresh)))
}

ira_federal_new_credit <- function(data, endyear=2032, credit_amount=7.5, 
                                   single_inc_thresh=150000, joint_inc_thresh=300000,
                                   car_msrp_thresh=55, truck_msrp_thresh=80) {
  # Tax Credit for new vehicles modeled after the Federal Tax Credit in the 
  # Inflation Reduction Act of 2022.
  # Inputs: 
  # - data: data.frame or data.table of choice alternatives
  # - endyear: scalar simulation year cutoff 
  # - credit_amount: either scalar or vector of the same length as data, credit value
  #                   in thousands of dollars. Default=7.5 per IRA
  # - single_inc_thresh, joint_inc_thresh: either scalar or vector of the same length
  #                   as data, threshold income for single and joint filers in dollars.
  #                   Defaults per IRA.
  # - car_msrp_thresh, truck_msrp_thresh: either scalar or vector of the same length
  #                   as data, threshold price for cars and all other body types, in
  #                   thousands of dollars. Defaults per IRA.
  
  if (simyear < 2023) return(0)
  if (simyear > endyear) return(0)
  
  cpi_coef <- ifelse(deflate_incentives, cpi[Year == simyear, ratio_2019], 1)
  
  credit <- (data$adopt_fuel %in% c("phev", "ev")) * (data$bev_energy >= 7) * credit_amount
  # msrp < 80k (vans, suvs, trucks), 55k (cars) (deflate cap fixed in nominal dollars)
  credit <- credit * (data$price <= (cpi_coef * ifelse(data$adopt_veh == "car", 
                                                       car_msrp_thresh, truck_msrp_thresh)))
  # HH income: 300k with spouse, 150k single HH
  credit <- credit * ira_fed_new_credit_income(data$income_fu, data$spouseTF, 
                                               single_inc_thresh, joint_inc_thresh)
  if (deflate_incentives) {
    # Convert nominal credit amount to 2019 (real) dollars
    credit <- cpi[Year == simyear, ratio_2019] * credit
  }
  return(credit)
}

ira_fed_used_credit_income <- function(income, spouse, single_inc_thresh=75000,
                                       joint_inc_thresh=150000) {
  # deflate income caps that are fixed in nominal dollars
  cpi_coef <- ifelse(deflate_incentives, cpi[Year == simyear, ratio_2019], 1)
  return(income <= (cpi_coef * ifelse(spouse == 0, single_inc_thresh, joint_inc_thresh)))
}

ira_federal_used_credit <- function(data, endyear=2032, credit_cap=4, price_percent=0.3,
                                    single_inc_thresh=75000, joint_inc_thresh=150000,
                                    price_cap=25) {
  # Tax Credit for used vehicles modeled after the Federal Tax Credit in the 
  # Inflation Reduction Act of 2022.
  # Inputs: 
  # - data: data.frame or data.table of choice alternatives
  # - endyear: scalar simulation year cutoff 
  # - credit_cap: either scalar or vector of the same length as data, maximum credit
  #                   in thousands of dollars. Default = 4 per IRA
  # - price_percent: either scalar or vector of the same length as data, decimal
  #                   proportion of the vehicle price awarded as the tax credit. 
  #                   Default = 0.3 per IRA.
  # - single_inc_thresh, joint_inc_thresh: either scalar or vector of the same length
  #                   as data, threshold income for single and joint filers in dollars.
  #                   Defaults per IRA.
  # - car_msrp_thresh, truck_msrp_thresh: either scalar or vector of the same length
  #                   as data, threshold price for cars and all other body types, in
  #                   thousands of dollars. Defaults per IRA. 
  # - price_cap: either scalar or vector of the same length as data, maximum vehicle
  #               price in thousands of dollars. Default = 25 per IRA.
  if (simyear < 2023) return(0)
  if (simyear > endyear) return(0)
  
  cpi_coef <- ifelse(deflate_incentives, cpi[Year == simyear, ratio_2019], 1)
  
  credit <- (data$adopt_fuel %in% c("phev", "ev", "fuelcell")) * (data$bev_energy >= 7)
  # deflate credit cap that is fixed in nominal dollars
  credit <- credit * pmin(data$price * price_percent, cpi_coef * credit_cap)
  # HH income
  credit <- credit * ira_fed_used_credit_income(data$income_fu, data$spouseTF, 
                                                single_inc_thresh, joint_inc_thresh)
  # price < cap (deflate price cap that is fixed in nominal dollars)
  credit <- credit * (data$price <= (cpi_coef * price_cap))
  # model year between 2 and X years old (choose age cap)
  if (deflate_incentives) {
    # Convert nominal credit amount to 2019 (real) dollars
    credit <- cpi[Year == simyear, ratio_2019] * credit
  }
  return(credit)
}

cvrp_rebate_income <- function(income, hhsize, pov_multiplier=4) {
  return(income < pov_multiplier * federal_poverty(hhsize))
}
state_cvrp_rebate <- function(data, pov_multiplier=4, msrp_thresholds=TRUE, 
                              car_msrp_thresh=45, truck_msrp_thresh=60) {
  # Rebate modeled after CA Clean Vehicle Rebate Program
  # Inputs: 
  # - data: data.frame or data.table of choice alternatives
  # - pov_multiplier: either scalar or vector of the same length as data, 
  #                   eligibility multiplier of federal poverty line. Default = 4
  #                   per CVRP. 
  # - msrp_thresholds: either scalar or vector of the same length as data, boolean
  #                     to enable/disable MSRP thresholds
  # - car_msrp_thresh, truck_msrp_thresh: either scalar or vector of the same length
  #                   as data, threshold price for cars and all other body types, in
  #                   thousands of dollars. Defaults per CVRP.  
  below_pov <- cvrp_rebate_income(data$income_fu, data$size_fu, pov_multiplier)
  rebate <- dplyr::case_when(below_pov & (data$adopt_fuel == "phev") ~ 6.5,
                             below_pov & (data$adopt_fuel == "ev") ~ 7.5,
                             below_pov & (data$adopt_fuel == "fuelcell") ~ 7.5,
                             !below_pov & data$adopt_fuel == "phev" ~ 1,
                             !below_pov & data$adopt_fuel == "ev" ~ 2,
                             !below_pov & data$adopt_fuel == "fuelcell" ~ 4.5,
                             TRUE ~ 0)
  
  cpi_coef <- ifelse(deflate_incentives, cpi[Year == simyear, ratio_2019], 1)
  
  # beginning 2022: MSRP caps: 45k (cars), 60k (others), hydrogen exempt
  # deflate price caps that are fixed in nominal dollars
  if ((simyear >= 2022) & (msrp_thresholds == TRUE)) {
    rebate <- rebate * ((data$price <= (cpi_coef * ifelse(data$adopt_veh == "car", 
                                                          car_msrp_thresh, truck_msrp_thresh))) | 
                          data$adopt_fuelfuelcell)
  }
  if (deflate_incentives) {
    # Convert nominal rebate amount to 2019 (real) dollars
    rebate <- cpi[Year == simyear, ratio_2019] * rebate
  }
  return(rebate)
}

pge_rebate_income <- function(income, geoid, hhsize) {
  # County HUD Low Income gets 4000 (assume threshold is fixed in real dollars)
  ordered_county_hhsize <- data.table(county_fips=as.integer(substr(geoid, 1, 4)), hhsize=hhsize)
  income_thresh <- hud_low_inc[ordered_county_hhsize, income, on=c("county_fips", "hhsize")]
  
  # Manual formula for households over 8 members: 
  # 8% of 4-person HH income * number of HH members over 8, added to the 8-person HH income
  income_thresh[is.na(income_thresh)] <- 
    (((hud_low_inc_4[ordered_county_hhsize[hhsize > 8], income, on = "county_fips"] * 0.08) * 
        ordered_county_hhsize[hhsize > 8, hhsize - 8]) + 
       hud_low_inc_8[ordered_county_hhsize[hhsize > 8], income, on = "county_fips"])
  return(income < income_thresh)
}

local_pge_rebate <- function(data, base_credit=1, addl_credit=3) {
  # Rebate modeled after PG&E Clean Vehicle Rebate
  # Inputs: 
  # - data: data.frame or data.table of choice alternatives
  # - base_credit, addl_credit: either scalars or vectors of the same length as 
  #                   data, credit amounts in thousands of dollars. Defaults per PG&E. 
  
  # # LJ 4/6/2023, looks like the lookup table only goes up to 8 person hh, which will make the >8 pp hh return NA.
  # LJ 6/20/2023 this error has been manually fixed in the function, no need to adjust here
  # data$size_fu[data$size_fu>8] <- 8
  
  if (simyear < 2023) return(0)
  
  rebate <- as.integer(data$adopt_fuel %in% c("phev", "ev"))
  income_mult <- base_credit + (addl_credit * pge_rebate_income(data$income_fu, 
                                                                data$tract_geoid, data$size_fu))
  rebate <- rebate * income_mult
  if (deflate_incentives) {
    # Convert nominal rebate amount to 2019 (real) dollars
    rebate <- cpi[Year == simyear, ratio_2019] * rebate
  }
  return(rebate)
}

apply_new_credits <- function(data) {
  credits <- old_federal_credit(data) + ira_federal_new_credit(data)
  credits <- pmin(credits, federal_tax_liability(data$income_fu, data$spouseTF) / 1000)
  return(credits)
}

apply_new_rebates <- function(data) {
  rebates <- state_cvrp_rebate(data) + local_pge_rebate(data)
  return(rebates)
}

apply_used_credits <- function(data) {
  credits <- ira_federal_used_credit(data)
  credits <- pmin(credits, federal_tax_liability(data$income_fu, data$spouseTF) / 1000)
  return(credits)
}

apply_used_rebates <- function(data) {
  rebates <- local_pge_rebate(data)
  return(rebates)
}

##### EXAMPLE CODE:
# simyear <- 2017
# data_folder <- "/Users/lingjin/Library/CloudStorage/GoogleDrive-ljin@lbl.gov/Shared\ drives/Shared_ATLAS/fromLing/SoftwareDevelopment/sfb_atlas_v2/atlas_output/rdata"
# new_data <- file.path(data_folder, "example.new.veh.RData")
# used_data <- file.path(data_folder, "example.used.veh.RData")
# load(new_data)
# load(used_data)
# vehmodepredict.new$bev_energy <- 80
# vehmodepredict.used$bev_energy <- 80
# # 
# vehmodepredict.new$tax_credit <- apply_new_credits(vehmodepredict.new)
# vehmodepredict.new$rebate <- apply_new_rebates(vehmodepredict.new)
# vehmodepredict.used$tax_credit <- apply_used_credits(vehmodepredict.used)
# vehmodepredict.used$rebate <- apply_used_rebates(vehmodepredict.used)
# 
