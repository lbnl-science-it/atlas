## Functions to apply the financial incentives based on vehicle and HH attributes

##TODO: Deflate credit/rebate amounts that are fixed in nominal dollars

## ---- preliminary-steps ----

# these libs are already loaded in the previous code, LJ 3/29/2023

federal_tax_liability <- function(income, hhsize) {
  # use marginal tax brackets, standard deduction and household income to 
  # estimate federal tax liability
  require(data.table)
  require(dplyr)
  
  
  std_deduction <- 12950
  income <- income - std_deduction
  income[income < 0] <- 0
  data <- data.table(roll_income=income, income=income, hhsize=hhsize)
  
  # for joint filing households, the marginal brackets are twice as high. For ease 
  # of joining, instead make the joining income half as large
  data[hhsize > 1, roll_income := roll_income / 2]
  setindex(data, roll_income)
  liability <- 
    federal_tax[data, ((accumulated_tax + (roll_income - income_bracket) * marginal_rate) * 
                         ifelse(hhsize > 1, 2, 1)), roll=TRUE, on="roll_income"]
  liability[liability < 0] <- 0
  return(round(liability, 0))
}

federal_poverty <- function(hhsize) {
  require(data.table)
  require(dplyr)
  
  return(fed_poverty[year == 2019, first_person] + fed_poverty[year == 2019, addl_person] * hhsize)
}

old_federal_credit <- function(data) {
  # BEV, PHEV: min battery 5 kWh
  # new vehicles
  # $417 * (kWh - 5), max 7500
  require(data.table)
  require(dplyr)
  
  if (simyear > 2022) return(0)
  
  credit <- (data$adopt_fuel %in% c("phev", "ev")) * (data$bev_energy >= 5)
  credit <- credit * pmin(2.5 + 0.417*(data$bev_energy - 5), 7.5)
  
  # Phase out credit for cars and SUVs because Tesla
  credit <- credit * (1 - (data$adopt_veh %in% c("car", "suv")) * (data$year >= 2019))
  return(credit)
}

ira_federal_new_credit <- function(data) {
  # BEV, PHEV
  require(data.table)
  require(dplyr)
  
  if (simyear < 2023) return(0)
  if (simyear > 2032) return(0)
  
  credit <- (data$adopt_fuel %in% c("phev", "ev")) * (data$bev_energy >= 7) * 7.5
  # msrp < 80k (vans, suvs, trucks), 55k (cars)
  credit <- credit * (data$price <= ifelse(data$adopt_veh == "car", 55, 80))
  # HH income: 300k with >= 2 members, 150k single HH
  credit <- credit * (data$income_fu <= ifelse(data$size_fu == 1, 150000, 300000))
  return(credit)
}

ira_federal_used_credit <- function(data) {
  # BEV, PHEV, Fuel Cell
  require(data.table)
  require(dplyr)
  
  if (simyear < 2023) return(0)
  if (simyear > 2032) return(0)
  
  credit <- (data$adopt_fuel %in% c("phev", "ev", "fuelcell")) * (data$bev_energy >= 7)
  credit <- credit * pmin(data$price * 0.3, 4)
  # HH income: 150k with >= 2 members, 75k single HH
  credit <- credit * (data$income_fu <= ifelse(data$size_fu == 1, 75000, 150000))
  # price < 25k
  credit <- credit * (data$price <= 25)
  # model year between 2 and X years old (choose age cap)
  
  return(credit)
}

state_cvrp_rebate <- function(data) {
  # new vehicles only
  require(data.table)
  require(dplyr)
  
   
  below_pov <- data$income_fu < 4 * federal_poverty(data$size_fu)
  credit <- dplyr::case_when(below_pov & (data$adopt_fuel == "phev") ~ 6.5,
                             below_pov & (data$adopt_fuel == "ev") ~ 7.5,
                             below_pov & (data$adopt_fuel == "fuelcell") ~ 7.5,
                             !below_pov & data$adopt_fuel == "phev" ~ 1,
                             !below_pov & data$adopt_fuel == "ev" ~ 2,
                             !below_pov & data$adopt_fuel == "fuelcell" ~ 4.5,
                             TRUE ~ 0)
  # beginning 2022: MSRP caps: 45k (cars), 60k (others), hydrogen exempt
  if (simyear >= 2022) {
    credit <- credit * ((data$price <= ifelse(data$adopt_veh == "car", 45, 60)) | data$adopt_fuelfuelcell)
  }
  return(credit)
}

local_pge_rebate <- function(data) {
  # beginning 2023, $1000 credit for used BEV and PHEV
  require(data.table)
  require(dplyr)
  # LJ 4/6/2023, looks like the lookup table only goes up to 8 person hh, which will make the >8 pp hh return NA.
  data$size_fu[data$size_fu>8] <- 8
  
  if (simyear < 2023) return(0)
  
  credit <- as.integer(data$adopt_fuel %in% c("phev", "ev"))
  
  # County HUD Low Income gets 4000 (assume threshold is fixed in real dollars)
  ordered_county_hhsize <- data.table(county_fips=as.integer(substr(data$tract_geoid, 1, 4)), 
                                      hhsize=data$size_fu)
  income_thresh <- hud_low_inc[ordered_county_hhsize, income, on=c("county_fips", "hhsize")]
  income_mult <- 1 + (3 * (data$income_fu <= income_thresh))
  credit <- credit * income_mult
  return(credit)
}

apply_new_credits <- function(data) {
  require(data.table)
  require(dplyr)
  
  credits <- old_federal_credit(data) + ira_federal_new_credit(data)
  credits <- pmin(credits, federal_tax_liability(data$income_fu, data$size_fu) / 1000)
  return(credits)
}

apply_new_rebates <- function(data) {
  require(data.table)
  require(dplyr)
  
  rebates <- state_cvrp_rebate(data) + local_pge_rebate(data)
  return(rebates)
}

apply_used_credits <- function(data) {
  require(data.table)
  require(dplyr)
  
  credits <- ira_federal_used_credit(data)
  credits <- pmin(credits, federal_tax_liability(data$income_fu, data$size_fu) / 1000)
  return(credits)
}

apply_used_rebates <- function(data) {
  rebates <- local_pge_rebate(data)
  return(rebates)
}

##### EXAMPLE CODE:
# simyear <- 2017
# data_folder <- "/Users/connor/cpjackson@berkeley.edu - Google Drive/My Drive/2021-Transportation/Data"
# new_data <- file.path(data_folder, "../Connor/fromLing/Data/atlas_output/rdata/example.new.veh.RData")
# used_data <- file.path(data_folder, "../Connor/fromLing/Data/atlas_output/rdata/example.used.veh.RData")
# load(new_data)
# load(used_data)
# vehmodepredict.new$bev_energy <- 80
# vehmodepredict.used$bev_energy <- 80
# 
# vehmodepredict.new$tax_credit <- apply_new_credits(vehmodepredict.new)
# vehmodepredict.new$rebate <- apply_new_rebates(vehmodepredict.new)
# vehmodepredict.used$tax_credit <- apply_used_credits(vehmodepredict.used)
# vehmodepredict.used$rebate <- apply_used_rebates(vehmodepredict.used)

