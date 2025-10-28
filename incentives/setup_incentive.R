# incentive related global variables/data

# for local test
# setwd('~/Library/CloudStorage/GoogleDrive-ljin@lbl.gov/Shared\ drives/Shared_ATLAS/fromLing/SoftwareDevelopment/sfb_atlas_v2/code_inside_container')
# inputdir = '../atlas_input'
# discIncTF = T
# read in cpi again as the previous cpi is a scaler value in data cleaning step
cpi <- fread(file.path(inputdir,"cpi.csv")) # ratio between 2019 and 2010 to convert demos 2010 dollars to 2019 used in psid estiamtes



if(!("data.table" %in% tolower((.packages())))){library(data.table)}
if(!("dplyr" %in% tolower((.packages())))){library(data.table)}

# whether deflate incentives from command line argument "discIncent"
deflate_incentives <- discIncent


literature_dir <- file.path("incentives") # the incentives folder is in the code dir
fed_poverty <- fread(file.path(literature_dir, "federal_pov_guidelines.csv"))
federal_tax <- fread(file.path(literature_dir, "federal_marginal_tax.csv"))
hud_low_inc <- fread(file.path(literature_dir, "hud_low_income_thresh_2022.csv"), header = TRUE)

federal_tax[, roll_income := income]
setnames(federal_tax, "income", "income_bracket")
setkey(federal_tax, roll_income)

hud_low_inc <- melt(hud_low_inc, id.vars = c("county", "county_fips"), variable.name = "hhsize", 
                    value.name="income", variable.factor = FALSE)
hud_low_inc[, hhsize := as.integer(hhsize)]

if (deflate_incentives) {
  # Low income thresholds in 2022 dollars. Deflate to 2019
  deflator_2022 <- cpi[Year == 2022, ratio_2019]
  hud_low_inc[, income := deflator_2022 * income]
}

hud_low_inc_4 <- hud_low_inc[hhsize == "4", .(county_fips, income)]
hud_low_inc_8 <- hud_low_inc[hhsize == "8", .(county_fips, income)]



