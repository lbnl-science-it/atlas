# incentive related global variables/data

if(!("data.table" %in% tolower((.packages())))){library(data.table)}
if(!("dplyr" %in% tolower((.packages())))){library(data.table)}


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

