# Final Output Visualization
# do it after the source_us_atlas_v1.R on mac

imgdir <- "/Users/lingjin/Dropbox/Research/SmartGrid_Behavioral/TransportationInitiative/ATLAS/ATLAS-National/Rscripts/Software/figures"
state.fips = '06'
# get the list of counties to run
synthpop.files <- list.files(
  path = synthpop_input_dir,
  pattern = paste0("^synthetic_households_.*\\.csv$"),
  full.names = FALSE
)
all.countyfips <- substr(synthpop.files, nchar('synthetic_households_06_')+1, nchar('synthetic_households_06_001'))

for(countyfips in all.countyfips){
  # Loading ATLAS output data
  households_output <- fread(paste0(outputdir, paste0("/atlas_hh_06_",countyfips,".csv")))
  setDT(households_output)
  
  vehicles_output <- fread(paste0(outputdir, paste0("/atlas_veh_06_",countyfips,".csv")))
  setDT(vehicles_output)
  
  data_temp <- fread(paste0(outputdir, paste0("/prediction_results_06_",countyfips,".csv")))
  setDT(data_temp)
  
  
  
  # Loading observed data (ACS)
  validation_data <- "/Users/lingjin/Library/CloudStorage/GoogleDrive-ljin@lbl.gov/Shared drives/Shared_ATLAS/Naomi/Project_Deliverable/Data/Validation_Data/"
  acs_veh  <- fread(paste0(validation_data, "ACS/2017/Clean/acs2017_veh_hhsize.csv"))
  experian_ca <- fread(paste0(validation_data, "Vehicle_Registration/Experian_US/Clean/Experian_CA_2017.csv"))
  
  # note: data_folder, tabdir, imgdir have been set from init.R
  
  # Vehicle Ownership ####
  
  # Validation of vehicle ownership uses ACS
  
  # Extract predicted number of vehicles ATLAS household_output 
  num_veh_pred <- households_output %>%
    group_by(nvehicles) %>% 
    summarize(Predicted = n())
  
  # Sum all predicted values from 4 to 13 vehicles
  sum_4plus <- num_veh_pred %>%
    filter(nvehicles >= 4) %>%
    summarize(Predicted = sum(Predicted)) %>%
    pull(Predicted)
  
  # Replace the value for nvehicles == 4 with the sum
  num_veh_pred <- num_veh_pred %>%
    mutate(Predicted = ifelse(nvehicles == 4, sum_4plus, Predicted),
           nvehicles = as.integer(nvehicles)) %>%
    # Optionally filter out rows > 4 since they've been aggregated
    filter(nvehicles <= 4)
  
  # Extract observed number of vehicles from ACS
  num_veh_obs <- acs_veh %>% 
    filter(county_fips == as.integer(paste0(state_fips,countyfips)), state_name == "California") %>% 
    select(veh0, veh1, veh2, veh3, veh4) %>% 
    summarize(
      veh0 = sum(veh0, na.rm = TRUE),
      veh1 = sum(veh1, na.rm = TRUE),
      veh2 = sum(veh2, na.rm = TRUE),
      veh3 = sum(veh3, na.rm = TRUE),
      veh4 = sum(veh4, na.rm = TRUE)
    ) %>% 
    pivot_longer(
      cols = everything(),
      names_to = "nvehicles",
      values_to = "Observed"
    ) %>% 
    mutate(nvehicles = as.integer(str_extract(nvehicles, "\\d")))
  
  # Merge the observed and the predicted data frames
  plot.hh.own <- left_join(num_veh_obs, num_veh_pred, by = "nvehicles")
  # plot.hh.own <- plot.hh.own %>% filter(nvehicles>0)
  
  plot.hh.own <- plot.hh.own %>% 
    pivot_longer(
      cols = c("Predicted", "Observed"),
      names_to = "cat",
      values_to = "num"
    )
  
  # # Make level of factors
  # plot.hh.own$veh <- factor(plot.hh.own$veh, levels = c("0", "1", "2", "3", "4+"))
  
  # Add the percentage of household 
  plot.hh.own <- plot.hh.own %>% 
    group_by(cat) %>% 
    mutate(pct = num / sum(num) * 100)
  
  # Plot vehicle ownership among households in California by county
  p_hh_own <- plot.hh.own %>% 
    ggplot(aes(y = pct, x = nvehicles, fill = cat)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.6) +
    labs(title = "Distribution of Number of Vehicles Owned", 
         x = NULL, y = "Percentage of Household", fill = "") +
    theme_bw() +
    theme(legend.position = "bottom")
  
  
  
  
  # Vehicle Type ####
  
  # Observed data
  library(readxl)
  xwalk_bodytype <- read_excel("/Users/lingjin/Library/CloudStorage/GoogleDrive-ljin@lbl.gov/Shared drives/Shared_ATLAS/Naomi/Documentation/xwalk/xwalk-experian-atlas.xlsx",
                               sheet = "vehtype-experian-atlas")
  colnames(xwalk_bodytype) <- c("LBL_TYPE", "bodytype")
  
  # LJ change 10/15/2025: we would use the experian-atlas xwalk
  # xwalk_powertrain <- read_excel("/Users/lingjin/Library/CloudStorage/GoogleDrive-ljin@lbl.gov/Shared drives/Shared_ATLAS/Naomi/Documentation/powertrain-xwalk-validationsets.xlsx",
  #                                sheet = "fueltype-experian-common")
  # colnames(xwalk_powertrain) <- c("EPA_FUEL", "power")
  xwalk_powertrain <- read_excel("/Users/lingjin/Library/CloudStorage/GoogleDrive-ljin@lbl.gov/Shared drives/Shared_ATLAS/Naomi/Documentation/powertrain-xwalk-validationsets.xlsx",
                                 sheet = "fueltype-experian-atlas")
  colnames(xwalk_powertrain) <- c("EPA_FUEL", "power")
  
  veh_obs <- experian_ca %>% 
    filter(COUNTY_CODE == as.integer(countyfips), state_name == "California") %>% 
    filter(!is.na(VEHAGE)) %>% 
    mutate(VEHAGE = ifelse(VEHAGE == 0, 1, VEHAGE)) %>% 
    left_join(xwalk_bodytype, by = "LBL_TYPE") %>% 
    left_join(xwalk_powertrain, by = "EPA_FUEL") %>% 
    mutate(vintage = case_when(VEHAGE<6 ~ "0~5 years", 
                               VEHAGE>5 & VEHAGE<12 ~ "6~11 years",
                               VEHAGE>11 ~ "12+ years"),
           cat = "Observed") %>% 
    filter(bodytype %in% c("car", "van", "suv", "pickup")) %>% 
    select(bodytype, power, vintage, cat, VEHICLE_COUNT)
  
  
  
  # Predicted data
  veh_pred <- vehicles_output %>% 
    rename(power = pred_power, vintage = vintage_category) %>% 
    group_by(bodytype, power, vintage) %>% 
    summarize(VEHICLE_COUNT = n(), .groups = "drop") %>% 
    mutate(cat = "Predicted") %>% 
    select(bodytype, power, vintage, cat, VEHICLE_COUNT)
  
  
  
  ## Body type ####
  
  veh_pred_bodytype <- veh_pred %>% 
    group_by(bodytype) %>% 
    summarize(VEHICLE_COUNT = sum(VEHICLE_COUNT)) %>% 
    mutate(cat = "Predicted")
  
  veh_obs_bodytype <- veh_obs %>% 
    group_by(bodytype) %>% 
    summarize(VEHICLE_COUNT = sum(VEHICLE_COUNT)) %>% 
    mutate(cat = "Observed")
  
  plot.veh.type <- rbind(veh_obs_bodytype, veh_pred_bodytype)
  
  
  # Add the percentage 
  plot.veh.type <- plot.veh.type %>% 
    group_by(cat) %>% 
    mutate(pct = VEHICLE_COUNT / sum(VEHICLE_COUNT) * 100) 
  
  # Make level of the vehicle as factors
  plot.veh.type$bodytype <- factor(plot.veh.type$bodytype, 
                                   levels = c("car", "van", "suv", "pickup"))
  
  # Plot vehicle type distribution among households in California
  p_veh_body <- plot.veh.type %>% 
    ggplot(aes(y = pct, x = bodytype, fill = cat)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.6) +
    labs(title = "Vehicle Body Type Distribution", 
         x = NULL, y = "Percentage of Vehicle", fill = "") +
    scale_x_discrete(label = c("Car", "Van", "SUV", "Pick-up Truck")) +
    theme_bw() +
    theme(
      # axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      legend.position = "bottom")
  
  
  ## Body type & Vintage ####
  
  veh_pred_bodyvintage <- veh_pred %>% 
    mutate(veh = case_when(bodytype == "car" & vintage == "0~5 years" ~ "car1",
                           bodytype == "car" & vintage == "6~11 years" ~ "car2",
                           bodytype == "car" & vintage == "12+ years" ~ "car3",
                           bodytype == "van" & vintage == "0~5 years" ~ "van1",
                           bodytype == "van" & vintage == "6~11 years" ~ "van2",
                           bodytype == "van" & vintage == "12+ years" ~ "van3",
                           bodytype == "suv" & vintage == "0~5 years" ~ "suv1",
                           bodytype == "suv" & vintage == "6~11 years" ~ "suv2",
                           bodytype == "suv" & vintage == "12+ years" ~ "suv3",
                           bodytype == "pickup" & vintage == "0~5 years" ~ "pickup1",
                           bodytype == "pickup" & vintage == "6~11 years" ~ "pickup2",
                           bodytype == "pickup" & vintage == "12+ years" ~ "pickup3")) %>% 
    group_by(veh) %>% 
    summarize(VEHICLE_COUNT = sum(VEHICLE_COUNT)) %>% 
    mutate(cat = "Predicted")
  
  veh_obs_bodyvintage <- veh_obs %>% 
    mutate(veh = case_when(bodytype == "car" & vintage == "0~5 years" ~ "car1",
                           bodytype == "car" & vintage == "6~11 years" ~ "car2",
                           bodytype == "car" & vintage == "12+ years" ~ "car3",
                           bodytype == "van" & vintage == "0~5 years" ~ "van1",
                           bodytype == "van" & vintage == "6~11 years" ~ "van2",
                           bodytype == "van" & vintage == "12+ years" ~ "van3",
                           bodytype == "suv" & vintage == "0~5 years" ~ "suv1",
                           bodytype == "suv" & vintage == "6~11 years" ~ "suv2",
                           bodytype == "suv" & vintage == "12+ years" ~ "suv3",
                           bodytype == "pickup" & vintage == "0~5 years" ~ "pickup1",
                           bodytype == "pickup" & vintage == "6~11 years" ~ "pickup2",
                           bodytype == "pickup" & vintage == "12+ years" ~ "pickup3")) %>% 
    group_by(veh) %>%   
    summarize(VEHICLE_COUNT = sum(VEHICLE_COUNT)) %>% 
    mutate(cat = "Observed")
  
  plot.veh.typeage <- rbind(veh_obs_bodyvintage, veh_pred_bodyvintage)
  
  
  # Add the percentage of household 
  plot.veh.typeage <- plot.veh.typeage %>% 
    group_by(cat) %>% 
    mutate(pct = VEHICLE_COUNT / sum(VEHICLE_COUNT) * 100)
  
  # Make level of the vehicle as factors
  plot.veh.typeage$veh <- factor(plot.veh.typeage$veh, 
                                 levels = c("car1", "car2", "car3", 
                                            "van1", "van2", "van3", 
                                            "suv1", "suv2", "suv3",
                                            "pickup1", "pickup2", "pickup3"))
  
  # Plot vehicle type distribution among households in California
  p_veh_typeage <- plot.veh.typeage %>% 
    ggplot(aes(y = pct, x = veh, fill = cat)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.6) +
    labs(title = "Vehicle Body Type Distribution", 
         x = NULL, y = "Percentage of Vehicle", fill = "") +
    scale_x_discrete(labels = function(x) recode(x, 
                                                 "car1" = "Car 0-5",
                                                 "car2" = "Car 6-11",
                                                 "car3" = "Car 12+",
                                                 "van1" = "Van 0-5",
                                                 "van2" = "Van 6-11",
                                                 "van3" = "Van 12+",
                                                 "suv1" = "SUV 0-5",
                                                 "suv2" = "SUV 6-11",
                                                 "suv3" = "SUV 12+",
                                                 "pickup1" = "Pick-up Truck 0-5",
                                                 "pickup2" = "Pick-up Truck 6-11",
                                                 "pickup3" = "Pick-up Truck 12+")) + 
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      legend.position = "bottom")
  
  
  
  # Vehicle Age ####
  
  veh_pred_vintage <- veh_pred %>% 
    group_by(vintage) %>% 
    summarize(VEHICLE_COUNT = sum(VEHICLE_COUNT)) %>% 
    mutate(cat = "Predicted")
  
  veh_obs_vintage <- veh_obs %>% 
    group_by(vintage) %>%   
    summarize(VEHICLE_COUNT = sum(VEHICLE_COUNT)) %>% 
    mutate(cat = "Observed")
  
  plot.veh.vintage <- rbind(veh_obs_vintage, veh_pred_vintage)
  
  
  # Add the percentage of household 
  plot.veh.vintage <- plot.veh.vintage %>% 
    group_by(cat) %>% 
    mutate(pct = VEHICLE_COUNT / sum(VEHICLE_COUNT) * 100)
  
  # Set level of vintage
  plot.veh.vintage$vintage <- factor(plot.veh.vintage$vintage,
                                     # levels = c("1", "2", "3"))
                                     levels = c("0~5 years", "6~11 years", "12+ years"))
  
  # Plot vehicle age distribution among households in California by county
  p_vintage <- plot.veh.vintage %>%
    ggplot(aes(y = pct, x = vintage, fill = cat)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.6) +
    labs(title = "Vehicle Age Distribution",
         x = NULL, y = "Percentage of Vehicle", fill = "") +
    scale_x_discrete(labels = c("0-5 years","6-11 years","12+ years")) +
    theme_bw() +
    theme(legend.position = "bottom") 
  
  
  
  
  # Vehicle Powertrain  ####
  
  veh_pred_power <- veh_pred %>% 
    #  mutate(power = ifelse(power == "Hybrid", "ICE", power)) %>%  # LJ change 10/15/2025, we do wanna keep hygrid category
    group_by(power) %>% 
    summarize(VEHICLE_COUNT = sum(VEHICLE_COUNT)) %>% 
    mutate(cat = "Predicted")
  
  veh_obs_power <- veh_obs %>% 
    group_by(power) %>%   
    summarize(VEHICLE_COUNT = sum(VEHICLE_COUNT)) %>% 
    mutate(cat = "Observed")
  
  plot.power <- rbind(veh_obs_power, veh_pred_power)
  
  
  # Add the percentage of household owning such vehicle type by county
  plot.power <- plot.power %>%
    group_by(cat) %>% 
    mutate(pct = VEHICLE_COUNT / sum(VEHICLE_COUNT) * 100)
  
  # Set level of vintage
  plot.power$power <- factor(plot.power$power,
                             levels = c("AEV", "PHEV", "Hybrid", "ICE"))
  
  
  # Plot vehicle powertrain distribution among households in California by county
  p_power <- plot.power %>%
    # filter(!power %in% c("ICE")) %>%
    ggplot(aes(y = pct, x = power, fill = cat)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.6) +
    # geom_text(aes(label = paste0(round(pct, 2), "%")),  # Display count values
    #           position = position_dodge(width = 0.6), 
    #           vjust = -0.5, size = 3) +  # Adjust position and size
    labs(title = "Vehicle Power Train Type Distribution",
         x = NULL, y = "Percentage of Vehicle", fill = "") +
    theme_bw() +
    theme(legend.position = "bottom")
  
  
  # Plot vehicle powertrain distribution among households in California by county
  p_power_zev <- plot.power %>%
    filter(!power %in% c("ICE", "Hybrid")) %>%
    ggplot(aes(y = pct, x = power, fill = cat)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.6) +
    # geom_text(aes(label = paste0(round(pct, 2), "%")),  # Display count values
    #           position = position_dodge(width = 0.6), 
    #           vjust = -0.5, size = 3) +  # Adjust position and size
    labs(title = "Vehicle Power Train Type Distribution",
         x = NULL, y = "Percentage of Vehicle", fill = "") +
    theme_bw() +
    theme(legend.position = "bottom")
  
  
  
  
  # Plot saving ####
  
  # List of plots
  plots <- list(
    "1_hh_vehicle_ownership_2017" = p_hh_own,
    "2_vehicle_bodytype" = p_veh_body,
    "3_vehicle_bodytype_vintage" = p_veh_typeage,
    "4_vehicle_vintage" = p_vintage
  )
  
  # Save each plot
  for (name in names(plots)) {
    png(file.path(imgdir, paste0(name,"_06",countyfips, ".png")), width = 1000, height = 700)
    print(plots[[name]])
    dev.off()
  }
  
  
  # List of plots
  plots <- list(
    "5_power_veh" = p_power,
    "5_power_veh_zev" = p_power_zev
  )
  
  # Save each plot
  for (name in names(plots)) {
    png(file.path(imgdir, paste0(name,"_06",countyfips, ".png")), width = 700, height = 1000)
    print(plots[[name]])
    dev.off()
  }
  
}
