# This code predicts the fleet mix owned by a household
# The code is run 25 times and average value is computed for each of the alternatives
# Number of replications can be changed by changing the value of "nrep" in control file
# This code takes 'predicted annual mileage' as input from '1_Mileage_prediction.r'
# Output of this code serves as input to '4_Heuristic_mileage_reallocation.r'

VehicleFleet_model <- function(data1, model){ # change database to data1 to avoid overload the database variable somewhere else
  
  if (!("budget" %in% colnames(data1))) {
    database1 <- data1 %>% mutate(budget = 
                                       car1 + car2 + car3 + van1 + van2 +
                                       van3 + suv1 + suv2 + suv3 + pickup1 +
                                       pickup2 + pickup3 + motorbike +
                                       annual_nonmotor_new)
  } else {
    database1 <- data1
  }
  
  database1 <- database1 %>% rename(outside = annual_nonmotor_new)
  ###### Apollo function
  ### Initialise code
  apollo_initialise()
  
  ### Set core controls --> moved to parameters_us_atlas_v1.R
  
  # ################################################################# #
  #### DEFINE MODEL PARAMETERS   (moved to parameters_us_atlas_v1.R)                                  ####
  # ################################################################# #
  
  # ################################################################# #
  #### GROUP AND VALIDATE INPUTS                                   ####
  # ################################################################# #
  
  apollo_inputs <- apollo_validateInputs(database = database1) # LJ change to <- from <<-
  
  # ################################################################# #
  #### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
  # ################################################################# #
  
  apollo_probabilities=function(apollo_beta = apollo_beta, apollo_inputs = apollo_inputs, functionality="estimate"){ # LJ change to explicit argument assignment
    
    ### Attach inputs and detach after function exit
    apollo_attach(apollo_beta, apollo_inputs)
    on.exit(apollo_detach(apollo_beta, apollo_inputs))
    
    ### Create list of probabilities P
    P = list()
    
    ### Define individual alternatives
    alternatives  = c("outside", "car1", "car2", "car3",
                      "van1", "van2", "van3",
                      "suv1", "suv2", "suv3",
                      "pickup1", "pickup2", "pickup3",
                      "motorbike")
    
    ### Define availabilities
    avail = list(outside  = 1, car1=1, car2=1, car3=1, 
                 van1=1, van2=1, van3=1,  
                 suv1=1, suv2=1, suv3=1,  
                 pickup1=1, pickup2=1, pickup3=1,  
                 motorbike=1)
    
    ### Define continuous consumption for individual alternatives
    continuousChoice = list(outside=outside, car1=car1, car2=car2, car3=car3, 
                            van1=van1, van2=van2, van3=van3,  
                            suv1=suv1, suv2=suv2, suv3=suv3,  
                            pickup1=pickup1, pickup2=pickup2, pickup3=pickup3,  
                            motorbike=motorbike)
    
    ### Define utilities for individual alternatives
    V = list()
    V[["outside"]]  = 0
    V[["car1"]] <- delta_car1 + delta_car1_child*child +
      delta_car1_work0*work0 + delta_car1_work2*work2 + delta_car1_work3*work3 +
      delta_car1_retired*retired + delta_car1_HHSIZE*HHSIZE +
      delta_car1_emp_zscore*emp_zscore + delta_car1_log_pop_density*log_pop_density +
      delta_car1_log_job_density*log_job_density +
      delta_car1_log_job_below4*log_job_below4 +
      delta_car1_walkndx*walkndx +
      # delta_car1_pct_ag_land*pct_ag_land +
      # delta_car1_pct_water*pct_water + 
      # delta_car1_tas_acres*tas_acres + 
      delta_car1_tci*tci + 
      delta_car1_compact*compact_ndx + 
      delta_car1_hi_tps*hi_tps + 
      # delta_car1_income2*income2 + 
      # delta_car1_income3*income3 + 
      # delta_car1_income4*income4 + 
      delta_car1_income5*income5 
    
    V[["car2"]] <- delta_car2 + delta_car2_child*child +
      delta_car2_work0*work0 + delta_car2_work2*work2 + delta_car2_work3*work3 +
      delta_car2_retired*retired + delta_car2_HHSIZE*HHSIZE +
      delta_car2_emp_zscore*emp_zscore + delta_car2_log_pop_density*log_pop_density +
      delta_car2_log_job_density*log_job_density +
      delta_car2_log_job_below4*log_job_below4 +
      delta_car2_walkndx*walkndx +
      # delta_car2_pct_ag_land*pct_ag_land +
      # delta_car2_pct_water*pct_water + 
      # delta_car2_tas_acres*tas_acres + 
      delta_car2_tci*tci + 
      delta_car2_compact*compact_ndx + 
      delta_car2_hi_tps*hi_tps + 
      # delta_car2_income2*income2 + 
      # delta_car2_income3*income3 + 
      # delta_car2_income4*income4 + 
      delta_car2_income5*income5 
    
    V[["car3"]] <- delta_car3 + delta_car3_child*child +
      delta_car3_work0*work0 + delta_car3_work2*work2 + delta_car3_work3*work3 +
      delta_car3_retired*retired + delta_car3_HHSIZE*HHSIZE +
      delta_car3_emp_zscore*emp_zscore + delta_car3_log_pop_density*log_pop_density +
      delta_car3_log_job_density*log_job_density +
      delta_car3_log_job_below4*log_job_below4 +
      delta_car3_walkndx*walkndx +
      # delta_car3_pct_ag_land*pct_ag_land +
      # delta_car3_pct_water*pct_water + 
      # delta_car3_tas_acres*tas_acres + 
      delta_car3_tci*tci + 
      delta_car3_compact*compact_ndx + 
      delta_car3_hi_tps*hi_tps + 
      # delta_car3_income2*income2 + 
      # delta_car3_income3*income3 + 
      # delta_car3_income4*income4 + 
      delta_car3_income5*income5 
    
    V[["van1"]] <- delta_van1 + delta_van1_child*child +
      delta_van1_work0*work0 + delta_van1_work2*work2 + delta_van1_work3*work3 +
      delta_van1_retired*retired + delta_van1_HHSIZE*HHSIZE +
      delta_van1_emp_zscore*emp_zscore + delta_van1_log_pop_density*log_pop_density +
      delta_van1_log_job_density*log_job_density +
      delta_van1_log_job_below4*log_job_below4 +
      delta_van1_walkndx*walkndx +
      # delta_van1_pct_ag_land*pct_ag_land +
      # delta_van1_pct_water*pct_water + 
      # delta_van1_tas_acres*tas_acres + 
      delta_van1_tci*tci + 
      delta_van1_compact*compact_ndx + 
      delta_van1_hi_tps*hi_tps + 
      # delta_van1_income2*income2 + 
      # delta_van1_income3*income3 + 
      # delta_van1_income4*income4 + 
      delta_van1_income5*income5 
    
    V[["van2"]] <- delta_van2 + delta_van2_child*child +
      delta_van2_work0*work0 + delta_van2_work2*work2 + delta_van2_work3*work3 +
      delta_van2_retired*retired + delta_van2_HHSIZE*HHSIZE +
      delta_van2_emp_zscore*emp_zscore + delta_van2_log_pop_density*log_pop_density +
      delta_van2_log_job_density*log_job_density +
      delta_van2_log_job_below4*log_job_below4 +
      delta_van2_walkndx*walkndx +
      # delta_van2_pct_ag_land*pct_ag_land +
      # delta_van2_pct_water*pct_water + 
      # delta_van2_tas_acres*tas_acres + 
      delta_van2_tci*tci + 
      delta_van2_compact*compact_ndx + 
      delta_van2_hi_tps*hi_tps + 
      # delta_van2_income2*income2 + 
      # delta_van2_income3*income3 + 
      # delta_van2_income4*income4 + 
      delta_van2_income5*income5 
    
    V[["van3"]] <- delta_van3 + delta_van3_child*child +
      delta_van3_work0*work0 + delta_van3_work2*work2 + delta_van3_work3*work3 +
      delta_van3_retired*retired + delta_van3_HHSIZE*HHSIZE +
      delta_van3_emp_zscore*emp_zscore + delta_van3_log_pop_density*log_pop_density +
      delta_van3_log_job_density*log_job_density +
      delta_van3_log_job_below4*log_job_below4 +
      delta_van3_walkndx*walkndx +
      # delta_van3_pct_ag_land*pct_ag_land +
      # delta_van3_pct_water*pct_water + 
      # delta_van3_tas_acres*tas_acres + 
      delta_van3_tci*tci + 
      delta_van3_compact*compact_ndx + 
      delta_van3_hi_tps*hi_tps + 
      # delta_van3_income2*income2 + 
      # delta_van3_income3*income3 + 
      # delta_van3_income4*income4 + 
      delta_van3_income5*income5 
    
    V[["suv1"]] <- delta_suv1 + delta_suv1_child*child +
      delta_suv1_work0*work0 + delta_suv1_work2*work2 + delta_suv1_work3*work3 +
      delta_suv1_retired*retired + delta_suv1_HHSIZE*HHSIZE +
      delta_suv1_emp_zscore*emp_zscore + delta_suv1_log_pop_density*log_pop_density +
      delta_suv1_log_job_density*log_job_density +
      delta_suv1_log_job_below4*log_job_below4 +
      delta_suv1_walkndx*walkndx +
      # delta_suv1_pct_ag_land*pct_ag_land +
      # delta_suv1_pct_water*pct_water + 
      # delta_suv1_tas_acres*tas_acres + 
      delta_suv1_tci*tci + 
      delta_suv1_compact*compact_ndx + 
      delta_suv1_hi_tps*hi_tps + 
      # delta_suv1_income2*income2 + 
      # delta_suv1_income3*income3 + 
      # delta_suv1_income4*income4 + 
      delta_suv1_income5*income5 
    
    V[["suv2"]] <- delta_suv2 + delta_suv2_child*child +
      delta_suv2_work0*work0 + delta_suv2_work2*work2 + delta_suv2_work3*work3 +
      delta_suv2_retired*retired + delta_suv2_HHSIZE*HHSIZE +
      delta_suv2_emp_zscore*emp_zscore + delta_suv2_log_pop_density*log_pop_density +
      delta_suv2_log_job_density*log_job_density +
      delta_suv2_log_job_below4*log_job_below4 +
      delta_suv2_walkndx*walkndx +
      # delta_suv2_pct_ag_land*pct_ag_land +
      # delta_suv2_pct_water*pct_water + 
      # delta_suv2_tas_acres*tas_acres + 
      delta_suv2_tci*tci + 
      delta_suv2_compact*compact_ndx + 
      delta_suv2_hi_tps*hi_tps + 
      # delta_suv2_income2*income2 + 
      # delta_suv2_income3*income3 + 
      # delta_suv2_income4*income4 + 
      delta_suv2_income5*income5 
    
    V[["suv3"]] <- delta_suv3 + delta_suv3_child*child +
      delta_suv3_work0*work0 + delta_suv3_work2*work2 + delta_suv3_work3*work3 +
      delta_suv3_retired*retired + delta_suv3_HHSIZE*HHSIZE +
      delta_suv3_emp_zscore*emp_zscore + delta_suv3_log_pop_density*log_pop_density +
      delta_suv3_log_job_density*log_job_density +
      delta_suv3_log_job_below4*log_job_below4 +
      delta_suv3_walkndx*walkndx +
      # delta_suv3_pct_ag_land*pct_ag_land +
      # delta_suv3_pct_water*pct_water + 
      # delta_suv3_tas_acres*tas_acres + 
      delta_suv3_tci*tci + 
      delta_suv3_compact*compact_ndx + 
      delta_suv3_hi_tps*hi_tps + 
      # delta_suv3_income2*income2 + 
      # delta_suv3_income3*income3 + 
      # delta_suv3_income4*income4 + 
      delta_suv3_income5*income5 
    
    V[["pickup1"]] <- delta_pickup1 + delta_pickup1_child*child +
      delta_pickup1_work0*work0 + delta_pickup1_work2*work2 +
      delta_pickup1_work3*work3 + delta_pickup1_retired*retired +
      delta_pickup1_HHSIZE*HHSIZE +
      delta_pickup1_emp_zscore*emp_zscore +
      delta_pickup1_log_pop_density*log_pop_density +
      delta_pickup1_log_job_density*log_job_density +
      delta_pickup1_log_job_below4*log_job_below4 +
      delta_pickup1_walkndx*walkndx +
      # delta_pickup1_pct_ag_land*pct_ag_land +
      # delta_pickup1_pct_water*pct_water + 
      # delta_pickup1_tas_acres*tas_acres + 
      delta_pickup1_tci*tci + 
      delta_pickup1_compact*compact_ndx + 
      delta_pickup1_hi_tps*hi_tps + 
      # delta_pickup1_income2*income2 + 
      # delta_pickup1_income3*income3 + 
      # delta_pickup1_income4*income4 + 
      delta_pickup1_income5*income5 
    
    V[["pickup2"]] <- delta_pickup2 + delta_pickup2_child*child +
      delta_pickup2_work0*work0 + delta_pickup2_work2*work2 +
      delta_pickup2_work3*work3 + delta_pickup2_retired*retired +
      delta_pickup2_HHSIZE*HHSIZE +
      delta_pickup2_emp_zscore*emp_zscore +
      delta_pickup2_log_pop_density*log_pop_density +
      delta_pickup2_log_job_density*log_job_density +
      delta_pickup2_log_job_below4*log_job_below4 +
      delta_pickup2_walkndx*walkndx +
      # delta_pickup2_pct_ag_land*pct_ag_land +
      # delta_pickup2_pct_water*pct_water + 
      # delta_pickup2_tas_acres*tas_acres + 
      delta_pickup2_tci*tci + 
      delta_pickup2_compact*compact_ndx + 
      delta_pickup2_hi_tps*hi_tps + 
      # delta_pickup2_income2*income2 + 
      # delta_pickup2_income3*income3 + 
      # delta_pickup2_income4*income4 + 
      delta_pickup2_income5*income5 
    
    V[["pickup3"]] <- delta_pickup3 + delta_pickup3_child*child +
      delta_pickup3_work0*work0 + delta_pickup3_work2*work2 +
      delta_pickup3_work3*work3 + delta_pickup3_retired*retired +
      delta_pickup3_HHSIZE*HHSIZE +
      delta_pickup3_emp_zscore*emp_zscore +
      delta_pickup3_log_pop_density*log_pop_density +
      delta_pickup3_log_job_density*log_job_density +
      delta_pickup3_log_job_below4*log_job_below4 +
      delta_pickup3_walkndx*walkndx +
      # delta_pickup3_pct_ag_land*pct_ag_land +
      # delta_pickup3_pct_water*pct_water + 
      # delta_pickup3_tas_acres*tas_acres + 
      delta_pickup3_tci*tci + 
      delta_pickup3_compact*compact_ndx + 
      delta_pickup3_hi_tps*hi_tps + 
      # delta_pickup3_income2*income2 + 
      # delta_pickup3_income3*income3 + 
      # delta_pickup3_income4*income4 + 
      delta_pickup3_income5*income5 
    
    V[["motorbike"]] <- delta_motorbike + delta_motorbike_child*child +
      delta_motorbike_work0*work0 + delta_motorbike_work2*work2 +
      delta_motorbike_work3*work3 + delta_motorbike_retired*retired +
      delta_motorbike_HHSIZE*HHSIZE +
      delta_motorbike_emp_zscore*emp_zscore +
      delta_motorbike_log_pop_density*log_pop_density +
      delta_motorbike_log_job_density*log_job_density +
      delta_motorbike_log_job_below4*log_job_below4 +
      delta_motorbike_walkndx*walkndx +
      # delta_motorbike_pct_ag_land*pct_ag_land +
      # delta_motorbike_pct_water*pct_water + 
      # delta_motorbike_tas_acres*tas_acres + 
      delta_motorbike_tci*tci + 
      delta_motorbike_compact*compact_ndx + 
      delta_motorbike_hi_tps*hi_tps + 
      # delta_motorbike_income2*income2 + 
      # delta_motorbike_income3*income3 + 
      # delta_motorbike_income4*income4 + 
      delta_motorbike_income5*income5 
    
    ### Define alpha parameters
    alpha = list(
      outside=alpha_base,
      car1=alpha_base,
      car2=alpha_base,
      car3=alpha_base,
      van1=alpha_base,
      van2=alpha_base,
      van3=alpha_base,
      suv1=alpha_base,
      suv2=alpha_base,
      suv3=alpha_base,
      pickup1=alpha_base,
      pickup2=alpha_base,
      pickup3=alpha_base,
      motorbike=alpha_base)
    
    ### Define gamma parameters
    gamma = list( car1=gamma_car1,
                  car2=gamma_car2,
                  car3=gamma_car3,
                  van1=gamma_van1,
                  van2=gamma_van2,
                  van3=gamma_van3,
                  suv1=gamma_suv1,
                  suv2=gamma_suv2,
                  suv3=gamma_suv3,
                  pickup1=gamma_pickup1,
                  pickup2=gamma_pickup2,
                  pickup3=gamma_pickup3,
                  motorbike=gamma_motorbike)
    
    ### Define costs for individual alternatives
    cost = list(outside  = 1,
                car1=1,
                car2=1,
                car3=1,
                van1=1,
                van2=1,
                van3=1,
                suv1=1,
                suv2=1,
                suv3=1,
                pickup1=1,
                pickup2=1,
                pickup3=1,
                motorbike=1)
    
    ### Define settings for MDCEV model
    mdcev_settings <- list(alternatives      = alternatives,
                           avail             = avail,
                           continuousChoice  = continuousChoice,
                           V                 = V,
                           alpha             = alpha,
                           gamma             = gamma, 
                           sigma             = sigma, 
                           cost              = cost,
                           budget            = budget)
    
    ### Compute probabilities using MDCEV model
    P[["model"]] = apollo_mdcev(mdcev_settings, functionality)
    
    ### Take product across observation for same individual
    # P = apollo_panelProd(P, apollo_inputs, functionality)
    
    ### Prepare and return outputs of function
    P = apollo_prepareProb(P, apollo_inputs, functionality)
    return(P)
  }
  
  # ################################################################# #
  #### MODEL PREDICTION                                            ####
  # ################################################################# #
  
  model3_mdcev_full_pred = apollo_prediction(model, apollo_probabilities, apollo_inputs)
  
  return(model3_mdcev_full_pred);
  
}
