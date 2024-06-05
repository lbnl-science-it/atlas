##### MDCEV model for vehicle fleet mix ####
# This Code initializes and parameterizes an MDCEV model of vehicle fleet mix
# with 1 outside good (non-motorized mileage).
#
# Call this script in order to set up the apollo package, define the coefficients,
# and assemble the utility function.
#
# FULL MODEL

covariates <- c("NUMCHILD", "work0", "work2", "work3", "retired", "HHSIZE",
                "emp_zscore", "log_pop_density", "log_job_density")

### Initialize apollo package
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  = "nhts2017_full_sf",
  modelDescr = paste("MDCEV model on vehicle mileage data, alpha-gamma profile",
                     "with outside good and socio-demographics. Full model",
                     "including location variables."),
  indivID    = "household_id",
  nCores     = 4
)

source("3_MDCEV_initialization_fn.r")

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation.
# First, just list the alpha and gammas manually
apollo_beta = c(alpha_base    = 0,
                gamma_car1    = 1,
                gamma_car2    = 1,
                gamma_car3    = 1,
                gamma_van1    = 1,
                gamma_van2    = 1,
                gamma_van3    = 1,
                gamma_suv1    = 1,
                gamma_suv2    = 1,
                gamma_suv3    = 1,
                gamma_pickup1    = 1,
                gamma_pickup2    = 1,
                gamma_pickup3    = 1,
                gamma_motorbike    = 1,
                sigma = 1)

# construct_variables() defined in the 3_MDCEV_initialization_fn.R script
apollo_beta <- c(apollo_beta, construct_variables(alternatives_list, covariates))

### Vector with names (in quotes) of parameters to be kept fixed at their
### starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("alpha_base", "sigma")

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_prob_constructor <- function(alternatives_list) {

  apollo_prob <- function(apollo_beta, apollo_inputs, functionality="estimate") {

    alternatives <- c("outside", alternatives_list)

    ### Attach inputs and detach after function exit
    apollo_attach(apollo_beta, apollo_inputs)
    on.exit(apollo_detach(apollo_beta, apollo_inputs))

    ### Create list of probabilities P
    P = list()

    ### Define availability
    avail = list(outside = 1, car1=1, car2=1, car3=1,
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

    # Define the utility function
    V <- list()
    V[["outside"]] <- 0
    V[["car1"]] <- delta_car1_INTERCEPT + delta_car1_NUMCHILD*NUMCHILD +
      delta_car1_work0*work0 + delta_car1_work2*work2 + delta_car1_work3*work3 +
      delta_car1_retired*retired + delta_car1_HHSIZE*HHSIZE +
      delta_car1_emp_zscore*emp_zscore + delta_car1_log_pop_density*log_pop_density +
      delta_car1_log_job_density*log_job_density

    V[["car2"]] <- delta_car2_INTERCEPT + delta_car2_NUMCHILD*NUMCHILD +
      delta_car2_work0*work0 + delta_car2_work2*work2 + delta_car2_work3*work3 +
      delta_car2_retired*retired + delta_car2_HHSIZE*HHSIZE +
      delta_car2_emp_zscore*emp_zscore + delta_car2_log_pop_density*log_pop_density +
      delta_car2_log_job_density*log_job_density

    V[["car3"]] <- delta_car3_INTERCEPT + delta_car3_NUMCHILD*NUMCHILD +
      delta_car3_work0*work0 + delta_car3_work2*work2 + delta_car3_work3*work3 +
      delta_car3_retired*retired + delta_car3_HHSIZE*HHSIZE +
      delta_car3_emp_zscore*emp_zscore + delta_car3_log_pop_density*log_pop_density +
      delta_car3_log_job_density*log_job_density

    V[["van1"]] <- delta_van1_INTERCEPT + delta_van1_NUMCHILD*NUMCHILD +
      delta_van1_work0*work0 + delta_van1_work2*work2 + delta_van1_work3*work3 +
      delta_van1_retired*retired + delta_van1_HHSIZE*HHSIZE +
      delta_van1_emp_zscore*emp_zscore + delta_van1_log_pop_density*log_pop_density +
      delta_van1_log_job_density*log_job_density

    V[["van2"]] <- delta_van2_INTERCEPT + delta_van2_NUMCHILD*NUMCHILD +
      delta_van2_work0*work0 + delta_van2_work2*work2 + delta_van2_work3*work3 +
      delta_van2_retired*retired + delta_van2_HHSIZE*HHSIZE +
      delta_van2_emp_zscore*emp_zscore + delta_van2_log_pop_density*log_pop_density +
      delta_van2_log_job_density*log_job_density

    V[["van3"]] <- delta_van3_INTERCEPT + delta_van3_NUMCHILD*NUMCHILD +
      delta_van3_work0*work0 + delta_van3_work2*work2 + delta_van3_work3*work3 +
      delta_van3_retired*retired + delta_van3_HHSIZE*HHSIZE +
      delta_van3_emp_zscore*emp_zscore + delta_van3_log_pop_density*log_pop_density +
      delta_van3_log_job_density*log_job_density

    V[["suv1"]] <- delta_suv1_INTERCEPT + delta_suv1_NUMCHILD*NUMCHILD +
      delta_suv1_work0*work0 + delta_suv1_work2*work2 + delta_suv1_work3*work3 +
      delta_suv1_retired*retired + delta_suv1_HHSIZE*HHSIZE +
      delta_suv1_emp_zscore*emp_zscore + delta_suv1_log_pop_density*log_pop_density +
      delta_suv1_log_job_density*log_job_density

    V[["suv2"]] <- delta_suv2_INTERCEPT + delta_suv2_NUMCHILD*NUMCHILD +
      delta_suv2_work0*work0 + delta_suv2_work2*work2 + delta_suv2_work3*work3 +
      delta_suv2_retired*retired + delta_suv2_HHSIZE*HHSIZE +
      delta_suv2_emp_zscore*emp_zscore + delta_suv2_log_pop_density*log_pop_density +
      delta_suv2_log_job_density*log_job_density

    V[["suv3"]] <- delta_suv3_INTERCEPT + delta_suv3_NUMCHILD*NUMCHILD +
      delta_suv3_work0*work0 + delta_suv3_work2*work2 + delta_suv3_work3*work3 +
      delta_suv3_retired*retired + delta_suv3_HHSIZE*HHSIZE +
      delta_suv3_emp_zscore*emp_zscore + delta_suv3_log_pop_density*log_pop_density +
      delta_suv3_log_job_density*log_job_density

    V[["pickup1"]] <- delta_pickup1_INTERCEPT + delta_pickup1_NUMCHILD*NUMCHILD +
      delta_pickup1_work0*work0 + delta_pickup1_work2*work2 +
      delta_pickup1_work3*work3 + delta_pickup1_retired*retired +
      delta_pickup1_HHSIZE*HHSIZE +
      delta_pickup1_emp_zscore*emp_zscore +
      delta_pickup1_log_pop_density*log_pop_density +
      delta_pickup1_log_job_density*log_job_density

    V[["pickup2"]] <- delta_pickup2_INTERCEPT + delta_pickup2_NUMCHILD*NUMCHILD +
      delta_pickup2_work0*work0 + delta_pickup2_work2*work2 +
      delta_pickup2_work3*work3 + delta_pickup2_retired*retired +
      delta_pickup2_HHSIZE*HHSIZE +
      delta_pickup2_emp_zscore*emp_zscore +
      delta_pickup2_log_pop_density*log_pop_density +
      delta_pickup2_log_job_density*log_job_density

    V[["pickup3"]] <- delta_pickup3_INTERCEPT + delta_pickup3_NUMCHILD*NUMCHILD +
      delta_pickup3_work0*work0 + delta_pickup3_work2*work2 +
      delta_pickup3_work3*work3 + delta_pickup3_retired*retired +
      delta_pickup3_HHSIZE*HHSIZE +
      delta_pickup3_emp_zscore*emp_zscore +
      delta_pickup3_log_pop_density*log_pop_density +
      delta_pickup3_log_job_density*log_job_density

    V[["motorbike"]] <- delta_motorbike_INTERCEPT + delta_motorbike_NUMCHILD*NUMCHILD +
      delta_motorbike_work0*work0 + delta_motorbike_work2*work2 +
      delta_motorbike_work3*work3 + delta_motorbike_retired*retired +
      delta_motorbike_HHSIZE*HHSIZE +
      delta_motorbike_emp_zscore*emp_zscore +
      delta_motorbike_log_pop_density*log_pop_density +
      delta_motorbike_log_job_density*log_job_density


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
    cost = list(outside = 1,
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

    ### Define budget
    # budget = budget

    ### Define settings for MDCEV model
    mdcev_settings <- list(alternatives      = alternatives,
                           avail             = avail,
                           continuousChoice = continuousChoice,
                           V                 = V,
                           alpha             = alpha,
                           gamma             = gamma,
                           sigma             = sigma,
                           cost              = cost,
                           budget            = budget)

    ### Compute probabilities using MDCEV model
    P[["model"]] = apollo_mdcev(mdcev_settings, functionality)

    ### Prepare and return outputs of function
    P = apollo_prepareProb(P, apollo_inputs, functionality)
    return(P)
  }

  # return the function after construction
  return(apollo_prob)
}
