# Prediction and application code for the MDCEV Mileage allocation model.
#
# VehicleFleet_model() is the main application function. mileage_plot() can be
# called with its results.
# Output of this code serves as input to '4_Heuristic_mileage_reallocation.r'
#
# Calls a separate script (init_path) to define the model parameters,
# including the utility function and coefficients.

init_predict_mdcev <- function(pred_data, init_path, mdcev_model) {
  # Initialize and run an MDCEV model prediction
  # Requires: dataset of covariates for prediction, path to MDCEV initialization script,
  # corresponding MDCEV model object
  if (!("budget" %in% colnames(pred_data))) {
    database <<- pred_data %>% mutate(budget=car1 + car2 + car3 + van1 + van2 +
                                        van3 + suv1 + suv2 + suv3 + pickup1 +
                                        pickup2 + pickup3 + motorbike +
                                        annual_nonmotor_new)
  } else {
    database <<- pred_data
  }
  names(database)[which(colnames(database)=="annual_nonmotor_new")] <<- "outside"

  source(init_path)
  # script defines the constructor function, alternatives_list, and apollo_inputs
  apollo_probabilities <- apollo_prob_constructor(alternatives_list)

  mdcev_predicted = apollo_prediction(mdcev_model,
                                      apollo_probabilities,
                                      apollo_inputs)

  return(mdcev_predicted)
}


construct_mdcev_pred_table <- function(pred_table, modelname=NULL) {
  # Reshape the table of predicted values
  melted_preds <- melt(as.data.table(pred_table), id.vars = c("ID", "Observation"),
                       variable.name = "outcome")
  melted_preds[, c("outcome", "parameter", "measure") := tstrsplit(outcome, "_")]
  cast_preds <- dcast(melted_preds, ID + Observation + outcome ~ parameter + measure)
  if (cast_preds[, uniqueN(Observation)] == 1) {
    # this column is only useful for panel data
    cast_preds[, Observation := NULL][]
  }
  if (!is.null(modelname) & is.character(modelname)) {
    # Add the model name as a column, if passed
    cast_preds[, model_name := modelname][]
  }
  return(cast_preds)
}


average_mileage <- function(mileage_data, select=FALSE) {
  if (select == TRUE) mileage_data <- mileage_data[, c(3:16)]
  return(colSums(mileage_data) / nrow(mileage_data))
}

mileage_plot <- function(pred_table, obs_data=NULL, plot=TRUE, add_colors=TRUE, groupvar=NULL) {
  # Calculate average mileage (and average usage) for each vehicle type,
  # and optionally generate the plot

  if (!require(ggplot2)) plot <- FALSE

  # Predicted average mileage for each model object
  pred_avg <- pred_table[, .(miles = mean(cont_mean)), keyby = c("model_name", "outcome", groupvar)]

  if (!is.null(obs_data)) {
    obs_data <- as.data.table(obs_data)
    if (!("outside" %in% colnames(obs_data))) setnames(obs_data, "annual_nonmotor_new", "outside")
    select_cols <- c(groupvar, "outside", "car1", "car2", "car3",
                     "van1", "van2", "van3",
                     "suv1", "suv2", "suv3",
                     "pickup1", "pickup2", "pickup3", "motorbike")
    obs_mileage <- melt(obs_data[, ..select_cols], id.vars = groupvar,
                        variable.name="outcome", value.name="miles")
    obs_avgmileage <- obs_mileage[, .(model_name = "observed", miles = mean(miles)), keyby=c("outcome", groupvar)]
    pred_avg <- rbind(obs_avgmileage, pred_avg, use.names=TRUE)
  }

  if (plot==TRUE) {
    pl <- ggplot(pred_avg) +
      geom_col(aes(x=outcome, y=miles, group=model_name, fill=model_name), position='dodge') +
      labs(title="MDCEV Average Annual Mileage",
           x=NULL, y="Mileage") + coord_flip()
    if (add_colors == TRUE & !require(viridisLite)) {
      pl <- pl + scale_fill_viridis(discrete = TRUE)
    }
    if (!is.null(groupvar)) {
      pl <- pl + facet_wrap(groupvar)
    }
    return(pl)
  }
  return(pred_avg)
}

VehicleFleet_model <- function(pred_data, model3_obj, model_init_file, modelname="full model") {
  mdcev_predicted <- init_predict_mdcev(pred_data, model_init_file,
                                        model3_obj)
  mdcev_pred_table <-construct_mdcev_pred_table(mdcev_predicted, modelname)

  return(mdcev_pred_table)
}
