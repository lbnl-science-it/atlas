# MDCEV Initialization Functions and Variables
# Shared across all model specifications.

alternatives_list <- c("car1", "car2", "car3",
                       "van1", "van2", "van3",
                       "suv1", "suv2", "suv3",
                       "pickup1", "pickup2", "pickup3",
                       "motorbike")

# Define function to generate variables, used in each of the initialization scripts
construct_variables <- function(alternatives_list, covariates=NULL) {
  # construct the intercept and covariate variables by name

  # For the delta (utility) parameters, construct them programatically using the
  # list of alternatives and the list of covariates. First construct the intercepts
  intercept_names <- paste("delta", alternatives_list, "INTERCEPT", sep="_")
  delta_intercepts <- integer(length(intercept_names))  # initialize at 0
  names(delta_intercepts) <- intercept_names
  variable_list <- delta_intercepts
  if (!is.null(covariates)) {
    # If the model isn't just intercept-only, paste the names of each covariate
    # with each alternative, so we can give names to all our covariates.
    # NOTE that this creates all the covariates for all the outcomes
    coef_names <- do.call(function(...) paste("delta", ..., sep="_"),
                          expand.grid(alternatives_list, covariates))

    # Initialize the vector of delta coefficients at 0
    delta_coefs <- integer(length(coef_names))
    names(delta_coefs) <- coef_names
    variable_list <- c(variable_list, delta_coefs)
  }
  return(variable_list)
}


# Use this function to construct the Utility function expressions, and then paste
# in below. It may be possible to construct them programatically,
construct_utility_expr <- function(outc) {
  # construct the utility expression as a string
  utility_exp <- paste(paste("delta", outc, covariates, sep="_"),
                       covariates, sep="*", collapse=" + ")
  utility_exp <- paste(paste("delta", outc, "INTERCEPT", sep="_"), "+", utility_exp)
  utility_exp <- paste0("V[['", outc, "']] <- ", utility_exp)
  return(utility_exp)
}
# v_strings <- lapply(alternatives_list, construct_utility_expr)
# invisible(lapply(v_strings, write, "util.txt", append=TRUE))
