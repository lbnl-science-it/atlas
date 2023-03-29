# Code Changes for New MDCEV Specification

Implemented by Connor P. Jackson (cpjackson@lbl.gov) in February 2023. 

## New Files

These files were developed in the estimation and internal validation codebase for this model, which is available in its own repository at <https://github.com/connorp/atlas_vfc/>. These scripts are largely identical to their counterparts in the estimation repository, except a few minor tweaks to facilitate their alternate use in this prediction script. Ideally, any changes made to these scripts are made first in that repository, and then brought over to this one. 

- `3_MDCEV_application.r`
    - Model application functions for the MDCEV model
    - Functions to generate predicted data from the estimated coefficients, generate output plots
- `3_MDCEV_initialization_full_sf.r`
    - Script to initialize Apollo using the estimated model specification
    - Because Apollo cannot simply predict values using the model object, but instead requires the definition of a prediction function matched to the estimation function, we define an initialization script particular to each model specification, that is used for both estimation and prediction
    - This script defines the model specification that was used to estimate the saved coefficients, and then is used to initialize the model for prediction
- `3_MDCEV_initialization_fn.r`
    - Functions for MDCEV model initialization that are shared across all specifications
    
## Edited Files

- `coefs_2017.RData`
    - Replace MDCEV model object with newly estimated object
    - Also specify the file path of the initialization script for the estimated MDCEV model. Here, `3_MDCEV_initialization_full_sf.r`
- `Model_application_functions_2017.R`
    - Removed the MDCEV model application code from this script file
    - This code is now in `3_MDCEV_application.r`, and is sourced in
    - The functions defined in `3_MDCEV_application.r` are then called from this script
    - The data are reshaped and manipulated both before and after the MDCEV prediction code is run, in order to make it consistent with the existing script
- `Model_application_2017.R`
    - Remove Apollo initialization code from this script file
    - The initialization now takes place in `3_MDCEV_initialization_full_sf.r`
- `renv.lock`
    - Lockfile that records the R package dependencies for this codebase, to enhance reproducibility
    - For use with the `renv` package. 
