# ============================================================================== #
#### FIT CANDIDATE MODELS ####
# ============================================================================== #

# Function that fits candidate models by region
fit_candidate_models <- function(data_CGFS, bspde, region_name = "region") {
  
  fitted_candidate_models <- list()
  
  
  # Log-transformed swept area used as offset for total biomass
  # No offset for density (already standardized per km²)
  offset_by_response <- list(totalWeightKg = log(data_CGFS$sweptAreaKm2),
                             densityKgKm2  = NULL)
  
  
  # ------------------------------------------------------------------------------#
  # Loop over response variables
  # ------------------------------------------------------------------------------#
  for (response in responses) {
    
    # Construct the model formula for the current response
    model_formula <- as.formula(paste(response, "~", fixed_effect))
    
    # Select the candidate families corresponding to the current response
    families_chosen <- families_by_response[[response]]
    
    # Select the offset argument for the current response (if any)
    offset_arg <- offset_by_response[[response]]
    
    models_fitted <- list()              # Models that fitted successfully 
    models_fit_failed <- list()          # Models that failed during model fitting
    
    
    # ----------------------------------------------------------------------------#
    # Loop over candidate families
    # ----------------------------------------------------------------------------#
    
    for (family_name in names(families_chosen)) {
      
      cat("[",region_name, "English Channel ] Fitting:", response, "-", family_name, "\n")
      
      current_candidate_model <- tryCatch({
        
        # List of arguments passed to sdmTMB()
        args <- list(
          data = data_CGFS,
          formula = model_formula,
          mesh = bspde,
          family = families_chosen[[family_name]],
          spatial = "on",
          time = "year",
          spatiotemporal = "IID"
        )
        
        # Add offset to the model only if it is defined
        if (!is.null(offset_arg)) args$offset <- offset_arg
        
        # Fit the model using arguments
        do.call(sdmTMB, args)
        
      }, error = function(e) e)   # Return the error object instead of stopping execution
      
      if (inherits(current_candidate_model, "error")) {
        models_fit_failed[[family_name]] <- current_candidate_model$message    # Store the error message for this family
        next
      }
      
      models_fitted[[family_name]] <- current_candidate_model
    }
    
    fitted_candidate_models[[response]] <- list(
      models_fitted = models_fitted,                # Models that fitted successfully for this response
      models_fit_failed = models_fit_failed         # Models that failed during model fitting for this response
    )
  }
  
  return(list(region = region_name,
              fits   = fitted_candidate_models))
  
}


# ------------------------------------------------------------------------------#
#### store fitted models by region ####
# ------------------------------------------------------------------------------#

# object to store fitted models
fitted_models_by_region <- list()

if (isTRUE(East_English_Channel)) {
  fitted_models_by_region$east <- fit_candidate_models(data_CGFS = data_CGFS_east, 
                                                       bspde = mesh_by_region$east$bspde, 
                                                       region_name = "east")
}

if (isTRUE(West_English_Channel)) {
  fitted_models_by_region$west <- fit_candidate_models(data_CGFS = data_CGFS_west, 
                                                       bspde = mesh_by_region$west$bspde, 
                                                       region_name = "west")
}

