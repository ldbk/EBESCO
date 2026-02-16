# ------------------------------------------------------------------------------#
# Function used to extract only converged models from the diagnostics object
# ------------------------------------------------------------------------------#

get_converged_models <- function(all_fitted_models) {
  # Initialize an empty list with one element per region
  converged_models <- setNames(vector("list", length(all_fitted_models)),
                               names(all_fitted_models))
  
  # Loop over regions
  for (region in names(all_fitted_models)) {
    region_list <- all_fitted_models[[region]]
    
    if (is.null(region_list)) next    # Skip empty or NULL regions
    
    # Loop over responses within a region & extract valid models for this response
    for (response in names(region_list)) {
      models_valid <- region_list[[response]]$models_valid
      
      # Store only non-empty valid model lists
      if (!is.null(models_valid)) {
        converged_models[[region]][[response]] <- models_valid
      }
    }
  }
  # Remove regions that ended up empty
  converged_models <- converged_models[vapply(converged_models, length, integer(1)) > 0]
  return(converged_models)
}

# converged_models <- get_converged_models(sanity_by_region)
