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
    
    # Loop over mesh configurations
    for (mesh_name in names(region_list)) {
      mesh_list <- region_list[[mesh_name]]
      if (is.null(mesh_list)) next
      
    # Loop over responses 
    for (response in names(mesh_list)) {
      models_valid <- mesh_list[[response]]$models_valid
      
      # Store only non-empty valid model lists
      if (!is.null(models_valid)) {
        converged_models[[region]][[mesh_name]][[response]] <- models_valid
      }
    }
    }
  }
  # Remove regions that ended up empty
  converged_models <- converged_models[vapply(converged_models, length, integer(1)) > 0]
  return(converged_models)
}

# converged_models <- get_converged_models(sanity_by_region)
