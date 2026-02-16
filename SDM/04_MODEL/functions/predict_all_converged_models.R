
# ==============================================================================#
# Predict all valid models by region, produce maps 
# ==============================================================================#

# Prediction grids by region
grids_by_region <- list(east = grid_by_region$east$grid_pred,
                        west = grid_by_region$west$grid_pred)


# ------------------------------------------------------------------------------#
# Predict all converged models on their corresponding regional grids
# ------------------------------------------------------------------------------#

predict_all_converged_models <- function(converged_models, grids_by_region) {
  
  # Build the list of valid models only
  converged_models_predictions <- list()
  
  for (region_name in names(converged_models)) {
    
    grid_region <- grids_by_region[[region_name]]
    
    for (response in names(converged_models[[region_name]])) {
      
      for (model_name in names(converged_models[[region_name]][[response]])) {
        
        pred_model <- converged_models[[region_name]][[response]][[model_name]]
        
        converged_models_predictions[[region_name]][[response]][[model_name]] <- predict(
          pred_model,
          newdata = grid_region,
          type = "response",
          model = NA
        )
      }
    }
  }
  
  return(converged_models_predictions)
}


# ------------------------------------------------------------------------------#
# Run predictions for all regions, responses, and valid models
# converged_models_predictions <- predict_all_converged_models(converged_models, grids_by_region)

