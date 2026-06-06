
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
    
    grid_region_base <- grids_by_region[[region_name]]
    
    for (mesh_name in names(converged_models[[region_name]])) {
        
      for (response in names(converged_models[[region_name]][[mesh_name]])) {
        
        grid_region <- grid_region_base
        
        if (response == "totalWeightKg") {
          grid_region$logSweptAreaKm2 <- log(res_km * res_km)
          offset_pred <- grid_region$logSweptAreaKm2
        } else {
          offset_pred <- NULL
        }
        
        for (model_name in names(converged_models[[region_name]][[mesh_name]][[response]])) {
          
          pred_model <- converged_models[[region_name]][[mesh_name]][[response]][[model_name]]
          
          predictions <- predict(pred_model, 
                                 newdata = grid_region, 
                                 type = "response", 
                                 model = NA,
                                 offset = offset_pred)
          
          predictions_index <- predict(pred_model,
                                       newdata = grid_region,
                                       type = "response",
                                       model = NA,
                                       offset = offset_pred,
                                       return_tmb_object = TRUE)
          
          index <- get_index(predictions_index, area = res_km*res_km, bias_correct = TRUE)
          converged_models_predictions[[region_name]][[mesh_name]][[response]][[model_name]] <- list(
            predictions = predictions,
            index = index
          )
          
        }
      }
    }
  }
  return(converged_models_predictions)
}


# ------------------------------------------------------------------------------#
# Run predictions for all regions, responses, and valid models
# converged_models_predictions <- predict_all_converged_models(converged_models, grids_by_region)

