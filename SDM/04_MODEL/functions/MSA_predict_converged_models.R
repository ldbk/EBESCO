# ==============================================================================#
# Compute prediction from converged models
# ==============================================================================#

# ------------------------------------------------------------------------------#
# Prediction grid
# ------------------------------------------------------------------------------#

res_km <- 5
source(here::here("04_MODEL/functions/design_grid.R"))
grid_design <- design_grids(region_name)
prediction_grid <- grid_design$grid_pred

# ------------------------------------------------------------------------------#
# Simulate predictions on response scale and compute CV
# ------------------------------------------------------------------------------#

make_predictions <- function(models_converged, grid) {
  
  purrr::map_dfr(models_converged, function(current_converged_model) {
    
    fit <- current_converged_model$fit
    cutoff <- current_converged_model$cutoff
    n_vertices <- current_converged_model$n_vertices
    barrier <- current_converged_model$barrier
    boundary <- current_converged_model$boundary
    region <- current_converged_model$region
    
    predictions <- predict(fit,
                           newdata = grid,
                           type = "response",
                           model = NA)    # NA (default) returns the combined prediction from both components
                           
  
    predictions_output <- predictions
    predictions_output$cutoff <- cutoff
    predictions_output$n_vertices <- n_vertices
    predictions_output$barrier <- barrier
    predictions_output$boundary <- boundary
    predictions_output$region <- region
    
    predictions_output
  })
}
    


  
  
