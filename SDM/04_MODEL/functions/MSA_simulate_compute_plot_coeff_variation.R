# ==============================================================================#
# Simulation of predictions + Coffecient Variation maps 
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

simulate_cv <- function(models_converged, grid) {
  
  purrr::map_dfr(models_converged, function(current_converged_model) {
    
    fit <- current_converged_model$fit
    cutoff <- current_converged_model$cutoff
    cAIC <- current_converged_model$cAIC
    n_vertices <- current_converged_model$n_vertices
    barrier <- current_converged_model$barrier
    boundary <- current_converged_model$boundary
    region <- current_converged_model$region
    
    set.seed(123)
    
    simulations <- predict(fit,
                           newdata = grid,
                           type = "response",
                           model = NA,    # NA (default) returns the combined prediction from both components
                           nsim = 100)
    
    
    cv <- apply(simulations, 1, function(x) sd(x) / mean(x))
    
    cv_grid_output <- grid
    cv_grid_output$cv <- cv
    
    cv_grid_output$cutoff <- cutoff
    cv_grid_output$cAIC <- cAIC
    cv_grid_output$n_vertices <- n_vertices
    cv_grid_output$barrier <- barrier
    cv_grid_output$boundary <- boundary
    cv_grid_output$region <- region
    
    cv_grid_output
  })
}
    


  
  
