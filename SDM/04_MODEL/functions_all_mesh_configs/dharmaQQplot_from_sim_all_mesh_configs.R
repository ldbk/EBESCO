# ------------------------------------------------------------------------------#
# Compute and store DHARMa residual objects from sdmTMB models
#
# Arguments:
#   converged_models : nested list [region > mesh > response > family]
#   region_name      : "west" or "east"
#   nsim             : number of simulations for DHARMa residuals
#   seed             : seed for reproducibility
#
# Returns:
#   named list of DHARMa objects, one per model
# ------------------------------------------------------------------------------#

dharma_from_sim <- function(converged_models,
                            region_name = "region",
                            nsim = 1000,
                            seed = 1) {
  
  # Select data according to region 
  if (region_name == "west") {
    data_CGFS <- data_CGFS_west
  } else if (region_name == "east") {
    data_CGFS <- data_CGFS_east
  } else {
    stop("region_name must be 'west' or 'east'")
  }
  
  models_by_mesh <- converged_models[[region_name]]
  dharma_list <- list()
  
  # Loop over mesh 
  for (mesh_name in names(models_by_mesh)) {
    
    models_by_response <- models_by_mesh[[mesh_name]]
    
    # Loop over response variables
    for (response_name in names(models_by_response)) {
      
      models_by_family <- models_by_response[[response_name]]
      obs <- data_CGFS[[response_name]]
      
      # Loop over families 
      for (family_name in names(models_by_family)) {
        
        fitted_model <- models_by_family[[family_name]]
        
        model_id <- paste(region_name, mesh_name, response_name, family_name, sep = "_")
        
        message("DHARMa residuals: ", region_name, " - ", mesh_name,
                " - ", response_name, " - ", family_name)
        
        set.seed(seed)
        
        sim_matrix <- simulate(fitted_model, nsim = nsim, type = "mle-mvn")
        
        fitted_pred <- predict(fitted_model, type = "response")$est
        
        dharma_obj <- DHARMa::createDHARMa(simulatedResponse = as.matrix(sim_matrix),
                                           observedResponse = obs,
                                           fittedPredictedResponse = fitted_pred)
        
        dharma_list[[model_id]] <- dharma_obj
      }
    }
  }
  
  return(dharma_list)
}