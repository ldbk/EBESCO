
# ------------------------------------------------------------------------------#
# Compute RMSE and MAE from simulated datasets for all converged models 
#
#   - take the observed data
#   - simulate 1000 datasets from the model
#   - compare simulated vs observed values by computing RMSE and MAE
#
# Arguments:
#   converged_models : list of converged models (by region, variable, family)
#   region_name : west or east
#
# Returns: tibble with RMSE and MAE for each simulation
# ------------------------------------------------------------------------------#

rmse_mae_from_sim <- function(converged_models, region_name = "region") {
  
  # Select data according to region
  if (region_name == "west") {
    data_CGFS <- data_CGFS_west
  } else if (region_name == "east") {
    data_CGFS <- data_CGFS_east
  }
  
  set.seed(123)
  
  # Get models for this region
  models_by_mesh <- converged_models[[region_name]]    
  
  # Functions to compute errors
  rmse <- function(obs, sim) sqrt(mean((sim - obs)^2))
  mae  <- function(obs, sim) mean(abs(sim - obs))
  
  # Store results here
  metrics_list <- list()  

  for (mesh_name in names(models_by_mesh)) {
    
    models_by_response <- models_by_mesh[[mesh_name]]
    
  # --------------------------------------------------------------------------- #
  # Loop over response variables
  # --------------------------------------------------------------------------- #
  for (response_name in names(models_by_response)) {
    
    models_by_family <- models_by_response[[response_name]]
    obs <- data_CGFS[[response_name]]

    # ------------------------------------------------------------------------- #
    # Loop over the distribution families of the converged models for this response
    # ------------------------------------------------------------------------- #
    for (family_name in names(models_by_family)) {
      
      fitted_model <- models_by_family[[family_name]]
      model_id <- paste(region_name, mesh_name, response_name, family_name, sep = "_")
     
       message("Running simulations: ",
               region_name," - ",mesh_name," - ",
               response_name," - ",family_name)
      

      simulated_data <- simulate(fitted_model, nsim = 1000, type = "mle-mvn")
      
      simulated_data <- as.matrix(simulated_data)
      colnames(simulated_data) <- paste0("sim_", seq_len(ncol(simulated_data)))
      
      metrics_list[[model_id]] <- tibble::tibble(
        region = region_name,
        mesh = mesh_name,
        response = response_name,
        family = family_name,
        sim = colnames(simulated_data),
        RMSE = apply(simulated_data, 2, function(col) rmse(obs, col)),
        MAE  = apply(simulated_data, 2, function(col) mae(obs, col))
      )
      
    }
   }
  }
  
  dplyr::bind_rows(metrics_list)
  
}
