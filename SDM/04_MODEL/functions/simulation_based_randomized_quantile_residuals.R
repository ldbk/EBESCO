
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
  
  # Take the dataset based on the region
  if (region_name == "west") {
    data_CGFS <- data_CGFS_west
  } else if (region_name == "east") {
    data_CGFS <- data_CGFS_east
  }
  
  set.seed(123)
  
  # Get models for this region
  models_by_response <- converged_models[[region_name]]    
  
  # Functions to compute errors
  rmse <- function(obs, sim) sqrt(mean((sim - obs)^2))
  mae  <- function(obs, sim) mean(abs(sim - obs))
  
  # Store results here
  metrics_list <- list()  
  dharma_QQplot_list <- list()
  dharma_Moranplot_list <- list()
  
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
      model_id <- paste(region_name, response_name, family_name, sep = "_")
     
       message("Running simulations: ",region_name," - ",response_name," - ",family_name)
      
      # Simulate nsim datasets from the fitted model
      simulated_data <- simulate(fitted_model, nsim = 1000, type = "mle-mvn")

      # Compute RMSE and MAE for each simulation
      metrics_list[[model_id]] <- tibble::tibble(
        region = region_name,
        response = response_name,
        family = family_name,
        sim = paste0("sim_", seq_len(ncol(simulated_data))),
        RMSE = apply(simulated_data, 2, function(col) rmse(obs, col)),
        MAE = apply(simulated_data, 2, function(col) mae (obs, col)))
      
      dharma_QQplot_list[[model_id]] <- dharma_residuals(simulated_data, 
                                                         fitted_model, 
                                                         return_DHARMa = TRUE)
    }
  }
  
  list(metrics = dplyr::bind_rows(metrics_list),
       dharma = list(residuals = dharma_QQplot_list,
                     Moran_plot = dharma_Moranplot_list))
  
}
