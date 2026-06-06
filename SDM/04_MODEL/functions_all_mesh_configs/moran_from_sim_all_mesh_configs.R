# ------------------------------------------------------------------------------#
# Compute Moran's I on DHARMa scaled residuals from sdmTMB models
# across multiple independent simulations, by year
#
# Arguments:
#   converged_models : nested list [region > mesh > response > family]
#   region_name      : "west" or "east"
#   nsim_outer       : number of independent simulation rounds (default 100)
#   nsim_inner       : simulations per DHARMa batch (default 50, for stable quantiles)
#
# Returns:
#   tibble with columns:
#     region, mesh, response, family, sim_id, year,
#     moran_observed, moran_expected, moran_sd, moran_p.value
# ------------------------------------------------------------------------------#

moran_from_sim <- function(converged_models,
                           region_name = "region",
                           nsim_outer  = 10,
                           nsim_inner  = 10) {
  
  # -- Select data according to region ----------------------------------------
  if (region_name == "west") {
    data_CGFS <- data_CGFS_west
  } else if (region_name == "east") {
    data_CGFS <- data_CGFS_east
  } else {
    stop("region_name must be 'west' or 'east'")
  }
  
  models_by_mesh <- converged_models[[region_name]]
  results_list <- list()
  dharma_list <- list() 
  
  # -- Loop over mesh ----------------------------------------------------------
  for (mesh_name in names(models_by_mesh)) {
    
    models_by_response <- models_by_mesh[[mesh_name]]
    
    # -- Loop over response variables ------------------------------------------
    for (response_name in names(models_by_response)) {
      
      models_by_family <- models_by_response[[response_name]]
      obs <- data_CGFS[[response_name]]
      
      # -- Loop over families --------------------------------------------------
      for (family_name in names(models_by_family)) {
        
        fitted_model <- models_by_family[[family_name]]
        model_id <- paste(region_name, mesh_name,
                          response_name, family_name, sep = "_")
        
        message("Moran simulations: ", region_name, " - ", mesh_name,
                " - ", response_name, " - ", family_name)
        
        df_coords <- fitted_model$data |>
          dplyr::select(year, X, Y)
        
        for (s in seq_len(nsim_outer)) {
          
          set.seed(s) 
          
          sim_matrix <- simulate(fitted_model,
                                 nsim = nsim_inner,
                                 type = "mle-mvn")
          
          sim_matrix <- as.matrix(sim_matrix)
          
          fitted_pred <- stats::predict(fitted_model, type = "response")$est
          
          dharma_obj <- DHARMa::createDHARMa(simulatedResponse = sim_matrix,
                                             observedResponse = obs,
                                             fittedPredictedResponse = fitted_pred)
          
          # stocker uniquement le dernier dharma_obj pour les plots
          if (s == nsim_outer) dharma_list[[model_id]] <- dharma_obj
          
          dharma_resids <- dharma_obj$scaledResiduals
          
          df_resid <- dplyr::bind_cols(df_coords,
                                       tibble::tibble(residual = dharma_resids))
          
          moran_by_year <- df_resid |>
            dplyr::group_by(year) |>
            dplyr::group_modify(~ {
              
              dists <- as.matrix(dist(.x[, c("X", "Y")]))
              inv_dist <- 1 / dists
              diag(inv_dist) <- 0
              inv_dist[!is.finite(inv_dist)] <- 0
              
              moran <- ape::Moran.I(.x$residual, inv_dist, scaled = TRUE)
              
              data.frame(moran_observed = moran$observed,
                         moran_expected = moran$expected,
                         moran_sd = moran$sd,
                         moran_p.value  = moran$p.value)
            }) |>
            dplyr::ungroup() |>
            dplyr::mutate(region = region_name,
                          mesh = mesh_name,
                          response = response_name,
                          family = family_name,
                          sim_id = s,
                          .before  = 1)
          
          results_list[[paste(model_id, s, sep = "_s")]] <- moran_by_year
        }
      }
    }
  }
  list(moran_results = dplyr::bind_rows(results_list),
       dharma_plots = dharma_list)
}

