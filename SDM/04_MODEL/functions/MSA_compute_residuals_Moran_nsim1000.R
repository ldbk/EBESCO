

# Computes Moran’s index on simulated residuals across multiple runs for each model and year, 
# and summarises the frequency of significant spatial autocorrelation.


compute_residuals_Moran_DHARMa <- function(models_converged) {
  
  set.seed(123)
  
  purrr::map_dfr(models_converged, function(current_converged_model) {
    
    # Extract model components and metadata
    fit <- current_converged_model$fit
    computation_time <- current_converged_model$elapsed_fit
    cutoff <- current_converged_model$cutoff
    n_vertices <- current_converged_model$n_vertices
    barrier <- current_converged_model$barrier
    boundary <- current_converged_model$boundary
    region <- current_converged_model$region
    
    data_mod <- fit$data
    
    # Function to run a single simulation
    one_sim <- function(i) {
      
      message(region, " | barrier=", barrier, " | boundary=", boundary, " | cutoff=", cutoff, " | sim=", i)       
      # Simulate residuals (may vary between runs)
      residuals_i <- tryCatch(residuals(fit, type = "mle-mvn"), error = function(e) NULL)
      
      # Return NULL if residuals failed
      if (is.null(residuals_i)) return(NULL)
      
      # Combine residuals with spatial and temporal data
      df_res <- data_mod %>%
        dplyr::mutate(residual = as.numeric(residuals_i)) %>%
        dplyr::select(year, lat, lon, residual)
      
      # Compute Moran's index by year
      df_res %>%
        dplyr::group_by(year) %>%
        dplyr::filter(!all(is.na(residual)), !all(residual == 0)) %>%
        dplyr::group_modify(~{
          dists <- as.matrix(dist(.x[, c("lat", "lon")]))
          inv_dists <- 1 / dists
          diag(inv_dists) <- 0
          inv_dists[is.infinite(inv_dists)] <- 0
          moran <- tryCatch(ape::Moran.I(.x$residual, inv_dists, scaled = TRUE),
                            error = function(e) NULL)
          data.frame(moran_observed = moran$observed,
                     moran_expected = moran$expected,
                     moran_sd = moran$sd,
                     moran_p.value = moran$p.value)
        }) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(sim = i,
                      signif = dplyr::case_when(moran_p.value < 0.001 ~ "***",
                                                moran_p.value < 0.01  ~ "**",
                                                moran_p.value < 0.05  ~ "*",
                                                TRUE ~ "ns"),
                      significant = moran_p.value < 0.05,
                      cutoff = cutoff,
                      n_vertices = n_vertices,
                      barrier = barrier,
                      boundary = boundary,
                      region = region)
    }
    
    # Run all simulations
    sims_all <- purrr::map_dfr(seq_len(1000), one_sim)
    
    # Summarise results by year and model configuration
    sims_summary <- sims_all %>%
      dplyr::group_by(year, cutoff, n_vertices, barrier, boundary, region) %>%
      dplyr::summarise(
        n_sim_valid = sum(!is.na(moran_p.value)),
        n_moran_signif = sum(significant, na.rm = TRUE),
        prop_moran_signif = mean(significant, na.rm = TRUE),
        mean_moran_observed = mean(moran_observed, na.rm = TRUE),
        sd_moran_observed = sd(moran_observed, na.rm = TRUE),
        q025_moran_observed = stats::quantile(moran_observed, 0.025, na.rm = TRUE),
        q975_moran_observed = stats::quantile(moran_observed, 0.975, na.rm = TRUE),
        .groups = "drop"
      )
    
    sims_summary
  })
}
