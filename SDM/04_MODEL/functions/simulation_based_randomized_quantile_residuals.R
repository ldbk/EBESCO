

# ------------------------------------------------------------------------------#
# DHARMa simulation-based residual diagnostics for all valid models
# ------------------------------------------------------------------------------#

run_dharma_diagnostics <- function(sanity_output, data_CGFS, region_name = "region") {
  
  set.seed(123)
  
  dharma_qq_df <- data.frame(region = character(),
                             response = character(),
                             family = character(),
                             model = character(),
                             expected = numeric(),
                             observed = numeric())
  
  failed_models <- list()
  data_simulations <- list()  
  metrics_by_model <- list()     
  
  rmse <- function(obs, sim) sqrt(mean((sim - obs)^2))
  mae  <- function(obs, sim) mean(abs(sim - obs))
  
  metrics_df <- NULL
  
  # --------------------------------------------------------------------------- #
  # Loop over response variables
  # --------------------------------------------------------------------------- #
  for (response_name in names(sanity_output)) {
    
    models_valid <- sanity_output[[response_name]]$models_valid
    if (length(models_valid) == 0) next
    
    obs <- data_CGFS[[response_name]]
    
    # ensure nested list exists
    if (is.null(data_simulations[[response_name]])) data_simulations[[response_name]] <- list()
    if (is.null(metrics_by_model[[response_name]])) metrics_by_model[[response_name]] <- list()
    
    # ------------------------------------------------------------------------- #
    # Loop over valid models (families) for this response
    # ------------------------------------------------------------------------- #
    for (family_name in names(models_valid)) {
      
      fitted_model <- models_valid[[family_name]]
      model_label  <- paste(region_name, response_name, family_name, sep = " - ")
      
      cat("\nRunning DHARMa diagnostics for:", model_label, "\n")

      # Simulate nsim datasets from the fitted model
      data_simulation <- tryCatch(
        simulate(fitted_model, nsim = 1000, type = "mle-mvn"),
        error = function(e) e
      )
      if (inherits(data_simulation, "error")) {
        failed_models[[model_label]] <- paste("simulate() failed:", data_simulation$message)
        next
      }
      
      data_simulations[[response_name]][[family_name]] <- data_simulation
      
      # RMSE, MAE for each simulation column 
      sim_mat <- as.matrix(data_simulation)
      sim_tbl <- tibble::as_tibble(sim_mat, .name_repair = ~ paste0("sim_", seq_along(.x)))
      
      metrics <- tibble::tibble(
        sim  = names(sim_tbl),
        RMSE = vapply(sim_tbl, function(col) rmse(obs, col), numeric(1)),
        MAE  = vapply(sim_tbl, function(col) mae (obs, col), numeric(1))) %>%
        dplyr::mutate(region = region_name,
                      response = response_name,
                      family = family_name) %>%
        dplyr::select(region, response, family, sim, RMSE, MAE)
      
      metrics_by_model[[response_name]][[family_name]] <- metrics
      
      metrics_df <- dplyr::bind_rows(metrics_df, metrics)
      
      # Convert simulations to a DHARMa object using sdmTMB helper
      dharma_obj <- tryCatch(
        dharma_residuals(data_simulation, fitted_model, return_DHARMa = TRUE),
        error = function(e) e
      )
      if (inherits(dharma_obj, "error")) {
        failed_models[[model_label]] <- paste("dharma_residuals() failed:", dharma_obj$message)
        next
      }
      
      # Extract scaled DHARMa residuals
      dharma_residuals <- dharma_obj$scaledResiduals
      dharma_residuals <- dharma_residuals[is.finite(dharma_residuals)]
      
      lenght_resids <- length(dharma_residuals)
      
      if (lenght_resids == 0) {
        failed_models[[model_label]] <- "No finite DHARMa residuals"
        next
      }
      
      observed <- sort(dharma_residuals)
      expected <- ppoints(lenght_resids)  
      
      dharma_qq_df <- rbind(dharma_qq_df,
                       data.frame(region= region_name,
                                  response = response_name,
                                  family = family_name,
                                  model = model_label,
                                  expected = expected,
                                  observed = observed))
    }
  }
  
  
  # # QQ plot
  # plot_dharma_qq <- NULL
  # if (nrow(dharma_qq_df) > 0) {
  #   plot_dharma_qq <- ggplot(dharma_qq_df, aes(x = expected, y = observed)) +
  #     geom_abline(slope = 1, intercept = 0) +
  #     geom_point(size = 0.8, alpha = 0.7) +
  #     facet_grid(family ~ response) +
  #     labs(x = "Expected", y = "Observed",
  #          title = paste0("DHARMa residual QQ-plot (", region_name, ")")) +
  #     theme_bw()
  # }
  
  return(list(
    dharma_qq_df = dharma_qq_df,
    # plot_dharma_qq = plot_dharma_qq,
    failed_models = failed_models,
    data_simulations = data_simulations,
    metrics_df = metrics_df
  ))
  
}


# ------------------------------------------------------------------------------#
# Run DHARMa  diagnostics by region
# ------------------------------------------------------------------------------#

dharma_by_region <- list()

if (isTRUE(East_English_Channel)) {
  dharma_by_region$east <- run_dharma_diagnostics(
    sanity_output = sanity_by_region$east,
    data_CGFS = data_CGFS_east,
    region_name = "east")
}

if (isTRUE(West_English_Channel)) {
  dharma_by_region$west <- run_dharma_diagnostics(
    sanity_output = sanity_by_region$west,
    data_CGFS = data_CGFS_west,
    region_name = "west")
}


family_colors <- c(
  "tweedie" = "#66c2a5",
  "deltagamma" = "#fc8d62",
  "deltalognormal" = "#8da0cb",
  "deltagammapoissonlink" = "#e78ac3",
  "gamma" = "#ffd92f",
  "lognormal" = "#a6d854"
)

# WEST plots
# -----------------------------------------------------------------------------#

if (isTRUE(West_English_Channel)) {
  
  metrics_west <- dharma_by_region$west$metrics_df
  
  mean_totalWeightKg_west <- mean(data_CGFS_west$totalWeightKg)
  mean_densityKgKm2_west <- mean(data_CGFS_west$densityKgKm2)
  
  obs_means_west <- tibble(
    response = c("totalWeightKg", "densityKgKm2"),
    mean_obs  = c(mean_totalWeightKg_west, mean_densityKgKm2_west))
  
  west_MAE <- ggplot(metrics_west %>% filter(response %in% obs_means_west$response),
                     aes(x = family, y = MAE, color = family)) +
    scale_y_log10() +    
    geom_boxplot(outlier.alpha = 0.2) +
    facet_wrap(~ response, scales = "free_y") +
    geom_hline(data = obs_means_west, aes(yintercept = mean_obs),
               linetype = "dotted", linewidth = 0.7) +
    scale_color_manual(values = family_colors)+
    labs(x = NULL, y = "MAE", subtitle = "Dotted line = average observations") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.position = "none")
  
  west_RMSE <- ggplot(metrics_west %>% filter(response %in% obs_means_west$response),
                      aes(x = family, y = RMSE, color = family)) +
    scale_y_log10() +    
    geom_boxplot(outlier.alpha = 0.2) +
    facet_wrap(~ response, scales = "free_y") +
    geom_hline(data = obs_means_west, aes(yintercept = mean_obs),
               linetype = "dotted", linewidth = 0.7) +
    scale_color_manual(values = family_colors)+
    labs(x = NULL, y = "RMSE", 
         title = "West CGFS",
         subtitle = "Dotted line = average observations") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.position = "none")
  
}


# EAST plots
# -----------------------------------------------------------------------------#
if (isTRUE(East_English_Channel)) {

  metrics_east <- dharma_by_region$east$metrics_df

  mean_totalWeightKg_east <- mean(data_CGFS_east$totalWeightKg)
  mean_densityKgKm2_east <- mean(data_CGFS_east$densityKgKm2)

  obs_means_east <- tibble(
    response = c("totalWeightKg", "densityKgKm2"),
    mean_obs  = c(mean_totalWeightKg_east, mean_densityKgKm2_east))
  
  east_MAE <- ggplot(metrics_east %>% filter(response %in% obs_means_east$response),
                     aes(x = family, y = MAE, color = family)) +
    scale_y_log10() +    
    geom_boxplot(outlier.alpha = 0.2) +
    facet_wrap(~ response, scales = "free_y") +
    geom_hline(data = obs_means_east, aes(yintercept = mean_obs),
               linetype = "dotted", linewidth = 0.7) +
    scale_color_manual(values = family_colors)+
    labs(x = NULL, y = "MAE", 
         subtitle = "Dotted line = average observations") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.position = "none")
  
  east_RMSE <- ggplot(metrics_east %>% filter(response %in% obs_means_east$response),
                      aes(x = family, y = RMSE, color = family)) +
    scale_y_log10() +    
    geom_boxplot(outlier.alpha = 0.2) +
    facet_wrap(~ response, scales = "free_y") +
    geom_hline(data = obs_means_east, aes(yintercept = mean_obs),
               linetype = "dotted", linewidth = 0.7) +
    scale_color_manual(values = family_colors)+
    labs(x = NULL, y = "RMSE", 
         title = "East CGFS",
         subtitle = "Dotted line = average observations") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.position = "none")

}
