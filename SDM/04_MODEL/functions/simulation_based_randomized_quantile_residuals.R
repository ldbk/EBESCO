

# ------------------------------------------------------------------------------#
# DHARMa simulation-based residual diagnostics for all valid models
# ------------------------------------------------------------------------------#

rmse_mae_from_dharma_sim <- function(converged_models, region_name = "region") {
  
  if (region_name == "west") {
    data_CGFS <- data_CGFS_west
  } else if (region_name == "east") {
    data_CGFS <- data_CGFS_east
  }
  
  set.seed(123)
  
  models_by_response <- converged_models[[region_name]]    
  
  rmse <- function(obs, sim) sqrt(mean((sim - obs)^2))
  mae  <- function(obs, sim) mean(abs(sim - obs))
  
  metrics_list <- list()  
  
  # --------------------------------------------------------------------------- #
  # Loop over response variables
  # --------------------------------------------------------------------------- #
  for (response_name in names(models_by_response)) {
    
    models_by_family <- models_by_response[[response_name]]
    obs <- data_CGFS[[response_name]]
    
    # ------------------------------------------------------------------------- #
    # Loop over valid models (families) for this response
    # ------------------------------------------------------------------------- #
    for (family_name in names(models_by_family)) {
      
      fitted_model <- models_by_family[[family_name]]
      
      message("Running DHARMa simulations: ", region_name, " - ",
              response_name, " - ", family_name)
      
      # Simulate nsim datasets from the fitted model
      simulated_data <- tryCatch(
        simulate(fitted_model, nsim = 1000, type = "mle-mvn"),
        error = function(e) e
      )

      metrics_list[[length(metrics_list) + 1]] <- tibble::tibble(
        region = region_name,
        response = response_name,
        family = family_name,
        sim = paste0("sim_", seq_len(ncol(simulated_data))),
        RMSE = apply(simulated_data, 2, function(col) rmse(obs, col)),
        MAE  = apply(simulated_data, 2, function(col) mae (obs, col))
      )
    }
  }
  bind_rows(metrics_list)
}


# ------------------------------------------------------------------------------#
# Run DHARMa  diagnostics by region
# ------------------------------------------------------------------------------#

# dharma_by_region <- list()
# 
# if (isTRUE(East_English_Channel)) {
#   dharma_by_region$east <- rmse_mae_from_dharma_sim(converged_models, "east")
# }
# 
# if (isTRUE(West_English_Channel)) {
#   dharma_by_region$west <- rmse_mae_from_dharma_sim(converged_models, "west")
# }


# family_colors <- c(
#   "tweedie" = "#66c2a5",
#   "deltagamma" = "#fc8d62",
#   "deltalognormal" = "#8da0cb",
#   "deltagammapoissonlink" = "#e78ac3",
#   "gamma" = "#ffd92f",
#   "lognormal" = "#a6d854"
# )
# 
# # WEST plots
# # -----------------------------------------------------------------------------#
# 
# if (isTRUE(West_English_Channel)) {
#   
#   metrics_west <- dharma_by_region$west$metrics_df
#   
#   mean_totalWeightKg_west <- mean(data_CGFS_west$totalWeightKg)
#   mean_densityKgKm2_west <- mean(data_CGFS_west$densityKgKm2)
#   
#   obs_means_west <- tibble(
#     response = c("totalWeightKg", "densityKgKm2"),
#     mean_obs  = c(mean_totalWeightKg_west, mean_densityKgKm2_west))
#   
#   west_MAE <- ggplot(metrics_west %>% filter(response %in% obs_means_west$response),
#                      aes(x = family, y = MAE, color = family)) +
#     scale_y_log10() +    
#     geom_boxplot(outlier.alpha = 0.2) +
#     facet_wrap(~ response, scales = "free_y") +
#     geom_hline(data = obs_means_west, aes(yintercept = mean_obs),
#                linetype = "dotted", linewidth = 0.7) +
#     scale_color_manual(values = family_colors)+
#     labs(x = NULL, y = "MAE", subtitle = "Dotted line = average observations") +
#     theme_bw() +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1), 
#           legend.position = "none")
#   
#   west_RMSE <- ggplot(metrics_west %>% filter(response %in% obs_means_west$response),
#                       aes(x = family, y = RMSE, color = family)) +
#     scale_y_log10() +    
#     geom_boxplot(outlier.alpha = 0.2) +
#     facet_wrap(~ response, scales = "free_y") +
#     geom_hline(data = obs_means_west, aes(yintercept = mean_obs),
#                linetype = "dotted", linewidth = 0.7) +
#     scale_color_manual(values = family_colors)+
#     labs(x = NULL, y = "RMSE", 
#          title = "West CGFS",
#          subtitle = "Dotted line = average observations") +
#     theme_bw() +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1), 
#           legend.position = "none")
#   
# }
# 
# 
# # EAST plots
# # -----------------------------------------------------------------------------#
# if (isTRUE(East_English_Channel)) {
# 
#   metrics_east <- dharma_by_region$east$metrics_df
# 
#   mean_totalWeightKg_east <- mean(data_CGFS_east$totalWeightKg)
#   mean_densityKgKm2_east <- mean(data_CGFS_east$densityKgKm2)
# 
#   obs_means_east <- tibble(
#     response = c("totalWeightKg", "densityKgKm2"),
#     mean_obs  = c(mean_totalWeightKg_east, mean_densityKgKm2_east))
#   
#   east_MAE <- ggplot(metrics_east %>% filter(response %in% obs_means_east$response),
#                      aes(x = family, y = MAE, color = family)) +
#     scale_y_log10() +    
#     geom_boxplot(outlier.alpha = 0.2) +
#     facet_wrap(~ response, scales = "free_y") +
#     geom_hline(data = obs_means_east, aes(yintercept = mean_obs),
#                linetype = "dotted", linewidth = 0.7) +
#     scale_color_manual(values = family_colors)+
#     labs(x = NULL, y = "MAE", 
#          subtitle = "Dotted line = average observations") +
#     theme_bw() +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1), 
#           legend.position = "none")
#   
#   east_RMSE <- ggplot(metrics_east %>% filter(response %in% obs_means_east$response),
#                       aes(x = family, y = RMSE, color = family)) +
#     scale_y_log10() +    
#     geom_boxplot(outlier.alpha = 0.2) +
#     facet_wrap(~ response, scales = "free_y") +
#     geom_hline(data = obs_means_east, aes(yintercept = mean_obs),
#                linetype = "dotted", linewidth = 0.7) +
#     scale_color_manual(values = family_colors)+
#     labs(x = NULL, y = "RMSE", 
#          title = "East CGFS",
#          subtitle = "Dotted line = average observations") +
#     theme_bw() +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1), 
#           legend.position = "none")
# 
# }
