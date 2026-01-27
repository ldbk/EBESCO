
# ============================================================================== #
#### COMPUTE QUANTILE RESIDUALS + DIAGNOSTIC PLOTS (BY REGION) ####
# ============================================================================== #

compute_quantile_residuals <- function(sanity_output, region_name = "region") {
  
  residuals_long <- list()
  
  # --------------------------------------------------------------------------- #
  # Loop over response variables
  # --------------------------------------------------------------------------- #
  for (response_name in names(sanity_output)) {
    
    models_valid <- sanity_output[[response_name]]$models_valid
    
    # Skip if no valid model for this response
    if (length(models_valid) == 0) next
    
    # ------------------------------------------------------------------------- #
    # Loop over distribution families for this response
    # ------------------------------------------------------------------------- #
    for (family_name in names(models_valid)) {
      # Retrieve the fitted model
      fitted_model <- models_valid[[family_name]]
      
      
      # --------------------------------------------------------#
      # Delta models: extract residuals for both components
      # --------------------------------------------------------#
      if (grepl("^delta", family_name, ignore.case = TRUE)) {
        
        for (component_id in 1:2) {
          
          # Raw randomized quantile residuals returned by the model
          residuals_raw <- tryCatch(
            residuals(fitted_model, type = "mle-mvn", model = component_id),
            error = function(e) NULL
          )
          if (is.null(residuals_raw)) next
          
          # Convert to a plain numeric vector
          residuals_vec <- as.numeric(unlist(residuals_raw))
          
          # Keep only finite values (remove NA, NaN, Inf)
          residuals_vec <- residuals_vec[is.finite(residuals_vec)]
          
          # Store only if there are valid residuals
          if (length(residuals_vec) > 0) {
            residuals_long[[length(residuals_long) + 1]] <- data.frame(
              response = response_name,
              family = family_name,
              component = paste0("comp.", component_id),
              resid = residuals_vec)
          }
        }
        
        
        # --------------------------------------------------------#
        # Single component models
        # --------------------------------------------------------#  
      } else {
        
        # Raw randomized quantile residuals returned by the model
        residuals_raw <- tryCatch(
          residuals(fitted_model, type = "mle-mvn"),
          error = function(e) NULL
        )
        if (is.null(residuals_raw)) next
        
        # Convert to a plain numeric vector
        residuals_vec <- as.numeric(unlist(residuals_raw))
        
        # Keep only finite values (remove NA, NaN, Inf)
        residuals_vec <- residuals_vec[is.finite(residuals_vec)]
        
        # Store only if there are valid residuals
        if (length(residuals_vec) > 0) {
          residuals_long[[length(residuals_long) + 1]] <- data.frame(
            response = response_name,
            family = family_name,
            component = "comp.1",  
            resid = residuals_vec)
        }
      }
    }
  }
  
  # --------------------------------------------------------------------------- #
  # If no residuals were collected, return an empty result safely
  # --------------------------------------------------------------------------- #
  if (length(residuals_long) == 0) {
    return(list(
      residuals_df = data.frame(),
      plot_normalqq = NULL,
      plot_hist_frq = NULL,
      plot_hist_density = NULL
    ))
  }
  
  # Bind all residuals into a single data.frame
  residuals_df <- do.call(rbind, residuals_long)
  
  residuals_df$response <- factor(residuals_df$response, c("totalWeightKg", "densityKgKm2"))
  residuals_df$component <- factor(residuals_df$component, c("comp.1", "comp.2"))
  
  # Normal Q-Q plot of quantile residuals
  plot_normalqq <- ggplot(residuals_df, aes(sample = resid)) +
    stat_qq() +
    stat_qq_line() +
    facet_grid(family ~ response + component, scales = "free") +
    labs(x = "Theoretical quantiles", y = "Quantile residuals",
         title = "Normal Q-Q Plot") +
    theme_bw()
  
  # Histogram (frequencies)
  plot_hist_frq <- ggplot(residuals_df, aes(x = resid, fill = family)) +
    geom_histogram(bins = 30, alpha = 0.5, position = "identity") +
    facet_grid(family ~ response + component) +
    labs(x = "Quantile residuals", y = "Frequency",
         title = "Histogram of quantile residuals (frequencies)") +
    theme_bw() +
    theme(legend.position = "none")
  
  # Histogram (density) + N(0,1) reference curve
  plot_hist_density <- ggplot(residuals_df, aes(x = resid, fill = family)) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 30, alpha = 0.5, position = "identity") +
    stat_function(fun = dnorm, linewidth = 0.6, colour = "grey") +
    facet_grid(family ~ response + component) +
    labs(x = "Quantile residuals",y = "Density",
         title = "Histogram of quantile residuals (density)") +
    theme_bw() +
    theme(legend.position = "none")
  
  # Return residuals and plots in a single object
  return(list(
    residuals_df = residuals_df,
    plot_normalqq = plot_normalqq,
    plot_hist_frq = plot_hist_frq,
    plot_hist_density  = plot_hist_density
  ))
  
}


# --------------------------------------------------------#
#### Run residuals computations + plots by regions  ####
# --------------------------------------------------------#

residuals_by_region <- list()

# East English Channel
if (isTRUE(East_English_Channel)) {
  residuals_by_region$east <- compute_quantile_residuals(
    sanity_output = sanity_by_region$east,
    region_name = "east"
  )
}

# West English Channel
if (isTRUE(West_English_Channel)) {
  residuals_by_region$west <- compute_quantile_residuals(
    sanity_output = sanity_by_region$west,
    region_name = "west"
  )
}


