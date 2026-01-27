

# ------------------------------------------------------------------------------#
# DHARMa simulation-based residual diagnostics for all valid models
# ------------------------------------------------------------------------------#

run_dharma_diagnostics <- function(sanity_output, region_name = "region") {
  
  set.seed(123)
  
  dharma_qq_df <- data.frame(region   = character(),
                              response = character(),
                              family   = character(),
                              model    = character(),
                              expected = numeric(),
                              observed = numeric())
  
  failed_models <- list()
  
  # --------------------------------------------------------------------------- #
  # Loop over response variables
  # --------------------------------------------------------------------------- #
  for (response_name in names(sanity_output)) {
    
    models_valid <- sanity_output[[response_name]]$models_valid
    if (length(models_valid) == 0) next
    
    # ------------------------------------------------------------------------- #
    # Loop over valid models (families) for this response
    # ------------------------------------------------------------------------- #
    for (family_name in names(models_valid)) {
      
      fitted_model <- models_valid[[family_name]]
      model_label  <- paste(region_name, response_name, family_name, sep = " - ")
      
      cat("\nRunning DHARMa diagnostics for:", model_label, "\n")
      
      # Simulate nsim datasets from the fitted model
      data_simulation <- tryCatch(
        simulate(fitted_model, nsim = 10000, type = "mle-mvn"),
        error = function(e) e
      )
      if (inherits(data_simulation, "error")) {
        failed_models[[model_label]] <- paste("simulate() failed:", data_simulation$message)
        next
      }
      
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
  
  
  # QQ plot
  plot_dharma_qq <- NULL
  if (nrow(dharma_qq_df) > 0) {
    plot_dharma_qq <- ggplot(dharma_qq_df, aes(x = expected, y = observed)) +
      geom_abline(slope = 1, intercept = 0) +
      geom_point(size = 0.8, alpha = 0.7) +
      facet_grid(family ~ response) +
      labs(x = "Expected", y = "Observed",
           title = paste0("DHARMa residual QQ-plot (", region_name, ")")) +
      theme_bw()
  }
  
  return(list(
    dharma_qq_df = dharma_qq_df,
    plot_dharma_qq = plot_dharma_qq,
    failed_models = failed_models
  ))
  
}


# ------------------------------------------------------------------------------#
# Run DHARMa  diagnostics by region
# ------------------------------------------------------------------------------#

dharma_by_region <- list()

if (isTRUE(East_English_Channel)) {
  dharma_by_region$east <- run_dharma_diagnostics(
    sanity_output = sanity_by_region$east,
    region_name = "east")
}

if (isTRUE(West_English_Channel)) {
  dharma_by_region$west <- run_dharma_diagnostics(
    sanity_output = sanity_by_region$west,
    region_name = "west")
}

