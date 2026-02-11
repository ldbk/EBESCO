
# ============================================================================== #
#### COMPUTE QUANTILE RESIDUALS + DIAGNOSTIC PLOTS (BY REGION) ####
# ============================================================================== #

compute_quantile_residuals <- function(sanity_output, region_name = "region", data_CGFS) {
  
  residuals_long <- list()
  maps_long <- list()
  
  # --------------------------------------------------------------------------- #
  # Loop over response variables
  # --------------------------------------------------------------------------- #
  for (response_name in names(sanity_output)) {
    
    models_valid <- sanity_output[[response_name]]$models_valid
    if (length(models_valid) == 0) next
    
    # ------------------------------------------------------------------------- #
    # Loop over distribution families
    # ------------------------------------------------------------------------- #
    for (family_name in names(models_valid)) {
      
      fitted_model <- models_valid[[family_name]]
      
      # Delta models → 2 components, others → 1
      components <- if (grepl("^delta", family_name, ignore.case = TRUE)) 1:2 else 1
      
      map_store <- list()
      
      for (component_id in components) {
        
        # -------------------------------------------------------- #
        # Quantile residuals
        # -------------------------------------------------------- #
        residuals_raw <- tryCatch(
          {
            if (length(components) == 2) {
              residuals(fitted_model, type = "mle-mvn", model = component_id)
            } else {
              residuals(fitted_model, type = "mle-mvn")
            }
          },
          error = function(e) NULL
        )
        if (is.null(residuals_raw)) next
        
        residuals_vec <- as.numeric(unlist(residuals_raw))
        residuals_vec <- residuals_vec[is.finite(residuals_vec)]
        if (length(residuals_vec) == 0) next
        
        component_lab <- paste0("comp.", component_id)
        
        # -------------------------------------------------------- #
        # Store residuals (QQ + hist)
        # -------------------------------------------------------- #
        residuals_long[[length(residuals_long) + 1]] <- data.frame(
          response = response_name,
          family = family_name,
          component = component_lab,
          resid = residuals_vec
        )
        
        # -------------------------------------------------------- #
        # Residual maps
        # -------------------------------------------------------- #
        if (nrow(data_CGFS) == length(residuals_vec)) {
          
          map_store[[component_lab]] <- data.frame(
            lon = data_CGFS$lon,
            lat = data_CGFS$lat,
            year = data_CGFS$year,
            component = component_lab,
            residuals = residuals_vec
          )
        }
        # -------------------------------------------------------- #
        # Build one map per (response, family) with panels by component
        # -------------------------------------------------------- #
        if (length(map_store) > 0) {
          
          map_df_all <- do.call(rbind, map_store)
          
          p_map <- ggplot(map_df_all, aes(lon, lat, fill = residuals)) +
            geom_point(color = "grey", size = 2.5, shape = 21) +
            scale_fill_gradient2() +
            facet_grid(component ~ year) +  
            labs(
              x = "", y = "",
              fill = "Quantile residuals",
              title = paste(region_name, response_name, family_name, sep = " - ")
            ) +
            theme_bw()
          
          key <- paste(response_name, family_name, "map_by_component", sep = "_")
          maps_long[[key]] <- p_map
        }
        
      }
    }
  }
  
  # --------------------------------------------------------------------------- #
  # Empty safety
  # --------------------------------------------------------------------------- #
  if (length(residuals_long) == 0) {
    return(list(
      residuals_df = data.frame(),
      plot_normalqq = NULL,
      plot_hist_frq = NULL,
      plot_hist_density = NULL,
      maps = maps_long
    ))
  }
  
  # --------------------------------------------------------------------------- #
  # Combine residuals
  # --------------------------------------------------------------------------- #
  residuals_df <- do.call(rbind, residuals_long)
  
  residuals_df$response  <- factor(residuals_df$response,
                                   c("totalWeightKg", "densityKgKm2"))
  residuals_df$component <- factor(residuals_df$component,
                                   c("comp.1", "comp.2"))
  
  # --------------------------------------------------------------------------- #
  # Diagnostic plots
  # --------------------------------------------------------------------------- #
  plot_normalqq <- ggplot(residuals_df, aes(sample = resid)) +
    stat_qq() +
    stat_qq_line() +
    facet_grid(family ~ response + component, scales = "free") +
    labs(x = "Theoretical quantiles",
         y = "Quantile residuals",
         title = "Normal Q–Q plot of quantile residuals") +
    theme_bw()
  
  plot_hist_frq <- ggplot(residuals_df, aes(x = resid, fill = family)) +
    geom_histogram(bins = 30, alpha = 0.5, position = "identity") +
    facet_grid(family ~ response + component) +
    labs(x = "Quantile residuals",
         y = "Frequency",
         title = "Histogram of quantile residuals") +
    theme_bw() +
    theme(legend.position = "none")
  
  plot_hist_density <- ggplot(residuals_df, aes(x = resid, fill = family)) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 30, alpha = 0.5, position = "identity") +
    stat_function(fun = dnorm, linewidth = 0.6, colour = "grey") +
    facet_grid(family ~ response + component) +
    labs(x = "Quantile residuals",
         y = "Density",
         title = "Density of quantile residuals") +
    theme_bw() +
    theme(legend.position = "none")
  
  # --------------------------------------------------------------------------- #
  # Return
  # --------------------------------------------------------------------------- #
  return(list(
    residuals_df = residuals_df,
    plot_normalqq = plot_normalqq,
    plot_hist_frq = plot_hist_frq,
    plot_hist_density = plot_hist_density,
    maps = maps_long
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
    region_name = "east", 
    data_CGFS = data_CGFS_east
  )
}

# West English Channel
if (isTRUE(West_English_Channel)) {
  residuals_by_region$west <- compute_quantile_residuals(
    sanity_output = sanity_by_region$west,
    region_name = "west",
    data_CGFS = data_CGFS_west
  )
}

residuals_by_region$east$maps$densityKgKm2_deltagamma_map_by_component
View(residuals_by_region$east$residuals_df)
