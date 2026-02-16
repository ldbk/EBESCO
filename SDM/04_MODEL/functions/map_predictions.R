
# ==============================================================================#
# Map predictions for all converged models
# ==============================================================================#

# ------------------------------------------------------------------------------#
# Build land polygons and plotting bounds per region 
# ------------------------------------------------------------------------------#

get_land_region <- function(grid_pred) {
  
  # Convert resolution from km to degrees
  res_deg_lat <- res_km / 111.32
  res_deg_lon <- res_km / (111.32 * cos(50*pi/180))
  
  # Define plot limits with a one-cell buffer
  xmin_plot <- range(grid_pred$lon)[1] - res_deg_lon
  xmax_plot <- range(grid_pred$lon)[2] + res_deg_lon
  ymin_plot <- range(grid_pred$lat)[1] - res_deg_lat
  ymax_plot <- range(grid_pred$lat)[2] + res_deg_lat
  
  land <- ne_download(scale = "large", type = "land", category = "physical", returnclass = "sf") %>%
    st_transform(4326) %>%
    st_crop(xmin = xmin_plot, ymin = ymin_plot, xmax = xmax_plot, ymax = ymax_plot)
  
  list(land = land,
       xlim = c(xmin_plot, xmax_plot),
       ylim = c(ymin_plot, ymax_plot))
  
}

land_by_region <- list(east = get_land_region(grids_by_region$east),
                       west = get_land_region(grids_by_region$west))


# ------------------------------------------------------------------------------#
# Plot prediction maps for a given variable and model
# ------------------------------------------------------------------------------#

plot_pred_map <- function(data, land_obj, response, fill_base, subtitle) {
  
  legend_title <- ifelse(response == "totalWeightKg",
                         "Biomass (kg)",
                         "Density of \nbiomass \n(kg/km²)")
  
  p <- ggplot(data, aes(lon, lat, fill = .data[[fill_base]])) +
    geom_raster() +  
    geom_sf(data = land_obj$land, fill = "grey80", inherit.aes = FALSE, color = "grey80") +
    scale_y_continuous(breaks = pretty_breaks(n = 3)) +
    scale_x_continuous(breaks = pretty_breaks(n = 4)) +
    coord_sf(xlim = land_obj$xlim, ylim = land_obj$ylim, expand = FALSE) +
    facet_wrap(~year, nrow = 2) +
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = "white", colour = NA),
          panel.border = element_rect(color = "white", fill = NA, linewidth = 2),
          strip.background = element_rect(fill = "grey40", color = "white"),
          strip.text = element_text(color = "white", face = "bold")) +
    labs(x = "", y = "", subtitle = subtitle)
  
  if (fill_base == "est") p <- p + labs(fill = legend_title)
  
  if (fill_base %in% c("est", "est_non_rf")) {
    p <- p + scale_fill_distiller(trans = "log10", palette = "RdYlBu",
                                  labels = label_number(drop0trailing = TRUE))
  } else if (fill_base %in% c("omega_s", "epsilon_st")) {
    p <- p + scale_fill_gradient2(midpoint = 0,
                                  labels = label_number(drop0trailing = TRUE))
  } 
  return(p)
  
}


# ------------------------------------------------------------------------------#
# Variables to plot
# ------------------------------------------------------------------------------#

delta_models <- c("deltagamma", "deltalognormal", "deltagammapoissonlink")
# one_comp_models <- c("tweedie", "gamma", "lognormal")  

prediction_maps <- list()

for (region in names(converged_models_predictions)) {
  
  land_obj <- land_by_region[[region]]
  
  for (response in names(converged_models_predictions[[region]])) {
    
    for (model_name in names(converged_models_predictions[[region]][[response]])) {
      
      predictions <- converged_models_predictions[[region]][[response]][[model_name]]
      predictions <- as.data.frame(predictions)
      
      # choose which variables to plot depending on the model type
      if (model_name %in% delta_models) {
        fills_to_plot <- "est"
      } else {
        fills_to_plot <- c("est", "est_non_rf", "omega_s", "epsilon_st")
      }

      for (fill_base in fills_to_plot) {
        
        p <- plot_pred_map(
          data = predictions,
          land_obj = land_obj,
          response = response,
          fill_base = fill_base,
          subtitle = paste(region, response, model_name, fill_base, sep = " - ")
        )
        
        prediction_maps[[region]][[response]][[model_name]][[fill_base]] <- p
      }
    }
  }
}



# ------------------------------------------------------------------------------#
# Example access:
# prediction_maps$east$densityKgKm2$deltagamma$est
# prediction_maps$east$densityKgKm2$tweedie$omega_s
# prediction_maps$east$totalWeightKg$tweedie$omega_s
# ------------------------------------------------------------------------------#
