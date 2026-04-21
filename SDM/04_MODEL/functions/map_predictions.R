
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
  
  land <- rnaturalearth::ne_download(scale = "large", type = "land", category = "physical", returnclass = "sf") %>%
    sf::st_transform(4326) %>%
    sf::st_crop(xmin = xmin_plot, ymin = ymin_plot, xmax = xmax_plot, ymax = ymax_plot)
  
  list(land = land,
       xlim = c(xmin_plot, xmax_plot),
       ylim = c(ymin_plot, ymax_plot))
  
}

land_by_region <- list()

if (!is.null(grid_by_region$east$grid_pred)) {
  land_by_region$east <- get_land_region(grid_by_region$east$grid_pred)
}

if (!is.null(grid_by_region$west$grid_pred)) {
  land_by_region$west <- get_land_region(grid_by_region$west$grid_pred)
}

# ------------------------------------------------------------------------------#
# Plot prediction maps for a given variable and model
# ------------------------------------------------------------------------------#

plot_pred_map <- function(data, land_obj, response, subtitle) {
  
  legend_title <- ifelse(response == "totalWeightKg",
                         "Biomass (kg)",
                         "Density of \nbiomass \n(kg/km²)")
  
  ggplot(data, aes(lon, lat, fill = est)) +
    geom_raster() +  
    geom_sf(data = land_obj$land, fill = "grey80", inherit.aes = FALSE, color = "grey80") +
    scale_y_continuous(breaks = pretty_breaks(n = 3)) +
    scale_x_continuous(breaks = pretty_breaks(n = 4)) +
    coord_sf(xlim = land_obj$xlim, ylim = land_obj$ylim, expand = FALSE) +
    facet_wrap(~year, nrow = 2) +
    scale_fill_distiller(trans = "log10", palette = "RdYlBu",
                         labels = label_number(drop0trailing = TRUE))+
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = "white", colour = NA),
          panel.border = element_rect(color = "white", fill = NA, linewidth = 2),
          strip.background = element_rect(fill = "grey40", color = "white"),
          strip.text = element_text(color = "white", face = "bold")) +
    labs(x = "", y = "", subtitle = subtitle, fill = legend_title)
}


# ------------------------------------------------------------------------------#
# Variables to plot
# ------------------------------------------------------------------------------#

prediction_maps <- list()

for (region in names(converged_models_predictions)) {
  
  prediction_maps[[region]] <- list()
  land_obj <- land_by_region[[region]]
  
  for (response in names(converged_models_predictions[[region]])) {
    
    prediction_maps[[region]][[response]] <- list()
    
    for (model_name in names(converged_models_predictions[[region]][[response]])) {
      
      predictions <- converged_models_predictions[[region]][[response]][[model_name]]
      predictions <- as.data.frame(predictions)
      
        p <- plot_pred_map(
          data = predictions,
          land_obj = land_obj,
          response = response,
          subtitle = paste(region, response, model_name, sep = " - ")
        )
        
        prediction_maps[[region]][[response]][[model_name]] <- p
    }
  }
}



