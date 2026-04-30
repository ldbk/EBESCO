# ==============================================================================#
# Simulation of predictions + Coffecient Variation maps 
# ==============================================================================#

# ------------------------------------------------------------------------------#
# Prediction grids by region
# ------------------------------------------------------------------------------#
grids_by_region <- list(
  east = grid_by_region$east$grid_pred,
  west = grid_by_region$west$grid_pred
)

# ------------------------------------------------------------------------------#
# function to run nsim predictions on response scale and compute CV
# ------------------------------------------------------------------------------#

simulate_cv <- function(model, grid) {
  
  set.seed(123)
  
  simulations <- predict(model, 
                         newdata = grid, 
                         type = "response", 
                         model = NA, 
                         nsim = 1000)
  

  grid$cv <- apply(simulations, 1, function(x) sd(x) / mean(x))

  return(grid)
}

# ------------------------------------------------------------------------------#
# Apply simulations function for all valid models
# ------------------------------------------------------------------------------#
sim_cv_all <- list()

for (region in names(converged_models)) {
  
  grid_region <- grids_by_region[[region]]

  for (response in names(converged_models[[region]])) {
    
    for (model_name in names(converged_models[[region]][[response]])) {
      
      mod <- converged_models[[region]][[response]][[model_name]]
      
      pred_cv <- simulate_cv(mod, grid_region)
      
      sim_cv_all[[region]][[response]][[model_name]] <- pred_cv
    }
  }
}

# CV common limits
cv_range_global <- range(unlist(lapply(sim_cv_all, function(region_list) {
  unlist(lapply(region_list, function(response_list) {
    lapply(response_list, function(df) df$cv)
  }))
})))

# ------------------------------------------------------------------------------#
# Build land polygons + bounds per region (for maps)
# ------------------------------------------------------------------------------#
get_land_region <- function(grid_df) {
  
  # Convert resolution from km to degrees
  res_deg_lat <- res_km / 111.32
  res_deg_lon <- res_km / (111.32 * cos(50*pi/180))
  
  # Define plot limits with a one-cell buffer
  xmin_plot <- range(grid_df$lon)[1] - res_deg_lon
  xmax_plot <- range(grid_df$lon)[2] + res_deg_lon
  ymin_plot <- range(grid_df$lat)[1] - res_deg_lat
  ymax_plot <- range(grid_df$lat)[2] + res_deg_lat
  
  land <- land_ne %>%
    st_transform(4326) %>%
    st_crop(xmin = xmin_plot, ymin = ymin_plot, xmax = xmax_plot, ymax = ymax_plot)
  
  list(land = land,
       xlim = c(xmin_plot, xmax_plot),
       ylim = c(ymin_plot, ymax_plot))
  
}

# Build land objects once per region 
land_by_region <- list()
for (region in names(sim_cv_all)) {
  first_response <- names(sim_cv_all[[region]])[1]
  first_model <- names(sim_cv_all[[region]][[first_response]])[1]
  grid_region <- sim_cv_all[[region]][[first_response]][[first_model]]
  land_by_region[[region]] <- get_land_region(grid_region)
}

# ------------------------------------------------------------------------------#
# Plot function for coefficient variation map 
# ------------------------------------------------------------------------------#
plot_cv_map <- function(data, land_obj, subtitle, fill_limits) {
  
  ggplot(data, aes(lon, lat, fill = cv)) +
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
    labs(x = "", y = "", subtitle = subtitle, fill = "CV") +
    scale_fill_viridis_c(trans = "log10", limits = fill_limits, oob = scales::squish,
                         labels = label_number(drop0trailing = TRUE))
}



# ------------------------------------------------------------------------------#
# Apply plot CV function to create CV plots for every region/response/model
# ------------------------------------------------------------------------------#
plots_cv_all <- list()

for (region in names(sim_cv_all)) {
  
  land_obj <- land_by_region[[region]]
  
  for (response in names(sim_cv_all[[region]])) {
    
    for (model_name in names(sim_cv_all[[region]][[response]])) {
      
      df <- as.data.frame(sim_cv_all[[region]][[response]][[model_name]])
      
      plots_cv_all[[region]][[response]][[model_name]] <- plot_cv_map(
        data = df,
        land_obj = land_obj,
        subtitle = paste(region, response, model_name, "CV", sep = " - "),
        fill_limits = cv_range_global
      )
    }
  }
}





