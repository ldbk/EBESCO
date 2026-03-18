


plot_meshes_patchwork <- function(meshes_options_region, ncol){
  
  plots <- purrr::map(meshes_options_region, function(current_converged_model) {
    
    region_name = current_converged_model$region
    data_CGFS <- if (region_name == "west") {
      data_CGFS_west
    } else if (region_name == "east") {
      data_CGFS_east
    } 
    
    mesh_m <- current_converged_model$mesh$mesh
    mesh_m$loc <- mesh_m$loc * 1000
    
    # Compute the mesh bounding box in meters
    mesh_m_bbox <- sf::st_bbox(c(xmin = min(mesh_m$loc[, 1]),
                                 xmax = max(mesh_m$loc[, 1]),
                                 ymin = min(mesh_m$loc[, 2]),
                                 ymax = max(mesh_m$loc[, 2])), crs = utm_crs_used)
    
    # Build "land_mesh" containing coastlines cropped to the mesh extent
    land_mesh <- rnaturalearth::ne_countries(scale = "large",
                                             country = c("United Kingdom", "France"),
                                             returnclass = "sf") %>%
      sf::st_transform(utm_crs_used) %>%
      sf::st_crop(mesh_m_bbox)
    
    cutoff <- current_converged_model$cutoff
    
    plot <- ggplot() +
      geom_sf(data = land_mesh, fill = "grey80", color = NA) +
      gg(mesh_m, edge.color = "grey50", edge.linewidth = 0.2) +
      ggspatial::geom_spatial_point(data = data_CGFS, aes(lon, lat), shape = 1,
                                    color = "black", size = 1.2, alpha = 0.5) +
      coord_sf(crs = st_crs(land_mesh)) +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.ticks = element_line(colour = "black")) +
      labs(x = "", y = "",
           subtitle = paste0("cutoff = ", round(cutoff, 1), " km"))
    
    bspde <- current_converged_model$bspde
    if (!is.null(bspde)) {
      mesh_df_water <- bspde$mesh_sf[bspde$normal_triangles, ]
      mesh_df_land  <- bspde$mesh_sf[bspde$barrier_triangles, ]
      
      plot <- plot +
        ggplot2::geom_sf(data = mesh_df_water, colour = "darkblue", size = 1.2, shape = 1, alpha = 0.3) +
        ggplot2::geom_sf(data = mesh_df_land, colour = "red", size = 1.2, shape = 1, alpha = 0.3)
    }
    
    plot
    
  })
  
  combined_plot <- patchwork::wrap_plots(plots, ncol = ncol)  
}

