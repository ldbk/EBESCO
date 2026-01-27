
# ============================================================================== #
# DESIGN GRID AGGREGATE RASTER CELLS (UTM grid, clipped cells, area per cell)   #
# ============================================================================== #

design_grids <- function(data_CGFS, region_name = "region") {


  # Use the shapefile of the English Channel to define the grid extent
  if (region_name == "east"){
    EC_shp <- vect(here("01_DATA", "shapefiles", "split_English_Channel", "east_English_Channel.shp"))
  } else if (region_name == "west"){
    EC_shp <- vect(here("01_DATA", "shapefiles", "split_English_Channel", "west_English_Channel.shp"))
  }


  # ------------------------------------------------------------------------------#
  #### Build UTM grid, clip to polygon, compute area by cell ####
  # ------------------------------------------------------------------------------#

  # Extract the spatial extent of the EC shapefile
  EC_ext <- ext(EC_shp)

  # Convert the grid resolution to degrees only to build and mask the raster using the shapefile.
  # Then convert back to kilometers because the model requires metric coordinates.
  # Conversion from km to degrees at ~50°N latitude
  res_deg_lat <- res_km / 111.32
  res_deg_lon <- res_km / (111.32 * cos(50*pi/180))

  # Create a regular raster grid with given resolution
  grid_rast <- rast(extent = EC_ext,
                    resolution = c(res_deg_lon, res_deg_lat),
                    crs = crs(EC_shp))

  # Add dummy values (e.g., 1) to all cells so that mask() can work
  grid_rast[] <- 1

  # Keep only cells located inside the English Channel polygon
  grid_rast_EC <- mask(grid_rast, EC_shp)

  # Extract the coordinates of valid cell centers (only inside EC)
  cells_in_EC <- which(!is.na(grid_rast_EC[]))

  # Get cell center coordinates
  coords <- xyFromCell(grid_rast_EC, cells_in_EC) %>%
    as.data.frame() %>%
    dplyr::rename(lon = x, lat = y)

  # Compute the prediction grid: extract lon/lat of valid cells and convert to UTM coordinates (km)
  grid_pred <- add_utm_columns(coords,
                               ll_names = c("lon", "lat"),
                               ll_crs = 4326,
                               utm_names = c("X", "Y"),
                               utm_crs = utm_crs_used,
                               units = "km")

  # replicate across years
  grid_pred <- replicate_df(grid_pred, "year", factor(unique(data_CGFS$year)))


  xmin_plot <- min(grid_pred$lon) - res_deg_lon
  xmax_plot <- max(grid_pred$lon) + res_deg_lon
  ymin_plot <- min(grid_pred$lat) - res_deg_lat
  ymax_plot <- max(grid_pred$lat) + res_deg_lat

  land <- ne_download(scale = "large", type = "land", category = "physical", returnclass = "sf") %>%
    st_transform(4326) %>%
    st_crop(xmin = xmin_plot, ymin = ymin_plot, xmax = xmax_plot, ymax = ymax_plot)


  plot_grid <- ggplot(grid_pred, aes(lon, lat)) +
    geom_tile(width = res_deg_lon, height = res_deg_lat, color = "black", fill = "white") +
    geom_sf(data=land, fill="grey80", inherit.aes = FALSE, color = NA) +
    coord_sf(xlim = c(xmin_plot, xmax_plot),
             ylim = c(ymin_plot, ymax_plot),
             expand = FALSE) +
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = "white", colour = NA),
          panel.border = element_rect(color = "white", fill = NA, linewidth = 2),
          strip.background = element_rect(fill = "grey40", color = "white"),
          strip.text = element_text(color = "white", face = "bold"))+
    labs(x = "", y = "") +
    ggtitle(bquote("Grille de projection (résolution de " *.(res_km)* " km)"))


  # Return everything useful
  return(list(region = region_name,
              grid_pred = grid_pred,
              plot_grid = plot_grid))

}


# ------------------------------------------------------------------------------#
#### Build grids by region depending on user-defined flags ####
# ------------------------------------------------------------------------------#

grid_by_region <- list()

if (isTRUE(East_English_Channel)) {
  grid_by_region$east <- design_grids(data_CGFS = data_CGFS_east, region_name = "east")
}

if (isTRUE(West_English_Channel)) {
  grid_by_region$west <- design_grids(data_CGFS = data_CGFS_west, region_name = "west")
}


