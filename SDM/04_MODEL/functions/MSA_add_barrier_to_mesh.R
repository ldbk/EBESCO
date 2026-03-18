
add_barrier_to_mesh <- function(mesh0) {
  
  # Extract mesh node coordinates
  mesh_coords <- as.data.frame(mesh0$mesh$loc) %>%
    dplyr::rename(X_km = V1, Y_km = V2) %>%
    dplyr::mutate(X_m = X_km * 1000, Y_m = Y_km * 1000)
  
  mesh_sf <- sf::st_as_sf(mesh_coords, coords = c("X_m", "Y_m"), crs = utm_crs_used)
  mesh_bbox <- sf::st_bbox(mesh_sf)
  
  # Load land polygons and crop to mesh extent
  land <- rnaturalearth::ne_countries(scale = "large",
                                      country = c("United Kingdom", "France"),
                                      returnclass = "sf")%>%
    sf::st_transform(utm_crs_used) %>%
    sf::st_crop(mesh_bbox) 
  
  # Add barrier SPDE mesh
  bspde <- sdmTMBextra::add_barrier_mesh(spde_obj = mesh0,
                                         barrier_sf = land,
                                         range_fraction = 0.1,
                                         proj_scaling = 1000)  
  
  return(list(mesh = bspde, bspde = bspde))
  
}