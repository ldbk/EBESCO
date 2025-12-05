
# ------------------------------------------------------------------------------#
# DESIGN GRID AGGREGATE RASTER CELLS # 
# ------------------------------------------------------------------------------#


# ------------------------------------------------------------------------------#
#### Compute shortest mean distance ####
# ------------------------------------------------------------------------------#

# Compute the nearest neighbor euclidean distance (km) for all points each year
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/dist 
# shortest_distance_matrix <- data_CGFS_crs %>%
#   dplyr::select(year, X, Y) %>%
#   group_by(year) %>%
#   mutate(
#     nearest_neighbor_dist_km = {                    
#       distance_matrix <- as.matrix(stats::dist(cbind(X, Y), method = "euclidean"))
#       diag(distance_matrix) <- NA                     # remove self-distance 
#       apply(distance_matrix, 1, min, na.rm = TRUE)    # keep minimum distance for each row/point
#     }
#   ) %>%
#   ungroup()

# Mean of the shortest distances per year
# mean_shortest_distance <- shortest_distance_matrix %>%
#   group_by(year) %>%
#   summarise(mean_nearest_neighbor_dist_km = mean(nearest_neighbor_dist_km),
#             sd_nearest_neighbor_dist_km = sd(nearest_neighbor_dist_km),
#             n_points = n())

# check Nsamp vs n_points 
# ckeck_Nsamp <- data_CGFS_crs %>%
#   group_by(year) %>%
#   summarise(Nsamp = length(unique(haulID)))

# global_mean_dist <- mean_shortest_distance %>% summarise(mean_dist = mean(mean_nearest_neighbor_dist_km))



# ------------------------------------------------------------------------------#
#### 1. DESIGN GRID WITH BATHYMETRY RASTER #### 
# ------------------------------------------------------------------------------#

if (grepl("depth", model_version)) {
  
  # grid resolution in km
  # res_km = 13 --> SOURCE
  
  # horizontal and vertical aggregation factor
  # https://stackoverflow.com/questions/1253499/simple-calculations-for-working-with-lat-lon-and-km-distance
  fact_x <- res_km / (0.001041667 * 111.32 * cos(pi*50/180))     # 50 = latitude (lat * pi / 180)
  fact_y <- res_km / (0.001041667 * 111.32)  
  
  # Aggregate a raster object to create a new one
  # Aggregation groups rectangular areas to create larger cells
  # https://www.rdocumentation.org/packages/raster/versions/3.6-32/topics/aggregate
  
  if (study_domain == "Entire_English_Channel"){
    bathy <- raster(here("01_DATA/bathymetry/EC_Bathy/Bathy_English_Channel.nc"))
  }
  if (study_domain == "Eastern_English_Channel"){
    bathy <- raster(here("01_DATA/bathymetry/EEC_Bathy/Bathy_Eastern_English_Channel.nc"))
  }    
  
  # Aggregate the bathymetry raster using the specified horizontal/vertical aggregation factors and a mean function
  bathy_res_km <- raster::aggregate(bathy,                        
                                    fact = c(fact_x, fact_y),     
                                    fun = mean)                   # fun = function used to aggregate values (example mean, modal, min or max)
  
  
  # plot(bathy_res_km, main = "Bathymetry", col = terrain.colors(100))
  
  # Get a data.frame with raster cell values
  # https://www.rdocumentation.org/packages/raster/versions/3.6-32/topics/as.data.frame 
  grid <- as.data.frame(bathy_res_km, xy = TRUE) %>%   # xy If TRUE, also return the spatial coordinates
    rename(lon  = x, lat  = y)%>%
    filter(!is.na(depth))
  
  # Check dataframe
  # ggplot(grid) +
  #   geom_raster(aes(x = lon, y = lat, fill = depth))
  
  # Compute the prediction grid: extract lon/lat of valid cells and convert to UTM coordinates (km)
  grid_pred <- add_utm_columns(grid, 
                               ll_names = c("lon", "lat"), 
                               ll_crs = 4326, 
                               utm_names = c("X", "Y"),
                               utm_crs = utm_crs_used,
                               units = "km")
  
  # ggplot(grid_pred) +
  #   geom_raster(aes(x = X, y = Y, fill = depth))
  
  # If the model includes a year effect, replicate values across years
  if (grepl("year", model_version) && !grepl("gear", model_version)) {   
    grid_pred <- replicate_df(grid_pred, "year", unique(data_CGFS_crs$year))
  }
  
}



# ------------------------------------------------------------------------------#
#### 2. DESIGN GRID WITHOUT COVARIABLE #### 
# ------------------------------------------------------------------------------#

if (!grepl("depth", model_version)) {              
  
  # Use the shapefile of the English Channel (EC) to define the grid extent
  if (study_domain == "Entire_English_Channel"){
    EC_shp <- vect(here("01_DATA", "shapefiles", "English_Channel", "English_Channel.shp"))
  }
  if (study_domain == "Eastern_English_Channel"){
    EC_shp <- vect(here("01_DATA", "shapefiles", "Eastern_English_Channel", "Eastern_English_Channel.shp"))
  }    
  
  # Extract the spatial extent of the EC shapefile
  EC_ext <- ext(EC_shp)     
  
  # We convert the grid resolution to degrees only to build and mask the raster using the shapefile.
  # Then we convert back to kilometers because the model requires metric coordinates.
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
  
  # If the model includes a year effect, replicate values across years
  if (grepl("year", model_version)) {  
    grid_pred <- replicate_df(grid_pred, "year", unique(data_CGFS_crs$year))
  }
  
}







