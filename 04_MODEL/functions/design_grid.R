
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
# 
# # Mean of the shortest distances 
# summary(shortest_distance_matrix)



# ------------------------------------------------------------------------------#
#### 1. DESIGN GRID WITHOUT RASTER (bathymetry, substrate) #### 
# ------------------------------------------------------------------------------#

if (!isTRUE(depth_FE) && !isTRUE(substrate_factor_FE)) {
  
  # Use the shapefile of the English Channel (EC) to define the grid extent
  if (study_domain == "Entire_English_Channel"){
    EC_shp <- vect(here("01_DATA", "shapefiles", "English_Channel", "English_Channel.shp"))
  } 
  else if (study_domain == "Eastern_English_Channel"){
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
  
  #  replicate values across years
  grid_pred <- replicate_df(grid_pred, "year", factor(unique(data_CGFS_crs$year)))
  
  # If the model includes a gear effect 
  # Add a gear category based on longitude: GOV 36/47 East if lon < -1.43, GOV 36/49 West if lon >= -1.43
  if (isTRUE(gear_factor_FE)) {  
    grid_pred <- grid_pred %>%
      mutate(gear = if_else(lon < -1.43, "GOV 36/47", "GOV 36/49"),
             gear = factor(gear, levels = c("GOV 36/47", "GOV 36/49")))
  }
  
}



subset(data_test_model, presence_absence == 0)


# ------------------------------------------------------------------------------#
#### 2. DESIGN GRID WITH RASTER (bathymetry, substrate) #### 
# ------------------------------------------------------------------------------#

if (isTRUE(depth_FE) || isTRUE(substrate_factor_FE)) {
  
  
  # horizontal and vertical aggregation factor
  fact_x <- res_km / (0.001041667 * 111.32 * cos(pi*50/180))     # 50 = latitude (lat * pi / 180)
  fact_y <- res_km / (0.001041667 * 111.32)  
  
  # Aggregate a raster object to create a new one
  # Aggregation groups rectangular areas to create larger cells

  if (study_domain == "Entire_English_Channel"){
    EC_shp <- vect(here("01_DATA", "shapefiles", "English_Channel", "English_Channel.shp"))
    if (isTRUE(depth_FE)) bathy <- raster(here("01_DATA/bathymetry/EC_Bathy/Bathy_English_Channel.nc"))
    if (isTRUE(substrate_factor_FE)) substrate <- raster(here("01_DATA/substrate/Substrate5levels_English_Channel.nc"))
  
    } else if (study_domain == "Eastern_English_Channel"){
    EC_shp <- vect(here("01_DATA", "shapefiles", "Eastern_English_Channel", "Eastern_English_Channel.shp"))
    if (isTRUE(depth_FE)) bathy <- raster(here("01_DATA/bathymetry/EEC_Bathy/Bathy_Eastern_English_Channel.nc"))
  }    
  
  
  if (isTRUE(depth_FE)){
  # Aggregate the bathymetry raster using the specified horizontal/vertical aggregation factors and a mean function
  bathy_res_km <- raster::aggregate(bathy,                        
                                    fact = c(fact_x, fact_y),     
                                    fun = mean)                   # fun = function used to aggregate values (example mean, modal, min or max)
  
  # plot(bathy_res_km, main = "Bathymetry", col = terrain.colors(100))
  
  # Get a data.frame with raster cell values
  grid_depth <- as.data.frame(bathy_res_km, xy = TRUE) %>%   # xy If TRUE, also return the spatial coordinates
    rename(lon  = x, lat  = y)%>%
    filter(!is.na(depth))
  
  }
  
  if (isTRUE(substrate_factor_FE)){
    # Aggregate the substrate raster using the specified horizontal/vertical aggregation factors and a modal function
    substrate_res_km <- raster::aggregate(substrate, 
                                          fact = c(fact_x, fact_y), 
                                          fun  = modal)
    
    # plot(substrate_res_km, main = "Substrate")
    
    # Get a data.frame with raster cell values
    grid_substrate_before <- as.data.frame(substrate_res_km, xy = TRUE) %>%   # xy If TRUE, also return the spatial coordinates
      rename(lon  = x, lat  = y, substrate = Substrate5levels_English_Channel) %>%
      mutate(substrate = factor(substrate)) %>%
      filter(!is.na(substrate))
    
    if (substrate_rock_handling == "recode_to_neighbors") {
      source(here("04_MODEL/functions/impute_substrate5_focal_iter.R"))
      grid_substrate_after <- impute_substrate5_focal_iter(grid_substrate_before)
    
      } else if(substrate_rock_handling == "exclude_from_grid"){
      grid_substrate_after <- grid_substrate_before %>%
        filter(substrate != 5)
    }
  }

  
  # grid_substrate_after <- impute_substrate5_focal_iter(
  #   grid_substrate_before,
  #   k_neighbors = 8,
  #   min_valid_neighbors = 4,
  #   max_iter = 100
  # )
  # substrate_palette <- c("1" = "#CC99CC",
  #                        "2" = "#fdb462",
  #                        "3" = "#7fc97f",
  #                        "4" = "#80b1d3",
  #                        "5" = "#fb8072")
  # 
  # plot_before <- ggplot(grid_substrate_before, aes(lon, lat, fill = factor(substrate))) +
  #   geom_tile() +
  #   coord_equal() +
  #   scale_fill_manual(values = substrate_palette) +
  #   labs(title = "Avant", x="", y="", fill = "Substrat")
  # 
  # plot_after <- ggplot(grid_substrate_after, aes(lon, lat, fill = factor(substrate))) +
  #   geom_tile() +
  #   coord_equal() +
  #   scale_fill_manual(values = substrate_palette) +
  #   labs(title = "Après", x="", y="", fill = "Substrat")
  # 
  # ggpubr::ggarrange(plot_before,
  #                   plot_after,
  #                   ncol = 1)
  
  if (isTRUE(depth_FE) && isTRUE(substrate_factor_FE)) { 
    global_grid <- merge(grid_substrate_after, grid_depth, by = c("lon", "lat"))                    
  
    } else if (isTRUE(depth_FE) && !isTRUE(substrate_factor_FE)) {
      global_grid = grid_depth         
    } else if (!isTRUE(depth_FE) && isTRUE(substrate_factor_FE)) {
      global_grid = grid_substrate_after
    }
  
  # Compute the prediction grid: extract lon/lat of valid cells and convert to UTM coordinates (km)
  grid_pred <- add_utm_columns(global_grid, 
                               ll_names = c("lon", "lat"), 
                               ll_crs = 4326, 
                               utm_names = c("X", "Y"),
                               utm_crs = utm_crs_used,
                               units = "km")
  
  #  replicate values across years
  grid_pred <- replicate_df(grid_pred, "year", factor(unique(data_CGFS_crs$year)))
  
  # If the model includes a gear effect 
  # Add a gear category based on longitude: GOV 36/47 East if lon < -1.43, GOV 36/49 West if lon >= -1.43
  if (isTRUE(gear_factor_FE)) {  
    grid_pred <- grid_pred %>%
      mutate(gear = if_else(lon < -1.43, "GOV 36/47", "GOV 36/49"),
             gear = factor(gear, levels = c("GOV 36/47", "GOV 36/49")))
  }
  
}


 
# # ------------------------------------------------------------------------------#
# #### 2. DESIGN GRID WITH BATHYMETRY RASTER #### 
# # ------------------------------------------------------------------------------#
# 
# if (isTRUE(depth_FE) && !isTRUE(substrate_factor_FE)) {
#   
#   # grid resolution in km
#   # res_km = 13 --> SOURCE
#   
#   # horizontal and vertical aggregation factor
#   # https://stackoverflow.com/questions/1253499/simple-calculations-for-working-with-lat-lon-and-km-distance
#   fact_x <- res_km / (0.001041667 * 111.32 * cos(pi*50/180))     # 50 = latitude (lat * pi / 180)
#   fact_y <- res_km / (0.001041667 * 111.32)  
#   
#   # Aggregate a raster object to create a new one
#   # Aggregation groups rectangular areas to create larger cells
#   # https://www.rdocumentation.org/packages/raster/versions/3.6-32/topics/aggregate
#   
#   if (study_domain == "Entire_English_Channel"){
#     bathy <- raster(here("01_DATA/bathymetry/EC_Bathy/Bathy_English_Channel.nc"))
#   }
#   else if (study_domain == "Eastern_English_Channel"){
#     bathy <- raster(here("01_DATA/bathymetry/EEC_Bathy/Bathy_Eastern_English_Channel.nc"))
#   }    
#   
#   # Aggregate the bathymetry raster using the specified horizontal/vertical aggregation factors and a mean function
#   bathy_res_km <- raster::aggregate(bathy,                        
#                                     fact = c(fact_x, fact_y),     
#                                     fun = mean)                   # fun = function used to aggregate values (example mean, modal, min or max)
#   
#   
#   # plot(bathy_res_km, main = "Bathymetry", col = terrain.colors(100))
#   
#   # Get a data.frame with raster cell values
#   grid <- as.data.frame(bathy_res_km, xy = TRUE) %>%   # xy If TRUE, also return the spatial coordinates
#     rename(lon  = x, lat  = y)%>%
#     filter(!is.na(depth))
#   
#   # Compute the prediction grid: extract lon/lat of valid cells and convert to UTM coordinates (km)
#   grid_pred <- add_utm_columns(grid, 
#                                ll_names = c("lon", "lat"), 
#                                ll_crs = 4326, 
#                                utm_names = c("X", "Y"),
#                                utm_crs = utm_crs_used,
#                                units = "km")
#   
#   # If the model includes a year effect, replicate values across years
#   if (isTRUE(year_factor_FE)) {   
#     grid_pred <- replicate_df(grid_pred, "year", unique(data_CGFS_crs$year))
#   }
#   
#   # If the model includes a gear effect 
#   # Add a gear category based on longitude: GOV 36/47 East if lon < -1.43, GOV 36/49 West if lon >= -1.43
#   if (isTRUE(gear_factor_FE)) {  
#     grid_pred <- grid_pred %>%
#       mutate(gear = if_else(lon < -1.43, "GOV 36/47", "GOV 36/49"),
#              gear = factor(gear, levels = c("GOV 36/47", "GOV 36/49")))
#   }
#   
# }
# 
# 
# 
# # ------------------------------------------------------------------------------#
# #### 3. DESIGN GRID WITH BATHYMETRY && SUBSTRATE RASTERS #### 
# # ------------------------------------------------------------------------------#
# 
# if (isTRUE(depth_FE) && isTRUE(substrate_factor_FE)) {
#   
#   # grid resolution in km
#   # res_km = 13 --> SOURCE
#   
#   # horizontal and vertical aggregation factor
#   # https://stackoverflow.com/questions/1253499/simple-calculations-for-working-with-lat-lon-and-km-distance
#   fact_x <- res_km / (0.001041667 * 111.32 * cos(pi*50/180))     # 50 = latitude (lat * pi / 180)
#   fact_y <- res_km / (0.001041667 * 111.32)  
#   
#   # Aggregate a raster object to create a new one
#   # Aggregation groups rectangular areas to create larger cells
#   # https://www.rdocumentation.org/packages/raster/versions/3.6-32/topics/aggregate
#   
#   if (study_domain == "Entire_English_Channel"){
#     bathy <- raster(here("01_DATA/bathymetry/EC_Bathy/Bathy_English_Channel.nc"))
#     substrate <- raster(here("01_DATA/substrate/Substrate5levels_English_Channel.nc"))
#   }
#   if (study_domain == "Eastern_English_Channel"){
#     bathy <- raster(here("01_DATA/bathymetry/EEC_Bathy/Bathy_Eastern_English_Channel.nc"))
#   }    
#   
#   # Aggregate the bathymetry raster using the specified horizontal/vertical aggregation factors and a mean function
#   bathy_res_km <- raster::aggregate(bathy,                        
#                                     fact = c(fact_x, fact_y),     
#                                     fun = mean)                   # fun = function used to aggregate values (example mean, modal, min or max)
#   
#   # plot(bathy_res_km, main = "Bathymetry", col = terrain.colors(100))
#   
#   
#   # Get a data.frame with raster cell values
#   grid_depth <- as.data.frame(bathy_res_km, xy = TRUE) %>%   # xy If TRUE, also return the spatial coordinates
#     rename(lon  = x, lat  = y)%>%
#     filter(!is.na(depth))
#   
#   
#   # Aggregate the substrate raster using the specified horizontal/vertical aggregation factors and a modal function
#   substrate_res_km <- raster::aggregate(substrate, 
#                                         fact = c(fact_x, fact_y), 
#                                         fun  = modal)
#   
#   # plot(substrate_res_km, main = "Substrate")
#   
#   # Get a data.frame with raster cell values
#   # grid_substrate_before <- as.data.frame(substrate_res_km, xy = TRUE) %>%   # xy If TRUE, also return the spatial coordinates
#   #   rename(lon  = x, lat  = y, substrate = Substrate5levels_English_Channel)%>%
#   #   filter(!is.na(substrate))
#   
#   grid_substrate <- as.data.frame(substrate_res_km, xy = TRUE) %>%   # xy If TRUE, also return the spatial coordinates
#     rename(lon  = x, lat  = y, substrate = Substrate5levels_English_Channel)%>%
#     filter(!is.na(substrate))
#   
#   # source(here("04_MODEL/functions/impute_substrate5_focal_iter.R"))
#   
#   # grid_substrate <- impute_substrate5_focal_iter(grid_substrate_before)
#   
#   # grid_substrate_after <- impute_substrate5_focal_iter(
#   #   grid_substrate_before,
#   #   k_neighbors = 8,
#   #   min_valid_neighbors = 4,
#   #   max_iter = 100
#   # )
#   # substrate_palette <- c("1" = "#CC99CC",
#   #                        "2" = "#fdb462",
#   #                        "3" = "#7fc97f",
#   #                        "4" = "#80b1d3",
#   #                        "5" = "#fb8072")
#   # 
#   # plot_before <- ggplot(grid_substrate_before, aes(lon, lat, fill = factor(substrate))) +
#   #   geom_tile() +
#   #   coord_equal() +
#   #   scale_fill_manual(values = substrate_palette) +
#   #   labs(title = "Avant", x="", y="", fill = "Substrat")
#   # 
#   # plot_after <- ggplot(grid_substrate_after, aes(lon, lat, fill = factor(substrate))) +
#   #   geom_tile() +
#   #   coord_equal() +
#   #   scale_fill_manual(values = substrate_palette) +
#   #   labs(title = "Après", x="", y="", fill = "Substrat")
#   # 
#   # ggpubr::ggarrange(plot_before,
#   #                   plot_after,
#   #                   ncol = 1)
# 
#   
#   
#   # Merge bathymetry and substrate grids
#   global_grid <- merge(grid_substrate, grid_depth, by = c("lon", "lat"))
#   
#   # Compute the prediction grid: extract lon/lat of valid cells and convert to UTM coordinates (km)
#   grid_pred <- add_utm_columns(global_grid, 
#                                ll_names = c("lon", "lat"), 
#                                ll_crs = 4326, 
#                                utm_names = c("X", "Y"),
#                                utm_crs = utm_crs_used,
#                                units = "km")
#   
#   # If the model includes a year effect, replicate values across years
#   if (isTRUE(year_factor_FE)) {   
#     grid_pred <- replicate_df(grid_pred, "year", unique(data_CGFS_crs$year))
#   }
#   
#   # If the model includes a gear effect
#   # western CGFS data longitude range : –5.88° to –1.53° 
#   # eastern CGFS data longitude range : –1.32° to +1.65°
#   # midpoint : –1.43°
#   # Add a gear category based on longitude: GOV 36/47 East, GOV 36/49 West 
#   if (isTRUE(gear_factor_FE)) {  
#     grid_pred <- grid_pred %>%
#       mutate(gear = if_else(lon < -1.43, "GOV 36/47", "GOV 36/49"),
#              gear = factor(gear, levels = c("GOV 36/47", "GOV 36/49")))
#   }
#   
#   
# }





