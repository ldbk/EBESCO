# ------------------------------------------------------------------------------#
# ------------------------------------------------------------------------------#
#                SCRIPT TO GENERATE INPUT DATA FOR THE MODEL 
# ------------------------------------------------------------------------------#
# ------------------------------------------------------------------------------#

  
  # ------------------------------------------------------------------------------#
  #### 1. LOAD THE DATA #### 
  # ------------------------------------------------------------------------------#
  
  
  catch_ECGFS <- read_delim("01_DATA/survey/ECGFS/CGFS_1988_2024_ELFIC.V1.4_catch_2025-03-28.csv",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
  
  operation_ECGFS <- read_delim("01_DATA/survey/ECGFS/CGFS_1988_2024_ELFIC.V1.4_operation_2025-03-28.csv",
                                delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
  
  # others_parameters <- read_delim("01_DATA/ECGFS/CGFS_1988_2024_ELFIC.V1.4_otherParameters_2025-03-28.csv", 
  #                                 delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
  # --> many NA for depth variable
  
  # operation_CGFS_clean <- operation_CGFS %>%
  #   filter(!is.na(startLatDD), !is.na(startLongDD))
  
  # unique(catch_CGFS$comment)
  # unique(operation_CGFS$comment)
  # unique(operation_CGFS$gear)
  # unique(catch_CGFS$stationCode)
  
  # lat_range <- range(operation_CGFS$startLatDD)
  # lon_range <- range(operation_CGFS$startLongDD)
  
  
  catch_WCGFS <-  read_delim("01_DATA/survey/WCGFS/WCGFS_2018_2024_ELFIC.V1.5_catch_2025-03-31.csv", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
  
  operation_WCGFS <-  read_delim("01_DATA/survey/WCGFS/WCGFS_2018_2024_ELFIC.V1.5_operation_2025-03-31.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
  
  
  # ------------------------------------------------------------------------------#
  ####  2. TRANSFORM CGFS DATA #### 
  # ------------------------------------------------------------------------------#
  
  # ------------------#
  # Eastern CGFS
  # ------------------#
  
  # select data corresponding to the species of interest
  sp_catch_ECGFS <- catch_ECGFS %>%
    dplyr::filter(scientificName == sp_scientific) %>%
    dplyr::select(serie, year, stationCode, operationID, haulID, totalNumber, totalWeightKg)
  
  # Combine catch, coordinates and other parameters data frames
  sp_data_ECGFS <- operation_ECGFS %>%
    left_join(sp_catch_ECGFS, by = c("serie", "year", "stationCode", "operationID", "haulID"))%>%
    filter(year >= 2018)
  
  
  # ------------------#
  # Western CGFS
  # ------------------#
  
  # select data corresponding to the species of interest
  sp_catch_WCGFS <- catch_WCGFS %>%
    dplyr::filter(scientificName == sp_scientific) %>%
    dplyr::select(serie, year, stationCode, operationID, haulID, totalNumber, totalWeightKg)
  
  # Combine catch, coordinates and other parameters data frames
  sp_data_WCGFS <- operation_WCGFS %>%
    left_join(sp_catch_WCGFS, by = c("serie", "year", "stationCode", "operationID", "haulID")) 
  
  
  # ---------------------------------#
  # Combine Eastern & Western CGFS
  # ---------------------------------#
  sp_data_CGFS <- bind_rows(sp_data_ECGFS, sp_data_WCGFS)
  
  
  # Replace missing values in totalNumber and totalWeightKg with 0
  # Calculate midpoint coordinates for each haul
  # Compute density in kg/km²
  # Create a presence/absence variable (0 = absence, 1 = presence)
  sp_data_CGFS_density <- sp_data_CGFS %>%
    mutate(totalNumber = ifelse(is.na(totalNumber), 0, totalNumber),
           totalWeightKg = ifelse(is.na(totalWeightKg), 0, totalWeightKg)) %>%
    mutate(lat = (endLatDD + startLatDD)/2,
           lon = (endLongDD + startLongDD)/2) %>%
    mutate(densityKgKm2 = totalWeightKg/sweptAreaKm2) %>%
    mutate(presence_absence = ifelse(totalWeightKg == 0, 0, 1))
  
  
 
  # ------------------------------------------------------------------------------#
  ####  3. ADD BATHYMETRY TO DENSITY DATA #### 
  # ------------------------------------------------------------------------------#
  
  
  # Load the bathymetry raster from a NetCDF file
  bathy_EC <- raster(here("01_DATA", "bathymetry", "EC_Bathy", "Bathy_English_Channel.nc"), varname ="depth")
  
  # Plot the cropped bathymetry
  # plot(bathy_EC, main = "Bathymetry of English Channel", col = terrain.colors(100))
  
  bathy_EC_spat <- rast(bathy_EC)
  
  # Convert the CGFS data frame to a spatial vector
  pts_CGFS <- vect(sp_data_CGFS_density,
                   geom = c("lon", "lat"),
                   crs  = crs(bathy_EC_spat))
  
  # Extract depth values from the raster at point locations
  # terra::extract() returns a data frame with ID + raster values
  depth_df <- terra::extract(bathy_EC_spat, pts_CGFS)
  
  # Add depth values to the original data frame
  # The second column of depth_df is the raster value (first is ID)
  sp_data_CGFS_density$depth <- depth_df[, 2]
  
  
  # Plot depth by location and year
  ggplot(sp_data_CGFS_density, aes(x = lon, y = lat)) +
    geom_point() +
    facet_wrap(~ year) +
    scale_color_viridis_c(option = "magma", direction = -1, name = "Depth (m)") +
    labs(title = "Depth (m) at CGFS stations – Eastern English Channel")

  # ------------------------------------------------------------------------------#
  ####  4. ADD SUBSTRATE TO DENSITY DATA #### 
  # ------------------------------------------------------------------------------#
  
  # https://emodnet.ec.europa.eu/en/seabed-habitats 
  
  # Load the substrate raster from a NetCDF file
  substrate_EC <- raster(here("01_DATA", "substrate", "Substrate5levels_English_Channel.nc"))
  
  # Plot the cropped substrate
  # plot(substrate_EC, add=TRUE, main = "Substrate of English Channel")
  
  # Convert the substrate raster from a 'RasterLayer' object (raster package)
  # to a 'SpatRaster' object (terra package) so it can be used with terra functions
  substrate_EC_spat <- rast(substrate_EC)
  
  # Convert the CGFS data frame to a spatial vector
  pts_CGFS_substrate <- vect(sp_data_CGFS_density,
                             geom = c("lon", "lat"),
                             crs  = crs(bathy_EC_spat))
  
  # Extract depth values from the raster at point locations
  # terra::extract() returns a data frame with ID + raster values
  subtrate_df <- terra::extract(substrate_EC_spat, pts_CGFS_substrate)
  
  # Add depth values to the original data frame
  # The second column of depth_df is the raster value (first is ID)
  sp_data_CGFS_density$substrate <- subtrate_df[, 2]
  
  #remove CGFS points out of the English Channel shapefile (depth = NA)
  CGFS_density_bathy <- sp_data_CGFS_density %>%
    filter(!is.na(depth))

  # object to save substrate classes
  substrate_csv <- read_csv(here("01_DATA/substrate/Multiscale - folk 7.csv"))
  substrate_levels <- levels(factor(substrate_csv$folk_5cl_txt))
  
  # ------------------------------------------------------------------------------#
  #### 5. ADD UTM #### 
  # ------------------------------------------------------------------------------#
  
  utm_crs_used <- get_crs(CGFS_density_bathy, c("lon", "lat"))
  
  # Add UTM (Universal Transverse Mercator) coordinates to a data frame
  # This is useful since geostatistical modeling should generally be performed in an equal-distance projection.
  data_CGFS_crs <- add_utm_columns(CGFS_density_bathy, 
                                   ll_names = c("lon", "lat"),   # longitude and latitude column names
                                   ll_crs = 4326,                # CRS = coordinates reference system --> 4326 = WGS84 (most commun)
                                   utm_names = c("X", "Y"),      # output column names for the UTM columns
                                   utm_crs = utm_crs_used,       # output CRS value for the UTM zone
                                   units = "km")                 # UTM units
  
  
  data_CGFS_east <- data_CGFS_crs %>% filter(gear == "GOV 36/47")
  data_CGFS_west <- data_CGFS_crs %>% filter(gear == "GOV 36/49")
  
  
  # data_test_model <- data_CGFS_crs %>% 
  #   dplyr::select(year, X, Y, lat, lon, gear, depth, substrate, presence_absence, densityKgKm2, totalWeightKg, sweptAreaKm2) %>%
  #   mutate(year = factor(year)) %>%
  #   mutate(gear = factor(gear)) %>%
  #   mutate(substrate = factor(substrate))
  
  data_CGFS_east <- data_CGFS_east %>% 
    dplyr::select(year, X, Y, lat, lon, presence_absence, densityKgKm2, totalWeightKg, sweptAreaKm2) %>%
    mutate(year = factor(year)) 
  
  data_CGFS_west <- data_CGFS_west %>% 
    dplyr::select(year, X, Y, lat, lon, presence_absence, densityKgKm2, totalWeightKg, sweptAreaKm2) %>%
    mutate(year = factor(year)) 
  




