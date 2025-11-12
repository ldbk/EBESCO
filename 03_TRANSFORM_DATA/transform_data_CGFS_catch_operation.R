#################################################################################
#                SCRIPT TO GENERATE INPUT DATA FOR THE MODEL 
#################################################################################

# ------------------------------------------------------------------------------
# LOAD THE DATA 
# ------------------------------------------------------------------------------


catch_CGFS <- read_delim("01_DATA/CGFS-ELFIC-1988-2024-SEANOE/CGFS_1988_2024_ELFIC.V1.4_catch_2025-03-28.csv",
                         delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)

operation_CGFS <- read_delim("01_DATA/CGFS-ELFIC-1988-2024-SEANOE/CGFS_1988_2024_ELFIC.V1.4_operation_2025-03-28.csv",
                             delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)

# others_parameters <- read_delim("01_DATA/CGFS-ELFIC-1988-2024-SEANOE/CGFS_1988_2024_ELFIC.V1.4_otherParameters_2025-03-28.csv", 
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


# ------------------------------------------------------------------------------
# TRANSFORM CGFS DATA 
# ------------------------------------------------------------------------------

# select data corresponding to the species of interest
sp_catch_CGFS <- catch_CGFS %>%
  dplyr::filter(scientificName == sp_scientific) %>%
  dplyr::select(serie, year, stationCode, operationID, haulID, totalNumber, totalWeightKg)

# Combine catch, coordinates and other parameters data frames
sp_data_CGFS <- operation_CGFS %>%
  left_join(sp_catch_CGFS, by = c("serie", "year", "stationCode", "operationID", "haulID")) 

# Replace missing values in totalNumber and totalWeightKg with 0
# Calculate midpoint coordinates for each haul
# Compute density in kg/km²
# Create a presence/absence variable (0 = absence, 1 = presence)
# Keep only data from 2015 onwards
sp_data_CGFS_density <- sp_data_CGFS %>%
  mutate(totalNumber = ifelse(is.na(totalNumber), 0, totalNumber),
         totalWeightKg = ifelse(is.na(totalWeightKg), 0, totalWeightKg)) %>%
  mutate(lat = (endLatDD + startLatDD)/2,
         lon = (endLongDD + startLongDD)/2) %>%
  mutate(densityKgKm2 = totalWeightKg/sweptAreaKm2) %>%
  mutate(presence_absence = ifelse(totalWeightKg == 0, 0, 1))%>%
  filter(year >= 2015)


# ------------------------------------------------------------------------------
# LOAD AND PLOT BATHYMETRY
# ------------------------------------------------------------------------------

# Load the bathymetry raster from a NetCDF file
bathy_emod <- raster(here("01_DATA", "E4_2024.nc", "E4_2024.nc"), varname ="elevation")

# Plot the bathymetry
plot(bathy_emod, main = "Bathymetry EMOD 2024", col = terrain.colors(100))

# Define the bounding box coordinates: xmin, xmax, ymin, ymax
bbox <- c(-1.9, 2.5, 49.2, 51.4)

# Create an extent object from the bounding box
bbox_extent <- extent(bbox)

# Crop the raster to the defined extent
bathy_crop <- crop(bathy_emod, bbox_extent)

# Plot the cropped bathymetry
plot(bathy_crop, main = "Bathymetry of Eastern English Channel", col = terrain.colors(100))


# ------------------------------------------------------------------------------
# ADD BATHYMETRY TO DENSITY DATA
# ------------------------------------------------------------------------------

# Extract the longitude and latitude columns from the dataset
coordinates <- sp_data_CGFS_density[, c("lon", "lat")]

# Extract the raster depth values at the specified coordinates and store them in a 'depth' column in the data frame
sp_data_CGFS_density$depth <- raster::extract(bathy_crop, coordinates)

# Define the bounding box coordinates: xmin, xmax, ymin, ymax from CGFS hauls coordinates
bbox_CGFS <- st_bbox(st_as_sf(sp_data_CGFS_density, coords = c("lon", "lat"), crs = 4326))

# --- Convertir le raster en data.frame pour ggplot ---
bathy_df <- as.data.frame(bathy_emod, xy = TRUE, na.rm = TRUE)
colnames(bathy_df) <- c("lon", "lat", "elevation")

# --- Carte annuelle avec fond bathymétrique ---
annual_map_sp <- basemap(limits = bbox_CGFS) +
  geom_raster(data = sp_data_CGFS_density, aes(x = lon, y = lat, fill = elevation), alpha = 0.6) +
  scale_fill_viridis(option = "C", name = "Élévation (m)") 




# ------------------------------------------------------------------------------
# ADD UTM AND CREATE MESH
# ------------------------------------------------------------------------------

# Add UTM (Universal Transverse Mercator) coordinates to a data frame
# This is useful since geostatistical modeling should generally be performed in an equal-distance projection.
data_CGFS_crs <- add_utm_columns(sp_density_CGFS, 
                                 ll_names = c("lon", "lat"),   # longitude and latitude column names
                                 ll_crs = 4326,                # CRS = coordinates reference system --> 4326 = WGS84 (most commun)
                                 utm_names = c("X", "Y"),      # output column names for the UTM columns
                                 utm_crs = get_crs(sp_density_CGFS, c("lon", "lat")),   # output CRS value for the UTM zone
                                 units = c("km", "m"))         # UTM units

# Create a mesh object that contains matrices to apply the SPDE approach
# Cutoff defines the minimum allowed distance between mesh vertices in the units of X and Y
mesh <- make_mesh(data_CGFS_crs, c("X", "Y"), cutoff = 10)
plot(mesh)

fit <- sdmTMB(densityKgKm2 ~ s(depth),
              data = data_CGFS_crs,
              family = tweedie(link = "log"),
              mesh = mesh, spatial = "on" )

fit_depth_year <- sdmTMB(data = data_CGFS_crs,
             formula = densityKgKm2 ~ 0 + as.factor(year) + as.factor(depth),
             mesh = mesh,
             family = tweedie(link = "log"),
             time = "year",
             spatiotemporal = "IID")
