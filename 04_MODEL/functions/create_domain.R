
# ------------------------------------------------------------------------------#
# SCRIPT PURPOSE
# ------------------------------------------------------------------------------#

# Prepare spatial supports 
# Outputs generated:
#  - Shapefile: English Channel
#  - Shapefile: Eastern English Channel
#  - Raster (TIFF and NetCDF): Bathymetry for both English Channel and Eastern EC

# ------------------------------------------------------------------------------#




# ------------------------------------------------------------------------------#
####  LOAD MARINE REGIONS DATA #### 
# ------------------------------------------------------------------------------#

# Load the shapefile of world sea areas downloaded from 
# https://www.marineregions.org/downloads.php#iho
SeaAreas <- terra::vect(here("01_DATA", "shapefiles", "World_Seas_IHO_v3", "World_Seas_IHO_v3.shp"))

# Inspect the sea area names to check availability
# unique(SeaAreas$NAME)  

# Extract only the English Channel polygon from the dataset
English_Channel <- SeaAreas[SeaAreas$NAME == "English Channel", ] 

# Export the extracted English Channel shapefile for later use
# writeVector(English_Channel, here("01_DATA/shapefiles/English_Channel/English_Channel.shp"))





# ------------------------------------------------------------------------------#
#### EASTERN ENGLISH CHANNEL SHAPEFILE #### 
# ------------------------------------------------------------------------------#

# Define a geographic bounding box to delimit the eastern part of the Channel
east_ext <- ext(-1.5, 1.930905, 49.2, 51.166)

# Convert the extent into a polygon with the same CRS as the English Channel shapefile
east_box <- as.polygons(east_ext, crs = crs(English_Channel))

# Clip the English Channel using the polygon to keep only its eastern part
Eastern_English_Channel <- intersect(English_Channel, east_box)

# Plot the spatial result to visually confirm the extracted marine area
# as.data.frame(Eastern_English_Channel)
# plot(Eastern_English_Channel, col = "grey80", main = "Eastern_English_Channel")

# Save
# writeVector(Eastern_English_Channel, here("01_DATA/shapefiles/Eastern_English_Channel/Eastern_English_Channel.shp"))



# ------------------------------------------------------------------------------#
#### EASTERN ENGLISH CHANNEL BATHYMETRY #### 
# ------------------------------------------------------------------------------#

# Load bathymetry raster from EMODnet 
bathy_emod <- rast(here("01_DATA", "bathymetry", "E4_2024.nc", "E4_2024.nc"), sub = "elevation")

# Load Eastern English Channel shapefile as SpatVector
EEC <- vect(here("01_DATA", "shapefiles", "Eastern_English_Channel", "Eastern_English_Channel.shp"))

# Reproject shapefile to match raster CRS 
EEC_shapefile  <- project(EEC, bathy_emod)

# Crop raster to polygon bounding box
bathy_EEC_crop <- crop(bathy_emod, EEC_shapefile)

# Mask with the polygon 
bathy_EEC <- mask(bathy_EEC_crop, EEC_shapefile)

# Convert depth in absolute value
bathy_EEC_depth <- app(bathy_EEC, fun = abs)
names(bathy_EEC_depth) <- "depth"

# Plot
# plot(bathy_EEC_depth, main = "Bathymetry – Eastern English Channel")

# Save depth raster both as TIFF and NetCDF 
# writeRaster(bathy_EEC_depth, 
#             here("01_DATA/bathymetry/EEC_Bathy/Bathy_Eastern_English_Channel.tif"), 
#             overwrite = TRUE)
# 
# writeCDF(bathy_EEC_depth, 
#          filename = here("01_DATA/bathymetry/EEC_Bathy/Bathy_Eastern_English_Channel.nc"),
#          varname = "depth", unit = "m", overwrite=TRUE)






# ------------------------------------------------------------------------------#
#### ENTIRE ENGLISH CHANNEL BATHYMETRY #### 
# ------------------------------------------------------------------------------#


# Load bathymetry raster as SpatRaster
bathy_emod <- rast(here("01_DATA", "bathymetry", "E4_2024.nc", "E4_2024.nc"), sub = "elevation")

# Load Eastern English Channel shapefile as SpatVector
EC <- vect(here("01_DATA", "shapefiles", "English_Channel", "English_Channel.shp"))

# Reproject shapefile to match raster CRS 
EC_shapefile  <- project(EC, bathy_emod)

# Crop to polygon extent 
bathy_EC_crop <- crop(bathy_emod, EC_shapefile)

# Mask with the polygon 
bathy_EC <- mask(bathy_EC_crop, EC_shapefile)

# depth in absolute value
bathy_EC_depth <- app(bathy_EC, fun = abs)
names(bathy_EC_depth) <- "depth"

# # Plot
# plot(bathy_EC_depth, main = "Bathymetry – Eastern English Channel")

# # Save depth raster both as TIFF and NetCDF 
# writeRaster(bathy_EC_depth, 
#             here("01_DATA/bathymetry/EC_Bathy/Bathy_English_Channel.tif"), 
#             overwrite = TRUE)
# 
# writeCDF(bathy_EC_depth, 
#          filename = here("01_DATA/bathymetry/EC_Bathy/Bathy_English_Channel.nc"),
#          varname = "depth", unit = "m", overwrite=TRUE)



# ------------------------------------------------------------------------------#
#### ENTIRE ENGLISH CHANNEL SUBSTRATE (7 levels) #### 
# ------------------------------------------------------------------------------#


# Load substrate data from CSV file
substrate_csv <- read_csv("01_DATA/substrate/Multiscale - folk 7.csv")

# Convert CSV table to an sf object (geometry stored in WKT format)
substrate_sf_7levels <- st_as_sf(substrate_csv, wkt = "geom", crs = 4326) %>%
  dplyr::select(folk_7cl) 

# Convert sf object to spatial vector
substrate_vect_7levels <- vect(substrate_sf_7levels)
# plot(substrate_vect_7levels)

# Load English Channel boundary as a SpatVector
EC <- vect(here("01_DATA", "shapefiles", "English_Channel", "English_Channel.shp"))

# Reproject shapefile to match raster CRS 
EC_shapefile  <- project(EC, substrate_vect_7levels)

# Crop substrate polygons to the English Channel extent
substrate_EC_crop_7levels <- crop(substrate_vect_7levels, EC_shapefile)

# Mask substrate polygons so only areas inside the English Channel remain
substrate_EC_7levels <- mask(substrate_EC_crop_7levels, EC_shapefile)


# plot(substrate_EC_7levels, "folk_7cl", 
#      main = "Substrats - Folk 7 classes")

# Produce a substrate raster with the same extent, resolution, and CRS as the bathymetry
substrate_raster_7levels <- rasterize(substrate_EC_7levels, 
                                      bathy_EC_depth, 
                                      field = "folk_7cl")

# save
# writeCDF(substrate_raster_7levels,
#          filename = here("01_DATA/substrate/Substrate7levels_English_Channel.nc"),
#          overwrite=TRUE)


# ------------------------------------------------------------------------------#
#### ENTIRE ENGLISH CHANNEL SUBSTRATE (5 levels) #### 
# ------------------------------------------------------------------------------#


# Load substrate data from CSV file
substrate_csv <- read_csv(here("01_DATA/substrate/Multiscale - folk 7.csv"))

# Convert CSV table to an sf object (geometry stored in WKT format)
substrate_sf_5levels <- st_as_sf(substrate_csv, wkt = "geom", crs = 4326) %>%
  dplyr::select(folk_5cl) 

# Convert sf object to spatial vector
substrate_vect_5levels <- vect(substrate_sf_5levels)
# plot(substrate_vect_5levels)

# Load English Channel boundary as a SpatVector
EC <- vect(here("01_DATA", "shapefiles", "English_Channel", "English_Channel.shp"))

# Reproject shapefile to match raster CRS 
EC_shapefile  <- project(EC, bathy_emod)

# Crop substrate polygons to the English Channel extent
substrate_EC_crop_5levels <- crop(substrate_vect_5levels, EC_shapefile)

# Mask substrate polygons so only areas inside the English Channel remain
substrate_EC_5levels <- mask(substrate_EC_crop_5levels, EC_shapefile)


# plot(substrate_EC_5levels, "folk_5cl", 
#      main = "Substrats - Folk 5 classes")

# Produce a substrate raster with the same extent, resolution, and CRS as the bathymetry
substrate_raster_5levels <- rasterize(substrate_EC_5levels, 
                                      bathy_EC_depth, 
                                      field = "folk_5cl")

# save
# writeCDF(substrate_raster_5levels,
#          filename = here("01_DATA/substrate/Substrate5levels_English_Channel.nc"),
#          overwrite=TRUE)



