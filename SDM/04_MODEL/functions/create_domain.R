
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

# # Extract only the English Channel polygon from the dataset
# Celtic_Sea <- SeaAreas[SeaAreas$NAME == "Celtic Sea", ] 
# 
# # Export the extracted English Channel shapefile for later use
# writeVector(Celtic_Sea, here("01_DATA/shapefiles/Celtic_Sea/Celtic_Sea.shp"))



# ------------------------------------------------------------------------------#
#### West ENGLISH CHANNEL EXTENSION #### 
# ------------------------------------------------------------------------------#
EEC_shp <- terra::vect(here("01_DATA", "shapefiles", "split_English_Channel", "east_English_Channel.shp"))
WEC_shp <- terra::vect(here("01_DATA", "shapefiles", "split_English_Channel", "west_English_Channel.shp"))
CS_shp <- terra::vect(here("01_DATA", "shapefiles", "Celtic_Sea", "Celtic_Sea.shp"))

# Keep only the Celtic Sea : lat in (48.5 ; 50) & lon < -5.9
cs_clip_ext <- terra::ext(-6, -4.138, 48.5, 50.03) %>% 
  terra::as.polygons(crs = crs(CS_shp))

CS_part <- terra::intersect(CS_shp, cs_clip_ext)

# Join West English Channel + clipped Celtic Sea part
mask_shp <- terra::union(WEC_shp, CS_part)

# remove internal barriers between english channel and celtic sea 
westEC_extension <- terra::aggregate(mask_shp)

# terra::writeVector(westEC_extension, 
#                    here("01_DATA/shapefiles/Western_English_Channel_extension/Western_English_Channel_extension.shp"))


# Join West English Channel extended + East EC
mask_shp <- terra::union(EEC_shp, westEC_extension)

# remove internal barriers 
EC_extension <- terra::aggregate(mask_shp)

# terra::writeVector(EC_extension,
#                    here("01_DATA/shapefiles/English_Channel_extension/English_Channel_extension.shp"))


# ------------------------------------------------------------------------------#
#### BAIE DE SEINE SHAPEFILE #### 
# ------------------------------------------------------------------------------#

# Define a geographic bounding box to delimit the eastern part of the Channel
baieSeine_ext <- terra::ext(-1.5, 0.5, 49.2, 50.1)

# Convert the extent into a polygon with the same CRS as the English Channel shapefile
BaieSeine_box <- terra::as.polygons(baieSeine_ext, crs = crs(English_Channel))

# Clip the English Channel using the polygon to keep only its eastern part
BaieSeine <- terra::intersect(English_Channel, BaieSeine_box)

# Plot the spatial result to visually confirm the extracted marine area
as.data.frame(BaieSeine)
plot(BaieSeine, col = "grey80", main = "Baie de Seine")

# Save
# terra::writeVector(BaieSeine, here("01_DATA/shapefiles/BaieDeSeine/BaieDeSeine.shp"))





# ------------------------------------------------------------------------------#
#### EASTERN ENGLISH CHANNEL SHAPEFILE #### 
# ------------------------------------------------------------------------------#

# Define a geographic bounding box to delimit the eastern part of the Channel
east_ext <- terra::ext(-1.4263, 1.930905, 49.2, 51.166)

# Convert the extent into a polygon with the same CRS as the English Channel shapefile
east_box <- terra::as.polygons(east_ext, crs = crs(English_Channel))

# Clip the English Channel using the polygon to keep only its eastern part
east_English_Channel <- terra::intersect(English_Channel, east_box)

# Plot the spatial result to visually confirm the extracted marine area
as.data.frame(east_English_Channel)
plot(east_English_Channel, col = "grey80", main = "Eastern_English_Channel")

# Save
# terra::writeVector(east_English_Channel, 
#                    here("01_DATA/shapefiles/split_English_Channel/east_English_Channel.shp"))


# ------------------------------------------------------------------------------#
#### WESTERN ENGLISH CHANNEL SHAPEFILE #### 
# ------------------------------------------------------------------------------#

# Define a geographic bounding box to delimit the western part of the Channel
west_ext <- terra::ext(-5.709, -1.4263, 48.4, 51.166)

# Convert the extent into a polygon with the same CRS as the English Channel shapefile
west_box <- terra::as.polygons(west_ext, crs = crs(English_Channel))

# Clip the English Channel using the polygon to keep only its western part
west_English_Channel <- terra::intersect(English_Channel, west_box)

# Plot the spatial result to visually confirm the extracted marine area
as.data.frame(west_English_Channel)
terra::plot(west_English_Channel, col = "grey80", main = "western_English_Channel")

# Save
# terra::writeVector(west_English_Channel, 
#                    here("01_DATA/shapefiles/split_English_Channel/west_English_Channel.shp"))


# ------------------------------------------------------------------------------#
#### EASTERN ENGLISH CHANNEL BATHYMETRY #### 
# ------------------------------------------------------------------------------#

# Load bathymetry raster from EMODnet 
bathy_emod <- terra::rast(here("01_DATA", "bathymetry", "E4_2024.nc", "E4_2024.nc"), 
                          sub = "elevation")

# Load Eastern English Channel shapefile as SpatVector
EEC <- terra::vect(here("01_DATA", "shapefiles", "Eastern_English_Channel", "Eastern_English_Channel.shp"))

# Reproject shapefile to match raster CRS 
EEC_shapefile <- terra::project(EEC, bathy_emod)

# Crop raster to polygon bounding box
bathy_EEC_crop <- terra::crop(bathy_emod, EEC_shapefile)

# Mask with the polygon 
bathy_EEC <- terra::mask(bathy_EEC_crop, EEC_shapefile)

# Convert depth in absolute value
bathy_EEC_depth <- terra::app(bathy_EEC, fun = abs)
names(bathy_EEC_depth) <- "depth"

# Plot
# plot(bathy_EEC_depth, main = "Bathymetry – Eastern English Channel")

# Save depth raster both as TIFF and NetCDF 
# terra::writeRaster(bathy_EEC_depth, 
#             here("01_DATA/bathymetry/EEC_Bathy/Bathy_Eastern_English_Channel.tif"), 
#             overwrite = TRUE)
# 
# terra::writeCDF(bathy_EEC_depth, 
#                 filename = here("01_DATA/bathymetry/EEC_Bathy/Bathy_Eastern_English_Channel.nc"),
#                 varname = "depth", unit = "m", overwrite=TRUE)






# ------------------------------------------------------------------------------#
#### ENTIRE ENGLISH CHANNEL BATHYMETRY #### 
# ------------------------------------------------------------------------------#


# Load bathymetry raster as SpatRaster
bathy_emod <- terra::rast(here("01_DATA", "bathymetry", "E4_2024.nc", "E4_2024.nc"), sub = "elevation")

# Load Eastern English Channel shapefile as SpatVector
EC <- terra::vect(here("01_DATA", "shapefiles", "English_Channel", "English_Channel.shp"))

# Reproject shapefile to match raster CRS 
EC_shapefile  <- terra::project(EC, bathy_emod)

# Crop to polygon extent 
bathy_EC_crop <- terra::crop(bathy_emod, EC_shapefile)

# Mask with the polygon 
bathy_EC <- terra::mask(bathy_EC_crop, EC_shapefile)

# depth in absolute value
bathy_EC_depth <- terra::app(bathy_EC, fun = abs)
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
substrate_csv <- readr::read_csv("01_DATA/substrate/Multiscale - folk 7.csv")

# Convert CSV table to an sf object (geometry stored in WKT format)
substrate_sf_7levels <- sf::st_as_sf(substrate_csv, wkt = "geom", crs = 4326) %>%
  dplyr::select(folk_7cl) 

# Convert sf object to spatial vector
substrate_vect_7levels <- terra::vect(substrate_sf_7levels)
# plot(substrate_vect_7levels)

# Load English Channel boundary as a SpatVector
EC <- terra::vect(here("01_DATA", "shapefiles", "English_Channel", "English_Channel.shp"))

# Reproject shapefile to match raster CRS 
EC_shapefile <- terra::project(EC, substrate_vect_7levels)

# Crop substrate polygons to the English Channel extent
substrate_EC_crop_7levels <- terra::crop(substrate_vect_7levels, EC_shapefile)

# Mask substrate polygons so only areas inside the English Channel remain
substrate_EC_7levels <- terra::mask(substrate_EC_crop_7levels, EC_shapefile)


# plot(substrate_EC_7levels, "folk_7cl", 
#      main = "Substrats - Folk 7 classes")

# Produce a substrate raster with the same extent, resolution, and CRS as the bathymetry
substrate_raster_7levels <- terra::rasterize(substrate_EC_7levels, 
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
substrate_csv <- readr::read_csv(here("01_DATA/substrate/Multiscale - folk 7.csv"))

# Convert CSV table to an sf object (geometry stored in WKT format)
substrate_sf_5levels <- sf::st_as_sf(substrate_csv, wkt = "geom", crs = 4326) %>%
  dplyr::select(folk_5cl) 

# Convert sf object to spatial vector
substrate_vect_5levels <- terra::vect(substrate_sf_5levels)
# plot(substrate_vect_5levels)

# Load English Channel boundary as a SpatVector
EC <- terra::vect(here("01_DATA", "shapefiles", "English_Channel", "English_Channel.shp"))

# Reproject shapefile to match raster CRS 
EC_shapefile <- terra::project(EC, bathy_emod)

# Crop substrate polygons to the English Channel extent
substrate_EC_crop_5levels <- terra::crop(substrate_vect_5levels, EC_shapefile)

# Mask substrate polygons so only areas inside the English Channel remain
substrate_EC_5levels <- terra::mask(substrate_EC_crop_5levels, EC_shapefile)


# plot(substrate_EC_5levels, "folk_5cl", 
#      main = "Substrats - Folk 5 classes")

# Produce a substrate raster with the same extent, resolution, and CRS as the bathymetry
substrate_raster_5levels <- terra::rasterize(substrate_EC_5levels, 
                                             bathy_EC_depth, 
                                             field = "folk_5cl")

# save
# terra::writeCDF(substrate_raster_5levels,
#                 filename = here("01_DATA/substrate/Substrate5levels_English_Channel.nc"),
#                 overwrite=TRUE)



