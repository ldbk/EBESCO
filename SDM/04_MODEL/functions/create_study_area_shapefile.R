
# ============================================================================== #
# GENERATE SPATIAL STUDY DOMAINS - ENGLISH CHANNEL
# ============================================================================== #
#
# Description:
#   Generate spatial study domains for the Eastern and Western English Channel
#   based on the Marine Regions (IHO) geographical boundaries.
#   As the Western Channel Ground Fish Survey (WCGFS) extends into the Celtic
#   Sea, the Western English Channel polygon is extended to encompass the
#   full survey area.
#
# Objectives:
#   1. Extract the English Channel and Celtic Sea polygons from Marine Regions.
#   2. Delineate the Eastern and Western English Channel spatial domains.
#   3. Extend the Western English Channel domain into the Celtic Sea to
#      include sampling locations covered by the WCGFS survey.
#
# Input:
#   - World Seas IHO v3 shapefile (Marine Regions).
#     Source: https://www.marineregions.org/downloads.php#iho
#
# Outputs:
#   - English_Channel.shp: Original English Channel boundary.
#   - east_English_Channel.shp: Eastern English Channel domain.
#   - west_English_Channel.shp: Western English Channel domain.
#   - Western_English_Channel_extension.shp: Western English Channel
#     extended into the Celtic Sea to cover the WCGFS survey area.
# ============================================================================== #


# ------------------------------------------------------------------------------ #
####  LOAD MARINE REGIONS DATA #### 
# ------------------------------------------------------------------------------ #

# Load the World Seas IHO v3 shapefile from Marine Regions
SeaAreas <- terra::vect(here("01_DATA/shapefiles/World_Seas_IHO_v3/World_Seas_IHO_v3.shp"))

# Inspect the sea area names to check availability
# unique(SeaAreas$NAME)  

# Extract only the English Channel polygon from the dataset
English_Channel <- SeaAreas[SeaAreas$NAME == "English Channel", ] 

# Export the extracted English Channel shapefile for later use
# writeVector(English_Channel, here("01_DATA/shapefiles/English_Channel/English_Channel.shp"))

# Extract only the English Channel polygon from the dataset
Celtic_Sea <- SeaAreas[SeaAreas$NAME == "Celtic Sea", ]

# # Export the extracted English Channel shapefile for later use
# writeVector(Celtic_Sea, here("01_DATA/shapefiles/Celtic_Sea/Celtic_Sea.shp"))



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
#### West ENGLISH CHANNEL EXTENSION #### 
# ------------------------------------------------------------------------------#
WEC_shp <- terra::vect(here("01_DATA/shapefiles/split_English_Channel/west_English_Channel.shp"))
CS_shp <- terra::vect(here("01_DATA/shapefiles/Celtic_Sea/Celtic_Sea.shp"))

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



