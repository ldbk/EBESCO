
# ============================================================================== #
# DESIGN GRID AGGREGATE RASTER CELLS (UTM grid, clipped cells, area per cell)   #
# ============================================================================== #

# This script builds a regular raster grid with a 5km (res_km) resolution over the 
# Bay of Seine using a shapefile. 
# The resolution is temporarily converted from kilometers to degrees to create the raster in geographic 
# coordinates, and the grid is then clipped to keep only cells inside the polygon. 
# Finally, the coordinates of valid cell centers are extracted and converted to UTM coordinates

BaieSeine_shp <- vect("BaieDeSeine.shp")
res_km = 5


# Extract the spatial extent of the EC shapefile
BaieSeine_shp_ext <- ext(BaieSeine_shp)

# Convert the grid resolution to degrees only to build and mask the raster using the shapefile.
# Then convert back to kilometers because the model requires metric coordinates.
# Conversion from km to degrees at ~50°N latitude
res_deg_lat <- res_km / 111.32
res_deg_lon <- res_km / (111.32 * cos(50*pi/180))

# Create a regular raster grid with given resolution
grid_rast <- rast(extent = BaieSeine_shp_ext,
                  resolution = c(res_deg_lon, res_deg_lat),
                  crs = crs(BaieSeine_shp))

# Add dummy values to all cells so that mask() can work
grid_rast[] <- 1

# Keep only cells located inside the English Channel polygon
grid_rast_BdS <- mask(grid_rast, BaieSeine_shp)

# Extract the coordinates of valid cell centers (only inside EC)
cells_in_BdS <- which(!is.na(grid_rast_BdS[]))

# Get cell center coordinates
coords <- xyFromCell(grid_rast_BdS, cells_in_BdS) %>%
    as.data.frame() %>%
    dplyr::rename(lon = x, lat = y)

# Compute the prediction grid: extract lon/lat of valid cells and convert to UTM coordinates (km)
grid_pred <- add_utm_columns(coords,
                             ll_names = c("lon", "lat"),
                             ll_crs = 4326,
                             utm_names = c("X", "Y"),
                             utm_crs = 32630,
                             units = "km")


xmin_plot <- min(grid_pred$lon) - res_deg_lon
xmax_plot <- max(grid_pred$lon) + res_deg_lon
ymin_plot <- min(grid_pred$lat) - res_deg_lat
ymax_plot <- max(grid_pred$lat) + res_deg_lat

land <- ne_download(scale = "large", type = "land", category = "physical", returnclass = "sf") %>%
  st_transform(4326) %>%
  st_crop(xmin = xmin_plot, ymin = ymin_plot, xmax = xmax_plot, ymax = ymax_plot)


ggplot(grid_pred, aes(lon, lat)) +
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
  ggtitle(bquote("Grid projection (resolution of " *.(res_km)* " km)"))
