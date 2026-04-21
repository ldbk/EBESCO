
# ============================================================================== #
####  CREATE THE SPATIAL MESH FOR THE SPDE APPROACH ####
# ============================================================================== #


# Function that builds a mesh from CGFS data
create_mesh_by_region <- function(region_name = "region") {
  
  if (region_name == "west") {
    data_CGFS <- data_CGFS_west
  } else if (region_name == "east") {
    data_CGFS <- data_CGFS_east
  }
  
# # ------------------------------------------------------------------------------#
# #### 1. Compute shortest mean distance ####
# # ------------------------------------------------------------------------------#
# 
# # Compute the nearest neighbor euclidean distance (km) for all points each year
# shortest_distance_matrix <- data_CGFS_east %>%
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
# # Cutoff defines the minimum allowed distance between mesh vertices
# cutoff = mean(shortest_distance_matrix$nearest_neighbor_dist_km)
# #
# # # east : nearest_neighbor_dist_km : Mean : 12.974
# # # west : nearest_neighbor_dist_km : Mean : 22.435
# 
# min_dist <- shortest_distance_matrix %>%
#   group_by(year)%>%
#   summarise(min_dist = min(nearest_neighbor_dist_km))
# 
# mean(min_dist$min_dist)
# 
# max_dist <- shortest_distance_matrix %>%
#   group_by(year)%>%
#   summarise(max_dist = max(nearest_neighbor_dist_km))
# 
# mean(max_dist$max_dist)

# ------------------------------------------------------------------------------#
#### 2. Build the standard mesh without barriers ####
# ------------------------------------------------------------------------------#

# Create a mesh object that contains matrices to apply the SPDE approach
# Cutoff defines the minimum allowed distance between mesh vertices in the units of X and Y
# mesh <- make_mesh(data_CGFS, c("X", "Y"), cutoff = cutoff)
# plot(mesh)

  # Mesh parameters depend on the region size
  if (region_name == "west") {
    # West English Channel (~305 × 219 km)
    range0 <- 100                     # initial spatial range ≈ 1/3 of study area
    max_edge <- c(25, 100)            # # max triangle edge length; inner and outer meshes ; < spatial range/5 inside, 5 times max_edge outside
    cutoff <- 5                    # avoid very small triangles --> cutoff = max_edge/5

  } else if (region_name == "east") {
    # East English Channel (~210 × 183 km)
    range0 <- 73                    # initial spatial range ≈ 1/3 of study area
    max_edge <- c(50, 100)            # < spatial range/5 inside, , 5 times max_edge outside
    cutoff <- 10                      # avoid very small triangles --> cutoff = max_edge/5
  }
  
  mesh <- make_mesh(data_CGFS, c("X", "Y"),
                    fmesher_func = fmesher::fm_mesh_2d_inla,
                    max.edge = max_edge,
                       # - use 5 times max.edge in the outer extension/offset/boundary
                    cutoff = cutoff)
  # plot(mesh)
# ------------------------------------------------------------------------------#
#### 3. Add the land barrier to the mesh ####
# ------------------------------------------------------------------------------#

# Extract mesh vertex coordinates (in km) and convert them to meters
mesh_coords <- as.data.frame(mesh$mesh$loc)%>%
  rename(X_km = V1, Y_km = V2)%>%
  mutate(X_m = X_km * 1000,
         Y_m = Y_km * 1000)

# Convert mesh coordinates to an sf object in UTM (meters) to compute its spatial extent
mesh_sf <- st_as_sf(mesh_coords, coords = c("X_m", "Y_m"), crs = utm_crs_used)

# Get the mesh bounding box (in meters)
mesh_bbox <- st_bbox(mesh_sf)

# Load country polygons, reproject to UTM, and crop to the mesh extent
land <- ne_countries(scale = "large",
                     country = c("United Kingdom", "France"),
                     returnclass = "sf")%>%
  st_transform(utm_crs_used) %>%
  st_crop(mesh_bbox) 

# Add on the barrier mesh component
# Build the barrier mesh (mesh in km, barrier in meters --> handled via proj_scaling)
bspde <- add_barrier_mesh(spde_obj = mesh,
                          barrier_sf = land,
                          range_fraction = 0.1,
                          proj_scaling   = 1000)#,  
                          # plot = TRUE)


# ------------------------------------------------------------------------------#
#### 4. Plot the mesh  with land areas and triangle centers ####
# ------------------------------------------------------------------------------#
# Compute the mesh bounding box in meters because the mesh extends beyond the spatial domain, 
# and we need its full extent to correctly crop land polygons [aesthetic reasons]

# Copy the mesh object
mesh_m <- mesh$mesh

# Convert mesh coordinates from kilometers to meters
mesh_m$loc <- mesh_m$loc * 1000

# Compute the mesh bounding box in meters
mesh_m_bbox <- st_bbox(c(xmin = min(mesh_m$loc[, 1]),
                         xmax = max(mesh_m$loc[, 1]),
                         ymin = min(mesh_m$loc[, 2]),
                         ymax = max(mesh_m$loc[, 2])), crs = utm_crs_used)

# Build "land_mesh" containing coastlines cropped to the mesh extent
land_mesh <- ne_countries(scale = "large",
                          country = c("United Kingdom", "France"),
                          returnclass = "sf") %>%
  st_transform(utm_crs_used) %>%
  st_crop(mesh_m_bbox)

# Separate water triangles and land (barrier) triangles
mesh_df_water <- bspde$mesh_sf[bspde$normal_triangles, ]
mesh_df_land  <- bspde$mesh_sf[bspde$barrier_triangles, ]

# Plot the mesh, land areas, and triangle centers
plot_mesh <- ggplot() +
  geom_sf(data = land_mesh, fill = "grey80", color = NA) +
  gg(mesh_m, edge.color = "grey50", edge.linewidth = 0.2) +
  geom_sf(data = mesh_df_water, size = 1.5, colour = "darkblue", alpha = 0.5) +
  geom_sf(data = mesh_df_land,  size = 1.5, colour = "red", alpha = 0.5) +
  geom_spatial_point(data = data_CGFS, aes(lon, lat), color = "black", size = 1.5, alpha = 0.5)+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 4))+
  coord_sf(crs = st_crs(land_mesh)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(colour = "black"))+
  labs(x="", y="", title = paste0("Mesh with a cutoff of ", round(cutoff,1), "km"),
       subtitle = "red = centers of triangles on land\nblue = centers of triangles at sea")


# Return everything useful 
return(list(
  region = region_name,
  cutoff = cutoff,
  mesh = mesh,
  bspde = bspde,
  plot_mesh = plot_mesh
))

}


# ------------------------------------------------------------------------------#
#### 5. Build meshes by region depending on user-defined flags ####
# ------------------------------------------------------------------------------#
# 
# mesh_by_region <- list()
# 
# If East_English_Channel is TRUE, build the east mesh
# if (isTRUE(East_English_Channel)) {
#   mesh_by_region$east <- create_mesh_by_region(region_name = "east")
# }
# 
# # If West_English_Channel is TRUE, build the west mesh
# if (isTRUE(West_English_Channel)) {
#   mesh_by_region$west <- create_mesh_by_region(region_name = "west")
# }
# 





