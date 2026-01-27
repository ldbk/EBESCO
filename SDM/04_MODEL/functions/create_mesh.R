
# ============================================================================== #
####  CREATE THE SPATIAL MESH FOR THE SPDE APPROACH ####
# ============================================================================== #


# Function that builds a mesh from CGFS data
run_mesh_spde <- function(data_CGFS, region_name = "region") {


# ------------------------------------------------------------------------------#
#### 1. Compute shortest mean distance ####
# ------------------------------------------------------------------------------#

# Compute the nearest neighbor euclidean distance (km) for all points each year
shortest_distance_matrix <- data_CGFS %>%
  dplyr::select(year, X, Y) %>%
  group_by(year) %>%
  mutate(
    nearest_neighbor_dist_km = {
      distance_matrix <- as.matrix(stats::dist(cbind(X, Y), method = "euclidean"))
      diag(distance_matrix) <- NA                     # remove self-distance
      apply(distance_matrix, 1, min, na.rm = TRUE)    # keep minimum distance for each row/point
    }
  ) %>%
  ungroup()

# Cutoff defines the minimum allowed distance between mesh vertices 
cutoff = mean(shortest_distance_matrix$nearest_neighbor_dist_km)

# east : nearest_neighbor_dist_km : Mean : 12.974 
# west : nearest_neighbor_dist_km : Mean : 22.435


# ------------------------------------------------------------------------------#
#### 2. Build the standard mesh without barriers ####
# ------------------------------------------------------------------------------#

# Create a mesh object that contains matrices to apply the SPDE approach
# Cutoff defines the minimum allowed distance between mesh vertices in the units of X and Y
mesh <- make_mesh(data_CGFS, c("X", "Y"), cutoff = cutoff)
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
  geom_sf(data = mesh_df_water, size = 1, colour = "blue") +
  geom_sf(data = mesh_df_land,  size = 1, colour = "red") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 4))+
  coord_sf(crs = st_crs(land_mesh)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(colour = "black"))+
  labs(x="", y="", title = paste0("Maillage triangulaire avec cutoff de ", cutoff, "km"),
       subtitle = "rouge = centre des triangles sur la terre\nbleu = centre des triangles en mer")

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

mesh_by_region <- list()

# If East_English_Channel is TRUE, build the east mesh
if (isTRUE(East_English_Channel)) {
  mesh_by_region$east <- run_mesh_spde(data_CGFS = data_CGFS_east, region_name = "east")
}

# If West_English_Channel is TRUE, build the west mesh
if (isTRUE(West_English_Channel)) {
  mesh_by_region$west <- run_mesh_spde(data_CGFS = data_CGFS_west, region_name = "west")
}

