
# ============================================================================== #
####  CREATE THE SPATIAL MESH FOR THE SPDE APPROACH ####
# ============================================================================== #


# Function that builds a mesh from CGFS data
create_mesh_by_region <- function(region_name = "region", sp_name_safe) {
  
  
  if (region_name == "west") {
    data_CGFS <- data_CGFS_west
    EC_shp <- vect(here("01_DATA", "shapefiles", "Western_English_Channel_extension", 
                        "Western_English_Channel_extension.shp"))
    simplify_keep = 0.0025
  } else if (region_name == "east") {
    data_CGFS <- data_CGFS_east
    EC_shp <- vect(here("01_DATA", "shapefiles", "split_English_Channel", 
                        "east_English_Channel.shp"))
    simplify_keep = 0.005
  }
  

  mesh_params <- readxl::read_excel(here::here("01_DATA", "mesh_params_per_species_region.xlsx"))%>%
    filter(species == sp_name_safe, region == region_name)
  
  cutoff = mesh_params$cutoff
  max_edge = c(mesh_params$max_edge, 100)

  
  mesh <- make_mesh(data_CGFS, c("X", "Y"),
                    fmesher_func = fmesher::fm_mesh_2d_inla,
                    max.edge = max_edge,
                    cutoff = cutoff,
                    boundary  = add_boundary_to_mesh(EC_shp = EC_shp, simplify_keep = simplify_keep))



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
land_mesh <- land_ne %>%
  st_transform(utm_crs_used) %>%
  st_crop(mesh_m_bbox)

# Plot the mesh, land areas, and triangle centers
plot_mesh <- ggplot() +
  geom_sf(data = land_mesh, fill = "grey80", color = NA) +
  gg(mesh_m, edge.color = "grey50", edge.linewidth = 0.2) +
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





