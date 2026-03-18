
# ------------------------------------------------------------------------------#
# Function to build and compare several customized spatial meshes
# depending on cutoff and max.edge parameters, with optional boundary
# constraint and/or land barrier.
# ------------------------------------------------------------------------------#
source(here::here("04_MODEL", "functions", "MSA_add_boundary_to_mesh.R"))
source(here::here("04_MODEL", "functions", "MSA_add_barrier_to_mesh.R"))

create_meshes_configurations <- function(region_name,
                                         cutoff_values,
                                         max_edge_in, 
                                         max_edge_out,
                                         add_barrier = FALSE,
                                         boundary = FALSE) {
  
  # ---------------------------------------------------------------------------
  # Select data, shapefile and parameters depending on the region
  # ---------------------------------------------------------------------------
  
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
  
  # ---------------------------------------------------------------------------#
  # Store results: one entry per cutoff
  # ---------------------------------------------------------------------------#
  configs <- vector("list", length(cutoff_values))
  names(configs) <- paste0("cutoff_", cutoff_values)
  
  
  # ---------------------------------------------------------------------------#
  # Loop over all mesh parameter configurations
  # ---------------------------------------------------------------------------#
 
  for (i in seq_along(cutoff_values)) {
    
    cutoff  <- cutoff_values[i]
    max_edge <- c(max_edge_in[i], max_edge_out[i])
    config_name <- paste0("cutoff_", cutoff)
    
    message(glue::glue(
      "{sp_scientific} | {region_name} | Testing cutoff = {cutoff} ; max.edge = ({max_edge[1]}, {max_edge[2]}) ; boundary = {boundary} ; barrier = {add_barrier}"
    ))
    
    config_result <- tryCatch({
      
      # Base mesh arguments
      mesh_arguments <- list(data = data_CGFS,
                             xy_cols = c("X", "Y"),
                             fmesher_func = fmesher::fm_mesh_2d_inla,
                             max.edge = max_edge,
                             cutoff = cutoff)
      
      # OPTION : BOUNDARY (internal function to add coastline boundary for inner mesh)
      # -------------------------------------------------------------------------#
      if (isTRUE(boundary)) {
        mesh_arguments$boundary <- add_boundary_to_mesh(
          EC_shp = EC_shp,
          simplify_keep = simplify_keep
        )
      }
      
      # Build mesh
      mesh0 <- do.call(sdmTMB::make_mesh, mesh_arguments)
      
      # OPTION : BARRIER  (internal function to add land barrier to a mesh)
      # -------------------------------------------------------------------------#
      if (isTRUE(add_barrier)) {
        barrier_output <- add_barrier_to_mesh(mesh0)
        mesh_used <- barrier_output$mesh         
        bspde_used <- barrier_output$bspde       
      } else {
        mesh_used <- mesh0
        bspde_used <- NULL
      }
      
      # -------------------------------------------------------------------------#
      # Fit spatial-temporal model with this mesh configuration
      # -------------------------------------------------------------------------#
      
      if ((sp_scientific == "Trachurus trachurus") && (region_name == "west")) {
        family_type = Gamma(link = "log")
      } else {
        family_type = tweedie(link = "log")
      }
      
      fit <- sdmTMB::sdmTMB(data = data_CGFS,
                            formula = densityKgKm2 ~ 1,
                            mesh = mesh_used,
                            family = family_type,
                            spatial = "on",
                            time = "year",
                            spatiotemporal = "IID")
      
      # Metrics
      n_vertices <- mesh_used$mesh$n
      cAIC_value <- sdmTMB::cAIC(fit)
      
      # -------------------------------------------------------------------------#
      # Fit spatial-temporal model with this mesh configuration
      # -------------------------------------------------------------------------#
      
      # Store everything for this cutoff
      list(cutoff = cutoff,      
           max_edge = max_edge,
           boundary = boundary,
           barrier = add_barrier,
           mesh = mesh_used,
           bspde = bspde_used,
           n_vertices = n_vertices,
           cAIC_value = cAIC_value,
           fit = fit, 
           region = region_name)
      
    }, error = function(e) {
      message(glue::glue("configuration failed: {e$message}"))
      NULL
    })
    
    configs[[config_name]] <- config_result
  }
  
  # If no fit worked, return NULL
  if (all(sapply(configs, is.null))) {
    return(NULL)
  }
  
  # Return results
  return(configs)
}
# ==============================================================================#
# WEST REGION 
# ==============================================================================#
# 
# meshes_west <- create_meshes_configurations(region_name = "west", 
#                                   add_barrier = FALSE,
#                                   boundary = FALSE)
# 
# meshes_barrier_west  <- create_meshes_configurations(region_name = "west", 
#                                            add_barrier = TRUE,
#                                            boundary = FALSE)
# 
# meshes_boundary_west  <- create_meshes_configurations(region_name = "west", 
#                                             add_barrier = FALSE,
#                                             boundary = TRUE)





# ==============================================================================#
# EAST REGION
# ==============================================================================#
# 
# meshes_east <- create_meshes_configurations(region_name = "east", 
#                                   add_barrier = FALSE,
#                                   boundary = FALSE)
# 
# meshes_barrier_east  <- create_meshes_configurations(region_name = "east", 
#                                            add_barrier = TRUE,
#                                            boundary = FALSE)
# 
# meshes_boundary_east  <- create_meshes_configurations(region_name = "east", 
#                                             add_barrier = FALSE,
#                                             boundary = TRUE)











