# ------------------------------------------------------------------------------
# Impute substrate class = 5 using the majority class of nearby grid cells
# ------------------------------------------------------------------------------

# Substrate class 5, for which no observations is available, are imputed on the 
# prediction grid using a k-nearest-neighbor majority rule (k = 8), based on 
# surrounding grid cells.

impute_substrate5 <- function(grid, n_neighbors = 8) {
  
  # Basic checks
  stopifnot(all(c("lon", "lat", "substrate") %in% names(grid)))
  
  # Indices of grid cells where substrate == 5
  idx_substrate_5 <- which(grid$substrate == 5)
  
  # If no substrate == 5, return grid unchanged
  if (!length(idx_substrate_5)) return(grid)
  
  # Extract spatial coordinates
  coordinates <- as.matrix(grid[, c("lon", "lat")])
  
  # Loop over each cell to be imputed
  for (i in idx_substrate_5) {
    
    # Squared Euclidean distances to all other grid cells
    distances_sq <- (coordinates[, 1] - coordinates[i, 1])^2 +
      (coordinates[, 2] - coordinates[i, 2])^2
    distances_sq[i] <- Inf  # exclude the focal cell
    
    # Indices of the n nearest neighbors
    nearest_neighbors <- order(distances_sq)[
      seq_len(min(n_neighbors, nrow(grid) - 1))
    ]
    
    # Substrate values of neighboring cells
    neighbor_substrates <- grid$substrate[nearest_neighbors]
    
    # Keep only valid substrate classes (exclude NA and 5)
    neighbor_substrates <- neighbor_substrates[
      !is.na(neighbor_substrates) & neighbor_substrates != 5
    ]
    
    # If no valid neighbors are available, skip imputation
    if (!length(neighbor_substrates)) next
    
    # Majority (modal) substrate class among neighbors
    majority_substrate <- as.numeric(
      names(which.max(table(neighbor_substrates)))
    )
    
    # Replace substrate value
    grid$substrate[i] <- majority_substrate
  }
  
  return(grid)
}
