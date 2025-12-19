# ------------------------------------------------------------------------------
# Impute substrate class = 5 using iterative KNN majority (k = 8)
# ------------------------------------------------------------------------------
# Cells with substrate == 5 are imputed using the majority class among their
# k nearest neighbors (k = 8), excluding NA and class 5.
# Iterative approach: at each iteration, only impute cells that have at least
# min_valid_neighbors valid neighbors among the k nearest neighbors.
# This allows progressive filling of large patches of class 5.


impute_substrate5_focal_iter <- function(grid,
                                        k_neighbors = 8,
                                        min_valid_neighbors = 4,
                                        max_iter = 100) {
  
  # Basic checks
  stopifnot(all(c("lon", "lat", "substrate") %in% names(grid)))
  
  # If no substrate == 5, return unchanged
  if (!any(grid$substrate == 5, na.rm = TRUE)) return(grid)
  
  # Spatial coordinates (fixed)
  coordinates <- as.matrix(grid[, c("lon", "lat")])
  
  for (iter in seq_len(max_iter)) {
    
    idx_substrate_5 <- which(grid$substrate == 5)
    if (!length(idx_substrate_5)) break
    
    grid_before <- grid$substrate
    n_imputed_this_iter <- 0
    
    # Loop over each cell to be imputed
    for (i in idx_substrate_5) {
      
      # Squared Euclidean distances to all other grid cells
      distances_sq <- (coordinates[, 1] - coordinates[i, 1])^2 +
        (coordinates[, 2] - coordinates[i, 2])^2
      distances_sq[i] <- Inf  # exclude focal cell
      
      # Indices of the k nearest neighbors
      nearest_neighbors <- order(distances_sq)[
        seq_len(min(k_neighbors, nrow(grid) - 1))
      ]
      
      # Substrate values of neighboring cells
      neighbor_substrates <- grid$substrate[nearest_neighbors]
      
      # Keep only valid substrate classes (exclude NA and 5)
      valid_neighbors <- neighbor_substrates[
        !is.na(neighbor_substrates) & neighbor_substrates != 5
      ]
      
      # Require at least min_valid_neighbors to impute
      if (length(valid_neighbors) < min_valid_neighbors) next
      
      # Majority (modal) substrate class among valid neighbors
      majority_substrate <- as.numeric(
        names(which.max(table(valid_neighbors)))
      )
      
      # Replace substrate value
      grid$substrate[i] <- majority_substrate
      n_imputed_this_iter <- n_imputed_this_iter + 1
    }
    
    # Stop if no change occurred (nothing imputable with current rule)
    if (identical(grid$substrate, grid_before) || n_imputed_this_iter == 0) break
  }
  
  return(grid)
}
