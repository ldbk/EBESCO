
# ============================================================================== #
# MAPPING ANNUAL BIOMASS PREDICTIONS FROM SELECTED MODELS 
# ============================================================================== #
#
# This script generates annual species biomass prediction maps from selected
# SDM models for the eastern and western study regions.
#
# For each species and region, the selected model is specified by its mesh
# cutoff, response variable and distribution family. The script automatically
# loads the latest saved predictions and extracts those corresponding to
# the selected models.
#
# Outputs:
#   - combined_maps$east: maps from selected models in the eastern region
#   - combined_maps$west: maps from selected models in the western region
#
# ============================================================================== #


# ------------------------------------------------------------------------------ #
#### 1. SELECT MODELS AND SET PARAMETERS ####
# ------------------------------------------------------------------------------ #

# Selected mesh cutoff and distribution family for each species and region
chosen_models <- list(
  east = list(
    Chelidonichthys_cuculus = c("cutoff_10", "totalWeightKg", "deltalognormal"),
    Chelidonichthys_lucerna = c("cutoff_15", "totalWeightKg", "deltalognormalpoissonlink"),
    Conger_conger = c("cutoff_15", "totalWeightKg", "deltalognormal"),
    Dicentrarchus_labrax = c("cutoff_15", "totalWeightKg", "tweedie"),
    Hippocampus_hippocampus = c("cutoff_15", "totalWeightKg", "deltalognormalpoissonlink"),
    Solea_solea = c("cutoff_15", "totalWeightKg", "deltalognormalpoissonlink"),
    Trachurus_trachurus = c("cutoff_15", "totalWeightKg", "deltalognormalpoissonlink"),
    Zeus_faber = c("cutoff_10", "totalWeightKg", "deltagammapoissonlink")
  ),
  west = list(
    Capros_aper = c("cutoff_10", "totalWeightKg", "deltalognormal"),
    Chelidonichthys_cuculus = c("cutoff_7", "totalWeightKg", "tweedie"),
    Conger_conger = c("cutoff_7", "totalWeightKg", "deltagamma"),
    Dicentrarchus_labrax = c("cutoff_15", "totalWeightKg", "tweedie"),
    Engraulis_encrasicolus = c("cutoff_10", "totalWeightKg", "deltalognormalpoissonlink"),
    Micromesistius_poutassou = c("cutoff_15", "totalWeightKg", "deltagamma"),
    Trachurus_trachurus = c("cutoff_15", "totalWeightKg", "lognormal"),
    Zeus_faber = c("cutoff_3", "totalWeightKg", "tweedie")
  )
)

output_dir <- here::here("05_OUTPUTS")
margin_m <- 5000  # Map margin in meters


# ------------------------------------------------------------------------------ #
#### 2. LOAD LATEST PREDICTIONS AND ASSOCIATED GRIDS ####
# ------------------------------------------------------------------------------ #

predictions <- list()
grids <- list()

# Identify all species selected in at least one region
species <- unique(unlist(lapply(chosen_models, names), use.names = FALSE))

for (sp in species) {
  
  species_dir <- file.path(output_dir, sp)
  
  if (!dir.exists(species_dir)) next
  
  # Find saved model predictions
  prediction_files <- list.files(
    species_dir,
    pattern = paste0("^", sp, "_model_predictions\\.rds$"),
    recursive = TRUE,
    full.names = TRUE
  )
  
  if (!length(prediction_files)) {
    message("No predictions: ", sp)
    next
  }
  
  # Select the most recently modified prediction file
  latest_file <- prediction_files[which.max(file.info(prediction_files)$mtime)]
  
  # Retrieve the prediction grid from the same modelling run
  grid_file <- file.path(dirname(latest_file),
                         paste0(sp, "_prediction_grids.rds"))
  
  if (!file.exists(grid_file)) {
    message("No prediction grid: ", sp)
    next
  }
  
  predictions[[sp]] <- readRDS(latest_file)
  grids[[sp]] <- readRDS(grid_file)
  
  message("Loaded: ", sp)
}


# ------------------------------------------------------------------------------ #
#### 3. PREPARE PREDICTIONS AND SPATIAL EXTENTS ####
# ------------------------------------------------------------------------------ #

# Transform land polygons once to the prediction CRS
land_utm <- sf::st_transform(land_ne, utm_crs_used)

map_data <- lapply(names(chosen_models), function(region) {
  
  region_data <- list()
  
  for (sp in names(chosen_models[[region]])) {
    
    if (is.null(predictions[[sp]]) || is.null(grids[[sp]])) next
    
    # Retrieve the selected mesh and distribution family
    model <- chosen_models[[region]][[sp]]
    
    # Extract predictions using the selected model parameters:
    # model[1] = mesh cutoff
    # model[2] = response variable
    # model[3] = distribution family
    pred <- predictions[[sp]][[region]][[model[1]]][[model[2]]][[model[3]]]$predictions
    
    grid_pred <- grids[[sp]][[region]]$grid_pred
    
    if (!is.data.frame(pred) || !nrow(pred) || is.null(grid_pred)) {
      message("Model or grid unavailable: ", region, " - ", sp)
      next
    }
    
    # Generate abbreviated scientific name for facet labels
    parts <- strsplit(sp, "_", fixed = TRUE)[[1]]
    
    pred$species_names <- sprintf("italic('%s. %s')",
                                  substr(parts[1], 1, 1),
                                  parts[2])
    
    # Define map extent with a fixed margin
    xlim <- range(grid_pred$X_map_m, na.rm = TRUE) + c(-margin_m, margin_m)
    ylim <- range(grid_pred$Y_map_m, na.rm = TRUE) + c(-margin_m, margin_m)
    
    # Crop land polygons to the map extent
    land <- suppressWarnings(sf::st_crop(land_utm, 
                                         xmin = xlim[1],
                                         xmax = xlim[2],
                                         ymin = ylim[1],
                                         ymax = ylim[2]))
    
    region_data[[sp]] <- list(pred = pred,
                              land = land,
                              xlim = xlim,
                              ylim = ylim)
  }
  
  region_data
  
})

names(map_data) <- names(chosen_models)


# ------------------------------------------------------------------------------ #
#### 4. DEFINE MAPPING FUNCTION ####
# ------------------------------------------------------------------------------ #

# Format legend labels without scientific notation
format_legend <- function(x) {
  format(x, scientific = FALSE, trim = TRUE)
}

plot_maps <- function(data, show_x = TRUE, show_strip = TRUE) {
  
  p <- ggplot2::ggplot(data$pred, aes(X_map_m, Y_map_m, fill = est)) +
    ggplot2::geom_raster() +
    ggplot2::geom_sf(data = data$land,
                     fill = "grey80",
                     color = "grey80",
                     inherit.aes = FALSE) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
    ggplot2::coord_sf(crs = 32630,
                      xlim = data$xlim,
                      ylim = data$ylim,
                      expand = FALSE)+
    ggplot2::facet_grid(species_names ~ year,
                        labeller = ggplot2::label_parsed) +
    ggplot2::scale_fill_distiller(trans = "log10",
                                  palette = "RdYlBu",
                                  labels = format_legend,
                                  guide = guide_colourbar(direction = "horizontal",
                                                          barwidth = grid::unit(5, "cm"))) +
    ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
    ggplot2::theme(panel.grid = element_blank(),
                   panel.background = element_rect(fill = "white", colour = NA),
                   panel.border = element_rect(color = "white", fill = NA, linewidth = 2),
                   strip.background = element_rect(fill = "grey40", color = "white"),
                   strip.text = element_text(color = "white"),
                   strip.text.x = element_text(size = 8),
                   strip.text.y = element_text(size = 7),
                   axis.text = element_text(size = 7),
                   legend.text = element_text(size = 8),
                   panel.spacing = unit(0, "lines"),
                   plot.margin = margin(-5, 0, -5, 0),
                   legend.margin = margin(0, 0, 0, 0),
                   legend.box.margin = margin(0, 0, 0, 0))
  
  # Display x-axis labels only for the last species
  if (!show_x) {
    p <- p + ggplot2::theme(axis.text.x = element_blank(),
                            axis.ticks.x = element_blank())
  }
  
  # Display year labels only for the first species
  if (!show_strip) {
    p <- p + ggplot2::theme(strip.text.x = element_blank(),
                            strip.background.x = element_blank())
  }
  
  p
}


# ------------------------------------------------------------------------------ #
#### 5. GENERATE AND COMBINE MAPS BY REGION ####
# ------------------------------------------------------------------------------ #

combined_maps <- lapply(map_data, function(region_data) {
  
  # Count the number of species available in this region
  n_species <- length(region_data)
  
  # Skip regions without available species
  if (n_species == 0) return(NULL)
  
  # Generate one map per species
  species_maps <- lapply(seq_len(n_species), function(species_index) {
    
    plot_maps(data = region_data[[species_index]],
              show_x = species_index == n_species,  # Show x-axis only on the bottom map
              show_strip = species_index == 1)      # Show year labels only on the top map
  })
  
  # Stack species maps vertically
  patchwork::wrap_plots(species_maps, ncol = 1) &
    ggplot2::theme(legend.position = "right",
                   plot.margin = ggplot2::margin(0, 2, 0, 0))
})


# ------------------------------------------------------------------------------ #
#### 6. DISPLAY MAPS ####
# ------------------------------------------------------------------------------ #

combined_maps$west
combined_maps$east
