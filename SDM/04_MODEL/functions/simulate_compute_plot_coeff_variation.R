# ==============================================================================#
# Simulations + Coffecient Variation maps for all valid models 
# ==============================================================================#

# Load sanity by region object
valid_models_predict <- readRDS(here("05_OUTPUTS", "model_diagnostics", 
                                     paste0("model_diagnostics_", sp_safe),                          
                                     "sanity_by_region.rds"))

# Example access : 
# valid_models_predict <- readRDS("~/EBESCO/SDM/05_OUTPUTS/model_diagnostics/model_diagnostics_Zeus_faber/sanity_by_region.rds")

# ------------------------------------------------------------------------------#
# Function used to extract only valid models from the diagnostics object
# ------------------------------------------------------------------------------#

get_valid_models <- function(valid_models_predict) {
  # Initialize an empty list with one element per region
  res <- setNames(vector("list", length(valid_models_predict)),
                  names(valid_models_predict))
  
  # Loop over regions
  for (region in names(valid_models_predict)) {
    region_list <- valid_models_predict[[region]]
    
    if (is.null(region_list)) next    # Skip empty or NULL regions
    
    # Loop over responses within a region & extract valid models for this response
    for (response in names(region_list)) {
      models_valid <- region_list[[response]]$models_valid
      
      # Store only non-empty valid model lists
      if (!is.null(models_valid)) {
        res[[region]][[response]] <- models_valid
      }
    }
  }
  # Remove regions that ended up empty
  res <- res[vapply(res, length, integer(1)) > 0]
  return(res)
}

valid_models_only <- get_valid_models(valid_models_predict)

# ------------------------------------------------------------------------------#
# Prediction grids by region
# ------------------------------------------------------------------------------#
grids_by_region <- list(
  east = grid_by_region$east$grid_pred,
  west = grid_by_region$west$grid_pred
)

# ------------------------------------------------------------------------------#
# run nsim predictions on response scale and compute CV
# ------------------------------------------------------------------------------#

simulate_cv <- function(model, grid) {
  
  set.seed(123)
  
  simulations <- predict(model, 
                         newdata = grid, 
                         type = "response", 
                         model = NA, 
                         nsim = 100)
  

  grid$cv <- apply(simulations, 1, function(x) sd(x) / mean(x))

  return(grid)
}

# ------------------------------------------------------------------------------#
# Simulations for all valid models
# ------------------------------------------------------------------------------#
sim_cv_all <- list()

for (region in names(valid_models_only)) {
  
  grid_region <- grids_by_region[[region]]

  for (response in names(valid_models_only[[region]])) {
    
    for (model_name in names(valid_models_only[[region]][[response]])) {
      
      mod <- valid_models_only[[region]][[response]][[model_name]]
      
      pred_cv <- simulate_cv(mod, grid_region)
      
      sim_cv_all[[region]][[response]][[model_name]] <- pred_cv
    }
  }
}

# ------------------------------------------------------------------------------#
# SAVE simulation outputs (data)
# ------------------------------------------------------------------------------#
out_rds_sim <- here(
  "05_OUTPUTS", "model_diagnostics",
  paste0("model_diagnostics_", sp_safe),
  paste0("simulations_cv_", sp_safe, ".rds")
)

dir.create(dirname(out_rds_sim), showWarnings = FALSE, recursive = TRUE)
saveRDS(sim_cv_all, out_rds_sim)


# ------------------------------------------------------------------------------#
# Build land polygons + bounds per region 
# ------------------------------------------------------------------------------#
get_land_region <- function(grid_df) {
  
  # Convert resolution from km to degrees
  res_deg_lat <- res_km / 111.32
  res_deg_lon <- res_km / (111.32 * cos(50*pi/180))
  
  # Define plot limits with a one-cell buffer
  xmin_plot <- range(grid_df$lon)[1] - res_deg_lon
  xmax_plot <- range(grid_df$lon)[2] + res_deg_lon
  ymin_plot <- range(grid_df$lat)[1] - res_deg_lat
  ymax_plot <- range(grid_df$lat)[2] + res_deg_lat
  
  land <- ne_download(scale = "large", type = "land", category = "physical", returnclass = "sf") %>%
    st_transform(4326) %>%
    st_crop(xmin = xmin_plot, ymin = ymin_plot, xmax = xmax_plot, ymax = ymax_plot)
  
  list(land = land,
       xlim = c(xmin_plot, xmax_plot),
       ylim = c(ymin_plot, ymax_plot))
  
}

# Build land objects once per region 
land_by_region <- list()
for (region in names(sim_cv_all)) {
  first_resp <- names(sim_cv_all[[region]])[1]
  first_mod <- names(sim_cv_all[[region]][[first_resp]])[1]
  grid_df0 <- sim_cv_all[[region]][[first_resp]][[first_mod]]
  land_by_region[[region]] <- get_land_region(grid_df0)
}

# ------------------------------------------------------------------------------#
# Plot CV map 
# ------------------------------------------------------------------------------#
plot_cv_map <- function(data, land_obj, subtitle) {
  
  ggplot(data, aes(lon, lat, fill = cv)) +
    geom_raster() +
    geom_sf(data = land_obj$land, fill = "grey80", inherit.aes = FALSE, color = "grey80") +
    scale_y_continuous(breaks = pretty_breaks(n = 3)) +
    scale_x_continuous(breaks = pretty_breaks(n = 4)) +
    coord_sf(xlim = land_obj$xlim, ylim = land_obj$ylim, expand = FALSE) +
    facet_wrap(~year, nrow = 2) +
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = "white", colour = NA),
          panel.border = element_rect(color = "white", fill = NA, linewidth = 2),
          strip.background = element_rect(fill = "grey40", color = "white"),
          strip.text = element_text(color = "white", face = "bold")) +
    labs(x = "", y = "", subtitle = subtitle, fill = "CV") +
    scale_fill_viridis_c(
      trans = "log10",
      labels = label_number(drop0trailing = TRUE)
    )
}

# ------------------------------------------------------------------------------#
# Create CV plots for every region/response/model
# ------------------------------------------------------------------------------#
plots_cv_all <- list()

for (region in names(sim_cv_all)) {
  
  land_obj <- land_by_region[[region]]
  
  for (response in names(sim_cv_all[[region]])) {
    
    for (model_name in names(sim_cv_all[[region]][[response]])) {
      
      df <- as.data.frame(sim_cv_all[[region]][[response]][[model_name]])
      
      plots_cv_all[[region]][[response]][[model_name]] <- plot_cv_map(
        data = df,
        land_obj = land_obj,
        subtitle = paste(region, response, model_name, "CV", sep = " - ")
      )
    }
  }
}

# ------------------------------------------------------------------------------#
# Save plots
# ------------------------------------------------------------------------------#
out_rds_plots <- here(
  "05_OUTPUTS", "model_diagnostics",
  paste0("model_diagnostics_", sp_safe),
  paste0("plots_cv_", sp_safe, ".rds")
)

dir.create(dirname(out_rds_plots), showWarnings = FALSE, recursive = TRUE)
saveRDS(plots_cv_all, out_rds_plots)

# ------------------------------------------------------------------------------
# Example access:
# plots_cv_all$east$totalWeightKg$deltagammapoissonlink
# plots_cv_all$west$totalWeightKg$lognormal
# ------------------------------------------------------------------------------



# ln <- as.data.frame(sim_cv_all$west$totalWeightKg$lognormal)
# ln <- ln %>% rename(cv_ln = cv)
# gm <- as.data.frame(sim_cv_all$west$totalWeightKg$gamma)
# gm <- gm %>% rename(cv_gm = cv)
# 
# gm_ln <- ln %>% left_join(gm, by = join_by(lon, lat, X, Y, year))
# 
# gm_ln <- gm_ln %>% mutate(delta_cv = cv_ln - cv_gm)
# 
# ggplot(gm_ln, aes(lon, lat, fill = delta_cv)) +
#   geom_raster() +
#   geom_sf(data = land_obj$land, fill = "grey80", inherit.aes = FALSE, color = "grey80") +
#   scale_y_continuous(breaks = pretty_breaks(n = 3)) +
#   scale_x_continuous(breaks = pretty_breaks(n = 4)) +
#   coord_sf(xlim = land_obj$xlim, ylim = land_obj$ylim, expand = FALSE) +
#   facet_wrap(~year, nrow = 2) +
#   theme(panel.grid = element_blank(),
#         panel.background = element_rect(fill = "white", colour = NA),
#         panel.border = element_rect(color = "white", fill = NA, linewidth = 2),
#         strip.background = element_rect(fill = "grey40", color = "white"),
#         strip.text = element_text(color = "white", face = "bold")) +
#   scale_fill_gradient2()




