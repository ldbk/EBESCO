
valid_models_predict <- readRDS(here("05_OUTPUTS", "model_diagnostics", 
                                     paste0("model_diagnostics_", sp_safe),                          
                                     "sanity_by_region.rds"))




get_valid_models <- function(valid_models_predict) {
  # pré-initialise toutes les régions avec des listes vides
  res <- setNames(vector("list", length(valid_models_predict)),
                  names(valid_models_predict))
  
  for (region in names(valid_models_predict)) {
    region_list <- valid_models_predict[[region]]
    if (is.null(region_list) || length(region_list) == 0) next
    
    for (response in names(region_list)) {
      models_valid <- region_list[[response]]$models_valid
      
      if (!is.null(models_valid) && length(models_valid) > 0) {
        # crée la sous-liste région si besoin (déjà list() ici)
        res[[region]][[response]] <- models_valid
      }
    }
  }
  
  # optionnel : enlever les régions vides si tu ne veux pas les garder
  res <- res[vapply(res, length, integer(1)) > 0]
  
  res
}



valid_models_only <- get_valid_models(valid_models_predict)

grids_by_region <- list(east = grid_by_region$east$grid_pred,
                        west = grid_by_region$west$grid_pred)


predict_all_valid_models <- function(valid_models_predict, grids_by_region) {
  
  valid_models_only <- get_valid_models(valid_models_predict)
  
  preds <- list()
  
  for (region in names(valid_models_only)) {
    
    grid_region <- grids_by_region[[region]]
    if (is.null(grid_region)) stop("Pas de grid fournie pour la région : ", region)
    
    for (response in names(valid_models_only[[region]])) {
      
      for (model_name in names(valid_models_only[[region]][[response]])) {
        
        mod <- valid_models_only[[region]][[response]][[model_name]]
        
        preds[[region]][[response]][[model_name]] <- predict(
          mod,
          newdata = grid_region
        )
      }
    }
  }
  
  return(preds)
}


preds_all <- predict_all_valid_models(valid_models_predict, grids_by_region)
  


# Choisit quoi mapper : base (standard) ou base1+base2 (delta)
get_fill_expr <- function(data, base) {
  c1 <- paste0(base, "1")
  c2 <- paste0(base, "2")
  
  if (all(c(c1, c2) %in% names(data))) {
    parse_expr(paste0(c1, " + ", c2))
  } else if (base %in% names(data)) {
    sym(base)
  } 
}

plot_pred_map <- function(data, grid_pred, sp_scientific, response, fill_base, subtitle) {
  
  # conversion km -> degrés
  res_deg_lat <- res_km / 111.32
  res_deg_lon <- res_km / (111.32 * cos(50*pi/180))
  
  # limites avec buffer
  xmin_plot <- range(grid_pred$lon, na.rm = TRUE)[1] - res_deg_lon
  xmax_plot <- range(grid_pred$lon, na.rm = TRUE)[2] + res_deg_lon
  ymin_plot <- range(grid_pred$lat, na.rm = TRUE)[1] - res_deg_lat
  ymax_plot <- range(grid_pred$lat, na.rm = TRUE)[2] + res_deg_lat
  
  land <- ne_download(scale = "large", type = "land", category = "physical", returnclass = "sf") %>%
    st_transform(4326) %>%
    st_crop(xmin = xmin_plot, ymin = ymin_plot, xmax = xmax_plot, ymax = ymax_plot)
  
  fill_expr <- get_fill_expr(data, fill_base)
  
  legend_title <- ifelse(response == "totalWeightKg", "Biomasse (kg)", "Densité de \nbiomasse \n(kg/km²)")
  
  
  if (fill_base == "est") {
    p <- ggplot(data, aes(lon, lat, fill = exp(!!fill_expr))) +
      geom_raster()
  } else {
    p <- ggplot(data, aes(lon, lat, fill = !!fill_expr)) +
      geom_raster()
  }
  
  p <- p +
    geom_sf(data = land, fill = "grey80", inherit.aes = FALSE, color = "grey80") +
    scale_y_continuous(breaks = pretty_breaks(n = 3)) +
    scale_x_continuous(breaks = pretty_breaks(n = 4)) +
    coord_sf(xlim = c(xmin_plot, xmax_plot),
             ylim = c(ymin_plot, ymax_plot),
             expand = FALSE) +
    facet_wrap(~year, nrow = 2)+
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = "white", colour = NA),
          panel.border = element_rect(color = "white", fill = NA, linewidth = 2),
          strip.background = element_rect(fill = "grey40", color = "white"),
          strip.text = element_text(color = "white", face = "bold")) +
    labs(x = "", y = "", subtitle = subtitle)
  
  if (fill_base == "est" && !is.null(legend_title)) {
    p <- p + labs(fill = legend_title)
  }
  
  if (fill_base %in% c("est", "est_non_rf")) {
    p <- p +
      scale_fill_distiller(trans = "log10", palette = "RdYlBu",
        labels = label_number(drop0trailing = TRUE))
  } else if (fill_base %in% c("omega_s", "epsilon_st")) {
    p <- p +
      scale_fill_gradient2(low = muted("blue"), mid = "white", high = muted("red"), midpoint = 0,
        labels = scales::label_number(drop0trailing = TRUE))
  } 
  return(p)
  
}



fills_to_plot <- c("est", "est_non_rf", "omega_s", "epsilon_st")

out_dir <- here::here("05_OUTPUTS", "maps_predictions", paste0("maps_predictions_", sp_safe))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

plots_all <- list()

for (region in names(preds_all)) {
  
  grid_pred <- grids_by_region[[region]]
  
  for (response in names(preds_all[[region]])) {
    for (model_name in names(preds_all[[region]][[response]])) {
      
      pred <- preds_all[[region]][[response]][[model_name]]
      if (!inherits(pred, "data.frame")) pred <- as.data.frame(pred)
      
      for (fill_base in fills_to_plot) {
        
        # skip si la variable n’existe pas (ni base, ni base1/base2)
        ok <- (fill_base %in% names(pred)) ||
          all(paste0(fill_base, c("1","2")) %in% names(pred))
        if (!ok) next
        
        p <- plot_pred_map(
          data = pred,
          grid_pred = grid_pred,
          sp_scientific = sp_scientific,  
          response = response,
          fill_base = fill_base,
          subtitle = paste(region, " - ", response, " - ", model_name, " - ", fill_base)
        )
        
        plots_all[[region]][[response]][[model_name]][[fill_base]] <- p
        
      }
    }
  }
}



out_rds <- here::here(
  "05_OUTPUTS", "model_diagnostics",
  paste0("model_diagnostics_", sp_safe),
  paste0("plots_predictions_", sp_safe, ".rds")
)

dir.create(dirname(out_rds), showWarnings = FALSE, recursive = TRUE)

saveRDS(plots_all, out_rds)


# plots_all$east$densityKgKm2$tweedie$est_non_rf
# plots_all$east$totalWeightKg$tweedie$omega_s
