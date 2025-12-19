


# Predict values on the full grid for all years
pred_fit <- predict(model, newdata = grid_pred)

# Convert resolution from kilometers to degrees 
# Adjusted with cos(latitude) for longitudinal distortion
km_to_deg_lat <- (res_km)/ (111.32)                       # conversion km -> degree (latitude)  1° latitude ≈ 111.32 km
km_to_deg_lon <- (res_km)/ (111.32 * cos(pi*50/180))      # conversion km -> degree (longitude)

# Define plotting boundaries with buffer (half grid cell = res_km/2)
xmin_plot = range(grid_pred$lon)[1] - km_to_deg_lon
xmax_plot = range(grid_pred$lon)[2] + km_to_deg_lon
ymin_plot = range(grid_pred$lat)[1] - km_to_deg_lat
ymax_plot = range(grid_pred$lat)[2] + km_to_deg_lat

# coastline <- ne_download(scale = "large", type = "coastline", category = "physical", returnclass = "sf") %>%
#   st_transform(4326) %>%
#   st_crop(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)

# Download and crop land polygon for plotting
land <- ne_download(scale = "large", type = "land", category = "physical", returnclass = "sf") %>%
  st_transform(4326) %>%
  st_crop(xmin = xmin_plot, ymin = ymin_plot, xmax = xmax_plot, ymax = ymax_plot)

# if (response == "densityKgKm2") {               
#   
#   ggplot(pred_fit, aes(lon, lat, fill = exp(est))) + 
#     geom_raster() +
#     facet_wrap(~year) +
#     geom_sf(data=land, fill="grey80", inherit.aes = FALSE, color = "grey80") +
#     scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+
#     scale_x_continuous(breaks = scales::pretty_breaks(n = 4))+
#     scale_fill_viridis_c(option = "magma", trans = "log10", 
#                          labels = scales::label_number(drop0trailing = TRUE))+
#     coord_sf(xlim = c(xmin_plot, xmax_plot), 
#              ylim = c(ymin_plot, ymax_plot), 
#              expand = FALSE) +
#     theme(panel.grid = element_blank(),
#           panel.background = element_rect(fill = "white", colour = NA), 
#           panel.border = element_rect(color = "white", fill = NA, linewidth = 2),
#           strip.background = element_rect(fill = "grey40", color = "white"),
#           strip.text = element_text(color = "white", face = "bold"))+
#     ggtitle(bquote("Distribution spatiale de la densité de biomasse de " * 
#                      italic(.(sp_scientific)) * ""))+
#     labs(x = "", y ="", 
#          subtitle = "Effets fixes + aléatoires spatiaux + aléatoires spatio-temporels")
#   
# }
# 
# if (response == "presence_absence") {               
#   
#   ggplot(pred_fit, aes(lon, lat, fill = exp(est))) + 
#     geom_raster() +
#     facet_wrap(~year) +
#     geom_sf(data=land, fill="grey80", inherit.aes = FALSE, color = "grey80") +
#     scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+
#     scale_x_continuous(breaks = scales::pretty_breaks(n = 4))+
#     scale_fill_viridis_c(option = "magma", trans = "log10", 
#                          labels = scales::label_number(drop0trailing = TRUE))+
#     coord_sf(xlim = c(xmin_plot, xmax_plot), 
#              ylim = c(ymin_plot, ymax_plot), 
#              expand = FALSE) +
#     theme(panel.grid = element_blank(),
#           panel.background = element_rect(fill = "white", colour = NA), 
#           panel.border = element_rect(color = "white", fill = NA, linewidth = 2),
#           strip.background = element_rect(fill = "grey40", color = "white"),
#           strip.text = element_text(color = "white", face = "bold"))+
#     ggtitle(bquote("Distribution spatiale de " * italic(.(sp_scientific)) * ""))+
#     labs(x = "", y ="", 
#          subtitle = "Effets fixes + aléatoires spatiaux + aléatoires spatio-temporels")
#   
# }


