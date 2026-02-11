# ============================================================================== #
#### COMPUTE & PLOT RESIDUALS OF MODELS FITTED WITHOUT RANDOM FIELD ####
# ============================================================================== #

residuals_withoutRF <- function(data_CGFS, region = "region") {
  
  if (sp_scientific == "Trachurus trachurus" && region == "west") {
    model_withoutRF <- sdmTMB(data = data_CGFS,
                              formula = densityKgKm2 ~ 1,
                              family = Gamma(link = "log"), 
                              spatial = "off", 
                              time = NULL, 
                              spatiotemporal = "off")
  } else {
    model_withoutRF <- sdmTMB(data = data_CGFS,
                              formula = densityKgKm2 ~ 1,
                              family = tweedie(link = "log"), 
                              spatial = "off", 
                              time = NULL, 
                              spatiotemporal = "off")
  }
  
  resids_withoutRF <- residuals(model_withoutRF, type = "mle-mvn")
  resids_withoutRF <- tibble(residuals = resids_withoutRF)
  resids_for_plot <- bind_cols(data_CGFS, resids_withoutRF)

  # resids_for_plot <- resids_for_plot %>%
  #   group_by(year) %>%
  #   filter(residuals >= quantile(residuals, 0.025),
  #          residuals <= quantile(residuals, 0.975)) %>%
  #   ungroup()
  
  ggplot(resids_for_plot, aes(lon, lat, fill = residuals)) +
    geom_point(color = "grey", size = 2.5, shape = 21) +
    scale_fill_gradient2(low = "red", high = "blue")+
    facet_wrap(~ year) +
    labs(x = "", y = "", fill = "Quantile residuals",
         title = bquote(italic(.(sp_scientific)) ~ " - " ~ .(region)))+
    theme_bw()
  
}


# if (isTRUE(West_English_Channel)) {
#   plot_resids_withoutRF_west <- residuals_withoutRF(data_CGFS_west, region = "west")
# }
# 
# if (isTRUE(East_English_Channel)) {
#   plot_resids_withoutRF_east <- residuals_withoutRF(data_CGFS_east, region = "east")
# }
