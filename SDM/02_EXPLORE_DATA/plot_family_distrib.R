# ==============================================================================#
# Family distributions explorations : fitting + plotting
# ==============================================================================#


compute_plot_distr <- function(region, data, variable) {
  
  # ---------------------------------------------------------------------------- #
  #  Filter data by gear
  # ---------------------------------------------------------------------------- #
  if (region == "west"){
    dataframe <- data %>% 
      dplyr::filter(gear == "GOV 36/49") %>% 
      dplyr::mutate(region = "west") 
  }
  if (region == "east"){
    dataframe <- data %>% 
      dplyr::filter(gear == "GOV 36/47") %>% 
      dplyr::mutate(region = "east")
  }
  
  variable_all <- dataframe[[variable]]
  variable_pos <- variable_all[variable_all > 0]
  
  # ---------------------------------------------------------------------------- #
  #  Gamma VS Lognormal on positiv biomass (zero excluded)
  # ---------------------------------------------------------------------------- #
  
  # Fit distributions on positive values
  gamma_fit <- try(fitdist(variable_pos, "gamma")$estimate, silent = TRUE)
  lnorm_fit <- try(fitdist(variable_pos, "lnorm")$estimate, silent = TRUE)
  
  # Plot empirical histogram + fitted positive biomass
  plot_gamma_lognormal <- ggplot(data.frame(w = variable_pos), aes(x = w)) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 300, fill = "grey85", color = "grey60") +
    theme_minimal() +
    theme(legend.position = "top")+
    labs(x = paste(variable), y = "Densité", color = "Distribution",
         title = paste(region, " - ", variable, " - ", "gamma/lognormal"))
  
  # add gamma if fit was successful
  if (!inherits(gamma_fit, "try-error")) {
    plot_gamma_lognormal <- plot_gamma_lognormal +
      stat_function(aes(color = "Gamma"), fun = dgamma, linewidth = 0.8, alpha = 0.7,
                    args = list(shape = gamma_fit["shape"], rate = gamma_fit["rate"]))
  }
  
  # add lognormal if fit was successful
  if (!inherits(lnorm_fit, "try-error")) {
    plot_gamma_lognormal <- plot_gamma_lognormal +
      stat_function(aes(color = "Lognormale"), fun = dlnorm, linewidth = 0.8, alpha = 0.7,
                    args = list(meanlog = lnorm_fit["meanlog"], sdlog = lnorm_fit["sdlog"]))
  }
  
  plot_gamma_lognormal <- plot_gamma_lognormal +
    scale_color_manual(values = c("Gamma" = "red", "Lognormale" = "blue"))
  
  # ---------------------------------------------------------------------------- #
  #  Tweedie on all biomass (zero included)
  # ---------------------------------------------------------------------------- #
  
  # Estimate the Tweedie power parameter by profile likelihood
  invisible(capture.output({tw_prof1 <- try(tweedie.profile(variable_all ~ 1, 
                                                            p.vec = seq(1.05, 1.95, .05)), 
                                            silent = TRUE)}))

  plot_tweedie <- ggplot(data.frame(w = variable_all), aes(x = w)) +
    geom_histogram(aes(y = after_stat(density)),
                   bins=300, fill = "grey85", color = "grey60") +
    theme_minimal() +
    labs(x = paste(variable), y = "Densité",
         title = paste(region, " - ", variable, " - ", "tweedie"))
  
  if (!inherits(tw_prof1, "try-error")) {
    # Estimate phi = dispersion parameter at fixed Tweedie power parameter
    power <- tw_prof1$p.max
    invisible(capture.output({tw_prof2 <- try(tweedie.profile(variable_all ~ 1, 
                                                             p.vec = power), 
                                             silent = TRUE)}))
    
    if (!inherits(tw_prof2, "try-error")) {
      phi <- tw_prof2$phi.max
      mu <- mean(variable_all)
      
      plot_tweedie <- plot_tweedie +
        stat_function(fun = dtweedie,
                      args = list(mu = mu, phi = phi, power = power),
                      linewidth = 0.9, alpha = 0.85, color = "darkgreen")
    }
  }
  
  list(plot_gamma_lognormal = plot_gamma_lognormal,
       plot_tweedie = plot_tweedie)
}


# p_density_west <- compute_plot_distr("west", "densityKgKm2")
# p_weight_west <- compute_plot_distr("west", "totalWeightKg")
# 
# distrib_west <- (p_density_west$plot_gamma_lognormal | p_weight_west$plot_gamma_lognormal) /
#   (p_density_west$plot_tweedie | p_weight_west$plot_tweedie)
# 
# p_density_east <- compute_plot_distr("east", "densityKgKm2")
# p_weight_east  <- compute_plot_distr("east", "totalWeightKg")
# 
# distrib_east <- (p_density_east$plot_gamma_lognormal | p_weight_east$plot_gamma_lognormal) /
#   (p_density_east$plot_tweedie | p_weight_east$plot_tweedie)




# # ---------------------------------------------------------------------------- #
# #  Filter data by gear
# # ---------------------------------------------------------------------------- #
# west_df <- data_CGFS_crs %>% filter(gear == "GOV 36/49")
# east_df <- data_CGFS_crs %>% filter(gear == "GOV 36/47")
# 
# 
# # ---------------------------------------------------------------------------- #
# #  Gamma VS Lognormal on positiv biomass (zero excluded)
# # ---------------------------------------------------------------------------- #
# west_biomass_all <- west_df$totalWeightKg
# west_biomass_pos <- west_biomass_all[west_biomass_all > 0]
# 
# # Fit distributions on positive values
# fit_west_gamma <- fitdist(west_biomass_pos, "gamma")
# fit_west_lognormal <- fitdist(west_biomass_pos, "lnorm")
# 
# # Plot empirical histogram + fitted positive biomass
# ggplot(data.frame(w = west_biomass_pos), aes(x = w)) +
#   geom_histogram(aes(y = after_stat(density)),
#                  bins=300, fill = "grey85", color = "grey60") +
#   stat_function(aes(color = "Gamma"), fun = dgamma, linewidth = 0.8, alpha = 0.7,
#                 args = list(shape = fit_west_gamma$estimate["shape"],
#                             rate  = fit_west_gamma$estimate["rate"])) +
#   stat_function(aes(color = "Lognormale"), fun = dlnorm, linewidth = 0.8, alpha = 0.7,
#                 args = list(meanlog = fit_west_lognormal$estimate["meanlog"],
#                             sdlog   = fit_west_lognormal$estimate["sdlog"])) +
#   scale_color_manual(values = c("Gamma" = "red", "Lognormale" = "blue")) +
#   theme_minimal() +
#   labs(x = "Biomasse (kg)", y = "Densité", color = "Distribution",
#        title = "Distribution théorique potentielle des données de biomasses positives")
# 
# 
# 
# # ---------------------------------------------------------------------------- #
# #  Tweedie on all biomass (zero included)
# # ---------------------------------------------------------------------------- #
# 
# # Estimate the Tweedie power parameter by profile likelihood
# west_biomass_tweedie <- tweedie.profile(west_biomass_all ~ 1, p.vec = seq(1.05, 1.95, by = 0.05))
# power <- west_biomass_tweedie$p.max
# 
# # Estimate phi = dispersion parameter at fixed Tweedie power parameter 
# fit_west_biomass_tweedie <- tweedie.profile(west_biomass_all ~ 1, p.vec = power)
# phi <- fit_west_biomass_tweedie$phi.max
# 
# # mean (intercept only ~ 1)
# mu  <- mean(west_biomass_all)   
# 
# # Plot histogram + Tweedie density 
# ggplot(data.frame(w = west_biomass_all), aes(x = w)) +
#   geom_histogram(aes(y = after_stat(density)),
#                  binwidth = 0.1, fill = "grey85", color = "grey60") +
#   stat_function(fun = dtweedie,
#                 args = list(mu = mu, phi = phi, power = power),
#                 linewidth = 0.9, alpha = 0.85, color = "darkgreen") +
#   theme_minimal() +
#   labs(x = "Biomasse (kg)", y = "Densité",
#        title = "Ajustement Tweedie (zéros inclus) sur la biomasse")
