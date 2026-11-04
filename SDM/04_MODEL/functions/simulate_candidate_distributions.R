# ============================================================================== #
# SIMULATE AND VISUALIZE CANDIDATE RESPONSE DISTRIBUTIONS
# ============================================================================== #
#
# Description:
#   Simulate candidate probability distributions for fish biomass or density
#   observations from the ECGFS and WCGFS surveys, and compare their
#   distributions with the observed data.
#
# Objectives:
#   1. Estimate distribution parameters from observed survey responses.
#   2. Simulate delta-lognormal, delta-gamma, and Tweedie distributions
#      to account for zero-inflated, continuous positive responses.
#   3. Handle the specific case of complete occurrence (presence probability
#      equal to 1) using lognormal and gamma distributions.
#   4. Visualize observed and simulated response distributions to assess
#      candidate distribution families.
#
# ============================================================================== #


# ----------------------------------------------------------------------------- #
#### 1. DEFINE THE DISTRIBUTION SIMULATION FUNCTION ####
# ----------------------------------------------------------------------------- #

sim_plot_distrib <- function(data_CGFS, region, response){
  
  
  set.seed(123)
  
  pos_response <- response[response > 0]
  nb_hauls <- length(response)
  
  # ----------------------------------------------------------------------------- #
  #### 2. HANDLE THE CASE OF COMPLETE SPECIES OCCURRENCE ####
  # ----------------------------------------------------------------------------- #
  
  # Specific case : probability of presence = 1 
  if (sp_scientific == "Trachurus trachurus" && region == "west") {
    
    # 1) LOGNORMAL
    # lognormal parameters
    sdlog =  sqrt(log(1 + (sd(pos_response)^2)/(mean(pos_response)^2)))
    meanlog = log(mean(pos_response)) - (1/2)*sdlog^2 
    
    # simulate delta-lognormal
    sim_lognormal <- rlnorm(nb_hauls, meanlog = meanlog, sdlog = sdlog)
    
    # 2) GAMMA
    # gamma parameters
    gshape = mean(pos_response)^2/sd(pos_response)^2   
    gscale = sd(pos_response)^2/mean(pos_response)
    
    # simulate delta-gamma
    sim_gamma <- rgamma(nb_hauls, shape = gshape, scale = gscale)
    
    # PLOT
    response_name <- deparse(substitute(response))
    xlab <- paste(response_name)
    
    plot(density(response),
         lwd = 2,
         col = "black",
         xlab = xlab,
         ylab = "Density",
         main = "")
    
    lines(density(sim_lognormal), lwd = 2, lty = 2, col = "dodgerblue3")
    lines(density(sim_gamma),  lwd = 2, lty = 2, col = "springgreen4")
    
    legend("topright",
           legend = c("Observed", "Lognormal", "Gamma"),
           col = c("black", "dodgerblue3", "springgreen4"),
           lwd = 2,
           lty = c(1, 2, 2),
           bty = "n")
    
    return(recordPlot())
    
  } else {
    
    # ----------------------------------------------------------------------------- #
    #### 3. HANDLE THE CASE OF COMPLETE SPECIES OCCURRENCE ####
    # ----------------------------------------------------------------------------- #
    
    # --- Binomial presence-absence component ---
    # ----------------------------------------------------------------------------- #
    # presence probability 
    classic_ppresence <- mean(response > 0)
    
    # simulate binomial
    sim_binomial <- rbinom(n = nb_hauls,                 # number of observations
                           size = 1,                     # number of trials 
                           prob = classic_ppresence)     # probability of success on each trial
    
    
    # --- Delta-lognormal distribution ---
    # ------------------------------------ #
    # lognormal parameters and simulate delta-lognormal
    sim_dlognormal <- tryCatch({
      sdlog  <- sqrt(log(1 + (sd(pos_response)^2)/(mean(pos_response)^2)))
      meanlog <- log(mean(pos_response)) - 0.5 * sdlog^2
      sim_binomial * rlnorm(nb_hauls, meanlog = meanlog, sdlog = sdlog)
    }, error = function(e) NULL)
    
    
    # --- Delta-gamma distribution ---
    # ------------------------------------ #
    # gamma parameters and simulate delta-gamma
    sim_dgamma <- tryCatch({
      gshape <- mean(pos_response)^2 / sd(pos_response)^2
      gscale <- sd(pos_response)^2 / mean(pos_response)
      sim_binomial * rgamma(nb_hauls, shape = gshape, scale = gscale)
    }, error = function(e) NULL)
    
    
    # --- Tweedie distribution ---
    # ------------------------------------ #
    # Estimate the Tweedie power parameter by profile likelihood
    sim_tweedie2 <- tryCatch(suppressWarnings(tweedie.profile(response ~ 1, 
                                                              p.vec = seq(1.05, 1.95, 0.05))),
                             error = function(e) NULL)
    
    if (!is.null(sim_tweedie2)) {
      power <- sim_tweedie2$p.max
      phi <- sim_tweedie2$phi.max
      mu <- mean(response, na.rm = TRUE)
      sim_tweedie <- fishMod::rTweedie(n = nb_hauls, p = power, mu = mu, phi = phi)
    } else {
      sim_tweedie <- NULL   
    }
    
    
    # -------------------------------------------------------------------------- #
    #### 4. VISUALIZE OBSERVED AND SIMULATED DISTRIBUTIONS ####
    # -------------------------------------------------------------------------- #
    
    # Compare the empirical density of observed responses with the simulated 
    # delta-lognormal, delta-gamma, and Tweedie distributions.
    # Simulation curves are skipped if the corresponding density calculation fails.
    
    response_name <- deparse(substitute(response))
    xlab <- paste(response_name)
    
    plot(density(response),
         lwd = 2,
         col = "black",
         xlab = xlab,
         ylab = "Density",
         main = "")
    
    try(lines(density(sim_dlognormal), lwd = 2, lty = 2, col = "dodgerblue3"), silent = TRUE)
    try(lines(density(sim_dgamma), lwd = 2, lty = 2, col = "springgreen4"), silent = TRUE)
    try(lines(density(sim_tweedie), lwd = 2, lty = 2, col = "darkorange3"), silent = TRUE)
    
    legend("topright",
           legend = c("Observed", "Delta-lognormal", "Delta-gamma", "Tweedie"),
           col = c("black", "dodgerblue3", "springgreen4", "darkorange3"),
           lwd = 2,
           lty = c(1, 2, 2, 2),
           bty = "n")
    
    return(recordPlot())
    
  }
  
}


# -------------------------------------------------------------------------- #
#### 5. GENERATE DISTRIBUTION COMPARISON PLOTS BY REGION ####
# -------------------------------------------------------------------------- #

# # --- Western English Channel (WCGFS) ---
# if (isTRUE(West_English_Channel)) {
#   plot_dist_west_biomass <- sim_plot_distrib(data_CGFS_west, "west", data_CGFS_west$totalWeightKg)
#   plot_dist_west_density <- sim_plot_distrib(data_CGFS_west, "west", data_CGFS_west$densityKgKm2)
# }
# 
# # --- Eastern English Channel (WCGFS) ---
# if (isTRUE(East_English_Channel)) {
#   plot_dist_east_biomass <- sim_plot_distrib(data_CGFS_east, "east", data_CGFS_east$totalWeightKg)
#   plot_dist_east_density <- sim_plot_distrib(data_CGFS_east, "east", data_CGFS_east$densityKgKm2)
# }
# 









