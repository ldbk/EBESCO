
# ============================================================================== #
#### GENERAL MODEL TEMPLATE WITH sdmTMB() ####
# ============================================================================== #

# m <- sdmTMB(data  = my_data,                   # data frame with response, covariates, coords, time
            # formula = response ~ predictors,   # fixed-effects structure
            # mesh  = mesh,                      # SPDE mesh object
            # family = ...,                      # e.g. binomial(link = "logit"), tweedie(link = "log")
            # spatial = "on",                    # "off", "on", or "only"
            # time   = "year",                   # column name for time slices
            # spatiotemporal = "IID"             # "off", "IID", "AR1", or "RW"
            # offset = log(effort))              # optional: e.g. swept area, transect length, etc.


# --------------------------------------- #
# SPATIAL & SPATIOTEMPORAL RANDOM FIELDS
# --------------------------------------- #
# spatial:
# - "off" : no spatial random effects
# - "on" : include spatial field + fixed effects
# - "only" : spatial field only (no fixed effects in linear predictor)

# To include spatiotemporal random fields, you must:
# 1) Provide a time variable (e.g. time = "year")
# 2) Choose a spatiotemporal structure:
# - "off" : no spatiotemporal field
# - "IID" : independent and identically distributed, independent fields for each time step (no temporal correlation)
# - "AR1" : first-order autoregressive in time (persistent patterns)
# - "RW" : random walk in time (smooth temporal evolution)

# In practice:
# - "IID" if you want independent yearly deviations.
# - "AR1" or "RW" if you expect temporal dependence in the spatial pattern.






# ============================================================================== #
#### TESTING ALTERNATIVE SPATIOTEMPORAL MODELS #### 
# ============================================================================== #

data_test_model <- data_CGFS_crs %>% 
  dplyr::select(year, X, Y, lat, lon, gear, depth, substrate, presence_absence, densityKgKm2, totalWeightKg, sweptAreaKm2)


# ------------------------------------------------------------------------------ #
#### m1 : presence/absence ~ year #### 
# ------------------------------------------------------------------------------ #


if (model_version %in% c("m1_presence_year")){
  
  model_formula = presence_absence ~ 0 + as.factor(year)
  
  model <- sdmTMB(data = data_test_model,
                  formula = model_formula,
                  mesh = mesh,
                  family = binomial(link = "logit"),
                  spatial = "on", 
                  time = "year",
                  spatiotemporal = "IID")
  
}



# ------------------------------------------------------------------------------ #
#### m2 : density ~ year #### 
# ------------------------------------------------------------------------------ #

if (model_version %in% c("m2_density_year")){
  
  model_formula = densityKgKm2 ~ 0 + as.factor(year)
    
  model <- sdmTMB(data = data_test_model,
                  formula = model_formula,
                  mesh = mesh,
                  family = tweedie(link = "log"),
                  spatial = "on", 
                  time = "year",
                  spatiotemporal = "IID")
}


# ------------------------------------------------------------------------------ #
#### m3 : presence/absence ~ year + depth #### 
# ------------------------------------------------------------------------------ #

if (model_version %in% c("m3_presence_year_depth")){
  
  model_formula = presence_absence ~ 0 + as.factor(year) + depth
    
  model <- sdmTMB(data = data_test_model,
                  formula = model_formula,
                  mesh = mesh,
                  family = binomial(link = "logit"),
                  spatial = "on", 
                  time = "year",
                  spatiotemporal = "IID")
}



# ------------------------------------------------------------------------------ #
#### m4 : density ~ year  + depth #### 
# ------------------------------------------------------------------------------ #

if (model_version %in% c("m4_density_year_depth")){
  
  model_formula = densityKgKm2 ~ 0 + as.factor(year) + depth
    
  model <- sdmTMB(data = data_test_model,
                  formula = model_formula,
                  mesh = bspde,
                  family = tweedie(link = "log"),
                  spatial = "on", 
                  time = "year",
                  spatiotemporal = "IID")
  
}



# ------------------------------------------------------------------------------ #
#### m5 : density ~ year + depth + gear #### 
# ------------------------------------------------------------------------------ #

if (model_version %in% c("m5_density_year_depth_gear")){
  
  model_formula = densityKgKm2 ~ 0 + as.factor(year) + depth + as.factor(gear)
  
  model <- sdmTMB(data = data_test_model,
                  formula = model_formula,
                  mesh = bspde,
                  family = tweedie(link = "log"),
                  spatial = "on", 
                  time = "year",
                  spatiotemporal = "IID")
  
}


# ------------------------------------------------------------------------------ #
#### m6 : density ~ year + depth + substrate + gear #### 
# ------------------------------------------------------------------------------ #

if (model_version %in% c("m6_density_year_depth_substrate_gear")){
  
  model_formula = densityKgKm2 ~ 0  + as.factor(year) + depth + as.factor(substrate) + as.factor(gear)
  
  model <- sdmTMB(data = data_test_model,
                  formula = model_formula,
                  mesh = bspde,
                  family = tweedie(link = "log"),
                  # family = delta_lognormal(link1 = "logit", link2 = "log"),
                  # family = delta_gamma(link1 = "logit", link2 = "log"),
                  # family =  delta_gamma(type = "poisson-link"),
                  spatial = "on", 
                  time = "year",
                  spatiotemporal = "IID")
  
}


if (model_version %in% c("m7_presence_year_depth_substrate_gear")){
  
  model_formula = presence_absence ~ 0 + as.factor(year) + depth + as.factor(substrate) + as.factor(gear)
  
  model <- sdmTMB(data = data_test_model,
                  formula = model_formula,
                  mesh = bspde,
                  family = binomial(link = "logit"),
                  spatial = "on", 
                  time = "year",
                  spatiotemporal = "IID")
  
}




# #-----------------------#
# # Parameter estimates
# #-----------------------#
# # Fixed effects with confidence intervals
# tidy(model)
# # Random effects and variance parameters
# tidy(model, "ran_pars")
# 
# 
# #-----------------------#
# # Model diagnostics
# #-----------------------#
# # Randomized quantile residuals
# data_test_model$resids <- residuals(model) 
# qqnorm(data_test_model$resids)
# qqline(data_test_model$resids)
# 
# # Spatial visualization of residuals
# ggplot(data_test_model, aes(X, Y, col = resids)) +
#   scale_colour_gradient2() +
#   geom_point(size = 2) +
#   facet_wrap(~year) +
#   coord_fixed()


