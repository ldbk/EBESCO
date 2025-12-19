# ----------------------------------------------------------------------------- #
# BUILD MODEL FORMULA, SELECT DISTRIBUTION FAMILY, RUN THE MODEL
# ----------------------------------------------------------------------------- #
# This script:
# - Builds the fixed-effects model formula based on selected covariates (in source script)
# - Selects the distribution family depending on the response variable and chosen distribution
# - Fits the sdmTMB model
# ----------------------------------------------------------------------------- #

data_test_model <- data_CGFS_crs %>% 
  dplyr::select(year, X, Y, lat, lon, gear, depth, substrate, presence_absence, densityKgKm2, totalWeightKg, sweptAreaKm2) %>%
  mutate(year = factor(year)) %>%
  mutate(gear = factor(gear)) %>%
  mutate(substrate = factor(substrate))

# ----------------------------------------------------------------------------- #
#### Build the model formula ####
# ----------------------------------------------------------------------------- #

# Initialize an empty character vector to store predictor terms
covariables <- character()

# Add covariates conditionally, based on user choices (in source script)
if (isTRUE(year_factor_FE)) covariables <- c(covariables, "0 + as.factor(year)")
if (isTRUE(depth_FE)) covariables <- c(covariables, "depth")
if (isTRUE(gear_factor_FE)) covariables <- c(covariables, "as.factor(gear)")
if (isTRUE(substrate_factor_FE)) covariables <- c(covariables, "as.factor(substrate)")

# Combine response and predictors into a model formula
model_formula <- as.formula(paste(response, "~", paste(covariables, collapse = " + ")))



# ----------------------------------------------------------------------------- #
#### Select the distribution family ####
# ----------------------------------------------------------------------------- #

# Models with biomass density as response
if (response == "densityKgKm2") {

    if (distribution_family == "tweedie") {
      selected_family <- tweedie(link = "log")

    } else if (distribution_family == "delta_gamma") {
      selected_family <- delta_gamma(link1 = "logit", link2 = "log")

    } else if (distribution_family == "delta_lognormal") {
      selected_family <- delta_lognormal(link1 = "logit", link2 = "log")

    } else if (distribution_family == "delta_gamma_poisson") {
      selected_family <- delta_gamma(type = "poisson-link")

    }

  # Presence/absence models
  } else {
    selected_family <- binomial(link = "logit")
}


# ----------------------------------------------------------------------------- #
#### Fit the model ####
# ----------------------------------------------------------------------------- #

model <- sdmTMB(data = data_test_model,
                formula = totalWeightKg ~ as.factor(year) + depth + as.factor(gear) + as.factor(substrate),
                mesh = bspde,
                family = delta_gamma(type = "poisson-link"),
                spatial = "on", 
                offset = log(data_test_model$sweptAreaKm2))
                # time = "year",
                # spatiotemporal = "IID")

 
# model <- sdmTMB(data = data_test_model,
#                 formula = model_formula,
#                 mesh = bspde,
#                 family =  select_family,
#                 spatial = "on", 
#                 time = "year",
#                 spatiotemporal = "IID")
