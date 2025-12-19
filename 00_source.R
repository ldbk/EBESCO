
# ------------------------------------------------------------------------------#
# ------------------------------------------------------------------------------#

# SCRIPT TO RUN SPECIES DISTRIBUTION MODELS

# ------------------------------------------------------------------------------#
# ------------------------------------------------------------------------------#


rm(list=ls())

# ------------------------------------------------------------------------------#
####  LOAD PACKAGES  #### 
# ------------------------------------------------------------------------------#

library(here)
source(here('04_MODEL/packages/packages.R'))


# ------------------------------------------------------------------------------#
####  DEFINE PARAMETERS  #### 
# ------------------------------------------------------------------------------#

## Define species
# ----------------#
sp_scientific = "Solea solea"
# sp_scientific = "Dicentrarchus labrax"

common_name = "sole"
# common_name = "seabass"

## Define domain
# ----------------#
# study_domain = "Eastern_English_Channel"
study_domain = "Entire_English_Channel"


# ------------------------------------------------------------------------------#
####  LOAD AND TRANSFORM DATA  #### 
# ------------------------------------------------------------------------------#

source(here('03_TRANSFORM_DATA/transform_data.R'))


# ------------------------------------------------------------------------------#
####  CREATE THE MESH  #### 
# ------------------------------------------------------------------------------#

# Cutoff defines the minimum allowed distance between mesh vertices 
cutoff = 16

source(here('04_MODEL/functions/create_mesh.R'))


# ------------------------------------------------------------------------------#
####  BUILD & RUN THE MODEL  #### 
# ------------------------------------------------------------------------------#

# ------------------------------#
# Choose the response variable
# ------------------------------#
# response = "presence_absence"    # presence/absence (binomial, logit link by default)
response = "densityKgKm2"        # biomass density (requires choosing a distribution family below)


# -------------------------------#
# Choose the distribution family (used only if Response = "densityKgKm2")
# -------------------------------#
distribution_family = "delta_gamma_poisson"
# distribution_family = "tweedie"
# distribution_family = "delta_gamma"
# distribution_family = "delta_lognormal"


# -----------------------------#
# Select predictive covariates (TRUE / FALSE) 
# -----------------------------#
year_factor_FE = TRUE              # categorical fixed effect (0 + factor(year))
depth_FE = TRUE                     # continuous depth effect
gear_factor_FE = TRUE               # categorical substrate effect
substrate_factor_FE = TRUE          # categorical gear effect

source(here("04_MODEL/functions/build_run_model.R"))

# Final formula for verification
print(model_formula)

# ------------------------------------------------------------------------------#
####  CHECK OUTPUTS  #### 
# ------------------------------------------------------------------------------#

summary(model)

sanity(model)

# Fixed effects with confidence intervals
tidy(model)

# Random effects and variance parameters
tidy(model, "ran_pars")


# ------------------------------------------------------------------------------#
####  DESIGN THE PREDICTION GRID  #### 
# ------------------------------------------------------------------------------#

# grid resolution in km
res_km = 6

# If the model includes a categorical substrate fixed effect (substrate_factor_FE = TRUE),
# decide how to handle grid cells classified as "Rock and boulder" (no survey observations in these cells).
# Options:
# - "recode_to_neighbors" : reassign these cells to the dominant substrate of neighbouring cells (prediction kept).
# - "exclude_from_grid" : remove these cells from the prediction grid (no spatial prediction for these areas).

# substrate_rock_handling = "recode_to_neighbors"
substrate_rock_handling = "exclude_from_grid"
  
source(here('04_MODEL/functions/design_grid.R'))


# ------------------------------------------------------------------------------#
####  PREDICT ON THE GRID #### 
# ------------------------------------------------------------------------------#

source(here("04_MODEL/functions/predict_on_grid.R"))


# ------------------------------------------------------------------------------#
####  CREATE NEW REPERTORY AND SAVE OUTPUTS ####  
# ------------------------------------------------------------------------------#

# Create output directory (in folder 04_OUTPUTS) structured by domain, species, 
# response, and predictors, then save model diagnostics, predictions, and HTML report
source(here("04_MODEL/functions/save_results.R"))

