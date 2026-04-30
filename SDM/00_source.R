
# ------------------------------------------------------------------------------#
# ------------------------------------------------------------------------------#
# SCRIPT TO RUN SPECIES DISTRIBUTION MODELS
# ------------------------------------------------------------------------------#
# ------------------------------------------------------------------------------#
rm(list=ls())

# ------------------------------------------------------------------------------#
####  LOAD PACKAGES  #### 
# ------------------------------------------------------------------------------#
source(here::here('04_MODEL/packages/packages.R'))

# ------------------------------------------------------------------------------#
####  DEFINE PARAMETERS  #### 
# ------------------------------------------------------------------------------#
## Define species
# ----------------#
# sp_scientific <- "Capros aper"                 # Boarfish / Sanglier
# sp_scientific <- "Chelidonichthys cuculus"     # Red gurnard / Grondin rouge
# sp_scientific <- "Chelidonichthys lucerna"     # Tub gurnard / Grondin perlon
# sp_scientific <- "Conger conger"               # European conger / Congre d'Europe
# sp_scientific <- "Dicentrarchus labrax"        # European seabass / Bar européen
# sp_scientific <- "Engraulis encrasicolus"      # European anchovy / Anchois
# sp_scientific <- "Eutrigla gurnardus"          # Grey gurnard / Grondin gris
# sp_scientific <- "Melanogrammus aeglefinus"    # Haddock / Églefin
# sp_scientific <- "Merluccius merluccius"       # European hake / Merlu européen
# sp_scientific <- "Micromesistius poutassou"    # Blue whiting / Merlan bleu
# sp_scientific <- "Pollachius pollachius"       # Pollack / Lieu jaune
# sp_scientific <- "Squalus acanthias"           # Spiny dogfish / Aiguillat commun
sp_scientific <- "Trachurus trachurus"         # Atlantic horse mackerel / Chinchard commun
# sp_scientific <- "Zeus faber"                  # John Dory / Saint-Pierre
# sp_scientific <- "Clupea harengus"             # Atlantic herring / Hareng de l’Atlantique
# sp_scientific <- "Solea solea"                 # Common sole / Sole commune

sp_name_safe <- gsub("[^A-Za-z0-9_]", "_", sp_scientific)


## Define study refions for the species 
# ----------------------------------------#
species_criteria_region <- readRDS(here::here("01_DATA/species_criteria_region_10percent.rds"))%>%
  dplyr::filter(species == sp_scientific)

West_English_Channel = isTRUE(species_criteria_region$west_region)
East_English_Channel = isTRUE(species_criteria_region$east_region)

# ------------------------------------------------------------------------------#
####  LOAD AND TRANSFORM DATA  #### 
# ------------------------------------------------------------------------------#
source(here::here('03_TRANSFORM_DATA/transform_data.R'))

# ------------------------------------------------------------------------------#
####  COMPUTE AND PLOT MORAN's INDEX = SPATIAL AUTOCORRELATION  #### 
# ------------------------------------------------------------------------------#
source(here::here('04_MODEL/functions/compute_plot_Moran.R'))

# ------------------------------------------------------------------------------#
####  PLOT RESIDUALS OF MODEL FITTED WITHOUT RANDOM FIELD #### 
# ------------------------------------------------------------------------------#
source(here::here('04_MODEL/functions/compute_plot_residuals_withoutRF.R'))

# ------------------------------------------------------------------------------#
####  CREATE THE MESH  #### 
# ------------------------------------------------------------------------------#
source(here::here('04_MODEL/functions/create_mesh_species.R'))
source(here::here('04_MODEL/functions/MSA_add_boundary_to_mesh.R'))

mesh_by_region <- list()
# If West_English_Channel is TRUE, build the west mesh
if (isTRUE(West_English_Channel)) {
  mesh_by_region$west <- create_mesh_by_region(region_name = "west", sp_name_safe)
}
# If East_English_Channel is TRUE, build the east mesh
if (isTRUE(East_English_Channel)) {
  mesh_by_region$east <- create_mesh_by_region(region_name = "east", sp_name_safe)
}


# ------------------------------------------------------------------------------#
####  BUILD & FIT CANDITATE MODELS  #### 
# ------------------------------------------------------------------------------#
# Vector of response variables to be tested sequentially
responses <- c("totalWeightKg", "densityKgKm2")

# Fixed-effects
fixed_effect <- "1"

source(here::here("04_MODEL/functions/fit_candidate_models.R"))

# object to store fitted models
fitted_models_by_region <- list()

if (isTRUE(East_English_Channel)) {
  fitted_models_by_region$east <- fit_candidate_models(data_CGFS = data_CGFS_east, 
                                                       mesh = mesh_by_region$east$mesh, 
                                                       region_name = "east")
}

if (isTRUE(West_English_Channel)) {
  fitted_models_by_region$west <- fit_candidate_models(data_CGFS = data_CGFS_west, 
                                                       mesh = mesh_by_region$west$mesh, 
                                                       region_name = "west")
}

# ------------------------------------------------------------------------------#
#### SANITY FILTER FITTED MODELS ####
# ------------------------------------------------------------------------------#

source(here::here("04_MODEL/functions/sanity_filter_models.R"))

sanity_by_region <- list()
# Apply sanity checks to West English Channel models
if (isTRUE(West_English_Channel)) {
  sanity_by_region$west <- sanity_filter_models(fitted_models_by_region$west)
}
# Apply sanity checks to East English Channel models
if (isTRUE(East_English_Channel)) {
  sanity_by_region$east <- sanity_filter_models(fitted_models_by_region$east)
}


# ------------------------------------------------------------------------------#
#### EXTRACT CONVERGED MODELS and THEIR PARAMETER ESTIMATES ####
# ------------------------------------------------------------------------------#

source(here::here("04_MODEL/functions/get_converged_models.R"))
converged_models <- get_converged_models(sanity_by_region)

source(here::here("04_MODEL/functions/extract_parameter_estimates.R"))
params_west <- extract_params_converged_models(converged_models, region = "west")%>%
  mutate(region = "west")

params_east <- extract_params_converged_models(converged_models, region = "east")%>%
  mutate(region = "east")

params_all <- bind_rows(params_west, params_east) %>%
  mutate(region = factor(region, levels = c("west", "east")))


# ------------------------------------------------------------------------------#
####  COMPUTE ANALYTICAL RANDOMIZED QUANTILE RESIDUALS  #### 
# ------------------------------------------------------------------------------#

source(here::here("04_MODEL/functions/analytical_randomized_quantile_residuals.R"))
residuals_by_region <- list()

# West English Channel
if (isTRUE(West_English_Channel)) {
  residuals_by_region$west <- randomized_quantile_resids(converged_models, "west")
}
# East English Channel
if (isTRUE(East_English_Channel)) {
  residuals_by_region$east <- randomized_quantile_resids(converged_models, "east")
}


# ------------------------------------------------------------------------------#
####  SIMULATION BASED ANALYTICAL RANDOMIZED QUANTILE RESIDUALS  #### 
# ------------------------------------------------------------------------------#

source(here::here("04_MODEL/functions/simulation_based_randomized_quantile_residuals.R"))
simulation_errors_by_region <- list()
if (isTRUE(West_English_Channel)) {
  simulation_errors_by_region$west <- rmse_mae_from_sim(converged_models, "west")
}
if (isTRUE(East_English_Channel)) {
  simulation_errors_by_region$east <- rmse_mae_from_sim(converged_models, "east")
}


# ------------------------------------------------------------------------------#
####  cAIC weights  #### 
# ------------------------------------------------------------------------------#
source(here::here("04_MODEL/functions/AIC_weights.R"))
AIC_by_region <- list()

if (isTRUE(West_English_Channel)) {
  AIC_by_region$west <- compute_cAIC_weights(converged_models, "west")
}
if (isTRUE(East_English_Channel)) {
  AIC_by_region$east <- compute_cAIC_weights(converged_models, "east")
}


# ------------------------------------------------------------------------------#
####  RANDOM CROSS-VALIDATION #### 
# ------------------------------------------------------------------------------#
source(here::here("04_MODEL/functions/cross_validation_random.R"))
randomCV_by_region <- list()

if (isTRUE(West_English_Channel)) {
  randomCV_by_region$west <- random_CV(converged_models, "west")
}
if (isTRUE(East_English_Channel)) {
  randomCV_by_region$east <-  random_CV(converged_models, "east")
}


# ------------------------------------------------------------------------------#
####  SPATIO TEMPORAL BLOCKED CROSS-VALIDATION #### 
# ------------------------------------------------------------------------------#
source(here::here("04_MODEL/functions/cross_validation_spatiotemporal_block.R"))
blockedCV_by_region <- list()

if (isTRUE(West_English_Channel)) {
  blockedCV_by_region$west <- spatiotemp_blocked_CV(converged_models, "west")
}
if (isTRUE(East_English_Channel)) {
  blockedCV_by_region$east <-  spatiotemp_blocked_CV(converged_models, "east")
}


# ------------------------------------------------------------------------------#
####  DESIGN THE PREDICTION GRID  ####
# ------------------------------------------------------------------------------#
# grid resolution in km
res_km = 5
source(here::here('04_MODEL/functions/design_grid.R'))
grid_by_region <- list()
if (isTRUE(West_English_Channel)) grid_by_region$west <- design_grids("west")
if (isTRUE(East_English_Channel)) grid_by_region$east <- design_grids("east")


# ------------------------------------------------------------------------------#
####  PREDICT ON THE GRID AND MAP PREDICTIONS ####
# ------------------------------------------------------------------------------#
source(here::here("04_MODEL/functions/predict_all_converged_models.R"))
converged_models_predictions <- predict_all_converged_models(converged_models, grids_by_region)

source(here::here("04_MODEL/functions/map_predictions_common_fill_limits.R"))
# source(here::here("04_MODEL/functions/map_predictions.R"))


# ------------------------------------------------------------------------------#
####  SIMULATE & COMPUTE, PLOT COEFFICIENT OF VARIATION ####
# ------------------------------------------------------------------------------#
source(here::here("04_MODEL/functions/simulate_compute_plot_coeff_variation.R"))

# ------------------------------------------------------------------------------#
####  SAVE OUTPUTS ####  
# ------------------------------------------------------------------------------#
source(here::here("04_MODEL/functions/save_model_diagnostics.R"))


# ------------------------------------------------------------------------------#
####  SAVE ALL PLOTS INTO HTLM FILE ####
# ------------------------------------------------------------------------------#
rmarkdown::render(input = here::here("04_MODEL", "report", "report.Rmd"),
                  output_dir = new_repertory_model,
                  output_file = paste0(sp_name_safe, "_report", ".html"))


