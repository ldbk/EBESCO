
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
# sp_scientific <- "Trachurus trachurus"         # Atlantic horse mackerel / Chinchard commun
sp_scientific <- "Zeus faber"                  # John Dory / Saint-Pierre
# sp_scientific <- "Clupea harengus"             # Atlantic herring / Hareng de l’Atlantique
# sp_scientific <- "Solea solea"                 # Common sole / Sole commune

sp_name_safe <- gsub("[^A-Za-z0-9_]", "_", sp_scientific)


## Define study refions for the species 
# ----------------------------------------#
species_criteria_region <- readRDS(here::here("01_DATA/species_criteria_region_10percent.rds"))%>%
  dplyr::mutate(west_region = ifelse(species == sp_scientific, FALSE, west_region))%>%
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
####  CREATE THE MESH CONFIGURATIONS #### 
# ------------------------------------------------------------------------------#
source(here::here("04_MODEL/functions/MSA_add_boundary_to_mesh.R"))
source(here::here("04_MODEL/functions_all_mesh_configs/create_mesh_all_configs.R"))

# west_params <- list(cutoff_values = c(1, 2, 3, 5, 7, 10, 15, 20),
#                     max_edge_in = c(5, 10, 15, 25, 35, 50, 75, 100),
#                     max_edge_out = rep(100, 8))
# 
east_params <- list(cutoff_values = c(1, 2, 3, 5, 7, 10, 15, 20),
                    max_edge_in = c(5, 10, 15, 25, 35, 50, 75, 100),
                    max_edge_out = rep(100, 8))

# west_params <- list(cutoff_values = c(15, 20),
#                     max_edge_in = c(75, 100),
#                     max_edge_out = rep(100, 2))
# 
# east_params <- list(cutoff_values = c(15, 20),
#                     max_edge_in = c(75, 100),
#                     max_edge_out = rep(100, 2))

meshes_by_region <- list()

if (isTRUE(West_English_Channel)) {
  meshes_by_region$west <- do.call(create_meshes_configurations,
                                   c(list(region_name = "west"), west_params))
}

if (isTRUE(East_English_Channel)) {
  meshes_by_region$east <- do.call(create_meshes_configurations,
                                   c(list(region_name = "east"), east_params))
}


# ------------------------------------------------------------------------------#
####  BUILD & FIT CANDITATE MODELS  #### 
# ------------------------------------------------------------------------------#
# Vector of response variables to be tested sequentially
responses <- c("totalWeightKg")

# Fixed-effects
fixed_effect <- "1"

source(here::here("04_MODEL/functions_all_mesh_configs/fit_candidate_models_all_mesh_configs.R"))

# object to store fitted models
fitted_models_by_region <- list()

if (isTRUE(West_English_Channel)) {
  fitted_models_by_region$west <- fit_candidate_models(
    data_CGFS = data_CGFS_west,
    meshes_configurations = meshes_by_region$west,
    region_name = "west"
  )
}

if (isTRUE(East_English_Channel)) {
  fitted_models_by_region$east <- fit_candidate_models(
    data_CGFS = data_CGFS_east,
    meshes_configurations = meshes_by_region$east,
    region_name = "east"
  )
}

# ------------------------------------------------------------------------------#
#### SANITY FILTER FITTED MODELS ####
# ------------------------------------------------------------------------------#

source(here::here("04_MODEL/functions_all_mesh_configs/sanity_filter_models_all_mesh_configs.R"))

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

source(here::here("04_MODEL/functions_all_mesh_configs/get_converged_models_all_mesh_configs.R"))
converged_models <- get_converged_models(sanity_by_region)

source(here::here("04_MODEL/functions_all_mesh_configs/extract_parameter_estimates_all_mesh_configs.R"))
params_west <- extract_params_converged_models(converged_models, region = "west")%>%
  mutate(region = "west")

params_east <- extract_params_converged_models(converged_models, region = "east")%>%
  mutate(region = "east")

params_all <- bind_rows(params_west, params_east) %>%
  mutate(region = factor(region, levels = c("west", "east")))


# ------------------------------------------------------------------------------#
####  SIMULATION BASED ANALYTICAL RANDOMIZED QUANTILE RESIDUALS  #### 
# ------------------------------------------------------------------------------#

source(here::here("04_MODEL/functions_all_mesh_configs/RMSE_MAE_from_sim_all_mesh_configs.R"))
simulation_errors_by_region <- list()
if (isTRUE(West_English_Channel)) {
  simulation_errors_by_region$west <- rmse_mae_from_sim(converged_models, "west")
}
if (isTRUE(East_English_Channel)) {
  simulation_errors_by_region$east <- rmse_mae_from_sim(converged_models, "east")
}

source(here::here("04_MODEL/functions_all_mesh_configs/moran_from_sim_all_mesh_configs.R"))
moran_by_region <- list()
if (isTRUE(West_English_Channel)) {
  moran_by_region$west <- moran_from_sim(converged_models, "west")
}
if (isTRUE(East_English_Channel)) {
  moran_by_region$east <- moran_from_sim(converged_models, "east")
}
# ------------------------------------------------------------------------------#
####  cAIC weights  #### 
# ------------------------------------------------------------------------------#
source(here::here("04_MODEL/functions_all_mesh_configs/AIC_weights_all_mesh_configs.R"))
AIC_by_region <- list()

if (isTRUE(East_English_Channel)) {
  AIC_by_region$east <- compute_cAIC_weights(converged_models, "east")
}


# ------------------------------------------------------------------------------#
####  RANDOM CROSS-VALIDATION #### 
# ------------------------------------------------------------------------------#
source(here::here("04_MODEL/functions_all_mesh_configs/cross_validation_random_all_mesh_configs.R"))
randomCV_by_region <- list()

if (isTRUE(West_English_Channel)) {
  randomCV_by_region$west <- random_CV(converged_models, meshes_by_region, "west")
}
if (isTRUE(East_English_Channel)) {
  randomCV_by_region$east <-  random_CV(converged_models, meshes_by_region, "east")
}


# ------------------------------------------------------------------------------#
####  SPATIO TEMPORAL BLOCKED CROSS-VALIDATION #### 
# ------------------------------------------------------------------------------#
source(here::here("04_MODEL/functions_all_mesh_configs/cross_validation_blocked_all_mesh_configs.R"))
blockedCV_by_region <- list()

if (isTRUE(West_English_Channel)) {
  blockedCV_by_region$west <- spatiotemp_blocked_CV(converged_models, meshes_by_region, "west")
}
if (isTRUE(East_English_Channel)) {
  blockedCV_by_region$east <-  spatiotemp_blocked_CV(converged_models, meshes_by_region, "east")
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
source(here::here("04_MODEL/functions_all_mesh_configs/predict_all_converged_models_all_mesh_configs.R"))
converged_models_predictions <- predict_all_converged_models(converged_models, grids_by_region)

source(here::here("04_MODEL/functions_all_mesh_configs/map_pred_common_fill_limits_all_mesh_configs.R"))
# source(here::here("04_MODEL/functions/map_predictions.R"))


# ------------------------------------------------------------------------------#
####  SIMULATE & COMPUTE, PLOT COEFFICIENT OF VARIATION ####
# ------------------------------------------------------------------------------#
source(here::here("04_MODEL/functions_all_mesh_configs/simulate_compute_plot_coeff_variation_all_mesh_configs.R"))

# ------------------------------------------------------------------------------#
####  SAVE OUTPUTS ####  
# ------------------------------------------------------------------------------#
source(here::here("04_MODEL/functions/save_model_diagnostics.R"))


# ------------------------------------------------------------------------------#
####  SAVE ALL PLOTS INTO HTLM FILE ####
# ------------------------------------------------------------------------------#
rmarkdown::render(input = here::here("04_MODEL", "report", "report_all_mesh_configs.Rmd"),
                  output_dir = new_repertory_model,
                  output_file = paste0(sp_name_safe, "_report", ".html"))


