
# ============================================================================== #
# SPECIES DISTRIBUTION MODELLING WORKFLOW
#
# Objectives:
# - Prepare species-specific survey data for the selected study regions.
# - Explore spatial autocorrelation and preliminary model diagnostics.
# - Generate spatial meshes at different resolutions.
# - Fit candidate sdmTMB models across likelihood formulations
#   and mesh resolutions.
# - Assess model convergence, parameter estimates and diagnostics.
# - Evaluate goodness-of-fit, predictive performance and
#   prediction uncertainty using complementary metrics.
# - Generate spatial predictions, uncertainty estimates and associated maps.
# - Compile model results and diagnostics into an HTML report.
# ============================================================================== #

# ------------------------------------------------------------------------------ #
#### 1. INITIALISE THE MODELLING ENVIRONMENT ####
# ------------------------------------------------------------------------------ #

# Clear the current R environment
rm(list = ls())

# Load required packages
source(here::here("04_MODEL/packages/packages.R"))

# ------------------------------------------------------------------------------ #
#### 2. DEFINE SPECIES AND STUDY REGIONS ####
# ------------------------------------------------------------------------------ #

# Select the species to model
## Define species
# ----------------#
# sp_scientific <- "Capros aper"                 # Boarfish / Sanglier
# sp_scientific <- "Chelidonichthys cuculus"     # Red gurnard / Grondin rouge
# sp_scientific <- "Chelidonichthys lucerna"     # Tub gurnard / Grondin perlon
# sp_scientific <- "Conger conger"               # European conger / Congre d"Europe
# sp_scientific <- "Dicentrarchus labrax"        # European seabass / Bar européen
# sp_scientific <- "Engraulis encrasicolus"      # European anchovy / Anchois
# sp_scientific <- "Hippocampus hippocampus"     # seahorse / Hippocampe
# sp_scientific <- "Merluccius merluccius"       # European hake / Merlu européen
# sp_scientific <- "Micromesistius poutassou"    # Blue whiting / Merlan bleu
sp_scientific <- "Trachurus trachurus"         # Atlantic horse mackerel / Chinchard commun
# sp_scientific <- "Zeus faber"                  # John Dory / Saint-Pierre
# sp_scientific <- "Solea solea"                 # Common sole / Sole commune


## Define study refions for the species 
# ----------------------------------------#
# Load the previously established species-by-region selection table.
# For each species, TRUE/FALSE indicates whether it was selected
# for modelling in the eastern and western English Channel.
species_criteria_region <- readRDS(here::here("01_DATA/species_criteria_region_10percent.rds"))%>%
  dplyr::filter(species == sp_scientific)

# Set the study regions according to the predefined selection.
West_English_Channel = isTRUE(species_criteria_region$west_region)
East_English_Channel = isTRUE(species_criteria_region$east_region)

# Create the species-specific output directory
source(here::here("04_MODEL/functions/setup_model_outputs.R"))


# ------------------------------------------------------------------------------ #
#### 3. LOAD AND PREPARE SURVEY DATA ####
# ------------------------------------------------------------------------------ #

# Load and transform CGFS survey data for the selected species and regions
source(here::here("03_TRANSFORM_DATA/transform_data.R"))


# ------------------------------------------------------------------------------ #
#### 4. ASSESS SPATIAL AUTOCORRELATION AND PRELIMINARY RESIDUALS ####
# ------------------------------------------------------------------------------ #

# Compute and plot Moran's I to assess spatial autocorrelation
source(here::here("04_MODEL/functions/assess_spatial_autocorrelation.R"))

# Fit preliminary models without random fields and examine residuals
source(here::here("04_MODEL/functions/assess_residuals_without_random_fields.R"))


# ------------------------------------------------------------------------------ #
#### 5. GENERATE SPATIAL MESH CONFIGURATIONS ####
# ------------------------------------------------------------------------------ #

# Load the function used to add boundaries to spatial meshes
source(here::here("04_MODEL/functions/add_mesh_boundary.R"))

# Define mesh parameters for the Western English Channel
west_params <- list(cutoff_values = c(3, 5, 7, 10, 15),
                    max_edge_in = c(15, 25, 35, 50, 75),
                    max_edge_out = rep(100, 5))

# Define mesh parameters for the Eastern English Channel
east_params <- list(cutoff_values = c(3, 5, 7, 10, 15),
                    max_edge_in = c(15, 25, 35, 50, 75),
                    max_edge_out = rep(100, 5))

# Generate all mesh configurations for the selected regions
source(here::here("04_MODEL/functions/create_meshes.R"))


# ------------------------------------------------------------------------------ #
#### 6. DEFINE AND FIT CANDIDATE MODELS ####
# ------------------------------------------------------------------------------ #

# Select response variables: "totalWeightKg" and/or "densityKgKm2"
responses <- c("totalWeightKg")

# Define the fixed-effects formula (1 = as an intercept-only model)
fixed_effect <- "1"

# Fit candidate models across all mesh configurations
source(here::here("04_MODEL/functions/fit_candidate_models.R"))


# ------------------------------------------------------------------------------ #
#### 7. CHECK MODEL CONVERGENCE AND EXTRACT PARAMETERS ####
# ------------------------------------------------------------------------------ #

# Assess model convergence and numerical diagnostics
source(here::here("04_MODEL/functions/check_model_convergence.R"))

# Retain models that pass the convergence checks (on sanity results)
source(here::here("04_MODEL/functions/extract_converged_models.R"))

# Extract parameter estimates from converged models
source(here::here("04_MODEL/functions/extract_model_parameters.R"))


# ------------------------------------------------------------------------------ #
#### 8. ASSESS MODEL RESIDUAL DIAGNOSTICS ####
# ------------------------------------------------------------------------------ #

# Generate simulation-based DHARMa residual diagnostics
source(here::here("04_MODEL/functions/compute_dharma_residuals.R"))


# ------------------------------------------------------------------------------ #
#### 8. ASSESS MODEL GOODNESS-OF FIT ####
# ------------------------------------------------------------------------------ #

# Compute conditional AIC and delta cAIC for model comparison
source(here::here("04_MODEL/functions/compute_model_caic.R"))

# Compute coefficient of determination (R²) between observations and predictions
source(here::here("04_MODEL/functions/compute_R_squared.R"))


# ------------------------------------------------------------------------------ #
#### 9. PERFORM REPEATED RANDOM CROSS-VALIDATION ####
# ------------------------------------------------------------------------------ #

# Define the number of repetitions and folds
n_repeats <- 10
k_folds <- 10

# Run repeated random cross-validation for avaluating model predictive performance
source(here::here("04_MODEL/functions/run_repeated_random_cross_validation.R"))



# ------------------------------------------------------------------------------ #
####  SPATIO TEMPORAL BLOCKED CROSS-VALIDATION #### 
# ------------------------------------------------------------------------------ #

source(here::here("04_MODEL/functions/run_block_cross_validation.R"))



# ------------------------------------------------------------------------------ #
#### 10. GENERATE REGIONAL PREDICTION GRIDS ####
# ------------------------------------------------------------------------------ #

# Define prediction grid resolution in kilometres
res_km <- 5

# Generate prediction grids for the selected regions
source(here::here("04_MODEL/functions/create_prediction_grid.R"))


# ------------------------------------------------------------------------------ #
#### 11. GENERATE MODEL PREDICTIONS AND BIOMASS MAPS ####
# ------------------------------------------------------------------------------ #

# Predict converged models and compute bias-corrected abundance indices
source(here::here("04_MODEL/functions/predict_converged_models.R"))

# Generate spatial and spatiotemporal prediction maps
source(here::here("04_MODEL/functions/map_model_predictions.R"))



# ------------------------------------------------------------------------------ #
#### 12. ESTIMATE AND MAP PREDICTION UNCERTAINTY ####
# ------------------------------------------------------------------------------ #

# Compute simulation-based coefficients of variation
source(here::here("04_MODEL/functions/compute_simulated_coefficient_variation.R"))

# Generate annual maps of prediction uncertainty
source(here::here("04_MODEL/functions/plot_simulated_coefficient_variation.R"))


# ------------------------------------------------------------------------------ #
#### 13. GENERATE THE MODELLING REPORT ####
# ------------------------------------------------------------------------------ #

# Render the HTML report containing model results and diagnostics
rmarkdown::render(input = here::here("04_MODEL/report/sdm_report.Rmd"),
                  output_dir = new_repertory_model,
                  output_file = paste0(sp_name_safe, "_report", ".html"))

