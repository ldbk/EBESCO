
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
# sp_scientific <- "Zeus faber"                  # John Dory / Saint-Pierre
sp_scientific <- "Clupea harengus"             # Atlantic herring / Hareng de l’Atlantique
# sp_scientific <- "Solea solea"                 # Common sole / Sole commune


## Define domain
# ----------------#

region_validity_prop_presence <- readRDS(here("01_DATA/species_region_validity.rds"))%>%
  filter(scientificName == sp_scientific)

West_English_Channel = isTRUE(region_validity_prop_presence$west[1])
East_English_Channel = isTRUE(region_validity_prop_presence$east[1])

# ------------------------------------------------------------------------------#
####  LOAD AND TRANSFORM DATA  #### 
# ------------------------------------------------------------------------------#

source(here('03_TRANSFORM_DATA/transform_data.R'))


# ------------------------------------------------------------------------------#
####  COMPUTE AND PLOT MORAN's INDEX = SPATIAL AUTOCORRELATION  #### 
# ------------------------------------------------------------------------------#

source(here('04_MODEL/functions/compute_plot_Moran.R'))

# ------------------------------------------------------------------------------#
####  PLOT RESIDUALS OF MODEL FITTED WITHOUT RANDOM FIELD #### 
# ------------------------------------------------------------------------------#

source(here('04_MODEL/functions/compute_plot_residuals_withoutRF.R'))

# ------------------------------------------------------------------------------#
####  CREATE THE MESH  #### 
# ------------------------------------------------------------------------------#

source(here('04_MODEL/functions/create_mesh.R'))


# ------------------------------------------------------------------------------#
####  BUILD & RUN MODELS  #### 
# ------------------------------------------------------------------------------#

# Vector of response variables to be tested sequentially
responses <- c("totalWeightKg", "densityKgKm2")

# Fixed-effects
fixed_effect <- "1"

# Define the candidate families for the different response variables 
families_by_response <- list(
  # Candidate distributions for total biomass
  totalWeightKg = list(
    tweedie = sdmTMB::tweedie(link = "log"),
    deltagamma = delta_gamma(link1 = "logit", link2 = "log"),
    deltalognormal = delta_lognormal(link1 = "logit", link2 = "log"),
    deltagammapoissonlink = delta_gamma(type = "poisson-link")#, 
    # lognormal = lognormal(link = "log"),
    # gamma = Gamma(link = "log")
  ),
  # Candidate distributions for biomass density
  densityKgKm2 = list(
    tweedie = sdmTMB::tweedie(link = "log"),
    deltagamma = delta_gamma(link1 = "logit", link2 = "log"),
    deltalognormal = delta_lognormal(link1 = "logit", link2 = "log")#,
    # lognormal = lognormal(link = "log"),
    # gamma = Gamma(link = "log")
  )
)

source(here("04_MODEL/functions/fit_candidate_models.R"))

source(here("04_MODEL/functions/sanity_filter_models.R"))

source(here("04_MODEL/functions/extract_sdmTMB_params.R"))


# ------------------------------------------------------------------------------#
####  COMPUTE ANALYTICAL RANDOMIZED QUANTILE RESIDUALS  #### 
# ------------------------------------------------------------------------------#

source(here("04_MODEL/functions/analytical_randomized_quantile_residuals.R"))


# ------------------------------------------------------------------------------#
####  SIMULATION BASED ANALYTICAL RANDOMIZED QUANTILE RESIDUALS  #### 
# ------------------------------------------------------------------------------#

source(here("04_MODEL/functions/simulation_based_randomized_quantile_residuals.R"))


# ------------------------------------------------------------------------------#
####  AIC weights  #### 
# ------------------------------------------------------------------------------#

# https://doi.org/10.1093/icesjms/fsaf040
source(here("04_MODEL/functions/AIC_weights.R"))


# ------------------------------------------------------------------------------#
####  RANDOM CROSS-VALIDATION #### 
# ------------------------------------------------------------------------------#

source(here("04_MODEL/functions/cross_validation_random.R"))


# ------------------------------------------------------------------------------#
####  SPATIO TEMPORAL BLOCKED CROSS-VALIDATION #### 
# ------------------------------------------------------------------------------#

source(here("04_MODEL/functions/cross_validation_spatiotemporal_block.R"))

# ------------------------------------------------------------------------------#
####  SAVE OUTPUTS ####  
# ------------------------------------------------------------------------------#

# REPORT 
sp_safe <- gsub("[^A-Za-z0-9_]", "_", sp_scientific)

base_dir = here("05_OUTPUTS", "model_diagnostics")

# Create species-specific directory
sp_dir <- file.path(base_dir, paste0("model_diagnostics_", sp_safe))
if (!dir.exists(sp_dir)) dir.create(sp_dir, recursive = TRUE)


source(here("04_MODEL/functions/save_model_diagnostics.R"))

save_model_diagnostics(
  sp_scientific = sp_scientific,
  sanity_by_region = sanity_by_region,
  residuals_by_region = residuals_by_region,
  dharma_by_region = dharma_by_region,
  AIC_by_region = AIC_by_region,
  randomCV_by_region = randomCV_by_region,
  blockedCV_by_region = blockedCV_by_region
)

# ------------------------------------------------------------------------------#
####  DESIGN THE PREDICTION GRID  ####
# ------------------------------------------------------------------------------#

# grid resolution in km
res_km = 5

source(here('04_MODEL/functions/design_grid.R'))



# ------------------------------------------------------------------------------#
####  PREDICT ON THE GRID ####
# ------------------------------------------------------------------------------#

source(here("04_MODEL/functions/predict_plot_models_valid.R"))


# ------------------------------------------------------------------------------#
####  SAVE ALL PLOTS INTO HTLM FILE ####
# ------------------------------------------------------------------------------#

rmarkdown::render(
  input = here("04_MODEL", "report", "report.Rmd"),
  output_dir = sp_dir,
  output_file = paste0(
    "report_", sp_safe, "_", format(Sys.time(), "%d-%b-%Y-%H.%M"), ".html"
  )
)


