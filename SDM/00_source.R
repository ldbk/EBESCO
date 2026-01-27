
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

region_validity_prop_presence <- readRDS(here("05_OUTPUTS/model_diagnostics/model_diagnostics_Chelidonichthys_cuculus/cross_validation_by_region.rds"))


# ------------------------------------------------------------------------------#
####  DEFINE PARAMETERS  #### 
# ------------------------------------------------------------------------------#

## Define species
# ----------------#
# sp_scientific <- "Capros aper"               # boarfish / bogue, poisson sanglier
# sp_scientific <- "Chelidonichthys cuculus"   # red gurnard / grondin rouge
# sp_scientific <- "Chelidonichthys lucerna"   # tub gurnard / grondin perlon
# sp_scientific <- "Conger conger"             # conger / congre
# sp_scientific <- "Dicentrarchus labrax"      # seabass / bar
# sp_scientific <- "Engraulis encrasicolus"    # anchovy / anchois
# sp_scientific <- "Eutrigla gurnardus"        # grey gurnard / grondin gris
# sp_scientific <- "Merluccius merluccius"     # hake / merlu
# sp_scientific <- "Micromesistius poutassou"  # blue whiting / merlan bleu
sp_scientific <- "Solea solea"               # sole / sole
# sp_scientific <- "Trachurus trachurus"       # horse mackerel / chinchard (commun)
# sp_scientific <- "Zeus faber"                # John dory / Saint-Pierre

## Define domain
# ----------------#

region_validity_prop_presence <- readRDS(here("01_DATA/region_validity_prop_presence.rds")) %>%
  filter(scientificName == sp_scientific)

East_English_Channel = isTRUE(region_validity_prop_presence$east[1])
West_English_Channel = isTRUE(region_validity_prop_presence$west[1])

# East_English_Channel = TRUE
# West_English_Channel = TRUE 

# ------------------------------------------------------------------------------#
####  LOAD AND TRANSFORM DATA  #### 
# ------------------------------------------------------------------------------#

source(here('03_TRANSFORM_DATA/transform_data.R'))


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
    deltagammapoisson = delta_gamma(type = "poisson-link")
  ),
  # Candidate distributions for biomass density
  densityKgKm2 = list(
    tweedie = sdmTMB::tweedie(link = "log"),
    deltagamma = delta_gamma(link1 = "logit", link2 = "log"),
    deltalognormal = delta_lognormal(link1 = "logit", link2 = "log")
  )
)

source(here("04_MODEL/functions/fit_candidate_models.R"))

source(here("04_MODEL/functions/sanity_filter_models.R"))

# View(sanity_by_region)


# ------------------------------------------------------------------------------#
####  COMPUTE ANALYTICAL RANDOMIZED QUANTILE RESIDUALS  #### 
# ------------------------------------------------------------------------------#

source(here("04_MODEL/functions/analytical_randomized_quantile_residuals.R"))


# ------------------------------------------------------------------------------#
####  SIMULATION BASED ANALYTICAL RANDOMIZED QUANTILE RESIDUALS  #### 
# ------------------------------------------------------------------------------#

# source(here("04_MODEL/functions/simulation_based_randomized_quantile_residuals.R"))


# ------------------------------------------------------------------------------#
####  AIC weights  #### 
# ------------------------------------------------------------------------------#

# https://doi.org/10.1093/icesjms/fsaf040
source(here("04_MODEL/functions/AIC_weights.R"))


# ------------------------------------------------------------------------------#
####  CROSS-VALIDATION #### 
# ------------------------------------------------------------------------------#

source(here("04_MODEL/functions/cross_validation.R"))

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
  # dharma_by_region = dharma_by_region,
  AIC_by_region = AIC_by_region,
  cross_validation_by_region = cross_validation_by_region
)

# ------------------------------------------------------------------------------#
####  DESIGN THE PREDICTION GRID  ####
# ------------------------------------------------------------------------------#

# grid resolution in km
res_km = 6

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

