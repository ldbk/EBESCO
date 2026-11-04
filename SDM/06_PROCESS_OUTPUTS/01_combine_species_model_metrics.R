# ============================================================================== #
# EXTRACT AND COMBINE MODEL EVALUATION METRICS
# ============================================================================== #
#
# This script extracts and combines model evaluation metrics across species
# and study regions, using the latest available saved SDM outputs.
#
# For each species and region, metrics are compiled for all available model
# configurations, defined by mesh cutoff, response variable and distribution
# family.
#
# The following metrics are processed:
#   1. Repeated random cross-validation (scores and convergence)
#   2. Coefficient of variation in predictions
#   3. Conditional Akaike Information Criterion (cAIC)
#   4. Squared correlation between observed and fitted values (R2)
#   5. Block cross-validation (scores and convergence)
#
# Results are combined separately for the eastern and western regions.
# Missing files or model outputs are skipped with an informative message.
#
# Outputs:
#   - metrics$east: combined evaluation metrics for the eastern region
#   - metrics$west: combined evaluation metrics for the western region
#
# ============================================================================== #


# ------------------------------------------------------------------------------ #
#### 1. SELECT SPECIES AND SET PARAMETERS ####
# ------------------------------------------------------------------------------ #

species_by_region <- list(
  east = c("Chelidonichthys_cuculus",
           "Chelidonichthys_lucerna",
           "Conger_conger",
           "Dicentrarchus_labrax",
           "Hippocampus_hippocampus",
           "Solea_solea",
           "Trachurus_trachurus",
           "Zeus_faber"),
  west = c("Capros_aper",
           "Chelidonichthys_cuculus",
           "Conger_conger",
           "Dicentrarchus_labrax",
           "Engraulis_encrasicolus",
           "Micromesistius_poutassou",
           "Trachurus_trachurus",
           "Zeus_faber"))

# Input and output directories
output_dir <- here::here("05_OUTPUTS")
metrics_dir <- file.path(output_dir, "COMBINED_METRICS")

dir.create(metrics_dir, recursive = TRUE, showWarnings = FALSE)


# ------------------------------------------------------------------------------ #
#### 2. DEFINE FILE LOADING FUNCTION ####
# ------------------------------------------------------------------------------ #

# Load the latest available file for a given species and output type
load_latest_output <- function(species, file_suffix) {
  
  species_dir <- file.path(output_dir, species)
  
  if (!dir.exists(species_dir)) {
    message("Species directory unavailable: ", species)
    return(NULL)
  }
  
  files <- list.files(species_dir,
                      pattern = paste0("^", species, "_", file_suffix, "\\.rds$"),
                      recursive = TRUE,
                      full.names = TRUE)
  
  if (!length(files)) {
    message("File unavailable: ", species, " - ", file_suffix)
    return(NULL)
  }
  
  latest_file <- files[which.max(file.info(files)$mtime)]
  
  readRDS(latest_file)
}


# ------------------------------------------------------------------------------ #
#### 3. DEFINE COEFFICIENT OF VARIATION EXTRACTION FUNCTION ####
# ------------------------------------------------------------------------------ #

# Extract coefficient of variation in predictions
extract_coeff_var <- function(coeff_var, species, region) {
  
  purrr::imap_dfr(coeff_var, function(by_response, mesh) {
    purrr::imap_dfr(by_response, function(by_family, response) {
      purrr::imap_dfr(by_family, function(df, family) {
        
        dplyr::mutate(as.data.frame(df),
                      species = species,
                      region = region,
                      mesh = mesh,
                      response = response,
                      family = family)
      })
    })
  })
}


# ------------------------------------------------------------------------------ #
#### 4. COMBINE MODEL EVALUATION METRICS ####
# ------------------------------------------------------------------------------ #

# Initialise combined metrics
metrics <- list()


# 4.1. REPEATED RANDOM CROSS-VALIDATION
# ------------------------------------------------------------------------------ #

metrics$randomCV <- list()

for (region in names(species_by_region)) {
  
  results <- lapply(species_by_region[[region]], function(species) {
    
    obj <- load_latest_output(species, "repeated_randomCV")
    
    if (is.null(obj) || is.null(obj[[region]]$cv_scores)) return(NULL)
    
    dplyr::mutate(obj[[region]]$cv_scores,
                  species = species,
                  region = region)
  })
  
  metrics$randomCV[[region]] <- dplyr::bind_rows(results)
}


# 4.2. COEFFICIENT OF VARIATION IN PREDICTIONS
# ------------------------------------------------------------------------------ #

metrics$coeff_var <- list()

for (region in names(species_by_region)) {
  
  results <- lapply(species_by_region[[region]], function(species) {
    
    obj <- load_latest_output(species, "coefficient_variation_predictions")
    
    if (is.null(obj) || is.null(obj[[region]])) return(NULL)
    
    extract_coeff_var(obj[[region]], species, region)
  })
  
  metrics$coeff_var[[region]] <- dplyr::bind_rows(results)
}


# 4.3. CONDITIONAL AKAIKE INFORMATION CRITERION
# ------------------------------------------------------------------------------ #

metrics$cAIC <- list()

for (region in names(species_by_region)) {
  
  results <- lapply(species_by_region[[region]], function(species) {
    
    obj <- load_latest_output(species, "conditional_AIC")
    
    if (is.null(obj) || is.null(obj[[region]])) return(NULL)
    
    dplyr::mutate(obj[[region]],
                  species = species,
                  region = region)
  })
  
  metrics$cAIC[[region]] <- dplyr::bind_rows(results)
}


# 4.4. R-SQUARED
# ------------------------------------------------------------------------------ #

metrics$R2 <- list()

for (region in names(species_by_region)) {
  
  results <- lapply(species_by_region[[region]], function(species) {
    
    obj <- load_latest_output(species, "R2")
    
    if (is.null(obj) || is.null(obj[[region]])) return(NULL)
    
    dplyr::mutate(obj[[region]],
                  species = species,
                  region = region)
  })
  
  metrics$R2[[region]] <- dplyr::bind_rows(results)
}


# 4.5. BLOCKED CROSS-VALIDATION
# ------------------------------------------------------------------------------ #

metrics$blockCV <- list()

for (region in names(species_by_region)) {
  
  results <- lapply(species_by_region[[region]], function(species) {
    
    obj <- load_latest_output(species, "blockCV")
    
    if (is.null(obj) || is.null(obj[[region]])) return(NULL)
    
    output <- obj[[region]]
    
    if (is.null(output$cv_scores) ||
        is.null(output$cv_outputs)) return(NULL)
    
    convergence <- extract_blockCV_convergence(output$cv_outputs, species, region)
    
    scores <- dplyr::mutate(output$cv_scores,
                            species = species,
                            region = region)
    
    list(scores = dplyr::left_join(
      scores, 
      convergence,
      by = c("species", "region", "mesh", "response", "family")),
      convergence = convergence)
  })
  metrics$blockCV[[region]] <- list(
    scores = dplyr::bind_rows(lapply(results, `[[`, "scores")),
    convergence = dplyr::bind_rows(lapply(results, `[[`, "convergence"))
  )
}


# ------------------------------------------------------------------------------ #
#### 5. SAVE COMBINED METRICS ####
# ------------------------------------------------------------------------------ #

for (region in names(species_by_region)) {
  
  region_dir <- file.path(metrics_dir, toupper(region))
  dir.create(region_dir, recursive = TRUE, showWarnings = FALSE)
  
  for (metric_name in names(metrics)) {
    
    saveRDS(metrics[[metric_name]][[region]],
            file.path(region_dir, paste0(metric_name, "_all_species.rds")))
  }
  message("Combined metrics saved: ", region)
}


# ------------------------------------------------------------------------------ #
#### 6. ACCESS COMBINED METRICS ####
# ------------------------------------------------------------------------------ #

# Eastern region
metrics$randomCV$east
metrics$coeff_var$east
metrics$cAIC$east
metrics$R2$east
metrics$blockCV$east

# Western region
metrics$randomCV$west
metrics$coeff_var$west
metrics$cAIC$west
metrics$R2$west
metrics$blockCV$west
