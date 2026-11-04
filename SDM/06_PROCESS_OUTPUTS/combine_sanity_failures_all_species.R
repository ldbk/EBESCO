# ============================================================================== #
#### EXTRACT AND COMBINE FAILED SANITY CHECKS ####
# ============================================================================== #
# 
# This script combines model convergence sanity checks across species for the
# eastern and western study regions.
#
# For each species, it:
# - Searches recursively for the latest convergence_sanity_results.rds file
#   within the species-specific dated output folders.
# - Extracts failed sanity checks for each cutoff, response and model family.
# - Creates binary indicators for each check (0 = failed, 1 = no failure recorded).
# - Identifies combinations of failed checks for each model.
#
# Only models with at least one failed check are retained.
#
# ============================================================================== #


# ------------------------------------------------------------------------------ #
#### 1. SELECT SPECIES AND SET PARAMETERS ####
# ------------------------------------------------------------------------------ #

# Species included in the eastern region
species_east <- c("Chelidonichthys_cuculus",
                  "Chelidonichthys_lucerna",
                  "Conger_conger",
                  "Dicentrarchus_labrax",
                  "Hippocampus_hippocampus",
                  "Solea_solea",
                  "Trachurus_trachurus",
                  "Zeus_faber")

# Species included in the western region
species_west <- c("Capros_aper",
                  "Chelidonichthys_cuculus",
                  "Conger_conger",
                  "Dicentrarchus_labrax",
                  "Engraulis_encrasicolus",
                  "Micromesistius_poutassou",
                  "Trachurus_trachurus",
                  "Zeus_faber")

# Input and output directories
output_dir <- here::here("05_OUTPUTS")
metrics_dir <- file.path(output_dir, "COMBINED_METRICS")
dir.create(metrics_dir, recursive = TRUE, showWarnings = FALSE)

# Individual sanity checks (excluding the overall all_ok indicator)
cols_fail <- c("hessian_ok",
               "eigen_values_ok",
               "nlminb_ok",
               "range_ok",
               "gradients_ok",
               "se_magnitude_ok",
               "se_na_ok",
               "sigmas_ok")


# ------------------------------------------------------------------------------ #
#### 2. EXTRACT FAILED SANITY CHECKS ####
# ------------------------------------------------------------------------------ #

extract_sanity_failed <- function(species_name, region_name) {
  
  # Find all matching files in the species-specific output folders
  files <- list.files(
    file.path(output_dir, species_name),
    pattern = paste0("^", species_name, "_convergence_sanity_results\\.rds$"),
    recursive = TRUE,
    full.names = TRUE
  )
  
  if (!length(files)) {
    message("Missing file: ", species_name)
    return(NULL)
  }
  
  # Load the latest file containing sanity results
  sanity <- readRDS(files[which.max(file.info(files)$mtime)])
  
  # Select the requested region
  region_obj <- sanity[[region_name]]
  
  if (is.null(region_obj)) {
    message("Missing region: ", species_name, " - ", region_name)
    return(NULL)
  }
  
  # Extract failed checks for each cutoff, response and model family
  imap_dfr(region_obj, function(cutoff_obj, cutoff_name) {
    imap_dfr(cutoff_obj, function(response_obj, response_name) {
      imap_dfr(response_obj$models_sanity_failed, function(checks, family_name) {
        
        # Keep only checks explicitly equal to FALSE
        failed_tests <- cols_fail[map_lgl(cols_fail, ~ identical(checks[[.x]], FALSE))]
        
        tibble(species = species_name,
               region = region_name,
               mesh = cutoff_name,
               response = response_name,
               family = family_name,
               failed_test = failed_tests)
      })
    })
  })
}


# ------------------------------------------------------------------------------ #
#### 3. FORMAT SANITY RESULTS ####
# ------------------------------------------------------------------------------ #

combine_sanity <- function(species_list, region_name) {
  
  # Combine failed checks across the selected species
  failed <- map_dfr(species_list, ~ extract_sanity_failed(.x, region_name))
  
  if (!nrow(failed)) return(failed)
  
  # Convert failed checks into binary columns (0 = failed, 1 = no failure recorded)
  results <- failed %>%
    mutate(value = 0L) %>%
    pivot_wider(id_cols = c(species, region, mesh, response, family),
                names_from = failed_test,
                values_from = value,
                values_fill = 1L)
  
  # Add check columns that are absent from the results
  for (col in cols_fail) {
    if (!col %in% names(results)) results[[col]] <- 1L
  }
  
  # Identify the combination of failed checks for each model
  results %>%
    rowwise() %>%
    mutate(pattern = paste(sub("_ok$", "", cols_fail[c_across(all_of(cols_fail)) == 0]),
                           collapse = " + ")) %>%
    ungroup() %>%
    dplyr::select(species, region, mesh, response, family, all_of(cols_fail), pattern)
}


# ------------------------------------------------------------------------------ #
#### 4. COMBINE RESULTS SEPARATELY ####
# ------------------------------------------------------------------------------ #

# Eastern region
sanity_east <- combine_sanity(species_east, "east")

# Western region
sanity_west <- combine_sanity(species_west, "west")


# ------------------------------------------------------------------------------ #
#### 5. SAVE RESULTS SEPARATELY ####
# ------------------------------------------------------------------------------ #

# Create regional output directories if needed
dir.create(file.path(metrics_dir, "EAST"),
           recursive = TRUE,
           showWarnings = FALSE)

dir.create(file.path(metrics_dir, "WEST"),
           recursive = TRUE,
           showWarnings = FALSE)

# Save eastern region results
saveRDS(sanity_east, file.path(metrics_dir, "EAST", "sanity_all_species.rds"))

# Save western region results
saveRDS(sanity_west, file.path(metrics_dir, "WEST", "sanity_all_species.rds"))

