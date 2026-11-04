# ============================================================================== #
# COMPUTE CONDITIONAL AKAIKE INFORMATION CRITERION FOR CONVERGED SDMTMB MODELS
#
# Objectives:
# - Calculate conditional AIC (cAIC) for all converged models.
# - Compare model families within each region, mesh and response variable.
# - Compute delta cAIC to assess relative model support.
# - Rank models by cAIC for model selection.
# - Combine all model comparisons into a single data frame.
# ============================================================================== #


# ------------------------------------------------------------------------------ #
#### 1. DEFINE CONDITIONAL AIC COMPUTATION FUNCTION ####
# ------------------------------------------------------------------------------ #

# Compute cAIC and delta cAIC for each mesh and response variable.
# Return a nested list containing one comparison table per response.
compute_cAIC <- function(region_name) {
  
  # Retrieve converged models for the selected region
  models_by_mesh <- converged_models[[region_name]]
  
  # Initialise regional cAIC results
  cAIC_list <- list()
  
  # Loop over mesh configurations
  for (mesh_name in names(models_by_mesh)) {
    
    models_by_response <- models_by_mesh[[mesh_name]]
    
    # Loop over response variables
    for (response_name in names(models_by_response)) {
      
      models_by_family <- models_by_response[[response_name]]
      
      # Skip responses without converged models
      if (is.null(models_by_family) || length(models_by_family) == 0) {
        message("Skipping cAIC (no converged models): ",
                region_name, " | ", mesh_name, " | ", response_name)
        next
      }
      message("Computing cAIC: ", 
              region_name, " | ", mesh_name, " | ", response_name)
      
      # Compute conditional AIC for each model
      cAIC_values <- vapply(models_by_family,
                            function(current_model) {
                              val <- sdmTMB::cAIC(current_model, what = "cAIC")
                              as.numeric(val)
                            },
                            numeric(1))
      
      
      # Store model comparison metrics
      cAIC_df <- data.frame(region = region_name,
                            mesh = mesh_name,
                            response = response_name,
                            family = names(cAIC_values),
                            cAIC = as.numeric(cAIC_values))
      
      cAIC_list[[paste(mesh_name, response_name, sep = "_")]] <- cAIC_df
      
    }
  }
  
  
  # Combine all model configurations within the region
  cAIC_df_all <- dplyr::bind_rows(cAIC_list)
  
  # Compute delta cAIC across all meshes and families
  # within each response variable
  cAIC_df_all <- cAIC_df_all %>%
    dplyr::group_by(region, response) %>%
    dplyr::mutate(deltacAIC = cAIC - min(cAIC, na.rm = TRUE),
                  rank = dplyr::min_rank(cAIC)) %>%
    dplyr::arrange(region, response, rank) %>%
    dplyr::ungroup()
  
  # Return conditional AIC model comparison results
  return(cAIC_df_all)
}



# ------------------------------------------------------------------------------ #
#### 2. COMPUTE CONDITIONAL AIC BY REGION ####
# ------------------------------------------------------------------------------ #

# Initialise regional conditional AIC results
cAIC_by_region <- list()

# Compute cAIC for the Western English Channel
if (isTRUE(West_English_Channel)) {
  cAIC_by_region$west <- compute_cAIC("west")
}
# Compute cAIC for the Eastern English Channel
if (isTRUE(East_English_Channel)) {
  cAIC_by_region$east <- compute_cAIC("east")
}


# ------------------------------------------------------------------------------ #
#### 3. SAVE CONDITIONAL AIC RESULTS ####
# ------------------------------------------------------------------------------ #

# Save conditional AIC values and differences by region
saveRDS(cAIC_by_region,
        file.path(new_repertory_model, 
                  paste0(sp_name_safe, "_conditional_AIC.rds")))


