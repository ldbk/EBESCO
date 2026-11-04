
# ============================================================================== #
# COMPUTE R-SQUARED FOR CONVERGED SDMTMB MODELS
#
# Objectives:
# - Calculate Pearson's squared correlation (R2) for all converged models.
# - Assess the correlation between observed and predicted values.
# - Compute p-values and statistical significance for each model.
# ============================================================================== #

# ------------------------------------------------------------------------------ #
#### 1. DEFINE R-SQUARED COMPUTATION FUNCTIONS ####
# ------------------------------------------------------------------------------ #

# Extract fitted predictions from an sdmTMB model
get_pred <- function(fit) {
  predict(fit, newdata = fit$data, type = "response", model = NA)$est
}


# Compute Pearson's squared correlation and associated significance
compute_R2 <- function(fit, region, mesh, response, family) {
  
  # Extract observed and predicted values
  obs  <- fit$data[[response]]
  pred <- get_pred(fit)
  
  # Compute Pearson correlation test
  test <- cor.test(obs, pred)
  
  # Store model performance metrics
  R2_df <- data.frame(region = region,
                      mesh = mesh,
                      response = response,
                      family = family,
                      R2 = unname(test$estimate)^2,
                      p_value = test$p.value,
                      significant = test$p.value < 0.05)
  
  return(R2_df)
}


# Compute R2 for each mesh, response variable and model family.
# Return a single data frame containing all model performance metrics.
compute_R2_by_region <- function(region_name) {
  
  # Retrieve converged models for the selected region
  models_by_mesh <- converged_models[[region_name]]
  
  # Initialise regional R2 results
  R2_list <- list()
  
  # Loop over mesh configurations
  for (mesh_name in names(models_by_mesh)) {
    
    models_by_response <- models_by_mesh[[mesh_name]]
    
    # Loop over response variables
    for (response_name in names(models_by_response)) {
      
      models_by_family <- models_by_response[[response_name]]
      
      message("Computing R2: ",
              region_name, " | ", mesh_name, " | ", response_name)
      
      # Compute R2 for each model family
      R2_df <- dplyr::bind_rows(
        lapply(names(models_by_family), function(family_name) {
          
          compute_R2(fit = models_by_family[[family_name]],
                     region = region_name,
                     mesh = mesh_name,
                     response = response_name,
                     family = family_name)
        })
      )
      
      # Store model performance metrics
      R2_list[[paste(mesh_name, response_name, sep = "_")]] <- R2_df
      
    }
  }
  
  # Combine all model configurations within the region
  R2_df_all <- dplyr::bind_rows(R2_list)
  
  # Return R2 model performance results
  return(R2_df_all)
}


# ------------------------------------------------------------------------------ #
#### 2. COMPUTE R-SQUARED BY REGION ####
# ------------------------------------------------------------------------------ #

# Initialise regional R2 results
R2_by_region <- list()

# Compute R2 for the Western English Channel
if (isTRUE(West_English_Channel)) {
  R2_by_region$west <- compute_R2_by_region("west")
}

# Compute R2 for the Eastern English Channel
if (isTRUE(East_English_Channel)) {
  R2_by_region$east <- compute_R2_by_region("east")
}


# ------------------------------------------------------------------------------ #
#### 3. SAVE R-SQUARED RESULTS ####
# ------------------------------------------------------------------------------ #

# Save R2 values and significance metrics by region
saveRDS(R2_by_region,
        file.path(new_repertory_model,
                  paste0(sp_name_safe, "_R2.rds")))



