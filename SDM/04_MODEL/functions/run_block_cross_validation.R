
# ============================================================================== #
# SPATIOTEMPORAL BLOCKED CROSS-VALIDATION OF CONVERGED MODELS
#
# Objectives:
# - Perform spatiotemporal blocked k-fold cross-validation on converged models.
# - Evaluate each model family, response variable and mesh configuration.
# - Assess predictive performance using log-likelihood, RMSE and MAE.
# ============================================================================== #

# ------------------------------------------------------------------------------ #
#### 1. DEFINE BLOCKED CROSS-VALIDATION FUNCTION ####
# ------------------------------------------------------------------------------ #

# Perform spatiotemporal blocked k-fold CV for all converged models in a region.
spatiotemp_blocked_CV <- function(region_name) {
  
  if (region_name == "west") {
    data_CGFS <- data_CGFS_west
    data_folds <- readRDS(here("01_DATA/folds_CV/west_3folds.rds"))
  } else if (region_name == "east") {
    data_CGFS <- data_CGFS_east
    data_folds <- readRDS(here("01_DATA/folds_CV/east_4folds.rds"))
  }
  
  # Retrieve converged models and spatial meshes for the selected region
  models_by_mesh <- converged_models[[region_name]]
  meshes_region <- meshes_by_region[[region_name]]
  
  # Calculate the log-transformed swept area for models using an offset
  data_CGFS <- data_CGFS %>%  mutate(logSweptAreaKm2 = log(sweptAreaKm2))
  
  # ------------------------------------------------------------------------------ #
  #### 1.1. ASSIGN PREDEFINED FOLDS TO SURVEY DATA ####
  # ------------------------------------------------------------------------------ #
  
  # Assign predefined spatiotemporal folds to survey observations
  data_CV <- data_CGFS %>%
    mutate(year = as.factor(year)) %>%
    left_join(data_folds %>% 
                mutate(year = as.factor(year))%>%
                dplyr::select(year, lon, lat, fold_id),
              by = c("year", "lon", "lat"))
  
  # Keep the number of predefined folds
  k_folds <- length(unique(data_CV$fold_id))
  
  # Initialise nested lists for CV fits and scores 
  cv_outputs <- list()
  cv_scores <- list()
  
  # ------------------------------------------------------------------------------ #
  #### 1.2. CROSS-VALIDATION OF CONVERGED MODELS ####
  # ------------------------------------------------------------------------------ #
  
  # Loop over mesh configurations
  for (mesh_name in names(models_by_mesh)) {
    
    models_by_response <- models_by_mesh[[mesh_name]]
    
    cv_outputs[[mesh_name]] <- list()
    cv_scores[[mesh_name]] <- list()
    
    # Loop over response variables  
    for (response_name in names(models_by_response)) {
      
      # Extract the models for this response
      models_by_family <- models_by_response[[response_name]]
      
      cv_outputs[[mesh_name]][[response_name]] <- list()
      cv_scores[[mesh_name]][[response_name]] <- list()
      
      # Loop over model distribution families/likelihoods
      for (family_name in names(models_by_family)) {
        
        # Retrieve the validated model corresponding to this family.
        fitted_model <- models_by_family[[family_name]]
        
        # Reuse the same formula and distribution family that were used to fit the validated model
        model_formula <- formula(fitted_model)
        distr_family <- fitted_model$family
        mesh <- meshes_by_region[[region_name]][[mesh_name]]$mesh
        
        cat("\n Blocked cross-validation: ", 
            region_name, " - ", mesh_name, "-",
            response_name, " - ", family_name)
        
        # ---------------------------------------------------------------------- #
        #### 1.3. FIT MODELS USING SPATIOTEMPORAL BLOCKED FOLDS ####
        # ---------------------------------------------------------------------- #
        
        # Build the argument list for sdmTMB_cv() 
        sdmTMB_cv_args <- list(
          formula = model_formula,
          data = data_CV,
          mesh = mesh,
          family = distr_family,
          spatial = "on",
          time = "year",
          spatiotemporal = "iid",
          fold_ids = data_CV$fold_id,
          k_folds = k_folds
        )
        
        # Apply a swept-area offset to total catch weight models
        if (response_name  == "totalWeightKg") sdmTMB_cv_args$offset <- "logSweptAreaKm2"
        
        # Run cross-validation
        # Fit the CV model while capturing errors without stopping the loop
        cv_fit <- tryCatch(do.call(sdmTMB::sdmTMB_cv, sdmTMB_cv_args),
                           error = function(e) e)
        
        # Store the cross-validation output indexed by response and family
        cv_outputs[[mesh_name]][[response_name]][[family_name]] <- cv_fit
        
        # -------------------------------------------------------------------- #
        #### 1.4. RECORD FAILED CROSS-VALIDATION RUNS ####
        # -------------------------------------------------------------------- #
        
        # Record failed CV runs and continue to the next repetition
        if (inherits(cv_fit, "error")) {
          
          cv_scores[[mesh_name]][[response_name]][[family_name]][[rep_id]] <- tibble(
            region = region_name,
            mesh = mesh_name,
            response = response_name,
            family = family_name,
            repeat_id = rep_id,
            sum_loglik = NA,
            rmse = NA,
            mae = NA,
            n_folds = NA,
            n_converged = NA,
            all_converged = FALSE,
            error = cv_fit$message
          )
          
          next
        }
        
        # ---------------------------------------------------------------------- #
        #### 1.5. CALCULATE PREDICTIVE PERFORMANCE METRICS ####
        # ---------------------------------------------------------------------- #
        
        # Extract observed and out-of-fold predicted values
        obs <- cv_fit$data[[response_name]]
        pred <- cv_fit$data$cv_predicted
        
        # Store individual CV predictions for global performance assessment
        # Save a tibble of summary metrics for this response-family model:
        # - sum_loglik: summed predictive log-likelihood 
        # - rmse: root mean squared error between observed and predicted values
        # - mae: mean absolute error between observed and predicted values
        cv_scores[[mesh_name]][[response_name]][[family_name]] <- tibble(
          region = region_name,
          mesh = mesh_name,
          response = response_name,
          family = family_name,
          sum_loglik = cv_fit$sum_loglik,
          rmse = sqrt(mean((obs - pred)^2, na.rm = TRUE)),
          mae = mean(abs(obs - pred), na.rm = TRUE),
          n_folds = length(cv_fit$pdHess),
          n_converged = sum(cv_fit$pdHess, na.rm = TRUE),
          all_converged = isTRUE(cv_fit$converged))
      }
    }
  }
  
  
  # ------------------------------------------------------------------------------ #
  #### 2. COMBINE CROSS-VALIDATION RESULTS ####
  # ------------------------------------------------------------------------------ #
  cv_scores_df <- purrr::map_dfr(cv_scores, function(mesh_list) {
    purrr::map_dfr(mesh_list, function(response_list) {
      dplyr::bind_rows(response_list)
    })
  })
  
  list(data_CV = data_CV,
       cv_outputs = cv_outputs,
       cv_scores = cv_scores_df)
}


# ------------------------------------------------------------------------------ #
#### 3. RUN BLOCKED CROSS-VALIDATION BY REGION ####
# ------------------------------------------------------------------------------ #

blockCV_by_region <- list()

if (isTRUE(West_English_Channel)) {
  blockCV_by_region$west <- spatiotemp_blocked_CV("west")
}
if (isTRUE(East_English_Channel)) {
  blockCV_by_region$east <-  spatiotemp_blocked_CV("east")
}

# ------------------------------------------------------------------------------ #
#### 4. SAVE BLOCKED CROSS-VALIDATION RESULTS ####
# ------------------------------------------------------------------------------ #

saveRDS(blockCV_by_region,
        file.path(new_repertory_model, paste0(sp_name_safe, "_blockCV.rds")))

