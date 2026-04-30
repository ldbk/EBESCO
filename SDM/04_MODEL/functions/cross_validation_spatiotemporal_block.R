
# ==============================================================================#
# Spatiotemporal blocked cross-validation on convergent sdmTMB models
# ==============================================================================#

spatiotemp_blocked_CV <- function(converged_models, region_name) {
  
  if (region_name == "west") {
    data_CGFS <- data_CGFS_west
    mesh = mesh_by_region$west$mesh
    data_folds <- readRDS(here("01_DATA/folds_CV/west_3folds.rds"))
  } else if (region_name == "east") {
    data_CGFS <- data_CGFS_east
    mesh = mesh_by_region$east$mesh
    data_folds <- readRDS(here("01_DATA/folds_CV/east_4folds.rds"))
  }
  
  # Create the offset column (sdmTMB_cv() expects offset = "column name")
  data_CGFS <- data_CGFS %>%  mutate(logSweptAreaKm2 = log(sweptAreaKm2))
  
  # ------------------------------------------------------------------------------#
  # Spatio-temporal blocking
  # ------------------------------------------------------------------------------#

  data_CV <- data_CGFS %>%
    mutate(year = as.factor(year)) %>%
    left_join(data_folds %>% 
                mutate(year = as.factor(year))%>%
                dplyr::select(year, lon, lat, fold_id),
              by = c("year", "lon", "lat"))
  
  k_folds <- length(unique(data_CV$fold_id))

  # ------------------------------------------------------------------------------#
  # Run CV across validated models
  # ------------------------------------------------------------------------------#
  
  cv_outputs <- list()     # store the full sdmTMB_cv() output object for each response and family
  cv_scores <- list()      # store a summary tibble of performance metrics for each response and family
  
  
  models_by_response <- converged_models[[region_name]]
  
  for (response_name in names(models_by_response)) {
    
    # Extract the models for this response
    models_by_family <- models_by_response[[response_name]]
    
    for (family_name in names(models_by_family)) {
      
      # Retrieve the validated model corresponding to this family.
      fitted_model <- models_by_family[[family_name]]
      
      # Reuse the same formula and distribution family that were used to fit the validated model
      formula <- formula(fitted_model)
      distr_family <- fitted_model$family
      
      cat("\n Blocked cross-validation: ", region_name, " - ", response_name, " - ", family_name)
      
      # Look up whether this response needs an offset or not
      # offset_col <- offset_col_by_response[[response]]
      
      # Build the argument list for sdmTMB_cv() 
      sdmTMB_cv_args <- list(
        formula = formula,
        data = data_CV,
        mesh = mesh,
        family = distr_family,
        spatial = "on",
        time = "year",
        spatiotemporal = "iid",
        fold_ids = data_CV$fold_id,
        k_folds = k_folds
      )
      
      # If an offset column is defined for this response, add it to the argument list
      if (response_name  == "totalWeightKg") sdmTMB_cv_args$offset <- "logSweptAreaKm2"

      # Run cross-validation
      cv_fit <- tryCatch(do.call(sdmTMB::sdmTMB_cv, sdmTMB_cv_args),
                         error = function(e) e)
      
      # Store the cross-validation output indexed by response and family
      cv_outputs[[response_name]][[family_name]] <- cv_fit
      
      if (inherits(cv_fit, "error")) next
      
      obs <- cv_fit$data[[response_name]]     # observed response values
      pred <- cv_fit$data$cv_predicted        # cross-validated predictions
      
      # Save a tibble of summary metrics for this response-family model:
      # - sum_loglik: summed predictive log-likelihood 
      # - rmse: root mean squared error between observed and predicted values
      # - mae: mean absolute error between observed and predicted values
      cv_scores[[response_name]][[family_name]] <- tibble(
        region = region_name,
        response = response_name,
        family = family_name,
        sum_loglik = cv_fit$sum_loglik,
        rmse = sqrt(mean((obs - pred)^2, na.rm = TRUE)),
        mae = mean(abs(obs - pred), na.rm = TRUE))
      
    }
  }

  list(data_CV = data_CV,
       cv_outputs = cv_outputs,
       cv_scores = bind_rows(unlist(cv_scores, recursive = FALSE)))
  
}


# if (isTRUE(West_English_Channel)) {
#   
#   blockedCV_summary_west <- imap_dfr(blockedCV_by_region$west$cv_outputs,
#                                      function(resp_list, response) {
#                                        imap_dfr(resp_list, function(cv, family) {
#                                          tibble(response = response,
#                                                 family = family,
#                                                 n_folds_tot = length(cv$pdHess),
#                                                 n_converged = sum(cv$pdHess),
#                                                 pourcent_converged = 100 * mean(cv$pdHess),
#                                                 all_converged = isTRUE(cv$converged))
#                                        })
#                                      })
#   
# }
# 
# if (isTRUE(East_English_Channel)) {
#   
#   blockedCV_summary_east <- imap_dfr(blockedCV_by_region$east$cv_outputs,
#                                 function(resp_list, response) {
#                                   imap_dfr(resp_list, function(cv, family) {
#                                     tibble(response = response,
#                                            family = family,
#                                            n_folds_tot = length(cv$pdHess),
#                                            n_converged = sum(cv$pdHess),
#                                            pourcent_converged = 100 * mean(cv$pdHess),
#                                            all_converged = isTRUE(cv$converged))
#                                   })
#                                 })
#   
# }