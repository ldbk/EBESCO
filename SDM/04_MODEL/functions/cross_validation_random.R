
# ==============================================================================#
# Random cross-validation on convergent sdmTMB models
# ==============================================================================#


random_CV <- function(converged_models, region_name) {
  
  if (region_name == "west") {
    data_CGFS <- data_CGFS_west
    mesh = mesh_by_region$west$bspde
  } else if (region_name == "east") {
    data_CGFS <- data_CGFS_east
    mesh = mesh_by_region$east$bspde
  }
  
  # Create the offset column (sdmTMB_cv() expects offset = "column name")
  data_CGFS <- data_CGFS %>%  mutate(logSweptAreaKm2 = log(sweptAreaKm2))
  
  k_folds = 10
  set.seed(123) 
  
  # Randomly assign each observation (row) to one fold ID
  data_CGFS$fold_ids <- sample(seq_len(k_folds), size = nrow(data_CGFS), replace = TRUE)
  
  cv_outputs <- list()     # store the full sdmTMB_cv() output object for each response and family
  cv_scores <- list()      # store a summary tibble of performance metrics for each response and family
  
  models_by_response <- converged_models[[region_name]]
  
  
  for (response_name in names(models_by_response)) {
    
    # Extract the models that passed your validation step for this response
    models_by_family <- models_by_response[[response_name]]
    
    for (family_name in names(models_by_family)) {
      
      fitted_model <- models_by_family[[family_name]]
      formula <- formula(fitted_model)
      distr_family <- fitted_model$family
      
      message("Cross-validation: ", region_name, " - ", response_name, " - ", family_name)
      
      # Build the argument list for sdmTMB_cv() 
      sdmTMB_cv_args <- list(
        formula = formula,
        data = data_CGFS,
        mesh = mesh,
        family = distr_family,
        spatial = "on",
        time = "year",
        spatiotemporal = "iid",
        fold_ids = data_CGFS$fold_ids,
        k_folds = k_folds
      )
      
      # If an offset column is defined for this response, add it to the argument list
      if (response_name == "totalWeightKg") sdmTMB_cv_args$offset <- "logSweptAreaKm2"

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
      cv_scores[[response_name]][[family_name]] <- tibble::tibble(
        region = region_name,
        response = response_name,
        family = family_name,
        sum_loglik = cv_fit$sum_loglik,
        rmse = sqrt(mean((obs - pred)^2, na.rm = TRUE)),
        mae  = mean(abs(obs - pred), na.rm = TRUE)
      )
    }
  }

  list(cv_outputs = cv_outputs, 
       cv_scores = bind_rows(unlist(cv_scores, recursive = FALSE)))
}


randomCV_by_region <- list()

if (isTRUE(West_English_Channel)) {
  randomCV_by_region$west <- random_CV(converged_models, "west")
}

if (isTRUE(East_English_Channel)) {
  randomCV_by_region$east <-random_CV(converged_models, "east")
}




# if (isTRUE(East_English_Channel)) {
#   randomCV_summary_east <- imap_dfr(randomCV_by_region$east$cv_outputs,
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
# }
# 
# 
# if (isTRUE(West_English_Channel)) {
#   randomCV_summary_west <- imap_dfr(randomCV_by_region$west$cv_outputs,
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
# }
