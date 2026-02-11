
# ==============================================================================#
# Random cross-validation on convergent sdmTMB models
# ==============================================================================#


random_CV <- function(sanity_output, region, data_CGFS, mesh) {
  
  # Create the offset column (sdmTMB_cv() expects offset = "column name")
  data_CGFS <- data_CGFS %>%  mutate(logSweptAreaKm2 = log(sweptAreaKm2))
  
  k_folds = 10
  
  set.seed(123)     
  # Randomly assign each observation (row) to one fold ID
  fold_ids <- sample(seq_len(k_folds), size = nrow(data_CGFS), replace = TRUE)
  
  cv_outputs <- list()     # store the full sdmTMB_cv() output object for each response and family
  cv_scores <- list()      # store a summary tibble of performance metrics for each response and family
  
  
  for (response in names(sanity_output)) {
    
    # Extract the models that passed your validation step for this response
    models_valid <- sanity_output[[response]]$models_valid
    
    # If there are no valid models for this response, skip to the next response
    if (length(models_valid) == 0) next
    
    for (family_name in names(models_valid)) {
      
      # Retrieve the validated model corresponding to this family.
      valid_model_family <- models_valid[[family_name]]
      
      # Reuse the same formula and distribution family that were used to fit the validated model
      formula <- formula(valid_model_family)
      distr_family <- valid_model_family$family
      
      cat("\nCross-validation: ", region, " - ", response, " - ", family_name, "\n")
      
      # Look up whether this response needs an offset or not
      # offset_col <- offset_col_by_response[[response]]
      
      # Build the argument list for sdmTMB_cv() 
      sdmTMB_cv_args <- list(
        formula = formula,
        data = data_CGFS,
        mesh = mesh,
        family = distr_family,
        spatial = "on",
        time = "year",
        spatiotemporal = "iid",
        fold_ids = data_CGFS$fold_id,
        k_folds = k_folds
      )
      
      # If an offset column is defined for this response, add it to the argument list
      if (response == "totalWeightKg") sdmTMB_cv_args$offset <- "logSweptAreaKm2"
      # if (!is.null(offset_col)) sdmTMB_cv_args$offset <- offset_col  
      
      # Run cross-validation
      cv_fit <- tryCatch(do.call(sdmTMB::sdmTMB_cv, sdmTMB_cv_args),
                         error = function(e) e)
      
      # Store the cross-validation output indexed by response and family
      cv_outputs[[response]][[family_name]] <- cv_fit
      
      if (inherits(cv_fit, "error")) next
      
      obs <- cv_fit$data[[response]]          # observed response values
      pred <- cv_fit$data$cv_predicted        # cross-validated predictions
      
      # Save a tibble of summary metrics for this response-family model:
      # - sum_loglik: summed predictive log-likelihood 
      # - rmse: root mean squared error between observed and predicted values
      # - mae: mean absolute error between observed and predicted values
      cv_scores[[length(cv_scores) + 1]] <- tibble(
        region = region,
        response = response,
        family = family_name,
        sum_loglik = cv_fit$sum_loglik,
        rmse = sqrt(mean((obs - pred)^2, na.rm = TRUE)),
        mae = mean(abs(obs - pred), na.rm = TRUE))
      
    }
  }

  list(cv_outputs = cv_outputs,
       cv_scores = dplyr::bind_rows(cv_scores))
  
}


randomCV_by_region <- list()

if (isTRUE(East_English_Channel)) {
  randomCV_by_region$east <-  random_CV(sanity_output  = sanity_by_region$east,
                                                region = "east",
                                                data = data_CGFS_east,
                                                mesh = mesh_by_region$east$bspde)
}

if (isTRUE(West_English_Channel)) {
  randomCV_by_region$west <- random_CV(sanity_output  = sanity_by_region$west, 
                                               region = "west",
                                               data = data_CGFS_west,
                                               mesh = mesh_by_region$west$bspde)
}






if (isTRUE(East_English_Channel)) {
  randomCV_summary_east <- imap_dfr(randomCV_by_region$east$cv_outputs,
                                function(resp_list, response) {
                                  imap_dfr(resp_list, function(cv, family) {
                                    tibble(response = response,
                                           family = family,
                                           n_folds_tot = length(cv$pdHess),
                                           n_converged = sum(cv$pdHess),
                                           pourcent_converged = 100 * mean(cv$pdHess),
                                           all_converged = isTRUE(cv$converged))
                                  })
                                })
}


if (isTRUE(West_English_Channel)) {
  randomCV_summary_west <- imap_dfr(randomCV_by_region$west$cv_outputs,
                                function(resp_list, response) {
                                  imap_dfr(resp_list, function(cv, family) {
                                    tibble(response = response,
                                           family = family,
                                           n_folds_tot = length(cv$pdHess),
                                           n_converged = sum(cv$pdHess),
                                           pourcent_converged = 100 * mean(cv$pdHess),
                                           all_converged = isTRUE(cv$converged))
                                  })
                                })
}
