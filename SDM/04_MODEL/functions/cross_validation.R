# ------------------------------------------------------------------------------#
# ------------------------------------------------------------------------------#

# Cross-validation on VALID sdmTMB models

# ------------------------------------------------------------------------------#
# ------------------------------------------------------------------------------#

CV_valid_models <- function(sanity_output, region, data, mesh) {
  
  # Create the offset column (sdmTMB_cv() expects offset = "column name")
  data <- data %>%  mutate(logSweptAreaKm2 = log(sweptAreaKm2))
  
  # Define which response variables should use an offset column.
  # offset_col_by_response <- list(totalWeightKg = "logSweptAreaKm2",
  #                                densityKgKm2  = NULL)

  # Randomly assign each observation (row) to one fold ID
  # fold_ids <- sample(seq_len(k_folds), size = nrow(data_test_model), replace = TRUE)

  # ------------------------------------------------------------------------------#
  # Spatio-temporal blocking
  # ------------------------------------------------------------------------------#
  
  if (region == "west") data_folds <- readRDS(here("01_DATA/folds_CV/west_3folds.rds"))
  if (region == "east") data_folds <- readRDS(here("01_DATA/folds_CV/east_4folds.rds"))

  data_CV <- data %>%
    mutate(year = as.factor(year)) %>%
    left_join(data_folds %>% 
                mutate(year = as.factor(year))%>%
                dplyr::select(year, X, Y, lon, lat, fold_id),
              by = c("year", "X", "Y", "lon", "lat"))
  
  k_folds = length(unique(data_CV$fold_id))
  
  # ggplot(data_folds) +
  #   geom_point(aes(x = X, y = Y, color = factor(spatial_zone)),
  #              size = 5, alpha = 0.7) +
  #   geom_text(aes(x = X, y = Y, label = fold_id),
  #             size = 3) +
  #   facet_wrap(~ year) +
  #   theme_bw() +
  #   labs(title = "Manche Est — 21 folds (3 par année)",
  #        color = "Zone", x = "X", y = "Y")

      # ------------------------------------------------------------------------------#
  # Run CV across validated models
  # ------------------------------------------------------------------------------#
  
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
        data = data_CV,
        mesh = mesh,
        family = distr_family,
        spatial = "on",
        time = "year",
        spatiotemporal = "IID",
        fold_ids = data_CV$fold_id,
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

  list(data_CV = data_CV,
       cv_outputs = cv_outputs,
       cv_scores = dplyr::bind_rows(cv_scores))
  
}


cross_validation_by_region <- list()

if (isTRUE(East_English_Channel)) {
  cross_validation_by_region$east <-  CV_valid_models(sanity_output  = sanity_by_region$east,
                                                      region = "east",
                                                      data = data_CGFS_east,
                                                      mesh = mesh_by_region$east$bspde)
}

if (isTRUE(West_English_Channel)) {
  cross_validation_by_region$west <- CV_valid_models(sanity_output  = sanity_by_region$west, 
                                                     region = "west",
                                                     data = data_CGFS_west,
                                                     mesh = mesh_by_region$west$bspde)
}






# cross_validation_by_region$east$cv_scores
# 
# cross_validation_by_region$west$cv_scores
# 
# 
# 
# conv_summary_east <- imap_dfr(cross_validation_by_region$east$cv_outputs, 
#                               function(resp_list, response) {
#   imap_dfr(resp_list, function(cv, family) {
#     tibble(response = response,
#            family = family,
#            n_folds_tot = length(cv$pdHess),
#            n_converged = sum(cv$pdHess),
#            pourcent_converged = 100 * mean(cv$pdHess),
#            all_converged = isTRUE(cv$converged))
#   })
# })
# 
# conv_summary_east
# 
# 
# conv_summary_west <- imap_dfr(cross_validation_by_region$west$cv_outputs, 
#                               function(resp_list, response) {
#   imap_dfr(resp_list, function(cv, family) {
#     tibble(response = response,
#            family = family,
#            n_folds_tot = length(cv$pdHess),
#            n_converged = sum(cv$pdHess),
#            pourcent_converged = 100 * mean(cv$pdHess),
#            all_converged = isTRUE(cv$converged))
#   })
# })
# 
# conv_summary_west
