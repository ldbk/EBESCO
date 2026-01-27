# ============================================================================== #
#### COMPUTE AIC WEIGHTS FROM VALID MODELS ####
# ============================================================================== #

compute_AIC_weights <- function(sanity_output, region_name = "region") {
  
  AIC_weights_by_response <- list()
  
  # Loop over response variables
  for (response_var in names(sanity_output)) {
    
    fitted_models <- sanity_output[[response_var]]$models_valid
    
    # Skip if no valid models
    if (length(fitted_models) == 0) next
    
    # Compute AIC [AIC()] 
    AIC_values <- sapply(fitted_models, AIC)
    
    # Delta AIC
    deltaAIC <- AIC_values - min(AIC_values)
    
    # AIC weights : difference between the AIC for model i and the model with the minimum AIC
    relative_likelihood <- exp(-0.5 * deltaAIC)
    weights <- relative_likelihood / sum(relative_likelihood)
    
    AIC_df <- data.frame(region = region_name,
                         response = response_var,
                         family = names(AIC_values),
                         AIC = as.numeric(AIC_values),
                         deltaAIC = as.numeric(deltaAIC),
                         weight = as.numeric(weights))
    
    AIC_df <- AIC_df[order(AIC_df$AIC), ]
    
    AIC_weights_by_response[[response_var]] <- AIC_df
  }
  
  AIC_weights_df <- do.call(rbind, AIC_weights_by_response)
  rownames(AIC_weights_df) <- NULL
  
  return(AIC_weights_df)
}



AIC_by_region <- list()

if (isTRUE(East_English_Channel)) {
  AIC_by_region$east <- compute_AIC_weights(
    sanity_output = sanity_by_region$east,
    region_name = "east"
  )
}

if (isTRUE(West_English_Channel)) {
  AIC_by_region$west <- compute_AIC_weights(
    sanity_output = sanity_by_region$west,
    region_name = "west"
  )
}





