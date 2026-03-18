

check_sanity_meshes_config <- function(meshes_options_region) {
  
  cutoffs_converged_models <- list()
  cutoffs_sanity_failed <- list()
  
  for (cutoff_name in names(meshes_options_region)) {
    
    cat("\n\n----------------------\n")
    cat("CUTOFF:", cutoff_name, "\n")
    cat("----------------------\n")
    
    cutoff_obj <- meshes_options_region[[cutoff_name]]
    current_fit <- cutoff_obj$fit
    
    # Run sanity()
    sanity_res <- tryCatch(sdmTMB::sanity(current_fit), error = function(e) e)
    
    # if all_ok = TRUE --> model converged --> keep it
    if (!isTRUE(sanity_res$all_ok)) {
      cutoffs_sanity_failed[[cutoff_name]] <- sanity_res
    } else {
      cutoffs_converged_models[[cutoff_name]] <- cutoff_obj
    }
  }
  
  # Convert empty lists to NULL
  if (length(cutoffs_converged_models) == 0) cutoffs_converged_models <- NULL
  if (length(cutoffs_sanity_failed) == 0) cutoffs_sanity_failed <- NULL
  
  return(list(cutoffs_converged_models = cutoffs_converged_models,
              cutoffs_sanity_failed = cutoffs_sanity_failed))
  
}

# sanity_results <- check_sanity_meshes_config(meshes_boundary_west)
# 
# models_converged <- sanity_results$models_converged
# models_sanity_failed <- sanity_results$models_sanity_failed

# 
# extract fixed effect
# fixed <- tidy(fit) %>%
#   dplyr::mutate(type = "fixed",
#                 cutoff = cutoff,
#                 max_edge_in = max_edge[1],
#                 max_edge_out = max_edge[2],
#                 AIC = sdmTMB::cAIC(fit),
#                 n_vertices = mesh_used$mesh$n,
#                 add_barrier = add_barrier,
#                 boundary = boundary,
#                 region = region_name)
# 
# # extract random parameters
# random <- tidy(fit, effects = "ran_pars") %>%
#   dplyr::filter(term %in% c("range", "sigma_O")) %>%
#   dplyr::mutate(type = "random",
#                 cutoff = cutoff,
#                 max_edge_in = max_edge[1],
#                 max_edge_out = max_edge[2],
#                 AIC = sdmTMB::cAIC(fit),
#                 n_vertices = mesh_used$mesh$n,
#                 add_barrier = add_barrier,
#                 boundary = boundary,
#                 region = region_name)
# 
# # Combine parameters
# all_params[[paste0("cutoff_", cutoff)]] <- dplyr::bind_rows(fixed, random)
# 
# 
# # Combine all parameter outputs
# all_params_df <- dplyr::bind_rows(all_params)





