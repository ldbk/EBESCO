
extract_params_models_converged <- function(meshes_options_region) {
  
  purrr::map_dfr(meshes_options_region, function(current_converged_model) {
    
    current_fit <- current_converged_model$fit
    
    # metrics, params
    cutoff <- current_converged_model$cutoff
    computation_time <- current_converged_model$elapsed_fit
    barrier <- isTRUE(current_converged_model$barrier)
    boundary <- isTRUE(current_converged_model$boundary)
    n_vertices <- current_converged_model$n_vertices
    region <- current_converged_model$region
    
    
    # fixed
    fixed <- tidy(current_fit) %>%
      dplyr::mutate(type = "fixed",
                    cutoff = cutoff,
                    n_vertices = n_vertices,
                    computation_time = computation_time,
                    barrier = barrier,
                    boundary = boundary,
                    region = region)
    
    # random (range, sigma_O)
    random <- tidy(current_fit, effects = "ran_pars") %>%
      dplyr::filter(.data$term %in% c("range", "sigma_O")) %>%
      dplyr::mutate(type = "random",
                    cutoff = cutoff,
                    n_vertices = n_vertices,
                    computation_time = computation_time,
                    barrier = barrier,
                    boundary = boundary,
                    region = region)
    
    bind_rows(fixed, random)
  })
  
}