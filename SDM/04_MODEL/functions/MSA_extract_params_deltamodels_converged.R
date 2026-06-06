extract_params_deltamodels_converged <- function(meshes_options_region) {
  
  purrr::map_dfr(meshes_options_region, function(current_converged_model) {
    
    current_fit <- current_converged_model$fit
    
    cutoff <- current_converged_model$cutoff
    computation_time <- current_converged_model$elapsed_fit
    barrier <- isTRUE(current_converged_model$barrier)
    boundary <- isTRUE(current_converged_model$boundary)
    n_vertices <- current_converged_model$n_vertices
    region <- current_converged_model$region
    
    # Fixed effects
    fixed <- bind_rows(
      tidy(current_fit, conf.int = TRUE, model = 1) %>%
        mutate(component = "delta_1"),
      tidy(current_fit, conf.int = TRUE, model = 2) %>%
        mutate(component = "delta_2")) %>%
      mutate(type = "fixed")
    
    # Random effects
    random <- bind_rows(
      tidy(current_fit, effects = "ran_pars", conf.int = TRUE, model = 1) %>%
        mutate(component = "delta_1"),
      tidy(current_fit, effects = "ran_pars", conf.int = TRUE, model = 2) %>%
        mutate(component = "delta_2")) %>%
      filter(term %in% c("range", "sigma_O", "sigma_E")) %>%
      mutate(type = "random")
    
    bind_rows(fixed, random) %>%
      mutate(cutoff = cutoff,
             n_vertices = n_vertices,
             computation_time = computation_time,
             barrier = barrier,
             boundary = boundary,
             region = region,
             parameter = if_else(term == "(Intercept)", "intercept", term))
  })
}