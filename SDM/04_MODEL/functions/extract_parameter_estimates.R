
# ------------------------------------------------------------------------------#
#### EXTRACT PARAMETER ESTIMATES FROM CONVERGED MODELS ####
# ------------------------------------------------------------------------------#

extract_params_converged_models <- function(converged_models, region) {
  
  # Extract all model for the selected region
  models_by_response <- converged_models[[region]]
  
  # Loop over each response variable 
  params_by_response <- lapply(names(models_by_response), function(response) {
    
    # Extract all model for the selected response
    models_by_family <- models_by_response[[response]]

    # Loop over each model family
    params_by_family <- lapply(names(models_by_family), function(family) {
      
      # Extract all model for the selected family
      model <- models_by_family[[family]]
      
      # Identify whether the model is a delta model
      is_delta <- grepl("^delta", family, ignore.case = TRUE)
      
      # Extract fixed-effect parameters 
      fixed_params <- if (is_delta) {
        bind_rows(
          tidy(model, conf.int = TRUE, model = 1) %>% mutate(component = "delta_1"),
          tidy(model, conf.int = TRUE, model = 2) %>% mutate(component = "delta_2")
        )
      } else {
        tidy(model, conf.int = TRUE) %>% mutate(component = "single")
      }
      
      # Keep only the intercept term
      fixed_params <- fixed_params %>%
        filter(term == "(Intercept)") %>%
        mutate(param_type = "fixed")
      
      # Extract random-effect parameters
      random_params <- if (is_delta) {
        bind_rows(
          tidy(model, effects = "ran_pars", conf.int = TRUE, model = 1) %>%
            mutate(component = "delta_1"),
          tidy(model, effects = "ran_pars", conf.int = TRUE, model = 2) %>%
            mutate(component = "delta_2")
        )
      } else {
        tidy(model, effects = "ran_pars", conf.int = TRUE) %>%
          mutate(component = "single")
      }
      
      # Keep only spatial and spatiotemporal variance parameters
      random_params <- random_params %>%
        filter(term %in% c("range", "sigma_O", "sigma_E")) %>%
        mutate(param_type = "random")
      
      # Combine fixed and random parameters 
      bind_rows(fixed_params, random_params) %>%
        mutate(region = region,
               response = response,
               family = family,
               parameter = if_else(term == "(Intercept)", "intercept", term)) %>%
        dplyr::select(region, response, family, component, param_type,
                      parameter, estimate, std.error, conf.low, conf.high)
    })
    
    bind_rows(params_by_family)
  })
  
  bind_rows(params_by_response)
}



# params_west <- extract_params_converged_models(converged_models, region = "west")
# params_east <- extract_params_converged_models(converged_models, region = "east")
# 
# params_all <- bind_rows(params_east, params_west) %>%
#   mutate(region = factor(region, levels = c("west", "east")))


# params_all %>%
#   ggplot(aes(x = family, y = estimate, shape = component, color = family)) +
#   geom_point(size = 2.8, position = position_dodge(width = 0.6)) +
#   geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
#                 width = 0.2, position = position_dodge(width = 0.6)) +
#   facet_grid(parameter ~ region + response, scales = "free_y") +
#   labs(x = "Family",  y = "Estimate", shape = "Component",
#     title = "sdmTMB parameters estimates") +
#   guides(color = "none") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position = "top")

