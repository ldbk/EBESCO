library(dplyr)
library(tibble)
library(broom)

safe_tidy <- function(...) {
  tryCatch(broom::tidy(...), error = function(e) tibble::tibble())
}

extract_params_from_sanity <- function(sanity_output, region) {
  
  out <- list()
  
  for (response in names(sanity_output)) {
    
    models_valid <- sanity_output[[response]]$models_valid
    if (length(models_valid) == 0) next
    
    for (family in names(models_valid)) {
      
      model <- models_valid[[family]]
      is_delta <- grepl("^delta", family, ignore.case = TRUE)
      
      # fixed effects: intercept
      td_fix <- if (is_delta) {
        dplyr::bind_rows(
          safe_tidy(model, conf.int = TRUE, model = 1) %>% dplyr::mutate(component = "delta_1"),
          safe_tidy(model, conf.int = TRUE, model = 2) %>% dplyr::mutate(component = "delta_2")
        )
      } else {
        safe_tidy(model, conf.int = TRUE) %>% dplyr::mutate(component = "single")
      }
      
      td_fix <- td_fix %>%
        dplyr::filter(term == "(Intercept)") %>%
        dplyr::mutate(group = "fixed")
      
      # random parameters
      td_ran <- if (is_delta) {
        dplyr::bind_rows(
          safe_tidy(model, "ran_pars", conf.int = TRUE, model = 1) %>% dplyr::mutate(component = "delta_1"),
          safe_tidy(model, "ran_pars", conf.int = TRUE, model = 2) %>% dplyr::mutate(component = "delta_2")
        )
      } else {
        safe_tidy(model, "ran_pars", conf.int = TRUE) %>% dplyr::mutate(component = "single")
      }
      
      td_ran <- td_ran %>%
        dplyr::filter(term %in% c("range", "sigma_O", "sigma_E")) %>%
        dplyr::mutate(group = "ran_pars")
      
      keep_cols <- c("term", "estimate", "std.error", "conf.low", "conf.high",
                     "group", "component")
      
      td <- dplyr::bind_rows(
        td_fix %>% dplyr::select(dplyr::any_of(keep_cols)),
        td_ran %>% dplyr::select(dplyr::any_of(keep_cols))
      ) %>%
        dplyr::mutate(
          region = region,
          response = response,
          family = family,
          parameter = dplyr::if_else(term == "(Intercept)", "intercept", term)
        ) %>%
        dplyr::select(region, response, family, component, group,
                      parameter, estimate, std.error, conf.low, conf.high)
      
      out[[length(out) + 1]] <- td
    }
  }
  
  dplyr::bind_rows(out)
}



params_east <- extract_params_from_sanity(sanity_by_region$east, region = "east")
params_west <- extract_params_from_sanity(sanity_by_region$west, region = "west")

params_all <- bind_rows(params_east, params_west) %>%
  mutate(region = factor(region, levels = c("west", "east")))



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

