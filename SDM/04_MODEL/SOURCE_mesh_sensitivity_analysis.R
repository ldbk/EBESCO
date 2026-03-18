

# ------------------------------------------------------------------------------#
# ------------------------------------------------------------------------------#
# SCRIPT TO RUN MESH SENSITIVITY ANALYSIS PER SPECIES AND REGION
# ------------------------------------------------------------------------------#
# ------------------------------------------------------------------------------#
rm(list=ls())

# ------------------------------------------------------------------------------#
####  LOAD PACKAGES  #### 
# ------------------------------------------------------------------------------#
source(here::here('04_MODEL/packages/packages.R'))

# ------------------------------------------------------------------------------#
####  DEFINE PARAMETERS  #### 
# ------------------------------------------------------------------------------#
## Define species
# ----------------#
# sp_scientific <- "Capros aper"                 # Boarfish / Sanglier
# sp_scientific <- "Chelidonichthys cuculus"     # Red gurnard / Grondin rouge
# sp_scientific <- "Chelidonichthys lucerna"     # Tub gurnard / Grondin perlon
# sp_scientific <- "Conger conger"               # European conger / Congre d'Europe
# sp_scientific <- "Dicentrarchus labrax"        # European seabass / Bar européen
# sp_scientific <- "Engraulis encrasicolus"      # European anchovy / Anchois
# sp_scientific <- "Eutrigla gurnardus"          # Grey gurnard / Grondin gris
# sp_scientific <- "Melanogrammus aeglefinus"    # Haddock / Églefin
# sp_scientific <- "Merluccius merluccius"       # European hake / Merlu européen
# sp_scientific <- "Micromesistius poutassou"    # Blue whiting / Merlan bleu
# sp_scientific <- "Pollachius pollachius"       # Pollack / Lieu jaune
# sp_scientific <- "Squalus acanthias"           # Spiny dogfish / Aiguillat commun
sp_scientific <- "Trachurus trachurus"         # Atlantic horse mackerel / Chinchard commun
# sp_scientific <- "Zeus faber"                  # John Dory / Saint-Pierre
# sp_scientific <- "Clupea harengus"             # Atlantic herring / Hareng de l’Atlantique
# sp_scientific <- "Solea solea"                 # Common sole / Sole commune


## Define study refions for the species 
# ----------------------------------------#
species_criteria_region <- readRDS(here::here("01_DATA/species_criteria_region.rds"))%>%
  dplyr::filter(species == sp_scientific)

West_English_Channel = isTRUE(species_criteria_region$west_region)
East_English_Channel = isTRUE(species_criteria_region$east_region)

# ------------------------------------------------------------------------------#
####  LOAD AND TRANSFORM DATA  #### 
# ------------------------------------------------------------------------------#
source(here::here('03_TRANSFORM_DATA/transform_data.R'))



# ------------------------------------------------------------------------------#
# MESHES PARAMETERS
# ------------------------------------------------------------------------------#

west_params <- list(cutoff_values = c(3, 5, 7, 10, 15, 20, 30),
                    max_edge_in = c(15, 25, 35, 50, 75, 100, 150),
                    max_edge_out = rep(200, 7))

east_params <- list(cutoff_values = c(2.5, 3, 5, 7, 10, 15, 20),
                    max_edge_in = c(13, 15, 25, 35, 50, 75, 100),
                    max_edge_out = rep(200, 7))


# ------------------------------------------------------------------------------#
# WEST REGION
# ------------------------------------------------------------------------------#


if (isTRUE(West_English_Channel)) {

  source(here::here("04_MODEL", "functions", "MSA_create_meshes_configurations.R"))
  
  meshes_west <- do.call(create_meshes_configurations, 
                         c(list(region_name = "west", add_barrier = FALSE, boundary = FALSE),
                           west_params))
  
  meshes_barrier_west <- do.call(create_meshes_configurations, 
                                 c(list(region_name = "west", add_barrier = TRUE, boundary = FALSE),
                                   west_params))
  
  meshes_boundary_west <- do.call(create_meshes_configurations, 
                                  c(list(region_name = "west", add_barrier = FALSE, boundary = TRUE),
                                    west_params))
  
  
  source(here::here("04_MODEL", "functions", "MSA_check_sanity_meshes_config.R"))
  
  sanity_meshes_west <- check_sanity_meshes_config(meshes_west)
  models_converged_west <- sanity_meshes_west$cutoffs_converged_models
  
  sanity_meshes_barrier_west <- check_sanity_meshes_config(meshes_barrier_west)
  models_converged_barrier_west <- sanity_meshes_barrier_west$cutoffs_converged_models
  
  sanity_meshes_boundary_west <- check_sanity_meshes_config(meshes_boundary_west)
  models_converged_boundary_west <- sanity_meshes_boundary_west$cutoffs_converged_models
  
  
  source(here::here("04_MODEL", "functions", "MSA_extract_params_models_converged.R"))
  params_west <- extract_params_models_converged(models_converged_west)
  params_west_barrier <- extract_params_models_converged(models_converged_barrier_west)
  params_west_boundary <- extract_params_models_converged(models_converged_boundary_west)
  
  all_params_west <- dplyr::bind_rows(params_west,
                                      params_west_barrier,
                                      params_west_boundary)
  
  
  all_params_west %>%
    mutate(config = case_when(!barrier & !boundary ~ "no barrier, no boundary",
                              !barrier &  boundary ~ "no barrier, boundary",
                              barrier & !boundary ~ "barrier, no boundary")) %>%
    rename(metric = term, value = estimate) %>%
    bind_rows(all_params_west %>%
                mutate(config = case_when(!barrier & !boundary ~ "no barrier, no boundary",
                                          !barrier &  boundary ~ "no barrier, boundary",
                                          barrier & !boundary ~ "barrier, no boundary")) %>%
                group_by(cutoff, config) %>%
                summarise(cAIC = first(cAIC), n_vertices = first(n_vertices), .groups = "drop") %>%
                pivot_longer(c(cAIC, n_vertices), names_to = "metric", values_to = "value")) %>%
    mutate(cutoff = factor(cutoff),
           metric = factor(metric,
                           levels = c("(Intercept)", "range", "sigma_O", "cAIC", "n_vertices"),
                           labels = c("intercept", "spatial range", "sigma O", "cAIC", "number of vertices"))) %>%  
    ggplot(aes(x = cutoff, y = value, color = config, group = config)) +
    geom_point(position = position_dodge(0.5), size = 2.5) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width = 0, position = position_dodge(0.5), na.rm = TRUE) +
    facet_wrap(~metric, scales = "free_y") +
    theme_bw() +
    labs(x = "Cutoff", y = NULL, color = "Configuration")
  
  
  
  
  source(here::here("04_MODEL", "functions", "MSA_plot_meshes_patchwork.R"))
  
  plots_west <- plot_meshes_patchwork(models_converged_west, ncol=2)
  plots_boundary_west <- plot_meshes_patchwork(models_converged_boundary_west, ncol=2)
  plots_barrier_west <- plot_meshes_patchwork(models_converged_barrier_west, ncol=2)
  
  
}







