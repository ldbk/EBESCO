

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
## Define speciesedlihtaMG24.

# ----------------#
# sp_scientific <- "Capros aper"                 # Boarfish / Sanglier
# sp_scientific <- "Chelidonichthys cuculus"     # Red gurnard / Grondin rouge
# sp_scientific <- "Chelidonichthys lucerna"     # Tub gurnard / Grondin perlon
# sp_scientific <- "Clupea harengus"             # Atlantic herring / Hareng de l’Atlantique
# sp_scientific <- "Conger conger"               # European conger / Congre d'Europe
# sp_scientific <- "Dicentrarchus labrax"        # European seabass / Bar européen
# sp_scientific <- "Engraulis encrasicolus"      # European anchovy / Anchois
# sp_scientific <- "Eutrigla gurnardus"          # Grey gurnard / Grondin gris
# sp_scientific <- "Gadus morhua"                # morue
# sp_scientific <- "Gobius niger"
# sp_scientific <- "Hippocampus hippocampus"
# sp_scientific <- "Melanogrammus aeglefinus"    # Haddock / Églefin
# sp_scientific <- "Merluccius merluccius"       # European hake / Merlu européen
# sp_scientific <- "Micromesistius poutassou"    # Blue whiting / Merlan bleu
# sp_scientific <- "Pollachius pollachius"       # Pollack / Lieu jaune
sp_scientific <- "Solea solea"                 # Common sole / Sole commune
# sp_scientific <- "Squalus acanthias"           # Spiny dogfish / Aiguillat commun
# sp_scientific <- "Trachurus trachurus"         # Atlantic horse mackerel / Chinchard commun
# sp_scientific <- "Zeus faber"                  # John Dory / Saint-Pierre




## Define study refions for the species 
# ----------------------------------------#
species_criteria_region <- readRDS(here::here("01_DATA/species_criteria_region.rds"))%>%
  dplyr::filter(species == sp_scientific)

West_English_Channel = isTRUE(species_criteria_region$west_region)
East_English_Channel = isTRUE(species_criteria_region$east_region)

sp_name_safe <- gsub("[^A-Za-z0-9_]", "_", sp_scientific)

# ------------------------------------------------------------------------------#
####  LOAD AND TRANSFORM DATA  #### 
# ------------------------------------------------------------------------------#
source(here::here('03_TRANSFORM_DATA/transform_data.R'))



# ------------------------------------------------------------------------------#
# MESHES PARAMETERS
# ------------------------------------------------------------------------------#

west_params <- list(
  with_max = list(
    cutoff_values = c(3, 5, 7, 10, 15, 20),
    max_edge_in = c(15, 25, 35, 50, 75, 100),
    max_edge_out = rep(100, 6)
  ),
  no_max = list(
    cutoff_values = c(3, 5, 7, 10, 15, 20, 25, 30)
  )
)

east_params <- list(
  with_max = list(
    cutoff_values = c(2.5, 3, 5, 7, 10, 15),
    max_edge_in = c(13, 15, 25, 35, 50, 75),
    max_edge_out = rep(100, 6)
  ),
  no_max = list(
    cutoff_values = c(2.5, 3, 5, 7, 10, 15, 20, 25)
  )
)

# ------------------------------------------------------------------------------#
# LOAD MSA FUNCTIONS 
# ------------------------------------------------------------------------------#

source(here::here("04_MODEL/functions/MSA_create_meshes_configurations2.R"))
source(here::here("04_MODEL/functions/MSA_check_sanity_meshes_config.R"))
source(here::here("04_MODEL/functions/MSA_extract_params_models_converged.R"))
source(here::here("04_MODEL/functions/MSA_plot_meshes_patchwork.R"))

new_repertory_msa <- here(paste0("05_OUTPUTS/mesh_sensitivity_results/", 
                                 sp_name_safe, "_MSA_", 
                                 format(Sys.time(), "%d-%b-%Y-%H.%M")))
# Regions to process
regions <- list(
  west = list(valid = isTRUE(West_English_Channel), params = west_params),
  east = list(valid = isTRUE(East_English_Channel), params = east_params))

# Keep only valid regions
regions_valid <- regions[sapply(regions, function(x) x$valid)]

# Results container
msa <- list()

# ------------------------------------------------------------------------------#
# RUN MSA OVER REGION
# ------------------------------------------------------------------------------#
for (region_name in names(regions_valid)) {
  
  region_params_with_max <- regions_valid[[region_name]]$params$with_max
  region_params_no_max <- regions_valid[[region_name]]$params$no_max
  
  # 1) Create meshes configurations
  # ----------------------------------------------------------------------------#
  meshes_simple <- do.call(
    create_meshes_configurations,
    c(list(region_name, add_barrier = FALSE, boundary = FALSE),
      region_params_with_max)
  )
  
  meshes_barrier <- do.call(
    create_meshes_configurations,
    c(list(region_name, add_barrier = TRUE, boundary = FALSE),
      region_params_with_max)
  )
  
  meshes_boundary <- do.call(
    create_meshes_configurations,
    c(list(region_name, add_barrier = FALSE, boundary = TRUE),
      region_params_with_max)
  )
  
  meshes_simple_no_max <- do.call(
    create_meshes_configurations,
    c(list(region_name, add_barrier = FALSE, boundary = FALSE),
      region_params_no_max)
  )
  
  meshes_barrier_no_max <- do.call(
    create_meshes_configurations,
    c(list(region_name, add_barrier = TRUE, boundary = FALSE),
      region_params_no_max)
  )
  
  meshes_boundary_no_max <- do.call(
    create_meshes_configurations,
    c(list(region_name, add_barrier = FALSE, boundary = TRUE),
      region_params_no_max)
  )
  
  # 2) Sanity checks
  # ----------------------------------------------------------------------------#
  sanity_simple <- if (!is.null(meshes_simple)) {
    check_sanity_meshes_config(meshes_simple)
  } else NULL
  
  sanity_barrier <- if (!is.null(meshes_barrier)) {
    check_sanity_meshes_config(meshes_barrier)
  } else NULL
  
  sanity_boundary <- if (!is.null(meshes_boundary)) {
    check_sanity_meshes_config(meshes_boundary)
  } else NULL
  
  sanity_simple_no_max <- if (!is.null(meshes_simple_no_max)) {
    check_sanity_meshes_config(meshes_simple_no_max)
  } else NULL
  
  sanity_barrier_no_max <- if (!is.null(meshes_barrier_no_max)) {
    check_sanity_meshes_config(meshes_barrier_no_max)
  } else NULL
  
  sanity_boundary_no_max <- if (!is.null(meshes_boundary_no_max)) {
    check_sanity_meshes_config(meshes_boundary_no_max)
  } else NULL
  
  models_converged_simple <- if (!is.null(sanity_simple)) {
    sanity_simple$cutoffs_converged_models
  } else NULL
  
  models_converged_barrier <- if (!is.null(sanity_barrier)) {
    sanity_barrier$cutoffs_converged_models
  } else NULL
  
  models_converged_boundary <- if (!is.null(sanity_boundary)) {
    sanity_boundary$cutoffs_converged_models
  } else NULL
  
  models_converged_simple_no_max <- if (!is.null(sanity_simple_no_max)) {
    sanity_simple_no_max$cutoffs_converged_models
  } else NULL
  
  models_converged_barrier_no_max <- if (!is.null(sanity_barrier_no_max)) {
    sanity_barrier_no_max$cutoffs_converged_models
  } else NULL
  
  models_converged_boundary_no_max <- if (!is.null(sanity_boundary_no_max)) {
    sanity_boundary_no_max$cutoffs_converged_models
  } else NULL
  
  # 3) Extract params
  # ----------------------------------------------------------------------------#
  params_simple <- if (!is.null(models_converged_simple)) {
    extract_params_models_converged(models_converged_simple)
  } else NULL
  if (!is.null(params_simple)) params_simple <- params_simple %>% dplyr::mutate(config = "simple outer mesh")

  params_barrier <- if (!is.null(models_converged_barrier)) {
    extract_params_models_converged(models_converged_barrier)
  } else NULL
  if (!is.null(params_barrier)) params_barrier <- params_barrier %>% dplyr::mutate(config = "barrier outer mesh")
  
  params_boundary <- if (!is.null(models_converged_boundary)) {
    extract_params_models_converged(models_converged_boundary)
  } else NULL
  if (!is.null(params_boundary)) params_boundary <- params_boundary %>% dplyr::mutate(config = "boundary outer mesh")
  
  params_simple_no_max <- if (!is.null(models_converged_simple_no_max)) {
    extract_params_models_converged(models_converged_simple_no_max)
  } else NULL
  if (!is.null(params_simple_no_max)) params_simple_no_max <- params_simple_no_max %>% dplyr::mutate(config = "simple")
  
  params_barrier_no_max <- if (!is.null(models_converged_barrier_no_max)) {
    extract_params_models_converged(models_converged_barrier_no_max)
  } else NULL
  if (!is.null(params_barrier_no_max)) params_barrier_no_max <- params_barrier_no_max %>% dplyr::mutate(config = "barrier")

  params_boundary_no_max <- if (!is.null(models_converged_boundary_no_max)) {
    extract_params_models_converged(models_converged_boundary_no_max)
  } else NULL
  if (!is.null(params_boundary_no_max)) params_boundary_no_max <- params_boundary_no_max %>% dplyr::mutate(config = "boundary")
  
  
  all_params <- dplyr::bind_rows(
    params_simple,
    params_barrier,
    params_boundary,
    params_simple_no_max,
    params_barrier_no_max,
    params_boundary_no_max
  )
  
  # 4) Plot meshes patchwork
  # ----------------------------------------------------------------------------#
  plots_meshes_simple <- if (!is.null(models_converged_simple)) {
    plot_meshes_patchwork(models_converged_simple, ncol = 4)
  } else NULL
  
  plots_meshes_barrier <- if (!is.null(models_converged_barrier)) {
    plot_meshes_patchwork(models_converged_barrier, ncol = 4)
  } else NULL
  
  plots_meshes_boundary <- if (!is.null(models_converged_boundary)) {
    plot_meshes_patchwork(models_converged_boundary, ncol = 4)
  } else NULL
  
  plots_meshes_simple_no_max <- if (!is.null(models_converged_simple_no_max)) {
    plot_meshes_patchwork(models_converged_simple_no_max, ncol = 4)
  } else NULL
  
  plots_meshes_barrier_no_max <- if (!is.null(models_converged_barrier_no_max)) {
    plot_meshes_patchwork(models_converged_barrier_no_max, ncol = 4)
  } else NULL
  
  plots_meshes_boundary_no_max <- if (!is.null(models_converged_boundary_no_max)) {
    plot_meshes_patchwork(models_converged_boundary_no_max, ncol = 4)
  } else NULL
  
  source(here::here("04_MODEL/functions/MSA_simulate_compute_plot_coeff_variation.R"))
  
  cv_outputs_simple <- if (!is.null(models_converged_simple)) {
    simulate_cv(models_converged_simple, prediction_grid)
  } else NULL
  if (!is.null(cv_outputs_simple)) cv_outputs_simple <- cv_outputs_simple %>% dplyr::mutate(config = "simple outer mesh")
  
  cv_outputs_barrier <- if (!is.null(models_converged_barrier)) {
    simulate_cv(models_converged_barrier, prediction_grid)
  } else NULL
  if (!is.null(cv_outputs_barrier)) cv_outputs_barrier <- cv_outputs_barrier %>% dplyr::mutate(config = "barrier outer mesh")
  
  cv_outputs_boundary <- if (!is.null(models_converged_boundary)) {
    simulate_cv(models_converged_boundary, prediction_grid)
  } else NULL
  if (!is.null(cv_outputs_boundary)) cv_outputs_boundary <- cv_outputs_boundary %>% dplyr::mutate(config = "boundary outer mesh")
  
  cv_outputs_simple_no_max <- if (!is.null(models_converged_simple_no_max)) {
    simulate_cv(models_converged_simple_no_max, prediction_grid)
  } else NULL
  if (!is.null(cv_outputs_simple_no_max)) cv_outputs_simple_no_max <- cv_outputs_simple_no_max %>% dplyr::mutate(config = "simple")
  
  cv_outputs_barrier_no_max <- if (!is.null(models_converged_barrier_no_max)) {
    simulate_cv(models_converged_barrier_no_max, prediction_grid)
  } else NULL
  if (!is.null(cv_outputs_barrier_no_max)) cv_outputs_barrier_no_max <- cv_outputs_barrier_no_max %>% dplyr::mutate(config = "barrier")
  
  cv_outputs_boundary_no_max <- if (!is.null(models_converged_boundary_no_max)) {
    simulate_cv(models_converged_boundary_no_max, prediction_grid)
  } else NULL
  if (!is.null(cv_outputs_boundary_no_max)) cv_outputs_boundary_no_max <- cv_outputs_boundary_no_max %>% dplyr::mutate(config = "boundary")
  
  all_cv_outputs <- dplyr::bind_rows(
    cv_outputs_simple,
    cv_outputs_barrier,
    cv_outputs_boundary,
    cv_outputs_simple_no_max,
    cv_outputs_barrier_no_max,
    cv_outputs_boundary_no_max
  )
  
  # ---------------------------------------------------------------------------#
  ####  SAVE ALL PLOTS INTO HTML FILE ####
  # ---------------------------------------------------------------------------#
  no_mesh <- all(c(
    is.null(models_converged_simple),
    is.null(models_converged_barrier),
    is.null(models_converged_boundary),
    is.null(models_converged_simple_no_max),
    is.null(models_converged_barrier_no_max),
    is.null(models_converged_boundary_no_max)
  ))
  
  if (!no_mesh) {
    
    rmarkdown::render(
      input = here::here("04_MODEL", "report", "report_MSA2.Rmd"),
      output_dir = new_repertory_msa,
      output_file = paste0(sp_name_safe, "_", region_name, "_report", ".html")
    )
    
    msa[[region_name]] <- list(
      region_params = list(
        with_max = region_params_with_max,
        no_max = region_params_no_max
      ),
      meshes = list(
        simple = meshes_simple,
        barrier = meshes_barrier,
        boundary = meshes_boundary,
        simple_no_max = meshes_simple_no_max,
        barrier_no_max = meshes_barrier_no_max,
        boundary_no_max = meshes_boundary_no_max
      ),
      sanity = list(
        simple = sanity_simple,
        barrier = sanity_barrier,
        boundary = sanity_boundary,
        simple_no_max = sanity_simple_no_max,
        barrier_no_max = sanity_barrier_no_max,
        boundary_no_max = sanity_boundary_no_max
      ),
      models = list(
        simple = models_converged_simple,
        barrier = models_converged_barrier,
        boundary = models_converged_boundary,
        simple_no_max = models_converged_simple_no_max,
        barrier_no_max = models_converged_barrier_no_max,
        boundary_no_max = models_converged_boundary_no_max
      ),
      cv_outputs = list(
        simple = cv_outputs_simple,
        barrier = cv_outputs_barrier,
        boundary = cv_outputs_boundary,
        simple_no_max = cv_outputs_simple_no_max,
        barrier_no_max = cv_outputs_barrier_no_max,
        boundary_no_max = cv_outputs_boundary_no_max
      ),
      all_params = all_params,
      all_cv_outputs = all_cv_outputs,
      plots = list(
        meshes_simple = plots_meshes_simple,
        meshes_barrier = plots_meshes_barrier,
        meshes_boundary = plots_meshes_boundary,
        meshes_simple_no_max = plots_meshes_simple_no_max,
        meshes_barrier_no_max = plots_meshes_barrier_no_max,
        meshes_boundary_no_max = plots_meshes_boundary_no_max
      )
    )
  }
}


saveRDS(msa, file.path(new_repertory_msa, paste0(sp_name_safe, "_", "MSA_results.rds")))






