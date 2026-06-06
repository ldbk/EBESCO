



family_colors <- c("tweedie" = "#66c2a5",
                   "deltagamma" = "#fc8d62",
                   "deltalognormal" = "#99CCFF",
                   "deltagammapoissonlink" = "#e78ac3",
                   "deltalognormalpoissonlink" = "#a6d854",
                   "gamma" = "#ffd92f",
                   "lognormal" = "#8da0cb")

family_shapes <- c("tweedie" = 21,
                   "deltagamma" = 22,
                   "deltalognormal" = 23,
                   "deltagammapoissonlink" = 24,
                   "deltalognormalpoissonlink" = 25,
                   "gamma" = 21,
                   "lognormal" = 22)

species_list <- list("Capros_aper",
                     "Chelidonichthys_cuculus",     
                     "Chelidonichthys_lucerna", 
                     "Conger_conger",
                     "Dicentrarchus_labrax",
                     "Engraulis_encrasicolus",
                     "Micromesistius_poutassou",
                     "Hippocampus_hippocampus",
                     "Trachurus_trachurus",
                     "Solea_solea",
                     "Zeus_faber")


combine_index_by_region_all_species <- function(index_all_species) {
  purrr::imap_dfr(index_all_species, function(index_by_region, species_name) {
    purrr::imap_dfr(index_by_region, function(index_by_cutoff, region_name) {
      purrr::imap_dfr(index_by_cutoff, function(index_by_response, cutoff_name) {
        purrr::imap_dfr(index_by_response, function(index_by_family, response_name) {
          purrr::imap_dfr(index_by_family, function(model_obj, family_name) {
            as.data.frame(model_obj$index) %>%
              mutate(species = species_name,
                     region = region_name,
                     cutoff = cutoff_name,
                     response = response_name,
                     family = family_name)
          })
        })
      })
    })
  }) %>%
  relocate(species, region, cutoff, response, family, year, est, lwr, upr)
}


combine_index_from_folders <- function(parent_dir,
                                       species = species_list,
                                       recursive = FALSE) {
  
  species_dirs <- list.dirs(parent_dir, recursive = recursive, full.names = TRUE)
  species_dirs <- species_dirs[basename(species_dirs) %in% unlist(species)]
  
  index_list <- list()
  
  for (species_dir in species_dirs) {
    sp_name <- basename(species_dir)
    pred_path <- file.path(species_dir, 
                           paste0(sp_name, "_converged_models_predictions.rds"))
    
    if (file.exists(pred_path)) {
      index_list[[sp_name]] <- readRDS(pred_path)
    } else {
      warning("Missing file : ", pred_path)
    }
  }
  
  if (length(index_list) == 0) {
    stop("No file found *_converged_models_predictions.rds")
  }
    
    combine_index_by_region_all_species(index_list)
  
  }
  



index_all_df <- combine_index_from_folders(
  parent_dir = "~/EBESCO/SDM/05_OUTPUTS/OUTPUTS_ALL_MESH_FAMILY_DATARMOR",
  species = species_list,
  recursive = FALSE
)

index_east_biomass_df <- index_all_df %>%
  filter(region == "east", response == "totalWeightKg")


species_labs <- setNames(
  paste0("italic('", str_replace(unique(index_east_biomass_df$species), "_", " "), "')"),
  unique(index_east_biomass_df$species)
)

cutoff_labs <- c("cutoff_1"  = "Mesh M1 (cutoff = 1)",
                 "cutoff_2"  = "Mesh M2 (cutoff = 2)",
                 "cutoff_3"  = "Mesh M3 (cutoff = 3)",
                 "cutoff_5"  = "Mesh M4 (cutoff = 5)",
                 "cutoff_7"  = "Mesh M5 (cutoff = 7)",
                 "cutoff_10" = "Mesh M6 (cutoff = 10)",
                 "cutoff_15" = "Mesh M7 (cutoff = 15)",
                 "cutoff_20" = "Mesh M8 (cutoff = 20)")



ggplot(index_east_biomass_df, aes(x = year, y = (est / 1000),
           colour = family, fill = family, group = family)) +
  geom_ribbon(aes(ymin = (lwr / 1000), ymax = (upr / 1000)),
              alpha = 0.3, colour = NA) +
  geom_line() +
  geom_point(size = 2.5) +
  facet_grid(species ~ cutoff, scales = "free_y",
    labeller = labeller(species = as_labeller(species_labs, label_parsed),
                        cutoff = as_labeller(cutoff_labs))) +
  scale_colour_manual(values = family_colors) +
  scale_fill_manual(values = family_colors) +
  scale_shape_manual(values = family_shapes) +
  theme_bw() +
  labs(x = "Year", y = "Biomass index (tons)", colour = "Distribution",
       fill = "Distribution")





mesh_levels <- c("cutoff_3", "cutoff_5", "cutoff_7", "cutoff_10", "cutoff_15")

mesh_labs <- c(
  "cutoff_3"  = "Mesh M1",
  "cutoff_5"  = "Mesh M2",
  "cutoff_7"  = "Mesh M3",
  "cutoff_10" = "Mesh M4",
  "cutoff_15" = "Mesh M5"
)

index_east_biomass_df_plot <- index_east_biomass_df %>%
  filter(species %in% c("Zeus_faber", "Solea_solea", "Trachurus_trachurus", "Conger_conger"))%>%
  filter(cutoff %in% mesh_levels) %>%
  mutate(cutoff = factor(cutoff, levels = mesh_levels))


ggplot(index_east_biomass_df_plot,
       aes(x = year, y = est / 1000,
           colour = family, fill = family, group = family)) +
  geom_ribbon(aes(ymin = lwr / 1000, ymax = upr / 1000),
              alpha = 0.3, colour = NA) +
  geom_line() +
  geom_point(size = 2.5) +
  facet_grid(
    species ~ cutoff,
    scales = "free_y",
    labeller = labeller(
      species = as_labeller(species_labs, label_parsed),
      cutoff = as_labeller(mesh_labs)
    )
  ) +
  scale_colour_manual(values = family_colors) +
  scale_fill_manual(values = family_colors) +
  scale_shape_manual(values = family_shapes) +
  theme_bw() +
  labs(
    x = "Year",
    y = "Biomass index (tons)",
    colour = "Distribution",
    fill = "Distribution"
  )





index_east_biomass_df_plot <- index_east_biomass_df %>%
  filter(species %in% c("Solea_solea"))%>%
  filter(cutoff %in% mesh_levels) %>%
  mutate(cutoff = factor(cutoff, levels = mesh_levels))%>%
  filter(family %in% c("deltagamma", "deltalognormal"))


ggplot(index_east_biomass_df_plot,
       aes(x = year, y = est / 1000,
           colour = family, fill = family, group = family)) +
  geom_ribbon(aes(ymin = lwr / 1000, ymax = upr / 1000),
              alpha = 0.3, colour = NA) +
  geom_line() +
  geom_point(size = 2.5) +
  facet_wrap(
    ~ cutoff,
     nrow=1,
    labeller = labeller(
      cutoff = as_labeller(mesh_labs))
  ) +
  scale_colour_manual(values = family_colors) +
  scale_fill_manual(values = family_colors) +
  scale_shape_manual(values = family_shapes) +
  theme_bw() +
  labs(
    x = "Year",
    y = "Biomass index (tons)",
    colour = "Distribution",
    fill = "Distribution"
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
