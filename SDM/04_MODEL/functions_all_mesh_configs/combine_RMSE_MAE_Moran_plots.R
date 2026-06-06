
combine_simulation_errors_all_species <- function(errors_all_species,
                                                  table = c("metrics", "moran_results")) {
  table <- match.arg(table)
  
  purrr::imap_dfr(errors_all_species, function(errors_by_region, species_name) {
    purrr::imap_dfr(errors_by_region, function(region_obj, region_name) {
      region_obj[[table]] %>%
        mutate(
          species = species_name,
          .before = 1
        )
    })
  })
}


combine_simulation_errors_from_folders <- function(parent_dir,
                                                   species = species_list,
                                                   recursive = FALSE,
                                                   table = c("metrics", "moran_results")) {
  
  table <- match.arg(table)
  
  species_dirs <- list.dirs(parent_dir, recursive = recursive, full.names = TRUE)
  species_dirs <- species_dirs[basename(species_dirs) %in% unlist(species)]
  
  errors_list <- list()
  
  for (species_dir in species_dirs) {
    sp_name <- basename(species_dir)
    
    file_path_rds <- file.path(
      species_dir,
      paste0(sp_name, "_simulation_errors_by_region.rds")
    )
    
    file_path_no_ext <- file.path(
      species_dir,
      paste0(sp_name, "_simulation_errors_by_region")
    )
    
    if (file.exists(file_path_rds)) {
      errors_list[[sp_name]] <- readRDS(file_path_rds)
    } else if (file.exists(file_path_no_ext)) {
      errors_list[[sp_name]] <- readRDS(file_path_no_ext)
    } else {
      warning("Missing file : ", file_path_rds)
    }
  }
  
  if (length(errors_list) == 0) {
    stop("No file found *_simulation_errors_by_region(.rds)")
  }
  
  combine_simulation_errors_all_species(errors_list, table = table)
}

simulation_metrics_df <- combine_simulation_errors_from_folders(
  parent_dir = "~/EBESCO/SDM/05_OUTPUTS/OUTPUTS_ALL_MESH_FAMILY_DATARMOR",
  species = species_list,
  recursive = FALSE,
  table = "metrics"
)

simulation_moran_df <- combine_simulation_errors_from_folders(
  parent_dir = "~/EBESCO/SDM/05_OUTPUTS/OUTPUTS_ALL_MESH_FAMILY_DATARMOR",
  species = species_list,
  recursive = FALSE,
  table = "moran_results"
)



# ------------------------------------------------------------------------------#
# PLOT RMSE, MAE
# ------------------------------------------------------------------------------#

simulation_metrics_east_biomass_df <- simulation_metrics_df %>%
  filter(region == "east", response == "totalWeightKg") %>%
  dplyr::select(-RMSE)

species_labs <- setNames(
  paste0("italic('", stringr::str_replace(unique(simulation_metrics_east_biomass_df$species), "_", " "), "')"),
  unique(simulation_metrics_east_biomass_df$species)
)

cutoff_labs <- c("cutoff_3"  = "Mesh M1",
                 "cutoff_5"  = "Mesh M2",
                 "cutoff_7"  = "Mesh M3",
                 "cutoff_10"  = "Mesh M4",
                 "cutoff_15"  = "Mesh M5")

MAE_filter <- simulation_metrics_east_biomass_df %>%
  filter(!mesh %in% c("cutoff_1", "cutoff_2", "cutoff_20"))%>%
  filter(species %in% c("Zeus_faber", "Solea_solea", "Trachurus_trachurus", "Conger_conger"))

ggplot(MAE_filter,
       aes(x = family, y = MAE, colour = family, fill = family)) +
  geom_boxplot(alpha = 0.4, outlier.alpha = 0.2) +
  facet_grid(species ~ mesh, scales = "free_y",
             labeller = labeller(species = as_labeller(species_labs, label_parsed),
                                 mesh = as_labeller(cutoff_labs))) +
  scale_colour_manual(values = family_colors) +
  scale_fill_manual(values = family_colors) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(x = "Family", y = "Mean Absolute Error (kg)")


MAE_filter %>% filter(species == "Solea_solea")%>%
  mutate(mesh = factor(mesh,
                       levels = c("cutoff_3", "cutoff_5", "cutoff_7", "cutoff_10", "cutoff_15")))%>%
                       ggplot(aes(x = family, y = MAE, colour = family, fill = family)) +
  geom_boxplot(alpha = 0.4, outlier.alpha = 0.2) +
  facet_wrap(~ mesh, scales = "free_y", nrow=1,
             labeller = labeller(species = as_labeller(species_labs, label_parsed),
                                 mesh = as_labeller(cutoff_labs))) +
  scale_colour_manual(values = family_colors) +
  scale_fill_manual(values = family_colors) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(x = "Family", y = "Mean Absolute Error (kg)")


# ------------------------------------------------------------------------------#
# PLOT MORAN
# ------------------------------------------------------------------------------#

simulation_moran_east_biomass_df <- simulation_moran_df %>%
  filter(region == "east",
         response == "totalWeightKg")

species_labs <- setNames(
  paste0("italic('", stringr::str_replace(unique(simulation_moran_east_biomass_df$species), "_", " "), "')"),
  unique(simulation_moran_east_biomass_df$species)
)

moran_summary <- simulation_moran_east_biomass_df %>%
  group_by(species, year, mesh, family) %>%
  summarise(
    n_sim = n(),
    n_sig = sum(moran_p.value < 0.05, na.rm = TRUE),
    prop_sig = mean(moran_p.value < 0.05, na.rm = TRUE),
    mean_moran = mean(moran_observed, na.rm = TRUE),
    .groups = "drop"
  )%>%
  filter(!mesh %in% c("cutoff_1", "cutoff_2", "cutoff_20"))%>%
  filter(species %in% c("Zeus_faber", "Solea_solea", "Trachurus_trachurus", "Conger_conger"))

ggplot(moran_summary,
       aes(x = year, y = family, fill = prop_sig)) +
  geom_tile(color = "white") +
  facet_grid(species ~ mesh, scales = "free_y", space = "free_y",
             labeller = labeller(
               species = as_labeller(species_labs, label_parsed),
               mesh = as_labeller(cutoff_labs))) +
  ggplot2:: scale_fill_distiller(palette = "RdYlBu")+
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(x = "Year", y = "Family")

library(ggplot2)
library(dplyr)
library(scales)

moran_summary_plot <- moran_summary %>%
  mutate(
    prop_sig_pct = 100 * prop_sig,
    family = factor(family, levels = names(family_colors)),
    mesh = factor(mesh, levels = mesh_levels, labels = mesh_labs)
  )

ggplot(
  moran_summary_plot,
  aes(x = year, y = family, fill = prop_sig_pct)
) +
  geom_tile(color = "white", linewidth = 0.35) +
  geom_text(
    aes(label = sprintf("%.0f", prop_sig_pct)),
    size = 3.2,
    colour = "black"
  ) +
  facet_grid(
    species ~ mesh,
    scales = "free_y",
    space = "free_y",
    labeller = labeller(
      species = as_labeller(species_labs, label_parsed),
      mesh = as_labeller(mesh_labs)
    )
  ) +
  ggplot2:: scale_fill_distiller(palette = "RdYlBu")+
  
  scale_x_discrete(expand = c(0, 0)) +
  labs(
    x = "Year",
    y = "Family",
    fill = "% Moran I \nsignificatif"
  ) +
  theme_bw(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    panel.spacing = unit(0.6, "lines"),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )



