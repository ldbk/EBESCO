#------------------------------------------------------------------------------#
# Combine and plot results of cAIC and cross-validation of all species
#------------------------------------------------------------------------------#

# common params 

family_colors <- c("tweedie" = "#66c2a5",
                   "deltagamma" = "#fc8d62",
                   "deltalognormal" = "#99CCFF",
                   "deltagammapoissonlink" = "#e78ac3",
                   "deltalognormalpoissonlink" = "#8da0cb",
                   "gamma" = "#ffd92f",
                   "lognormal" = "#a6d854")

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


#------------------------------------------------------------------------------#
# Combine and plot cAIC weights for all species, region, responses 
#------------------------------------------------------------------------------#

combine_AIC_by_region_all_species <- function(AIC_all_species) {
    purrr::imap_dfr(AIC_all_species, function(AIC_by_region, species_name) {
      purrr::imap_dfr(AIC_by_region, function(df_region_species, region_name) {
        as.data.frame(df_region_species) %>%
          mutate(species = species_name, region  = region_name)
    })
  }) %>%
    relocate(species, region, response, family, AIC, deltaAIC, weight)
}


combine_AIC_from_folders <- function(parent_dir, species = species_list, recursive = FALSE) {
  
  species_dirs <- list.dirs(parent_dir, recursive = recursive, full.names = TRUE)
  species_dirs <- species_dirs[basename(species_dirs) %in% unlist(species)]
  aic_list <- list()
  
  for (species_dir in species_dirs) {
    sp_name <- basename(species_dir)
    aic_path <- file.path(species_dir, paste0(sp_name, "_AIC_by_region.rds"))
    if (file.exists(aic_path)) aic_list[[sp_name]] <- readRDS(aic_path)
  }
  combine_AIC_by_region_all_species(aic_list)
}

AIC_all_df <- combine_AIC_from_folders(
  parent_dir ="~/EBESCO/SDM/05_OUTPUTS/MODELS_OUTPUTS_DATARMOR",
  species = species_list,
  recursive = FALSE)


ggplot(AIC_all_df, aes(x = weight*100, y = species, 
                       shape = family, fill = family)) +
  geom_point(size = 4, alpha = 0.85, color = "grey30")+
  scale_shape_manual(values = family_shapes)+
  scale_fill_manual(values = family_colors)+
  scale_x_continuous(limits = c(0, 100)) +
  facet_grid(response ~ region) +
  labs(x = "AIC weight (%)", fill = "Family", shape = "Family") +
  theme_bw() +
  theme(legend.position = "top",
        axis.text.y = element_text(face = "italic")) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE),
         fill  = guide_legend(nrow = 2, byrow = TRUE),
         shape = guide_legend(nrow = 2, byrow = TRUE))


#------------------------------------------------------------------------------#
# Combine and plot random CV results for all species, region, responses 
#------------------------------------------------------------------------------#

combine_randomCV_by_region_all_species <- function(randomCV_all_species) {
  purrr::imap_dfr(randomCV_all_species, function(randomCV_by_region, species_name) {
    purrr::imap_dfr(randomCV_by_region, function(XX, region_name) {
      df_region_species <- XX$cv_scores
      as.data.frame(df_region_species) %>%
        mutate(species = species_name, region  = region_name)
    })
  }) %>%
    relocate(species, region, response, family, sum_loglik, rmse, mae)
}


combine_randomCV_from_folders <- function(parent_dir, species = species_list, recursive = FALSE) {
  
  species_dirs <- list.dirs(parent_dir, recursive = recursive, full.names = TRUE)
  species_dirs <- species_dirs[basename(species_dirs) %in% unlist(species)]
  randomCV_list <- list()
  
  for (species_dir in species_dirs) {
    sp_name <- basename(species_dir)
    randomCV_path <- file.path(species_dir, paste0(sp_name, "_randomCV_by_region.rds"))
    if (file.exists(randomCV_path)) randomCV_list[[sp_name]] <- readRDS(randomCV_path)
  }
  combine_randomCV_by_region_all_species(randomCV_list)
}

randomCV_all_df <- combine_randomCV_from_folders(
  parent_dir ="~/EBESCO/SDM/05_OUTPUTS/MODELS_OUTPUTS_DATARMOR",
  species = species_list,
  recursive = FALSE)

randomCV_delta_all_df <- randomCV_all_df %>%
  mutate(region = factor(region, levels = c("west", "east"))) %>%
  group_by(species, region, response) %>%
  mutate(delta_sumLL = sum_loglik - max(sum_loglik))%>%
  ungroup()

ggplot(randomCV_delta_all_df, aes(x = delta_sumLL, y = species, 
                                  shape = family, fill = family)) +
  geom_point(size = 4, alpha = 0.85, color = "grey30")+
  scale_shape_manual(values = family_shapes)+
  scale_fill_manual(values = family_colors)+
  facet_grid(response ~ region) +
  labs(x = "Random CV sum loglik", shape = "Family", fill = "Family") +
  theme_bw() +
  theme(legend.position = "top",
        axis.text.y = element_text(face = "italic")) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE),
         fill  = guide_legend(nrow = 2, byrow = TRUE),
         shape = guide_legend(nrow = 2, byrow = TRUE))


#------------------------------------------------------------------------------#
# Combine and plot spatio-temporal blocked CV results for all species, region, responses 
#------------------------------------------------------------------------------#

combine_blockedCV_by_region_all_species <- function(blockedCV_all_species) {
  purrr::imap_dfr(blockedCV_all_species, function(blockedCV_by_region, species_name) {
    purrr::imap_dfr(blockedCV_by_region, function(XX, region_name) {
      df_region_species <- XX$cv_scores
      as.data.frame(df_region_species) %>%
        mutate(species = species_name, region  = region_name)
    })
  }) %>%
    relocate(species, region, response, family, sum_loglik, rmse, mae)
}


combine_blockedCV_from_folders <- function(parent_dir, species = species_list, recursive = FALSE) {
  
  species_dirs <- list.dirs(parent_dir, recursive = recursive, full.names = TRUE)
  species_dirs <- species_dirs[basename(species_dirs) %in% unlist(species)]
  blockedCV_list <- list()
  
  for (species_dir in species_dirs) {
    sp_name <- basename(species_dir)
    blockedCV_path <- file.path(species_dir, paste0(sp_name, "_blockedCV_by_region.rds"))
    if (file.exists(blockedCV_path)) blockedCV_list[[sp_name]] <- readRDS(blockedCV_path)
  }
  combine_blockedCV_by_region_all_species(blockedCV_list)
}

blockedCV_all_df <- combine_blockedCV_from_folders(
  parent_dir ="~/EBESCO/SDM/05_OUTPUTS/MODELS_OUTPUTS_DATARMOR",
  species = species_list,
  recursive = FALSE)

blockedCV_delta_all_df <- blockedCV_all_df %>%
  mutate(region = factor(region, levels = c("west", "east"))) %>%
  group_by(species, region, response) %>%
  mutate(delta_sumLL = sum_loglik - max(sum_loglik))%>%
  ungroup()

ggplot(blockedCV_delta_all_df, aes(x = delta_sumLL, y = species, 
                                  shape = family, fill = family)) +
  geom_point(size = 4, alpha = 0.85, color = "grey30")+
  scale_shape_manual(values = family_shapes)+
  scale_fill_manual(values = family_colors)+
  facet_grid(response ~ region) +
  labs(x = "blocked CV sum loglik", shape = "Family", fill = "Family") +
  theme_bw() +
  theme(legend.position = "top",
        axis.text.y = element_text(face = "italic")) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE),
         fill  = guide_legend(nrow = 2, byrow = TRUE),
         shape = guide_legend(nrow = 2, byrow = TRUE))



AIC_plot_df <- AIC_all_df %>%
  transmute(species, region, response, family,
            metric = "cAIC weight (%)",
            value  = weight * 100)

blockedCV_plot_df <- blockedCV_delta_all_df %>%
  transmute(species, region, response, family,
            metric = "ΔΣLL blocked CV",
            value = pmax(delta_sumLL, -100))

randomCV_plot_df <- randomCV_delta_all_df %>%
  transmute(species, region, response, family,
            metric = "ΔΣLL random CV",
            value = pmax(delta_sumLL, -100))

CV_cAIC_df <- bind_rows(AIC_plot_df, blockedCV_plot_df, randomCV_plot_df) %>%
  mutate(response = factor(response, levels = c("totalWeightKg", "densityKgKm2")),
         metric = factor(metric, levels = c("cAIC weight (%)", 
                                            "ΔΣLL random CV",
                                            "ΔΣLL blocked CV")))


CV_cAIC_df <- CV_cAIC_df %>%
  mutate(region = factor(region, levels = c("west", "east"))) %>%
  # filter(!species %in% c("Capros_aper", "Engraulis_encrasicolus")) %>%
  mutate(species = str_replace_all(species, "_", " "),
         species = factor(species, levels = rev(sort(unique(species)))))

CV_cAIC_df_west <- CV_cAIC_df %>% filter(region == "west")
CV_cAIC_df_east <- CV_cAIC_df %>% filter(region == "east")

bg_species <- CV_cAIC_df %>%
  distinct(species)%>%
  arrange(species) %>%
  mutate(species_rank = row_number(),
         bg_fill = ifelse(species_rank %% 2 == 0, "grey90", "white"),
         ymin = as.numeric(species) - 0.5, 
         ymax = as.numeric(species) + 0.5, 
         xmin = -Inf,
         xmax = Inf)


ggplot(CV_cAIC_df, aes(x = value, y = species, fill = family, shape = family)) +
  geom_rect(data = bg_species,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = bg_fill),
            inherit.aes = FALSE, alpha = 1, color = NA) +
  scale_fill_identity()+
  ggnewscale::new_scale_fill() +
  geom_point(aes(fill = family, shape = family), size = 4, alpha = 0.8) +
  scale_fill_manual(values = family_colors, name = "Family") +
  scale_shape_manual(values = family_shapes, name = "Family") +
  facet_grid(region + response ~ metric, scales = "free_x",
             labeller = labeller(response = c(totalWeightKg = "Biomass (kg)",
                                              densityKgKm2 = "Density (kg/km²)"))) +
  labs(x = NULL, y = NULL,  fill = "Family", shape = "Family") +
  theme_bw() +
  theme(legend.position = "top",
        strip.background = element_rect(fill = "white"),
        panel.spacing = unit(0.8, "lines"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_text(face = "italic"))
  # guides(color = guide_legend(nrow = 2, byrow = TRUE),
  #        fill  = guide_legend(nrow = 2, byrow = TRUE),
  #        shape = guide_legend(nrow = 2, byrow = TRUE))



