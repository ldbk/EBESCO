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

species_list <- list("Solea_solea", "Zeus_faber")


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
  parent_dir ="~/EBESCO/SDM/05_OUTPUTS/OUTPUTS_ALL_MESH_FAMILY_DATARMOR",
  species = species_list,
  recursive = FALSE)

AIC_east_biomass <- AIC_all_df %>% filter(region == "east", response == "totalWeightKg")



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
  parent_dir ="~/EBESCO/SDM/05_OUTPUTS/OUTPUTS_ALL_MESH_FAMILY_DATARMOR",
  species = species_list,
  recursive = FALSE)

randomCV_delta_all_df <- randomCV_all_df %>%
  mutate(region = factor(region, levels = c("west", "east"))) %>%
  group_by(species, region, response, mesh) %>%
  mutate(delta_sumLL = sum_loglik - max(sum_loglik, na.rm = TRUE)) %>%
  ungroup()

randomCV_east_biomass <- randomCV_delta_all_df %>% filter(region == "east", response == "totalWeightKg")


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
  parent_dir ="~/EBESCO/SDM/05_OUTPUTS/OUTPUTS_ALL_MESH_FAMILY_DATARMOR",
  species = species_list,
  recursive = FALSE)

blockedCV_delta_all_df <- blockedCV_all_df %>%
  mutate(region = factor(region, levels = c("west", "east"))) %>%
  group_by(species, region, response, mesh) %>%
  mutate(delta_sumLL = sum_loglik - max(sum_loglik, na.rm = TRUE)) %>%
  ungroup()

blockedCV_east_biomass <- blockedCV_delta_all_df %>%
  filter(region == "east", response == "totalWeightKg")




#------------------------------------------------------------------------------#
# PLOT ALL
#------------------------------------------------------------------------------#

AIC_plot_df <- AIC_east_biomass %>%
  transmute(species, region, response, family, mesh,
            metric = "cAIC weight (%)",
            value  = weight * 100)

blockedCV_plot_df <- blockedCV_east_biomass %>%
  transmute(species, region, response, family, mesh,
            metric = "ΔΣLL blocked CV",
            value = pmax(delta_sumLL, -100))

randomCV_plot_df <- randomCV_east_biomass %>%
  transmute(species, region, response, family, mesh,
            metric = "ΔΣLL random CV",
            value = pmax(delta_sumLL, -100))

cutoff_labs <- c("cutoff_1"  = "Mesh M1 (cutoff = 1)",
                 "cutoff_2"  = "Mesh M2 (cutoff = 2)",
                 "cutoff_3"  = "Mesh M3 (cutoff = 3)",
                 "cutoff_5"  = "Mesh M4 (cutoff = 5)",
                 "cutoff_7"  = "Mesh M5 (cutoff = 7)",
                 "cutoff_10" = "Mesh M6 (cutoff = 10)",
                 "cutoff_15" = "Mesh M7 (cutoff = 15)",
                 "cutoff_20" = "Mesh M8 (cutoff = 20)")

CV_cAIC_df <- bind_rows(AIC_plot_df, blockedCV_plot_df, randomCV_plot_df) %>%
  mutate(metric = factor(metric, 
                         levels = c("cAIC weight (%)", "ΔΣLL random CV", "ΔΣLL blocked CV")),
    species_clean = str_replace_all(species, "_", " "),
    species_clean = factor(species_clean, levels = rev(sort(unique(species_clean)))),
    mesh = factor(mesh, levels = names(cutoff_labs), labels = cutoff_labs))

species_labs <- setNames(paste0("italic('", levels(CV_cAIC_df$species_clean), "')"),
                         levels(CV_cAIC_df$species_clean))

CV_cAIC_df2 <- CV_cAIC_df %>%
  group_by(species_clean, metric) %>%
  mutate(mesh_pos = dense_rank(mesh)) %>%
  ungroup()

bg_mesh <- CV_cAIC_df2 %>%
  distinct(species_clean, metric, mesh_pos) %>%
  filter(mesh_pos %% 2 == 0) %>%
  mutate(ymin = mesh_pos - 0.5, ymax = mesh_pos + 0.5, xmin = -Inf, xmax = Inf)

ggplot(CV_cAIC_df2, aes(x = value, y = mesh, fill = family, shape = family)) +
  geom_rect(data = bg_mesh, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            inherit.aes = FALSE, fill = "grey90", colour = NA) +
  geom_point(size = 4, alpha = 0.85, colour = "black") +
  scale_fill_manual(values = family_colors, name = "Family") +
  scale_shape_manual(values = family_shapes, name = "Family") +
  facet_grid(species_clean ~ metric, scales = "free_x",
             labeller = labeller(species_clean = as_labeller(species_labs, label_parsed))) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(legend.position = "top",
        strip.background = element_rect(fill = "white"),
        panel.spacing = unit(0.8, "lines"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())






AIC_plot_df <- AIC_east_biomass %>%
  transmute(
    species, region, response, family, mesh,
    metric = "cAIC weight (%)",
    value  = weight * 100
  )

randomCV_plot_df <- randomCV_east_biomass %>%
  transmute(
    species, region, response, family, mesh,
    metric = "ΔΣLL random CV",
    value = pmax(delta_sumLL, -100)
  )

blockedV_plot_df <- blockedCV_east_biomass %>%
  transmute(
    species, region, response, family, mesh,
    metric = "ΔΣLL random CV",
    value = pmax(delta_sumLL, -100)
  )

mesh_levels <- c(
  "cutoff_3",
  "cutoff_5",
  "cutoff_7",
  "cutoff_10",
  "cutoff_15"
)

mesh_labs <- c(
  "cutoff_3"  = "Mesh M1",
  "cutoff_5"  = "Mesh M2",
  "cutoff_7"  = "Mesh M3",
  "cutoff_10" = "Mesh M4",
  "cutoff_15" = "Mesh M5"
)

CV_cAIC_df <- bind_rows(
  AIC_plot_df,
  randomCV_plot_df
) %>%
  filter(species %in% c("Zeus_faber", "Solea_solea", "Trachurus_trachurus", "Conger_conger"))%>%
  filter(mesh %in% mesh_levels) %>%
  mutate(
    metric = factor(
      metric,
      levels = c(
        "cAIC weight (%)",
        "ΔΣLL random CV"
      )
    ),
    species_clean = str_replace_all(species, "_", " "),
    species_clean = factor(
      species_clean,
      levels = c("Conger conger", "Solea solea","Trachurus trachurus","Zeus faber")
    ),
    mesh = factor(
      mesh,
      levels = rev(mesh_levels),
      labels = rev(mesh_labs)
    )
  )

species_labs <- setNames(
  paste0(
    "italic('",
    levels(CV_cAIC_df$species_clean),
    "')"
  ),
  levels(CV_cAIC_df$species_clean)
)

CV_cAIC_df2 <- CV_cAIC_df %>%
  group_by(species_clean, metric) %>%
  mutate(mesh_pos = dense_rank(mesh)) %>%
  ungroup()

bg_mesh <- CV_cAIC_df2 %>%
  distinct(species_clean, metric, mesh_pos) %>%
  filter(mesh_pos %% 2 == 0) %>%
  mutate(
    ymin = mesh_pos - 0.5,
    ymax = mesh_pos + 0.5,
    xmin = -Inf,
    xmax = Inf
  )

ggplot(
  CV_cAIC_df2,
  aes(
    x = value,
    y = mesh,
    fill = family,
    shape = family
  )
) +
  geom_rect(
    data = bg_mesh,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    ),
    inherit.aes = FALSE,
    fill = "grey90",
    colour = NA
  ) +
  geom_point(
    size = 4,
    alpha = 0.85,
    colour = "black"
  ) +
  scale_fill_manual(
    values = family_colors,
    name = "Family"
  ) +
  scale_shape_manual(
    values = family_shapes,
    name = "Family"
  ) +
  facet_grid(
    species_clean ~ metric,
    scales = "free_x",
    labeller = labeller(
      species_clean = as_labeller(
        species_labs,
        label_parsed
      )
    )
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_bw() +
  theme(
    legend.position = "top",
    panel.spacing = unit(0.8, "lines"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )










CV_cAIC_df2 %>% filter(species == "Solea_solea") %>% ggplot(
  aes(
    x = value,
    y = mesh,
    fill = family,
    shape = family
  )
) +
  geom_rect(
    data = bg_mesh,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    ),
    inherit.aes = FALSE,
    fill = "grey90",
    colour = NA
  ) +
  geom_point(
    size = 4,
    alpha = 0.85,
    colour = "black"
  ) +
  scale_fill_manual(
    values = family_colors,
    name = "Family"
  ) +
  scale_shape_manual(
    values = family_shapes,
    name = "Family"
  ) +
  facet_wrap(
    ~ metric,
    scales = "free_x",
    nrow=1
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    panel.spacing = unit(0.8, "lines"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )