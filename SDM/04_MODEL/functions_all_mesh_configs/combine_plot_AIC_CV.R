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

species_list <- list("Chelidonichthys_cuculus",     
                     "Chelidonichthys_lucerna", 
                     "Conger_conger",
                     "Dicentrarchus_labrax",
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
    aic_path <- file.path(species_dir, paste0(sp_name, "_AIC_weights.rds"))
    if (file.exists(aic_path)) aic_list[[sp_name]] <- readRDS(aic_path)
  }
  combine_AIC_by_region_all_species(aic_list)
}

AIC_all_df <- combine_AIC_from_folders(
  parent_dir ="~/EBESCO/SDM/05_OUTPUTS/OUTPUTS_ALL_MESH_FAMILY_DATARMOR/noAIC",
  species = species_list,
  recursive = FALSE)

AIC_east_biomass <- AIC_all_df %>% filter(region == "east", response == "totalWeightKg")


AIC_delta_all_df <- AIC_all_df %>%
  group_by(species, region, response) %>%
  mutate(deltaAIC_species = AIC - min(AIC),
         weight_species = exp(-0.5 * deltaAIC_species) / sum(exp(-0.5 * deltaAIC_species))) %>%
  ungroup()

#------------------------------------------------------------------------------#
# Combine and plot random CV results for all species, region, responses 
#------------------------------------------------------------------------------#

combine_randomCV_by_region_all_species <- function(randomCV_all_species) {
  purrr::imap_dfr(randomCV_all_species, function(randomCV_by_region, species_name) {
    purrr::imap_dfr(randomCV_by_region, function(randomCV_region, region_name) {
     
      cv_scores <- as.data.frame(randomCV_region$cv_scores)
      
      cv_summary <- purrr::imap_dfr(randomCV_region$cv_outputs, function(mesh_list, mesh) {
        purrr::imap_dfr(mesh_list, function(response_list, response) {
          purrr::imap_dfr(response_list, function(cv_object, family) {
            
            tibble::tibble(
              mesh               = mesh,
              response           = response,
              family             = family,
              n_folds_tot        = length(cv_object$pdHess),
              n_converged        = if (is.null(cv_object$pdHess)) NA_integer_ else sum(cv_object$pdHess),
              pourcent_converged = if (is.null(cv_object$pdHess)) NA_real_   else 100 * mean(cv_object$pdHess),
              all_converged      = if (is.null(cv_object$pdHess)) NA         else cv_object$converged
            )
            
          })
        })
      })
      
      cv_scores %>%
        dplyr::left_join(cv_summary, by = c("mesh", "response", "family")) %>%
        dplyr::mutate(species = species_name, region = region_name)
    })
  }) %>%
    dplyr::relocate(species, region, mesh, response, family, 
                    sum_loglik, rmse, mae,
                    n_folds_tot, n_converged, pourcent_converged, all_converged)
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
  parent_dir ="~/EBESCO/SDM/05_OUTPUTS/OUTPUTS_ALL_MESH_FAMILY_DATARMOR/noAIC",
  species = species_list,
  recursive = FALSE)

randomCV_delta_all_df <- randomCV_all_df %>%
  mutate(region = factor(region, levels = c("west", "east"))) %>%
  group_by(species, region, response) %>%
  mutate(delta_sumLL = sum_loglik - max(sum_loglik, na.rm = TRUE)) %>%
  ungroup()

randomCV_east_biomass <- randomCV_delta_all_df %>% 
  filter(region == "east", response == "totalWeightKg")%>%
  filter(all_converged)


#------------------------------------------------------------------------------#
# Combine and plot spatio-temporal blocked CV results for all species, region, responses 
#------------------------------------------------------------------------------#

combine_blockedCV_by_region_all_species <- function(blockedCV_all_species) {
  purrr::imap_dfr(blockedCV_all_species, function(blockedCV_by_region, species_name) {
    purrr::imap_dfr(blockedCV_by_region, function(blockedCV_by_region, region_name) {
      
      cv_scores <- as.data.frame(blockedCV_by_region$cv_scores)
      
      cv_summary <- purrr::imap_dfr(blockedCV_by_region$cv_outputs, function(mesh_list, mesh) {
        purrr::imap_dfr(mesh_list, function(response_list, response) {
          purrr::imap_dfr(response_list, function(cv_object, family) {
            
            tibble::tibble(
              mesh               = mesh,
              response           = response,
              family             = family,
              n_folds_tot        = length(cv_object$pdHess),
              n_converged        = if (is.null(cv_object$pdHess)) NA_integer_ else sum(cv_object$pdHess),
              pourcent_converged = if (is.null(cv_object$pdHess)) NA_real_   else 100 * mean(cv_object$pdHess),
              all_converged      = if (is.null(cv_object$pdHess)) NA         else cv_object$converged
            )
            
          })
        })
      })
      
      cv_scores %>%
        dplyr::left_join(cv_summary, by = c("mesh", "response", "family")) %>%
        dplyr::mutate(species = species_name, region = region_name)
    })
  }) %>%
    dplyr::relocate(species, region, mesh, response, family, 
                    sum_loglik, rmse, mae,
                    n_folds_tot, n_converged, pourcent_converged, all_converged)
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
  parent_dir ="~/EBESCO/SDM/05_OUTPUTS/OUTPUTS_ALL_MESH_FAMILY_DATARMOR/noAIC",
  species = species_list,
  recursive = FALSE)

blockedCV_delta_all_df <- blockedCV_all_df %>%
  mutate(region = factor(region, levels = c("west", "east"))) %>%
  group_by(species, region, response) %>%
  mutate(delta_sumLL = sum_loglik - max(sum_loglik, na.rm = TRUE)) %>%
  ungroup()

blockedCV_east_biomass <- blockedCV_delta_all_df %>%
  filter(region == "east", response == "totalWeightKg") %>%
  filter(all_converged)



#------------------------------------------------------------------------------#
# PLOT ALL - comparison by species
#------------------------------------------------------------------------------#

AIC_east_biomass <- AIC_delta_all_df %>%
  filter(region == "east", response == "totalWeightKg")

AIC_plot_df <- AIC_east_biomass %>%
  transmute(
    species, region, response, family, mesh,
    metric = "cAIC weight (%)",
    value  = weight_species * 100
  )

randomCV_plot_df <- randomCV_east_biomass %>%
  transmute(
    species, region, response, family, mesh,
    metric = "ΔΣLL random CV",
    value = pmax(delta_sumLL, -100)
  )

blockedCV_plot_df <- blockedCV_east_biomass %>%
  transmute(
    species, region, response, family, mesh,
    metric = "ΔΣLL blocked CV",
    value = pmax(delta_sumLL, -100)
  )

cutoff_labs <- c(
  "cutoff_3"  = "M1",
  "cutoff_5"  = "M2",
  "cutoff_7"  = "M3",
  "cutoff_10" = "M4",
  "cutoff_15" = "M5")

CV_cAIC_df <- bind_rows(
  AIC_plot_df,
  randomCV_plot_df,
  blockedCV_plot_df
) %>%
  mutate(
    metric = factor(
      metric,
      levels = c("cAIC weight (%)", "ΔΣLL random CV", "ΔΣLL blocked CV")
    ),
    species_clean = str_replace_all(species, "_", " "),
    species_clean = factor(species_clean, levels = rev(sort(unique(species_clean)))),
    mesh = factor(mesh, levels = names(cutoff_labs), labels = cutoff_labs),
    family = factor(family, levels = names(family_colors))
  )

species_labs <- setNames(
  paste0("italic('", levels(CV_cAIC_df$species_clean), "')"),
  levels(CV_cAIC_df$species_clean)
)

CV_cAIC_df2 <- CV_cAIC_df %>%
  group_by(species_clean, metric) %>%
  mutate(mesh_pos = as.numeric(mesh)) %>%
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

ggplot(CV_cAIC_df2, aes(x = value, y = mesh, fill = family, shape = family)) +
  geom_rect(
    data = bg_mesh,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "grey90",
    colour = NA
  ) +
  geom_vline(
    data = data.frame(
      metric = factor(
        c("ΔΣLL random CV", "ΔΣLL blocked CV"),
        levels = levels(CV_cAIC_df2$metric)
      ),
      xintercept = 0
    ),
    aes(xintercept = xintercept),
    inherit.aes = FALSE,
    linetype = "dashed",
    linewidth = 0.3
  ) +
  geom_point(size = 4, alpha = 0.85, colour = "black") +
  scale_fill_manual(values = family_colors, name = "Family") +
  scale_shape_manual(values = family_shapes, name = "Family") +
  facet_grid(
    species_clean ~ metric,
    scales = "free_x",
    labeller = labeller(
      species_clean = as_labeller(species_labs, label_parsed)
    )
  ) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(
    legend.position = "top",
    strip.background = element_rect(fill = "white"),
    panel.spacing = unit(0.8, "lines"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )



families_keep <- c(
  "tweedie",
  "deltagamma",
  "deltalognormal",
  "deltagammapoissonlink",
  "deltalognormalpoissonlink"
)

family_labs <- c(
  tweedie = "Tweedie",
  deltagamma = "Δ-Gamma",
  deltalognormal = "Δ-Lognormal",
  deltagammapoissonlink = "Δ-Gamma-P",
  deltalognormalpoissonlink = "Δ-Lognormal-P"
)


library(tidytext)
heat_df <- CV_cAIC_df %>%
  mutate(
    species_clean = str_replace_all(species, "_", " "),
    species_clean = str_replace(species_clean, " ", "\n"),
    species_clean = factor(
      species_clean,
      levels = rev(
        species_list %>%
          str_replace_all("_", " ") %>%
          str_replace(" ", "\n")
      )
    ))%>%
  mutate(mesh = recode(as.character(mesh),
                "Mesh M3 (cutoff = 3)"  = "M1",
                "Mesh M4 (cutoff = 5)"  = "M2",
                "Mesh M5 (cutoff = 7)"  = "M3",
                "Mesh M6 (cutoff = 10)" = "M4",
                "Mesh M7 (cutoff = 15)" = "M5"),
         mesh = factor(mesh, levels = c("M1", "M2", "M3", "M4", "M5")))%>%
  filter(family %in% families_keep) %>%
  mutate(family = factor(family, levels = families_keep)) %>%
  complete(species_clean, metric, family, mesh)%>%
  mutate(value_label2 = ifelse(is.na(value), "NC", NA_character_))

heat_cAIC    <- heat_df %>% filter(metric == "cAIC weight (%)")
heat_random  <- heat_df %>% filter(metric == "ΔΣLL random CV")
heat_blocked <- heat_df %>% filter(metric == "ΔΣLL blocked CV")

p1 <- ggplot(heat_cAIC, aes(mesh, family, fill = value)) +
  geom_tile(colour = "grey85", linewidth = 0.25) +
  geom_text(aes(label = value_label2), size = 2.5)+
  facet_wrap(~ species_clean, nrow=1) +
  scico::scale_fill_scico(palette = "vik", name = "cAIC weight (%)", 
                          na.value = "white", limits = c(0, 100),
                          guide = guide_colourbar(barheight = unit(2, "cm"),
                                                  barwidth  = unit(0.6, "cm")))+
  scale_y_discrete(labels = family_labs)+
  theme_bw()+
  theme(legend.position = "right",
        legend.title = element_text(size = 9),
        strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(face = "italic"),
        panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

p2 <- ggplot(heat_random, aes(mesh, family, fill = value)) +
  geom_tile(colour = "grey85", linewidth = 0.25) +
  geom_text(aes(label = value_label2), size = 2.5)+
  facet_wrap(~ species_clean, nrow=1) +
  scico::scale_fill_scico(palette = "vik", name = "ΔΣLL random CV", na.value = "white",
                          guide = guide_colourbar(barheight = unit(2, "cm"),
                                                  barwidth  = unit(0.6, "cm")))+
  scale_y_discrete(labels = family_labs)+
  theme_bw()+
  labs(y = "observation families")+
  theme(legend.position = "right",
        legend.title = element_text(size = 9),
        axis.title.y = element_text(margin = margin(r = 8)),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

p3 <- ggplot(heat_blocked, aes(mesh, family, fill = value)) +
  geom_tile(colour = "grey85", linewidth = 0.25) +
  geom_text(aes(label = value_label2), size = 2.5)+
  facet_wrap(~ species_clean, nrow=1) +
  scale_y_discrete(labels = family_labs)+
  scico::scale_fill_scico(palette = "vik", name = "ΔΣLL blocked CV", na.value = "white",
                          guide = guide_colourbar(barheight = unit(2, "cm"),
                                                  barwidth  = unit(0.6, "cm")))+
  theme_bw()+
  labs(x = "mesh configurations")+
  theme(legend.position = "right",
        legend.title = element_text(size = 9),
        axis.title.x = element_text(margin = margin(t = 8)),
        strip.background = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_blank(),
        panel.grid = element_blank())

(p1 / p2 / p3)


##########################################"


#------------------------------------------------------------------------------#
# Delta RMSE
#------------------------------------------------------------------------------#

randomCV_east_biomass <- randomCV_east_biomass %>%
  group_by(species) %>%
  mutate(
    delta_rmse_rel = (rmse - min(rmse, na.rm = TRUE)) / min(rmse, na.rm = TRUE)
  ) %>%
  ungroup()

blockedCV_east_biomass <- blockedCV_east_biomass %>%
  group_by(species) %>%
  mutate(
    delta_rmse_rel = (rmse - min(rmse, na.rm = TRUE)) / min(rmse, na.rm = TRUE)
  ) %>%
  ungroup()

rmse_df <- bind_rows(
  randomCV_east_biomass %>%
    mutate(metric = "ΔRMSE random CV"),
  blockedCV_east_biomass %>%
    mutate(metric = "ΔRMSE blocked CV")
) %>%
  filter(
    family %in% families_keep,
    all_converged
  ) %>%
  mutate(
    species_clean = str_replace_all(species, "_", " "),
    species_clean = str_replace(species_clean, " ", "\n"),
    species_clean = factor(
      species_clean,
      levels = rev(unique(species_clean))
    )
  ) %>%
  mutate(
    mesh = recode(
      mesh,
      cutoff_3  = "M1",
      cutoff_5  = "M2",
      cutoff_7  = "M3",
      cutoff_10 = "M4",
      cutoff_15 = "M5"
    ),
    mesh = factor(mesh, levels = c("M1","M2","M3","M4","M5")),
    family = factor(family, levels = families_keep)
  )



rmse_heatmap <- ggplot(
  rmse_df,
  aes(mesh, family, fill = delta_rmse_rel)
) +
  geom_tile(colour = "grey85", linewidth = 0.25) +
  facet_grid(
    metric ~ species_clean, drop=FALSE ) +
  scico::scale_fill_scico(
    palette = "vik", trans = "log10",
    direction = -1,
    na.value = "white",
    name = "Relative ΔRMSE"
  ) +
  scale_y_discrete(labels = family_labs) +
  labs(
    x = "Mesh configuration",
    y = "Observation family"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "grey90"),
    strip.text.x = element_text(face = "italic", size = 8),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white"),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)
  )

rmse_heatmap
