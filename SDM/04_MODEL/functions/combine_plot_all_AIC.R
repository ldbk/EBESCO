# COMBINE AND PLOT ALL AIC WEIGHTS



combine_AIC_by_region_all_species <- function(AIC_all_species) {
  
  purrr::imap_dfr(AIC_all_species, function(AIC_by_region, sp) {
    
    purrr::imap_dfr(AIC_by_region, function(df_region, region_name) {
      
      df_region <- as.data.frame(df_region)
      
      df_region %>%
        mutate(
          species = sp,
          region  = region_name
        )
    })
  }) %>%
    relocate(species, region, response, family, AIC, deltaAIC, weight)
}

combine_AIC_from_folders <- function(parent_dir,
                                     pattern = "^model_diagnostics_",
                                     recursive = FALSE) {
  
  sp_dirs <- list.dirs(parent_dir, recursive = recursive, full.names = TRUE)
  sp_dirs <- sp_dirs[grepl(pattern, basename(sp_dirs))]
  
  aic_list <- list()
  
  for (d in sp_dirs) {
    sp_name <- sub(pattern, "", basename(d))   
    aic_path <- file.path(d, "AIC_by_region.rds")
    
    if (file.exists(aic_path)) {
      aic_list[[sp_name]] <- readRDS(aic_path)
    }
  }
  
  combine_AIC_by_region_all_species(aic_list)
}

AIC_all_df <- combine_AIC_from_folders(
  parent_dir = "~/EBESCO/05_OUTPUTS/model_diagnostics",
  recursive = FALSE)


ggplot(AIC_all_df,
       aes(x = weight*100, y = species, color = family, shape = family)) +
  geom_point(size = 4) +
  scale_x_continuous(limits = c(0, 100)) +
  facet_grid(response ~ region) +
  labs(x = "AIC weight (%)", color = "Family", shape = "Family") +
  theme_bw() +
  theme(legend.position = "top")





# COMBINE CV SCORES (all species x regions)

combine_CV_by_region_all_species <- function(CV_all_species) {
  
  purrr::imap_dfr(CV_all_species, function(CV_by_region, sp) {
    
    purrr::imap_dfr(CV_by_region, function(obj_region, region_name) {
      
      df_region <- obj_region$cv_scores
      
      as.data.frame(df_region) %>%
        mutate(
          species = sp,
          region  = region_name
        )
    })
  }) %>%
    relocate(species, region, response, family, sum_loglik, rmse, mae)
}

combine_CV_from_folders <- function(parent_dir,
                                    pattern = "^model_diagnostics_",
                                    recursive = FALSE) {
  
  sp_dirs <- list.dirs(parent_dir, recursive = recursive, full.names = TRUE)
  sp_dirs <- sp_dirs[grepl(pattern, basename(sp_dirs))]
  
  cv_list <- list()
  
  for (d in sp_dirs) {
    sp_name <- sub(pattern, "", basename(d))
    cv_path <- file.path(d, "cross_validation_by_region.rds")
    
    if (file.exists(cv_path)) {
      cv_list[[sp_name]] <- readRDS(cv_path)
    }
  }
  
  combine_CV_by_region_all_species(cv_list)
}

CV_all_df <- combine_CV_from_folders(
  parent_dir = "~/EBESCO/05_OUTPUTS/model_diagnostics",
  recursive  = FALSE)


CV_all_df_w <- CV_all_df %>%
  mutate(region = factor(region, levels = c("west", "east"))) %>%
  group_by(species, region, response) %>%
  mutate(delta_sumLL = sum_loglik - max(sum_loglik))%>%
  ungroup()



ggplot(CV_all_df_w,
       aes(x = delta_sumLL, y = species, color = family, shape = family)) +
  geom_point(size = 4) +
  facet_grid(response ~ region) +
  labs(x = "CV sum_loglik", color = "Family", shape = "Family") +
  theme_bw() +
  theme(legend.position = "top")



library(writexl)

combined_outputs <- AIC_all_df %>%
  left_join(CV_all_df_w, by = c("species", "region", "response", "family"))

# writexl::write_xlsx(combined_outputs,
                    # path = file.path("05_OUTPUTS", "combined_outputs.xlsx"))








# PLOT AIC + DELTA LL

CV_all_df_w <- CV_all_df_w %>%
  rename(delta_sumLL = delta_sumLL)  

AIC_plot_df <- AIC_all_df %>%
  transmute(species, region, response, family,
            metric = "AIC weight (%)",
            value  = weight * 100)

CV_plot_df <- CV_all_df_w %>%
  transmute(species, region, response, family,
            metric = "CV delta sum_loglik",
            value  = delta_sumLL)

both_df <- bind_rows(AIC_plot_df, CV_plot_df) %>%
  mutate(response = factor(response, levels = c("totalWeightKg", "densityKgKm2")),
         metric   = factor(metric, levels = c("AIC weight (%)", "CV delta sum_loglik")))


bg_df <- both_df %>%
  distinct(region, response, metric) %>%
  filter(response == "densityKgKm2") %>%
  mutate(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)


both_df <- both_df %>%
  mutate(region = factor(region, levels = c("west", "east"))) %>%
  # filter(!species %in% c("Capros_aper", "Engraulis_encrasicolus")) %>%
  mutate(species = str_replace_all(species, "_", " "),
         species = factor(species, levels = rev(sort(unique(species)))))

bg_species <- both_df %>%
  distinct(species) %>%
  arrange(species) %>%
  mutate(species_rank = row_number(),
         bg_fill = ifelse(species_rank %% 2 == 0, "grey92", "white"),
         ymin = as.numeric(species) - 0.5,
         ymax = as.numeric(species) + 0.5,
         xmin = -Inf,
         xmax = Inf)

ggplot(both_df, 
       aes(x = value, y = species, color = family, shape = family)) +
  
  geom_rect(data = bg_species,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = bg_fill),
            inherit.aes = FALSE, alpha = 1, color = NA) +
  scale_fill_identity() +
  geom_point(size = 4) +
  facet_grid(response ~ region + metric, scales = "free_x") +
  labs(x = NULL, color = "Family", shape = "Family") +
  theme_bw() +
  theme(legend.position = "top",
        strip.background = element_rect(fill = "white"),
        panel.spacing = unit(0.8, "lines"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_text(face = "italic"))






