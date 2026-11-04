
# ============================================================================== #
#### COMPARE MODEL PERFORMANCE ACROSS SPECIES AND CONFIGURATIONS ####
# ============================================================================== #
#
# Description:
# This script generates heatmaps comparing model performance across species,
# SPDE meshes and likelihood families for the EAST or WEST study region.
#
# It:
# - Loads previously computed model evaluation metrics.
# - Summarises the coefficient of variation, cAIC, R² and cross-validation scores.
# - Formats the results for comparison across model configurations.
#
# Change only 'region' to switch between EAST and WEST.
#
# ============================================================================== #


# ------------------------------------------------------------------------------ #
#### 1. SELECT REGION AND SPECIES ####
# ------------------------------------------------------------------------------ #

# Select study region: "EAST" or "WEST"
region <- "EAST"

# List of species modeled in the WEST region
species_list_west <- c("Capros_aper",
                       "Chelidonichthys_cuculus",
                       "Conger_conger",
                       "Dicentrarchus_labrax",
                       "Engraulis_encrasicolus",
                       "Micromesistius_poutassou",
                       "Trachurus_trachurus",
                       "Zeus_faber")

# List of species modeled in the EAST region
species_list_east <- c("Chelidonichthys_cuculus",
                       "Chelidonichthys_lucerna",
                       "Conger_conger",
                       "Dicentrarchus_labrax",
                       "Hippocampus_hippocampus",
                       "Solea_solea",
                       "Trachurus_trachurus",
                       "Zeus_faber")

# Select species according to the study region
species_list <- switch(region,
                       EAST = species_list_east,
                       WEST = species_list_west)



# ------------------------------------------------------------------------------ #
#### 2. LOAD COMBINED MODEL EVALUATION METRICS ####
# ------------------------------------------------------------------------------ #

base_dir <- here::here("05_OUTPUTS", "COMBINED_METRICS", region)

# Load model evaluation metrics for all species
cAIC_all_species <- readRDS(file.path(base_dir, "cAIC_all_species.rds"))
coeff_var_all_species <- readRDS(file.path(base_dir, "coeff_var_all_species.rds"))
R2_all_species <- readRDS(file.path(base_dir, "R2_all_species.rds"))
rep_randomCV_all_species <- readRDS(file.path(base_dir, "randomCV_all_species.rds"))
blockCV_all_species <- readRDS(file.path(base_dir, "blockCV_all_species.rds"))


# ------------------------------------------------------------------------------ #
#### 3. SET MODEL CONFIGURATIONS AND LABELS ####
# ------------------------------------------------------------------------------ #

# Compute mean and standard deviation of biomass for NRMSE calculation
source(here::here("03_TRANSFORM_DATA", "compute_mean_sd_biomass.R"))

# Likelihood families to compare
families_keep <- c("tweedie",
                   "deltagamma",
                   "deltalognormal",
                   "deltagammapoissonlink",
                   "deltalognormalpoissonlink")

# Display labels for likelihood families
family_labs <- c(tweedie = "Tweedie",
                 deltagamma = "\u0394-Gamma",
                 deltalognormal = "\u0394-Lognormal",
                 deltagammapoissonlink = "\u0394-Gamma-P",
                 deltalognormalpoissonlink = "\u0394-Lognormal-P")

# Display labels for SPDE meshes
cutoff_labs <- c("cutoff_3" = "M1",
                 "cutoff_5" = "M2",
                 "cutoff_7" = "M3",
                 "cutoff_10" = "M4",
                 "cutoff_15" = "M5")


# ------------------------------------------------------------------------------ #
#### 4. PREPARE MODEL EVALUATION METRICS ####
# ------------------------------------------------------------------------------ #

#### 4.1. Coefficient of variation ####
# ------------------------------------------------------------------------------ #

# Compute median prediction CV for each species and model configuration
cv_median_df <- coeff_var_all_species %>%
  dplyr::filter(family %in% families_keep,
                mesh %in% names(cutoff_labs)) %>%
  dplyr::mutate(species_clean = str_replace(species, "_", "\n"),
                mesh = factor(mesh, levels = names(cutoff_labs), labels = cutoff_labs),
                family = factor(family, levels = families_keep)) %>%
  dplyr::group_by(species_clean, mesh, family) %>%
  dplyr::summarise(cv_median = median(cv, na.rm = TRUE), .groups = "drop") %>%
  tidyr::complete(species_clean, mesh, family) %>%
  dplyr::mutate(label = ifelse(is.na(cv_median), "NC", sprintf("%.1f", cv_median)))


#### 4.2. cAIC ####
# ------------------------------------------------------------------------------ #

# Prepare delta cAIC values and mark missing configurations as NC = non convergent
cAIC_df_clean <- cAIC_all_species %>%
  dplyr::filter(family %in% families_keep,
                mesh %in% names(cutoff_labs)) %>%
  dplyr::mutate(species_clean = str_replace(species, "_", "\n"),
                mesh = factor(mesh, levels = names(cutoff_labs), labels = cutoff_labs),
                family = factor(family, levels = families_keep)) %>%
  dplyr::select(species_clean, mesh, family, deltacAIC) %>%
  tidyr::complete(species_clean, mesh, family, fill = list(deltacAIC = NA)) %>%
  dplyr::mutate(label = case_when(is.na(deltacAIC) ~ "NC", TRUE ~ sprintf("%.1f", deltacAIC)))


#### 4.3. R2 ####
# ------------------------------------------------------------------------------ #

# Prepare R² values and mark missing configurations as NC = non convergent
R2_df_clean <- R2_all_species %>%
  dplyr::filter(family %in% families_keep,
                mesh %in% names(cutoff_labs)) %>%
  dplyr::mutate(species_clean = str_replace(species, "_", "\n"),
                mesh = factor(mesh, levels = names(cutoff_labs), labels = cutoff_labs),
                family = factor(family, levels = families_keep))%>%
  dplyr::select(species_clean, mesh, family, R2)%>%
  tidyr::complete(species_clean, mesh, family, fill = list(R2 = NA)) %>%
  dplyr::mutate(label = ifelse(is.na(R2), "NC", sprintf("%.2f", R2)))


#### 4.4. REPEATED RANDOM CROSS-VALIDATION: SUM LOG-LIKELIHOOD ####
# ------------------------------------------------------------------------------ #

# Compare mean random-CV log-likelihood against the best configuration per species
deltamSLL_random_df_clean <- rep_randomCV_all_species %>%
  dplyr::filter(all_converged,
                family %in% families_keep,
                mesh %in% names(cutoff_labs)) %>%
  dplyr::group_by(species, mesh, family) %>%
  dplyr::summarise(mean_loglik = mean(sum_loglik, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(species) %>%
  dplyr::mutate(delta_loglik = max(mean_loglik, na.rm = TRUE) - mean_loglik,
                delta_loglik99 = pmin(delta_loglik, 99))%>%
  dplyr::ungroup() %>%
  dplyr::mutate(species_clean = str_replace(species, "_", "\n"),
                mesh = factor(mesh, levels = names(cutoff_labs), labels = cutoff_labs),
                family = factor(family, levels = families_keep)) %>%
  dplyr::select(species_clean, mesh, family, delta_loglik, delta_loglik99) %>%
  tidyr::complete(species_clean, mesh, family) %>%
  dplyr::mutate(label = dplyr::case_when(is.na(delta_loglik) ~ "NC",
                                         delta_loglik > 99 ~ ">99",
                                         TRUE ~ sprintf("%.1f", delta_loglik)))



#### 4.5. REPEATED RANDOM CROSS-VALIDATION: NORMALIZED RMSE ####
# ------------------------------------------------------------------------------ #

# Normalize random-CV RMSE by mean biomass
NRMSE_random_df_clean <- rep_randomCV_all_species %>%
  dplyr::filter(all_converged,
                family %in% families_keep,
                mesh %in% names(cutoff_labs)) %>%
  dplyr::group_by(species, mesh, family) %>%
  dplyr::summarise(RMSE = mean(rmse, na.rm = TRUE), .groups = "drop") %>%
  dplyr::left_join(sd_mean_biomass_summary %>%
                     dplyr::mutate(species = gsub(" ", "_", species)) %>%
                     dplyr::select(species, mean_biomassKg), by = "species") %>%
  dplyr::mutate(NRMSE_mean = RMSE / mean_biomassKg,
                species_clean = str_replace(species, "_", "\n"),
                mesh = factor(mesh, levels = names(cutoff_labs), labels = cutoff_labs),
                family = factor(family, levels = families_keep)) %>%
  dplyr::select(species_clean, mesh, family, NRMSE_mean) %>%
  tidyr::complete(species_clean, mesh, family) %>%
  dplyr::mutate(label = ifelse(is.na(NRMSE_mean), "NC", sprintf("%.2f", NRMSE_mean)))


#### 4.6. BLOCK CROSS-VALIDATION: SUM LOG-LIKELIHOOD ####
# ------------------------------------------------------------------------------ #

# Compare mean block-CV log-likelihood against the best configuration per species
deltamSLL_block_df_clean <- blockCV_all_species %>%
  dplyr::filter(all_folds_converged,
                family %in% families_keep,
                mesh %in% names(cutoff_labs)) %>%
  dplyr::group_by(species, mesh, family) %>%
  dplyr::summarise(mean_loglik = mean(sum_loglik, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(species) %>%
  dplyr::mutate(delta_loglik = max(mean_loglik, na.rm = TRUE) - mean_loglik,
                delta_loglik99 = pmin(delta_loglik, 99))%>%
  dplyr::ungroup() %>%
  dplyr::mutate(species_clean = str_replace(species, "_", "\n"),
                mesh = factor(mesh, levels = names(cutoff_labs), labels = cutoff_labs),
                family = factor(family, levels = families_keep)) %>%
  dplyr::select(species_clean, mesh, family, delta_loglik, delta_loglik99) %>%
  tidyr::complete(species_clean, mesh, family) %>%
  dplyr::mutate(label = dplyr::case_when(is.na(delta_loglik) ~ "NC",
                                         delta_loglik > 99 ~ ">99",
                                         TRUE ~ sprintf("%.1f", delta_loglik)))


#### 4.7. BLOCK CROSS-VALIDATION: NORMALIZED RMSE ####
# ------------------------------------------------------------------------------ #

# Normalize block-CV RMSE by mean biomass
NRMSE_block_df_clean <- blockCV_all_species %>%
  dplyr::filter(all_folds_converged,
                family %in% families_keep,
                mesh %in% names(cutoff_labs)) %>%
  dplyr::group_by(species, mesh, family) %>%
  dplyr::summarise(RMSE = mean(rmse, na.rm = TRUE), .groups = "drop") %>%
  dplyr::left_join(sd_mean_biomass_summary %>%
                     dplyr::mutate(species = gsub(" ", "_", species)) %>%
                     dplyr::select(species, mean_biomassKg), by = "species") %>%
  dplyr::mutate(NRMSE_mean = RMSE / mean_biomassKg,
                species_clean = str_replace(species, "_", "\n"),
                mesh = factor(mesh, levels = names(cutoff_labs), labels = cutoff_labs),
                family = factor(family, levels = families_keep)) %>%
  dplyr::select(species_clean, mesh, family, NRMSE_mean) %>%
  tidyr::complete(species_clean, mesh, family) %>%
  dplyr::mutate(label = ifelse(is.na(NRMSE_mean), "NC", sprintf("%.2f", NRMSE_mean)))



# ------------------------------------------------------------------------------ #
#### 5. COMBINE METRICS FOR PLOTTING ####
# ------------------------------------------------------------------------------ #

# Stack the seven metrics into one table
# value: value used for plotting (capped at 99 for delta log-likelihood)
# label: formatted value displayed on the plot
# real_value: original numeric value, without capping or rounding

combined_metrics_for_plot <- bind_rows(
  R2_df_clean %>%
    dplyr::transmute(species_clean, mesh, family, 
                     metric = "R²",
                     value = R2, label, real_value = R2),
  cAIC_df_clean %>%
    dplyr::transmute(species_clean, mesh, family, 
                     metric = "ΔcAIC", 
                     value = deltacAIC, label, real_value = deltacAIC),
  deltamSLL_random_df_clean %>%
    dplyr::transmute(species_clean, mesh, family, 
                     metric = "ΔΣLL random CV", 
                     value = delta_loglik99, label,  real_value = delta_loglik),
  NRMSE_random_df_clean %>%
    dplyr::transmute(species_clean, mesh, family, 
                     metric = "NRMSE random", 
                     value = NRMSE_mean, label, real_value = NRMSE_mean),
  deltamSLL_block_df_clean %>%
    dplyr::transmute(species_clean, mesh, family, 
                     metric = "ΔΣLL block CV", 
                     value = delta_loglik99, label, real_value = delta_loglik),
  NRMSE_block_df_clean %>%
    dplyr::transmute(species_clean, mesh, family, 
                     metric = "NRMSE block", 
                     value = NRMSE_mean, label, real_value = NRMSE_mean),
  cv_median_df %>%
    dplyr::transmute(species_clean, mesh, family, 
                     metric = "median coeff var", 
                     value = cv_median, label, real_value = cv_median)) %>%
  dplyr::mutate(metric = factor(metric, levels = c("ΔcAIC", 
                                                   "R²",  
                                                   "median coeff var", 
                                                   "ΔΣLL random CV",
                                                   "NRMSE random",
                                                   "ΔΣLL block CV",
                                                   "NRMSE block"))) %>%
  dplyr::mutate(family = factor(recode(as.character(family), !!!family_labs),
                                levels = unname(family_labs)))


# Apply EAST-specific labels for model fitting failures (NF) and non-normal Q-Q plots (NQ)
if (region == "EAST") {
  
  combined_metrics_for_plot <- combined_metrics_for_plot %>%
    dplyr::mutate(label = dplyr::case_when(
      species_clean == "Trachurus\ntrachurus" & family == "Δ-Gamma-P" & label != "NC" ~ "NQ",
      species_clean == "Trachurus\ntrachurus" & mesh == "M2" & family == "Δ-Gamma" & label == "NC" ~ "NF",
      species_clean == "Dicentrarchus\nlabrax" & family == "Tweedie" & mesh == "M4" & 
        metric == "NRMSE random" ~ ">9.5", TRUE ~ label),
      value = dplyr::if_else(label == "NQ", NA_real_, value))
  
}

# Save combined metrics in the selected region directory
output_dir <- here::here("05_OUTPUTS", "COMBINED_METRICS", region)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

saveRDS(combined_metrics_for_plot,
        file.path(output_dir, "combined_metrics_all_species.rds"))

