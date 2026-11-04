
# ============================================================================== #
# PRINCIPAL COMPONENT ANALYSIS ON METRICS
# ============================================================================== #
#
# DESCRIPTION:
# Perform a Principal Component Analysis (PCA) of model evaluation metrics
# to explore differences among species, likelihood families and mesh sizes.
#
#   - Loads the combined model evaluation metrics.
#   - Identifies the selected model configuration for each species.
#   - Performs a PCA using the five evaluation metrics.
#   - Conducts a sensitivity analysis excluding five atypical configurations.
#   - Visualizes PCA results by species, likelihood family and mesh size,
#     highlighting the selected model configurations.
#
# ------------------------------------------------------------------------------ #


# ------------------------------------------------------------------------------ #
#### 1. LOAD DATA AND IDENTIFY SELECTED MODELS ####
# ------------------------------------------------------------------------------ #

# Load model metrics; real_value contains the original numerical values.
# Use unrounded, uncapped metrics for PCA.
model_metrics <- readRDS(here("05_OUTPUTS/COMBINED_METRICS/EAST/combined_metrics.rds"))


# Define the final selected configuration for each species.
# The join below uses these identifiers to highlight chosen models in plots.
selected_models <- tribble(
  ~species_clean,              ~mesh, ~family,
  "Chelidonichthys\ncuculus",  "M4",  "Δ-Lognormal",
  "Chelidonichthys\nlucerna",  "M5",  "Δ-Lognormal-P",
  "Conger\nconger",            "M5",  "Δ-Lognormal",
  "Dicentrarchus\nlabrax",     "M5",  "Tweedie",
  "Hippocampus\nhippocampus",  "M5",  "Δ-Lognormal-P",
  "Solea\nsolea",              "M5",  "Δ-Lognormal-P",
  "Trachurus\ntrachurus",      "M5",  "Δ-Lognormal-P",
  "Zeus\nfaber",               "M4",  "Δ-Gamma-P") %>%
  dplyr::mutate(selection = "chosen") 

# Keep only the five metrics used in the PCA
pca_data <- model_metrics %>%
  dplyr::filter(metric %in% c("R²", "ΔcAIC", "median coeff var", 
                              "ΔΣLL random CV", "NRMSE random")) %>%
  dplyr::select(species_clean, mesh, family, metric, real_value) %>%
  tidyr::pivot_wider(names_from = metric, values_from = real_value) %>%
  dplyr::rename(`R²` = `R²`,
                `ΔcAIC` = `ΔcAIC`,
                `median CV` = `median coeff var`,
                `ΔΣLL` = `ΔΣLL random CV`,
                `NRMSE` = `NRMSE random`)%>%
  dplyr::left_join(selected_models, by = c("species_clean", "mesh", "family")) %>%
  dplyr::mutate(selection = tidyr::replace_na(selection, "others"),
                selection = factor(selection, levels = c("others", "chosen")))


# ------------------------------------------------------------------------------ #
#### 2. PCA ON FIVE EVALUATION METRICS ####
# ------------------------------------------------------------------------------ #

# Keep configurations with complete data for the five metrics
pca_data_complete <- pca_data %>%
  tidyr::drop_na(`R²`, `ΔcAIC`, `median CV`, `ΔΣLL`, `NRMSE`)


# Prepare the five numeric PCA columns.
# R² is already better when larger; negate the other four metrics because
# smaller values represent better performance. This simplifies interpretation
# of variable arrows, but does not change PCA eigenvalues or contributions.
pca_input <- pca_data_complete %>%
  dplyr::transmute(`R²`= `R²`, 
                   `ΔcAIC` = -`ΔcAIC`,
                   `median CV` = -`median CV`,
                   `ΔΣLL` = -`ΔΣLL`,
                   `NRMSE` = -`NRMSE`) %>%
  as.data.frame()

# Run PCA on standardized variables
# Center and scale variables so different metric units are comparable.
pca_full <- FactoMineR::PCA(pca_input,  scale.unit = TRUE,  graph = FALSE)

# Examine explained variance and variable contributions
pca_full$eig
pca_full$var$contrib


# ------------------------------------------------------------------------------ #
#### 3. IDENTIFY ATYPICAL CONFIGURATIONS ####
# ------------------------------------------------------------------------------ #

# Match each model configuration to its PCA coordinates and its percentage
# contribution to the first three axes of the FULL PCA.
individual_contributions <- dplyr::bind_cols(pca_data_complete %>%
  dplyr::select(species_clean, mesh, family, selection),
  as.data.frame(pca_full$ind$coord),
  as.data.frame(pca_full$ind$contrib) %>%
    dplyr::select(Dim.1, Dim.2, Dim.3) %>%
    dplyr::rename(contrib_Dim1 = Dim.1,
                  contrib_Dim2 = Dim.2,
                  contrib_Dim3 = Dim.3)) 

# Identify configurations contributing most strongly to each axis
individual_contributions %>%
  dplyr::arrange(dplyr::desc(contrib_Dim1)) %>%
  dplyr::select(species_clean, mesh, family, selection, Dim.1, contrib_Dim1) %>%
  utils::head(15)

individual_contributions %>%
  dplyr::arrange(dplyr::desc(contrib_Dim2)) %>%
  dplyr::select(species_clean, mesh, family, selection, Dim.2, contrib_Dim2) %>%
  utils::head(15)

individual_contributions %>%
  dplyr::arrange(dplyr::desc(contrib_Dim3)) %>%
  dplyr::select(species_clean, mesh, family, selection, Dim.3, contrib_Dim3) %>%
  utils::head(15)


# ------------------------------------------------------------------------------ #
#### 4. SENSITIVITY ANALYSIS ####
# ------------------------------------------------------------------------------ #

# Exclude five atypical configurations identified from the full PCA
# based on their individual contributions to the principal axes.
# This is a manually defined scenario: verify each exclusion using the
# individual contributions above and its model-performance context.
excluded_configurations <- tibble::tribble(
  ~species_clean,                 ~mesh, ~family,
  "Solea\nsolea",                 "M1",  "Δ-Gamma",
  "Solea\nsolea",                 "M1",  "Δ-Gamma-P",
  "Hippocampus\nhippocampus",     "M1",  "Δ-Gamma",
  "Chelidonichthys\ncuculus",     "M2",  "Δ-Lognormal-P",
  "Chelidonichthys\ncuculus",     "M1",  "Δ-Lognormal-P"
)

# Remove outlier configurations
pca_data_sensitivity <- pca_data_complete %>%
  dplyr::anti_join(excluded_configurations, by = c("species_clean", "mesh", "family"))


# Rebuild the PCA input with exactly the same metrics and sign reversals.
# This PCA is re-standardized on the reduced set of configurations.
pca_input_sensitivity <- pca_data_sensitivity %>%
  dplyr::transmute(`R²`= `R²`, 
                   `ΔcAIC` = -`ΔcAIC`,
                   `median CV` = -`median CV`,
                   `ΔΣLL` = -`ΔΣLL`,
                   `NRMSE` = -`NRMSE`) %>%
  as.data.frame()

pca_sensitivity <- FactoMineR::PCA(pca_input_sensitivity,
                                   scale.unit = TRUE,
                                   graph = FALSE)

# Examine explained variance and variable contributions
pca_sensitivity$eig
pca_sensitivity$var$contrib


# ------------------------------------------------------------------------------ #
#### 5. PREPARE PCA COORDINATES FOR PLOTTING ####
# ------------------------------------------------------------------------------ #

# Use sensitivity PCA for final figures
pca_coordinates <- dplyr::bind_cols(pca_data_sensitivity %>%
  dplyr::select(species_clean, mesh, family, selection),
  as.data.frame(pca_sensitivity$ind$coord)) %>%
  dplyr::mutate(mesh = factor(mesh, levels = paste0("M", 1:5)))


# ------------------------------------------------------------------------------ #
#### 5. PCA BIPLOTS ####
# ------------------------------------------------------------------------------ #

# Fixed colors keep family, mesh and species labels consistent across panels.
species_colors <- c("Chelidonichthys\ncuculus" = "#4575b4",
                    "Chelidonichthys\nlucerna" = "#74add1",
                    "Conger\nconger" = "#abd9e9",
                    "Dicentrarchus\nlabrax" = "#e0f3f8",
                    "Hippocampus\nhippocampus" = "#fee090",
                    "Solea\nsolea" = "#fdae61",
                    "Trachurus\ntrachurus" = "#f46d43",
                    "Zeus\nfaber" = "#d73027")

mesh_colors <- c(M1 = "#4575b4",
                 M2 = "#abd9e9",
                 M3 = "#fee090",
                 M4 = "#fdae61",
                 M5 = "#d73027")

family_colors <- c("Tweedie" = "#4575b4",
                   "Δ-Gamma" = "#abd9e9",
                   "Δ-Lognormal" = "#fee090",
                   "Δ-Gamma-P" = "#fdae61",
                   "Δ-Lognormal-P" = "#d73027")


# PCA axes 1 and 2: color configurations by family, mesh and species.
# All configurations are circles; selected configurations are overlaid
# as larger, black-outlined triangles.

p_family_D1.2 <- factoextra::fviz_pca_biplot(pca_sensitivity,  
                                             geom.ind = "blank",  
                                             col.var = "black",
                                             repel = TRUE) +
  ggplot2::geom_point(data = pca_coordinates,
                      aes(x = Dim.1, y = Dim.2, color = family),
                      shape = 16, size = 2.5,
                      inherit.aes = FALSE) +
  ggplot2::geom_point(data = pca_coordinates %>% 
                        dplyr::filter(selection == "chosen"),
                      aes(x = Dim.1, y = Dim.2, fill = family),
                      shape = 24, size = 4,
                      colour = "black", stroke = 0.7, 
                      inherit.aes = FALSE) +
  ggplot2::scale_color_manual(values = family_colors) +
  ggplot2::scale_fill_manual(values = family_colors, guide = "none") +
  ggplot2::labs(color = "likelihood", title = NULL)+ #title = "PCA — likelihood") +
  ggplot2::theme_minimal()+
  ggplot2::theme(legend.position = "none")


p_mesh_D1.2 <- factoextra::fviz_pca_biplot(pca_sensitivity,  
                                           geom.ind = "blank",  
                                           col.var = "black",
                                           repel = TRUE) +
  ggplot2::geom_point(data = pca_coordinates, 
                      aes(x = Dim.1, y = Dim.2, color = mesh),
                      shape = 16, size = 2.5, 
                      inherit.aes = FALSE) +
  ggplot2::geom_point(data = pca_coordinates %>% 
                        dplyr::filter(selection == "chosen"),
                      aes(x = Dim.1, y = Dim.2, fill = mesh),
                      shape = 24, size = 4,
                      colour = "black", stroke = 0.7, 
                      inherit.aes = FALSE) +
  ggplot2::scale_color_manual(values = mesh_colors,
                              breaks = c("M1", "M2", "M3", "M4", "M5")) +
  ggplot2::scale_fill_manual(values = mesh_colors,
                             breaks = c("M1", "M2", "M3", "M4", "M5"),
                             guide = "none") +
  ggplot2::labs(color = "mesh", title = NULL) + # "PCA — mesh") +
  ggplot2::theme_minimal()+
  ggplot2::theme(legend.position = "none")


p_species_D1.2 <- factoextra::fviz_pca_biplot(pca_sensitivity,  
                                              geom.ind = "blank",  
                                              col.var = "black",
                                              repel = TRUE) +
  ggplot2::geom_point(data = pca_coordinates,
                      aes(x = Dim.1, y = Dim.2, color = species_clean),
                      shape = 16, size = 2.5, 
                      inherit.aes = FALSE) +
  ggplot2::geom_point(data = pca_coordinates %>% 
                        dplyr::filter(selection == "chosen"),
                      aes(x = Dim.1, y = Dim.2, fill = species_clean),
                      shape = 24, size = 4,
                      colour = "black", stroke = 0.7,
                      inherit.aes = FALSE) +
  ggplot2::scale_color_manual(values = species_colors) +
  ggplot2::scale_fill_manual(values = species_colors, guide = "none") +
  ggplot2::labs(color = "species", title = NULL) + #"PCA — species") +
  ggplot2::theme_minimal()+
  ggplot2::theme(legend.position = "none")




# PCA axes 1 and 3: repeat the same three groupings.
# Legends are shown below this row of panels.

p_family_D1.3 <- factoextra::fviz_pca_biplot(pca_sensitivity,  
                                             axes = c(1,3),
                                             geom.ind = "blank",  
                                             col.var = "black",
                                             repel = TRUE) +
  ggplot2::geom_point(data = pca_coordinates,
                      aes(x = Dim.1, y = Dim.3, color = family),
                      shape = 16, size = 2.5, 
                      inherit.aes = FALSE) +
  ggplot2::geom_point(data = pca_coordinates %>% 
                        dplyr::filter(selection == "chosen"),
                      aes(x = Dim.1, y = Dim.3, fill = family),
                      shape = 24, size = 4,
                      colour = "black", stroke = 0.7, 
                      inherit.aes = FALSE) +
  ggplot2::scale_color_manual(values = family_colors) +
  ggplot2::scale_fill_manual(values = family_colors, guide = "none") +
  ggplot2::labs(color = "likelihood", title = NULL) +
  ggplot2::theme_minimal()+
  ggplot2::guides(colour = guide_legend(nrow = 2, byrow = TRUE,
                                        override.aes = list(shape = 15)))+    
  ggplot2::theme(legend.text = element_text(size = 10),
                 legend.position = "bottom")


p_mesh_D1.3 <- factoextra::fviz_pca_biplot(pca_sensitivity,  
                                           axes = c(1, 3),
                                           geom.ind = "blank",  
                                           col.var = "black",
                                           repel = TRUE) +
  ggplot2::geom_point(data = pca_coordinates, 
                      aes(x = Dim.1, y = Dim.3, color = mesh),
                      shape = 16, size = 2.5, 
                      inherit.aes = FALSE) +
  ggplot2::geom_point(data = pca_coordinates %>% 
                        dplyr::filter(selection == "chosen"),
                      aes(x = Dim.1, y = Dim.3, fill = mesh),
                      shape = 24, size = 4,
                      colour = "black", stroke = 0.7, 
                      inherit.aes = FALSE) +
  ggplot2::labs(color = "mesh", title = NULL) +
  ggplot2::scale_color_manual(values = mesh_colors,
                              breaks = c("M1", "M2", "M3", "M4", "M5")) +
  ggplot2::scale_fill_manual(values = mesh_colors,
                             breaks = c("M1", "M2", "M3", "M4", "M5"), 
                             guide = "none") +
  ggplot2::theme_minimal()+
  ggplot2::guides(colour = guide_legend(nrow = 2, byrow = TRUE,
                                        override.aes = list(shape = 15)))+    
  ggplot2::theme(legend.text = element_text(size = 10),
                 legend.position = "bottom")

p_species_D1.3 <- factoextra::fviz_pca_biplot(pca_sensitivity,  
                                              axes = c(1, 3),
                                              geom.ind = "blank",  
                                              col.var = "black",
                                              repel = TRUE) +
  ggplot2::geom_point(data = pca_coordinates,
                      aes(x = Dim.1, y = Dim.3, color = species_clean),
                      shape = 16, size = 2.5, 
                      inherit.aes = FALSE) +
  ggplot2::geom_point(data = pca_coordinates %>% 
                        dplyr::filter(selection == "chosen"),
                      aes(x = Dim.1, y = Dim.3, fill = species_clean),
                      shape = 24, size = 4,
                      colour = "black", stroke = 0.7, 
                      inherit.aes = FALSE) +
  ggplot2::labs(color = "species", title = NULL) +
  ggplot2::scale_color_manual(values = species_colors) +
  ggplot2::scale_fill_manual(values = species_colors, guide = "none") +
  ggplot2::theme_minimal()+
  ggplot2::guides(color = guide_legend(nrow = 3, byrow = TRUE,
                                       override.aes = list(shape = 15)))+                                        
  ggplot2::theme(legend.text = element_text(size = 10, face = "italic"),
                 legend.position = "bottom")



# Reorder plot layers so that model configurations are displayed
# above the PCA variable arrows.

# PCA axes 1 and 2
p_family_D1.2$layers <- c(tail(p_family_D1.2$layers, 2), 
                          head(p_family_D1.2$layers, -2))

p_mesh_D1.2$layers <- c(tail(p_mesh_D1.2$layers, 2),
                        head(p_mesh_D1.2$layers, -2))

p_species_D1.2$layers <- c(tail(p_species_D1.2$layers, 2),
                           head(p_species_D1.2$layers, -2))


# PCA axes 1 and 3
p_family_D1.3$layers <- c(tail(p_family_D1.3$layers, 2),
                          head(p_family_D1.3$layers, -2))

p_mesh_D1.3$layers <- c(tail(p_mesh_D1.3$layers, 2),
                        head(p_mesh_D1.3$layers, -2))

p_species_D1.3$layers <- c(tail(p_species_D1.3$layers, 2),
                           head(p_species_D1.3$layers, -2))



# Combine the six PCA plots into two rows:
# Top row: axes 1 and 2, coloured by family, mesh and species.
# Bottom row: axes 1 and 3, with the same colour groupings.
(p_family_D1.2 + p_mesh_D1.2 + p_species_D1.2) / 
  (p_family_D1.3 + p_mesh_D1.3 + p_species_D1.3) 








