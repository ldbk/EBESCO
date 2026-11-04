
# ------------------------------------------------------------------------------ #
#### MEAN AND SD OF BIOMASS BY SPECIES ####
# ------------------------------------------------------------------------------ #

sd_mean_biomass_summary <- lapply(species_list, function(sp) {
  
  sp_scientific <- gsub("_", " ", sp)
  sp_name_safe <- sp
  
  source(here::here("03_TRANSFORM_DATA/transform_data.R"), local = TRUE)
  
  data_region <- switch(region,
                        EAST = data_CGFS_east,
                        WEST = data_CGFS_west)
  
  tibble::tibble(species = sp_scientific,
                 mean_biomassKg = mean(data_region$totalWeightKg, na.rm = TRUE),
                 sd_biomassKg = sd(data_region$totalWeightKg, na.rm = TRUE))
  
}) %>%
  dplyr::bind_rows() %>%
  dplyr::arrange(species)
