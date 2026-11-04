
# ============================================================================== #
# SELECT FISH SPECIES FOR EBESCO ANALYSES - ECGFS AND WCGFS
# ============================================================================== #
#
# Description:
#   Identify actively sound-producing fish recorded in the Eastern and Western
#   Channel Ground Fish Surveys (2018 onward), add two EBESCO target species,
#   and evaluate annual occurrence in survey hauls by region.
#
# Objectives:
#   1. Match surveyed species against the Looby et al. (2022) sound-production
#      database and identify species with documented active sound production.
#   2. Include two additional EBESCO target species and flag species listed
#      for fishery statistics in the FAO ASFIS dataset.
#   3. Calculate annual haul-level occurrence percentages by survey and species.
#   4. Retain species-region combinations with at least 10% occurrence in
#      every year for which a species-year record is present.
#   5. Visualize occurrence patterns and prepare regional selection criteria.
#
# Inputs:
#   - ECGFS and WCGFS catch and operation CSV files.
#   - Looby et al. (2022) fish sound-production Excel dataset.
#   - FAO ASFIS species CSV dataset.
#
# Output:
#   - species_criteria_region_10percent.rds (optional; export commented out).
#   - Diagnostic heatmaps of annual occurrence percentage and haul counts.
# ============================================================================== #

# ------------------------------------------------------------------------------ #
#### 1. LOAD SURVEY AND SPECIES REFERENCE DATA #### 
# ------------------------------------------------------------------------------ #

# Load ECGFS catch and operation records; remove duplicates and retain 2018 onward
catch_ECGFS <- readr::read_delim("01_DATA/survey/ECGFS/CGFS_1988_2024_ELFIC.V1.4_catch_2025-03-28.csv",
                          delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
catch_ECGFS <- catch_ECGFS %>% dplyr::distinct() %>% dplyr::filter(year >= 2018)


operation_ECGFS <- readr::read_delim("01_DATA/survey/ECGFS/CGFS_1988_2024_ELFIC.V1.4_operation_2025-03-28.csv",
                              delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
operation_ECGFS <- operation_ECGFS %>% dplyr::distinct() %>% dplyr::filter(year >= 2018)


# Load WCGFS catch and operation records; remove duplicates and retain 2018 onward
catch_WCGFS <-  readr::read_delim("01_DATA/survey/WCGFS/WCGFS_2018_2024_ELFIC.V1.5_catch_2025-03-31.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
catch_WCGFS <- catch_WCGFS %>% dplyr::distinct()%>% dplyr::filter(year >= 2018)


operation_WCGFS <-  readr::read_delim("01_DATA/survey/WCGFS/WCGFS_2018_2024_ELFIC.V1.5_operation_2025-03-31.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
operation_WCGFS <- operation_WCGFS %>% dplyr::distinct()%>% dplyr::filter(year >= 2018)


# Load the reference database of fish sound-production evidence
# Load Looby et al. (2022) fish sound production dataset
# https://link.springer.com/article/10.1007/s11160-022-09702-1 
fish_soundproduction <- readxl::read_excel("01_DATA/soniferous_fish_Looby_etal2022.xlsx", sheet ="Data")%>%
  dplyr::rename(scientificName = `Scientific Name`)

# Load the FAO ASFIS reference list for fishery statistics
# © FAO. 2026. ASFIS List of Species for Fishery Statistics Purposes. In: Fisheries and Aquaculture. [Cited Friday, February 13th 2026]. 
# https://www.fao.org/fishery/en/collection/asfis
fish_commercial_importance <- readr::read_csv("01_DATA/ASFIS_sp_2025.csv", show_col_types = FALSE)%>%
  dplyr::rename(scientificName = `Scientific_Name`)


# ------------------------------------------------------------------------------- #
#### 2. IDENTIFY ACTIVELY SOUND-PRODUCING SPECIES - WCGFS ####
# ------------------------------------------------------------------------------- #

list_species_WCGFS <- unique(catch_WCGFS$scientificName) 

# Split genus and species for CGFS selection to match Looby data
species_gen_sp_WCGFS <- data.frame(original = list_species_WCGFS) %>%
  tidyr::separate(original, into = c("Genus", "Species"), sep = " ", extra = "merge", fill = "right")

# Keep only actively sound-producing fishes in CGFS data 
fish_subset_WCGFS <- fish_soundproduction %>%
  dplyr::semi_join(species_gen_sp_WCGFS, by = c("Genus", "Species"))

fish_activeproduction_WCGFS <- fish_subset_WCGFS %>%
  dplyr::filter(`Active Sound Production` == 1) 

# List of actively sound-producing species
species_activeprod_WCGFS <- unique(fish_activeproduction_WCGFS$scientificName)

selection_criteria_west_df <- data.frame(species = species_activeprod_WCGFS,
                                         west_region = TRUE,
                                         active_sound = TRUE)


# ------------------------------------------------------------------------------- #
#### 3. IDENTIFY ACTIVELY SOUND-PRODUCING SPECIES - ECGFS ####
# ------------------------------------------------------------------------------- #

list_species_ECGFS <- unique(catch_ECGFS$scientificName) 

# Split genus and species for CGFS selection to match Looby data
species_gen_sp_ECGFS <- data.frame(original = list_species_ECGFS) %>%
  tidyr::separate(original, into = c("Genus", "Species"), sep = " ", extra = "merge", fill = "right")

# Keep only actively sound-producing fishes in CGFS data 
fish_subset_ECGFS <- fish_soundproduction %>%
  dplyr::semi_join(species_gen_sp_ECGFS, by = c("Genus", "Species"))

fish_activeproduction_ECGFS <- fish_subset_ECGFS %>%
  dplyr::filter(`Active Sound Production` == 1) 

# List of actively sound-producing species
species_activeprod_ECGFS <- unique(fish_activeproduction_ECGFS$scientificName)

selection_criteria_east_df <- data.frame(species = species_activeprod_ECGFS,
                                         east_region = TRUE,
                                         active_sound = TRUE)

# join selection_criteria of west and east
selection_criteria_df <- dplyr::full_join(selection_criteria_east_df,
                                          selection_criteria_west_df,
                                          by = "species") %>%
  dplyr::mutate(east_region = ifelse(is.na(east_region), FALSE, east_region),
                west_region = ifelse(is.na(west_region), FALSE, west_region),
                active_sound = ifelse(is.na(active_sound.x), active_sound.y, active_sound.x)) %>%
  dplyr::select(species, east_region, west_region, active_sound)


# ------------------------------------------------------------------------------- #
#### 4. INCLUDE ADDITIONAL EBESCO TARGET SPECIES ####
# ------------------------------------------------------------------------------- #

selection_criteria_df <- selection_criteria_df %>%
  dplyr::add_row(species = "Dicentrarchus labrax",
                 east_region = TRUE,
                 west_region = TRUE,
                 active_sound = FALSE) %>%
  dplyr::add_row(species = "Solea solea",
                 east_region = TRUE,
                 west_region = FALSE,   # gear, capturability
                 active_sound = FALSE)


# ------------------------------------------------------------------------------- #
#### 5. FLAG SPECIES LISTED FOR FISHERY STATISTICS ####
# ------------------------------------------------------------------------------- #

commercial_species <- fish_commercial_importance %>%
  dplyr::filter(FishStat_Data == "YES") %>%
  dplyr::pull(scientificName)

selection_criteria_df <- selection_criteria_df %>%
  dplyr::mutate(commercial_importance = species %in% commercial_species)


# ------------------------------------------------------------------------------- #
#### 6. CALCULATE ANNUAL SPECIES OCCURRENCE - WCGFS ####
# ------------------------------------------------------------------------------- #

# Number of hauls per year
number_haul_year_WCGFS <- operation_WCGFS %>%
  dplyr::group_by(year)%>%
  dplyr::summarise(nb_haul = dplyr::n_distinct(haulID), .groups = "drop")

# Species presence per year
count_species_presence_year_WCGFS <- catch_WCGFS %>%
  dplyr::group_by(scientificName, year) %>%
  dplyr::summarise(nb_presence = dplyr::n_distinct(haulID), .groups = "drop")

# Join yearly presence with annual number of hauls and compute proportion of presence
count_peryear_WCGFS <- count_species_presence_year_WCGFS %>%
  dplyr::left_join(number_haul_year_WCGFS, by = "year")%>%
  dplyr::mutate(prop_pres = 100*nb_presence/nb_haul)


# ------------------------------------------------------------------------------- #
#### 7. CALCULATE ANNUAL SPECIES OCCURRENCE - ECGFS ####
# ------------------------------------------------------------------------------- #

# Number of hauls per year
number_haul_year_ECGFS <- operation_ECGFS %>%
  dplyr::group_by(year)%>%
  dplyr::summarise(nb_haul = dplyr::n_distinct(haulID), .groups = "drop")

# Species presence per year
count_species_presence_year_ECGFS <- catch_ECGFS %>%
  dplyr::group_by(scientificName, year) %>%
  dplyr::summarise(nb_presence = dplyr::n_distinct(haulID), .groups = "drop")

# Join yearly presence with annual number of hauls and compute proportion of presence
count_peryear_ECGFS <- count_species_presence_year_ECGFS %>%
  dplyr::left_join(number_haul_year_ECGFS, by = "year")%>%
  dplyr::mutate(prop_pres = 100*nb_presence/nb_haul)


# ------------------------------------------------------------------------------- #
#### 8. EVALUATE SPECIES OCCURRENCE BY REGION ####
# ------------------------------------------------------------------------------- #

#### 8.1. Prepare occurrence data for candidate species ####
# ------------------------------------------------------------------------------- #

# WEST CGFS 
selected_species_west <- selection_criteria_df %>%
  dplyr::filter(west_region) %>% dplyr::pull(species)

species_activeprod_WCGFS_count <- count_peryear_WCGFS %>%
  dplyr::filter(scientificName %in% selected_species_west)%>%
  dplyr::mutate(region = "west")

# EAST CGFS 
selected_species_east <- selection_criteria_df %>%
  dplyr::filter(east_region) %>% dplyr::pull(species)

species_activeprod_ECGFS_count <- count_peryear_ECGFS %>%
  dplyr::filter(scientificName %in% selected_species_east)%>% 
  dplyr::mutate(region = "east")

# Combine occurrence data from both regions
all_activesoundprod_CGFS <- bind_rows(species_activeprod_WCGFS_count, species_activeprod_ECGFS_count)%>%
  dplyr::mutate(region = factor(region, levels = c("west", "east")))


#### 8.2. Calculate global occurrence across all survey years ####
# ------------------------------------------------------------------------------- #

# Calculate global occurrence separately for each region.
# The denominator includes all hauls, including years without species captures.
total_hauls_east <- sum(number_haul_year_ECGFS$nb_haul)
total_hauls_west <- sum(number_haul_year_WCGFS$nb_haul)

global_occurrence <- all_activesoundprod_CGFS %>%
  dplyr::group_by(scientificName, region) %>%
  dplyr::summarise(total_presence = sum(nb_presence), .groups = "drop") %>%
  dplyr::mutate(total_hauls = dplyr::if_else(region == "east",
                                             total_hauls_east,
                                             total_hauls_west),
                global_occurence = 100 * total_presence / total_hauls) %>%
  dplyr::select(scientificName, region, global_occurence) %>%
  tidyr::pivot_wider(names_from = region, 
                     values_from = global_occurence,
                     names_prefix = "global_occurrence_")


#### 8.3. Select species with at least 10% annual occurrence ####
# ------------------------------------------------------------------------------- #

# Retain species-region combinations with at least 10% occurrence
# in every year represented in the annual occurrence data.
all_activesoundprod_CGFS_10percent <- all_activesoundprod_CGFS %>%
  dplyr::group_by(scientificName, region) %>%
  dplyr::filter(min(prop_pres) >= 10) %>%
  dplyr::ungroup()


# ------------------------------------------------------------------------------- #
#### 9. VISUALIZE ANNUAL SPECIES OCCURRENCE #### 
# ------------------------------------------------------------------------------- #
# PLOT PROPORTION OF PRESENCE OF SELECTED SPECIES
all_activesoundprod_CGFS_10percent %>%
  ggplot2::ggplot(aes(x = factor(year), 
             y = factor(scientificName, levels = rev(sort(unique(scientificName)))), 
             fill = prop_pres)) +
  ggplot2::geom_tile(color= "white") +
  ggplot2::geom_text(aes(label = round(prop_pres,1)), size = 3.5, color ="black") +
  ggplot2::scale_fill_distiller(palette = "RdYlBu",
                       labels = scales::label_number()) +
  ggplot2::facet_wrap(~region)+
  ggplot2::labs(x = "Years", y = NULL, fill = "Occurrence\nin hauls (%)")+
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.y = element_text(size = 10, face = "italic"),
                 axis.title.x = element_text(size = 10, margin = margin(t = 8)),
                 axis.title.y = element_text(size = 10, margin = margin(r = 8)),
                 plot.subtitle = element_text(hjust = 0.15, size =10),
                 strip.text = element_text(size =12),
                 panel.grid = element_blank(), 
                 legend.title = element_text(size = 10, margin = margin(b = 12)),
                 legend.position ="right")


# PLOT NUMBER OF PRESENCE OF SELECTED SPECIES
all_activesoundprod_CGFS %>%
  ggplot2::ggplot(aes(x = factor(year), 
                      y = factor(scientificName, levels = rev(sort(unique(scientificName)))), 
                      fill = nb_presence)) +
  ggplot2::geom_tile(color= "white") +
  ggplot2::geom_text(aes(label = nb_presence), size = 3.5, color ="black") +
  ggplot2::scale_fill_distiller(palette = "RdYlBu", trans = "log10", 
                                labels = scales::label_number()) +
  ggplot2::facet_wrap(~region)+
  ggplot2::labs(x = "Years", y = NULL, fill = "Number of \npresence")+
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.y = element_text(size = 10, face = "italic"),
                 axis.title.x = element_text(size = 10, margin = margin(t = 8)),
                 axis.title.y = element_text(size = 10, margin = margin(r = 8)),
                 plot.subtitle = element_text(hjust = 0.15, size =10),
                 strip.text = element_text(size =12),
                 panel.grid = element_blank(), 
                 legend.title = element_text(size = 10, margin = margin(b = 12)),
                 legend.position ="right")


# ------------------------------------------------------------------------------- #
#### 9. FINALIZE REGION-SPECIFIC SPECIES SELECTION ####
# ------------------------------------------------------------------------------- #

# Data frame of species selection criteria by region,
# restricted to species reaching at least 10% occurrence
selection_filtered <- selection_criteria_df %>%
  filter(species %in% all_activesoundprod_CGFS_10percent$scientificName)

# Build a species-by-region presence table indicating whether
# each species is recorded in the east and/or west region
region_presence <- all_activesoundprod_CGFS_10percent %>%
  distinct(scientificName, region) %>%
  mutate(present = TRUE) %>%
  tidyr::pivot_wider(names_from = region, 
                     values_from = present, 
                     values_fill = FALSE)

# Update the regional criteria:
# if a species is absent from a region in the 10% occurrence data,
# set the corresponding regional criterion to FALSE
selection_updated_10percent <- selection_filtered %>%
  left_join(region_presence, by = c("species" = "scientificName")) %>%
  mutate(east_region = east_region & coalesce(east, FALSE),
         west_region = west_region & coalesce(west, FALSE)) %>%
  dplyr::select(-east, -west)%>%
  arrange(species)%>%
  dplyr::left_join(global_occurrence, by = c("species" = "scientificName"))


# ------------------------------------------------------------------------------- #
#### 10. SAVE FINAL SPECIES SELECTION #### 
# ------------------------------------------------------------------------------- #

saveRDS(selection_updated_10percent,
        file = here::here("01_DATA", "species_criteria_region_10percent.rds"))


