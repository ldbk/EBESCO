

# ------------------------------------------------------------------------------#
#### 1. LOAD THE DATA #### 
# ------------------------------------------------------------------------------#
# EAST CGFS

catch_ECGFS <- read_delim("01_DATA/survey/ECGFS/CGFS_1988_2024_ELFIC.V1.4_catch_2025-03-28.csv",
                          delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)

catch_ECGFS <- catch_ECGFS %>% distinct()


operation_ECGFS <- read_delim("01_DATA/survey/ECGFS/CGFS_1988_2024_ELFIC.V1.4_operation_2025-03-28.csv",
                              delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)

operation_ECGFS <- operation_ECGFS %>% distinct()


# WEST CGFS

catch_WCGFS <-  read_delim("01_DATA/survey/WCGFS/WCGFS_2018_2024_ELFIC.V1.5_catch_2025-03-31.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)

catch_WCGFS <- catch_WCGFS %>% distinct()


operation_WCGFS <-  read_delim("01_DATA/survey/WCGFS/WCGFS_2018_2024_ELFIC.V1.5_operation_2025-03-31.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)

operation_WCGFS <- operation_WCGFS %>% distinct()



#-------------------------------------------------------------------------------#
# WEST CGFS
#-------------------------------------------------------------------------------#
# Number of hauls per year
number_haul_year_WCGFS <- operation_WCGFS %>%
  group_by(year)%>%
  summarise(nb_haul = length(unique(haulID)),
            nb_row = n())

# Species presence per year
count_species_presence_year_WCGFS <- catch_WCGFS %>%
  group_by(scientificName, year) %>%
  summarise(nb_presence = length(unique(haulID)))

# Species presence across all years
count_species_presence_WCGFS <- catch_WCGFS %>%
  group_by(scientificName) %>%
  summarise(nb_presence = length(unique(haulID)))

n_years_WCGFS <- n_distinct(count_species_presence_year_WCGFS$year)

species_filtered_WCGFS <- count_species_presence_year_WCGFS %>%
  filter(nb_presence > 0) %>%
  group_by(scientificName) %>%
  filter(n_distinct(year) == n_years_WCGFS) %>%
  ungroup()

unique(species_filtered_WCGFS$scientificName)

# Join yearly presence with number of hauls
count_peryear_WCGFS <- species_filtered_WCGFS %>%
  left_join(number_haul_year_WCGFS, by = "year")%>%
  mutate(prop_pres = 100*nb_presence/nb_haul)


#-------------------------------------------------------------------------------#
# EAST CGFS
#-------------------------------------------------------------------------------#
# Number of hauls per year
number_haul_year_ECGFS <- operation_ECGFS %>%
  filter(year >= 2018)%>%
  group_by(year)%>%
  summarise(nb_haul = length(unique(haulID)),
            nb_row = n())

# Species presence per year
count_species_presence_year_ECGFS <- catch_ECGFS %>%
  filter(year >= 2018)%>%
  group_by(scientificName, year) %>%
  summarise(nb_presence = length(unique(haulID)))

# Species presence across all years
count_species_presence_ECGFS <- catch_ECGFS %>%
  group_by(scientificName) %>%
  summarise(nb_presence = length(unique(haulID)))

n_years_ECGFS <- n_distinct(count_species_presence_year_ECGFS$year)

species_filtered_ECGFS <- count_species_presence_year_ECGFS %>%
  filter(nb_presence > 0) %>%
  group_by(scientificName) %>%
  filter(n_distinct(year) == n_years_ECGFS) %>%
  ungroup()

unique(species_filtered_ECGFS$scientificName)

# Join yearly presence with number of hauls
count_peryear_ECGFS <- species_filtered_ECGFS %>%
  left_join(number_haul_year_ECGFS, by = "year")%>%
  mutate(prop_pres = 100*nb_presence/nb_haul)


# ------------------------------------------------------------------------------#
#### 3. ACTIVE SOUND PRODUCER #### 
# ------------------------------------------------------------------------------

# Load Looby et al. (2022) fish sound production dataset
# https://link.springer.com/article/10.1007/s11160-022-09702-1 
fish_soundproduction <- read_excel("01_DATA/soniferous_fish_Looby_etal2022.xlsx", sheet ="Data")

#-------------------------------------------------------------------------------#
# WEST
#-------------------------------------------------------------------------------#
list_species_filtered_WCGFS <-unique(species_filtered_WCGFS$scientificName) 

# Split genus and species for CGFS selection to match Looby data
species_gen_sp_WCGFS <- data.frame(original = list_species_filtered_WCGFS) %>%
  separate(original, into = c("Genus", "Species"), sep = " ", extra = "merge", fill = "right")

# Keep only actively sound-producing fishes
fish_subset_WCGFS <- fish_soundproduction %>%
  semi_join(species_gen_sp_WCGFS, by = c("Genus", "Species"))

# conserve uniquement les poissons produisant des sons de manière active
fish_activeproduction_WCGFS <- fish_subset_WCGFS %>%
  filter(`Active Sound Production` == 1) 

# List of actively sound-producing species
species_activeprod_WCGFS <- unique(fish_activeproduction_WCGFS$`Scientific Name`)

# > species_activeprod_ECGFS
# [1] "Conger conger"           "Clupea harengus"        
# [3] "Engraulis encrasicolus"  "Trachurus trachurus"    
# [5] "Gobius niger"            "Chelidonichthys cuculus"
# [7] "Chelidonichthys lucerna" "Eutrigla gurnardus"     
# [9] "Hippocampus hippocampus" "Zeus faber"  


# species_activeprod_WCGFS
# [1] "Conger conger"            "Engraulis encrasicolus"  
# [3] "Melanogrammus aeglefinus" "Micromesistius poutassou"
# [5] "Pollachius pollachius"    "Merluccius merluccius"   
# [7] "Capros aper"              "Trachurus trachurus"     
# [9] "Chelidonichthys cuculus"  "Eutrigla gurnardus"      
# [11] "Zeus faber"               "Squalus acanthias"  

species_activeprod_WCGFS_count <- count_peryear_WCGFS %>%
  filter(scientificName %in% c(species_activeprod_WCGFS, "Dicentrarchus labrax"))%>%
  mutate(region = "west")

#-------------------------------------------------------------------------------#
# EAST
#-------------------------------------------------------------------------------#
list_species_filtered_ECGFS <-unique(species_filtered_ECGFS$scientificName) 

# Split genus and species for CGFS selection to match Looby data
species_gen_sp_ECGFS <- data.frame(original = list_species_filtered_ECGFS) %>%
  separate(original, into = c("Genus", "Species"), sep = " ", extra = "merge", fill = "right")

# Keep only actively sound-producing fishes
fish_subset_ECGFS <- fish_soundproduction %>%
  semi_join(species_gen_sp_ECGFS, by = c("Genus", "Species"))

# conserve uniquement les poissons produisant des sons de manière active
fish_activeproduction_ECGFS <- fish_subset_ECGFS %>%
  filter(`Active Sound Production` == 1) 

# List of actively sound-producing species
species_activeprod_ECGFS <- unique(fish_activeproduction_ECGFS$`Scientific Name`)

# > species_activeprod_ECGFS
# [1] "Conger conger"           "Clupea harengus"        
# [3] "Engraulis encrasicolus"  "Trachurus trachurus"    
# [5] "Gobius niger"            "Chelidonichthys cuculus"
# [7] "Chelidonichthys lucerna" "Eutrigla gurnardus"     
# [9] "Hippocampus hippocampus" "Zeus faber"  


species_activeprod_ECGFS_count <- count_peryear_ECGFS %>%
  filter(scientificName %in% c(species_activeprod_ECGFS, "Solea solea", "Dicentrarchus labrax"))%>%
  filter(!scientificName %in% c("Gobius niger", "Hippocampus hippocampus"))%>%
  mutate(region = "east")




# COMBINE ALL 

all_activeprod_CGFS <- bind_rows(species_activeprod_WCGFS_count, species_activeprod_ECGFS_count)%>%
  mutate(region = factor(region, levels = c("west", "east")),
         scientificName = factor(scientificName, levels = rev(sort(unique(scientificName)))))

all_activeprod_CGFS %>%
  ggplot(aes(x = factor(year), y = scientificName, fill = prop_pres)) +
  geom_tile(color= "white") +
  geom_text(aes(label = round(prop_pres,1)), size = 3.5, color ="black") +
  scale_fill_distiller(palette = "RdYlBu", trans = "log10", 
                       labels = scales::label_number()) +
  facet_wrap(~region)+
  labs(x = "Years", y = NULL, fill = "Presence (%)")+
  theme_bw() +
  theme(axis.text.y = element_text(size = 10, face = "italic", hjust = 0.5),
        axis.title.x = element_text(size = 10, margin = margin(t = 8)),
        axis.title.y = element_text(size = 10, margin = margin(r = 8)),
        plot.subtitle = element_text(hjust = 0.15, size =10),
        strip.text = element_text(size =12),
        panel.grid = element_blank(), 
        legend.title = element_text(size = 10, margin = margin(b = 12)),
        legend.position ="right")



species_region_validity_prefilters <- all_activeprod_CGFS %>%
  dplyr::select(scientificName, region)%>%
  distinct()


species_region_validity <- species_region_validity_prefilters %>%
  mutate(value = TRUE) %>%
  pivot_wider(names_from  = region, values_from = value, values_fill = FALSE)



fish_commercial_importance <- read_csv("01_DATA/ASFIS_sp_2025.csv")%>%
  rename(scientificName = Scientific_Name)


sound_commercial_fish_region_validity <- species_region_validity %>%
  left_join(fish_commercial_importance, by = "scientificName")


saveRDS(sound_commercial_fish_region_validity,
        file = here::here("01_DATA", "species_region_validity.rds"))






