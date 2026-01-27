

# ------------------------------------------------------------------------------#
#### 1. LOAD THE DATA #### 
# ------------------------------------------------------------------------------#

catch_ECGFS <- read_delim("01_DATA/survey/ECGFS/CGFS_1988_2024_ELFIC.V1.4_catch_2025-03-28.csv",
                          delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)

operation_ECGFS <- read_delim("01_DATA/survey/ECGFS/CGFS_1988_2024_ELFIC.V1.4_operation_2025-03-28.csv",
                              delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)


catch_WCGFS <-  read_delim("01_DATA/survey/WCGFS/WCGFS_2018_2024_ELFIC.V1.5_catch_2025-03-31.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)

operation_WCGFS <-  read_delim("01_DATA/survey/WCGFS/WCGFS_2018_2024_ELFIC.V1.5_operation_2025-03-31.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)

length(unique(catch_WCGFS$scientificName))
# 293 sp



# ------------------------------------------------------------------------------#
#### 2. COMBINE & FILTER DATA #### 
# ------------------------------------------------------------------------------#

catch_CGFS <- bind_rows(catch_ECGFS, catch_WCGFS) %>%
  filter(year >= 2018)

operation_CGFS <- bind_rows(operation_ECGFS, operation_WCGFS) %>%
  filter(year >= 2018)



# ------------------------------------------------------------------------------#
#### 3. COUNT HAULS, PRESENCE #### 
# ------------------------------------------------------------------------------#

# Number of hauls per year
number_haul_year <- operation_CGFS %>%
  group_by(year)%>%
  summarise(number_haul = length(unique(haulID)))

# Species presence per year
count_species_presence_year <- catch_CGFS %>%
  group_by(scientificName, year) %>%
  summarise(number_presence = length(unique(haulID)))

# Species presence across all years
count_species_presence <- catch_CGFS %>%
  group_by(scientificName) %>%
  summarise(number_presence = length(unique(haulID)))

# Join yearly presence with number of hauls
count_peryear <- count_species_presence_year %>%
  left_join(number_haul_year, by = "year")



# ------------------------------------------------------------------------------#
#### 3. COUNT HAULS, PRESENCE #### 
# ------------------------------------------------------------------------------#

# Species with >5% presence across all years
CGFS_proportion5_allyear <- count_species_presence %>%
  mutate(proportion = (100*number_presence)/length(unique(operation_CGFS$haulID))) %>%
  filter(proportion > 5)

species5_allyear <- CGFS_proportion5_allyear$scientificName

# Species with >10% presence across all years
CGFS_proportion10_allyear <- count_species_presence %>%
  mutate(proportion = (100*number_presence)/length(unique(operation_CGFS$haulID))) %>%
  filter(proportion > 10)

species10_allyear <- CGFS_proportion10_allyear$scientificName

# Yearly proportion for species with >5% presence
CGFS_proportion5_peryear <- count_peryear %>%
  filter(scientificName %in% species5_allyear) %>%
  mutate(proportion = (number_presence)/number_haul) 

# Yearly proportion for species with >10% presence
CGFS_proportion10_peryear <- count_peryear %>%
  filter(scientificName %in% species10_allyear) %>%
  mutate(proportion = (number_presence)/number_haul) 

# Heatmap of yearly proportions
CGFS_proportion10_peryear %>%
ggplot(aes(x = year, y = scientificName, fill = proportion)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma", name = "Proportion") +
  labs(title = "Proportion d'occurrence des espèces par année",
       x = "Année", y = "Espèce") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6),   
        panel.grid = element_blank())



# ------------------------------------------------------------------------------#
#### 4. COUNT NUMBER ON SPECIES OF PROPORTION10 #### 
# ------------------------------------------------------------------------------#

# Annual total catch for species with >10% presence
speciesprop10_catch <- catch_CGFS %>%
  filter(scientificName %in% species10_allyear) %>%
  group_by(scientificName, year)%>%
  filter(!scientificName %in% c("Ophiothrix fragilis"))%>%
  summarise(annual_sum = sum(totalNumber))

# Bar plot of annual total catch by species
speciesprop10_catch %>%
  ggplot(aes(x = annual_sum, y = scientificName, fill = factor(year))) +
  geom_col() +
  labs(title = "Nombre total capturé par année",
       x = "Nombre d'individus", y = "Espèce") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6),   
        panel.grid = element_blank())

# 10% presence → 80 species
# Manual filtering: remove non-commercial taxa
# Final selection: 50 species
species <- c(
    "Aequipecten opercularis",       # pétoncle blanc
    "Alloteuthis",                   # genre de calmars
    "Buccinum undatum",              # bulot
    "Cancer pagurus" ,               # tourteau 
    "Chelidonichthys cuculus",       # grondin rouge
    "Chelidonichthys lastoviza",     # grondin strié
    "Chelidonichthys lucerna",       # grondin perlon
    "Conger conger" ,                # congre
    "Dicentrarchus labrax",          # bar européen
    "Engraulis encrasicolus",        # anchois commun
    "Eutrigla gurnardus",            # grondin gris 
    "Hyperoplus lanceolatus",        # lançon commun
    "Illex coindetii",               # encornet rouge 
    "Limanda limanda",               # limande commune
    "Loligo forbesii",               # encornet veiné
    "Loligo vulgaris",               # calmar commun
    "Lophius piscatorius",           # baudroie commune
    "Maja brachydactyla",            # araignée de mer 
    "Merlangius merlangus",          # merlan
    "Merluccius merluccius",         # merlu commun
    "Micromesistius poutassou",      # merlan bleu
    "Microstomus kitt",              # sole-limande
    "Mimachlamys varia",             # pétoncle noir
    "Mullus surmuletus",             # rouget-barbet de roche
    "Pecten maximus",                # CSJ
    "Pleuronectes platessa" ,        # plie commune
    "Raja brachyura",                # raie lisse 
    "Raja clavata",                  # raie bouclée 
    "Raja undulata",                 # raie brunette
    "Sardina pilchardus",            # sardine commune
    "Scomber scombrus" ,             # maquereau
    "Scyliorhinus canicula",         # petite roussette
    "Scyliorhinus stellaris",        # grande roussette
    "Sepia officinalis",             # seiche commune
    "Solea solea",                   # sole commune
    "Spondyliosoma cantharus",       # dorade grise
    "Sprattus sprattus",             # sprat
    "Todaropsis eblanae",            # toutenon souffleur (calmar) 
    "Trachurus trachurus",           # chinchard
    "Trisopterus luscus",            # taucaud commun
    "Trisopterus minutus",           # capelan de l'atlantique = petit tacaud 
    "Capros aper",                   # sanglier
    "Blennius ocellaris",            # blennie
    "Callionymus lyra",              # dragonnet lyre
    "Echiichthys vipera",            # petite vive
    "Galeorhinus galeus",            # requin-hâ
    "Necora puber",                  # étrille
    "Mustelus asterias",             # émissole tachetée
    "Sepiola",                       # petit céphalopode 4-5 cm de long
    "Zeus faber"                     # st pierre
)

# 30 non-selected species
noselected_sp <- c(
  "Aequorea",                  # méduse 
  "Alcyonidium diaphanum",     # bryozoaire
  "Alcyonium digitatum",       # bryozoaire
  "Anseropoda placenta",       # étoile de mer 
  "Aphrodita aculeata",        # annélide polychète
  "Ascidia",                   # ascidie 
  "Asterias rubens",           # étoile de mer 
  "Calliactis palliata",       # anémone 
  "Chrysaora hysoscella",      # méduse 
  "Crepidula fornicata",       # crépidule
  "Crossaster papposus",       # étoile peigne
  "Echinus esculentus",        # oursin 
  "Henricia",                  # genre d’étoiles de mer 
  "Hippocampus hippocampus",   # petit hippocampe
  "Hyas coarctatus",           # crabe hyas 
  "Inachus dorsettensis",      # crabe-araignée du Dorset
  "Liocarcinus depurator",     # crabe nageur 
  "Liocarcinus holsatus",      # crabe nageur 
  "Liocarcinus vernalis",      # crabe nageur 
  "Macropodia",                # macropodes (crabes-araignées)
  "Macropodia rostrata",       # macropodes (crabes-araignées)
  "Macropodia tenuirostris",   # macropodes (crabes-araignées)
  "Ophiura ophiura",           # ophiure commune
  "Pagurus bernhardus",        # bernard-l'ermite commun
  "Pagurus prideaux",          # bernard-l'ermite de Prideaux
  "Pilumnus hirtellus",        # crabe velu 
  "Pisa armata",               # crabe-araignée armé
  "Psammechinus miliaris",     # oursin 
  "Pyuridae",                  # ascidies 
  "Styela clava"               # ascidie 
)

# Heatmap of annual total catch per selected species and year
speciesprop10_catch %>%
  filter(scientificName %in% species) %>%
  ggplot(aes(x = factor(year), y = reorder(scientificName, annual_sum, FUN = mean), fill = annual_sum)) +
  geom_tile(color= "white") +
  geom_text(aes(label = annual_sum), size = 3, color ="white") +
  scale_fill_viridis_c(option = "viridis", trans = "log10", 
                       labels = scales::label_number(), 
                       guide = guide_colorbar(barwidth  = unit(7, "cm"),
                                              barheight = unit(0.4, "cm"))) +
  labs(x = "Années", y = "Espèces", fill = "Nombres d'individus") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10, face = "italic"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        panel.grid = element_blank(), 
        legend.title = element_text(size = 10),
        legend.position ="top")


# Presence proportion per year for selected species
proportion_peryear_species <- count_peryear %>%
  filter(scientificName %in% species)%>%
  mutate(proportion = (number_presence)/number_haul)

# Barplot of yearly presence proportion by species
proportion_peryear_species%>%
  ggplot(aes(x = proportion/7, y = reorder(scientificName, proportion, FUN = mean), fill = factor(year))) +
  geom_col() +
  labs(x = "Proportion de présence", y = "Espèces", fill = "Années") +
  scale_x_continuous(breaks = seq(0, 1, 0.1), minor_breaks = FALSE)+
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10, face = "italic"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10),
        legend.key.size = unit(0.4, "cm"),   
        legend.key.height = unit(0.4, "cm"),
        legend.key.width = unit(0.4, "cm"))



# Load Looby et al. (2022) fish sound production dataset
# https://link.springer.com/article/10.1007/s11160-022-09702-1 
fish_soundproduction <- read_excel("01_DATA/soniferous_fish_Looby_etal2022.xlsx", sheet ="Data")

# Split genus and species for CGFS selection to match Looby data
species_gen_sp <- data.frame(original = species) %>%
  separate(original, into = c("Genus", "Species"), sep = " ", extra = "merge", fill = "right")

# Keep only actively sound-producing fishes
fish_subset <- fish_soundproduction %>%
  semi_join(species_gen_sp, by = c("Genus", "Species"))

# conserve uniquement les poissons produisant des sons de manière active
fish_activeproduction <- fish_subset %>%
  filter(`Active Sound Production` == 1) 

# List of actively sound-producing species
species_activeprod <- unique(fish_activeproduction$`Scientific Name`)

# Add a marker for actively sound-producing fishes
proportion_peryear_species_speaker <- proportion_peryear_species %>%
  mutate(is_active = scientificName %in% species_activeprod,
         speaker = ifelse(is_active, "x", ""))

# Barplot with active sound producers marked by "x"
proportion_peryear_species_speaker%>%
  ggplot(aes(x = proportion/7, y = reorder(scientificName, proportion, FUN = mean), fill = factor(year))) +
  geom_col() +
  geom_text(aes(x = -0.02, label = speaker), size = 5, hjust = 1.5) +
  labs(x = "Proportion de présence", y = "Espèces", fill = "Années",
       subtitle = expression("Production active de son (Looby, "*italic("et al.,")*" 2022)    " ~bold("X"))) +
  scale_x_continuous(breaks = seq(0, 1, 0.1), minor_breaks = FALSE)+
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10, face = "italic"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.subtitle = element_text(hjust = 0.55, size =10),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10),
        legend.key.size = unit(0.4, "cm"),   
        legend.key.height = unit(0.4, "cm"),
        legend.key.width = unit(0.4, "cm"))


# Add a marker for actively sound-producing fishes on the catch heatmap
speciesprop10_catch_speaker <- speciesprop10_catch %>%
  mutate(is_active = scientificName %in% species_activeprod,
         speaker = ifelse(is_active, "x", ""))

# Heatmap of annual catch with "x" for active sound producers
speciesprop10_catch_speaker %>%
  filter(scientificName %in% species) %>%
  ggplot(aes(x = factor(year), y = reorder(scientificName, annual_sum, FUN = mean), fill = annual_sum)) +
  geom_tile(color= "white") +
  geom_text(aes(label = annual_sum), size = 3, color ="white") +
  geom_text(aes(x = -0.01, label = speaker), size = 5, hjust = -0.8) +
  scale_fill_viridis_c(option = "viridis", trans = "log10", 
                       labels = scales::label_number(), 
                       guide = guide_colorbar(barwidth  = unit(7, "cm"),
                                              barheight = unit(0.4, "cm"))) +
  labs(x = "Années", y = "Espèces", fill = "Nombres d'individus", 
       subtitle = expression("Production active de son (Looby, "*italic("et al.,")*" 2022)    " ~bold("X"))) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10, face = "italic"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.subtitle = element_text(hjust = 0.15, size =10),
        panel.grid = element_blank(), 
        legend.title = element_text(size = 10),
        legend.position ="top")



