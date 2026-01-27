
# ------------------------------------------------------------------------------#
####  LOAD DATA ####
# ------------------------------------------------------------------------------#

catch_ECGFS <- read_delim(
  "01_DATA/survey/ECGFS/CGFS_1988_2024_ELFIC.V1.4_catch_2025-03-28.csv",
  delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)

operation_ECGFS <- read_delim(
  "01_DATA/survey/ECGFS/CGFS_1988_2024_ELFIC.V1.4_operation_2025-03-28.csv",
  delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)

catch_WCGFS <- read_delim(
  "01_DATA/survey/WCGFS/WCGFS_2018_2024_ELFIC.V1.5_catch_2025-03-31.csv",
  delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)

operation_WCGFS <- read_delim(
  "01_DATA/survey/WCGFS/WCGFS_2018_2024_ELFIC.V1.5_operation_2025-03-31.csv",
  delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)


# ------------------------------------------------------------------------------#
#### SPECIES LIST ####
# ------------------------------------------------------------------------------#

species_list <- c(
  "Conger conger",
  "Engraulis encrasicolus",
  "Micromesistius poutassou",
  "Merluccius merluccius",
  "Capros aper",
  "Trachurus trachurus",
  "Chelidonichthys cuculus",
  "Chelidonichthys lucerna",
  "Eutrigla gurnardus",
  "Zeus faber",
  "Solea solea",
  "Dicentrarchus labrax"
)

east_annual_number_hauls <- operation_ECGFS %>%
  filter(year %in% c(2018:2024)) %>%
  group_by(year) %>%
  summarise(nb_hauls = n_distinct(haulID))%>%
  mutate(region = "east")

west_annual_number_hauls <- operation_WCGFS %>%
  group_by(year) %>%
  summarise(nb_hauls = n_distinct(haulID))%>%
  mutate(region = "west")

east_annual_number_presence <- catch_ECGFS %>%
  filter(year %in% c(2018:2024),
         scientificName %in% species_list) %>%
  group_by(scientificName, year) %>%
  summarise(nb_presence = n_distinct(haulID), .groups = "drop") %>%
  mutate(region = "east") %>%
  tidyr::complete(scientificName = species_list,
                  year = 2018:2024,
                  fill = list(nb_presence = 0, region = "east"))


west_annual_number_presence <- catch_WCGFS %>%
  filter(scientificName %in% species_list) %>%
  group_by(scientificName, year) %>%
  summarise(nb_presence = n_distinct(haulID), .groups = "drop") %>%
  mutate(region = "west") %>%
  group_by(scientificName) %>%
  complete(year = c(2018:2024), fill = list(nb_presence = 0)) %>%
  ungroup() %>%
  mutate(region = "west")

east_prop <- east_annual_number_presence %>%
  left_join(east_annual_number_hauls, by = c("region", "year")) %>%
  mutate(prop_presence = nb_presence/nb_hauls)
  
west_prop <- west_annual_number_presence %>%
  left_join(west_annual_number_hauls, by = c("region", "year")) %>%
  mutate(prop_presence = nb_presence/nb_hauls)


prop_presence_all <- bind_rows(east_prop, west_prop) 
prop_presence_all$region <- factor(prop_presence_all$region, levels = c("west", "east"))

ggplot(prop_presence_all, aes(x = factor(year), y = scientificName, fill = nb_presence)) +
  geom_tile(color = "white", linewidth = 0.2) +
  geom_text(aes(label = nb_presence), size = 3, color = "white") +
  facet_wrap(~ region, labeller = as_labeller(c(west = "Ouest", east = "Est"))) +
  scale_fill_viridis_c(name = "Nombre de \nprésences") +
  labs(x = "Années", y = "Espèces") +
  theme_minimal() +
  theme(axis.text.y = element_text(face = "italic"),
        panel.grid = element_blank())

ggplot(prop_presence_all, aes(x = factor(year), y = scientificName, fill = prop_presence)) +
  geom_tile(color = "white", linewidth = 0.2) +
  geom_text(aes(label = round(prop_presence, 2)), size = 3, color = "white") +
  facet_wrap(~ region, labeller = as_labeller(c(west = "Ouest", east = "Est"))) +
  scale_fill_viridis_c(name = "Proportion \nde présences") +
  labs(x = "Années", y = "Espèces") +
  theme_minimal() +
  theme(axis.text.y = element_text(face = "italic"),
        panel.grid = element_blank())



region_validity_prop_presence <- prop_presence_all %>%
  group_by(scientificName, region)%>%
  summarise(region_validity  = all(prop_presence != 0), .groups = "drop") %>%
  pivot_wider(names_from = region, values_from = region_validity)


saveRDS(region_validity_prop_presence, file = here::here("01_DATA", "region_validity_prop_presence.rds"))

