#################################################################################
#                SCRIPT TO GENERATE INPUT DATA FOR THE MODEL 
#################################################################################

# ------------------------------------------------------------------------------
# LOAD THE DATA 
# ------------------------------------------------------------------------------


catch_CGFS <- read_delim(here("01_DATA", "CGFS-ELFIC-1988-2024-SEANOE", "CGFS_1988_2024_ELFIC.V1.4_catch_2025-03-28.csv"), 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)

operation_CGFS <- read_delim(here("01_DATA", "CGFS-ELFIC-1988-2024-SEANOE", "CGFS_1988_2024_ELFIC.V1.4_operation_2025-03-28.csv"), 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)


# others_parameters <- read_delim("01_DATA/CGFS-ELFIC-1988-2024-SEANOE/CGFS_1988_2024_ELFIC.V1.4_otherParameters_2025-03-28.csv", 
#                                 delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
# --> many NA for depth variable

# operation_CGFS_clean <- operation_CGFS %>%
#   filter(!is.na(startLatDD), !is.na(startLongDD))

# unique(catch_CGFS$comment)
# unique(operation_CGFS$comment)
# unique(operation_CGFS$gear)
# unique(catch_CGFS$stationCode)

# lat_range <- range(operation_CGFS$startLatDD)
# lon_range <- range(operation_CGFS$startLongDD)


# ------------------------------------------------------------------------------
# DATA EXPLORATION : SAMPLING EFFORT
# ------------------------------------------------------------------------------

bbox <- c(-1.9, 2.5, 49.2, 51.4)   # Eastern English Channel coordinates

# All years
# ------------------------------------------------------------------------------
annual_map_points <- basemap(limits = bbox ) +
  geom_spatial_point(data = operation_CGFS, 
                     aes(x = startLongDD, y = startLatDD), color = "blue")+
  facet_wrap(~ year)+
  ggtitle("Spatial distribution of haul start positions by year from the CGFS campaign")+
  labs(x="Longitude", y="Latitude")


annual_color_points <- basemap(limits = bbox ) +
  geom_spatial_point(data = operation_CGFS, 
                     aes(x = startLongDD, y = startLatDD, color = factor(year)), 
                     size = 2, alpha = 0.4)+
  ggtitle("Spatial distribution of haul start positions by year from the CGFS campaign")+
  labs(x="Longitude", y="Latitude", color="Years")



# Years since 2005
# ------------------------------------------------------------------------------
operation_CGFS_2005 <- operation_CGFS %>%
  filter(year >= 2005)

annual_color_2005 <- basemap(limits = bbox ) +
  geom_spatial_point(data = operation_CGFS_2005, 
                     aes(x = startLongDD, y = startLatDD, color = factor(year)), size = 2, alpha = 0.6)

annual_map_2005 <- basemap(limits = bbox ) +
  geom_spatial_point(data = operation_CGFS_2005, 
                     aes(x = startLongDD, y = startLatDD), color = "blue")+
  facet_wrap(~ year)+
  ggtitle("Spatial distribution of haul start positions by year from the CGFS campaign")


# Years since 2014
# ------------------------------------------------------------------------------

operation_CGFS_2015 <- operation_CGFS %>%
  filter(year > 2014)

annual_color_2015 <- basemap(limits = bbox ) +
  geom_spatial_point(data = operation_CGFS_2015, 
                     aes(x = startLongDD, y = startLatDD, color = factor(year)), 
                     size = 2, alpha = 0.6)+
  ggtitle("Spatial distribution of haul start positions by year from the CGFS campaign since 2015")+
  labs(x="Longitude", y="Latitude", color ="Years")

annual_map_2015 <- basemap(limits = bbox ) +
  geom_spatial_point(data = operation_CGFS_2015, 
                     aes(x = startLongDD, y = startLatDD), color = "blue")+
  facet_wrap(~ year)+
  ggtitle("Spatial distribution of haul start positions by year from the CGFS campaign since 2015")


# Years 2019-2021
# ------------------------------------------------------------------------------

operation_CGFS_2020 <- operation_CGFS %>%
  filter(year %in% c(2019,2020,2021))

annual_color_2020 <- basemap(limits = bbox ) +
  geom_spatial_point(data = operation_CGFS_2020, 
                     aes(x = startLongDD, y = startLatDD, color = factor(year)), size = 2, alpha = 0.6)

annual_map_2020 <- basemap(limits = bbox ) +
  geom_spatial_point(data = operation_CGFS_2020, 
                     aes(x = startLongDD, y = startLatDD), color = "blue", size=2)+
  facet_wrap(~ year)+
  ggtitle("Spatial distribution of haul start positions by year from the CGFS campaign")



# Mid haul location 
# ------------------------------------------------------------------------------
operation_CGFS_mid_haul <- operation_CGFS %>%
  mutate(lat = (endLatDD + startLatDD)/2,
         lon = (endLongDD + startLongDD)/2)

operation_CGFS_mid_haul_2005 <- operation_CGFS_mid_haul %>%
  filter(year >= 2005)

annual_color_2005_mid_haul <- basemap(limits = bbox ) +
  geom_spatial_point(data = operation_CGFS_mid_haul_2005, 
                     aes(x = startLongDD, y = startLatDD, color = factor(year)), size = 2, alpha = 0.6)

annual_map_2005_mid_haul <- basemap(limits = bbox ) +
  geom_spatial_point(data = operation_CGFS_mid_haul_2005, 
                     aes(x = startLongDD, y = startLatDD), color = "blue")+
  facet_wrap(~ year)+
  ggtitle("Spatial distribution of haul mid positions by year from the CGFS campaign")


# ------------------------------------------------------------------------------
# DATA EXPLORATION : SOLEA SOLEA DENSITY
# ------------------------------------------------------------------------------

# select data corresponding to the species of interest
sp_catch_CGFS <- catch_CGFS %>%
  dplyr::filter(scientificName == "Solea solea") %>%
  dplyr::select(serie, year, stationCode, operationID, haulID, totalNumber, totalWeightKg)

# Combine catch, coordinates and other parameters data frames
sp_data_CGFS <- operation_CGFS %>%
  left_join(sp_catch_CGFS, by = c("serie", "year", "stationCode", "operationID", "haulID")) 

# Replace missing values in totalNumber and totalWeightKg with 0
# Calculate midpoint coordinates for each haul
# Compute density in kg/km²
# Create a presence/absence variable (0 = absence, 1 = presence)
# Keep only data from 2015 onwards
sp_data_CGFS_density <- sp_data_CGFS %>%
  mutate(totalNumber = ifelse(is.na(totalNumber), 0, totalNumber),
         totalWeightKg = ifelse(is.na(totalWeightKg), 0, totalWeightKg)) %>%
  mutate(lat = (endLatDD + startLatDD)/2,
         lon = (endLongDD + startLongDD)/2) %>%
  mutate(densityKgKm2 = totalWeightKg/sweptAreaKm2) %>%
  mutate(presence_absence = ifelse(totalWeightKg == 0, 0, 1))%>%
  filter(year >= 2015)

# Map of Solea solea density by year
annual_map_sp <- basemap(limits = bbox ) +
  geom_spatial_point(data = sp_data_CGFS_density, 
                     aes(x = startLongDD, y = startLatDD, color = factor(presence_absence), size = densityKgKm2), 
                     alpha = 0.6)+
  scale_color_manual(values = c("0" = "red", "1" = "blue"), labels = c("Absence", "Presence"), name ="Presence") +
  scale_size_continuous(name ="Density (kg/km²)") +
  facet_wrap(~ year)+
  ggtitle(expression(paste("Spatial distribution of ", italic("Solea solea"), " density by year from the CGFS campaign")))

# Scatter plot of density by year
ggplot(sp_data_CGFS_density, aes(x = factor(year), y = densityKgKm2)) +
  geom_point()+
  theme_minimal() +
  labs(x = "Years", y = "Density (kg/km²)")+
  ggtitle(expression(paste("Density of ", italic("Solea solea"), " per haul across years")))

# Histogram of density by year
ggplot(sp_data_CGFS_density, aes(x = densityKgKm2)) +
  geom_histogram()+
  facet_wrap(~year)+
  labs(x = "Density (kg/km²)", y= "Number of hauls") +
  theme_minimal()+
  theme(strip.background = element_rect(color = "black", fill = "grey90"))+
  ggtitle(expression(paste("Frequency distribution of ", italic("Solea solea"), " density per year")))

# Column plot of total density by year
ggplot(sp_data_CGFS_density, aes(x = factor(year), y=densityKgKm2)) + 
  geom_col() +
  labs(y = "Density (kg/km²)", x = "Years") +
  ggtitle(expression(paste("Total density of ", italic("Solea solea"), " from CGFS hauls per year")))+
  theme_minimal()


