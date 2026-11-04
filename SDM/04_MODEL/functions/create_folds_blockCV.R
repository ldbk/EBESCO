
# ============================================================================== #
# SPATIO-TEMPORAL CROSS-VALIDATION FOLD GENERATION
# ============================================================================== #
#
# Description:
#   Generate spatially structured, year-specific folds for the Eastern Channel
#   Ground Fish Survey (ECGFS) and Western Channel Ground Fish Survey (WCGFS).
#
# Objectives:
#   1. Extract sampling locations from survey operations (2018–2024).
#   2. Identify spatial clusters within each survey year using k-means.
#   3. Assign a unique fold ID to each year-specific spatial cluster.
#   4. Assess the distribution of sampling operations across folds.
#   5. Visualize the spatial distribution of folds for each survey year.
#   6. Save fold assignments for subsequent spatio-temporal cross-validation.
#
# Methodology:
#   - ECGFS: 4 spatial clusters per year.
#   - WCGFS: 3 spatial clusters per year.
#   - Clustering is performed independently for each survey year.
#   - K-means is applied to the geographical coordinates of sampling locations.
#   - A fixed random seed ensures reproducibility.
#
# Inputs:
#   - ECGFS and WCGFS survey operation datasets (CSV).
#   - Natural Earth land polygons (RDS).
#
# Outputs:
#   - west_3folds.rds: WCGFS fold assignments.
#   - east_4folds.rds: ECGFS fold assignments.
#
# ============================================================================== #

# ------------------------------------------------------------------------------ # 
#### 1. LOAD AND PREPARE SURVEY DATA ####
# ------------------------------------------------------------------------------ # 

# Load ECGFS survey operations
operation_ECGFS <- read_delim("01_DATA/survey/ECGFS/CGFS_1988_2024_ELFIC.V1.4_operation_2025-03-28.csv",
                              delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)

# Remove duplicates, retain surveys from 2018 onwards and calculate the midpoint 
# coordinates of each sampling operation
operation_ECGFS <- operation_ECGFS%>% 
  distinct()%>% 
  filter(year >= 2018)%>%
  mutate(lat = (endLatDD + startLatDD)/2,
         lon = (endLongDD + startLongDD)/2)%>%
  dplyr::select(lon, lat, year) %>% distinct()

# Load WCGFS survey operations
operation_WCGFS <-  read_delim("01_DATA/survey/WCGFS/WCGFS_2018_2024_ELFIC.V1.5_operation_2025-03-31.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)

# Remove duplicates and calculate sampling operation midpoints
operation_WCGFS <- operation_WCGFS%>% distinct()%>%
  mutate(lat = (endLatDD + startLatDD)/2,
         lon = (endLongDD + startLongDD)/2)%>%
  dplyr::select(lon, lat, year) %>% distinct()

# Load Natural Earth land polygons
ne_10m_land <- readRDS("~/EBESCO/SDM/01_DATA/ne_10m_land.rds")


# ------------------------------------------------------------------------------ # 
#### 2. ECGFS GENERATE SPATIO-TEMPORAL FOLDS ####
# ------------------------------------------------------------------------------ # 

# Set random seed for reproducible clustering
set.seed(123)

# For each year, sampling locations are divided into 4 spatial clusters using
# k-means based on longitude and latitude. The algorithm is run with 1000 random
# initializations, retaining the solution with the lowest within-cluster sum of squares.
# Each year × spatial cluster combination is then assigned a unique fold ID,
# resulting in 4 distinct folds per year.
east_4folds <- operation_ECGFS %>%
  group_by(year) %>%  
  mutate(spatial_zone = kmeans(cbind(lon, lat), centers = 4, nstart = 1000)$cluster) %>%                           
  ungroup() %>%
  mutate(year = as.integer(as.character(year))) %>%
  mutate(year_index = dense_rank(year),
         fold_id = (year_index - 1) * 4 + spatial_zone,
         fold_id = factor(fold_id))

# ------------------------------------------------------------------------------ # 
#### 3. ECGFS : CHECK FOLD DISTRIBUTION ####
# ------------------------------------------------------------------------------ # 

# Count sampling locations within each year-specific spatial cluster
east_4folds %>%
  st_drop_geometry() %>%
  count(year, spatial_zone, fold_id) %>%
  arrange(year, spatial_zone) %>%
  print(n = Inf)

# Check the number of sampling locations assigned to each fold
east_4folds %>%
  st_drop_geometry() %>%
  count(fold_id) %>%
  arrange(as.integer(as.character(fold_id)))%>%
  print(n = Inf)


# ------------------------------------------------------------------------------ # 
#### 4. ECGFS : VISUALIZE SPATIO-TEMPORAL FOLDS ####
# ------------------------------------------------------------------------------ # 

# Define colors for spatial clusters
color_manual <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c")

# Map sampling locations colored by spatial cluster,
# with fold IDs displayed and separate panels for each year
ggplot() +
  geom_sf(data = ne_10m_land, fill = "grey85", color = "grey85") +
  geom_point(data = east_4folds,
             aes(x = lon, y = lat, color = factor(spatial_zone)),
             size = 6.5, alpha = 0.7) +
  geom_text(data = east_4folds,
            aes(x = lon, y = lat, label = fold_id),
            size = 3.8) +
  facet_wrap(~ year, nrow = 2) +
  scale_color_manual(values = color_manual) +
  scale_x_continuous(breaks = seq(-1, 1, 1))+
  coord_sf(xlim = range(east_4folds$lon, na.rm = TRUE) + c(-0.2, 0.2),
           ylim = range(east_4folds$lat, na.rm = TRUE) + c(-0.2, 0.2),
           expand = FALSE) +
  theme_bw() +
  labs(x = NULL, y = NULL) +
  theme(text = element_text(size = 14),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "grey30", color = NA),
        strip.text = element_text(color = "white", face = "bold"))


# ------------------------------------------------------------------------------ # 
#### 5. WCGFS : GENERATE SPATIO-TEMPORAL FOLDS ####
# ------------------------------------------------------------------------------ # 

# Set random seed for reproducible clustering
set.seed(123)

# For each year, sampling locations are divided into 3 spatial clusters using
# k-means based on longitude and latitude. The algorithm is run with 1000 random
# initializations, retaining the solution with the lowest within-cluster sum of squares.
# Each year × spatial cluster combination is then assigned a unique fold ID,
# resulting in 4 distinct folds per year.
west_3folds <- operation_WCGFS %>%
  group_by(year) %>%
  mutate(spatial_zone = kmeans(cbind(lon, lat), centers = 3, nstart = 1000)$cluster) %>%
  ungroup() %>%
  mutate(year = as.integer(as.character(year))) %>%
  mutate(year_index = dense_rank(year),
         fold_id = (year_index - 1) * 3 + spatial_zone,
         fold_id = factor(fold_id))


# ------------------------------------------------------------------------------ # 
#### 6. WCGFS : CHECK FOLD DISTRIBUTION ####
# ------------------------------------------------------------------------------ # 

# Count sampling locations within each year-specific spatial cluster
west_3folds %>%
  st_drop_geometry() %>%
  count(year, spatial_zone, fold_id) %>%
  arrange(year, spatial_zone)%>%
  print(n = Inf)

# Check the number of sampling locations assigned to each fold
west_3folds %>%
  st_drop_geometry() %>%
  count(fold_id) %>%
  arrange(as.integer(as.character(fold_id)))%>%
  print(n = Inf)

# ------------------------------------------------------------------------------ # 
#### 7. WCGFS : VISUALIZE SPATIO-TEMPORAL FOLDS ####
# ------------------------------------------------------------------------------ # 

# Define colors for spatial clusters
color_manual <- c("#a6cee3", "#1f78b4", "#b2df8a")

# Map sampling locations colored by spatial cluster,
# with fold IDs displayed and separate panels for each year
ggplot() +
  geom_sf(data = ne_10m_land, fill = "grey85", color = "grey85") +
  geom_point(data = west_3folds,
             aes(x = lon, y = lat, color = factor(spatial_zone)),
             size = 6.5, alpha = 0.7) +
  geom_text(data = west_3folds,
            aes(x = lon, y = lat, label = fold_id),
            size = 3.8) +
  facet_wrap(~ year, nrow = 2) +
  scale_color_manual(values = color_manual) +
  scale_x_continuous(breaks = seq(-1, 1, 1))+
  coord_sf(xlim = range(west_3folds$lon, na.rm = TRUE) + c(-0.2, 0.2),
           ylim = range(west_3folds$lat, na.rm = TRUE) + c(-0.2, 0.2),
           expand = FALSE) +
  theme_bw() +
  labs(x = NULL, y = NULL) +
  theme(text = element_text(size = 14),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "grey30", color = NA),
        strip.text = element_text(color = "white", face = "bold"))


# ------------------------------------------------------------------------------ # 
#### 8. SAVE SPATIO-TEMPORAL FOLD ASSIGNMENTS ####
# ------------------------------------------------------------------------------ # 
saveRDS(west_3folds, file = here::here("01_DATA", "folds_CV", "west_3folds.rds"))
saveRDS(east_4folds, file = here::here("01_DATA", "folds_CV", "east_4folds.rds"))





