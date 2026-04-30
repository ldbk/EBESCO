

operation_ECGFS <- read_delim("01_DATA/survey/ECGFS/CGFS_1988_2024_ELFIC.V1.4_operation_2025-03-28.csv",
                              delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)

operation_ECGFS <- operation_ECGFS%>% distinct()%>% filter(year >= 2018)%>%
  mutate(lat = (endLatDD + startLatDD)/2,
         lon = (endLongDD + startLongDD)/2)%>%
  dplyr::select(lon, lat, year) %>% distinct()


operation_WCGFS <-  read_delim("01_DATA/survey/WCGFS/WCGFS_2018_2024_ELFIC.V1.5_operation_2025-03-31.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)

operation_WCGFS <- operation_WCGFS%>% distinct()%>%
  mutate(lat = (endLatDD + startLatDD)/2,
         lon = (endLongDD + startLongDD)/2)%>%
  dplyr::select(lon, lat, year) %>% distinct()



set.seed(123)

east_4folds <- operation_ECGFS %>%
  group_by(year) %>%   # annual spatial clustering 
  mutate(spatial_zone = kmeans(cbind(lon, lat), 
                               centers = 4,      # centers = 4 : 4 clusters = 4 zones spatiales par année
                               nstart = 1000)      # k-means 40 fois avec 40 initialisations différentes des centres
         $cluster) %>%                           # ID de zone (1 à 4)
  ungroup() %>%
  mutate(year = as.integer(as.character(year))) %>%
  mutate(year_index = dense_rank(year),
         fold_id = (year_index - 1) * 4 + spatial_zone,
         fold_id = factor(fold_id))

east_4folds %>%
  st_drop_geometry() %>%
  count(year, spatial_zone, fold_id) %>%
  arrange(year, spatial_zone) %>%
  print(n = Inf)

east_4folds %>%
  st_drop_geometry() %>%
  count(fold_id) %>%
  arrange(as.integer(as.character(fold_id)))%>%
  print(n = Inf)

ggplot(east_4folds) +
  geom_point(aes(x = lon, y = lat, color = factor(spatial_zone)),
             size = 5, alpha = 0.7) +
  geom_text(aes(x = lon, y = lat, label = fold_id),
            size = 3) +
  facet_wrap(~ year, nrow=2) +
  theme_bw() +
  labs(title = "Eastern English Channel — 28 folds (4 per year)", 
       color = "Zone", x = NULL, y = NULL)




west_3folds <- operation_WCGFS %>%
  group_by(year) %>%
  mutate(spatial_zone = kmeans(cbind(lon, lat), centers = 3, nstart = 1000)$cluster) %>%
  ungroup() %>%
  mutate(year = as.integer(as.character(year))) %>%
  mutate(year_index = dense_rank(year),
         fold_id = (year_index - 1) * 3 + spatial_zone,
         fold_id = factor(fold_id))

west_3folds %>%
  st_drop_geometry() %>%
  count(year, spatial_zone, fold_id) %>%
  arrange(year, spatial_zone)%>%
  print(n = Inf)

west_3folds %>%
  st_drop_geometry() %>%
  count(fold_id) %>%
  arrange(as.integer(as.character(fold_id)))%>%
  print(n = Inf)

ggplot(west_3folds) +
  geom_point(aes(x = lon, y = lat, color = factor(spatial_zone)),
             size = 5, alpha = 0.7) +
  geom_text(aes(x = lon, y = lat, label = fold_id),
            size = 3) +
  facet_wrap(~ year, nrow=2) +
  theme_bw() +
  labs(title = "Western English Channel — 21 folds (3 per year)", 
       color = "Zone", x = NULL, y = NULL)

saveRDS(west_3folds, file = here::here("01_DATA", "folds_CV", "west_3folds.rds"))
saveRDS(east_4folds, file = here::here("01_DATA", "folds_CV", "east_4folds.rds"))






# data_folds <- readRDS(here("01_DATA/folds_CV/east_4folds.rds"))
# 
# data_folds <- readRDS(here("01_DATA/folds_CV/west_3folds.rds"))
# 
# ggplot(data_folds) +
#   geom_point(aes(x = X, y = Y, color = factor(spatial_zone)),
#              size = 5, alpha = 0.7) +
#   geom_text(aes(x = X, y = Y, label = fold_id),
#             size = 3) +
#   facet_wrap(~ year, nrow=2) +
#   theme_bw() +
#   labs(title = "Manche Est — 28 folds (4 par année)", 
#        color = "Zone", x = "X", y = "Y")

