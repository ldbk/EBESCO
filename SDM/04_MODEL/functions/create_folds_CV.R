


set.seed(123)

east_4folds <- data_CGFS_east %>%
  group_by(year) %>%   # annual spatial clustering 
  mutate(spatial_zone = kmeans(cbind(X, Y), 
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
  geom_point(aes(x = X, y = Y, color = factor(spatial_zone)),
             size = 5, alpha = 0.7) +
  geom_text(aes(x = X, y = Y, label = fold_id),
            size = 3) +
  facet_wrap(~ year) +
  theme_bw() +
  labs(title = "Manche Est — 21 folds (3 par année)", 
       color = "Zone", x = "X", y = "Y")




west_3folds <- data_CGFS_west %>%
  group_by(year) %>%
  mutate(spatial_zone = kmeans(cbind(X, Y), centers = 3, nstart = 1000)$cluster) %>%
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
  geom_point(aes(x = X, y = Y, color = factor(spatial_zone)),
             size = 5, alpha = 0.7) +
  geom_text(aes(x = X, y = Y, label = fold_id),
            size = 3) +
  facet_wrap(~ year) +
  theme_bw() +
  labs(title = "Manche Est — 21 folds (3 par année)", 
       color = "Zone", x = "X", y = "Y")

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

