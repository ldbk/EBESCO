


# compute min and max latitude and longitude (rounding down the min and up the max for map limits)
lat_range <- range(sp_density_CGFS$lat)
lat_limit <- c(floor(lat_range[1]), ceiling(lat_range[2]))

lon_range <- range(sp_density_CGFS$lon)
lon_limit <- c(floor(lon_range[1]), ceiling(lon_range[2]))

lat_polygon <- c(lat_limit[1], lat_limit[1], lat_limit[2], lat_limit[2], lat_limit[1])
lon_polygon <- c(lon_limit[1], lon_limit[2], lon_limit[2], lon_limit[1], lon_limit[1])


# Create polygon boundary from longitude and latitude coordinates
domain_points_df <- data.frame(lon = lon_polygon, lat = lat_polygon, domain_id = 'domain')

domain_polygon <- domain_points_df %>%
  st_as_sf(coords = c("lon", "lat"), crs = st_crs("+proj=longlat +datum=WGS84")) %>%
  group_by(domain_id) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

sf_ices_rec <- sf::st_read(here("01_DATA/ICES_rectangles/ICES_Statistical_Rectangles_Eco.shp")) %>%
  sf::st_set_crs( "+proj=longlat +datum=WGS84") %>%
  st_intersection(domain_polygon)

sf_ices_areas <- sf::st_read(here("01_DATA/ICES_areas/ICES_Areas_20160601_cut_dense_3857.shp")) %>%
  sf::st_transform(crs =  st_crs(sf_ices_rec)) %>%
  st_make_valid() %>%
  st_intersection(domain_polygon)


