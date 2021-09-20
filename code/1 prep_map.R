pacman::p_load(rio,
               tidyverse,
               magrittr,
               sf,
               tmap,
               raster,
               fasterize,
               tictoc)

path_paper <- "/Users/jeppeviero/Dropbox/02 PhD/0 Papers/13 apples/"
path_paper

path_data <- "/Users/jeppeviero/Dropbox/02 PhD/0 Papers/13 apples/data/"
path_data

path_graph <- "/Users/jeppeviero/Dropbox/02 PhD/0 Papers/13 apples/tex/"
path_graph

setwd("/Users/jeppeviero/Dropbox/02 PhD/0 Papers/13 apples")
getwd()

# ---------------------------------------------------------
# Create Western Eurasia map
# ---------------------------------------------------------
# ----- Conflict data
conf_sf <- rio::import("data/historical_conflict_locations.dta") %>% 
  st_as_sf(.,
           coords = c("longitude", "latitude"),
           crs = 4326)

ne <- st_read(dsn = "data/ne_10m_admin_0_map_units",
              layer = "ne_10m_admin_0_map_units",
              crs = 4326) %>% 
  dplyr::select(c(SOVEREIGNT,
                  GEOUNIT,
                  SUBUNIT,
                  TYPE,
                  ADMIN,
                  GU_A3,
                  SUBREGION,
                  REGION_WB,
                  REGION_UN,
                  CONTINENT,
                  ADM0_A3,
                  ISO_A3)) %>% 
  dplyr::mutate(poly_id = row_number())

europe <- ne %>% 
  filter(REGION_UN == "Europe" | SOVEREIGNT == "Turkey" ) %>% 
  filter(SOVEREIGNT != "Russia")

rus <- ne %>% 
  filter(SOVEREIGNT == "Russia")
rus1 <- st_cast(rus, "POLYGON")[1,]
rus2 <- st_cast(rus, "POLYGON")[2,]

europe <- rbind(europe, rus1, rus2)

rm(ne, rus, rus1, rus2)

# ----- Last changes to Europe map (North African enclaves)
spain <- europe %>%
  filter(SOVEREIGNT == "Spain") %>% 
  st_cast("POLYGON") %>% 
  mutate(row_id = row_number()) %>% 
  filter(row_id != 1 & row_id != 2 & row_id != 22 & row_id != 19 & row_id != 21 & row_id != 20) %>% 
  dplyr::select(-row_id)

temp1 <- spain %>% 
  group_by(SOVEREIGNT) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  #summarise(do_union = T) %>%
  ungroup()

tm_shape(temp1) +
  tm_polygons(col = "red", alpha = 0.3) +
  tm_shape(conf_sf) +
  tm_dots(col = "red")

spain <- spain[1,] %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

temp1 <- temp1 %>% 
  left_join(.,
            spain,
            by = "SOVEREIGNT") %>% 
  relocate(geometry, .after = last_col())

temp2 <- europe %>% 
  filter(SOVEREIGNT != "Spain")

names(temp1)
names(temp2)

europe <- rbind(temp1, temp2)
rm(spain, temp1, temp2)

tm_shape(europe) +
  tm_polygons(col = "red", alpha = 0.3) +
  tm_shape(conf_sf) +
  tm_dots(col = "red")


europe <- st_crop(europe,
                  st_bbox(c(xmin = -13, xmax = 32,
                            ymin = 35, ymax = 65),
                          crs = st_crs(europe)))

# ----- Unionize Europe map
europe_union <- st_union(europe)


# ----- Crop both maps
europe <- st_crop(europe,
                  st_bbox(c(xmin = -13, xmax = 32,
                            ymin = 35, ymax = 65),
                          crs = st_crs(europe)))

europe_union <- st_crop(europe_union,
                        st_bbox(c(xmin = -13, xmax = 32,
                                  ymin = 35, ymax = 65),
                                crs = st_crs(europe_union)))


tm_shape(europe) +
  tm_polygons(col = "red", alpha = 0.3) +
  tm_shape(conf_sf) +
  tm_dots(col = "red")

tm_shape(europe_union) +
  tm_polygons(col = "red", alpha = 0.3) +
  tm_shape(conf_sf) +
  tm_dots(col = "red")


st_write(europe_union,
         dsn = "data/europe_map_union",
         layer = "europe_map_union",
         driver = "ESRI Shapefile",
         delete_layer = T,
         layer_options = "ENCODING=UTF-8")

st_write(europe,
         dsn = "data/europe_map",
         layer = "europe_map",
         driver = "ESRI Shapefile",
         delete_layer = T,
         layer_options = "ENCODING=UTF-8")
















# # ---------------------------------------------------------
# # Create spatial raster grid
# # ---------------------------------------------------------
# r <- raster(ncol = 180, 
#             nrow = 90)
# extent(r) <- extent(eurasia)
# # crs(r) <- crs_longlat
# 
# # ---------------------------------------------------------
# # Create gridded geographic data
# # ---------------------------------------------------------
# eurasia <- eurasia %>% 
#   dplyr::mutate(identify_col = row_number()) %>% 
#   dplyr::select(c(GEOUNIT, SOVEREIGNT, SUBREGION, REGION_WB, REGION_UN, CONTINENT, identify_col))
# 
# eurasia_df <- data.frame(ID = 1:length(unique(eurasia$identify_col)),
#                          identifier = unique(eurasia$identify_col))
# eurasia$ID <- eurasia_df$ID[match(eurasia$identify_col, eurasia_df$identifier)]
# 
# # thecrs <- "+proj=longlat +datum=WGS84 +no_defs"
# # r <- raster::projectRaster(r, crs = thecrs)
# # extent(r) <- extent(world)
# # proj4string(r) <- proj4string(world)
# 
# eurasia <- sf::st_cast(eurasia, 
#                        "MULTIPOLYGON")
# 
# eurasia_non_spatial <- eurasia %>% 
#   as.data.frame() %>% 
#   dplyr::select(-geometry)
# 
# rp_ratified <- fasterize(eurasia,
#                          r,
#                          field = "ID",
#                          fun = "last",
#                          background = 0,
#                          by = NULL)
# 
# rp_ratified <- ratify(rp_ratified)
# rat <- levels(rp_ratified)[[1]]
# rat$identifier <- eurasia_df$identifier[match(rat$ID, eurasia_df$ID)]
# rat$IDs <- eurasia_df$ID[match(rat$identifier, eurasia_df$identifier)]
# levels(rp_ratified) <- rat
# 
# eurasia_2 <- raster::rasterize(eurasia_union,
#                                rp_ratified,
#                                mask = T)
# 
# sf_eurasia <- as(eurasia_2,
#                  Class = "SpatialPolygonsDataFrame") %>%
#   st_as_sf() %>% 
#   mutate(ID = layer) %>%
#   left_join(., rat, by = "ID") %>%
#   mutate(identify_col = identifier) %>% 
#   dplyr::select(c(ID, identify_col)) %>%
#   left_join(.,
#             eurasia_non_spatial,
#             by = "identify_col") %>% 
#   mutate(cell_id = row_number(),
#          cell_area_km2 = unclass(st_area(.))/1000000) %>% 
#   dplyr::select(-c(ID.x,
#                    ID.y,
#                    identify_col)) %>% 
#   dplyr::rename(geo_c = GEOUNIT,
#                 sov_c = SOVEREIGNT,
#                 subr_c = SUBREGION,
#                 rwb_c = REGION_WB,
#                 run_c = REGION_UN,
#                 cont_c = CONTINENT,
#                 ca_km2 = cell_area_km2) %>% 
#   dplyr::select(-c(geo_c))
# 
# centroids <- sf_eurasia
# centroids <- centroids %>%
#   mutate(cell_id = row_number()) %>%
#   dplyr::select(cell_id)
# centroids <- st_centroid(centroids) %>% 
#   st_transform(crs = 4326)
# centroids <- st_geometry(centroids)
# 
# centroids_matrix <- st_coordinates(centroids) %>% 
#   as.data.frame() %>% 
#   dplyr::rename(lon_c = X,
#                 lat_c = Y) %>% 
#   mutate(cell_id = row_number())
# 
# sf_eurasia <- sf_eurasia %>% 
#   left_join(., 
#             centroids_matrix,
#             by = "cell_id") %>% 
#   as.data.frame() %>% 
#   dplyr::select(-geometry)
# 
# save(sf_eurasia,
#      file = "5_centennia/centennia_geodata.Rdata")
# 
# rm(eurasia_2, centroids, centroids_matrix, eurasia_df, eurasia_non_spatial, 
#    rat, rp_ratified, eurasia, eurasia_union, r, sf_eurasia)


