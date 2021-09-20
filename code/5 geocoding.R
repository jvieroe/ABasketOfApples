pacman::p_load(rio,
               tidyverse,
               magrittr,
               sf,
               tmap,
               raster,
               fasterize,
               tictoc,
               future,
               furrr,
               haven,
               stargazer,
               fixest,
               zoo,
               knitr,
               magick,
               stringi,
               broom,
               hrbrthemes,
               readr,
               nngeo,
               purrr)


path_paper <- "/Users/jeppeviero/Dropbox/02 PhD/0 Papers/13 apples/"
path_paper

path_data <- "/Users/jeppeviero/Dropbox/02 PhD/0 Papers/13 apples/data/"
path_data

path_graph <- "/Users/jeppeviero/Dropbox/02 PhD/0 Papers/13 apples/tex/"
path_graph

setwd("/Users/jeppeviero/Dropbox/02 PhD/0 Papers/13 apples")
getwd()

set_crs <- 5643 #  5514, 25832

# ---------------------------------------------------------
# Load data
# ---------------------------------------------------------
# ----- City data
bairoch_sf <- rio::import("data/city data/bairoch_data/bairoch.Rdata") %>% 
  distinct(., city_id, .keep_all = T) %>% 
  dplyr::select(c(city_id, latitude, longitude)) %>% 
  st_as_sf(.,
           coords = c("longitude", "latitude"),
           crs = 4326) %>% 
  st_transform(set_crs)

# ----- Map data
europe <- st_read(dsn = "data/europe_map",
                  layer = "europe_map",
                  crs = 4326) %>% 
  st_transform(set_crs)

tmap_mode("view")
tm_shape(europe) +
  tm_polygons(col = "red", alpha = 0.3)

# ----- Match
tic()
geo_data <- st_join(bairoch_sf, europe, join = st_nearest_feature) %>% 
  as.data.frame() %>% 
  dplyr::select(c(city_id,
                  SOVEREIGNT,
                  SUBREGION,
                  REGION_WB,
                  REGION_UN,
                  CONTINENT))
toc()



# ----- Export
save(geo_data,
     file = "data/geodata/geodata.Rdata")
