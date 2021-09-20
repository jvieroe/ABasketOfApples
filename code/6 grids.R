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

# ---------------------------------------------------------
# Create grid
# ---------------------------------------------------------
st_crs(europe)
st_is_longlat(europe)

grid_spacing <- 150*10^3

grid_sf <- europe %>% 
  st_transform(set_crs) %>% 
  st_make_grid(.,
               cellsize = c(grid_spacing, grid_spacing),
               square = T) %>% 
  st_as_sf() %>% 
  mutate(area_km2 = unclass(st_area(.))/1000000) %>% 
  mutate(length = sqrt(area_km2)) %>% 
  mutate(grid_id = row_number())

hist(grid_sf$area_km2)
min(grid_sf$area_km2)
max(grid_sf$area_km2)
mean(grid_sf$area_km2)
sd(grid_sf$area_km2)

ggplot() +
  geom_sf(data = europe, fill = "grey90", alpha = 0.35, size = 0.25) +
  geom_sf(data = grid_sf, fill = NA, size = 0.1) +
  theme_list$theme_anatem_map +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

ggsave(plot = last_plot(),
       file = paste0(path_graph,
                     "grid_map.png"))
knitr::plot_crop(paste0(path_graph,
                        "grid_map.png"),
                 quiet = T)

city_grid <- st_join(bairoch_sf, grid_sf, join = st_nearest_feature) %>% 
  as.data.frame() %>% 
  dplyr::select(-c(geometry, area_km2, length))

# ----- Export
save(city_grid,
     file = "data/geodata/grid_data.Rdata")
