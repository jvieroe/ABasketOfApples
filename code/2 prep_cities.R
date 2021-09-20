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
# Prepare city data sets
# ---------------------------------------------------------
# ----- Load map data
europe_union <- st_read(dsn = "data/europe_map_union",
                         layer = "europe_map_union",
                         crs = 4326) %>% 
  st_transform(set_crs)

europe <- st_read(dsn = "data/europe_map",
                  layer = "europe_map",
                  crs = 4326) %>% 
  st_transform(set_crs)

# ----- Load Bairoch data
bairoch <- rio::import("data/city data/bairoch.dta")
names(bairoch)

bairoch <- bairoch %>% 
  arrange(country, city, year) %>% 
  group_by(country, city) %>% 
  mutate(indicator = cur_group_id()) %>% 
  ungroup()

# ----- Match to Bosker et al. data
bosker <- rio::import("data/city data/bagdad - london - final restat.dta")

bosker <- bosker %>% 
  rename(bosker_id = indicator)

tt <- bosker %>% filter(bosker_id %!in% bairoch$bosker_id) %>% 
  distinct(., bosker_id, .keep_all = T)

table(tt$country, useNA = "always") # One Greek city, otherwise only MENA
rm(tt)

bosker <- bosker %>% 
  dplyr::select(-c(city, country, latitude, longitude))

bairoch <- bairoch %>% 
  tidylog::left_join(.,
                     bosker,
                     by = c("bosker_id", "year")) %>% 
  filter(!is.na(bosker_id))

rm(bosker)

tt <- bairoch %>% 
  distinct(., indicator)
nrow(tt)  # 675 unique cities

rm(tt)

bairoch <- bairoch %>% 
  dplyr::select(-indicator)

bairoch <- bairoch %>% 
  arrange(country, city, year) %>% 
  group_by(country, city) %>% 
  mutate(city_id = cur_group_id()) %>% 
  ungroup()

bairoch <- bairoch %>% 
  relocate(city_id, .before = year)


# ----- Export Bairoch data
save(bairoch,
     file = "data/city data/bairoch_data/bairoch.Rdata")
rm(bairoch)

# ---------------------------------------------------------
# Prepare panel 1000 - 1800
# ---------------------------------------------------------
bairoch <- rio::import("data/city data/bairoch_data/bairoch.Rdata")

# ----- Interpolate Bairoch data
bairoch <- bairoch %>% 
  filter(year %in% seq(800, 1800, 100))
table(bairoch$year, useNA = "always")

bairoch_exp <- bairoch %>% 
  dplyr::select(city_id, year) %>% 
  arrange(city_id, year) %>% 
  group_by(city_id) %>% 
  summarise_all(~ list(seq(800, 1800, 100))) %>% 
  unnest(cols = c(city_id, year)) %>% 
  as.data.frame()

bairoch_exp <- bairoch_exp %>% 
  tidylog::left_join(.,
                     bairoch,
                     by = c("city_id", "year")) %>% 
  arrange(city_id, year) %>% 
  as.data.frame()

bairoch_exp <- bairoch_exp %>% 
  mutate(lnpop = log(city_pop + 1)) %>% 
  arrange(city_id, year) %>% 
  group_by(city_id) %>% 
  mutate(temp1 = zoo::na.approx(lnpop, year, na.rm = F),
         temp2 = zoo::na.approx(city_pop, year, na.rm = F)) %>% 
  ungroup() %>% 
  rename(city_pop_orig = city_pop) %>% 
  mutate(popexp = exp(temp1) - 1,
         poplin = temp2) %>% 
  dplyr::select(-c(lnpop, temp1, temp2))

bairoch <- bairoch_exp
rm(bairoch_exp)

bairoch <- bairoch %>% 
  group_by(city_id) %>% 
  fill(c(city, country, latitude, longitude, coord_source, bosker_id,
         dittmar_id, cox_id, blaydes_id),
       .direction = "downup") %>% 
  as.data.frame()

bairoch <- bairoch %>% 
  mutate(lnpopexp = log(popexp + 1),
         lnpoplin = log(poplin + 1))

class(bairoch)

bairoch <- bairoch %>% 
  mutate(interpol = ifelse(is.na(city_pop_orig),
                           1,
                           0))

tt <- bairoch %>% filter(interpol == 1 & !is.na(city_pop_orig))
nrow(tt)
tt <- bairoch %>% filter(interpol == 0 & is.na(city_pop_orig))
nrow(tt)
rm(tt)

tt <- bairoch %>% dplyr::select(city_pop_orig, lnpopexp, lnpoplin)
rm(tt)

bairoch <- bairoch %>% 
  dplyr::select(-city_pop_orig)

bairoch <- bairoch %>% 
  relocate(latitude, .after = country) %>% 
  relocate(longitude, .after = latitude) %>% 
  relocate(popexp, .after = longitude) %>% 
  relocate(lnpopexp, .after = popexp) %>% 
  relocate(poplin, .after = lnpopexp) %>% 
  relocate(lnpoplin, .after = poplin)

# ----- Export Bairoch data
save(bairoch,
     file = "data/city data/bairoch_data/bairoch_longpanel.Rdata")

rm(bairoch)

# ---------------------------------------------------------
# Prepare panel for IV: 1700 - 1800
# ---------------------------------------------------------
bairoch <- rio::import("data/city data/bairoch_data/bairoch.Rdata")

# ----- Interpolate Bairoch data
bairoch <- bairoch %>% 
  filter(year %in% seq(1700, 1800, 50))
table(bairoch$year, useNA = "always")

# bairoch_exp <- bairoch %>% 
#   dplyr::select(city_id, year) %>% 
#   arrange(city_id, year) %>% 
#   group_by(city_id) %>% 
#   summarise_all(~ list(seq(800, 1800, 100))) %>% 
#   unnest(cols = c(city_id, year)) %>% 
#   as.data.frame()
# 
# bairoch_exp <- bairoch_exp %>% 
#   tidylog::left_join(.,
#                      bairoch,
#                      by = c("city_id", "year")) %>% 
#   arrange(city_id, year) %>% 
#   as.data.frame()
# 
# bairoch_exp <- bairoch_exp %>% 
#   mutate(lnpop = log(city_pop + 1)) %>% 
#   arrange(city_id, year) %>% 
#   group_by(city_id) %>% 
#   mutate(temp1 = zoo::na.approx(lnpop, year, na.rm = F),
#          temp2 = zoo::na.approx(city_pop, year, na.rm = F)) %>% 
#   ungroup() %>% 
#   rename(city_pop_orig = city_pop) %>% 
#   mutate(popexp = exp(temp1) - 1,
#          poplin = temp2) %>% 
#   dplyr::select(-c(lnpop, temp1, temp2))
# 
# bairoch <- bairoch_exp
# rm(bairoch_exp)

bairoch <- bairoch %>% 
  mutate(lncity_pop = log(city_pop + 1))

# bairoch <- bairoch %>% 
#   group_by(city_id) %>% 
#   fill(c(city, country, latitude, longitude, coord_source, bosker_id,
#          dittmar_id, cox_id, blaydes_id),
#        .direction = "downup") %>% 
#   as.data.frame()
# 
# bairoch <- bairoch %>% 
#   mutate(lnpopexp = log(popexp + 1),
#          lnpoplin = log(poplin + 1))
# 
# class(bairoch)
# 
# bairoch <- bairoch %>% 
#   mutate(interpol = ifelse(is.na(city_pop_orig),
#                            1,
#                            0))
# 
# tt <- bairoch %>% filter(interpol == 1 & !is.na(city_pop_orig))
# nrow(tt)
# tt <- bairoch %>% filter(interpol == 0 & is.na(city_pop_orig))
# nrow(tt)
# rm(tt)
# 
# tt <- bairoch %>% dplyr::select(city_pop_orig, lnpopexp, lnpoplin)
# rm(tt)
# 
# bairoch <- bairoch %>% 
#   dplyr::select(-city_pop_orig)
# 
# bairoch <- bairoch %>% 
#   relocate(latitude, .after = country) %>% 
#   relocate(longitude, .after = latitude) %>% 
#   relocate(popexp, .after = longitude) %>% 
#   relocate(lnpopexp, .after = popexp) %>% 
#   relocate(poplin, .after = lnpopexp) %>% 
#   relocate(lnpoplin, .after = poplin)

# ----- Export Bairoch data
save(bairoch,
     file = "data/city data/bairoch_data/bairoch_shortpanel.Rdata")

rm(bairoch)


# ----- Load Dincecco & Onorato 2017 conflict data
bairoch <- rio::import("data/city data/bairoch_data/bairoch.Rdata")

conf <- rio::import(paste0(path_data,
                           "conflict data/historical_conflict_locations.dta")) %>% 
  filter(conflict_name != "ceuta") %>% 
  mutate(century = plyr::round_any(start_year, 100, floor)) %>% 
  st_as_sf(.,
           coords = c("longitude", "latitude"),
           crs = 4326) %>% 
  st_transform(set_crs)

# ----- Visualize city and conflict data
conf_sf <- conf %>% 
  st_as_sf(.,
           coords = c("longitude", "latitude"),
           crs = 4326) %>% 
  st_transform(set_crs)

bai_sf <- bairoch %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326) %>% 
  st_transform(set_crs)

tmap_mode("view")
tm_shape(europe) +
  tm_polygons(col = "red", alpha = 0.3) +
  tm_shape(bai_sf) +
  tm_dots(col = "black") +
  tm_shape(conf_sf) +
  tm_dots(col = "red")

rm(bai_sf, bairoch, conf, conf_sf, europe, europe_union)


