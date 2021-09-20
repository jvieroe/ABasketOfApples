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
values <- seq(1700, 1750, 50)
values

# ----- City data
bairoch_sf <- rio::import("data/city data/bairoch_data/bairoch_shortpanel.Rdata") %>% 
  dplyr::select(c(city_id, year, latitude, longitude)) %>% 
  filter(year %in% values) %>% 
  st_as_sf(.,
           coords = c("longitude", "latitude"),
           crs = 4326) %>% 
  st_transform(set_crs)

# ----- Conflict data
conf_sf <- rio::import("data/conflict data/historical_conflict_locations.dta") %>% 
  filter(conflict_name != "ceuta") %>% 
  mutate(halfcentury = plyr::round_any(start_year, 50, floor)) %>% 
  filter(halfcentury %in% values) %>% 
  st_as_sf(.,
           coords = c("longitude", "latitude"),
           crs = 4326) %>% 
  st_transform(set_crs)

table(conf_sf$halfcentury)

post1750 <- conf_sf %>% 
  filter(start_year > 1750)

janitor::tabyl(post1750$start_year)

# ---------------------------------------------------------
# Split data by century
# ---------------------------------------------------------
table(bairoch_sf$year)
table(conf_sf$halfcentury)

list_bairoch <- split(bairoch_sf,
                      f = bairoch_sf$year)

list_conf <- split(conf_sf,
                   f = conf_sf$halfcentury)

func_match <- function(bairoch, conflict, list_005, list_025, list_050, list_100,
                       vect_005, vect_025, vect_050, vect_100) {
  
  # Measure distance to nearest conflict in subsequent century
  bairoch <- bairoch %>% 
    mutate(dist_m = st_nn(bairoch,
                          conflict,
                          k = 1,
                          sparse = F,
                          returnDist = T,
                          progress = T)[[2]] %>% unlist()) %>%
    mutate(dist_nearest_conflict = dist_m / 1000) %>% 
    dplyr::select(-dist_m)
  
  # Number of conflicts in subsequent century (max 50 km)
  list_005 = st_nn(bairoch,
                   conflict,
                   k = nrow(conflict),
                   sparse = F,
                   returnDist = T,
                   progress = T,
                   maxdist = 5*(10^3))
  
  # Number of conflicts in subsequent century (max 50 km)
  list_025 = st_nn(bairoch,
                   conflict,
                   k = nrow(conflict),
                   sparse = F,
                   returnDist = T,
                   progress = T,
                   maxdist = 25*(10^3))
  
  # Number of conflicts in subsequent century (max 50 km)
  list_050 = st_nn(bairoch,
                   conflict,
                   k = nrow(conflict),
                   sparse = F,
                   returnDist = T,
                   progress = T,
                   maxdist = 50*(10^3))
  
  # Number of conflicts in subsequent century (max 100 km)
  list_100 = st_nn(bairoch,
                   conflict,
                   k = nrow(conflict),
                   sparse = F,
                   returnDist = T,
                   progress = T,
                   maxdist = 100*(10^3))
  
  # Add conflict counts to data set
  list_005 <- list_005[[2]]
  list_025 <- list_025[[2]]
  list_050 <- list_050[[2]]
  list_100 <- list_100[[2]]
  
  vect_005 <- map_dbl(list_005, length)
  vect_025 <- map_dbl(list_025, length)
  vect_050 <- map_dbl(list_050, length)
  vect_100 <- map_dbl(list_100, length)
  
  bairoch <- bairoch %>% 
    mutate(n_conf005 = vect_005,
           n_conf025 = vect_025,
           n_conf050 = vect_050,
           n_conf100 = vect_100)
  
  return(bairoch)
  
}

match_list <- purrr::map2(list_bairoch,
                          list_conf,
                          func_match)

match_data <- bind_rows(match_list, .id = "year_id") %>% 
  tibble() %>% 
  dplyr::select(-geometry) %>% 
  mutate(conflict005 = ifelse(dist_nearest_conflict <= 5,
                              1,
                              0)) %>% 
  mutate(conflict025 = ifelse(dist_nearest_conflict <= 25,
                              1,
                              0)) %>% 
  mutate(conflict050 = ifelse(dist_nearest_conflict <= 50,
                              1,
                              0)) %>% 
  mutate(conflict100 = ifelse(dist_nearest_conflict <= 100,
                              1,
                              0))

save(match_data,
     file = "data/conflict data/matched_conflict_short.Rdata")

