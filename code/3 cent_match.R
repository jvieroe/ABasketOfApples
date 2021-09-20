library(pacman)
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
               purrr,
               update = T)


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
# ----- Load map data
europe_union <- st_read(dsn = "data/europe_map_union",
                        layer = "europe_map_union",
                        crs = 4326) %>% 
  st_transform(set_crs)

# ----- Load Centennia data
centennia <- st_read(dsn = "/Users/jeppeviero/Dropbox/Under Empire/Data and code/5_centennia/centennia_full",
                     layer = "centennia_full",
                     crs = 4326) %>% 
  st_transform(set_crs)

centennia <- centennia %>% 
  filter(year %in% seq(1000, 1800, 10))

tmap_mode("view")
tm_shape(centennia[centennia$year == 1500,]) +
  tm_polygons(col = "red", alpha = 0.3)

# ----- Load Bairoch data
bairoch <- rio::import("data/city data/bairoch_data/bairoch.Rdata")

bairoch <- bairoch %>% 
  filter(year %in% seq(1000, 1800, 100))

raw <- bairoch %>% 
  distinct(., city_id, .keep_all = T) %>% 
  dplyr::select(city_id, latitude, longitude)

bairoch <- bairoch %>% 
  dplyr::select(city_id, year) %>% 
  arrange(city_id, year) %>% 
  group_by(city_id) %>% 
  summarise_all(~ list(seq(1000, 1800, 10))) %>% 
  unnest(cols = c(city_id, year)) %>% 
  as.data.frame() %>% 
  arrange(city_id, year)

bairoch <- bairoch %>% 
  tidylog::left_join(.,
                     raw,
                     by = "city_id")

rm(raw)

bairoch <- bairoch %>% 
  mutate(id_yr = paste(city_id,
                       year,
                       sep = "_"))

bairoch <- bairoch %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326) %>% 
  st_transform(set_crs)


list_bairoch <- split(bairoch,
                      f = bairoch$year)

# ---------------------------------------------------------
# Match cities to polities
# ---------------------------------------------------------
# ----- Split data
cent <- centennia

list_centennia <- split(cent,
                        f = cent$year)

# ----- Define function
func_centennia <- function(cent, bairoch, state_df1,
                           cities_temp, state_df2,
                           state_df) { 
  
  cent <- cent %>% 
    dplyr::select(-year)
  
  state_df1 <- st_join(bairoch, cent, join = st_intersects) %>% 
    as.data.frame() %>% 
    dplyr::select(c(city_id, year, state, id_yr)) %>% 
    filter(!is.na(state)) %>% 
    distinct(.,
             id_yr,
             .keep_all = T)
  
  bairoch_temp <- bairoch %>%
    filter(id_yr %!in% state_df1$id_yr)
  
  state_df2 <- st_join(bairoch_temp, cent, join = st_nn, k = 1, maxdist = 5*10^3,
                       progress = F) %>%
    as.data.frame() %>%
    dplyr::select(c(city_id, year, state, id_yr)) %>%
    distinct(.,
             id_yr,
             .keep_all = T)
  
  state_df <- rbind(state_df1, state_df2)
  
  return(state_df)
  
}


# ----- Apply function
no_cores <- availableCores() - 3
no_cores
plan(multisession, workers = no_cores)
tic()
coding_list <- furrr::future_map2(list_centennia,
                                  list_bairoch,
                                  func_centennia,
                                  .progress = T,
                                  .options = furrr_options(seed = TRUE,
                                                           scheduling = 1))
toc()
future::plan(sequential)

# ----- Unpack data
coding_data <- bind_rows(coding_list, .id = "year_id") %>% 
  dplyr::mutate(cent = as.character(state)) %>%
  filter(year == year_id) %>% 
  dplyr::select(c(city_id, year, cent, id_yr)) %>% 
  arrange(city_id, year)

any(duplicated(coding_data$id_yr))

coding_data <- coding_data %>% 
  dplyr::select(-id_yr)


# ----- Export affiliations data
state_affil <- coding_data %>% 
  filter(year %in% seq(1000, 1800, 100))

save(state_affil,
     file = "data/centennia_codings/cent_codings.Rdata")


# ----- Export affiliation stability data
stability <- coding_data %>% 
  mutate(century = plyr::round_any(year, 100, floor)) %>% 
  group_by(city_id, century) %>% 
  mutate(n_diff_state = n_distinct(cent)) %>% 
  mutate(first_cent = dplyr::first(cent),
         first_year = dplyr::first(year)) %>% 
  mutate(diff_cent = ifelse(cent != first_cent, 1, 0)) %>% 
  mutate(diff_cent = ifelse(is.na(cent) & !is.na(first_cent), 1, diff_cent)) %>% 
  mutate(diff_cent = ifelse(!is.na(cent) & is.na(first_cent), 1, diff_cent)) %>% 
  ungroup() %>% 
  mutate(state_change = ifelse(n_diff_state > 1, 1, 0)) %>% 
  group_by(city_id, century) %>% 
  mutate(change_year = ifelse(diff_cent == 1, year - first_year, NA)) %>% 
  mutate(first_change_cent = ifelse(state_change == 1, min(change_year, na.rm = T), NA)) %>% 
  ungroup() %>% 
  dplyr::select(-c(change_year, first_year)) %>% 
  # group_by(city_id) %>% 
  # mutate(first_cent = dplyr::first(cent),
  #        first_year = dplyr::first(year)) %>% 
  # mutate(change_year = ifelse(diff_cent == 1, year - first_year, NA)) %>% 
  # mutate(first_change = ifelse(state_change == 1, min(change_year, na.rm = T), NA)) %>% 
  # ungroup()  
  # dplyr::select(-c(diff_cent, change_year, first_year))
  dplyr::select(-diff_cent) %>% 
  relocate(n_diff_state, .after = first_change_cent)

names(stability)

stability <- stability %>% 
  distinct(., city_id, century, state_change,
           first_change_cent, n_diff_state,
           .keep_all = T)

save(stability,
     file = "data/centennia_codings/stability_codings.Rdata")

rm(stability, state_affil)
rm(coding_data, coding_list)

# ---------------------------------------------------------
# Calculate border distance (all borders)
# ---------------------------------------------------------
# ----- Split data
cent <- centennia %>% 
  filter(year %in% seq(1000, 1800, 100)) %>% 
  st_cast("MULTILINESTRING")

list_centennia <- split(cent,
                        f = cent$year)

bairoch <- bairoch %>% 
  filter(year %in% seq(1000, 1800, 100))

list_bairoch <- split(bairoch,
                      f = bairoch$year)


# ----- Define function
func_dist <- function(bairoch, cent, year_value, border_dist, df) {
  
  year_value <- bairoch$year[1]
  
  bairoch <- bairoch %>%
    dplyr::mutate(row_id = row_number()) 
  
  border_dist <- st_nn(bairoch, cent, k = 1, returnDist = T, progress = FALSE)
  border_dist <- border_dist$dist %>% unlist()
  
  bairoch <- bairoch %>% 
    as.data.frame() %>% 
    dplyr::select(c(city_id, row_id))
  
  df <- border_dist %>% 
    as.data.frame() %>% 
    dplyr::mutate(row_id = row_number(),
                  year = year_value) %>% 
    left_join(.,
              bairoch,
              by = "row_id") %>% 
    dplyr::select(-row_id)
  
  return(df)
  
}

# ----- Apply function
required_MB <- 1000
options(future.globals.maxSize = required_MB*1024^2)
no_cores <- availableCores() - 3
plan(multisession, workers = no_cores)
tic()
dist_list <- furrr::future_map2(list_bairoch,
                                list_centennia,
                                func_dist,
                                .progress = T,
                                .options = furrr_options(seed = TRUE,
                                                         scheduling = 1))
toc()
plan(sequential)

# ----- Unpack data
dist_data <- bind_rows(dist_list, .id = "year_id") %>% 
  dplyr::select(-year_id)

colnames(dist_data) <- c("border_distAll", "year", "city_id")

# ----- Export data
save(dist_data,
     file = "data/border_data/border_distAll.Rdata")

# century_borders <- dist_data %>% 
#   filter(year %in% seq(1000, 1800, 100))
# 
# save(century_borders,
#      file = "data/border_data/centuryborder_distAll.Rdata")

# # ---------------------------------------------------------
# # Calculate border distance (inland borders)
# # ---------------------------------------------------------
# # ----- Split data
# cent <- centennia
# 
# list_centennia <- split(cent,
#                         f = cent$year)
# 
# # ----- Define function
# func_dist <- function(bairoch, cent, year_value, border_dist, df) {
#   
#   year_value <- bairoch$year[1]
#   
#   # bairoch <- list_bairoch[[1]]
#   # cent <- list_centennia[[1]]
#   
#   bairoch <- bairoch %>%
#     dplyr::mutate(row_id = row_number())
#   
#   cent <- cent %>%
#     ms_simplify(keep = .2, keep_shapes = TRUE)
#   
#   cent <- cent %>%
#     ms_innerlines() %>%
#     as_tibble() %>%
#     st_as_sf()
#   
#   tm_shape(cent) +
#     tm_lines()
#   
#   border_dist <- st_nn(bairoch, cent, k = 1, returnDist = T, progress = FALSE)
#   border_dist <- border_dist$dist %>% unlist()
#   
#   bairoch <- bairoch %>% 
#     as.data.frame() %>% 
#     dplyr::select(c(city_id, row_id))
#   
#   df <- border_dist %>% 
#     as.data.frame() %>% 
#     dplyr::mutate(row_id = row_number(),
#                   year = year_value) %>% 
#     left_join(.,
#               bairoch,
#               by = "row_id") %>% 
#     dplyr::select(-row_id)
#   
#   return(df)
#   
# }
# 
# # ----- Apply function
# library(rmapshaper)
# required_MB <- 1000
# options(future.globals.maxSize = required_MB*1024^2)
# no_cores <- availableCores() - 2
# plan(multisession, workers = no_cores)
# tic()
# dist_list <- furrr::future_map2(list_bairoch,
#                                 list_centennia,
#                                 func_dist,
#                                 .progress = T,
#                                 .options = furrr_options(seed = TRUE,
#                                                          scheduling = 1))
# toc()
# plan(sequential)
# 
# # ----- Unpack data
# dist_data <- bind_rows(dist_list, .id = "year_id") %>% 
#   dplyr::select(-year_id)
# 
# colnames(dist_data) <- c("border_distPolity", "year", "city_id")
# 
# # ----- Export data
# save(dist_data,
#      file = "data/border_data/border_distPolity.Rdata")
# 
# century_borders <- dist_data %>% 
#   filter(year %in% seq(1000, 1800, 100))
# 
# save(century_borders,
#      file = "data/border_data/centuryborder_distPolity.Rdata")
