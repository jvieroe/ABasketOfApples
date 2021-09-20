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
bairoch <- rio::import("data/city data/bairoch_data/bairoch_longpanel.Rdata") %>% 
  filter(year %in% seq(1000, 1800, 100))

# ----- State data
affil <- rio::import("data/centennia_codings/cent_codings.Rdata")

# ----- Stability data
stability <- rio::import("data/centennia_codings/stability_codings.Rdata") %>% 
  dplyr::select(c(city_id, year, state_change, first_change_cent, n_diff_state))

# ----- Border data
borders <- rio::import("data/border_data/border_distAll.Rdata") %>% 
  filter(year %in% seq(1000, 1800, 100))

# ----- Conflict data
conf <- rio::import("data/conflict data/matched_conflict_long.Rdata")

# ----- Geodata
geodata <- rio::import("data/geodata/geodata.Rdata")

# ----- Grid data
grid_data <- rio::import("data/geodata/grid_data.Rdata")


# ---------------------------------------------------------
# Merge data
# ---------------------------------------------------------
bairoch <- bairoch %>% 
  tidylog::left_join(.,
                     geodata,
                     by = "city_id")

bairoch <- bairoch %>% 
  tidylog::left_join(.,
                     grid_data,
                     by = "city_id")

bairoch <- bairoch %>% 
  tidylog::left_join(.,
                     affil,
                     by = c("city_id",
                            "year"))

bairoch <- bairoch %>% 
  tidylog::left_join(.,
                     stability,
                     by = c("city_id",
                            "year"))

bairoch <- bairoch %>% 
  tidylog::left_join(.,
                     borders,
                     by = c("city_id",
                            "year"))

bairoch <- bairoch %>% 
  tidylog::left_join(.,
                     conf,
                     by = c("city_id",
                            "year"))


# ---------------------------------------------------------
# Export data
# ---------------------------------------------------------
save(bairoch,
     file = "data/apples_long.Rdata")


