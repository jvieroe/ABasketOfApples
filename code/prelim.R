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
               Manu,
               janitor)


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
# Define regression table dictionary
# ---------------------------------------------------------
myDict <- c("city_id" = "City",
            "lnpopexp" = "ln Urban pop.",
            "year" = "Century",
            "year_fac" = "Century",
            "cent" = "State",
            "SOVEREIGNT" = "Country",
            "grid_id" = "Grid",
            "conflict005" = "Battle dummy (5km)",
            "n_conf005" = "No. battles (5km)")



# ---------------------------------------------------------
# Load data
# ---------------------------------------------------------
# ----- City data
apples <- rio::import("data/apples_long.Rdata")
names(apples)

apples <- apples %>% 
  mutate(lag_conflict005 = dplyr::lag(conflict005, 1),
         lag_conflict025 = dplyr::lag(conflict025, 1),
         lag_conflict050 = dplyr::lag(conflict050, 1),
         lag_conflict100 = dplyr::lag(conflict100, 1))


apples <- apples %>% 
  mutate(grid_id = factor(grid_id),
         city_id = factor(city_id),
         year_fac = factor(year))

# ---------------------------------------------------------
# Descriptive statistics
# ---------------------------------------------------------
df <- apples %>% 
  dplyr::select(c(city_id,
                  year,
                  latitude,
                  longitude,
                  lnpopexp,
                  sea,
                  river,
                  hub_3rr,
                  elevation_m,
                  rugg10,
                  bishop,
                  soilquality,
                  ecozones,
                  archbishop,
                  capital,
                  university,
                  commune,
                  total_pop_country,
                  border_distAll,
                  interpol,
                  conflict005,
                  conflict025,
                  conflict050,
                  conflict100,
                  n_conf005,
                  n_conf025,
                  n_conf050,
                  n_conf100))

library(psych)

df2 <- print(psych::describe(df),
             digits = 3)

kable(df2, "latex", booktabs = TRUE)


# ---------------------------------------------------------
# Run models
# ---------------------------------------------------------
# ----- Table 1: Conflict dummy (5km)
mlist <- list()
mlist$m1 <- feols(conflict005 ~ lnpopexp
                  | city_id,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m2 <- feols(conflict005 ~ lnpopexp | city_id + year_fac,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m3 <- feols(conflict005 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university 
                  | city_id + year_fac,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m4 <- feols(conflict005 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university 
                  | city_id + year_fac + cent,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m5 <- feols(conflict005 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university 
                  | city_id + year_fac + cent + grid_id^year_fac,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m6 <- feols(conflict005 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university + lag_conflict005 
                  | city_id + year_fac + cent + grid_id^year_fac,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m7 <- feols(conflict005 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university + lag_conflict005
                  + soilquality:year_fac
                  + rugg10:year_fac
                  + sea:year_fac
                  + river:year_fac
                  + ecozones:year_fac
                  + elevation_m:year_fac
                  + hub_3rr:year_fac
                  | city_id + year_fac + cent + grid_id^year_fac,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m8 <- feols(conflict005 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university + lag_conflict005
                  + soilquality:year_fac
                  + rugg10:year_fac
                  + sea:year_fac
                  + river:year_fac
                  + ecozones:year_fac
                  + elevation_m:year_fac
                  + hub_3rr:year_fac
                  + border_distAll
                  | city_id + year_fac + cent + grid_id^year_fac,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)

etable(mlist, keep = c("lnpopexp"))

etable(mlist,
       file = paste0(path_graph,
                     "/tab_DummyDist005.tex"),
       replace = T,
       sdBelow = T,
       extraline = list("{title: \\midrule Model details} Time-variant BBvZ controls" =
                          c ("", "", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
                        "Lagged conflict dummy" =
                          c ("", "", "", "", "", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
                        "Time-invariant BBvZ controls $\\times$ year " =
                          c ("", "", "", "", "", "", "$\\checkmark$", "$\\checkmark$"),
                        "Dist. to border" =
                          c ("", "", "", "", "", "", "", "$\\checkmark$")),
       tex = T,
       dict = myDict,
       signifCode = c(`***` = 0.01, `**` = 0.05, `*` = 0.10),
       digits = "r3",
       keep = c("lnpopexp", "ln Urban pop."),
       fitstat = c("n", "ar2"),
       powerBelow = -10,
       depvar = T,
       style.tex = style.tex("aer",
                             tabular = "*",
                             fixef.where = "var",
                             fixef.suffix = " fixed effects",
                             fixef.title = "\\midrule",
                             var.title = "\\midrule",
                             stats.title = "\\midrule",
                             yesNo = "$\\checkmark$",
                             tablefoot = F))

# log([100 + p]/100)
log((100 + 25)/100)
log((100 + 25)/100)*0.068
log((100 + 25)/100)*0.036
log((100 + 25)/100)*0.064
log((100 + 25)/100)*0.057

hist(apples$n_conf005)

apples <- apples %>% 
  mutate(lncount = log(n_conf005))

hist(apples$lncount)

# ----- Conflict, distance summary
mlist <- list()
# 5 km
mlist$m1 <- feols(conflict005 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university 
                  | city_id + year_fac + cent,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m2 <- feols(conflict005 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university 
                  | city_id + year_fac + cent + grid_id^year_fac,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m3 <- feols(conflict005 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university + lag_conflict005 
                  | city_id + year_fac + cent + grid_id^year_fac,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m4 <- feols(conflict005 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university + lag_conflict005
                  + soilquality:year_fac
                  + rugg10:year_fac
                  + sea:year_fac
                  + river:year_fac
                  + ecozones:year_fac
                  + elevation_m:year_fac
                  + hub_3rr:year_fac
                  | city_id + year_fac + cent + grid_id^year_fac,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m5 <- feols(conflict005 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university + lag_conflict005
                  + soilquality:year_fac
                  + rugg10:year_fac
                  + sea:year_fac
                  + river:year_fac
                  + ecozones:year_fac
                  + elevation_m:year_fac
                  + hub_3rr:year_fac
                  + border_distAll
                  | city_id + year_fac + cent + grid_id^year_fac,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
# 25 km
mlist$m6 <- feols(conflict025 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university 
                  | city_id + year_fac + cent,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m7 <- feols(conflict025 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university 
                  | city_id + year_fac + cent + grid_id^year_fac,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m8 <- feols(conflict025 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university + lag_conflict025 
                  | city_id + year_fac + cent + grid_id^year_fac,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m9 <- feols(conflict025 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university + lag_conflict025
                  + soilquality:year_fac
                  + rugg10:year_fac
                  + sea:year_fac
                  + river:year_fac
                  + ecozones:year_fac
                  + elevation_m:year_fac
                  + hub_3rr:year_fac
                  | city_id + year_fac + cent + grid_id^year_fac,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m10 <- feols(conflict025 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university + lag_conflict025
                  + soilquality:year_fac
                  + rugg10:year_fac
                  + sea:year_fac
                  + river:year_fac
                  + ecozones:year_fac
                  + elevation_m:year_fac
                  + hub_3rr:year_fac
                  + border_distAll
                  | city_id + year_fac + cent + grid_id^year_fac,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)


# 50 km
mlist$m11 <- feols(conflict050 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university 
                  | city_id + year_fac + cent,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m12 <- feols(conflict050 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university 
                  | city_id + year_fac + cent + grid_id^year_fac,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m13 <- feols(conflict050 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university + lag_conflict050 
                  | city_id + year_fac + cent + grid_id^year_fac,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m14 <- feols(conflict050 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university + lag_conflict050
                  + soilquality:year_fac
                  + rugg10:year_fac
                  + sea:year_fac
                  + river:year_fac
                  + ecozones:year_fac
                  + elevation_m:year_fac
                  + hub_3rr:year_fac
                  | city_id + year_fac + cent + grid_id^year_fac,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m15 <- feols(conflict050 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university + lag_conflict050
                  + soilquality:year_fac
                  + rugg10:year_fac
                  + sea:year_fac
                  + river:year_fac
                  + ecozones:year_fac
                  + elevation_m:year_fac
                  + hub_3rr:year_fac
                  + border_distAll
                  | city_id + year_fac + cent + grid_id^year_fac,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)

# 100 km
mlist$m16 <- feols(conflict100 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university 
                  | city_id + year_fac + cent,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m17 <- feols(conflict100 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university 
                  | city_id + year_fac + cent + grid_id^year_fac,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m18 <- feols(conflict100 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university + lag_conflict100 
                  | city_id + year_fac + cent + grid_id^year_fac,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m19 <- feols(conflict100 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university + lag_conflict100
                  + soilquality:year_fac
                  + rugg10:year_fac
                  + sea:year_fac
                  + river:year_fac
                  + ecozones:year_fac
                  + elevation_m:year_fac
                  + hub_3rr:year_fac
                  | city_id + year_fac + cent + grid_id^year_fac,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m20 <- feols(conflict100 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university + lag_conflict100
                  + soilquality:year_fac
                  + rugg10:year_fac
                  + sea:year_fac
                  + river:year_fac
                  + ecozones:year_fac
                  + elevation_m:year_fac
                  + hub_3rr:year_fac
                  + border_distAll
                  | city_id + year_fac + cent + grid_id^year_fac,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)

fit_list <- list()
values <- seq(1, 20, 1)

for (i in seq_along(values)) {
  
  model <- mlist[[values[i]]]
  
  model_no <- values[i]
  
  ct <- coeftable(model) %>% 
    tibble::rownames_to_column(., "predictor") %>% 
    filter(grepl(("lnpopexp"), predictor)) %>% 
    dplyr::rename(estimate = Estimate,
                  std_error = 'Std. Error') %>%
    dplyr::select(c(predictor, estimate, std_error)) %>% 
    dplyr::mutate(conf_low = estimate - (1.96 * std_error),
                  conf_high = estimate + (1.96 * std_error)) %>% 
    dplyr::mutate(m_no = model_no)
  
  fit_list[[i]] <- ct
  
}

fit_data <- bind_rows(fit_list) %>% 
  mutate(dist = case_when(m_no %in% c(1, 2, 3, 4, 5) ~ "5km",
                          m_no %in% c(6, 7, 8, 9, 10) ~ "25km",
                          m_no %in% c(11, 12, 13, 14, 15) ~ "50km",
                          m_no %in% c(16, 17, 18, 19, 20) ~ "100km")) %>% 
  mutate(type = case_when(m_no %in% c(1, 6, 11, 16) ~ "Model (4)",
                          m_no %in% c(2, 7, 12, 17) ~ "Model (5)",
                          m_no %in% c(3, 8, 13, 18) ~ "Model (6)",
                          m_no %in% c(4, 9, 14, 19) ~ "Model (7)",
                          m_no %in% c(5, 10, 15, 20) ~ "Model (8)")) %>% 
  mutate(distance = factor(dist,
                       levels = c("5km", "25km", "50km", "100km")),
         type = factor(type,
                       levels = c("Model (4)", "Model (5)", "Model (6)", "Model (7)", "Model (8)")))

ggplot(fit_data, aes(x = type, y = estimate, ymin = conf_low, ymax = conf_high)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(col = type),
                  position = position_dodge(width = .5)) +
  scale_colour_manual(values = get_pal("Kotare")) +
  scale_y_continuous(breaks = seq(-0.05, 0.10, 0.025),
                     labels = scaleFUN) +
  theme_list$theme_apples +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom") +
  facet_wrap(~ distance)

ggsave(plot = last_plot(),
       file = paste0(path_graph,
                     "/DistCoefs.png"))
knitr::plot_crop(paste0(path_graph,
                        "/DistCoefs.png"),
                 quiet = T)



# ----- Conflict count (5km)
mlist <- list()
mlist$m1 <- feols(n_conf005 ~ lnpopexp
                  | city_id,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m2 <- feols(n_conf005 ~ lnpopexp | city_id + year_fac,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m3 <- feols(n_conf005 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university 
                  | city_id + year_fac,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m4 <- feols(n_conf005 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university 
                  | city_id + year_fac + cent,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m5 <- feols(n_conf005 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university 
                  | city_id + year_fac + cent + grid_id^year_fac,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m6 <- feols(n_conf005 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university + lag_conflict005 
                  | city_id + year_fac + cent + grid_id^year_fac,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m7 <- feols(n_conf005 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university + lag_conflict005
                  + soilquality:year_fac
                  + rugg10:year_fac
                  + sea:year_fac
                  + river:year_fac
                  + ecozones:year_fac
                  + elevation_m:year_fac
                  + hub_3rr:year_fac
                  | city_id + year_fac + cent + grid_id^year_fac,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m8 <- feols(n_conf005 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university + lag_conflict005
                  + soilquality:year_fac
                  + rugg10:year_fac
                  + sea:year_fac
                  + river:year_fac
                  + ecozones:year_fac
                  + elevation_m:year_fac
                  + hub_3rr:year_fac
                  + border_distAll
                  | city_id + year_fac + cent + grid_id^year_fac,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)

etable(mlist, keep = c("lnpopexp"))

etable(mlist,
       file = paste0(path_graph,
                     "/tab_CountDist005.tex"),
       replace = T,
       sdBelow = T,
       extraline = list("{title: \\midrule Model details} Time-variant BBvZ controls" =
                          c ("", "", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
                        "Lagged conflict dummy" =
                          c ("", "", "", "", "", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
                        "Time-invariant BBvZ controls $\\times$ year " =
                          c ("", "", "", "", "", "", "$\\checkmark$", "$\\checkmark$"),
                        "Dist. to border" =
                          c ("", "", "", "", "", "", "", "$\\checkmark$")),
       tex = T,
       dict = myDict,
       signifCode = c(`***` = 0.01, `**` = 0.05, `*` = 0.10),
       digits = "r3",
       keep = c("lnpopexp", "ln Urban pop."),
       fitstat = c("n", "ar2"),
       powerBelow = -10,
       depvar = T,
       style.tex = style.tex("aer",
                             tabular = "*",
                             fixef.where = "var",
                             fixef.suffix = " fixed effects",
                             fixef.title = "\\midrule",
                             var.title = "\\midrule",
                             stats.title = "\\midrule",
                             yesNo = "$\\checkmark$",
                             tablefoot = F))

# log([100 + p]/100)
log((100 + 25)/100)
log((100 + 25)/100)*0.057


# ----- No. Conflict, distance summary
mlist <- list()
# 5 km
mlist$m1 <- feols(n_conf005 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university 
                  | city_id + year_fac + cent,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m2 <- feols(n_conf005 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university 
                  | city_id + year_fac + cent + grid_id^year_fac,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m3 <- feols(n_conf005 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university + lag_conflict005 
                  | city_id + year_fac + cent + grid_id^year_fac,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m4 <- feols(n_conf005 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university + lag_conflict005
                  + soilquality:year_fac
                  + rugg10:year_fac
                  + sea:year_fac
                  + river:year_fac
                  + ecozones:year_fac
                  + elevation_m:year_fac
                  + hub_3rr:year_fac
                  | city_id + year_fac + cent + grid_id^year_fac,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m5 <- feols(n_conf005 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university + lag_conflict005
                  + soilquality:year_fac
                  + rugg10:year_fac
                  + sea:year_fac
                  + river:year_fac
                  + ecozones:year_fac
                  + elevation_m:year_fac
                  + hub_3rr:year_fac
                  + border_distAll
                  | city_id + year_fac + cent + grid_id^year_fac,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
# 25 km
mlist$m6 <- feols(n_conf025 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university 
                  | city_id + year_fac + cent,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m7 <- feols(n_conf025 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university 
                  | city_id + year_fac + cent + grid_id^year_fac,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m8 <- feols(n_conf025 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university + lag_conflict025 
                  | city_id + year_fac + cent + grid_id^year_fac,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m9 <- feols(n_conf025 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university + lag_conflict025
                  + soilquality:year_fac
                  + rugg10:year_fac
                  + sea:year_fac
                  + river:year_fac
                  + ecozones:year_fac
                  + elevation_m:year_fac
                  + hub_3rr:year_fac
                  | city_id + year_fac + cent + grid_id^year_fac,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"),
                  data = apples)
mlist$m10 <- feols(n_conf025 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university + lag_conflict025
                   + soilquality:year_fac
                   + rugg10:year_fac
                   + sea:year_fac
                   + river:year_fac
                   + ecozones:year_fac
                   + elevation_m:year_fac
                   + hub_3rr:year_fac
                   + border_distAll
                   | city_id + year_fac + cent + grid_id^year_fac,
                   panel.id = c('city_id', 'year'),
                   cluster = c("city_id"),
                   data = apples)


# 50 km
mlist$m11 <- feols(n_conf050 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university 
                   | city_id + year_fac + cent,
                   panel.id = c('city_id', 'year'),
                   cluster = c("city_id"),
                   data = apples)
mlist$m12 <- feols(n_conf050 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university 
                   | city_id + year_fac + cent + grid_id^year_fac,
                   panel.id = c('city_id', 'year'),
                   cluster = c("city_id"),
                   data = apples)
mlist$m13 <- feols(n_conf050 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university + lag_conflict050 
                   | city_id + year_fac + cent + grid_id^year_fac,
                   panel.id = c('city_id', 'year'),
                   cluster = c("city_id"),
                   data = apples)
mlist$m14 <- feols(n_conf050 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university + lag_conflict050
                   + soilquality:year_fac
                   + rugg10:year_fac
                   + sea:year_fac
                   + river:year_fac
                   + ecozones:year_fac
                   + elevation_m:year_fac
                   + hub_3rr:year_fac
                   | city_id + year_fac + cent + grid_id^year_fac,
                   panel.id = c('city_id', 'year'),
                   cluster = c("city_id"),
                   data = apples)
mlist$m15 <- feols(n_conf050 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university + lag_conflict050
                   + soilquality:year_fac
                   + rugg10:year_fac
                   + sea:year_fac
                   + river:year_fac
                   + ecozones:year_fac
                   + elevation_m:year_fac
                   + hub_3rr:year_fac
                   + border_distAll
                   | city_id + year_fac + cent + grid_id^year_fac,
                   panel.id = c('city_id', 'year'),
                   cluster = c("city_id"),
                   data = apples)

# 100 km
mlist$m16 <- feols(n_conf100 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university 
                   | city_id + year_fac + cent,
                   panel.id = c('city_id', 'year'),
                   cluster = c("city_id"),
                   data = apples)
mlist$m17 <- feols(n_conf100 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university 
                   | city_id + year_fac + cent + grid_id^year_fac,
                   panel.id = c('city_id', 'year'),
                   cluster = c("city_id"),
                   data = apples)
mlist$m18 <- feols(n_conf100 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university + lag_conflict100 
                   | city_id + year_fac + cent + grid_id^year_fac,
                   panel.id = c('city_id', 'year'),
                   cluster = c("city_id"),
                   data = apples)
mlist$m19 <- feols(n_conf100 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university + lag_conflict100
                   + soilquality:year_fac
                   + rugg10:year_fac
                   + sea:year_fac
                   + river:year_fac
                   + ecozones:year_fac
                   + elevation_m:year_fac
                   + hub_3rr:year_fac
                   | city_id + year_fac + cent + grid_id^year_fac,
                   panel.id = c('city_id', 'year'),
                   cluster = c("city_id"),
                   data = apples)
mlist$m20 <- feols(n_conf100 ~ lnpopexp + capital + commune + total_pop_country + bishop + archbishop + university + lag_conflict100
                   + soilquality:year_fac
                   + rugg10:year_fac
                   + sea:year_fac
                   + river:year_fac
                   + ecozones:year_fac
                   + elevation_m:year_fac
                   + hub_3rr:year_fac
                   + border_distAll
                   | city_id + year_fac + cent + grid_id^year_fac,
                   panel.id = c('city_id', 'year'),
                   cluster = c("city_id"),
                   data = apples)

fit_list <- list()
values <- seq(1, 20, 1)

for (i in seq_along(values)) {
  
  model <- mlist[[values[i]]]
  
  model_no <- values[i]
  
  ct <- coeftable(model) %>% 
    tibble::rownames_to_column(., "predictor") %>% 
    filter(grepl(("lnpopexp"), predictor)) %>% 
    dplyr::rename(estimate = Estimate,
                  std_error = 'Std. Error') %>%
    dplyr::select(c(predictor, estimate, std_error)) %>% 
    dplyr::mutate(conf_low = estimate - (1.96 * std_error),
                  conf_high = estimate + (1.96 * std_error)) %>% 
    dplyr::mutate(m_no = model_no)
  
  fit_list[[i]] <- ct
  
}

fit_data <- bind_rows(fit_list) %>% 
  mutate(dist = case_when(m_no %in% c(1, 2, 3, 4, 5) ~ "5km",
                          m_no %in% c(6, 7, 8, 9, 10) ~ "25km",
                          m_no %in% c(11, 12, 13, 14, 15) ~ "50km",
                          m_no %in% c(16, 17, 18, 19, 20) ~ "100km")) %>% 
  mutate(type = case_when(m_no %in% c(1, 6, 11, 16) ~ "Model (4)",
                          m_no %in% c(2, 7, 12, 17) ~ "Model (5)",
                          m_no %in% c(3, 8, 13, 18) ~ "Model (6)",
                          m_no %in% c(4, 9, 14, 19) ~ "Model (7)",
                          m_no %in% c(5, 10, 15, 20) ~ "Model (8)")) %>% 
  mutate(distance = factor(dist,
                           levels = c("5km", "25km", "50km", "100km")),
         type = factor(type,
                       levels = c("Model (4)", "Model (5)", "Model (6)", "Model (7)", "Model (8)")))

ggplot(fit_data, aes(x = type, y = estimate, ymin = conf_low, ymax = conf_high)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(col = type),
                  position = position_dodge(width = .5)) +
  scale_colour_manual(values = get_pal("Kotare")) +
  scale_y_continuous(breaks = seq(-0.45, 0.20, 0.10),
                     labels = scaleFUN) +
  theme_list$theme_apples +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom") +
  facet_wrap(~ distance)

ggsave(plot = last_plot(),
       file = paste0(path_graph,
                     "/DistCoefsCount.png"))
knitr::plot_crop(paste0(path_graph,
                        "/DistCoefsCount.png"),
                 quiet = T)


# ----- Border analysis
apples <- apples %>% 
  mutate(bordercity = ifelse(border_distAll < 100*10^3,
                             1,
                             0)) %>% 
  mutate(bordercity = factor(bordercity))

hist(apples$border_distAll)
tabyl(apples$bordercity)
max(apples$border_distAll)

mlist <- list()
mlist$m1 <- feols(conflict005 ~ lnpopexp:bordercity + bordercity | city_id,
                  data = apples)
mlist$m2 <- feols(conflict005 ~ lnpopexp:bordercity + bordercity | city_id + year_fac,
                  data = apples)
mlist$m3 <- feols(conflict005 ~ lnpopexp:bordercity + bordercity + capital + commune + total_pop_country + bishop + archbishop + university | city_id + year_fac,
                  data = apples)
mlist$m4 <- feols(conflict005 ~ lnpopexp:bordercity + bordercity + capital + commune + total_pop_country + bishop + archbishop + university | city_id + year_fac + cent,
                  data = apples)
mlist$m5 <- feols(conflict005 ~ lnpopexp:bordercity + bordercity + capital + commune + total_pop_country + bishop + archbishop + university | city_id + year_fac + cent + SOVEREIGNT^year_fac,
                  data = apples)
mlist$m6 <- feols(conflict005 ~ lnpopexp:bordercity + bordercity + capital + commune + total_pop_country + bishop + archbishop + university | city_id + year_fac + cent + grid_id^year_fac,
                  data = apples)
mlist$m7 <- feols(conflict005 ~ lnpopexp:bordercity + bordercity + capital + commune + total_pop_country + bishop + archbishop + university + lag_conflict005 | city_id + year_fac + cent + grid_id^year_fac,
                  data = apples)

etable(mlist, keep = c("lnpopexp"))




