# libraries ---------------------------------------------------------------
library(tidyverse)
library(rp5pik)
library(sf)
library(mapview)
library(qs)

source("R/custom_functions.R")

# load data ---------------------------------------------------------------
# Load previously downloaded and preprocessed 
# semi-daily meteo data
rp5_12h <-
  qread(
    "workflow/data/rp5-12h_all_may23-jan24.qs"
  )

# Load station locations
locations <-
  data.table::fread("data/tidy/meteo_location.csv") %>%
  as_tibble() %>%
  dplyr::select(name = name_rus,
                id,
                lat = latitude,
                lon = longitude) %>%
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326)

# Extract X, Y coords of station locations
meteo_coord_utm <-
  locations %>%
  # st_transform(32637) %>%
  st_coordinates() %>%
  bind_cols(wmo = locations$id)

# Load watershed border
ws <-
  st_read("data/spatial/vector/Сетунь_водосбор_гидрологи.shp",
          options = "ENCODING=UTF-8") %>%
  dplyr::select(geometry) %>%
  st_buffer(0)

# Detect watershed centroid
ws_cent <-
  ws %>%
  st_centroid()

# Plot everything
mapview(ws, alpha.regions = 0) +
  mapview(locations, zcol = "name") +
  mapview(ws_cent, cex = 2)

# 2. Grid creation --------------------------------------------------------
# Replace NA with zero
# Add coordinates
meteo_fix <-
  rp5_12h %>%
  dplyr::select(wmo, datetime_tz, p, ta) %>%
  mutate(across(c(p:ta), ~replace(.x, is.na(.x), 0))) %>%
  left_join(meteo_coord_utm,
            by = "wmo") %>%
  rename(lon = X, lat = Y)

# get terraclimate vars for stations --------------------------------------
# To enhance the kriging interpolation we are going to 
# use TerraClimate reanalysis data to help capture 
# the typical spatial pattern. Here we load the mean 
# annual P and T values for every meteostation in our 
# dataset
tc_stations <-
  qread("data/tidy/meteo_dataframe_terraclimate_25may.qs") %>%
  distinct(wmo, .keep_all = T) %>%
  dplyr::select(wmo, contains("tc"))

# join tables -------------------------------------------------------------
meteo_df_tc <-
  meteo_fix %>%
  left_join(tc_stations,
            by = "wmo")

# SAVE --------------------------------------------------------------------
qsave(meteo_df_tc,
      "workflow/data/meteo_dataframe_terraclimate.qs")