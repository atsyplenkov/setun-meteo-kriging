# libraries ---------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(automap)
library(mapview)
library(terra)
library(sf)
library(sp)
library(qs)
# library(atslib)

source("R/custom_functions.R")

# load data ---------------------------------------------------------------
# Preprocessed semi-daily PiK dataset with 
# terraclimate values and coords
meteo_df <- 
  qread("workflow/data/meteo_dataframe_terraclimate.qs")

# Regular grid with TerraClimate values in every
# node
load("data/tidy/grid_tc.Rdata")

# basin centroid
centr <- 
  st_read("data/spatial/vector/Сетунь_водосбор_гидрологи.shp") %>% 
  st_transform(32637) %>% 
  dplyr::select(geometry) %>% 
  st_centroid() %>% 
  vect()

# Centroid coordinates
centr_coord <- 
  centr %>% 
  st_as_sf() %>% 
  st_transform(4326) %>% 
  st_coordinates()

# Temperature kriging -----------------------------------------------------
# ca. 160sec
temp_kr <-
  meteo_df %>% 
  dplyr::filter(
    as_date(datetime_tz) < as_date("2024-01-31")
  ) %>% 
  rename(date = datetime_tz) %>% 
  group_by(date) %>% 
  group_split() %>% 
  map_dfr(~krige_temperature(.x, ta))

# Precipitation kriging ---------------------------------------------------
# ca. 160s
prec_kr <-
  meteo_df %>% 
  dplyr::filter(
    as_date(datetime_tz) < as_date("2024-01-31")
  ) %>% 
  rename(date = datetime_tz) %>% 
  group_by(date) %>% 
  group_split() %>% 
  map_dfr(~krige_prcp(.x, p))

# explore
meteo_df %>% 
  rename(date = datetime_tz) %>% 
  filter(wmo %in% c("27515", "27524")) %>% 
  bind_rows(prec_kr %>% 
              transmute(date, wmo = "krig", p = var)) %>% 
  filter(month(date) == 10) |> 
  ggplot(aes(x = date, y = p,
             fill = factor(wmo))) +
  geom_col(position = position_dodge2(width = 0.5))

# merge together ----------------------------------------------------------
krig_df <- 
  prec_kr %>% 
  rename(p = var) %>% 
  left_join(
    temp_kr %>% 
      rename(ta = var),
    by = "date"
  ) %>% 
  transmute(wmo = "kriging",
            datetime_tz = date,
            p, ta,
            lon = centr_coord[1],
            lat = centr_coord[2]) %>% 
  # remove negative values
  mutate(p = if_else(p < 0, 0, p))

meteo_df_kr <- 
  meteo_df %>% 
  bind_rows(
    krig_df %>% 
      mutate(
        wmo = "kriging"
      )
  ) %>% 
  dplyr::select(wmo:lat) 

# PET calculations --------------------------------------------------------
meteo_df_kr_pet <- 
  meteo_df_kr %>% 
  rowid_to_column() %>%
  group_by(rowid) %>% 
  nest() %>% 
  mutate(
    pet = map_dbl(data, ~airGR::PE_Oudin(
      JD = yday(.x$datetime_tz),
      Temp = .x$ta,
      Lat = .x$lat,
      LatUnit = "deg",
      RunFortran = T
    ))
  ) %>% 
  unnest(cols = c(data)) %>% 
  ungroup()

# save --------------------------------------------------------------------
library(qs)
library(writexl)

qsave(
  meteo_df_kr_pet,
  "workflow/data/meteo_dataframe_kriging-pet_may23-jan24.qs"
)

meteo_df_kr_pet %>% 
  dplyr::select(wmo:ta, pet) %>%
  arrange(wmo) %>% 
  group_by(wmo) %>% 
  arrange(datetime_tz, .by_group = T) %>% 
  mutate(across(where(is.POSIXct), ~as.character(.x))) %>% 
  group_split() %>% 
  set_names(sort(unique(meteo_df_kr_pet$wmo))) %>% 
  writexl::write_xlsx("workflow/data/meteo_dataframe_kriging-pet_may23-jan24.xlsx")
