# libraries ---------------------------------------------------------------
library(tidyverse)
library(rp5pik)
library(sf)
library(mapview)
library(qs)

# load data ---------------------------------------------------------------
# Load meteostation locations for which we 
# are going to download raw meteorological data
locations <-
  data.table::fread("data/tidy/meteo_location.csv") %>%
  as_tibble() %>%
  dplyr::select(name = name_rus,
                id,
                lat = latitude,
                lon = longitude) %>%
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326)

# Setun River catchment
ws <-
  st_read("data/spatial/vector/Сетунь_водосбор_гидрологи.shp",
          options = "ENCODING=UTF-8") %>%
  dplyr::select(geometry) %>%
  st_buffer(0)

# Plot meteostations and Setun's watershed to 
# eyeball the location
mapview(ws, alpha.regions = 0) +
  mapview(locations, zcol = "name")

# WMO id ------------------------------------------------------------------
# Create a vector of WMO id's to use it in 
# meteo data downloading
wmos <-
  locations %>%
  filter(id != "MSU") %>%
  pull(id)

# download data -----------------------------------------------------------
# Download raw data from pogodaiklimat.ru. It may 
# contain gaps, errors and etc. 6 months will take 
# approx. 3 min
rp5_all <-
  rp5pik::rp_parse_pik(wmo_id = wmos,
                       start_date = "2023-05-01",
                       end_date = "2024-01-01")

# Backup data
# qsave(rp5_all, "data/temp/rp5_all_may23-jan24.qs")

# Remove 'bad' values
rp5_na <-
  rp5_all %>%
  dplyr::mutate(prec = dplyr::na_if(prec, 496)) %>%
  dplyr::mutate(prec = dplyr::na_if(prec, 263)) %>%
  dplyr::mutate(prec = dplyr::na_if(prec, 123))

# Aggregate data to semi-daily format, i.e. for 9AM 
# and 9PM measurements
rp5_12h <-
  rp5_na %>%
  rp_aggregate_pik(.period = "12h")

# save --------------------------------------------------------------------
# Save as a dataframe
qsave(
  rp5_12h,
  "workflow/data/rp5-12h_all_may23-jan24.qs"
)

library(writexl)

# Save as Excel file where every meteostation is 
# on separate list
rp5_12h %>%
  arrange(wmo) %>%
  group_by(wmo) %>%
  arrange(datetime_tz, .by_group = T) %>%
  mutate(across(where(is.POSIXct), ~as.character(.x))) %>%
  group_split() %>%
  set_names(sort(wmos)) %>%
  write_xlsx("workflow/data/rp5-12h_all_may23-jan24.xlsx")