# Parse pogoda i klimat files ---------------------------------------------
read_pik <- 
  function(path){
    
    station_name <-
      str_extract(path, "(?<=pik/)(.*)(?=\\.xlsx)")
    
    df <- 
      read_excel(path,
                 col_types = c("date", rep("numeric", 8)),
                 na = "Н/Д",
                 .name_repair = "minimal") %>% 
      magrittr::set_colnames(
        c("date", "temp", "temp_min", "temp_max",
          "wind", "wind2", "prcp", "pres", "snow")
      ) %>% 
      mutate(date = as_date(date)) %>% 
      mutate(name = station_name,
             .before = date)
    
    df
  }


# Dataframe to SpatialDataframe -------------------------------------------
df_sf_spdf <- 
  function(.data, ...){
    
    sel_data <- 
      .data %>% 
      dplyr::select(lon, lat, ..., any_of(c("prcp_tc", "srad_tc"))) %>% 
      rename(var = 3)
    
    sel_data %>% 
      st_as_sf(coords = c("lon", "lat"),
               crs = 4326) %>% 
      st_transform(32637) %>% 
      as_Spatial()
    
  }

# SpatialDataframe to Thiessen Polygons -----------------------------------
spdf_to_th <- 
  function(.data){
    
    th  <-  as(dirichlet(as.ppp(.data)), "SpatialPolygons")
    proj4string(th) <- proj4string(.data)
    th.z     <- over(th, .data, fn=mean)
    th.spdf  <-  SpatialPolygonsDataFrame(th, th.z)
    
    return(th.spdf)
    
  }

# Krigging temperature ----------------------------------------------------
krige_temperature <- 
  function(.data,
           ...,
           .point = centr,
           .grid = grid_sp,
           verbose = T){
    
    day <- 
      .data$date %>% 
      unique()
    
    # Convert dataframe to SPDF
    .data_sp <- 
      .data %>% 
      df_sf_spdf(...)
    
    # Kriging
    suppressWarnings(
      invisible(
        capture.output(
          cok_auto <- 
            autoKrige(var ~ srad_tc,
                      input_data = .data_sp,
                      new_data = .grid,
                      model = "Sph")
        )))
    
    # Extract variable
    var <- 
      cok_auto$krige_output %>% 
      as.data.frame() %>% 
      dplyr::select(1:3) %>% 
      terra::rast(type = "xyz",
                  crs = "EPSG:32637") %>% 
      terra::extract(.point) %>% 
      pull(2)
    
    if (verbose) {
      cat(paste0(as.character(day), " "))
    }
    
    tibble(date = day, var = var)
    
  }


# Kriging precipitation ---------------------------------------------------
krige_prcp <- 
  function(.data,
           ...,
           .point = centr,
           .grid = grid_sp,
           verbose = T){
    
    day <- 
      .data$date %>% 
      unique()
    
    # Convert dataframe to SPDF
    .data_sp <- 
      .data %>% 
      df_sf_spdf(...)
    
    values <- 
      .data %>% 
      dplyr::select(...) %>% 
      pull()
    
    if (length(unique(values)) == 1) {
      
      tibble(date = day, var = unique(values))
      
    } else {
      
      # Kriging
      suppressWarnings(
        invisible(
          capture.output(
            cok_auto <- 
              autoKrige(var ~ prcp_tc,
                        input_data = .data_sp,
                        new_data = .grid,
                        model = "Sph")
          )))
      
      # Extract variable
      var <- 
        cok_auto$krige_output %>% 
        as.data.frame() %>% 
        dplyr::select(1:3) %>% 
        terra::rast(type = "xyz",
                    crs = "EPSG:32637") %>% 
        terra::extract(.point) %>% 
        pull(2)
      
      if (verbose) {
        cat(paste0(as.character(day), " "))
      }
      
      tibble(date = day, var = var)
      
    }
  }

# PET Oudin ---------------------------------------------------------------
pet_oudin <- 
  function(.data, temperature, date, latitude){
    
    # Some tidyeval
    tt <- dplyr::enquo(temperature)
    dd <- dplyr::enquo(date)
    
    df <- 
      .data %>%
      dplyr::select(date = !!dd,
                    tt = !!tt) %>% 
      mutate(yday = lubridate::yday(date))
    
    airGR::PE_Oudin(JD = df$yday,
                    Temp = df$tt,
                    Lat = latitude,
                    LatUnit = "deg")
    
  }

# Split dataframe into list -----------------------------------------------
split_df <- 
  function(.data, n = 100){
    
    nr <- nrow(.data)
    split(.data, gl(ceiling(nr / n), n, nr))
    
  }

# Whittaker smooth filter -------------------------------------------------
whittaker_smooth <- 
  function(.data, .var, ...){
    
    smoothed_df <- 
      .data %>% 
      dplyr::mutate(
        smoothed = pracma::whittaker({{.var}}, ...)
      )
    
    return(smoothed_df)
    
  }

# Savitsky-Golay smoother -------------------------------------------------
library(phenofit)

sg_smoother <- 
  function(
    .data,
    .var,
    .weight,
    .factor = 30,
    .iter = 3
  ){
    
    wdf <- 
      .data %>% 
      dplyr::select(y = {{.var}},
                    w = {{.weight}})
    
    n_data <- 
      sum(!is.na(wdf$y))
    
    ylu <- quantile(wdf$y, c(0.001, 1), na.rm = T)
    
    wSG <- 
      phenofit::smooth_wSG(
        y = wdf$y,
        w = wdf$w,
        ylu = ylu, 
        nptperyear = .factor,
        iters = .iter
      )
    
    smoothed <- 
      purrr::pluck(wSG$zs, .iter)
    
    .data %>% 
      dplyr::mutate(smoothed = smoothed)
    
  }

# Weights assign ----------------------------------------------------------
assign_weights <- 
  function(
    .data,
    .var = Q,
    .date = datetime,
    .w_def = 1,
    .tr_up,
    .tr_down
  ){
    
    wdf <-
      .data %>% 
      dplyr::select(y = {{.var}},
                    date = {{.date}}) %>% 
      mutate(.w = .w_def)
    
    wdf %>% 
      mutate(
        change = Q - lag(Q), # dH per 10 min
        change_pp = Q - lead(Q), # dH per 10 min
        .w = ifelse(abs(change) >= .tr_up, 0.2, .w), # Threshold value!
        .w = ifelse(abs(change_pp) >= .tr_down, 0.2, .w)
      ) 
    
  }