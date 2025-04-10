#' **FUNCTION: detect_track_problems**
#' protocol for detecting following track problems:
#' 1 - imprecise sigfox from Aiguamolls - mark sigfox E4Warning data before 
#'     putting antenna in the park
#' 2 - deployment on and off time
#' 3 - duplicated timestamps (rounded to 1 min)
#' 4 - gps precision check
#' 5 - outlier check
#' 6 - check if there is any sensor type that is not wanted
#' 7 - empty geometry (no coordinates)
#' 8 - weird date - year before 1950 or in the future
#' 9 - weird coordinates - lon > 180 or lat > 90
#' output is a list with track problems and track info, that will be used to 
#' cross check with the deployment info
#' the cleaned track is saved if there are at least 3 points

# FUNCTION: detect_track_problems -----------------------------------------

detect_track_problems <- function(
    track,  
    deploy_on_time = NA, 
    deploy_off_time = NA, 
    sensors_of_interest = c(2299894820, 653)
){
  
  cols_out <- colnames(track)
  
  # DETECT TRACK PROBLEMS ---------------------------------------------------
  
  # 2 - imprecise sigfox from Aiguamolls -----------------------------------------
  
  # excluding points taken in aiguamolls before the antenna was put in the park
  sensor_ids <- unique(track$sensor_type_id)
  study_id <- unique(track$study_id)
  
  if(study_id == 4043292285 & 2299894820 %in% sensor_ids){
    
    # the date when the antenna was placed in the park
    antenna_date = as_datetime("13-11-2024", format = "%d-%m-%Y")
    
    track <- track |> 
      mutate(
        study_site_comment = case_when(
          timestamp > antenna_date & sensor_type_id == 2299894820 ~ "post-antenna",
          timestamp <= antenna_date & sensor_type_id == 2299894820 ~ "pre-antenna",
          .default = NA
        )
      )
    
    rm(antenna_date)
    
  } else {
    
    track <- track |> 
      mutate(study_site_comment = NA)
    
  }
  
  
  # 3 - deployment on and off time ----------------------------------------------
  
  if(is.na(deploy_on_time) | is.na(deploy_off_time)){
    
    track <- track |> 
      mutate(
        dep_on_time = as_datetime(deploy_on_time),
        dep_off_time = as_datetime(deploy_off_time),
        deploy_times = case_when(
          !is.na(dep_on_time) & timestamp < dep_on_time ~ "out",
          !is.na(dep_off_time) & timestamp > dep_off_time ~ "out", 
          .default = NA
        )
      ) |> 
      select(-dep_on_time, dep_off_time)
    
  } else {
    
    track <- track |> 
      mutate(deploy_times = NA)
    
  }
  
  
  # 4 - duplicated timestamps ----------------------------------------------------
  
  # rounding time-stamp to minute to take only one position per minute
  # ordering by gps_hdop, and or by the number of NAs in rows,
  # to minimize gps error and the amount of data we process 
  
  track <- track |> 
    mutate(
      n_na = rowSums(is.na(pick(everything()))),
      timestamp_rounded = floor_date(timestamp, unit = "min")
    ) |>
    arrange(
      across(
        any_of(
          c("timestamp_rounded", "gps_hdop", "gps_vdop", "n_na", "timestamp")
        )
      )
    ) |>
    mutate(
      # mark duplicated time-stamps
      duplicated_timestamp = duplicated(timestamp_rounded), 
      # just in case there is multiple sensor data in the data-set, 
      # should not be the case
      .by = sensor_type_id
    ) |>
    select(-n_na, -timestamp_rounded)
  
  
  # 5 - gps precision check ------------------------------------------------------
  
  # remove imprecise locations, at the intro to movement ecology course, 
  # they said if the hdop or vdop is bigger than 5
  # that point shouldn't be trusted
  
  if(sum(c("gps_hdop", "gps_vdop") %in% colnames(track)) > 0){
    
    track <- track |> 
      mutate(row_id = str_c("row", 1:n())) 
    
    track_check <- track |> 
      st_drop_geometry() |> 
      as_tibble() |> 
      # select only outlier columns
      select(any_of(c("gps_hdop", "gps_vdop")), row_id) |>
      pivot_longer(
        cols = contains("dop"), 
        names_to = "dop_type", 
        values_to = "dop_value"
      ) |>
      filter(!is.na(dop_value)) |>  
      # group_by row_id and check if there is any value with 
      # positive outlier
      summarize(dop_check = sum(as.numeric(dop_value) > 5), .by = row_id) |> 
      # mark outliers
      mutate(gps_precision = if_else(dop_check > 0, "bad", "ok")) |> 
      select(gps_precision, row_id)
    
    track <- track |> 
      left_join(track_check, by = "row_id") |>
      select(-row_id)
    
  } else{
    
    track <- track |> 
      mutate(gps_precision = NA)
    
  }
  
  
  # 6 - outlier check -------------------------------------------------------
  
  
  if(sum(str_detect("outlier", colnames(track))) > 0){
    
    track <- track |> 
      mutate(row_id = str_c("row", 1:n()))
    
    out_track <- track |> 
      st_drop_geometry() |>
      as_tibble() |>
      # select only outlier columns
      select(contains("outlier"), row_id) |>
      pivot_longer(
        cols = contains("outlier"), 
        names_to = "outlier_type", 
        values_to = "outlier_value"
      ) |>
      filter(!is.na(outlier_value)) |> 
      # group_by row_id and check if there is any value with outlier marked as T
      summarize(outlier_sum = sum(outlier_value), .by = row_id) |> 
      # mark outliers
      mutate(outlier = ifelse(outlier_sum > 0, "outlier", NA)) |> 
      select(outlier, row_id)
    
    track <- track |> 
      left_join(out_track, by = "row_id") |>
      select(-row_id)
    
  } else { 
    
    track <- track |> 
      mutate(outlier = NA)
    
  } 
  
  
  # SUMMARIZE TRACK PROBLEMS ------------------------------------------------
  
  track <- track |>
    mutate(
      year = year(timestamp),
      lon = st_coordinates(geometry)[, 1],
      lat = st_coordinates(geometry)[, 2], 
      sensor_type_check = sensor_type_id %in% sensors_of_interest 
    ) |> 
    mutate(
      track_problem = case_when(
        # 2 - sigfox E4Warning data before putting antenna in the park
        study_site_comment == "pre-antenna" ~ "E4Warning sigfox pre-antenna",
        # 3 - check that deployment on and off time match with the track
        deploy_times == "out" ~ "out of deployment on-off times",
        # 4 - duplicated time-stamps - resampled to 1 minute
        duplicated_timestamp == T ~ "duplicated timestamp (rounded to min)",
        # 5 - eliminate imprecise data
        gps_precision == "bad" ~ "hdop or vdop > 5",
        # 6 - remove outliers if they are marked already
        outlier == "outlier" ~ "outlier",
        # 7 - check if there is any sensor type that is not wanted
        sensor_type_check == F ~ "sensor type not of interest",
        # 8 - empty geometry (no coordinates)
        st_is_empty(geometry) ~ "empty geometry",
        # 9 - weird date - year before 1950 or in the future
        year < 1950 | date(timestamp) > Sys.Date() ~ "weird date",
        # 10 - weird coordinates - lon > 180 or lat > 90
        # the projection is EPSG:4326 (checked) - with test target species
        abs(lon) > 180 | abs(lat) > 90 ~ "weird coordinates",
        .default = NA
      ) 
    ) |> 
    select(all_of(cols_out), track_problem)
  
  cat("\n------------------track problems labeled!-------------------")

  return(track)
  
}
