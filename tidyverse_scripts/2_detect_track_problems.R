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
    sensors_of_interest = c(2299894820, 653), 
    dop_threshold = 5
){
  
  cols_out <- colnames(track)
  
  track <- track |> 
    mutate(row_id = 1:n())
  
  # 2 - imprecise sigfox from Aiguamolls ---------------------------------------
  
  # excluding points taken in aiguamolls before the antenna was put in the park
  sensor_ids <- unique(track$sensor_type_id)
  study_id <- unique(track$study_id)
  
  if(study_id == 4043292285 & 2299894820 %in% sensor_ids){
    
    # the date when the antenna was placed in the park
    antenna_date = as_datetime("13-11-2024", format = "%d-%m-%Y")
    
    track <- track |> 
      mutate(
        study_site_comment = case_when(
          t_ > antenna_date & sensor_type_id == 2299894820 ~ "post-antenna",
          t_ <= antenna_date & sensor_type_id == 2299894820 ~ "pre-antenna",
          .default = NA
        )
      )
    
    rm(antenna_date)
    
  } else {
    
    track <- track |> mutate(study_site_comment = NA)
    
  }
  
  
  # 3 - deployment on and off time ----------------------------------------------
  
  if(is.na(deploy_on_time) | is.na(deploy_off_time)){
    
    track <- track |> 
      mutate(
        dep_on_time = as_datetime(deploy_on_time),
        dep_off_time = as_datetime(deploy_off_time),
        deploy_times = case_when(
          !is.na(dep_on_time) & t_ < dep_on_time ~ "out",
          !is.na(dep_off_time) & t_ > dep_off_time ~ "out", 
          .default = NA
        )
      ) |> 
      select(-dep_on_time, dep_off_time)
    
  } else {
    
    track <- track |> mutate(deploy_times = NA)
    
  }
  
  # 4 - gps precision check ------------------------------------------------------
  
  # remove imprecise locations, at the intro to movement ecology course, 
  # they said if the hdop or vdop is bigger than 5
  # that point shouldn't be trusted
  
  if(sum(c("gps_hdop", "gps_vdop") %in% colnames(track)) > 0){
    
    track_check <- track |> 
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
      summarize(
        dop_check = sum(as.numeric(dop_value) > dop_threshold), 
        .by = row_id
      ) |> 
      # mark outliers
      mutate(gps_precision = if_else(dop_check > 0, "bad", "ok")) |> 
      select(gps_precision, row_id)
    
    track <- track |> 
      left_join(track_check, by = "row_id") 
    
  } else{
    
    track <- track |> 
      mutate(gps_precision = NA)
    
  }
  
  
  
  # 5 - duplicated timestamps ----------------------------------------------------

  # rounding time-stamp to minute to take only one position per minute
  # ordering by gps_hdop, and or by the number of NAs in rows,
  # to minimize gps error and the amount of data we process 
  
  duplicated_times <- track |> 
    filter(gps_precision == "ok" | is.na(gps_precision)) |> 
    select(
      t_, sensor_type_id, row_id, 
      any_of(c("gps_hdop", "gps_vdop", "gps_stalite_count")), 
    ) |> 
    mutate(
      n_na = rowSums(is.na(pick(everything()))),
      t_rounded = floor_date(t_, unit = "min")
    ) |> 
    arrange(
      across(any_of(c("t_rounded", "gps_hdop", "gps_vdop"))),
      across(any_of(c("gps_stalite_count")), desc),
      n_na, 
      desc(t_)
    ) |> 
    mutate(
      # mark duplicated time-stamps
      duplicated_t = duplicated(t_rounded), 
      # just in case there is multiple sensor data in the data-set, 
      # should not be the case
      .by = sensor_type_id
    ) |>
    select(row_id, duplicated_t)

  track <- track |> 
    left_join(duplicated_times, by = "row_id")
  
  # SUMMARIZE TRACK PROBLEMS ------------------------------------------------
  
  track <- track |>
    mutate(year = year(t_)) |> 
    mutate(
      track_problem = case_when(
        # 2 - sigfox E4Warning data before putting antenna in the park
        study_site_comment == "pre-antenna" ~ "E4Warning sigfox pre-antenna",
        # 3 - check that deployment on and off time match with the track
        deploy_times == "out" ~ "out of deployment on-off times",
        # 4 - eliminate imprecise data
        gps_precision == "bad" ~ "hdop or vdop greater than 5",
        # 4 - duplicated time-stamps - resampled to 1 minute
        duplicated_t == T ~ "duplicated timestamp (rounded to min)",
        # 6 - remove outliers if they are marked already
        outlier == "outlier" ~ "outlier",
        # 8 - empty geometry (no coordinates)
        is.na(x_) | is.na(y_) ~ "missing coordinates",
        # 9 - weird date - year before 1950 or in the future
        year < 1950 | date(t_) > Sys.Date() ~ "weird date",
        # 10 - weird coordinates - lon > 180 or lat > 90
        # the projection is EPSG:4326 (checked) - with test target species
        abs(x_) > 180 | abs(y_) > 90 ~ "weird coordinates",
        .default = NA
      ) 
    ) |> 
    select(all_of(cols_out), track_problem)
  
  cat("\n------------------track problems labeled!-------------------")

  return(track)
  
}


# 6 - outlier check -------------------------------------------------------


# if(sum(str_detect("outlier", colnames(track))) > 0){
#   
#   out_track <- track |> 
#     # select only outlier columns
#     select(contains("outlier"), row_id) |>
#     pivot_longer(
#       cols = contains("outlier"), 
#       names_to = "outlier_type", 
#       values_to = "outlier_value"
#     ) |>
#     filter(!is.na(outlier_value)) |> 
#     # group_by row_id and check if there is any value with outlier marked as T
#     summarize(outlier_sum = sum(outlier_value), .by = row_id) |> 
#     # mark outliers
#     mutate(outlier = ifelse(outlier_sum > 0, "outlier", NA)) |> 
#     select(outlier, row_id)
#   
#   track <- track |> 
#     left_join(out_track, by = "row_id") |>
#     select(-row_id)
#   
# } else { 
#   
#   track <- track |> 
#     mutate(outlier = NA)
#   
# } 
# 
#         # 7 - check if there is any sensor type that is not wanted
# sensor_type_check == F ~ "sensor type not of interest",

