
library(DescTools)
library(data.table)
library(amt)

#' **FUNCTION: detect_track_problems**
#' protocol for detecting following track problems:
#' 1 - imprecise sigfox from Aiguamolls - mark sigfox E4Warning data before 
#'     putting antenna in the park
#' 2 - deployment on and off time
#' 3 - gps precision check
#' 4 - duplicated timestamps (rounded to 1 min)
#' 5 - empty geometry (no coordinates)
#' 6 - weird coordinates - lon > 180 or lat > 90
#' 7 - weird date - year before 1950 or in the future
#' output is a list with track problems and track info, that will be used to 
#' cross check with the deployment info
#' the cleaned track is saved if there are at least 3 points
#' 
#' **FUNCTION: summarize_track_problems**
#' summarize all track problems and their duration, if indicated, save the 
#' cleaned track as well. 
#' 
#' **FUNCTION: find duplicated_tracks**
#' find which track from the duplicates to exclude based on the track duration. 
#' if the two tracks overlap in time, exclude the one that has shorter duration.

# FUNCTION: detect_track_problems -----------------------------------------

detect_track_problems <- function(
    track,  
    study_id = NA,
    deploy_on_time = NA, 
    deploy_off_time = NA, 
    dop_threshold = 5, 
    cols_out = NULL
){
  
  track_cols <- names(track)
  
  # 1 - imprecise sigfox from Aiguamolls -----------------------------------------
  
  antenna_date <- lubridate::as_datetime("13-11-2024", format = "%d-%m-%Y")
  
  # Using data.table syntax to add the 'study_site_comment' column
  track[
    sensor_type_id == bit64::as.integer64(2299894820), 
    pre_antenna := if(study_id == 4043292285) timestamp <= antenna_date else NA
  ] 
  
  # 2 - deployment on and off time ----------------------------------------------
  
  track[
    , out_deploy_time := (timestamp < deploy_on_time | timestamp > deploy_off_time)
  ]
  
  # 3 - gps precision check ------------------------------------------------------
  
  # remove imprecise locations, at the intro to movement ecology course, 
  # they said if the hdop or vdop is bigger than 5
  # that point shouldn't be trusted
  
  # Check both columns if they exist
  track[, bad_gps_precision := if (all(c("gps_hdop", "gps_vdop") %in% track_cols)) {
    (!is.na(gps_hdop) & gps_hdop > dop_threshold) | (!is.na(gps_vdop) & gps_vdop > dop_threshold)
  } else if ("gps_hdop" %in% track_cols) {
    !is.na(gps_hdop) & gps_hdop > dop_threshold
  } else if ("gps_vdop" %in% colnames(track)) {
    !is.na(gps_vdop) & gps_vdop > dop_threshold
  } else {
    NA
  }]

# 4 - duplicated timestamps ---------------------------------------------------

  track[, row_id := .I]
  # List of columns for ordering
  prec_cols <- c("gps_hdop", "gps_vdop", "gps_stalite_count")
  # define the sorting order: ascending (1) or descending (-1)
  prec_order <- c(1, 1, -1)
  
  # check which columns are in the data.table
  cols_order <- intersect(prec_cols, track_cols)
  cols_sort <- prec_order[prec_cols %in% cols_order]
  
  # full column list
  cols_select <- c("t_rounded", cols_order, "n_na", "row_id")
  cols_sort <- c(1, cols_sort, 1, -1)
  full_cols <- c(cols_select, "sensor_type_id")
  
  # round the timestamp to one minute
  duplicated_times <- track[bad_gps_precision %in% c(F, NA) & out_deploy_time %in% c(F, NA)][
    , t_rounded := lubridate::floor_date(timestamp, unit = "min")]
  # select the columns
  duplicated_times <- duplicated_times[ , ..full_cols]
  
  # construct the sorting logic, order the data.table by the selected columns
  sorting_logic <- do.call(order, lapply(seq_along(cols_select), function(i) {
    if(cols_sort[i] == -1) -duplicated_times[[cols_select[i]]] else duplicated_times[[cols_select[i]]]
  }))
  
  # label duplicated timestamps
  duplicated_times <- duplicated_times[
    sorting_logic
    ][
      , duplicated_time := duplicated(t_rounded), by = sensor_type_id
    ][
      , .(duplicated_time, row_id)
    ]
  
  # merge the duplicated labels with the original track
  track <- merge(track, duplicated_times, by = "row_id", all.x = T)[
    , year := year(timestamp)
  ]
  
  rm(duplicated_times)
  
  # columns to save in the OUTPUT track
  cols_out <- c(cols_out, "track_problem")
  
  track <- track[
    , track_problem := fcase(
      # 1 - sigfox E4Warning data before putting antenna in the park
      pre_antenna, "E4Warning SigFox pre-antenna",
      # 2 - check that deployment on and off time match with the track
      out_deploy_time, "out of deployment on-off times",
      # 3 - eliminate imprecise data
      bad_gps_precision, sprintf("hdop or vdop greater than %d", dop_threshold),
      # 4 - duplicated time-stamps - resampled to 1 minute
      duplicated_time, "duplicated timestamp (rounded to min)",
      # 5 - empty geometry (no coordinates)
      is.na(x) | is.na(y), "missing coordinates",
      # 6 - weird coordinates - lon > 180 or lat > 90
      # the projection is EPSG:4326 (checked) - with test target species
      abs(x) > 180 | abs(y) > 90, "weird coordinates",
      # 7 - weird date - year before 1950 or in the future
      year < 1950 | as.Date(timestamp) > Sys.Date(), "weird date",
      default = NA
    )
  ]
  
  if(!is.null(cols_out)){ track <- track[, ..cols_out] }
    
  return(track)
}
  

# FUNCTION: summarize track_problems --------------------------------------

summarize_track_problems <- function(
    track, time_col = "timestamp",
    save_cleaned_track = T, fout = NULL,
    cols_cleaned_track = NULL
    ){

  if(is.null(fout)){ stop("Provide a file path for the cleaned track!") }
  
  # summarize the track problems and their duration
  track_summarized <- track[, .(
    track_start = min(get(time_col), na.rm = T),
    track_end = max(get(time_col), na.rm = T),
    track_unique_days = uniqueN(as.Date(get(time_col)), na.rm = T),
    n_locs = .N
  ), by = .(track_problem, sensor_type_id)
  ][
    , track_period_days := round(
      difftime(track_end, track_start, units = "days"), 2)
  ]

    

  if(save_cleaned_track){

   track <- track[track_problem %in% c(NA, "")]
   
   # select output columns
   if(!is.null(cols_cleaned_track)){ track <- track[, ..cols_cleaned_track] }
  
   # if there is more than 3 locations, save the track
   if(nrow(track) > 3){

     track <- track |>
       make_track(
         x, y, timestamp,
         crs = st_crs(4326),
         all_cols = TRUE
       )
     
     saveRDS(track, file = fout, compress = F)

     track_summarized <- track_summarized[, saved := T][, file := fout]

   } else {
     
     # report if the track was saved or not
     track_summarized <- track_summarized[, saved := F][, file := NA]
     
   }
  
  }

  return(track_summarized)

}


# FUNCTION: find_duplicated_tracks ----------------------------------------

# find which track from the duplicates to exclude based on the temporal overlap
# and track duration or number of location per track 
# the idea for the function originates from a script provided by Anne Scharf
# input: df with duplicated tracks
# output: vector of tracks to exclude (identified by row numbers)

find_duplicated_tracks <- function(
    track_df, start, end, n_locs = NULL, duration = NULL,
    keep_criteria = "duration" # locations or duration
){
  
  track_df <- track_df[, track_id := .I]
  
  df <- copy(track_df)[, `:=`(
    track_start = get(start),
    track_end = get(end),
    n_na = rowSums(is.na(.SD))
  )]
  
  track_ids <- df[, track_id]
  
  if(nrow(df) == 1) {
    message("\nOnly one track provided, none excluded!")
    return(track_df[, to_exclude := F][, track_id := NULL])
  } 
  
  # create the table to check overlaps and 
  overlapping <- CJ(id1 = track_ids, id2 = track_ids)[id1 != id2]
  overlapping[, id_combo := paste0(pmin(id1, id2), pmax(id1, id2))] 
  # get unique combination of id1 and id2
  overlapping <- unique(overlapping, by = "id_combo") 
  overlapping[, `:=`(
    id1_start = df[match(id1, track_id), track_start],
    id1_end = df[match(id1, track_id), track_end],
    n_na1 = df[match(id1, track_id), n_na],
    id2_start = df[match(id2, track_id), track_start],
    id2_end = df[match(id2, track_id), track_end],
    n_na2 = df[match(id2, track_id), n_na]
  )]
  
  # Check for overlaps
  overlapping[
    , overlap := c(id1_start, id1_end) %overlaps% c(id2_start, id2_end), 
    by = .I]
  overlapping <- overlapping[overlap == TRUE]
  
  if (nrow(overlapping) == 0) {
    message("\nThere are no overlapping tracks!")
    return(track_df[, to_exclude := F][, track_id := NULL])
  } 
  
  if(keep_criteria == "locations") {
    
    df <- df[, n_locs := get("n_locs")]
    
    if(is.null(n_locs)){ 
      stop("Provide a column name for the number of locations!")
    }
    
    # Define which tracks to exclude
    overlapping[, `:=`(
      n_locs1 = df[match(id1, track_id), n_locs],
      n_locs2 = df[match(id2, track_id), n_locs]
    )]
    
    excluded <- overlapping[
      , selected := fcase(
        n_locs1 < n_locs2, id1,
        n_locs2 < n_locs1, id2,
        n_locs1 == n_locs2 & n_na1 > n_na2, id1,
        n_locs1 == n_locs2 & n_na2 > n_na1, id2,
        default = id2 )][, selected]
    
    excluded <- unique(excluded)
    
    message("\n Kept the track with the largest number of locations!")
    
  } 
  
  if(keep_criteria == "duration") {
    
    df <- df[, duration := get(duration)]
    
    # Define which tracks to exclude
    overlapping[, `:=`(
      duration1 = df[match(id1, track_id), duration],
      duration2 = df[match(id2, track_id), duration]
    )]
    
    excluded <- overlapping[
      , selected := fcase(
        duration1 < duration2, id1,
        duration2 < duration1, id2,
        duration1 == duration2 & n_na1 > n_na2, id1,
        duration1 == duration2 & n_na2 > n_na1, id2,
        default = id2)][, selected]
    
    excluded <- unique(excluded)
    message("\nKept the track with the longest duration!")
  }
  
  track_df <- track_df[
    , to_exclude := track_id %in% excluded][, track_id := NULL]
  
  # returns track_id which should be excluded
  return(track_df)
}
