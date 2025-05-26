#' ---
#' title: "5_0_distance_functions"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' Here are provided functions for caltulating distances, used in the script
#' get_daily_distances. 
#' **SECTION 1 - Function: add_day_cycle**
#' helper function to obtain day_cycle and day_period, since it is needed 
#' for other functions. 
#' input: track file with time stamp in column t_
#' **SECTION 2 - Function: get_dcp_df**
#' function to calculate distances for each day_cycle_period, 
#' it groups the data in day_cycle_period and calculates the distances 
#' between all locations during those times, and some basic stats
#' input: track_file with x_, y_, t_, optionally day_limits and cols_of_interest
#' resampling rate and resampling tolerance used to generate track bursts
#' **SECTION 3 - Function: get_night_day_steps**
#' function to calculate distances between the steps during the night and
#' from the respective point at the night, the furthest point during
#' the day
#' input: track_file with x_, y_, t_, optionally day_limits and cols_of_interest
#' resampling rate and resampling tolerance used to generate track bursts
#' output: list of two data frames: night_steps and day_steps

# 0 - Load packages ------------------------------------------------------


# alternative t_ > day_start & t_ < day_end, "day", "night" 
# gives slightly different results
# i defined night wihtout = sign so that we keep the "darkest"
# period always, I had doubts with the variables night and nightEnd, 
# but decided to keep this because of dawn and dusk, 
# to make sure it's before dawn and after dusk


add_day_cycle <- function(track, day_limits = NULL, add_day_limits = F){
  
  tcols <- c(colnames(track), "day_cycle", "day_period")
  
  if(add_day_limits){ tcols <- c(tcols, day_limits)}
  
  # Convert to data.table
  setDT(track)
  
  track <- track[, date := as.Date(t_)]
  
  sun_times_df <- setDT(suncalc::getSunlightTimes(
    data = data.frame(date = track$date, lat = track$y_, lon = track$x_), 
    keep = day_limits, 
    tz = "UTC"
  ))
  
  track <- cbind(track, sun_times_df[, ..day_limits])[
    !is.na(get(day_limits[1])) & !is.na(get(day_limits[2]))]
  # setorder(track, t_)
  
  
  if(nrow(track) == 0){
    
    message(
      sprintf(
        "Cannot calculated day cycle! %s not available for the coordinates provided.", 
        paste(day_limits, collapse = " or ")
      ))
    
    return(NULL)
    
  }
  
  track[, `:=`(
    day_cycle = ifelse(
      t_ < get(day_limits[1]),
      # Assign the previous day for times before day_start
      date - 1, 
      date),
    day_period = ifelse(
      t_ < get(day_limits[1]) | t_ > get(day_limits[2]), 
      "night", 
      "day"
    )
  )]
  
  # Remove unnecessary columns
  track <- track[, ..tcols]
  
  return(track)
  
}

# 1 - FUNCTION: get_dcp_df ------------------------------------------------

# Function to process each file
get_dcp_dist <- function(
    track, 
    day_limits = NULL,
    crs = sf::st_crs(4326)
){
  
  if(all(!c("day_cycle", "day_period") %in% colnames(track))){
    
    if(is.null(day_limits)){
      stop("Provide day limits for caluclating day cycle and period")
    }
    
    track <- add_day_cycle(track, day_limits = day_limits)
    
  } 
  
  if(is.null(track)){ return(data.table(day_period_distance_available = T)) }
  
  # making a copy of data so that it is not directly modified
  track_dt <- copy(track)
  
  track_dt[, n_locs := .N, by = c("day_cycle", "day_period")]
  
  calculate_distances <- function(dcp_one) {
    
    sf_points <- sf::st_as_sf(dcp_one, coords = c("x_", "y_"), crs = crs)
    dist <- sf::st_distance(sf_points)
    dist[lower.tri(dist, diag = TRUE)] <- NA
    
    return(data.table(
      dist_min = min(dist, na.rm = TRUE),
      dist_max = max(dist, na.rm = TRUE),
      dist_mean = mean(dist, na.rm = TRUE),
      dist_median = median(dist, na.rm = TRUE)
    ))
    
  }
  
  # Split into groups for each dcp
  # Group by dcp and calculate summaries
  dcp_median <- track_dt[, .(
      n_locs = first(n_locs),
      x_median = median(x_),
      y_median = median(y_),
      t_median = median(t_),
      t_min = min(t_),
      t_max = max(t_)
    ), 
    by = c("day_cycle", "day_period")][
      ,  t_span := fifelse(
        n_locs > 1, difftime(t_max, t_min, units = "hours", tz = "UTC"), NA)]
  
  dcp_distance <- track_dt[
    , if(unique(n_locs) > 1) calculate_distances(.SD),  
    by = c("day_cycle", "day_period")]
    
  dcp_summary <- merge(
    dcp_median, dcp_distance, by = c("day_cycle", "day_period"), all.x = TRUE
  )
  setorder(dcp_summary, t_min)
  
  dcp_summary[, day_period_distance_available := T]

  return(dcp_summary)
  
}


# 2 - FUNCTION: get_night_steps ------------------------------------------------

get_night_steps <- function(
    track, 
    crs = sf::st_crs(4326),
    day_limits = NULL
){
  
  # Check if day cycle needs to be added
  if(all(!c("day_cycle", "day_period") %in% colnames(track))){
    
    if(is.null(day_limits)){
      stop("Provide day limits for caluclating day cycle and period")
    }
    
    track <- add_day_cycle(track, day_limits = day_limits)
  
  } 
  
  if(is.null(track)) { return(data.table(night_steps_available = FALSE)) }
  
  # Get unique night locations with the maximum t_ for each day_cycle
  night_locs <- unique(track[
    day_period == "night",
    .SD[which.max(t_)],    
    by = day_cycle
  ])[
    , geometry := sf::st_as_sf(.SD, coords = c("x_", "y_"), crs = crs), 
    .SDcols = c("x_", "y_")]
  
  if(nrow(night_locs) < 2){
    
    return(data.table(night_steps_available = FALSE))
    
  }
  
  # Create night_steps by combining consecutive rows
  night_steps <- cbind(
    night_locs[-.N, .(
      t1_ = t_, x1_ = x_, y1_ = y_, geometry_1 = geometry, day_cycle_1 = day_cycle)], 
    night_locs[-1, .(
      t2_ = t_, x2_ = x_, y2_ = y_, geometry_2 = geometry, day_cycle_2 = day_cycle)]
  )[
    , consequtive_days := day_cycle_1 == (day_cycle_2 - 1)
    ][ consequtive_days == T ][, consequtive_days := NULL]
  
  if(nrow(night_steps) > 0){
    
    # Calculate the distances using st_distance
    night_steps[
      , sl_ := sf::st_distance(geometry_1, geometry_2, by_element = T)][
      , dt_ := difftime(t2_, t1_, units = "hours")][
      , night_steps_available := TRUE][
      , c("geometry_1", "geometry_2") := NULL]
    
  } else{
    
    night_steps <- data.table(night_steps_available = FALSE)

  }
  
  return(night_steps)
  
} 


# # 3 - FUNCTION: get_day_steps ---------------------------------------------


get_day_steps <- function(
    track, night_locs = NULL, day_limits = NULL, crs = sf::st_crs(4326)){
  
  # Check if day cycle needs to be added
  if(all(!c("day_cycle", "day_period") %in% colnames(track))){
    
    if(is.null(day_limits)){
      stop("Provide day limits for caluclating day cycle and period")
    }
    
    track <- add_day_cycle(track, day_limits = day_limits)
    
  } 
  
  if(is.null(track)){ return(data.table(day_steps_available = F)) }
  
  if(is.null(night_locs)){
    
    night_locs <- unique(track[
      day_period == "night",
      .SD[which.max(t_)],    
      by = day_cycle
    ])
    
  }
  
 night_locs <- night_locs[
    , .(t1_ = t_, x1_ = x_, y1_ = y_, day_cycle_1 = day_cycle)][
    , geometry_1 := sf::st_as_sf(.SD, coords = c("x1_", "y1_"), crs = crs), 
        .SDcols = c("x1_", "y1_")]
  
  day_locs <- unique(track[day_period == "day"])[
    , .(t2_ = t_, x2_ = x_, y2_ = y_, day_cycle_2 = day_cycle)][
    , day_cycle_1 := day_cycle_2 - 1][
    , geometry_2 := sf::st_as_sf(.SD, coords = c("x2_", "y2_"), crs = crs), 
    .SDcols = c("x2_", "y2_")]
  
  day_steps <- merge(
   day_locs,  night_locs, by = "day_cycle_1", all.x = TRUE
  )[!sf::st_is_empty(geometry_2) & !sf::st_is_empty(geometry_1)]
  
  if(nrow(day_steps) > 0){
    
    day_steps[
      , sl_ := sf::st_distance(geometry_1, geometry_2, by_element = T)][
        , `:=` (
          sl_min = min(sl_, na.rm = TRUE),
          sl_mean = mean(sl_, na.rm = TRUE),
          sl_median = median(sl_, na.rm = TRUE),
          sl_n_locs = .N
        ), 
        by = day_cycle_1
      ][ , c("geometry_1", "geometry_2") := NULL]
    
    setorder(day_steps, t1_)
    
    day_steps <- day_steps[, .SD[which.max(sl_)], by = day_cycle_1][
      , day_steps_available := T]
    
  } else{
    
    day_steps <- data.table(day_steps_available = F)
    
  }
  
  return(day_steps)

}



# FUNCTION: add_worldmap_data ---------------------------------------------

add_worldmap_data <- function(
    steps, crs = sf::st_crs(4326), align_start = T, scale = "medium", 
    world_cols = c("continent", "sovereignt", "admin")){
  
  if(align_start){
    
    steps <- steps[
      , geometry := sf::st_as_sf(
        .SD, coords = c("x1_", "y1_"), crs = crs), .SDcols = c("x1_", "y1_")] 
    
  } else{
    
    steps <- steps[
      , geometry := sf::st_as_sf(
        .SD, coords = c("x2_", "y2_"), crs = crs), .SDcols = c("x2_", "y2_")] 
    
  }
  
  world_cols_geo <- c(world_cols, "geometry")
  world <- as.data.table(
    rnaturalearth::ne_countries(scale = scale, returnclass = "sf"))[
      , ..world_cols_geo]
  
  world_id <- sf::st_nearest_feature(steps$geometry, world$geometry)
  
  steps <- cbind(steps[, geometry := NULL], world[world_id, ..world_cols])
  
  return(steps)
  
}






