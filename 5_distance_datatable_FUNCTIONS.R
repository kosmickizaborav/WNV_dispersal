#' ---
#' title: "5_0_distance_functions"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' Here are provided functions for caltulating distances, used in the script
#' get_daily_distances. 
#' **FUNCTION 1: add_day_cycle**
#' helper function to obtain day_cycle and day_period, since it is needed 
#' for other functions. It uses the suncalc package to obtain the day limits, 
#' and then adds day_cycle and day_period to the track data.
#'  - day cycle is defined as the period between the start of the day and the 
#'    star of the next one (i.e. all the tracks recorded before the dawn, are
#'    assigned to the day cycle of the previoous calendar date)
#'  - day period is assigned based on the definition of day start and day end
#'    (e.g. day start at dawn and finishes at dusk when night starts)
#' 
#' **FUNCTION 2 - get_dcp_dist**
#' function to calculate distances for each day_cycle_period (DCP), 
#' it groups the data in day_cycle_period and calculates the distances 
#' between all locations during DCP and some basic stats of these 
#' distances (min, max, median, mean). it also saves the median x_, y_ and t_, 
#' as they will later represent the sleeping position of the animal. 
#' 
#' **FUNCTION 3 - get_sleep_steps**
#' function to calculate the distances between the median night or day position
#' between the consecutive days. these will represent night steps (distance
#' between the sleeping position in consecutive days). they are done for night 
#' and day as we have diurnal and nocturnal species, so sleeping locations will 
#' be determined based on that. 
#' 
#' **FUNCTION 4 - get_active_steps**
#' function to calculate distances between the sleep steps and the active period
#' of the day: 
#'   1 - median sleep position to median active position
#'   2 - median sleep position to all active positions with extraction of the 
#'       point that generates the greatest distance - is the furthest away form
#'       the sleep poistion; info on minimum, mean and median distance 
#'       obtained also saved
#'       
#' ** FUNCTION 5 - add_worldmap_data**
#' function to add world map data to the steps data so that we can distinguish
#' between european and other points as well as to eliminate the tracks that 
#' don't have any steps available, in the previous calculation we preserved 
#' that information for consistency and ability to check the process

# FUNCTION 1: add_day_cycle ---------------------------------------------

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
  setorder(track, t_)
  
  
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

# FUNCTION 2: get_dcp_dist ------------------------------------------------

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


# FUNCTION 3: get_sleep_steps ------------------------------------------------

get_sleep_steps <- function(track, crs = sf::st_crs(4326)){
  
  no_result <- data.table(dcp_steps_available = FALSE)
  
  track <- split(track, by = "day_period", keep.by = T)

  all_steps <- rbindlist(lapply(track, function(dcpt){
    
    dcpt <- dcpt[!is.na(x_) & !is.na(y_)]
    
    if(nrow(dcpt) == 0) {return(no_result)}
    
    locs <- dcpt[
      , geometry := sf::st_as_sf(.SD, coords = c("x_", "y_"), crs = crs), 
      .SDcols = c("x_", "y_")]
    setorder(locs, t_)
    
    rm(dcpt)
    
    if(nrow(locs) < 2){ return(no_result) }
    
    # create steps by combining consecutive rows
    steps <- cbind(
      # start location of the step
      locs[-.N, .(
        t1_ = t_, x1_ = x_, y1_ = y_, geometry_1 = geometry, 
        day_cycle_1 = day_cycle)], 
      # end location of the step
      locs[-1, .(
        t2_ = t_, x2_ = x_, y2_ = y_, geometry_2 = geometry, 
        day_cycle_2 = day_cycle)]
    )
    
    # check if the start location and end location are in consecutive days
    steps <- steps[, consequtive_days := day_cycle_1 == (day_cycle_2 - 1)][
      consequtive_days == T ][, consequtive_days := NULL]
    
    if(nrow(steps) > 0){
      
      # Calculate the distances using st_distance
      steps[
        , sl_ := sf::st_distance(geometry_1, geometry_2, by_element = T)][
          , dt_ := difftime(t2_, t1_, units = "hours")][
            , dcp_steps_available := TRUE][
              , c("geometry_1", "geometry_2") := NULL]
      
      return(steps)
      
    } else{ return(no_result) }
    
  }), idcol = "day_period", fill = T)
  
  return(all_steps)
  
  # # Check if day cycle needs to be added
  # if(all(!c("day_cycle", "day_period") %in% colnames(track))){
  #   if(is.null(day_limits)){
  #     stop("Provide day limits for caluclating day cycle and period")
  #   }
  #   track <- add_day_cycle(track, day_limits = day_limits)
  # }
  # get unique night locations with the maximum t_ for each day_cycle
  # this was done to get the furthest point during the night, abandoned in dcp
  # night_locs <- unique(track[
  #   day_period == for_day_period,
  #   .SD[which.max(t_)],    
  #   by = day_cycle
  # ])
  
} 



# FUNCTION 4: get_active_steps ------------------------------------------

get_active_steps <- function(
    track, 
    sleep_locs = NULL, 
    day_limits = NULL, 
    crs = sf::st_crs(4326), 
    night_to_day = T){
  
  if(night_to_day){ 
    s_start = "night"; s_end = "day"; day_cycle_diff = 1
  } else{
    s_start = "day"; s_end = "night"; day_cycle_diff = 0
  }
  
  no_result <- data.table(
    active_steps_available = F, 
    step_type = paste0(s_start, "_", s_end)
  )
  
  # Check if day cycle needs to be added
  if(all(!c("day_cycle", "day_period") %in% colnames(track))){
    if(is.null(day_limits)){
      stop("Provide day limits for caluclating day cycle and period")
    }
    track <- add_day_cycle(track, day_limits = day_limits)
  } 
  
  if(is.null(track)){ return(no_result) }
  
  if(is.null(sleep_locs)){ sleep_locs <- track }
  
  # making sure they come from the right period
  sleep_locs <- sleep_locs[day_period == s_start]
  
  if(nrow(sleep_locs) == 0){ return(no_result) }
  
  # get the locations of sleeping 
  sleep_locs <- sleep_locs[
    , .(t1_ = t_, x1_ = x_, y1_ = y_, day_cycle_1 = day_cycle)][
    , geometry_1 := sf::st_as_sf(.SD, coords = c("x1_", "y1_"), crs = crs), 
    .SDcols = c("x1_", "y1_")]
  
  # get active locations
  active_locs <- unique(track[day_period == s_end])[
    , .(t2_ = t_, x2_ = x_, y2_ = y_, day_cycle_2 = day_cycle)][
    , day_cycle_1 := day_cycle_2 - day_cycle_diff][
    , geometry_2 := sf::st_as_sf(.SD, coords = c("x2_", "y2_"), crs = crs), 
    .SDcols = c("x2_", "y2_")]
  
  steps <- merge(sleep_locs, active_locs, by = "day_cycle_1", all.x = TRUE)[
    !sf::st_is_empty(geometry_2) & !sf::st_is_empty(geometry_1)]
  
  if(nrow(steps) > 0){
    
    steps[, sl_ := sf::st_distance(geometry_1, geometry_2, by_element = TRUE)][
      , `:=`(
        sl_min    = min(sl_), #if (.N == 1) NA else 
        sl_mean   = mean(sl_),
        sl_median = median(sl_),
        sl_n_steps = .N
      ),
      by = day_cycle_1
    ][
      , c("geometry_1", "geometry_2") := NULL
    ]
    
    setorder(steps, t1_)
    
    steps <- steps[, .SD[which.max(sl_)], by = day_cycle_1][
      , active_steps_available := TRUE][
      , step_type := paste0(s_start, "_", s_end)]
    
    
    return(steps)
    
  } else{ return(no_result) }

}


# FUNCTION 5: add_worldmap_data ---------------------------------------------

add_worldmap_data <- function(
    steps, crs = sf::st_crs(4326), align_start = T, scale = "medium", 
    world_cols = c("continent", "sovereignt", "admin")){
  
  # overlay the map with start or the end point of the step (didn't check both
  # to preserve the country information in case we need it later)
  if(align_start){
    
    steps <- steps[
      , geometry := sf::st_as_sf(
        .SD, coords = c("x1_", "y1_"), crs = crs), .SDcols = c("x1_", "y1_")] 
  } else{
    
    steps <- steps[
      , geometry := sf::st_as_sf(
        .SD, coords = c("x2_", "y2_"), crs = crs), .SDcols = c("x2_", "y2_")] 
  }
  
  # columns to add to the data
  world_cols_geo <- c(world_cols, "geometry")
  # load the world map and convert it to data.table
  world <- as.data.table(
    rnaturalearth::ne_countries(scale = scale, returnclass = "sf"))[
      , ..world_cols_geo]
  
  # check the nearest geometry
  world_id <- sf::st_nearest_feature(steps$geometry, world$geometry)
  # add columns to the original step data
  steps <- cbind(steps[, geometry := NULL], world[world_id, ..world_cols])
  
  return(steps)
  
}






