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


library(data.table)
library(suncalc)

add_day_cycle <- function(track, day_limits = NULL) {
  
  # Convert to data.table
  setDT(track)
  
  sun_times_df <- getSunlightTimes(
    data = data.frame(date = as.Date(track$t_), lat = track$y_, lon = track$x_), 
    keep = day_limits, 
    tz = "UTC"
  )
  
   
  # track <- track |> 
  #   cbind(sun_times_df[, day_limits]) |> 
  #   filter(!dplyr::if_all(all_of(sun_times), ~is.na(.))) |> 
  #   select(-burst_)
  
  
  track[, `:=`(
    date = as.IDate(t_), 
    day_cycle = ifelse(
      t_ < get(day_limits[1]),
      # Assign the previous day for times before day_start
      format(date - 1, "%d%m%Y"), 
      format(date, "%d%m%Y")
    ),
    day_period = ifelse(
      t_ < get(day_limits[1]) | t_ > get_day_limits[2], 
      "night", 
      "day"
    )
  )]
  
  # Remove unnecessary columns
  track[, date := NULL]
  
  return(track)
}

add_day_cycle <- function(track, day_limits = NULL){
  
  track |> 
    # proceed only if we have day and night limit 
    # in the northern latitudes sometimes the sun doesn't set so 
    # to filter out those locations if we are using the nautical dusk
    mutate(
      date = as.Date(t_), 
      # before I defined day_cycle as day of the year, but decided to stick
      # with the numbers because in this way I don't mix feb 29 and march 1st
      # str_c(yday(date - 1), "_", year(date - 1)),
      # str_c(yday(date), "_", year(date))
      day_cycle = if_else(
        t_ < day_start,
        # in case the time is before day start for that date
        # asign the day_cycle to the previous day, so that night has continuity
        as.character(format(date-1, "%d%m%Y")), 
        as.character(format(date, "%d%m%Y"))     
      ),
      # alternative t_ > day_start & t_ < day_end, "day", "night" 
      # gives slightly different results
      # i defined night wihtout = sign so that we keep the "darkest"
      # period always, I had doubts with the variables night and nightEnd, 
      # but decided to keep this because of dawn and dusk, 
      # to make sure it's before dawn and after dusk
      day_period = if_else(t_ < day_start | t_ > day_end, "night", "day")
    ) |> 
    select(-date, -day_start, -day_end)
  
}
