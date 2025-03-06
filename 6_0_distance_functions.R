#' ---
#' title: "6_0_distance_functions"
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

library(tidyverse)
library(here)
library(units)
library(amt)
library(sf)


# 1 - FUNCTION: get_day_cycle ---------------------------------------------

add_day_cycle <- function(track){
  
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

# 1 - FUNCTION: get_dcp_df ------------------------------------------------

# Function to process each file
get_dcp_df <- function(
    df, 
    day_lim = NULL, 
    cols_of_interest = NULL
){
  
  track <- df |> 
    select(x_, y_, t_, any_of(c(cols_of_interest, day_lim))) |> 
    filter(!if_all(all_of(c("day_start", "day_end")), ~is.na(.)))
  
  if(nrow(track) > 0) {
    
    dcp_track <- track |>
      add_day_cycle() |> 
      mutate(dcp = str_c(day_cycle, "_", day_period)) |> 
      # group by dcp and calculate the distances between the points
      group_split(dcp) |> 
      map(~{
        
        dcp_one <- .x 
        
        # calculate median location of during the day_cycle_period
        dcp_summary <- dcp_one |> 
          summarize(
            n_locs = n(), 
            x_median = median(x_),
            y_median = median(y_),
            t_median = median(t_), 
            t_min = min(t_),
            t_max = max(t_), 
            dcp = unique(dcp)
          )
        
        if(dcp_summary$n_locs > 1){
          
          # distance matrix between all the points available for dcp
          dist <- dcp_one |> 
            st_as_sf(coords = c("x_", "y_"), crs = get_crs(track)) |> 
            st_distance()
          dist[lower.tri(dist, diag = TRUE)] <- NA
          
          dcp_summary |> 
            mutate(
              dist_min = min(dist, na.rm = T),
              dist_max = max(dist, na.rm = T),
              dist_mean = mean(dist, na.rm = T),
              dist_median = median(dist, na.rm = T), 
              t_span = set_units(as_units(t_max - t_min), "hour")
            ) |> 
            mutate(
              across(
                starts_with("dist"), ~ifelse(is.numeric(.x), .x, NA)
              ), 
              dcp_available = T
            )
          
        } else{
          
          dcp_summary |> 
            mutate(dcp_available = FALSE)
          
        }
      }) |>  # map period
      list_rbind()
    
  } else {
    
    dcp_track <- tibble(n_locs = NA, dcp_available = FALSE)
    
  }
  
  return(dcp_track)
}


# 3 - FUNCTION: get_night_day_steps ---------------------------------------

get_night_day_steps <- function(
    df, 
    day_lim = NULL, 
    get_day_cycle = T, 
    cols_of_interest = NULL, 
    # for night steps
    resample_rate = hours(24),
    resample_tolerance = hours(12)
){
  
  if(get_day_cycle == T){
    
    track <- df |> 
      select(x_, y_, t_, any_of(c(cols_of_interest, day_lim))) |> 
      add_day_cycle()
    
  } else{
    
    track <- df |> 
      select(x_, y_, t_, day_cycle, day_period, any_of(c(cols_of_interest)))
    
  }
  
  
  if(sum(track$day_period == "night", na.rm = T) < 3){
    
    return(
      list(
        night_steps = tibble(night_steps_available = F),
        day_steps = tibble(day_steps_calculated = F)
      ) 
    )
    
  }
  
  # get night steps
  night_steps <- track |> 
    filter(day_period == "night", t_ == max(t_), .by = day_cycle) |> 
    track_resample(
      rate = resample_rate,
      tolerance = resample_tolerance
    ) |> 
    filter_min_n_burst(min_n = 3) |>
    # keep columns from the start, so that we have the same day
    # cycle in night and days later, if we would keep info for the end
    # it would point to the next day
    steps_by_burst(lonlat = T, keep_cols = 'start') 
  
  if(nrow(night_steps) == 0){
    
    # if there is no steps, just keep the file name
    return(
      list(
        night_steps = tibble(night_steps_available = F),
        day_steps = tibble(day_steps_calculated = F)
      ) 
    )
    
  }
  
  # addstep id to the night steps 
  # and join them to the rest of the data for the species
  night_steps <- night_steps |> 
    mutate(
      step_id = str_c("b", burst_, "_s", 1:n()), 
      night_steps_available = T, 
      .by = burst_
    )
  
  # 2 - Calculate day steps -----------------------------------------------
  
  # keep only columns relevant for calculating day steps
  
  day_steps <- get_day_steps(
    night_steps = night_steps, 
    track = track,
    cols_of_interest = cols_of_interest
  )
  
  return(list(night_steps = night_steps, day_steps = day_steps))
  
} 



# 3 - FUNCTION: get_day_steps ---------------------------------------------

get_day_steps <- function(night_steps, track, cols_of_interest = NULL){
  
  # keep only columns relevant for calculating day steps
  
  track <- track |> 
    select(t_, x_, y_, day_period)
  
  # calulate day steps
  day_steps <- night_steps |> 
    select(
      t1_, t2_, x1_, y1_, step_id, day_cycle, any_of(cols_of_interest)
    ) |> 
    st_as_sf(
      coords = c("x1_", "y1_"), crs = get_crs(track), remove = F
    ) |> 
    # calculate day steps by taking each night step
    # sub sample track to include all points between t1_ and t2_ of the step, 
    # calculate distance from the step origin to all other points, and stats
    # keep only the last point (the one with the highest distance)
    group_split(step_id) |> 
    map(~{
      
      step_df <- .x
      
      day_step_df <- track |> 
        filter(t_ > step_df$t1_, t_ <= step_df$t2_) |>
        st_as_sf(
          coords = c("x_", "y_"), crs = get_crs(track), remove = F
        ) |>
        mutate(sl_ = st_distance(geometry, step_df)[,1]) |>  
        st_drop_geometry() |> 
        # remove the points that fall during the night
        # in case we selected the t2_ from the night step 
        filter(day_period == "day") 
      
      if(nrow(day_step_df) > 0){
        
        day_step_df |> 
          # order by distance and time
          arrange(sl_, desc(t_)) |> 
          mutate(
            sl_min = min(sl_, na.rm = T),
            sl_mean = mean(sl_, na.rm = T),
            sl_median = median(sl_, na.rm = T), 
            sl_n_locs = n()
          ) |> 
          # take the last value (highest sl_ /and earliest t2_)
          slice_tail(n = 1) |> 
          mutate(
            across(
              starts_with("sl_"), ~ifelse(is.numeric(.x), .x, NA)
            )
          ) |> 
          # adding all relevant columns
          bind_cols(step_df |> select(-t2_) |> st_drop_geometry()) |> 
          rename_with(
            ~str_replace(.x, "_$", "2_"), all_of(matches("[txy]_$"))
          ) |> 
          mutate(day_steps_available = T)
        
      } else{
        
        step_df |> 
          select(-t2_) |> 
          st_drop_geometry() |> 
          mutate(
            sl_ = NA, 
            day_steps_available = F
          )
        
      }
      
    }) |>  # close map step_id
    list_rbind() 
  
  return(day_steps)
  
}


