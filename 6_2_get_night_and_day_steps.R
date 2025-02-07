#' ---
#' title: "6 2 calculate night and day steps"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' This script is used to generate the distances between the consecutive nights
#' as well as the maximum distances reached during the day.
#' INPUT:
#' - libraries needed
#' - sp and sp_dir: species and its folder 
#' - files and lfl: list of the files to be processed, and their number 
#' - limits for day and night: day_start, day_end
#' - cols_of_interest, day_lim: columns of interest and the day limit
#' 
#'  **SECTION 1 - calculate night steps**
#'  **SECTION 2 - calculate day steps**
#'  
#'  The output is saved in the 6_distances folder as night and day steps

# define output dataframe
all_night_steps <- tibble()
all_day_steps <- tibble()


# 0 - load libraries ------------------------------------------------------

# Load required libraries
library(tidyverse)
library(sf)
library(amt)

# 1 - Function: get_night_day_steps ----------------------------------------


get_night_day_steps <- function(
    fin, 
    day_lim = NULL, 
    cols_of_interest = NA, 
    # for night steps
    resample_rate = hours(24),
    resample_tolerance = hours(2),
    dir = here(sp_dir, "5_resampled")
    ){
  
  track <- here(dir, fin) |> 
    read_rds() |> 
    select(x_, y_, t_, all_of(c(cols_of_interest, day_lim))) |> 
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
    filter(!if_all(all_of(c("day_start", "day_end")), ~is.na(.))) |> 
    select(-date, -day_start, -day_end)
  
  
  if (sum(track$day_period == "night") < 3) {
    
    return(
      list(
        night_steps = tibble(track_file = fin, night_steps_available = F),
        day_steps = tibble(track_file = fin, day_steps_calculated = F)
      ) 
    )
    
  }
  
  # get night steps
  night_steps <- track |> 
    filter(day_period == "night") |> 
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
        night_steps = tibble(track_file = fin, night_steps_available = F),
        day_steps = tibble(track_file = fin, day_steps_calculated = F)
      ) 
    )
    
  }
  
  # add file name and step id to the night steps 
  # and join them to the rest of the data for the species
  night_steps_out <- night_steps |> 
    mutate(
      track_file = fin,
      step_id = str_c("b", burst_, "_s", 1:n()), 
      night_steps_available = T, 
      .by = burst_
    )
  
  rm(night_steps)
  
  # 2 - Calculate day steps -----------------------------------------------
  
  # keep only columns relevant for calculating day steps
  night_steps <- night_steps_out |> 
    select(
      t1_, t2_, x1_, y1_, step_id, track_file, day_cycle,
      all_of(cols_of_interest)
    ) 
  
  track <- track |> 
    select(t_, x_, y_, day_period)
  
  # calculate day steps by taking each night step
  # subsample track to all points between the two points of the step, 
  # calculate distance to all points available, and stats
  # keep only the last point (the one with the highest distance)
  day_steps <- night_steps |> 
    st_as_sf(
      coords = c("x1_", "y1_"), crs = get_crs(track), remove = F
    ) |> 
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
          # odrder by distance and time
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
          ) 
      } else{
        
        step_df |> 
          select(-t2_) |> 
          st_drop_geometry() |> 
          mutate(sl_ = NA)
        
      }
      
    }) |>  # close map step_id
    list_rbind() 
  
  return(list(night_steps = night_steps_out, day_steps = day_steps))
  
} 
  

