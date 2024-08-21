#' ---
#' title: "Re-sampling tracking data and extracting basic track parameters"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' **SECTION 1 - Re-sampling tracks**
#' - filter tracks that were labelled previously (scrips 2_group_by_species.R)
#' - keep unique locations (to avoid having step length 0)
#' - re-sample tracks with predefined rate and tolerance (24 h +- 2h)
#' - save re-sampled tracks per species
#' **1.2 - Summary of the resapled tracks**
#' - save summary of the sampling rate
#' 
#' **SECTION 2 - Converting track to steps**
#' load the tracks from 1 and convert them to steps:
#' - keep only the bursts with 3 locations
#' - calculate turning angle and distances 
#'     
#' **SECTION 3 - Plotting distances and turning angles**
#' - plotting step length
#' - plotting turning angles



# 0 - packages and files --------------------------------------------------

# to handle movement data
library(data.table)
library(tidyverse)
library(here)
library(suncalc)
library(move2)
library(atlastools)
library(sf)
library(amt)
library(nngeo)

source("0_helper_functions.R")


main_cols <- c("track_id", "timestamp", "lon", "lat",
               "track_problem", "loc_problem")

# getting species of interest
target_sp <- here("Data", "downloadable_studies_deployments_filtered.csv") |> 
  read_csv(show_col_types = F) |> 
  distinct(taxon_canonical_name) |> 
  as_vector() |> 
  str_replace(" ", "_")

# resample rate and tolerance
resample_rate = hours(24)
resample_tolerance = hours(2)



# 1 - Resampling tracks -------------------------------------------------------

# resampling tracks with the predafined rate and tolerance

target_sp |> 
  map(~{
    
    sp <- .x 
    
    resampled_tracks <- here("Data", "Studies", sp) |> 
      # list all studies
      list.files(pattern = "study.csv") |> 
      map(~{
        
        fname <- .x
        
        here("Data", "Studies", sp, fname) |> 
          read_csv(show_col_types = F) |> 
          # keep points that have no problem
          filter(is.na(track_problem)) |> 
          select(-track_problem) |> 
          # select columns of interest
          select(any_of(main_cols)) |> 
          mk_track(
            .x = lon, 
            .y = lat, 
            .t = timestamp, 
            crs = sf::st_crs(4326),
            # crs = 4326,
            # alternative way to specify crs
            all_cols = T
          ) |> 
          # eliminating duplicated positions per individual do avoid distances 0,
          # this is a bit extreme as positions can be days spaces apart, 
          # but couldn't think of a better way
          distinct(x_, y_, .keep_all = T, .by = track_id) |> 
          mutate(
            study_id = str_remove_all(fname, str_c(sp, "|_|", "study.csv"))
          ) |> 
          group_split(study_id, track_id) |> 
          map(~{
            
            # re-sampled tracks using predefined sampling rate and tolerance
            .x |> 
              track_resample(
                rate = resample_rate, 
                tolerance = resample_tolerance
              ) 
            
          }) |> 
          bind_rows() 
        
      }) |> 
      bind_rows() 
    
    # saving re-sampled tracks
    resampled_tracks |> 
      write_csv(
        here("Data", "Studies", sp, str_c(sp, "_all_tracks_resampled.csv"))
      )
    

# 1.2 - summary of the resampled tracks --------------------------------------

    # it reports an error in summary if you pass these tracks, 
    # but I wanted to keep track of them, so saved them like this
    filtered <- resampled_tracks |> 
      filter(n() < 2, .by = c("study_id", "track_id")) |> 
      mutate(
        n = n(), 
        comment = "insufficient tracking time"
      ) |> 
      select(study_id, track_id, comment)
      
    resampled_tracks |> 
      filter(n() >= 2, .by = c("study_id", "track_id")) |> 
      summarize_sampling_rate_many(
        c("study_id", "track_id"), time_unit = "hour"
      ) |> 
      bind_rows(filtered) |> 
      write_csv(
        here(
          "Data", "Studies", sp, 
          str_c(sp, "_all_tracks_resampled_sampling_rate.csv")
        )
      )
    
    print(str_c(sp, " DONE!"))
    
  }, 
  .progress = T
  )



# 2 - Converting track to steps -----------------------------------------------


target_sp |> 
  map(~{
    
    sp <- .x 
    
    fname <- here("Data", "Studies", sp) |> 
      list.files(pattern = "_all_tracks_resampled.csv") 
    
    here("Data", "Studies", sp, fname) |> 
      read_csv(show_col_types = F) |> 
      mk_track(
        .x = x_, 
        .y = y_, 
        .t = t_, 
        crs = sf::st_crs(4326),
        # crs = 4326,
        # alternative way to specify crs
        all_cols = T
      ) |> 
      group_split(study_id, track_id) |> 
      map(~{
        
        .x |> 
          # for transforming the coordinate system, in case needed in the future
          # crs_sub <-  mt_aeqd_crs(mt_as_move2(sub), center = "centroid", units = "m")
          # transform_coords(st_crs(crs_sub)) |>
          filter_min_n_burst(min_n = 3) |>
          steps_by_burst(lonlat = T, keep_cols = T) |> 
          mutate(
            track_id = unique(.x$track_id), 
            study_id = unique(.x$study_id)
          )
        
      }) |> 
      bind_rows() |> 
      write_csv(
        here("Data", "Studies", sp, str_replace(fname, ".csv", "_steps.csv"))
      )
      
      
  })



# 3 - Plotting distances and turning angles -------------------------------

# making plots of step length and turning angles

target_sp |> 
  map(~{
    
    sp <- .x 
    
    fname <- here("Data", "Studies", sp) |> 
      list.files(pattern = "_all_tracks_resampled_steps.csv") 
    
    steps_df <- here("Data", "Studies", sp, fname) |> 
      read_csv(show_col_types = F) |> 
      rename(step = sl_, turn = ta_) |> 
      mutate(step = step/1000) |> 
      mutate(speed = step/(dt_/3600))
    
    # plotting step lenght
    {
      
      steps_df |> 
        ggplot() +
        geom_histogram(aes(x = step), binwidth = 10) +
        labs(
          title = str_c(
            str_replace(sp, "_", " "), " - step lengths from all tracks"
          ), 
          x = "step length [km]"
        ) +
        theme_bw(base_size = 14) 
      
      } |> 
      ggsave(
        file = here("Data", "Graphs", str_c(sp, "_steps.png"))
      )
        #width = 20, height = 25, units = "cm")
    
    # plotting turning angles
    {
      
      steps_df |> 
        ggplot() +
        geom_histogram(aes(x = turn), binwidth = 0.1) +
        labs(
          title = str_c(
            str_replace(sp, "_", " "), " - turning angles from all tracks"
          ), 
          x = "turning angle [rad]"
        ) +
        theme_bw(base_size = 14) 
      
      } |> 
      ggsave(
        file = here("Data", "Graphs", str_c(sp, "_turns.png"))
        ) 
        #width = 20, height = 25, units = "cm")
      
    
    # {
    #   
    #   steps_df |> 
    #     ggplot() +
    #     geom_histogram(aes(x = speed), binwidth = 10) +
    #     labs(
    #       title = str_c(
    #         str_replace(sp, "_", " "), " - speed from all tracks"
    #       ), 
    #       x = "speed [km/h]"
    #     ) +
    #     theme_bw(base_size = 14) 
    #   
    #   } |> 
    #   ggsave(
    #     file = here("Data", "Graphs", str_c(sp, "_speed.png"))
    #   )

  })

# Warning messages:
# 1: One or more parsing issues, call `problems()` on your data frame for details, e.g.:
#   dat <- vroom(...)
# problems(dat) 
# 2: One or more parsing issues, call `problems()` on your data frame for details, e.g.:
#   dat <- vroom(...)
# problems(dat) 
# 3: One or more parsing issues, call `problems()` on your data frame for details, e.g.:
#   dat <- vroom(...)
# problems(dat) 
# 4: One or more parsing issues, call `problems()` on your data frame for details, e.g.:
#   dat <- vroom(...)
# problems(dat) 
# 5: Removed 625 rows containing non-finite values (`stat_bin()`). 
# 6: Removed 1 rows containing non-finite values (`stat_bin()`). 
# 7: Removed 446 rows containing non-finite values (`stat_bin()`). 
# 8: Removed 125 rows containing non-finite values (`stat_bin()`). 
# 9: Removed 259 rows containing non-finite values (`stat_bin()`). 

