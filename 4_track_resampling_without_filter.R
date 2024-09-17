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
#' **1.2 - Summary of the resampled tracks**
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



# 1 - Re-sampling tracks -------------------------------------------------------

# re-sampling tracks with the predefined rate and tolerance

target_sp |> 
  map(~{
    
    sp <- .x 
    
    # folder of the species data
    sp_dir <- here("Data", "Studies", sp)
    
    sp_dir |> 
      # list all studies
      list.files(pattern = "_study.rds") |> 
      map(~{
        
        fname <- .x
        
        # load movebank track Anne's advice, instead of loading like csv
        # read_csv(show_col_types = F) |> 
        # mk_track(
        #   .x = lon, 
        #   .y = lat, 
        #   .t = timestamp, 
        #   crs = sf::st_crs(4326),
        #   # crs = 4326,
        #   # alternative way to specify crs
        #   all_cols = T
        # )
        move_track <- here(sp_dir, fname) |> 
          readRDS() |> 
          # keep points that have no problem
          filter(is.na(track_problem)) 
        
        if(nrow(move_track) > 0) {
          
          # converting move2 track to amt track
          move_track |>
            track(
              x = st_coordinates(move_track)[,1],
              y = st_coordinates(move_track)[,2],
              t = move2::mt_time(move_track),
              crs = sf::st_crs(move_track)
            ) |>
            # eliminating duplicated positions per individual 
            # to avoid distances 0,
            # this is a bit extreme as positions can be days spaced apart,
            # but couldn't think of a better way
            distinct(x_, y_, .keep_all = T, .by = track_id) |>
            mutate(
              study_id = str_remove_all(fname, str_c(sp, "|_|", "study.rds"))
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
            bind_rows() |> 
            mutate(resample_comment = "track resampled")
          
        } else {
          
          # in case there are 0 locations after filtering tracks
          tibble(
            study_id = str_remove_all(fname, str_c(sp, "|_|", "study.rds")), 
            resample_comment = "didn't pass filtering"
            )
          
        }
        
      }) |> 
      bind_rows() |> 
      # saving re-sampled tracks
      saveRDS(
        here(sp_dir, str_c(sp, "_all_tracks_resampled.rds")), 
        compress = T
      )
    
    print(str_c(sp, " DONE!"))
    
    }, 
    .progress = T
    )
    

# 1.2 - summary of the resampled tracks --------------------------------------

target_sp |> 
  map(~{
    
    sp <- .x 
    sp_dir <- here("Data", "Studies", sp)
    
    resampled_tracks <- here(sp_dir, str_c(sp, "_all_tracks_resampled.rds")) |> 
      readRDS() |> 
      mutate(n = n(), .by = c("study_id", "track_id"))
    
    # it reports an error in summary if you pass these tracks,
    # because there is insufficient data to summarize
    # but I wanted to keep track of them, so saved them like this
    filtered <- resampled_tracks |>
      filter(n < 2) 

    resampled_tracks |>
      filter(n >= 2) |>
      summarize_sampling_rate_many(
        c("study_id", "track_id"), time_unit = "hour"
      ) |>
      as_tibble() 
    
    if(nrow(filtered) > 0){
      
      filtered <- filtered |>
        mutate(
          resample_comment = ifelse(
            resample_comment == "track resampled", 
            "insufficient tracking time", 
            resample_comment
          )
        )
          
      resampled_tracks <- resampled_tracks |> 
        bind_rows(filtered)
          
    }
    
    resampled_tracks |> 
      write_csv(here(sp_dir, str_c(sp, "_all_tracks_resampled_summary.csv")))

    print(str_c(sp, " summary DONE!"))
    
  }, 
  .progress = T
  )



# 2 - Converting track to steps -----------------------------------------------


target_sp |> 
  map(~{
    
    sp <- .x 
    sp_dir <- here("Data", "Studies", sp)
    
    resampled_tracks <- here(sp_dir, str_c(sp, "_all_tracks_resampled.rds")) |> 
      readRDS() |>
      group_split(study_id, track_id) |> 
      map(~{
        
        .x |> 
          # for transforming the coordinate system, for the future
          # crs_sub <- mt_aeqd_crs(mt_as_move2(sub), center = "centroid", units = "m")
          # transform_coords(st_crs(crs_sub)) |>
          filter_min_n_burst(min_n = 3) |>
          steps_by_burst(lonlat = T, keep_cols = T) |> 
          mutate(
            track_id = unique(.x$track_id), 
            study_id = unique(.x$study_id)
          )
        
      }) |> 
      bind_rows() |> 
      saveRDS(
        here(sp_dir, str_c(sp, "_all_tracks_resampled_steps.rds")), 
        compress = T
      )
      
    print(str_c(sp, " summary DONE!"))
      
  }, 
  .progress = T
  )



# 3 - Plotting distances and turning angles -------------------------------

# making plots of step length and turning angles

target_sp |> 
  map(~{
    
    sp <- .x 
    
    sp_dir <- here("Data", "Studies", sp)
    
    steps_df <- here(sp_dir, str_c(sp, "_all_tracks_resampled_steps.rds")) |> 
      readRDS() |> 
      rename(step = sl_, turn = ta_) |> 
      mutate(
        step = step/1000, 
        dt_ = as.numeric(dt_, units = "hours"), 
        speed = step/dt_
      )
    
    # plotting step length
    {
      
      steps_df |> 
        ggplot() +
        geom_histogram(aes(x = step), binwidth = 1) +
        labs(
          title = str_c(
            str_replace(sp, "_", " "), " - step lengths from all tracks"
          ), 
          x = "step length [km] | binwidth = 1"
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
          x = "turning angle [rad] | binwidth = 0.1"
        ) +
        theme_bw(base_size = 14) 
      
      } |> 
      ggsave(file = here("Data", "Graphs", str_c(sp, "_turns.png"))) 
        #width = 20, height = 25, units = "cm")
      
    
    {

      steps_df |>
        ggplot() +
        geom_histogram(aes(x = speed), binwidth = 1) +
        labs(
          title = str_c(
            str_replace(sp, "_", " "), " - speed from all tracks"
          ),
          x = "speed [km/h] | binwidth = 1"
        ) +
        theme_bw(base_size = 14)

      } |>
      ggsave(
        file = here("Data", "Graphs", str_c(sp, "_speed.png"))
      )

  })


# Warning messages:
# 1: Removed 625 rows containing non-finite values (`stat_bin()`). 
# 2: Removed 1 rows containing non-finite values (`stat_bin()`). 
# 3: Removed 446 rows containing non-finite values (`stat_bin()`). 
# 4: Removed 125 rows containing non-finite values (`stat_bin()`). 
# 5: Removed 259 rows containing non-finite values (`stat_bin()`). 



