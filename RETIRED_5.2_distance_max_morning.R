#' ---
#' title: "Calculating maximum distances per day cycle"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' **SECTION 1 - Calculate max daily distance **
#' using one location per day morning file, we take the track between the start 
#' and end point of the step and compute the distances between the start point
#' and all the other available points in the track, saving the maximum 
#' distance achieved for that period
#' 
#' **SECTION 3 - generate one file per species**
#' output: "5.2_all_tracks_max_daily_distance.rds"


# 0 - packages and files --------------------------------------------------

# to handle movement data
library(data.table)
library(tidyverse)
library(here)
library(move2)
library(sf)
library(amt)
library(ggpubr)

source("0_helper_functions.R")


# getting the species of interest
target_sp <- here("Data", "1_downloadable_studies_deployments_filtered.csv") |> 
  read_csv(show_col_types = F) |> 
  distinct(species) |> 
  as_vector() |> 
  str_replace(" ", "_")

target_sp |> 
  map(~{
    
    mdist_dir <- here(
      "Data",  "Studies", .x, "5_distances", "2_max_distance_morning"
    )
    
    if(!dir.exists(mdist_dir)){ mdist_dir |>  dir.create() }
    
  })

# summary graphs directory
ggraph_dir <- here("Data",  "Graphs")


# 1 - Calculate max daily distance ----------------------------------------


target_sp |> 
  map(~{
    
    sp <- .x 
    # folder of the species data
    sp_dir <- here("Data",  "Studies", sp)
    
    # loading the one loc per day morning steps 
    morning_steps <- here(
      sp_dir, "5_distances", "5.1_all_tracks_one_loc_morning_steps.rds"
      ) |> 
      read_rds() |> 
      select(track_file, t1_, t2_, x1_, y1_, step_id, burst_) 
    
    
    # list all deployments
    files <- unique(morning_steps$track_file)
    lfl <- length(files)
     
    files |> 
      map(~{
        
        fin <- .x
        fout <- str_replace(fin, "_speed_filtered.rds", "_max_dist_morning.rds")
        
        print(paste(sp, which(fin == files), "|", lfl))
        
        # taking only the points around the dawn
        track <- here(sp_dir, "4_filtered_speed", fin) |> 
          read_rds() 
        
        morning_steps |> 
          filter(track_file == fin) |> 
          st_as_sf(coords = c("x1_", "y1_"), crs = st_crs(track)) |> 
          group_split(step_id) |> 
          map(~{
            
            step_df <- .x 
              
            strack <- track |> 
              filter(timestamp >= step_df$t1_, timestamp <= step_df$t2_) 
            
            tibble(
                t2_ = strack$timestamp, 
                x2_ = st_coordinates(strack)[,1], 
                y2_ = st_coordinates(strack)[,2],
                n_locs = nrow(strack)
              ) |> 
              mutate(sl_ = st_distance(strack, step_df)[,1]) |> 
              mutate(
                t1_ = step_df$t1_, 
                x1_ = st_coordinates(step_df)[,1], 
                y1_ = st_coordinates(step_df)[,2],
                step_id = step_df$step_id, 
                burst_ = step_df$burst_
              ) |> 
              arrange(sl_, t2_) |> 
              slice_tail(n = 1)
              
          }) |>  # close map step_id
          bind_rows() |> 
          mutate(track_file = fin) |> 
          write_rds(
            here(here(sp_dir, "5_distances", "2_max_distance_morning", fout))
          )
        
      }) # close map files
     
    print(paste(sp, "DONE!"))
    
  }) # close map species 


# 2 - Generate one file per species ---------------------------------------

# grouping data for all tracks to one file per species

target_sp |> 
  map(~{
    
    sp <- .x
    ddir <- here("Data", "Studies", sp, "5_distances")
    
    # list all deployments that are re-sampled
    here(ddir, "2_max_distance_morning") |> 
      list.files(full.name = T) |> 
      map(read_rds) |> 
      bind_rows() |> 
      write_rds(here(ddir, "5.2_all_tracks_max_morning_steps.rds"))
    
    print(paste(sp, "DONE!"))
    
  }) 
    
