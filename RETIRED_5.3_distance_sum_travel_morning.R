#' ---
#' title: "Calculating total travel distance per day cycle"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' **SECTION 1 - Calculate total distance travelled**
#' using one location per day morning file, we take the track between the start 
#' and end point of the step and compute total distance travelled between
#' the two locations
#' 
#' **SECTION 3 - generate one file per species**
#' output: 


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
      "Data",  "Studies", .x, "5_distances", "3_sum_travel_morning"
    )
    
    if(!dir.exists(mdist_dir)){ mdist_dir |>  dir.create() }
    
  })

# summary graphs directory
# ggraph_dir <- here("Data",  "Graphs")


# 1 - Calculate total travel distance ----------------------------------------


target_sp |> 
  map(~{
    
    sp <- .x 
    # folder of the species data
    sp_dir <- here("Data",  "Studies", sp)
    
    # loading the one loc per day morning steps 
    morning_steps <- here(
      sp_dir, "5_distances", "5.1_all_tracks_one_loc_morning_steps.rds"
      ) |> 
      read_rds() 
    
    
    # list all deployments
    files <- unique(morning_steps$track_file)
    lfl <- length(files)
    
    files |> 
      map(~{
        
        fin <- .x
        fout <- str_replace(fin, "_speed_filtered.rds", "_sum_travel_morning.rds")
        
        print(paste(sp, which(fin == files), "|", lfl))
        
        # taking only the points around the dawn
        track <- here(sp_dir, "4_filtered_speed", fin) |> 
          read_rds() 
        
        morning_steps |> 
          select(track_file, step_id, t1_, x1_, y1_, t2_, x2_, y2_) |> 
          filter(track_file == fin) |> 
          group_split(step_id) |> 
          map(~{
            
            step_df <- .x 
            
            strack <- track |> 
              filter(timestamp >= step_df$t1_, timestamp <= step_df$t2_) 
            
            total_travel <- sum(mt_distance(strack, units = "m"), na.rm = T)
            
            step_df |> 
              mutate(sl_ = total_travel)
            
          }) |>  # close map step_id
          bind_rows() |> 
          mutate(track_file = fin) |> 
          write_rds(
            here(here(sp_dir, "5_distances", "3_sum_travel_morning", fout))
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
    here(ddir, "3_sum_travel_morning") |> 
      list.files(full.name = T) |> 
      map(read_rds) |> 
      bind_rows() |> 
      write_rds(here(ddir, "5.3_all_tracks_sum_travel_morning.rds"))
    
    print(paste(sp, "DONE!"))
    
  }) 
