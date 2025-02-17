#' ---
#' title: "Get daily distances"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' using different definitions of day and night periods, this scritp calculates
#' the distances during each period.
#'  **SECTION 1 - Distances per day period**
#' splits the track into day-cycle-periods (e.g. day1_night) and calculates
#' distances between all points avaialable in each period. after that uses that 
#' dataframe to calcualte step lenght from the median points obtained
#' **SECTION 2 - Night and day steps**
#' calculates night steps by using the last point available during the night, 
#' and then resamples to paeriod of 24 hours. the day steps are subsequently
#' calculated by taking the origin point of the night steps and then calculating
#' the distance from that point to all available points during that time. 


# 0 - Load packages -------------------------------------------------------

library(here)
library(tidyverse)
library(amt)
#load the functions for distance calculations
source(here("6_0_distance_functions.R"))

# getting the species of interest
target_sp <- here("Data", "1_downloadable_studies_deployments_filtered.rds") |> 
  read_rds() |>
  distinct(species) |> 
  as_vector()

# create output directory
target_sp |> 
  map(~{
    
    sp_dir <- here("Data", "Studies", str_replace(.x, " ", "_"))
    out_dir <- here(sp_dir, "6_distances")
    
    if(!dir.exists(out_dir)){ dir.create(out_dir) }
    
  })


# 0 - Parameters for functions --------------------------------------------

# keeping just columns of interest
coi <- c(
  "individual_local_identifier", "sensor_type", "sex", "manipulation_type", 
  "country", "country_admin", "continent", "within_eubb", "study_site"
)
coi_dcp <- c(
  "n_locs", "dist_min", "dist_max", "dist_mean", "dist_median", "t_span"
  )
  
# how do we define day and night (day limits)
day_limits <- list(
  c(day_start = "nightEnd", day_end =  "night"), 
  c(day_start = "nauticalDawn", day_end = "nauticalDusk"), 
  c(day_start = "dawn", day_end = "dusk")
)

# when converting median x and median y to steps use this crs
crs_dcp <- 4326


# Run for all species and day limits --------------------------------------

for(sp in target_sp){
  
  
  # folder with species data
  sp_dir <- here("Data",  "Studies", str_replace(sp, " ", "_"))
  
  # list all deployments
  files <- here(sp_dir, "5_resampled") |> list.files()
  lfl <- length(files)
  
  for(day_lim in day_limits){
    
    # define file names using the selected day limits
    
    dcp_file <- str_c(
      "1_all_tracks_dcp_distances_", 
      day_lim[["day_start"]], 
      "_", 
      day_lim[["day_end"]],
      ".rds"
    )
    
    dcp_night_file <- str_c(
      "1_all_tracks_dcp_night_steps_", 
      day_lim[["day_start"]], 
      "_", 
      day_lim[["day_end"]],
      ".rds"
    )
    
    dcp_day_file <- str_c(
      "1_all_tracks_dcp_day_steps_", 
      day_lim[["day_start"]], 
      "_", 
      day_lim[["day_end"]],
      ".rds"
    )
    
    night_file <- str_c(
      "2_all_tracks_night_steps_", 
      day_lim[["day_start"]],
      "_", 
      day_lim[["day_end"]], 
      ".rds"
    )
    
    day_file <- str_c(
      "3_all_tracks_max_day_steps_", 
      day_lim[["day_start"]],
      "_", 
      day_lim[["day_end"]], 
      ".rds"
    )
    


# 1 - Get distances per day_cycle_period and related steps -------------------


    file_check <- here(
      sp_dir, "6_distances", c(dcp_file, dcp_night_file, dcp_day_file)
      )
    
    if(sum(!file.exists(file_check)) > 0){
      
      # get the dcp file and save it
      dcp_df <- files |> 
        map(~{
          fin <- .x
          print(paste(sp, which(fin == files), "|", lfl))
          
          here(sp_dir, "5_resampled", fin) |> 
            read_rds() |> 
            get_dcp_df(day_lim = day_lim, cols_of_interest = coi) |> 
            mutate(track_file = fin)
          
        }) |> 
        set_names(files)
      
      dcp_df |> 
        list_rbind() |> 
        write_rds(here(sp_dir, "6_distances", dcp_file))
      
      # calculate step length from calculated median locations at day and night
      dcp_steps <- dcp_df |>  
        map(~{
          
          print(paste(sp, which(unique(.x$track_file) == files), "|", lfl))
          
          if(sum(.x$dcp_available == T) > 0){
            
            .x |> 
              separate_wider_delim(
                cols = dcp, delim = "_", names = c("day_cycle", "day_period")
              ) |> 
              make_track(
                x_median, y_median, t_median, crs = crs_dcp, all_cols = T
              ) |> 
              get_night_day_steps(
                get_day_cycle = F, cols_of_interest = coi_dcp
              )
            
          } else{
            .x
          }
          
        }) 
      
      
      rm(dcp_df)
      
      dcp_steps |> 
        map("night_steps") |>
        list_rbind(names_to = "track_file") |>
        select(-starts_with("sl_m")) |> 
        write_rds(here(sp_dir, "6_distances", dcp_night_file))
      
      dcp_steps |> 
        map("day_steps") |>
        list_rbind(names_to = "track_file") |>
        select(-starts_with("sl_m")) |> 
        write_rds(here(sp_dir, "6_distances", dcp_day_file))
      
      rm(dcp_steps)
      gc(verbose = F)
      
    }
    

# 2 - Get night and day steps ---------------------------------------------

    file_check <- here(sp_dir, "6_distances", c(night_file, day_file))
    
    if(sum(!file.exists(file_check)) > 0){
        
         daily_df <- files |> 
           map(~{
             
             fin <- .x
             print(paste(sp, which(fin == files), "|", lfl))
             
             here(sp_dir, "5_resampled", fin) |> 
               read_rds() |>
               get_night_day_steps(day_lim = day_lim, cols_of_interest = coi) 
             
           }) |> 
           set_names(files)
         
         map(daily_df, "night_steps") |>
           list_rbind(names_to = "track_file") |>
           write_rds(here(sp_dir, "6_distances", night_file))
         
         map(daily_df, "day_steps") |>
           list_rbind(names_to = "track_file") |>
           write_rds(here(sp_dir, "6_distances", day_file))
         
         rm(daily_df)
         gc(verbose = F)
          
       }
    
    cat(
      paste(
        "CALCULATION ENDED FOR: \n",
        "- day starts at:",
        day_lim[["day_start"]],
        "\n- day ends at:",
        day_lim[["day_end"]], 
        "\n"
      )
    )
    
  }
  
  cat(paste("\n", sp, "DONE!"))

}

