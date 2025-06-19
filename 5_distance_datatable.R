#' ---
#' title: "Get daily distances"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' using different definitions of day and night periods, this script calculates
#' the distances during each period. day cycle is defined as the period between
#' day start of one day until the day start of the next day, e.g. from dawn to 
#' dawn of the next day. day period is defined as night if the timestamp falls
#' between the end of the day and the start of the next one (e.g. dusk to dawn);
#' day is defined as the period between the start of the day and the end of the
#' day (e.g. dawn to dusk).
#' 
#'  **SECTION 1 - Distances per day period - DCP distances**
#' splits the track into day-cycle-periods (DCP) and calculates distances between all
#' points available in each period, as well as the median point for each period.
#' the median of these positions are then used to calculate the distances
#' between the sleeping locations and the median active locations. 
#' 
#' **SECTION 2 - Sleep steps - DCP**
#' because there are some nocturnal species in the dataset, we calculated the 
#' steps between sleeping positions for both night and days. sleeping positions
#' are represented as the median position per day period (for nocturnal species
#' we are going to use the day, and for diurnal night). these median position
#' come from the calculations done in section 1
#' 
#'  **SECTION 2 - Active steps - DCP -> DCP; DCP -> track**
#'  to calculate the active steps, we use the sleeping poistion as the starting 
#'  point of the day, and then calculated two types of distances: 
#'   1 - sleep position to median position of the active period (e.g. for 
#'       diurnal species it would be from a median night poisiton to the median 
#'       day position)
#'   2 - sleep position to all the position available during the day period, 
#'       with extraction of the point that produces the biggest distance, 
#'       and preservation of minimum, median and mean values of the distances
#'       calculated (e.g. for diurnal species, it would be from the median 
#'       night poison to all the day position available, preserving the point 
#'       that constitutes the maximum distance)
#'       
#' **SECTION 3 - Add continent/country information**
#' for each exported dataset, continent and country fields are added using
#' spatial overlays. only steps with sufficient data are retained, for 
#' easier calculation later. 

# 0 - Load packages -------------------------------------------------------

library(data.table)
#library(parallel) - never used it in the end
#load the functions for distance calculations
source("5_distance_datatable_FUNCTIONS.R")
source("0_helper_functions.R")

data_dir <- here::here("Data")

# INPUT
f_resampled_report <- "4_resampled_report.csv"
resampled_tracks <- fread(file.path(data_dir, f_resampled_report))[saved == T]

target_sp <- unique(resampled_tracks$birdlife_name)
nsp <- length(target_sp)

create_dir(unique(resampled_tracks$birdlife_name), new_dir = "5_distances")

# how do we define day and night (day limits)
day_limits <- list(
  c("nightEnd", "night"), 
  c("nauticalDawn", "nauticalDusk"),
  c("dawn", "dusk")
)

# 1 - DCP distances -------------------------------------------------------

invisible(lapply(seq_along(target_sp), function(n){
  
  sp <- target_sp[n]
  # get available tracks for the species
  files <- resampled_tracks[birdlife_name == sp, file]
  sp_dir <- file.path(data_dir, "Studies", gsub(" ", "_", sp))
  lfl <- length(files)
  
  lapply(day_limits, function(dl){
    
    # OUTPUT file with day-cycle-period (DCP) distances
    dcp_dist_file <- paste0(
      "1_all_tracks_dcp_distances_", dl[1], "_", dl[2], ".rds") 
    dcp_dist_filepath <- file.path(sp_dir, "5_distances", dcp_dist_file)
    
    if(!file.exists(dcp_dist_filepath)){
      
      dcp_dist_all <- rbindlist(lapply(seq_along(files), function(i){
        
        # load the track 
        fin <- files[i]
        track <- readRDS(fin)
        
        # calculate DCP distances and median DCP positions
        dcp_dist <- get_dcp_dist(track, day_limits = dl)[, file := fin]
        
        cat(sprintf("\n %s - %s, %s - Processed track %d | %d!", 
            sp, dl[1], dl[2], i, lfl))
        
        return(dcp_dist)
        
      }), fill = T)
      
      # save data if exists
      if(nrow(dcp_dist_all) > 0){ fwrite(dcp_dist_all, dcp_dist_filepath) }
      
    }
    
  })
  
})
)


# 2 - DCP sleep steps ---------------------------------------------------------

lapply(seq_along(target_sp), function(n){
  
  # get species directory
  sp <- target_sp[n]
  dist_dir <- file.path(data_dir, "Studies", gsub(" ", "_", sp), "5_distances")
  
  # load DCP file created in section 1 - sleep positions
  dcp_dist_files <- grep(
    "1_all_tracks_dcp_distances_", list.files(dist_dir), value = T)
  
  # repeat the process for all dcp files
  lapply(seq_along(dcp_dist_files), function(i){
    
    fin <- dcp_dist_files[i]
    
    # OUTPUT files:
    # one for median night steps (diurnal species)
    # one for median day steps (nocturnal species)
    nfout <- gsub(
      "1_all_tracks_dcp_distances_", "2_all_tracks_dcp_night_steps_", fin)
    dfout <- gsub(
      "1_all_tracks_dcp_distances_", "2_all_tracks_dcp_day_steps_", fin)
    
    if(sum(!file.exists(file.path(dist_dir, c(nfout, dfout)))) > 0){
      
      # get only the median location, and day cycle and period
      dcp_dist_all <- fread(file.path(dist_dir, fin))[
        , .(x_ = x_median, y_ = y_median, t_ = t_median, 
            day_cycle, day_period, file)]
      # split into data for each track
      dcp_dist_all <- split(dcp_dist_all, by = "file")
      
      # get sleep steps for each DCP track
      dcp_steps <- rbindlist(lapply(dcp_dist_all, function(dcp_dist){
        
        return(get_sleep_steps(dcp_dist))
        
      }), idcol = "file", fill = T)
      
      fwrite(dcp_steps[day_period == "day"], file.path(dist_dir, dfout))
      fwrite(dcp_steps[day_period == "night"], file.path(dist_dir, nfout))
      
    }
      
  })
  
  cat(sprintf("\n %s - DCP steps done! - %d | %d!", sp, n, nsp))
  
})


# 3 - Active steps ---------------------------------------------------------

lapply(seq_along(target_sp), function(n){
  
  # get species directory
  sp <- target_sp[n]
  dist_dir <- file.path(data_dir, "Studies", gsub(" ", "_", sp), "5_distances")
  # load DCP file created in section 1 - sleep positions
  dcp_dist_files <- grep(
    "1_all_tracks_dcp_distances_", list.files(dist_dir), value = T)
  
  # repeat the process for all dcp files
  lapply(seq_along(dcp_dist_files), function(i){
    
    fin <- dcp_dist_files[i]

    # load median sleep locations
    dcp_dist_all <- fread(file.path(dist_dir, fin))[
      , .(x_ = x_median, y_ = y_median, t_ = t_median, 
          day_cycle, day_period, file)]
    
    dcp_dist_all <-  split(dcp_dist_all, by = "file")
    
    # because we have nocturnal and diurnal species, we need to calculate
    # the distance from day to night and night to day, so we first control for 
    # that
    lapply(c(T, F), function(ntd){
      
      # whether it's done for night to day or day to night
      stpt <- ifelse(ntd, "night_to_day_", "day_to_night_")
      
      # OUTPUT 1: median sleep to median active
      fout <- gsub(
        "1_all_tracks_dcp_distances_", 
        paste0("3_all_tracks_dcp_active_steps_", stpt), fin)
      
      # median to median
      if(!file.exists(file.path(dist_dir, fout))){
        
        dcp_active_steps <- rbindlist(lapply(dcp_dist_all, function(dcp_track){

          return(get_active_steps(dcp_track, night_to_day = ntd))
          
        }), idcol = "file", fill = T)
        
        fwrite(dcp_active_steps, file.path(dist_dir, fout))
        
        rm(dcp_active_steps)
        
      }
      
      # OUTPUT 2: median sleep to max active
      foutm <- gsub(
        "1_all_tracks_dcp_distances_", 
        paste0("4_all_tracks_max_active_steps_", stpt), fin)
      
      # median to max
      if(!file.exists(file.path(dist_dir, foutm))){
        
        # extract day limits because we need to assign day period to the 
        # re-sampled track again
        dl <- sub(".*dcp_distances_([^_]+)_([^_]+)\\.rds", "\\1 \\2", fin)
        dl <- strsplit(dl, " ")[[1]]
        
        max_active_steps <- rbindlist(lapply(dcp_dist_all, function(dcp_track){
          
          # load the re-sampled track
          track <- readRDS(dcp_track$file[1])
          
          max_steps <- get_active_steps(
            track = track, 
            sleep_locs = dcp_track, 
            day_limits = dl, 
            night_to_day = ntd)
          
          rm(track)
          
          return(max_steps)
          
        }), idcol = "file", fill = T)
        
        fwrite(max_active_steps, file.path(dist_dir, foutm))
        
      }

      }) # close ntd
   }) # close dcp_dist_files
      
  cat(sprintf("\n %s - Active steps done! - %d | %d!", sp, n, nsp))
  
})


# # 5 - Add europe  ---------------------------------------------------------


lapply(seq_along(target_sp), function(n){

  # get species directory
  sp <- target_sp[n]
  dist_dir <- file.path(data_dir, "Studies", gsub(" ", "_", sp), "5_distances")
  
  # get all the files that don't have already continent assigned
  files <- grep(
    "continent", grep("^[234]_", list.files(dist_dir), value = T),
    value = T, invert = T)

  lapply(seq_along(files), function(i){

    # INPUT & OUTPUT files
    fin <- files[i]
    fout <- gsub(".rds", "_continent.rds", fin)

    # laod steps
    steps <- fread(file.path(dist_dir, fin))
    
    # check which column indicates whether the step is available, and only load
    # the ones that have it
    step_available_col <- grep("available", names(steps), value = T)
    
    steps <- steps[get(step_available_col) == T][
      , n_steps_per_track := .N, by = file]


    if(nrow(steps) > 0){

      day_cycle_col <- grep("day_cycle_1$|day_cycle$", names(steps), value = T)

      steps <- add_worldmap_data(steps, align_start = T)

      names(steps)[
        names(steps) %in% c("sovereignt", "admin")] <- c("country", "country_admin")

      fwrite(steps, file.path(dist_dir, fout))

    }

    cat(sprintf("\n %s - Added continent! - %d | %d!", sp, n, nsp))

  })

})

