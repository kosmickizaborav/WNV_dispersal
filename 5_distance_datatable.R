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

library(data.table)
#library(parallel) - bever ysed ut ub tge ebd, 
#load the functions for distance calculations
source("5_distance_datatable_FUNCTIONS.R")
source("0_helper_functions.R")

data_dir <- here::here("Data")
# INPUT
file_resampled_report <- "4_resampled_report.csv"

resampled_tracks <- fread(file.path(data_dir, file_resampled_report))[saved == T]

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
  files <- resampled_tracks[birdlife_name == sp, file]
  sp_dir <- file.path(data_dir, "Studies", gsub(" ", "_", sp))
  lfl <- length(files)
  
  lapply(day_limits, function(dl){
    
    # dcp dist
    dcp_dist_file <- paste0(
      "1_all_tracks_dcp_distances_", dl[1], "_", dl[2], ".rds") 
    dcp_dist_filepath <- file.path(sp_dir, "5_distances", dcp_dist_file)
    
    if(!file.exists(dcp_dist_filepath)){
      
      dcp_dist_all <- rbindlist(lapply(seq_along(files), function(i){
        
        fin <- files[i]
        
        track <- readRDS(fin)
        
        dcp_dist <- get_dcp_dist(track, day_limits = dl)[, file := files[i]]
        
        cat(sprintf("\n %s - %s, %s - Processed track %d | %d!", sp, dl[1], dl[2], i, lfl))
        
        return(dcp_dist)
        
      }), fill = T)
      
      if(nrow(dcp_dist_all) > 0){ fwrite(dcp_dist_all, dcp_dist_filepath) }
      
    }
    
  })
  
})
)


# 2 - Night steps ---------------------------------------------------------

lapply(seq_along(target_sp), function(n){
  
  sp <- target_sp[n]
  
  dist_dir <- file.path(data_dir, "Studies", gsub(" ", "_", sp), "5_distances")
  
  dcp_dist_files <- grep(
    "1_all_tracks_dcp_distances_", list.files(dist_dir), value = T)
  
  lapply(seq_along(dcp_dist_files), function(i){
    
    fin <- dcp_dist_files[i]
    fout <- gsub(
      "1_all_tracks_dcp_distances_", "2_all_tracks_dcp_night_steps_", fin)
    fout_path <- file.path(dist_dir, fout)
    
    if(!file.exists(fout_path)){
      
      dcp_dist_all <- fread(file.path(dist_dir, fin))
      dcp_dist_all <- split(dcp_dist_all, by = "file")
      
      dcp_night_steps <- rbindlist(lapply(dcp_dist_all, function(dcp_dist){
        
        dcp_track <- dcp_dist[
          , .(x_ = x_median, y_ = y_median, t_ = t_median, day_cycle, day_period)]
        
        return(get_night_steps(dcp_track))
        
      }), idcol = "file", fill = T)
      
      fwrite(dcp_night_steps, fout_path)
      
    }
      
  })
  
  cat(sprintf("\n %s - Night steps done! - %d | %d!", sp, n, nsp))
  
})


# 3 - Day steps -----------------------------------------------------------


lapply(seq_along(target_sp), function(n){
  
  sp <- target_sp[n]
  dist_dir <- file.path(data_dir, "Studies", gsub(" ", "_", sp), "5_distances")
  
  dcp_dist_files <- grep("1_all_tracks_dcp_distances_", list.files(dist_dir), value = T)
  
  lapply(seq_along(dcp_dist_files), function(i){
    
    fin <- dcp_dist_files[i]
    fout <- gsub(
      "1_all_tracks_dcp_distances_", "3_all_tracks_dcp_day_steps_", fin)
    fout_path <- file.path(dist_dir, fout)
    
    if(!file.exists(fout_path)){
      
      dcp_dist_all <- fread(file.path(dist_dir, fin))
      dcp_dist_all <- split(dcp_dist_all, by = "file")
      
      dcp_night_steps <- rbindlist(lapply(dcp_dist_all, function(dcp_dist){
        
        dcp_track <- dcp_dist[
          , .(x_ = x_median, y_ = y_median, t_ = t_median, day_cycle, day_period)]
        
        return(get_day_steps(dcp_track))
        
      }), idcol = "file", fill = T)
      
      fwrite(dcp_night_steps, file.path(dist_dir, fout))
      
    }
    
  })
  
  cat(sprintf("\n %s - Day steps done! - %d | %d!", sp, n, nsp))
  
})


# 4 - Day steps max -----------------------------------------------------------


lapply(seq_along(target_sp), function(n){
  
  sp <- target_sp[n]
  sp_dir <- file.path(data_dir, "Studies", gsub(" ", "_", sp))
  
  files <- resampled_tracks[birdlife_name == sp, file]
  lfl <- length(files)
  
  
  lapply(day_limits, function(dl){
    
    # dcp dist
    dcp_dist_filepath <- file.path(sp_dir, "5_distances", paste0(
      "1_all_tracks_dcp_distances_", dl[1], "_", dl[2], ".rds"))
    night_max_filepath <- gsub(
      "1_all_tracks_dcp_distances_", "4_all_tracks_dcp_max_day_steps_", 
      dcp_dist_filepath)
    
    if(!file.exists(night_max_filepath)){
      
      dcp_dist_all <- fread(dcp_dist_filepath)[day_period == "night"][
        , .(x_ = x_median, y_ = y_median, t_ = t_median, day_cycle, day_period, file)]
      
      if(file.exists(dcp_dist_filepath)){
        
        night_steps_max_all <- rbindlist(lapply(seq_along(files), function(i){
          
          fin <- files[i]
          
          night_locs <- dcp_dist_all[file == fin]
          track <- readRDS(fin)
          
          night_steps_max <- get_day_steps(
            track, night_locs = night_locs, day_limits = dl)[, file := fin]
          
          return(night_steps_max)
          
        }), fill = T)
        
        if(nrow(night_steps_max_all) > 0){ 
          fwrite(night_steps_max_all, night_max_filepath) }
        
      }
      
    }
    
    cat(sprintf(
      "\n %s [%s-%s] - Maximum day steps done! - %d | %d!", sp, dl[1], dl[2], n, nsp))
    
  })
  

  
})



# 5 - Add europe  ---------------------------------------------------------


lapply(seq_along(target_sp), function(n){
  
  sp <- target_sp[n]
  
  dist_dir <- file.path(data_dir, "Studies", gsub(" ", "_", sp), "5_distances")
  files <- grep(
    "continent", grep("^[234]_", list.files(dist_dir), value = T), 
    value = T, invert = T)
  
  lapply(seq_along(files), function(i){
    
    fin <- files[i]
    
    fout <- gsub(".rds", "_continent.rds", fin)
    
    steps <- fread(file.path(dist_dir, fin))
    step_available_col <- grep("available", names(steps), value = T)
    
    steps <- steps[get(step_available_col) == T][
      , n_steps := .N, by = file][n_steps >= 10]
    
    
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

