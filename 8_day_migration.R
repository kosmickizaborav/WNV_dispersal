library(data.table)
source("0_helper_functions.R")

data_dir <- here::here("Data")
study_dir <- file.path(data_dir, "Studies")

min_steps <- 20
regions <- c("World", "Europe")

# DIRECTORIES
data_dir <- here::here("Data")
graph_dir <- file.path(data_dir, "Graphs", "8_day_migration")

dist_dirs <- file.path(
  list.dirs(file.path(study_dir, full.path = T), "6_distances"))

files <- list.files(
  dist_dirs, pattern = "4_all_tracks_max_active_steps_.*continent.rds",
  full.names = TRUE)


# Europe check ------------------------------------------------------------


files <- list.files(
  dist_dirs, pattern = "4_all_tracks_max_active_steps_.*continent.rds",
  full.names = TRUE)


# X - Median active step per month ----------------------------------------

active_dirs <- gsub("5_distances", "8_active_steps", dist_dirs)

invisible(lapply(active_dirs, dir.create, showWarnings = FALSE))

fout_names <- c(
  "1_monthly_max_active_steps.rds", "1_daily_max_active_steps.rds")

step_summary <- function(dt, by = NULL){
  dt[, .(
    min_sl = min(sl_),
    median_sl = median(sl_),
    median_sl = median(sl_),
    max_sl = max(sl_), 
    median_sl_n_steps = as.numeric(median(sl_n_steps)), 
    n_steps = .N, 
    n_tracks = uniqueN(file), 
    n_years = uniqueN(year_), 
    n_country = uniqueN(country),
    countries = paste(unique(country), collapse = "_"), 
    step_type = unique(step_type)
    ), by = by]
}


lapply(seq_along(target_sp), function(n){
  
  sp <- target_sp[n]
  
  sp_files <- grep(gsub(" ", "_", sp), dist_files, value = T)
  out_dir <- unique(gsub("5_distances/.*$", "8_active_steps", sp_files))
  
  fouts <- file.path(out_dir, fout_names)
  
  if(sum(!file.exists(fouts)) > 0){
    
    lapply(fouts, function(fout){
      
      cby <- ifelse(grepl("monthly", fout), "month_", "yd_")
      
      out_sum <- rbindlist(lapply(sp_files, function(fin){
        
        dl <- sub(
          ".*(night_to_day|day_to_night)_([^_]+_[^_]+)_continent\\.rds$", 
          "\\2", fin)
        
        steps <- fread(fin)[n_steps_per_track >= min_steps]
        
        if(nrow(steps) == 0){ return(NULL)}
        
        # get months
        steps[, ':=' (
          month_ = month(day_cycle_1),
          year_ = year(day_cycle_1), 
          yd_ = day_cycle_to_yd(day_cycle_1))]
        
        continent_check <- 
        
        steps_eu <- steps[continent == "Europe"]

        if(nrow(steps_eu) == nrow(steps)){
          
          ss <- step_summary(steps_eu, by = cby)[, region := "Europe"]
          
        } else{
          
          ss <- rbind(
            step_summary(steps, by = cby)[, region := "World"],
            if(nrow(steps_eu) > 0) step_summary(steps_eu, by = cby)[, region := "Europe"] else NULL
          )
          
        }
  
        setorderv(ss, cby)
        
        ss[, day_limits := dl]
        
      }), fill = T)[, species := sp]
      
      fwrite(out_sum, fout)
    
    }) # lapply fouts
    
  } # close if file exists
  
  cat(n, "|", nsp, "-", sp, "DONE!\n")
  
})





lapply(fout_names, function(fout){
  
  files <- list.files(active_dirs, pattern = fout, full.names = T)
  
  dt <- rbindlist(lapply(files, fread), fill = T)
})




  dtn <- ifelse(
    traits[species == sp, nocturnal] == 1, "day_to_night", "night_to_day") 
  
  dist_files <- list.files(
    dist_dir, full.names = TRUE, 
    pattern = paste0("4_all_tracks_max_active_steps_", dn, ".*continent\\.rds$"))
  
  lapply(dist_files, function(fin){
    
    dl <- sub(
      paste0(".*", dtn, "_([^_]+)_([^_]+)_continent\\.rds"), "\\1_\\2", fin)
    
    pout_dir <- grep(dl, out_dirs, value = T)
    
    steps <- fread(fin)[n_steps_per_track >= min_steps]
    
    month_steps <- 
      
      lapply(pout_dir)
    
    
  })



  

# plots -------------------------------------------------------------------

  dir.create(graph_dir, showWarnings = FALSE)
  

  # how do we define day and night (day limits)
  day_limits <- c("nightEnd_night", "nauticalDawn_nauticalDusk", "dawn_dusk")
  
  out_dirs <- file.path(
    graph_dir, rep(regions, each=length(day_limits)), day_limits)
  # Create the directories (including parents)
  invisible(sapply(out_dirs, dir.create, recursive = TRUE, showWarnings = FALSE))  





lapply(target_sp, function(sp){
  
  dist_dir <- file.path(data_dir, "Studies", gsub(" ", "_", sp), "5_distances")
  
  dn <- ifelse(
    traits[species == sp, nocturnal] == 1, "day_to_night", "night_to_day") 
  
  dist_files <- list.files(
    dist_dir, full.names = TRUE, 
    pattern = paste0("4_all_tracks_max_active_steps_", dn, ".*continent\\.rds$"))
  
  lapply(dist_files, function(fin){
    
    dl <- sub(
      paste0(".*", dn, "_([^_]+)_([^_]+)_continent\\.rds"), "\\1_\\2", fin)
    
    pout_dir <- grep(dl, out_dirs, value = T)
    
    steps <- fread(fin)[n_steps_per_track >= min_steps]
    
    month_steps <- 
    
    lapply(pout_dir)
    
    
  })
  

  
  
})


