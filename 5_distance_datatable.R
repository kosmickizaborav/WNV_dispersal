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
library(ggplot2)
library(patchwork)
#library(parallel) - never used it in the end
#load the functions for distance calculations
source("5_distance_datatable_FUNCTIONS.R")
source("0_helper_functions.R")

data_dir <- here::here("Data2")
graph_dir <- file.path(data_dir, "Graphs")

# INPUT
f_resampled_report <- "4_resampled_report.csv"
resampled_tracks <- fread(file.path(data_dir, f_resampled_report))[saved == T]

target_sp <- unique(resampled_tracks$birdlife_name)
nsp <- length(target_sp)

# output directory
dist_dirs <- file.path(
  data_dir, "Studies", gsub(" ", "_", target_sp), "5_distances")

invisible(sapply(dist_dirs, dir.create, showWarnings = F))

# how do we define day and night (day limits)
day_limits <- list(
  c("nightEnd", "night"), 
  c("nauticalDawn", "nauticalDusk"),
  c("dawn", "dusk")
)

# TRAITS
nocturnal_dt <- fread(here::here("Published_data", "BirdFuncDat.txt"))[
  Scientific %in% target_sp][, .(species = Scientific, nocturnal = Nocturnal)][
    , nocturnal := fifelse(species == "Nycticorax nycticorax", 1, nocturnal)]


# 1 - DCP distances -------------------------------------------------------

lapply(seq_along(target_sp), function(n){

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

        return(dcp_dist)

      }), fill = T)

      # save data if exists
      if(nrow(dcp_dist_all) > 0){ fwrite(dcp_dist_all, dcp_dist_filepath) }

    }

  })

  cat(paste("\n", n, "|", nsp, "-", sp, "DONE!"))

})

rm(day_limits, resampled_tracks)

dcp_files <- list.files(
  dist_dirs, "1_all_tracks_dcp_distances_", full.names = T)
dcp_files <- grep("continent", dcp_files, value = T, invert = T)
ndcp <- length(dcp_files)


# 2 - DCP sleep steps ---------------------------------------------------------

lapply(seq_along(dcp_files), function(n){
  
  fin <- dcp_files[n]
  fout <- gsub("1_all_tracks_dcp_distances", "2_all_tracks_dcp_sleep_steps", fin)
  sp <- gsub("_", " ", sub(".*/Studies/([^/]+)/5_distances/.*", "\\1", fin))
  
  if(!file.exists(fout)){
    
    # if the species is nocturnal take day locations otherwise take night locations
    stype <- ifelse(
      nocturnal_dt[species == sp, nocturnal] == 1, "day", "night")
    
    # get only the median location, and day cycle and period
    dcp_tracks <- fread(fin)[day_period == stype][
      , .(x_ = x_median, y_ = y_median, t_ = t_median, day_cycle, day_period, file)]
    
    # split into data for each track
    if(nrow(dcp_tracks) == 0){ 
      dcp_tracks <- data.table(file = fin, x_ = NA, y_ = NA) }
    
    dcp_tracks <- split(dcp_tracks, by = "file")
    
    # get sleep steps for each DCP track
    sleep_steps <- rbindlist(lapply(dcp_tracks, function(dcp_track){
      
      return(get_sleep_steps(dcp_track))
      
    }), idcol = "file", fill = T)
    
    sleep_steps <- sleep_steps[, step_type := stype]
    
    fwrite(sleep_steps, fout)
    
    cat(paste("\n", n, "|", ndcp, "-", sp, "sleep steps done!"))
    
  }
  
})

# 3 - Active steps ---------------------------------------------------------

lapply(seq_along(dcp_files), function(n){

  fin <- dcp_files[n]
  
  # get species name
  sp <- gsub("_", " ", sub(".*/Studies/([^/]+)/5_distances/.*", "\\1", fin))

  # if the species is nocturnal take day locations otherwise take night locations
  ntd <- nocturnal_dt[species == sp, nocturnal] == 0
  stpt <- ifelse(ntd, "night_to_day", "day_to_night")
  
  # get only the median location, and day cycle and period
  dcp_tracks <- fread(fin)[
    , .(x_ = x_median, y_ = y_median, t_ = t_median, day_cycle, day_period, file)]
  # split into data for each track
  dcp_tracks <- split(dcp_tracks, by = "file")
  
  # OUTPUT 1: median sleep to median active
  fout <- gsub(
    "1_all_tracks_dcp_distances", "3_all_tracks_median_active_steps", fin)
  
  if(!file.exists(fout)){
    
    median_active <- rbindlist(lapply(dcp_tracks, function(dcp_track){
      
      return(get_active_steps(dcp_track, night_to_day = ntd))
      
    }), idcol = "file", fill = T)
    
    fwrite(median_active, fout)
    
    rm(median_active, fout)
    
    cat(paste("\n", n, "|", ndcp, "-", sp, "median active steps done!"))
    
  }
  
  # OUTPUT 2: median sleep to max active
  fout <- gsub(
    "1_all_tracks_dcp_distances_", "4_all_tracks_max_active_steps", fin)
  
  # median to max
  if(!file.exists(fout)){
    
    # extract day limits because we need to assign day period to the 
    # re-sampled track again
    dl <- sub(".*dcp_distances_([^_]+)_([^_]+)\\.rds", "\\1 \\2", fin)
    dl <- strsplit(dl, " ")[[1]]
    
    max_active_steps <- rbindlist(lapply(dcp_tracks, function(dcp_track){
      
      tfin <- gsub(
        "/home/nina/R_projects/WNV_dispersal/Data", data_dir, dcp_track$file[1])
      
      # load the re-sampled track
      track <- readRDS(tfin)
      
      max_steps <- get_active_steps(
        track = track, 
        sleep_locs = dcp_track, 
        day_limits = dl, 
        night_to_day = ntd)
      
      rm(track)
      
      return(max_steps)
      
    }), idcol = "file", fill = T)
    
    fwrite(max_active_steps, fout)
    
    rm(max_active_steps, fout)
   
    
    cat(paste("\n", n, "|", ndcp, "-", sp, "max active steps done!"))
     
  }
  
})

rm(nocturnal_dt)

# 4 - Add europe  ---------------------------------------------------------

dist_files <- grep(
  "continent", list.files(dist_dirs, full.names = T), value = T, invert = T)
ndst <- length(dist_files)


lapply(seq_along(dist_files), function(n){
  
  fin <- dist_files[n]
  fout <- gsub(".rds", "_continent.rds", fin)
  
  # get species name
  sp <- gsub("_", " ", sub(".*/Studies/([^/]+)/5_distances/.*", "\\1", fin))
  
  if(!file.exists(fout)){

    # laod steps
    dist_locs <- fread(fin)
    
    # check which column indicates whether the step is available, and only load
    # the ones that have it
    all_cols <- names(dist_locs)
    avlb_col <- grep("available", all_cols, value = T)
    if("x_median" %in% all_cols){
      xy_cols <- c("x_median", "y_median") } else{ xy_cols <- c("x1_", "y1_") }
    x_col <- xy_cols[1]
    
    dist_locs <- dist_locs[get(avlb_col) == T]
    
    if(nrow(dist_locs) > 0){
      
      dist_locs <- dist_locs[!is.na(get(xy_cols[1])) & !is.na(get(xy_cols[2]))][
        , nrow_per_track := .N, by = file]
      
      # dist_locs <- dist_locs[, nrow_per_track := .N, by = file]
      
      dist_locs <- add_worldmap_data(
        dist_locs, align_start = T, coord_cols = xy_cols)
      
      setnames(
        dist_locs, old = c("sovereignt", "admin"), 
        new = c("country", "country_admin"))
      
      # manually modifying some assignments that have Europe as continent
      # because they overlap with the vector of Russian state
      # latitude of the border between Europe and Asia in Russia
      # https://en.wikipedia.org/wiki/Extreme_points_of_Europe Ural mountains
      dist_locs[
        , continent := fifelse(
          continent == "Europe" & country == "Russia" & get(x_col) > 66.618056, 
          "Asia", continent)]
      
      fwrite(dist_locs, fout)
      
      cat(paste("\n", n, "|", ndst, "-", sp, "continents added!"))
      
      }

    }
    

})

rm(dist_files, ndst)

# 5 - Check if only in europe ---------------------------------------------

# file_type <- c(
#   "1_all_tracks_dcp_distances", 
#   "2_all_tracks_dcp_sleep_steps", 
#   "3_all_tracks_median_active_steps", 
#   "4_all_tracks_max_active_steps"
# )

cont_files <- list.files(
  dist_dirs, pattern = ".*_continent.rds", full.names = T)
ncf <- length(cont_files)

regi_dt <- rbindlist(lapply(seq_along(cont_files), function(i){
  
  fin <- cont_files[i]
  
  cat(paste("\n", i, "|", ncf, "DONE!"))
   
  dt <- fread(fin)
  n_total <- nrow(dt)
  n_europe <- sum(dt$continent == "Europe", na.rm = TRUE)
  conts <- unique(dt$continent)
  
  by_track <- dt[, .(
    n_locs = .N, 
    in_eu = sum(continent == "Europe")), 
    by = file]
  by_track <- by_track[, .(
    tracks_in_europe_only = sum(in_eu == n_locs),
    tracks_in_world = .N, 
    median_locs_per_track = median(n_locs))]
  
  cbind(
    data.table(
      n_europe = n_europe,
      n_world = n_total,
      continents = paste(conts, collapse = "_"),
      step_type = unique(dt$step_type), 
      file = fin
    ), 
    by_track)
  
}), fill = T)

regi_dt[, ':=' (
  species = gsub(
    "_", " ", gsub(".*/Studies/([^/]+)/5_distances/.*", "\\1", file)),
  day_limits = sub(".*[distances|steps]_(.*)_continent.*", "\\1", file))]


regi_dt <- rbindlist(lapply(con_t, function(fin) {

  dt <- fread(fin)
  n_total <- nrow(dt)
  n_europe <- sum(dt$continent == "Europe", na.rm = TRUE)

  conts <- unique(dt$continent)

  data.table(
    n_points_europe = n_europe,
    n_points_world = n_total,
    continents = paste(conts, collapse = "_"),
    file = fin
  )

}))

rdt <- copy(regi_dt)[, ':=' (
  species = gsub(".*/Studies/([^/]+)/5_distances/.*", "\\1", file),
  file_name = basename(file),
  day_limit = gsub(
    ".*(steps|night_to_day|day_to_night)_([^_]+_[^_]+)_continent\\.rds$", "\\2",
    file))][
  , step_type := gsub(".*all_tracks_([^_])_steps", "\\1", file_name)]

species = gsub(".*/Studies/([^/]+)/5_distances/.*", "\\1", fin),

dl = sub(
  ".*(night_to_day|day_to_night)_([^_]+_[^_]+)_continent\\.rds$", "\\2",
  fin),

[, species := gsub("_", " ", species) ]

regi_dt <- regi_dt[, only_eu := fifelse( n_points_europe == n_points_world, T, F)]


# # 6 - Plot distances ------------------------------------------------------
# 
# regions <- c("World", "Europe")
# 
# # Main directory
# plots_dir <- file.path(graph_dir, "5_distances")
# 
# # Create folder names from day_limits
# dl_folders <- sapply(day_limits, function(x) paste(x, collapse="_"))
# # Create all combinations
# all_dirs <- file.path(
#   plots_dir, rep(regions, each=length(dl_folders)), dl_folders)
# # Create the directories (including parents)
# invisible(sapply(all_dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
# 
# n_steps_limit <- 10
# dist_limit <- 50
# 
# lapply(seq_along(target_sp), function(n){
#   
#   # get species directory
#   sp <- target_sp[n]
#   dist_dir <- file.path(data_dir, "Studies", gsub(" ", "_", sp), "5_distances")
#   
#   files <- list.files(dist_dirs, "^[234]_.*_continent.rds", full.names = T)
#   
#   lapply(day_limits, function(dl){
#     
#     # OUTPUT file with day-cycle-period (DCP) distances
#     dl_str <- paste(dl, collapse = "_") 
#     dl_files <- grep(dl_str, files, value = T)
#     
#     lapply(c("day_to_night", "night_to_day"), function(ntd){
#       
#       sstart <- sub("_.*", "", ntd)
#       send <- sub(".*_to_", "", ntd)
#       
#       sfin <- grep(paste0("2_all_tracks_dcp_", sub("_.*", "", ntd), "_steps"),
#                    dl_files, value = T)
#       
#       afin <- grep(paste0("4_all_tracks_max_active_steps_", ntd),
#                    dl_files, value = T)
#       
#       if(length(sfin) > 0 & length(afin) > 0){
# 
#         sleep_steps <- fread(file.path(dist_dir, sfin))[
#           , .(file, sl_, continent, day_cycle = day_cycle_1, n_steps_per_track)][
#             , step_type := paste("sleep:", sstart)]
#         
#         active_steps <- fread(file.path(dist_dir, afin))[
#           , .(file, sl_, continent, day_cycle = day_cycle_1, n_steps_per_track)][
#             , step_type := paste("active:", send)]
#         
#         all_steps <- rbindlist(list(sleep_steps, active_steps))[
#           n_steps_per_track >= n_steps_limit][
#             , yd := day_cycle_to_yd(day_cycle)]
#         rm(sleep_steps, active_steps)
#         
#         
#         lapply(regions, function(regi){
#           
#           if(regi == "Europe"){ 
#             for_stats <- all_steps[continent == "Europe"] } else{
#               for_stats <- all_steps }
#           
#           # setNames(
#           # lapply(quants, function(q) quantile(sl_km, q, na.rm = TRUE)), 
#           # paste0("q", qts * 100)),
#           steps_stat <- for_stats[, .(
#             sl_km_median = median(sl_, na.rm = TRUE)/1000,
#             n_steps = .N
#           ),  by = c("yd", "step_type")]
#           rm(for_stats)
#           
#           if(nrow(steps_stat) > 0){
#             
#             step_list <- CJ(
#               yd = 1:366,
#               step_type = c(paste("sleep:", sstart), paste("active:", send)),
#               unique = TRUE)
#             
#             # 4. Merge to ensure all combinations present, missing get NA
#             steps_stat <- steps_stat[step_list, on = .(yd, step_type)][
#               , step_type := factor(
#                 step_type, 
#                 levels = c(paste("sleep:", sstart), paste("active:", send)))][
#                   , n_steps := fifelse(is.na(n_steps), 0, n_steps)]              
#             
#             pout <- plot_median_distance(dt = steps_stat) +
#               plot_annotation(
#                 title = paste(sp, "-", regi), 
#                 subtitle = sprintf(
#                   "sleep: median %s to median %s, active: median %s to maximum %s distance", 
#                   sstart, sstart, sstart, send),
#                 caption = paste0(
#                   "day definition: [", gsub("_", ", ", dl_str), "]"),
#                 theme = theme(
#                   plot.title = element_text(hjust = 0.5), 
#                   plot.subtitle = element_text(hjust = 0.5)
#                 )
#               )
#             
#             pname <- paste0(gsub(" ", "_", sp), "_", ntd, "_steps.png")
#             
#             ggsave(
#               file.path(plots_dir, regi, dl_str, pname),
#               pout, height = 18, width = 18, units = "cm", bg = "white")
#             
#             
#           }
#           
#         })
#         
#         }
#     
#       
#     }) # close day_night
#     
#   }) # close day limits
#   
#   cat(paste0(n, " of ", nsp, ": ", sp, "\n"))
#   
# }) # close species
# 
# 
