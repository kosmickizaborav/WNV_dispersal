#' ---
#' title: "Find sleeping spots"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

# 0 - Load packages -------------------------------------------------------

library(data.table)
library(sf)
library(dbscan)
library(ggplot2)
library(patchwork)
library(ggbreak)
#source("6_distance_FUNCTIONS.R")
source("6_distance_PLOT_FUNCTIONS.R")
source("0_helper_functions.R")

# OUTPUT directories -------------------------------------------------------

# MAIN DATA DIRECTORY
data_dir <- here::here("Data")
study_dir <- file.path(data_dir, "Studies")
graphs_dir <- file.path(data_dir, "Graphs")


overview <- fread(list.files(
  data_dir, "6_overview_filter_30_max.*_continent.csv", full.names = T))

# INPUT variables --------------------------------------------------------------

# GET WHETHER THE ANIMAL IS NOCTURNAL OR DIURNAL FOR THE SLEEP SPOTS
target_sp <- gsub("_", " ", list.files(study_dir))

traits_dt <- fread(here::here("Published_data", "00_bird_traits.csv"))[
  birdlife_name %in% target_sp]
# there were duplicates because of some synonyms
traits_dt <- unique(traits_dt[, .(birdlife_name, nocturnal, migration_txt)])
rm(target_sp)

depl_dt <- fread(file.path(data_dir, "2_deployments_cleaned.csv"))
depl_dt <- depl_dt[, .(study_id, individual_local_identifier, bfile)]

# coordinate system to use when calculating distances
crs <- st_crs(4326)


# 1 - Populations -----------------------------------------

fin_name <- "4_all_tracks_max_active_steps_nauticalDawn_nauticalDusk_continent.rds"

# CALCULATE DISTANCES BETWEEN SLEEPING POSITIONS


target_sp <- overview[N_deployments > 1, species]
dist_dirs <- file.path(study_dir, gsub(" ", "_", target_sp), "6_distances")


# listing all dcp files
files <- list.files(dist_dirs, fin_name, full.names = T)

lapply(seq_along(files), function(i){
  
  fin <- files[i]
  
  dt <- fread(fin)
  
  # filter tracks that have less than 10 steps
  dt <- dt[, n_steps := .N, by = file][n_steps >= n_fltr]
  
  if(nrow(dt) == 0){ return(NULL) }
  
  # extract sensor
  dt[, sensor := sub(".*dep_(.*?)_sen.*", "\\1", file)][
    , sensor := fcase(sensor == 653, "gps", sensor == 2299894820, "sigfox")]
  
  # get track lengths and sensor
  dt_per_file <- dt[, .(sensor = unique(sensor)), by = file]
  
  # remove duplicated trakcs - deployments with both gps and sigfox
  dt_per_file[, track_id := sub("_dep_\\d+.*$", "", basename(file))]
  
  setorder(dt_per_file, sensor)
  
  dt_per_file[, duplicated_track := duplicated(track_id)]
  dpltrks <- dt_per_file[duplicated_track == T, file]
  
  dt <- dt[!file %in% dpltrks]
  setorder(dt, file, t1_)
  
  first <- dt[, .(x_ = x1_[1], y_ = y1_[1]), by = file]
  
  plot_on_world_map(first, as_steps = F, color_by = "file")
  
  
  st_first <- sf::st_as_sf(first, coords = c("x_", "y_"), crs = crs)
  dist <- sf::st_distance(st_first)
  dist[lower.tri(dist, diag = T)] <- NA 
  
  hist(dist)
  
})

