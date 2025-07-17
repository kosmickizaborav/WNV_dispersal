
library(data.table)
library(sf)
library(amt)
source("0_helper_functions.R")
source("2_clean_deployments_FUNCTIONS.R")

# this is the study id from aiguamolls de l'emporda
# E4WarningID <- 4043292285
data_dir <- here::here("Data")
study_dir <- file.path(data_dir, "Studies")


dep_files <- list.files(
  file.path(study_dir, list.files(study_dir), "1_deployments"), 
  pattern = "dep.rds", full.names = T)
ndp <- length(dep_files)

check <- rbindlist(lapply(seq_along(dep_files), function(i){
  cat("\n", i, "|", ndp)
  
  # get the file name
  fin <- dep_files[i]
  bfin <- basename(fin)
  out_dir <- gsub("1_deployments/.*", "2_track_problems", fin)
  
  track <- readRDS(fin) 
  
  track <- trackorg |> 
    mutate(n_na = rowSums(is.na(pick(everything())))) |>
    arrange(timestamp) |> 
    mutate(
      x = st_coordinates(geometry)[, 1],
      y = st_coordinates(geometry)[, 2]
    ) |> 
    st_drop_geometry() |> 
    units::drop_units() |> 
    as.data.table() |> 
    select(x, y, timestamp, sensor_type_id)
  
  # if there is multiple sensor types, split the track and save separately
  track_split <- split(track, by = "sensor_type_id")
  
 outout <-  rbindlist(lapply(track_split, function(t){
    
    sens <- bit64::as.integer64(unique(t$sensor_type_id))
    
    # file path for the labeled track problems
    fout <- file.path(
      out_dir, gsub(".rds", paste0("_", sens, "_sen.rds"), bfin))
    
    track_problem <- fread(fout)
    
    out_dt <- data.table(
      n_org = nrow(t), 
      n_prob = nrow(track_problem),
      timestamp_check = sum(t$timestamp == track_problem$timestamp), 
      x_check = all.equal(t$x, track_problem$x),
      y_check = all.equal(t$y, track_problem$y), 
      fout = fout, 
      fin = fin
      )
    
    return(out_dt)
    
    
  }))
 
 rm(track, track_split)
 
 return(outout)

}), fill = T)


fwrite(check, file.path(data_dir, "check_track_problems_new.csv"))



trial <- fread(file.path(data_dir, "check_track_problems_new.csv"))


check <- trial[, oki := n_org == n_prob & 
                    timestamp_check == n_org & 
                    x_check == n_org & 
                    y_check == n_org, 
                by = .(fout, fin)]

check <- check[oki == F | is.na(oki)]

false <- lapply(seq(nrow(check)), function(i){
  cat(i, "\n")
  fin <- check$fin[i]
  bfin <- basename(fin)
  out_dir <- gsub("1_deployments/.*", "2_track_problems", fin)
  
  track <- readRDS(fin) 
  
  track <- track |> 
    mutate(n_na = rowSums(is.na(pick(everything())))) |>
    arrange(timestamp) |> 
    mutate(
      x = st_coordinates(geometry)[, 1],
      y = st_coordinates(geometry)[, 2]
    ) |> 
    st_drop_geometry() |> 
    units::drop_units() |> 
    as.data.table() |> 
    select(x, y, timestamp, sensor_type_id)
  
  # if there is multiple sensor types, split the track and save separately
  track_split <- split(track, by = "sensor_type_id")
  
  outout <-  rbindlist(lapply(track_split, function(t){
    
    sens <- bit64::as.integer64(unique(t$sensor_type_id))
    
    # file path for the labeled track problems
    fout <- file.path(
      out_dir, gsub(".rds", paste0("_", sens, "_sen.rds"), bfin))
    
    track_problem <- fread(fout)
    
    out_dt <- data.table(
      n_org = nrow(t), 
      n_prob = nrow(track_problem),
      timestamp_check = sum(as.POSIXct(t$timestamp) == as.POSIXct(track_problem$timestamp)), 
      x_check = all.equal(t$x, track_problem$x),
      y_check = all.equal(t$y, track_problem$y), 
      fout = fout, 
      fin = fin
    )
    
    return(out_dt)
    
    
  }))
  
  rm(track, track_split)
  
  return(outout)
  
})
      