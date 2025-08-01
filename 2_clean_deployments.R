#' ---
#' title: "Clean deployments"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' **SECTION 0 - Define parameters and packages**
#'  
#' **SECTION 1 - Label track problems** 
#' we listed the downloaded deployments. using function detect_track_problems() 
#' that encompasses all track problems, we label and save them. 
#' both tracks with labelled problems 
#' and cleaned track were saved. in case animal was tracked
#' with two technologies simultaneously e.g. (SigFox & GPS) the track was split. 
#' summary of track problems were logged in the file file_track_problem.
#'
#'  **SECTION 2 - Detect duplicated deployments**
#'  from the file with cleaned tracks, we check for duplicated deployments. 
#'  specifically we look whether there is duplicated individual local
#'  identifiers or tag local identifiers that overlap in time period of the 
#'  deployment. if so, we exclude the track that has shorter duration. 
#'  

# 0 - Define parameters and packages ---------------------------------------

library(data.table)
library(sf)
library(amt)
source("0_helper_functions.R")
source("2_clean_deployments_FUNCTIONS.R")

# this is the study id from aiguamolls de l'emporda
# E4WarningID <- 4043292285
data_dir <- here::here("Data")
study_dir <- file.path(data_dir, "Studies")

# OUTPUT DIRECTORIES
create_dir(list.files(study_dir), c("2_track_problems", "2_cleaned"))


# 1 - Label problems------------------------------------------------------------

# here we separate deployments by sensor type id because for some species there
# is a simultaneous recording with GPS and SixFox

dep_files <- list.files(
  file.path(study_dir, list.files(study_dir), "1_deployments"), 
  pattern = "dep.rds", full.names = T)
ndp <- length(dep_files)

# find deployments that have not been processed yet
files <- unlist(lapply(seq_along(dep_files), function(i){
  
  cat("\n", i, "|", ndp)
  
  fin <- dep_files[i]
  bfin <- basename(fin)
  out_dir <- gsub("1_deployments/.*", "2_track_problems", fin)
  
  if(length(list.files(out_dir, pattern = gsub(".rds", "", bfin))) == 0){
    return(fin) } else { return(NULL) }
  
}))
nf <- length(files)

rm(dep_files, ndp)

if(nf > 0){
  
  # columns needed to detect track problems
  cols_gps <- c(
    "timestamp", "geometry", "gps_hdop", "gps_vdop", "gps_satellite_count", 
    "sensor_type_id"
  )
  
  invisible(lapply(seq_along(files), function(i){
    
    # get the file name
    fin <- files[i]
    bfin <- basename(fin)
    out_dir <- gsub("1_deployments/.*", "2_track_problems", fin)
    
    track <- readRDS(fin) 
    track_attr <- attr(track, "track_data")
    
    deploy_on <- if("deploy_on_timestamp" %in% colnames(track_attr)) track_attr$deploy_on_timestamp else NA
    deploy_off <- if("deploy_off_timestamp" %in% colnames(track_attr)) track_attr$deploy_off_timestamp else NA
    
    track <- track |> 
      mutate(n_na = rowSums(is.na(pick(everything())))) |>
      arrange(timestamp) |> 
      select(any_of(c(cols_gps, "n_na"))) |> 
      mutate(
        x = st_coordinates(geometry)[, 1],
        y = st_coordinates(geometry)[, 2]
      ) |> 
      st_drop_geometry() |> 
      units::drop_units() |> 
      as.data.table() |> 
      detect_track_problems(
        study_id = track_attr$study_id,
        deploy_on_time = deploy_on,
        deploy_off_time = deploy_off, 
        dop_threshold = 5, 
        cols_out = c("x", "y", "timestamp", "sensor_type_id")
      ) 
    
    # if there is multiple sensor types, split the track and save separately
    track_split <- split(track, by = "sensor_type_id")
    
    rm(track, track_attr)
    
    lapply(track_split, function(t){
      
      sens <- unique(t$sensor_type_id)
      
      # file path for the labeled track problems
      fout <- file.path(
        out_dir, gsub(".rds", paste0("_", sens, "_sen.rds"), bfin))
      
      fwrite(t, fout)
      
      return(NULL)
      
    })
    
    rm(track_split, sens, fout, fin, out_dir, deploy_on, deploy_off)
    gc(verbose = F)
    
    cat("\nTrack checked for problems: ", i, " | ", nf)
    
  }))
  

}



# 2 - Clean tracks ------------------------------------------------------------

# OUTPUT
file_track_problem <- "2_track_problems_report.csv"

if(!file.exists(file.path(data_dir, file_track_problem))){
  
  prob_files <- list.files(
    file.path(study_dir, list.files(study_dir), "2_track_problems"), 
    full.names = T)
  nf <- length(prob_files)
  
  probs_cleaned <- rbindlist(lapply(seq_along(prob_files), function(i){
    
    fin <- prob_files[i]
    fout <- gsub("2_track_problems", "2_cleaned", fin)
    
    track <- fread(fin)
    
    # create track problem summary, clean the track and save it
    problem_summary <- summarize_track_problems(
      track = track, cols_cleaned_track = c("x", "y", "timestamp")
    )
    
    track <- track[track_problem %in% c(NA, "")][
      , track_problem := NULL][, sensor_type_id := NULL]
    
    if(nrow(track) > 3){ 
      
      fwrite(track, fout)
      
      } else{ 
        
        fout <- gsub(".rds", "_nodata.rds", fout)
        fwrite(problem_summary, fout)
        
      }
    
    problem_summary[, file := fout][
      , species := gsub(".*/Studies/(.*?)/2_track_problems/.*", "\\1", fin)]
    
    rm(track, fin, fout)
    #gc(verbose = F)
    
    cat("\nTrack cleaned:", i, "|", nf)
    
    return(problem_summary)
    
  }), fill = T)
  
  probs_cleaned[, species  := gsub("_", " ", species)]
  
  fwrite(probs_cleaned, file.path(data_dir, file_track_problem))
  
  
}

# 2 - Detect duplicated deployments ---------------------------------------

# OUTPUT
file_deploy_clean <- "2_deployments_cleaned.csv"

deployments <- fread(file.path(data_dir, "1_deployments_to_download.csv"))
deployments[
  , file_org := make_file_name(study_id, individual_id, deployment_id)]
deployments <- deployments[, .(
  individual_id, individual_local_identifier, tag_id, tag_local_identifier, 
  file_org, study_id, deployment_id)]

# load the list of cleaned tracks
cleaned_tracks <- fread(file.path(data_dir, file_track_problem))
# check which files have data
cleaned_tracks[, saved := !grepl("nodata", file)]

cleaned_tracks <- cleaned_tracks[track_problem %in% c(NA, "") & saved == T][
  , bfile := basename(file)][
  , file_org := gsub("(_dep)[^/\\\\]*?(\\.rds)", "\\1\\2", bfile)][
  , sensor_type_id := bit64::as.integer64(
    gsub(".*_(\\d+)_sen\\.rds", "\\1", file))]

# merge deployment info
cleaned_tracks <- merge(cleaned_tracks, deployments, by = "file_org", all.x = T)

# if there individual local identifier is not provided check individual id
cleaned_tracks[
  individual_local_identifier %in% c("", NA), 
  individual_local_identifier := individual_id
]

# if tag local identifier is not provided check tag id
cleaned_tracks[
  tag_local_identifier %in% c("", NA), 
  tag_local_identifier := tag_id
]

# CHECK if duplicated individual local identifiers overlap in time 
cleaned_tracks <- rbindlist(lapply(
  split(
    cleaned_tracks, 
    by = c("individual_local_identifier", "species", "sensor_type_id")
  ), 
  function(x) {
    find_duplicated_tracks(
      track_df = x, 
      start = "track_start",, 
      end = "track_end", 
      duration = "track_unique_days", 
      keep_criteria = "duration"
      )}
))
setnames(cleaned_tracks, old = "to_exclude", new = "dup_ind_to_exclude")

# CHECK if duplicated tag local identifiers overlap in time 
cleaned_tracks <- rbindlist(lapply(
  split(
    cleaned_tracks, 
    by = c("tag_local_identifier", "species", "sensor_type_id")
  ), 
  function(x) {
    find_duplicated_tracks(
      track_df = x, 
      start = "track_start",, 
      end = "track_end", 
      duration = "track_unique_days", 
      keep_criteria = "duration"
    )}
))

setnames(cleaned_tracks, old = "to_exclude", new = "dup_tag_to_exclude")

cleaned_tracks <- cleaned_tracks[
  , (c("track_problem", "file_org", "saved")) := NULL][ 
  , excluded := dup_ind_to_exclude | dup_tag_to_exclude]

fwrite(cleaned_tracks, file.path(data_dir, file_deploy_clean))

