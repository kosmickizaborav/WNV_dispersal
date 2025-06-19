#' ---
#' title: "Clean deployments"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' **SECTION 0 - Define parameters and packages**
#' in this section we define variables that are used later in the script:
#' - target_sp - extracted from "1_deployments_download_report.rds"
#' - cols_gps - columns that are used for cleaning process
#' - create output directory for cleaned data
#'  
#' **SECTION 1 - Label and clean track problems** 
#' using file specified as file_dep_filter, we listed the downloaded 
#' deployments. using function detect_track_problems() that encompasses all 
#' track problems, we label and save them. both tracks with labelled problems 
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
source("2_clean_deployments_datatable_FUNCTIONS.R")

# this is the study id from aiguamolls de l'emporda
E4WarningID <- 4043292285
data_dir <- here::here("Data")
study_dir <- file.path(data_dir, "Studies")

# INPUT
file_dep_filter <- "1_deployments_to_download.csv"
# OUTPUT
file_track_problem <- "2_track_problems_report.csv"
file_deploy_clean <- "2_deployments_cleaned.csv"


# DEPLOYMENTS to process
deployments <- fread(file.path(data_dir, file_dep_filter))[
  , file := make_file_name(study_id, individual_id, deployment_id)
  ]

# target species
target_sp <- unique(deployments$birdlife_name)

# OUTPUT DIRECTORIES
create_dir(target_sp, c("2_track_problems", "2_cleaned"))


# 1 - Label and clean track problems-------------------------------------------

# here we separate deployments by sensor type id because for some species there
# is a simultaneous recording with GPS and SixFox

if(!file.exists(file.path(data_dir, file_track_problem))){
  
  # columns needed to detect track problems
  cols_gps <- c(
    "timestamp", "geometry", "gps_hdop", "gps_vdop", "gps_satellite_count", 
    "sensor_type_id"
  )
  
  # list all downloaded deployments
  deploy_problems <- copy(deployments)[
    , fin_path := get_file_path(file, "1_deployments", birdlife_name)
  ][file.exists(fin_path)][, fin_path]
  
  i_total <- length(deploy_problems)
  
  # generated the summary of cleaned tracks - including the numeber of
  # location with problems as well as the cleaned locations
  cleaned_tracks <- rbindlist(lapply(seq_along(deploy_problems), function(i){
    
    fin <- deploy_problems[i]
    
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
    
    track_split <- split(track, by = "sensor_type_id")
    
    rm(track, track_attr)
    
    problems_cleaned <- rbindlist(lapply(track_split, function(t){
      
      # file path for the labeled track problems
      fout_problem <- gsub(
        ".rds", 
        paste0("_", unique(t$sensor_type_id), "_sen.rds"),
        gsub("1_deployments", "2_track_problems", fin)
      )
      
      fwrite(t, fout_problem)
      
      # file path for the cleaned track
      fout_cleaned <- gsub("2_track_problems", "2_cleaned", fout_problem)
      
      # create track problem summary, clean the track and save it
      problem_summary <- summarize_track_problems(
        track = t, 
        fout = fout_cleaned, 
        cols_cleaned_track = c("x", "y", "timestamp")
      )
      
      
      return(problem_summary)
      
    }))
    
    rm(track_split)
    
    message("\nTrack checked for problems: ", i, " | ", i_total)
    
    return(problems_cleaned)
    
  }))
  
  fwrite(cleaned_tracks, file.path(data_dir, file_track_problem))
  
}

# 2 - Detect duplicated deployments ---------------------------------------

# load the list of cleaned tracks
cleaned_tracks <- fread(file.path(data_dir, file_track_problem))[
  track_problem %in% c(NA, "") & saved == T]

# get the original file name from download
cleaned_tracks[
  , original_file := paste0(
    tstrsplit(
      tstrsplit(file, "2_cleaned/", fixed = TRUE)[[2]], "_dep_", fixed = T
    )[[1]], 
    "_dep.rds"
    )]

# merge deployment info
cleaned_tracks <- merge(
  cleaned_tracks, deployments, by.x = "original_file", by.y = "file", all.x = T
  )

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
    by = c("individual_local_identifier", "birdlife_name", "sensor_type_id")
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
    by = c("tag_local_identifier", "birdlife_name", "sensor_type_id")
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

# save the list of the cleaned tracks with the columns of interest 
cols_out <- names(cleaned_tracks)[
  grep(
    "file|_id$|_identifier$|track|birdlife|manipulation|deployment|animal|sex|to_exclude", 
    names(cleaned_tracks)
  )]

cleaned_tracks <- cleaned_tracks[
  , ..cols_out
  ][ , (c("track_problem", "original_file")) := NULL
  ][ , excluded := dup_ind_to_exclude | dup_tag_to_exclude]

fwrite(cleaned_tracks, file.path(data_dir, file_deploy_clean))
