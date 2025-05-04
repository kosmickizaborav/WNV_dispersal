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
#' **SECTION 1 - Prepare deployment information** 
#' using file "1_downloadable_studies_deployments_filtered.rds", 
#' - correct manipulation_type - in some cases, manipulation type seem not
#'   specified correctly, so it is changed in accordance with
#'   manipulation_comment
#'   detected problematic manipulation comments:
#'   - 1. relocated and released for trials; for 2 trials was treated 
#'    to shift light cycle
#'   - 2. relocated and released for trials; for 1 trial was treated to shift 
#'   ight cycle
#'   - 3. CS (clock-shift)
#'   - 4. FL
#'   - 5. XY (magnetically deprived)
#'   - 6. AO (anosmic)
#'   - 7. A: anosmia treatment prior to relocation and release
#'   - 8. CAP: relocated with purified air during transportation
#'   - 9. animal owned by farm and may have been restricted in movement or
#'     transported for sale (see Deployment Comments) - couldn't find the column
#'     deployment comments in the deployment information so we excluded it
#'     in case the animal was restricted
#' - add column study_site - to distinguish E4Warning study site and others
#' - filter deployments that were not downloaded
#'
#' **SECTION 2 - Create output directories**
#' create directories for track_problmes and cleaned deployments, 
#' done separately, because it takes much more space and like this we can keep
#' the data separately. 
#' 
#'  **SECTION 3 - Label track problems**
#'  using function detect_track_problems() that encompasses all track problems, 
#'  we label them, and save the tracks with labels to be able to check them 
#'  out later
#'  
#'  **SECTION 4 - Summarize track problems**
#'  obtain the summary of locations with and without problems, this is used
#'  later to check for duplicated data (track period etc)
#'  
#'  


# 0 - Define parameters and packages ---------------------------------------

library(data.table)
library(sf)
library(amt)
source("0_helper_functions.R")
source("2_clean_deployments_datatable_FUNCTIONS.R")

# columns needed to detect track problems
cols_gps <- c(
  "timestamp", "geometry", "gps_hdop", "gps_vdop", "gps_satellite_count", 
  "sensor_type_id"
)

# this is the study id from aiguamolls de l'emporda
E4WarningID <- 4043292285
data_dir <- here::here("Data")
study_dir <- file.path(data_dir, "Studies")

# INPUT
file_dep_filter <- "1_deployments_to_download.csv"
# OUTPUT
file_track_problem <- "2_track_problems_report.csv"
file_deploy_clean <- "2_deployments_cleaned.csv"


# 1 - Create output directories -----------------------------------------------

deployments <- fread(file.path(data_dir, file_dep_filter))[
  , file := make_file_name(study_id, individual_id, deployment_id)
  ]

# create output directories
target_sp <- unique(deployments$birdlife_name)

create_dir(target_sp, c("2_track_problems", "2_cleaned"))

# 2 - Label and clean track problems-------------------------------------------

# here we separate deployments by sensor type id because for some species there
# is a simultaneous recording with GPS and SixFox

if(!file.exists(file.path(data_dir, file_track_problem))){
  
  deploy_problems <- copy(deployments)[
    , fin_path := get_file_path(file, "1_deployments", birdlife_name)
  ][file.exists(fin_path)][, fin_path]
  
  i_total <- length(deploy_problems)
  
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
      
      fout_problem <- gsub(
        ".rds", 
        paste0("_", unique(t$sensor_type_id), "_sen.rds"),
        gsub("1_deployments", "2_track_problems", fin)
      )
      
      fwrite(t, fout_problem)
      
      fout_cleaned <- gsub("2_track_problems", "2_cleaned", fout_problem)
      
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

# 3 - Detect duplicated deployments ---------------------------------------

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

# if there indiviudal local identifier is not provided check indiviudal id
cleaned_tracks[
  individual_local_identifier %in% c("", NA), 
  individual_local_identifier := individual_id
]

# if tag local identifier is not provided check tag id
cleaned_tracks[
  tag_local_identifier %in% c("", NA), 
  tag_local_identifier := tag_id
]

# check if duplicated individual local identifiers overlap in time 
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

# check if duplicated tag local identifiers overlap in time 
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
