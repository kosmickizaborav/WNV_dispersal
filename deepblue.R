#' ---
#' title: "Clean deployments"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------


# 0 - Define parameters and packages --------------------------------------

library(here)
library(tidyverse)
library(data.table)   
options(datatable.integer64 = "integer64")
library(furrr)           
library(sf)    
source(here("0_helper_functions.R"))
source(here("2_detect_track_problems.R"))
source(here("2_find_duplicated_tracks.R"))

# Set up parallel processing (use available cores)
plan(multisession, workers = availableCores() - 1)

# Configuration
cols_gps <- c(
  "timestamp", "geometry", "gps_hdop", "gps_vdop",
  "algorithm_marked_outlier", "import_marked_outlier",
  "manually_marked_outlier", "sensor_type_id"
)

E4WarningID <- 4043292285
data_dir <- here("Data")
target_sensors <- c(653, 2299894820)
manipulation_problem <- "restricted|purified|anosmi[ac]|shift|FL|magnetic"

# File paths
f_deployments_filtered <- "1_downloadable_studies_deployments_filtered.rds"
f_track_problem <- "2_track_problems_report.parquet"  # Changed to parquet
f_deploy_info_complete <- "2_deployment_info_complete.parquet"  # Changed to parquet

# 1 - Correct deployment metadata -----------------------------------------

# Load deployments
deployments_filtered <- file.path(data_dir, f_deployments_filtered) |> 
  read_rds() |> 
  as.data.table()

# removing potentially bad manipulation types

manipulation_problem <- "restricted|purified|anosmi[ac]|shift|FL|magnetic"

deployments_filtered[, manipulation_type := fcase(
  str_detect(manipulation_comments, manipulation_problem), "other",
  str_detect(manipulation_comments, "[Dd]isplacement"), "relocated",
  is.na(manipulation_type), "none",
  default = manipulation_type
)]
deployments_filtered[manipulation_type != "other"]
  

# Ensure files exist
deployments_filtered[, file := make_file_name(study_id, individual_id, deployment_id)]
deployments_filtered[, fin_exists := file.exists(get_file_path(file, folder = "1_deployments", species))]
deployments_filtered <- deployments_filtered[fin_exists == TRUE][, fin_exists := NULL]


process_deployments <- function() {
  deployments <- read_rds(file.path(data_dir, f_deployments_filtered)) |> 
    as.data.table()
  
  deployments[, manipulation_type := fcase(
    str_detect(manipulation_comments, manipulation_problem), "other",
    str_detect(manipulation_comments, "[Dd]isplacement"), "relocated",
    is.na(manipulation_type), "none",
    default = manipulation_type
  )]
  
  deployments <- deployments[manipulation_type != "other"]
  
  # Check file existence in parallel
  deployments[, file := make_file_name(study_id, individual_id, deployment_id)]
  deployments[, fin_path := get_file_path(file, "1_deployments", species)]
  deployments[, fin_exists := file.exists(fin_path)]
  
  deployments <- deployments[fin_exists == TRUE][, fin_exists := NULL]
  deployments[, file_n := .I]
  return(deployments)
}

deployments_filtered <- process_deployments()
dc <- nrow(deployments_filtered)

# 2 - Create output directories -------------------------------------------

create_dirs <- function(species_list) {
  future_walk(species_list, ~{
    sp_dir <- file.path(data_dir, "Studies", str_replace(.x, " ", "_"))
    dir.create(file.path(sp_dir, "2_track_problems"), showWarnings = FALSE)
    dir.create(file.path(sp_dir, "2_cleaned"), showWarnings = FALSE)
  })
}

create_dirs(unique(deployments_filtered$species))

# 3 - Label track problems ------------------------------------------------

label_track_problems <- function(deployments) {
  process_file <- function(info) {
    track <- read_rds(info$fin_path) |> 
      as.data.table() |> 
      detect_track_problems(
        deploy_on_time = info$deploy_on_timestamp,
        deploy_off_time = info$deploy_off_timestamp,
        sensors_of_interest = target_sensors
      )
    
    track[, .(geometry, timestamp, sensor_type_id, track_problem)] |> 
      write_rds(info$fout_path)
  }
  
  to_process <- deployments[
    !file.exists(get_file_path(file, "2_track_problems", species)),
    .(file_n, species, file, deploy_on_timestamp, deploy_off_timestamp)
  ][, `:=`(
    fin_path = get_file_path(file, "1_deployments", species),
    fout_path = get_file_path(file, "2_track_problems", species)
  )]
  
  # Process in parallel
  future_pwalk(
    split(to_process, seq_len(nrow(to_process)) %...>% 
            process_file(),
          .progress = TRUE
    )
    }

label_track_problems(deployments_filtered)

# 4 - Summarize track problems --------------------------------------------

summarize_track_problems <- function(deployments) {
  if (!file.exists(file.path(data_dir, f_track_problem))) {
    process_summary <- function(info) {
      track <- read_rds(info$fin_path) |> as.data.table()
      
      track[, .(
        track_start = min(timestamp),
        track_end = max(timestamp),
        track_period_days = as.numeric(difftime(max(timestamp), min(timestamp), units = "days")),
        track_unique_days = uniqueN(date(timestamp))),
        n_locs = .N,
        by = .(track_problem, sensor_type_id)
      ][, `:=`(
        file = info$file,
        species = info$species,
        file_n = info$file_n
      )]
    }
    
    summary_data <- deployments[
      , .(species, file_n, file, fin_path = get_file_path(file, "2_track_problems", species))
    ] |> 
      future_map_dfr(~process_summary(.x), .progress = TRUE)
    
    summary_data[, saved := n_locs >= 3 & is.na(track_problem))]
    
    # Write outputs
    write_parquet(summary_data, file.path(data_dir, f_track_problem))
    
    # Create problem summary
    summary_data[!is.na(track_problem)] |> 
      .[, .(count = sum(n_locs)), by = track_problem] |> 
      .[order(-count)] |> 
      .[, track_problem := str_replace(track_problem, ">", "greater than")] |> 
      write_csv(file.path(data_dir, "2_track_problems_total_summary.csv"))
}
}

summarize_track_problems(deployments_filtered)

# 5 - Check deployment information ----------------------------------------

check_deployment_info <- function(deployments) {
  if (!file.exists(file.path(data_dir, f_deploy_info_complete))) {
    process_deployment_info <- function(info) {
      track <- read_rds(info$fin_path) |> as.data.table()
      
      # Get unique identifiers from track
      id_cols <- track[, .SD, .SDcols = patterns("_id$|_identifier$")] |> 
        unique() |> 
        na.omit()
      
      # Merge with deployment info
      merge(
        info[, .SD, .SDcols = !patterns("fin_path")],
        id_cols,
        all = TRUE
      )
    }
    
    deployment_info <- deployments[
      , .(species, file_n, file, fin_path = get_file_path(file, "2_track_problems", species))
    ] |> 
      future_map_dfr(~process_deployment_info(.x), .progress = TRUE)
    
    write_parquet(deployment_info, file.path(data_dir, f_deploy_info_complete))
  }
}

check_deployment_info(deployments_filtered)

# 6 - Prepare deployment info for cleaned tracks --------------------------

prepare_cleaned_tracks <- function() {
  track_problem_report <- read_parquet(file.path(data_dir, f_track_problem)) |> 
    as.data.table()
  
  # Clean up track problem report
  track_problem_report[
    , `:=`(
      across(matches("track_(start|end|period_days|unique_days)"), ~ifelse(!saved, NA, .x)),
      n_locs = ifelse(!is.na(track_problem), NA, n_locs)
    )
  ]
  
  track_problem_report[
    , `:=`(
      track_saved = sum(saved) == 1,
      clean_locs = sum(n_locs, na.rm = TRUE)
    ),
    by = .(file, sensor_type_id)
  ]
  
  track_problem_report <- track_problem_report[
    (track_saved & saved) | (!track_saved & seq_len(.N) == 1),
    !c("track_problem", "n_locs", "track_saved")
  ][, track_period_days := round(track_period_days, 2)]
  
  # Merge with deployment info
  deploy_info_complete <- read_parquet(file.path(data_dir, f_deploy_info_complete)) |> 
    as.data.table()
  
  deploy_info_cleaned <- merge(
    deploy_info_complete,
    track_problem_report,
    by = c("species", "file", "sensor_type_id"),
    all = TRUE
  )
  
  deploy_info_cleaned[
    , file := make_file_name(study_id, individual_id, deployment_id, sensor_id = sensor_type_id)
  ][order(species, track_start)][, file_n := .I]
  
  return(deploy_info_cleaned)
}

deploy_info_cleaned <- prepare_cleaned_tracks()
dc <- nrow(deploy_info_cleaned)

# 7 - Save cleaned deployments --------------------------------------------

save_cleaned_deployments <- function(deploy_info_cleaned) {
  process_cleaned_file <- function(info) {
    track <- read_rds(info$fin_path) |> 
      as.data.table() |> 
      .[is.na(track_problem)]
    
    if (info$to_save) {
      track[, !c("track_problem", "sensor_type_id")] |> 
        write_rds(info$fout_path)
    }
  }
  
  to_save <- deploy_info_cleaned[
    saved == TRUE & !file.exists(get_file_path(file_out, "2_cleaned", species)),
    .(file_n, species, sensor_type_id, saved)
  ][, `:=`(
    file_out = file,
    file_in = make_file_name(study_id, individual_id, deployment_id),
    fin_path = get_file_path(file_in, "2_track_problems", species),
    fout_path = get_file_path(file_out, "2_cleaned", species),
    to_save = saved
  )]
  
  # Process in parallel by sensor type
  future_walk(
    split(to_save, by = "sensor_type_id"),
    ~process_cleaned_file(.x),
    .progress = TRUE
  )
}

save_cleaned_deployments(deploy_info_cleaned)

# 8 - Check for duplicated deployments ------------------------------------

check_duplicates <- function(deploy_info_cleaned) {
  saved_tracks <- deploy_info_cleaned[
    saved == TRUE,
    .(
      file_n, species, sensor_type_id, track_start, track_end, 
      clean_locs, track_unique_days,
      individual_local_identifier = fifelse(
        is.na(individual_local_identifier), individual_id, individual_local_identifier
      ),
      tag_local_identifier = fifelse(is.na(tag_local_identifier), tag_id, tag_local_identifier)
    )
  ]
  
  # Find duplicates by individual
  dup_ind <- saved_tracks[
    , .(data = list(.SD)),
    by = .(species, individual_local_identifier, sensor_type_id)
  ][, excluded := map(data, ~find_duplicated_tracks(
    .x, track_id = file_n, start = track_start, end = track_end,
    n_locs = clean_locs, duration = track_unique_days,
    keep_criteria = "duration"
  ))]
  
  # Find duplicates by tag
  dup_tag <- saved_tracks[
    , .(data = list(.SD)),
    by = .(species, tag_local_identifier, sensor_type_id)
  ][, excluded := map(data, ~find_duplicated_tracks(
    .x, track_id = file_n, start = track_start, end = track_end,
    n_locs = clean_locs, duration = track_unique_days,
    keep_criteria = "duration"
  ))]
  
  # Combine results
  deploy_info_cleaned[
    , `:=`(
      duplicated_individual = file_n %in% unlist(dup_ind$excluded),
      duplicated_tag = file_n %in% unlist(dup_tag$excluded),
      excluded = file_n %in% c(unlist(dup_ind$excluded), unlist(dup_tag$excluded))
    )
  ]
  
  write_rds(deploy_info_cleaned, file.path(data_dir, "2_deployment_info_cleaned.rds"))
  
  # Create summary
  summary <- deploy_info_cleaned[
    , .(
      locs_min = min(clean_locs[saved & !excluded], na.rm = TRUE),
      locs_mean = round(mean(clean_locs[saved & !excluded], na.rm = TRUE), 1),
      locs_max = max(clean_locs[saved & !excluded], na.rm = TRUE),
      locs_total = sum(clean_locs[saved & !excluded], na.rm = TRUE),
      days_min = min(track_unique_days[saved & !excluded], na.rm = TRUE),
      days_mean = round(mean(track_unique_days[saved & !excluded], na.rm = TRUE), 1),
      days_max = max(track_unique_days[saved & !excluded], na.rm = TRUE),
      total = .N,
      saved = sum(saved),
      excluded = sum(excluded)
    ),
    by = .(species, sensor_type_id)
  ][, sensor_type_id := fcase(
    sensor_type_id == 653, "GPS",
    sensor_type_id == 2299894820, "SigFox",
    default = NA_character_
  )][order(species, sensor_type_id)]
  
  write_csv(summary, file.path(data_dir, "2_deployment_info_cleaned_summary.csv"))
}

check_duplicates(deploy_info_cleaned)