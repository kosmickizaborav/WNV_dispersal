#' ---
#' title: "Clean deployments"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' **SECTION 0 - Define parameters and packages**
#' in this section we define variables that are used later in the script:
#' - target_sp - extracted from "1_deployments_download_report.rds"
#' - cols_gps - columns that are used for cleaning process
#' - E4WarningID - study id from aiguamolls de l'emporda
#' - target_sensors - for filtering sensor information from deployments
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

# COMMENTED OUT SO THAT IT CAN BE RUN AS A SOURCE
library(here)
library(tidyverse)
library(amt)
library(sf)
library(arrow)
source(here("0_helper_functions.R"))

# columns needed to detect track problems
cols_gps <- c(
  "timestamp", "geometry", "gps_hdop", "gps_vdop", "gps_satellite_count", 
  "algorithm_marked_outlier", "import_marked_outlier",
  "manually_marked_outlier", "sensor_type_id"
)

# this is the study id from aiguamolls de l'emporda
E4WarningID <- 4043292285
data_dir <- here("Data")
study_dir <- file.path(data_dir, "Studies")

target_sensors <- c(653, 2299894820)

# INPUT
f_deployments_filtered <- "1_downloadable_studies_deployments_filtered.rds"
# OUTPUT
f_track_problem <- "2_track_problems_report.rds"
f_deploy_info_complete <- "2_deployment_info_complete.rds"


source(here("2_detect_track_problems.R"))
source(here("2_find_duplicated_tracks.R"))

# # 1 - Correct deployment metadata---------------------------------------------

# manipulation_problem was defined by inspecting the following
# here("Data", "1_downloadable_studies_deployments_filtered.rds") |>
#   read_rds() |>
#   filter(!is.na(manipulation_comments)) |>
#   distinct(species, study_id, manipulation_comments, manipulation_type) |>
#   mutate(
#     manipulation_type_corrected = case_when(
#       str_detect(manipulation_comments, manipulation_problem) ~ "other",
#       str_detect(manipulation_comments, "[Dd]isplacement") ~ "relocated",
#       is.na(manipulation_type) ~ "none",
#       .default = manipulation_type
#     )
#   ) |>
#   arrange(species, manipulation_type, manipulation_type_corrected) |>
#   write_csv(here("Data", "2_manipulation_type_corrected.csv"))

manipulation_problem <- "restricted|purified|anosmi[ac]|shift|FL|magnetic"

deployments_filtered <- file.path(data_dir, f_deployments_filtered) |>
  read_rds() |>
  mutate(
    manipulation_type = case_when(
      str_detect(manipulation_comments, manipulation_problem) ~ "other",
      str_detect(manipulation_comments, "[Dd]isplacement") ~ "relocated",
      is.na(manipulation_type) ~ "none",
      .default = manipulation_type
    )
  ) |>
  filter(manipulation_type != "other")

deployments_filtered <- deployments_filtered |>
  mutate(
    file = make_file_name(study_id, individual_id, deployment_id),
    file_parquet = str_replace(file, ".rds", ".parquet"),
    fin_exists = check_file_exists(file, "1_deployments", species)
  ) |> 
  filter(fin_exists == T) |> 
  select(-fin_exists) |> 
  arrange(species, file) |> 
  mutate(file_n = 1:n()) 

# just for reporting on the progress
dc <- nrow(deployments_filtered)

target_sp <- deployments_filtered  |> 
  distinct(species) |> 
  pull() 

# 2 - Create output directories-------------------------------------------------

# create directory for the cleaned deployments and track problems
target_sp |> 
  walk(~{
    
    sp_dir <- file.path(data_dir, "Studies", str_replace(.x, " ", "_"))
    out_dir <- file.path(sp_dir, "2_track_problems")
    out_dir2 <- file.path(sp_dir, "2_cleaned")
    
    if(!dir.exists(out_dir)){ out_dir |> dir.create() } 
    if(!dir.exists(out_dir2)){ out_dir2 |> dir.create() }
    
  })


# 3 - Label track problems------------------------------------------------------

deploy_info <- deployments_filtered |>
  select(contains("timestamp"), file, species, file_n) |> 
  mutate(
    fin_path = get_file_path(file, "1_deployments", species),
    fout_path = get_file_path(file, "2_track_problems", species), 
    fout_exists = file.exists(fout_path)
  ) |> 
  filter(fout_exists == F) |> 
  select(-fout_exists, -file) |> 
  group_split(fin_path) |> 
  walk(~{
    
    deploy_info <- .x
    
    fin <- deploy_info$fin_path
    fout <- deploy_info$fout_path
    
    cat("\n", deploy_info$species, deploy_info$file_n, "|", dc)
    
    fin |> 
      read_rds() |> 
      mutate(
        x = st_coordinates(geometry)[,1],
        y = st_coordinates(geometry)[,2]
      ) |>
      st_drop_geometry() |>
      as_tibble() |> 
      make_track(
        x, y, timestamp,
        # movebank default, but checked that all tracks have this crs
        crs = st_crs(4326), 
        all_cols = T
      ) |> 
      detect_track_problems(
        deploy_on_time = deploy_info$deploy_on_timestamp,
        deploy_off_time = deploy_info$deploy_off_timestamp,
        sensors_of_interest = target_sensors
      ) |> 
      select(x_, y_, t_, sensor_type_id, track_problem) |> 
      write_rds(fout)

      invisible(gc())
    
  }) 

invisible(gc())


# 4 - Summarize track problems ------------------------------------------------

fp_track_problem <- file.path(data_dir, f_track_problem)

if(!file.exists(fp_track_problem)){
  
  files <- deployments_filtered |> 
    mutate(
      fin_path = get_file_path(file, folder = "2_track_problems", species)
    ) |> 
    select(species, file_n, file, fin_path) |> 
    group_split(fin_path) |>
    map(~{
      
      deploy_info <- .x
      
      fin <- deploy_info$fin_path
      
      cat("\n", deploy_info$species, deploy_info$file_n, "|", dc)
      cat("\n-----------------------summary done!-----------------------")
      
      fin |> 
        read_rds() |> 
        summarize(
          track_start = min(timestamp), 
          track_end = max(timestamp),
          track_period_days = as.numeric(
            difftime(track_end, track_start, units = "days")
          ),
          track_unique_days = length(unique(date(timestamp))), 
          n_locs = n(), 
          .by = c(track_problem, sensor_type_id)
        ) |> 
        bind_cols(deploy_info |> select(file, species, file_n))
      
    }) |> 
    list_rbind() |> 
    mutate(saved = n_locs >= 3 & is.na(track_problem)) |> 
    select(file_n, species, file, saved, everything())
  
  track_problem_report |> 
    write_rds(fp_track_problem)
  
  # for overleaf document
  track_problem_report |> 
    filter(!is.na(track_problem)) |>
    summarize(count = sum(n_locs, na.rm = T), .by = c(track_problem)) |> 
    arrange(desc(count)) |> 
    write_csv(file.path(data_dir, "2_track_problems_total_summary.csv"))
    
  
}
  
invisible(gc())

# 5 - Check deployment information ----------------------------------------

# this step was done because in some cases we had deployment information 
# was not complete (i.e. within the track data there was info on tag identifier
# that didn't exist in deployment information), so just to extract all the
# info possible. 

fp_deploy_info_complete <- file.path(data_dir, f_deploy_info_complete)

if(!file.exists(fp_deploy_info_complete)){
  
  deploy_info_complete <- deployments_filtered |>
    mutate(
       fin_path = get_file_path(file, folder = "2_track_problems", species)
    ) |> 
    group_split(fin_path) |> 
    map(~{
      
      invisible(gc())
      
      fin <- .x$fin_path
      
      cat("\n", .x$species, .x$file_n, "|", dc)
      cat("\n------------------deployment info checked!-----------------------")
      
      deploy_info <- .x |> 
        # remove all columns with missing values, to be able to merge
        select(where(~!all(is.na(.)))) |> 
        select(-fin_path)
      
      fin |> 
        read_rds() |> 
        # remove columns that are not needed
        select(-any_of(c("event_id", "event_group_id", "track_segment_id"))) |>
        select(ends_with("_id"), ends_with("_identifier")) |> 
        distinct() |> 
        select(where(~!all(is.na(.)))) |> 
        mutate(
          across(ends_with("_id"), bit64::as.integer64), 
          across(ends_with("_identifier"), as.character)
        ) |>
        left_join(deploy_info)
      
    }) |> 
    list_rbind() 
  
  deploy_info_complete |> 
    write_rds(fp_deploy_info_complete)
  
}

invisible(gc())
rm(deployments_filtered)


# 6 - Prepare deployment info for cleaned tracks --------------------------

# prepare deployment info overview for cleaned tracks

track_problem_report <- fp_track_problem |> 
  read_rds() |> 
  select(-file_n) |> 
  mutate(
    # keep only the values for the tracks that will be saved
    across(
      all_of(matches("track_(start|end|period_days|unique_days)")),
      ~if_else(!saved, NA, .x)
    ), 
    n_locs = if_else(!is.na(track_problem), NA, n_locs)
  ) |> 
  group_by(file, sensor_type_id) |> 
  mutate(
    # label the tracks that have segments without problems
    track_saved = sum(saved) == 1, 
    # summarize the number of clean locations
    clean_locs = sum(n_locs, na.rm = T),
  ) |> 
  # keep the tracks without problems or keep the first row of track with problem
  filter(
    (track_saved & saved) | (track_saved == F & row_number() == 1)
  ) |> 
  ungroup() |> 
  select(-track_problem, -n_locs, -track_saved) |> 
  mutate(track_period_days = round(track_period_days, 2))


deploy_info_cleaned <- fp_deploy_info_complete |> 
  read_rds() |> 
  select(
    -deploy_on_timestamp, 
    -deploy_off_timestamp, 
    -manipulation_comments, 
    -taxon_canonical_name, 
    -account
  ) |> 
  distinct() |> 
  left_join(
    track_problem_report, by = c("species", "file", "sensor_type_id")
  ) |>
  mutate(
    # adding a new file name for each sensor type
    file = make_file_name(
      study_id, individual_id, deployment_id, sensor_id = sensor_type_id
    )
  ) |> 
  arrange(species, track_start) |> 
  mutate(file_n = 1:n()) |> 
  select(file_n, species, file, saved, clean_locs, everything())

rm(track_problem_report)

dc <- nrow(deploy_info_cleaned)

# 7 - Check if deployments are duplicated ---------------------------------

saved_tracks <- deploy_info_cleaned |> 
  # there are inconsistencies with data, e.g. individual number of deployments
  # for study_id 348687678 is 1, but there are 19 deployment ids
  mutate(
    across(starts_with(c("tag_", "individual_")), as.character),
    individual_local_identifier = if_else(
      is.na(individual_local_identifier),
      individual_id,
      individual_local_identifier
    ),
    tag_local_identifier = if_else(
      is.na(tag_local_identifier),
      tag_id,
      tag_local_identifier
    )
  ) |> 
  filter(saved == T) 

dup_ind_to_exclude <- saved_tracks |> 
  group_split(species, individual_local_identifier, sensor_type_id) |> 
  map(~{
    
    ind_df <- .x
    
    find_duplicated_tracks(
      ind_df, track_id = file_n, start = track_start, end = track_end, 
      n_locs = clean_locs, duration = track_unique_days,
      keep_criteria = "duration"
    )
    
  }) |> 
  unlist() 

dup_tag_to_exclude <- saved_tracks |> 
  group_split(species, tag_local_identifier, sensor_type_id) |> 
  map(~{
    
    tag_df <- .x
    
    find_duplicated_tracks(
      tag_df, track_id = file_n, start = track_start, end = track_end, 
      n_locs = clean_locs, duration = track_unique_days,
      keep_criteria = "duration"
    )
    
  }) |> 
  unlist()

rm(saved_tracks)


deploy_info_cleaned <- deploy_info_cleaned |> 
  mutate(
    duplicated_individual = file_n %in% dup_ind_to_exclude, 
    duplicated_tag = file_n %in% dup_tag_to_exclude, 
    excluded = duplicated_individual | duplicated_tag,
  )

deploy_info_cleaned |> 
  write_rds(file.path(data_dir, "2_deployment_info_cleaned.rds"))


# 8 - Save cleaned deployments--------------------------------------------------

deploy_info_cleaned |>
  rename(file_out = file) |>
  mutate(
    fout_path = get_file_path(file_out, folder = "2_cleaned", species),
    fout_exists = file.exists(fout_path),
    file_in = make_file_name(study_id, individual_id, deployment_id),
    fin_path = get_file_path(file_in, folder = "2_track_problems", species)
  ) |> 
  filter(saved == T, fout_exists == F, excluded == F) |> 
  select(file_n, species, fin_path, fout_path, saved, sensor_type_id) |>
  group_split(fin_path) |>
  walk(~{
    
    deploy_info <- .x
    fin <- unique(deploy_info$fin_path)
    
    cat("\n", deploy_info$species, deploy_info$file_n, "|", dc)
    cat("\n---------------------cleaned track saved!-----------------------")
    
    fin |> 
      read_rds() |> 
      # filter only locations without track problems
      filter(is.na(track_problem)) |> 
      # if there are multiple sensor types, split the data
      group_split(sensor_type_id) |>
      walk(~{
        
        track <- .x
        
        sensor <- unique(track$sensor_type_id)
        fout <- deploy_info$fout_path[deploy_info$sensor_type_id == sensor]
        to_save <- deploy_info$saved[deploy_info$sensor_type_id == sensor]
        
        if(to_save){
          
          track |> 
            select(-track_problem, -sensor_type_id) |> 
            write_rds(fout)
          
        }
        
      })
    
    
    invisible(gc())
    
  })



# summary for the report
summary <- deploy_info_cleaned |> 
  mutate(
    locs = if_else(saved & !excluded, clean_locs, NA), 
    days = if_else(saved & !excluded, track_unique_days, NA)
  ) |> 
  summarize(
    total = n(), 
    saved = sum(saved), 
    excluded = sum(excluded),
    across(
      all_of(c("locs", "days")), 
      list(
        min = ~min(.x, na.rm = T),
        mean = ~round(mean(.x, na.rm = T), 1),
        max = ~max(.x, na.rm = T),
        total = ~sum(.x, na.rm = T)
      )
    ),
    .by = c(species, sensor_type_id)
  ) |> 
  mutate(
    sensor_type_id = case_when(
      sensor_type_id == 653 ~ "GPS",
      sensor_type_id == 2299894820 ~ "SigFox",
      .default = NA
    )
  ) |> 
  arrange(species, sensor_type_id) |> 
  select(-days_total) |> 
  write_csv(file.path(data_dir, "2_deployment_info_cleaned_summary.csv"))


