
library(here)
library(tidyverse)
source(here("0_helper_functions.R"))


run_segments <- list(
  "1_download_movebank.R" = F, 
  "2_clean_deployments.R" = T
)

# create a directory where data will be downloaded
data_dir <- here("Data")


# 0 - List of all ouput files ---------------------------------------------

# Download_movebank.R SECTION 1
f_studies_filtered <- "1_downloadable_studies.rds"
# Download_movebank.R SECTION 2
f_deployments <- "1_downloadable_studies_deployments.rds"
# Download_movebank.R SECTION 3
f_deployments_filtered <- "1_downloadable_studies_deployments_filtered.rds"
# Download_movebank.R SECTION 5, 6
f_download_report <- "1_deployments_download_report.rds"

# 1_download_movebank -----------------------------------------------------

# for study_download_function, identical to deployment metadata
target_sensors <- c(653, 2299894820) # c("gps", "sigfox-geolocation")

f_deployments_filtered <- "1_downloadable_studies_deployments_filtered.rds"

if(run_segments[["1_download_movebank.R"]]){
  
  # defining species of interest - original list of species
  target_sp <- c(
    "Anas platyrhynchos",
    "Columba livia",
    "Chroicocephalus ridibundus",
    "Sturnus vulgaris",
    "Turdus merula",
    "Circus aeruginosus",
    "Accipiter gentilis",
    "Passer domesticus"
  )
  
  # extended list of species
  species_extra <- here("Alex_data", "Palearctic_prevalence bird_species.xlsx") |>
    readxl::read_xlsx() |>
    filter(group_prevalence > 0) |>
    pull(BirdLife_name)
  
  target_sp <- c(target_sp, species_extra) |> unique()
  rm(species_extra)
  
  # defining sensors of interest
  # because they are written in different ways
  tags_ids <- c(
    "GPS", "Sigfox Geolocation", # sensor type ids from studies
    "gps", "sigfox-geolocation" # sensor type ids from deployments
  )
  
  
  # accepted manipulation types including NA values
  accepted_manipulation <- c("none", "relocated")
  
  # list of columns that seemed to be relevant, selected by exploring the
  # dataframe downloaded from movebank using movebank_download_deployment()
  col_deploy <- c(
    "taxon_canonical_name", "study_id", "deployment_id",
    "individual_id", "individual_local_identifier", "sensor_type_ids",
    "individual_number_of_deployments",
    "deployment_local_identifier", "tag_id", "tag_local_identifier",
    "sex", "animal_life_stage",  "manipulation_type",
    "manipulation_comments",
    "deploy_on_timestamp", "deploy_off_timestamp"
  )
  
  # accessing data from two different movebank accounts
  # additional account that is from the Rbook on movement
  # key_name = "rbook_account"
  # options("move2_movebank_key_name" = "movebank")
  # for downlaoding pigeon study:
  # ID 897179972Ornitela_Pigeon_Columba livia_Spiegel_Israel
  # another account with Aiguamolls data
  #
  # for checking all the accounts:
  # keyring::key_list()
  #           service        username
  # 1        movebank    TeamWikelski
  # 2   rbook_account           RBook
  # 3 kosmickizaborav Kosmickizaborav
  
  movebank_access <- list(
    main = "movebank",
    sub1 = "rbook_account",
    sub2 = "kosmickizaborav"
  )
  
  source(here("1_download_movebank.R"))
  
  rm(
    movebank_access, 
    f_studies_filtered, 
    f_deployments, 
    f_download_report
  )
  
}


# 2_clean_deployments -----------------------------------------------------

if(run_segments[["2_clean_deployments.R"]]){

  # columns needed to detect trakc problems
  cols_gps <- c(
    "timestamp", "geometry", "gps_hdop", "gps_vdop",
    "algorithm_marked_outlier", "import_marked_outlier",
    "manually_marked_outlier", "sensor_type_id"
  )
  
  # this is the study id from aiguamolls de l'emporda
  E4WarningID <- 4043292285
  
  # OUTPUT
  f_track_problem <- "2_track_problems_report.rds"
  f_deploy_info_complete <- "2_deployment_info_complete.rds"
  
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
    filter(manipulation_type != "other") |> 
    select(-manipulation_comments)
  
  source(here("2_clean_deployments.R"))
  

  }





