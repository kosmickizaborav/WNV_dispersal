#' ---
#' title: "Optimized Downloading Movebank Data"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------
#' **SECTION 0 - Define parameters and packages**
#' in this section we define variables that are used later in the script:
#' - target_sp - species of interest that we search for in Movebank
#' - tags_ids - sensor type ids that we are interested in, as defined in Movebank
#' - accepted_manipulation - manipulation types that are accepted,
#' - col_deploy - relevant columns from the deployment data
#'  
#' **SECTION 1 - Check available studies** 
#' access all studies from different accounts and filter studies that:
#' filter the sensors of interest 
#' 1 - with download access
#' 2 - have registered deployments
#' 3 - deploy sensors of interest
#' 4 - it is not specified as a test deployment
#' >>> OUTPUT: f_studies_filtered
#' 
#' **SECTION 2 - Download deployment information**
#' download deployment information from studies of interest,
#' done in this way, even if it is much slower then just filtering studies,
#' because in study info sometimes there is no species specified, 
#' so to make sure to include all potential studies
#' if download failed keep the error information
#' >>> OUTPUT: f_deployments
# 
#'**SECTION 3 - Filter deployment information**
#' create a new variable species that extracts species information from 
#' the column taxon_detail if it's not specified under taxon_canonical_name.
#' This step was done after seeing how taxon is specified in the selected study, 
#' it can happen that not all cases are included when adding new studies.
#' filter deployments that include:
#' 1 - species of interest
#' 2 - sensor type of interest
#' 3 - no manipulation with the tracked animal or animal relocated
#' >>> OUTPUT: f_deployments_filtered
#' 
#' **SECTION 4 - Create directories for species with data**
#' create a study folder for every species
#' 
#' **FUNCTION 1: safe_study_download**
#' safely wrapping the function movebank_download_study that was used to 
#' download data, had to specify two version because the function wouldn't
#' accept individual_local_identifier as NULL or NA
#' 
#' **FUNCTION 2: study_download_protocol**
#' function that wraps all the steps needed when downloading data, 
#' and returns the deployment information and the data status: 
#' - if an error occurred, returns the error message
#' - if no error occurred but no data available to save, annotate it
#' - if data available, save it and return file name
#' 
#' **SECTION 5 - Download and save deployments**
#' - download tracking data using study_id, individual_local_identifier, 
#'    and sensor_type_id
#' - save each individual deployment in files specified by study_id, 
#'    individual_id and deployment_id
#' - keep track of errors that occurred during download and 
#'    save it in download_report
#' 
#'  **SECTION 6 - Retry download for studies with errors**
#'  - retry downloading data for studies that had an error during download
#'    this time using only study_id, for some reason it fixed a few studies
#'    like that, assuming it's the way the individual was specified. 
#' >>> OUTPUT: f_download_report

# 0 - Define parameters and packages--------------------------------------------

library(data.table)
library(move2)
source("0_helper_functions.R")
source("1_download_movebank_datatable_FUNCTIONS.R")

data_dir <- here::here("Data")
if (!dir.exists(data_dir)) dir.create(data_dir)

# Extended species list
WNV_prevalence <- here::here("Alex_data", "Palearctic_prevalence bird_species.xlsx") |>
  readxl::read_xlsx() |> 
  as.data.table()

# take only species names that are have group pravelence > 0
sp_to_check <- WNV_prevalence[,BirdLife_name := squish_base(BirdLife_name)][
  group_prevalence > 0 & grepl(" ", BirdLife_name), BirdLife_name]

# check that names match birdlife names
target_sp_birdlife <- rename_to_birdlife(species_name = sp_to_check)

# to be able to search the database based on both synonyms provided and 
# original birdlife name
target_sp <- unique(
  c(target_sp_birdlife$original_name, target_sp_birdlife$birdlife_name)
  )

rm(sp_to_check, WNV_prevalence, target_sp_birdlife)

file_stu <- "1_downloadable_studies.csv"
file_dep_all <- "1_downloadable_studies_deployments.csv"
file_dep_filter <- "1_deployments_to_download.csv"
file_down_report <- "1_download_report.csv"


# 1 - Download study metadata---------------------------------------------------

if (!file.exists(file.path(data_dir, file_stu))) {
  
  # movebank Accounts
  movebank_access <- c("movebank", "rbook_account", "kosmickizaborav")
  
  studies <- download_study_metadata(
    movebank_access,
    tag_ids = c("GPS", "Sigfox Geolocation"), 
    save_file = T, 
    file_name = "1_downloadable_studies.csv", 
    file_dir = data_dir
    )

}

# 2 - Download deployment metadata ---------------------------------------------

# Check if deployment file exists
if (!file.exists(file.path(data_dir, file_dep_all))) {
  
  studies <- fread(file.path(data_dir, file_stu))
  
  # extract all study ids from studies
  taxon_ids <- unique(
    squish_base(
      unlist(
        strsplit(studies[!grepl("test|calibration", taxon_ids), taxon_ids], ",")
      )
    )
  )
    
  # Create a taxon filter for genus (any incomplete scientific names) and target species
  taxon_filter <- paste(
    c(grep(" ", taxon_ids, invert = TRUE, value = TRUE), target_sp), 
    collapse = "|"
    )
  
  # Filter studies of interest
  id_to_download <- studies[
      grepl(taxon_filter, taxon_ids) |
        grepl(taxon_filter, name) |
        grepl(taxon_filter, study_objective),
      .(id, account)
    ]
  
  deployments <- download_deployment_metadata(
    id_to_download,
    save_file = T, 
    tag_ids = c("gps", "sigfox-geolocation"), 
    accepted_manipulation =  c("none", "relocated"), 
    file_name = file_dep_all, 
    file_dir = data_dir
    )
  
  rm(studies)
  
}

# 3 - Filter deployments --------------------------------------------------

if(!file.exists(file.path(data_dir, file_dep_filter))){
  
  # filter deployments that contain target species
  deployments <- fread(file.path(data_dir, file_dep_all))
  
  # remove extra spaces in case they are in the strings
  sp_cols <- c("taxon_canonical_name", "taxon_detail")
  deployments[, (sp_cols) := lapply(.SD, function(x) {
    squish_base(x)}), .SDcols = sp_cols]
  
  # create a new column scientific_name, to extract potential species names that 
  # we missed
  deployments_filtered <- deployments[ , scientific_name := taxon_canonical_name ]
  deployments_filtered[ , scientific_name := fcase(
    grepl(" ", scientific_name), scientific_name, 
    # in case family is specified seems like species is in taxon detail
    grepl("idae$|Aves|Animalia", scientific_name), to_sentence_base(taxon_detail),
    grepl(" ", taxon_detail) & (!grepl(" ", scientific_name) | is.na(scientific_name)), taxon_detail,
    # when genus was specified epitaph of the species is in taxon detail
    !grepl(" ", taxon_detail), to_sentence_base(paste(scientific_name, taxon_detail)), 
    default = scientific_name
  ) ]
  deployments_filtered <- deployments_filtered[grepl(" ", scientific_name)]
  
  # match the species names to birdlife names and filter only deployments
  # that contain target species
  deployments_filtered <- rename_to_birdlife(
    deployments_filtered, species_name = "scientific_name"
  )[scientific_name %in% target_sp | birdlife_name %in% target_sp]
  
  # after checking the manipulation comments, we excluded some of the 
  # deployments that seemed to manipulate the animal more than just 
  # relocation
  manipulation_problem <- "restricted|purified|anosmi[ac]|shift|FL|magnetic"
  
  deployments_filtered <- deployments_filtered[, manipulation_type := fcase(
    # if the manipulation type is not specified, but there is a comment
    # that indicates some kind of manipulation
    grepl(manipulation_problem, manipulation_comments), "other",
    # from the comment seems like relocation 
    grepl("[Dd]isplacement", manipulation_comments), "relocated",
    manipulation_type == "", "none", 
    default = manipulation_type)
    ][manipulation_type != "other"]

  # sect columns of interest, from deployment metadata
  col_deploy <- c(
    "taxon_canonical_name", "study_id", "deployment_id",
    "individual_id", "individual_local_identifier", "sensor_type_ids",
    "individual_number_of_deployments",
    "deployment_local_identifier", "tag_id", "tag_local_identifier",
    "sex", "animal_life_stage",  "manipulation_type", "deployment_comments", 
    "deploy_on_timestamp", "deploy_off_timestamp"
  )
  col_deploy <- c("account", "scientific_name", "birdlife_name", col_deploy)
  
  deployments_filtered <- deployments_filtered[, ..col_deploy]
  
  fwrite(deployments_filtered, file.path(data_dir, file_dep_filter))
  
  rm(deployments)
  
} 

rm(file_dep_all, file_stu, target_sp)


# 4 - Download individual deployments -------------------------------------

# Load the filtered deployments data if it already exists
deployments_filtered <- fread(file.path(data_dir, file_dep_filter))[
  , .(account, study_id, individual_id, deployment_id, birdlife_name)
]
 
rm(file_dep_filter)

# create directories for each species
create_dir(
  species = deployments_filtered[, unique(birdlife_name)], 
  new_dir = "1_deployments"
)

download_report <- download_individual_deployments(
    deployments_filtered, 
    tag_ids = c("gps", "sigfox-geolocation"),
    studies_dir = here::here("Data", "Studies"), 
    species_col = "birdlife_name", 
    sub_dir = "1_deployments"
) 

fwrite(download_report, file.path(data_dir, file_down_report))


