#' ---
#' title: "Downloading Movebank data"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------
#' **SECTION 0 - Define parameters and packages**
#' in this section we define variables that are used later in the script:
#' - target_sp - species of interest that we search for in Movebank
#' - tags_ids - sensor type ids that we are interested in, as defined in Movebank
#' - col_deploy - relevant columns from the deployment data
#'  
#' **SECTION 1 - Check available studies** 
#' access all studies from different accounts and filter studies that:
#' filter the sensors of interest 
#' 1 - with download access
#' 2 - have registered deployments
#' 3 - deploy sensors of interest
#' 4 - it is not specified as a test deployment
#' >>> OUTPUT: downloadable_studies.rds
#' 
#' **SECTION 2 - Download deployment information**
#' download deployment information from studies of interest,
#' done in this way, even if it is much slower then just filtering studies,
#' because in study info sometimes there is no species specified, 
#' so to make sure to include all potential studies
#' if download failed keep the error information
#' >>> OUTPUT: downloadable_studies_deployments.rds
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
#' >>> OUTPUT: downloadable_studies_deployments_filtered.rds
#' 
#' **SECTION 4 - Create directories for species with data**
#' create a study folder for every species
# 
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
#' >>> OUTPUT: "1_deployments_download_report.rds" 


# 0 - Define parameters and packages--------------------------------------------

library(move2)
library(tidyverse)
library(here)
library(units)
library(readxl)


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
  read_xlsx() |> 
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

# list of columns that seemed to be relevant, selected by exploring the 
# dataframe downladed from movebank using movebank_download_deployment()
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

# create a directory where data will be downloaded
if(!dir.exists(here("Data"))){ 
  dir.create(here("Data")) 
  dir.create(here("Data", "Studies"))
  }


# 1 - Check available studies ---------------------------------------------

# checking downloadable studies and applying the filtering criteria
output_1 <- here("Data", "1_downloadable_studies.rds")

if(!file.exists(output_1)){
  
  cat("Accessing downloadable studies' information!\n")
  
  # checking all the studies that are available
  movebank_filtered <- movebank_access |> 
    map(~ {
      
      options("move2_movebank_key_name" = .x)
      
      # get info only for the studies with the download access, 
      # originally doenloaded with this argument in the function, but after saw 
      # that it keeps false
      # using all available accounts
      movebank_download_study_info() |> 
        mutate(account = .x)
      
    }) |> 
    bind_rows() |> 
    # it didn't work with just calling it within movebank_download_study_info
    filter(i_have_download_access == TRUE) |> 
    # at least one registered deployment
    filter(
      as.numeric(number_of_deployed_locations) > 0 & 
        !is.na(number_of_deployed_locations)
    ) |> 
    # filter the sensors of interest
    filter(str_detect(sensor_type_ids, str_c(tags_ids, collapse = "|"))) |>
    # filter out test deployments, 
    # no need to include is.na(is_test) because the values are just TRUE/FALSE
    filter(is_test == F) |> 
    # eliminate the studies that can be downloaded by multiple accounts
    distinct(id, .keep_all = T) 
  
  movebank_filtered |>
    write_rds(output_1)
  
} else {
  
  movebank_filtered <- output_1 |> 
    read_rds()
  
  t_output_1 <- round(file.mtime(output_1))
  
  warning(
    "movebank_filtered loaded from a file saved on: ", 
    as.character(t_output_1), 
    "\n"
  )
  
}

rm(output_1)

# 2 - Download deployment information-------------------------------------------

output_2 <- here("Data", "1_downloadable_studies_deployments.rds")

# accessing deployment information and filtering the deployments of interest
# downloading deployments from studies of interest, because in study info 
# in some cases the species is not well defined, 
# so, we did this step to make sure to include all potential studies

# define a safe function in case there was an error during download
safe_deployment_download <- safely(~ {
  movebank_download_deployment(
    .x, 'license-md5' = '51d5f89ddc94971109e50a17eb14f8be'
    )
})

# here we extract all the taxons specified in the movebank_filtered under the 
# column taxon_ids in order to check for studies that only have genus specified
# if we can find species in the deployment information 
# in order to avoid downloading all deployment information, 
# we download only the deployments that contain either explicitly specified
# target species or have some other generic specification such as 
# "Animalia" or genus. originally I downloaded also NA taxon_ids
# but when I downloaded those deployments, they don't specify the species 
# even there, so I excluded them
taxon_movebank <- movebank_filtered |> 
  filter(!is.na(taxon_ids)) |> 
  # removing test and calibration from this list
  mutate(taxon_ids = str_remove_all(taxon_ids, "test|calibration")) |> 
  pull(taxon_ids) |> 
  str_split(",") |>
  unlist() |> 
  unique() 

# not sure why, but in taxon_ids, the genus are specified with a space 
# at the end of the string, e.g. "Pelecanus " instead of "Pelecanus"
# I preserved this bug in order to filter easily - to avoid cleaning taxon_ids, 
# but maybe that should be corrected later

# creating a filter for the taxons of interest
taxon_filter <- str_c(
  c(
    # extracting only genus or families from the movebank taxon list, 
    # if the species is fully specify it will contain " ", otherwise no
    # adding $ to mark the end of the word that will help us distinguish species
    # and genus e.g. in case of "Pelecanus occidentalis" vs "Pelecanus ", 
    # without $ it would match both when I searched "Pelecanus " 
    str_c(taxon_movebank[!str_detect(str_squish(taxon_movebank), " ")], "$"),
    target_sp
  ), 
  collapse = "|"
  )

if(!file.exists(output_2)){
  
  
  # deployment download
  deployments <- movebank_filtered |> 
    # filter taxons of interest or unclear taxons
    filter(
      str_detect(taxon_ids, taxon_filter) | 
        # just in case the species was specified in the study name or objective
        str_detect(name, taxon_filter) | 
        str_detect(study_objective, taxon_filter)
    ) |> 
    # select distinct study IDs and account to download the data
    distinct(id, account) |>  
    group_split(id) |> 
    map(~ {
      
      acc <- .x$account
      id <- .x$id
      
      options("move2_movebank_key_name" = acc)
      
      down <- safe_deployment_download(id)
      
      if (!is.null(down$result)) {  # if there are some results, keep them
        
        tibble(down$result) |> 
          mutate(
            account = acc, 
            error_download_deployment = NA
          )
        
      } else { # if there are no results, save the error and study id
        
        tibble(
          study_id = id, 
          error_download_deployment = paste(
            down[["error"]][["message"]], collapse = " "
          ),
          account = acc
        ) 
        
      }
      
    }, 
    .progress = T
    ) |> 
    bind_rows()
  
  
  # saving the list of all deployments
  deployments |> 
    write_rds(output_2)
  
} else{
  
  deployments <- output_2 |> 
    read_rds()
  
  t_output_2 <- round(file.mtime(output_2))
  
  warning(
    "deployments loaded from a file saved on: ", 
    as.character(t_output_2), 
    "\n"
  )
  
  if(as.Date(t_output_1) != as.Date(t_output_2)){
    
    stop(
      "Files with studies and deployments are not downloaded on the same day!\n",
      "CHECK THE DATA BEFORE PROCEEDING!"
    )
  }
  
}

# remove study info, as it's not needed anymore
rm(
  movebank_filtered, 
  output_2, 
  taxon_movebank, 
  taxon_filter, 
  t_output_1, 
  t_output_2
)
invisible(gc())


# 3 - Filter deployment information--------------------------------------------

# filter deployments
deployments_filtered <- deployments |> 
  # removing extra space just in case
  mutate(
    across(any_of(c("taxon_canonical_name", "taxon_detail")), str_squish)
  ) |> 
  # create a new variable so that we keep the original taxon_canonical_name
  mutate(species = taxon_canonical_name) |> 
  mutate(
    species = case_when(
      str_detect(species, " ") ~ species, 
      # in case family is specified seems like species is in taxon detail
      str_detect(species, "idae$|Aves|Animalia") ~ str_to_sentence(taxon_detail),
      # when genus was specified epitaph of the species is in taxon detail
      !str_detect(taxon_detail, " ") ~ str_to_sentence(paste(species, taxon_detail)),
      .default = species
    )
  ) |> 
  # keep only the species that have the full name
  filter(str_detect(species, " ")) |> 
  # filter target species
  filter(species %in% target_sp) |> 
  # contains sensor type of interest
  filter(str_detect(sensor_type_ids, str_c(tags_ids, collapse = "|"))) |> 
  # no manipulation with tracked animals or relocation only
  filter(
    manipulation_type %in% c("none", "relocated") | is.na(manipulation_type)
  ) |> 
  select(any_of(c("species", "account", col_deploy)))

# saving data
deployments_filtered |> 
  write_rds(here("Data", "1_downloadable_studies_deployments_filtered.rds"))

# proceed only with filtered deployments
rm(deployments)
invisible(gc())


# 4 - Create directories for species with data -------------------------------

# create folder for every species that has downloadable data
deployments_filtered |> 
  distinct(species) |> 
  pull() |>
  map(~{
    
    sp_dir <- here("Data", "Studies", str_replace(.x, " ", "_"))
    
    if(!dir.exists(sp_dir)) {
      
      sp_dir |> dir.create()
      
      here(sp_dir, "1_deployments") |> dir.create()
       
    } # close if dir.exists
    
  }) # close map


# FUNCTION: safe_study_download -------------------------------------------

# Define a custom function to handle multiple arguments and wrap with safely
# sensor types: 653, 2299894820 - gps, sixfox
study_download <- function(
    study_id = NULL, 
    individual_local_identifier = NULL, 
    sensor_type_id =  c("gps", "sigfox-geolocation") # c(653, 2299894820)
    ) {

  # the function gives an error if NULL or NA for individual Local identifier
  # is provided, so we use two versions for the deployments that don't have
  # individual local identifier specified
  if(is.null(individual_local_identifier)){
    
    # it reported errors with the rest of the parameters, so I gave up
    out <- movebank_download_study(
      study_id, 
      sensor_type_id = sensor_type_id,
      remove_movebank_outliers = T,
      omit_derived_data = T,
      convert_spatial_columns = T,
      attributes = "all"
    ) 
    
  } else {
    
    out <- movebank_download_study(
      study_id, 
      sensor_type_id = sensor_type_id,
      individual_local_identifier = individual_local_identifier,
      remove_movebank_outliers = T,
      omit_derived_data = T,
      convert_spatial_columns = T,
      attributes = "all"
    ) 
    
  }
  
  return(out)
}

# wrap with safely
safe_study_download <- safely(study_download)


# FUNCTION: study_download_protocol ---------------------------------------

study_download_protocol <- function(
    account = NULL, 
    s_id = NULL, 
    ind_local = NULL,
    deployment_info = NULL, 
    data_dir = NULL
){
  
  if(!is.null(ind_local)){  cat("\n -individual:", ind_local) }
  
  # extract the deployment columns that we want to add to the tracking data
  deployment_info <- deployment_info |> 
    select(species, account, ends_with("_identifier"), ends_with("_id")) |> 
    select(where(~!all(is.na(.))))
  
  ind_id <- unique(deployment_info$individual_id) 
  depl_ids <- unique(deployment_info$deployment_id)
  sp <- unique(deployment_info$species)
  
  # set account to use for download
  options("move2_movebank_key_name" = account)
  
  # download deployments of interest
  down <- safe_study_download(
    study_id = s_id,
    individual_local_identifier = ind_local
  )
  
  df <- down$result 
  
  # if there is no data to save/error occurred, return it
  if(is.null(df)){ 
    
    cat("\n--------------------an error occurred!---------------------")
    
    # if there is error with download, save the info
    error_report <- deployment_info |>
      mutate(
        error_download_study = paste(
          down[["error"]][["message"]], collapse = " "
        ), 
        downloaded_on = Sys.time()
      )
    
    return(error_report)
    
  }
  
  df <- df |> 
    # adding id and identifier columns for easier management later
    # needed to do this step because in one study, 128184877, 
    # when data for the individual was downloaded, it contained 
    # only tag_local_identifier and individual_local_identifier, 
    # but the deployment info only contained individual_local_identifier, 
    # so when the columns were merged, there was no deployment_id, 
    # because it couldn't match NA from deployment_info 
    # with tag_local_identifier in the data
    left_join(deployment_info) |> 
    # it was important to add deployment_id check because in some
    # cases there was an individual with two deployments, but in one
    # it was manipulated, in the other no
    filter(
      !is.na(deployment_id), 
      individual_id %in% ind_id, 
      deployment_id %in% depl_ids
    ) 
  
  # if there is no data for the specified deployment, don't save the data
  # this is done to make sure that we have the deployments and individuals 
  # that we want
  if(nrow(df) == 0){
    
    cat("\n -deployment id: ", as.character(depl_ids))
    cat("\n----------------no data for the deployment!----------------")
    
    # if there is no data to save
    no_data_report <- deployment_info |> 
      mutate(
        error_download_study = "no data for the specified deployment id", 
        downloaded_on = Sys.time() 
      )
    
    return(no_data_report)
    
  }
  
  saved_report <- df |>
    # if there are multiple deployments per individual save them separately
    group_split(deployment_id) |>
    map(~{
      
      # taking out all the columns that have only NA values
      depl_df <- .x |>
        select(where(~!all(is.na(.))))
      
      depl_id <- unique(depl_df$deployment_id)
      ind_id <- unique(depl_df$individual_id)
      
      depl_info <- deployment_info |> 
        filter(deployment_id == depl_id, individual_id == ind_id)
      
      # define file name
      study_file <- str_c(
        s_id, "_study_", ind_id, "_ind_", depl_id, "_depl.rds"
      )
      
      # save the data
      depl_df |>
        write_rds(file = file.path(data_dir, study_file))
      
      cat("\n -deployment id: ", as.character(depl_id))
      cat("\n---------------------deployment saved!---------------------")
      
      # save the basic information for download report
      depl_info |> 
        mutate(
          n_locs = nrow(depl_df),
          downloaded_on = Sys.time()
        )
      
    }) |>
    bind_rows()
  
  return(saved_report)
  
}

# 5 - Download and save deployments ---------------------------------------

# download studies 
studies <- unique(deployments_filtered$study_id)
ls <- length(studies)

# checking which data is already downloaded 
downloaded_df <- target_sp |> 
  map(~{
    
    sp <- .x 
    sp_dir <- here("Data", "Studies", str_replace(sp, " ", "_"))
    
    # check what deployments are already downloaded
    tibble(file = list.files(here(sp_dir, "1_deployments"))) |> 
      mutate(
        study_id = bit64::as.integer64(str_split_i(file, "_", 1)),
        individual_id = bit64::as.integer64(str_split_i(file, "_", 3)),
        deployment_id = bit64::as.integer64(str_split_i(file, "_", 5)),
        species = sp, 
        downloaded_on = file.mtime(here(sp_dir, "1_deployments", file))
      ) 
    
  }) |> 
  bind_rows()

    
# downloading studies one by one
download_report <- deployments_filtered |> 
  arrange(species) |> 
  select(species, account, ends_with("_identifier"), ends_with("_id")) |> 
  group_split(study_id, individual_local_identifier) |>
  map(~{

    deployment_info <- .x 
    # account to use when downloading data
    account <- unique(deployment_info$account)
    # specifying study and individual that we want to download
    s_id <- unique(deployment_info$study_id)
    # getting individual ids
    ind_id <- unique(deployment_info$individual_id)
    ind_local <- unique(deployment_info$individual_local_identifier) |> 
      as.character()
    
    # folder where the data will be saved
    sp <- unique(deployment_info$species) 
    sp_dir <- here("Data", "Studies",  str_replace(sp, " ", "_"))
    
    cat("\n\n", sp, which(s_id == studies), "|", ls)
    
    if(is.na(ind_local)){ ind_local <- NULL }
    
    downloaded <- downloaded_df |> 
      filter(species == sp, study_id == s_id, individual_id %in% ind_id)

    # IF: data not downloaded
    # check if the individual and study are already downloaded
    if(nrow(downloaded) == 0){
      
      study_download_protocol(
        account = account, 
        s_id = s_id, 
        ind_local = ind_local,
        deployment_info = deployment_info, 
        data_dir = here(sp_dir, "1_deployments")
      )
      
    } else { # if individual and study are already downloaded 
      
      # ELSE: data already downloaded
      
      cat("\n -individual:", ind_local) 
      cat("\n--------------------already downloaded!--------------------")
      
      # keep the info from the downloaded file for the report
      deployment_info |> 
        left_join(
          downloaded, 
          by = c("species", "study_id", "individual_id", "deployment_id")
        )
      
    }
    
  },
  .progess = T
  ) |> 
  bind_rows() 

rm(deployments_filtered, downloaded_df)
invisible(gc())

# 6 - Retry download for studies with error ------------------------------------

# assuming that the error is caused by the way the individual local identifier
# was specified we retry downloading only using the study_id

download_report <- download_report |> 
  group_split(study_id, error_download_study) |>
  map(~{
    
    report <- .x
    error_msg <- unique(report$error_download_study)
    s_id <- unique(report$study_id)
    account <- unique(report$account)
    
    # folder where the data will be saved
    sp <- unique(report$species)
    sp_dir <- here("Data", "Studies",  str_replace(sp, " ", "_"))
    
    if(!is.na(error_msg)){
      
      cat("\n\n", sp, which(s_id == studies), "|", ls)
      cat("\n-------------retrying download for the study!--------------")
      
      study_download_protocol(
        account = account, 
        s_id = s_id, 
        ind_local = NULL,
        deployment_info = report, 
        data_dir = here(sp_dir, "1_deployments")
        )
      
    } else {
    
      report
        
    }

  }) |> 
  bind_rows()

# save the download report
download_report |> 
  write_rds(here("Data", "1_deployments_download_report.rds"))

# we can always transform it into csv later
# download_report |> 
#   write_csv(here("Data", "Studies", "1_individuals_download_report.csv"))


# There were 50 or more warnings (use warnings() to see the first 50)
# 50: `vroom()` finds reading problems with the movebank specification.
# ℹ This might relate to the returned data not fitting the expectation of the movebank data format specified in the package.
# ℹ For retrieving the specific problem you can enable `global_entrace` using rlang::global_entrace() then run the command and use
# `rlang::last_warnings()[[1]]$problems` to retrieve the problems.
# ℹ The requested url can then be retrieved with: `rlang::last_warnings()[[1]]$url`
# ℹ Alternatively in some cases you might be able to retrieve the problems calling `vroom::problems()` on the result of the function call
# that produced the warning.
