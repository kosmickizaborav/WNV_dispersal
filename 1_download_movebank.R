#' ---
#' title: "Downloading Movebank data"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' **SECTION 1 - Checking available deployments** 
#' 
#' **1.1. Accessing downloadable studies**
#' accessing all studies from different accounts and filter studies that:
#' 1 - have registered deployments
#' 2 - that deploy sensors of interest
#' 3 - with download access
#' 4 - not test deployments
#' >>> OUTPUT: downloadable_studies.csv
#' 
#' **1.2. Downloading deployment info and filter**
#' download deployment information from studies of interest. 
#' done in this way because in study info sometimes there is no species specified.
#' if download failed keep the error info
#' >>> OUTPUT: downloadable_studies_deployments.csv
# 
#' filtering deployments that include:
#' 1 - species of interest
#' 2 - sensor type of interest
#' 3 - no manipulation with the tracked animal 
#'     but everything saved, so we can filter it later
#' >>> OUTPUT: downloadable_studies_deployments_filtered.csv
#' 
#' **1.3. Creating folder for every species**
#' create a study folder for every species
# 
#' **SECTION 2 - Downloading deployments of interest**
#' - download the tracking data by specifying: individual_local_identifier,
#'     sensor_type_id, and study_id from before
#' - keep track of errors
#' >>> dowload report saved

# 0 - Defining parameters of interest -------------------------------------

library(move2)
library(tidyverse)
library(here)
library(units)


# defining species of interest
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

# defining sensors of interest 
# 
tags_ids <- c(
  "GPS", "Sigfox Geolocation", # sensor type ids from studies
  "gps", "sigfox-geolocation" # sensor type ids from deployments
  )


# defining columns from deployment
col_deploy <- c("taxon_canonical_name", "study_id", "deployment_id", 
                "individual_id", "individual_local_identifier", 
                "sensor_type_ids",
                "individual_number_of_deployments", 
                "deployment_local_identifier", "tag_id", "tag_local_identifier", 
                "sex", "animal_life_stage",  "manipulation_type",
                "manipulation_comments"
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

if(!dir.exists(here("Data"))){
  dir.create(here("Data"))
}


# 1 - Checking available deployments --------------------------------------


# 1.1. Accessing downloadable studies -------------------------------------

# checking downloadable studies and applying the filtering criteria

# checking all the studies that are available
movebank_filtered <- movebank_access |> 
  map(~ {
    
    options("move2_movebank_key_name" = .x)
    
    # get info only for the studies with the downlaod acess
    movebank_download_study_info(i_have_download_access = TRUE) |> 
      mutate(account = .x)
    
  }) |> 
  bind_rows() |> 
  # at least one registered deployment
  filter(
    as.numeric(number_of_deployed_locations) > 0 & !is.na(number_of_deployed_locations)
  ) |>
  # filter the sensors of interest
  filter(str_detect(sensor_type_ids, str_c(tags_ids, collapse = "|"))) |>
  # filter out test deployments
  filter(is_test != TRUE) |> 
  # eliminate the studies that can be downloaded by multiple accounts
  distinct(id, .keep_all = T) 

movebank_filtered |>
  write_csv(here("Data", "1_downloadable_studies.csv"))


# 1.2. Downloading deployment info and filter -----------------------------

# accessing deployment information and filtering the deployments of interest

# define a safe version of the function to handle errors
safe_deployment_download <- safely(~ {
  movebank_download_deployment(
    .x, 'license-md5' = '51d5f89ddc94971109e50a17eb14f8be'
    )
})

# downloading all available deployments from studies of interest
# this is done first because in the study info in some cases there
#  is no species defined
deployments <- movebank_filtered |> 
  filter(
    sum(unlist(str_split(taxon_ids, ",")) %in% c(target_sp, "Animalia")) > 0 | 
             is.na(taxon_ids)
  ) |> 
  distinct(id, account) |>  # Select distinct study IDs
  group_split(id) |> 
  map(~ {
    
    acc = unique(.x$account)
    
    options("move2_movebank_key_name" = acc)
    
    res <- safe_deployment_download(.x$id)
    
    
    if (!is.null(res$result)) {  # if there are some results, keep them
      
      tibble(res$result) |> 
        mutate(account = acc)
      
    } else { # if there are no results, save the error and study id
      
      tibble(
        study_id = .x$id, 
        error_text = as.character(res$error), 
        account = acc
      ) 
      
    }
    
  }, 
  .progress = T
  ) |> 
  bind_rows() |> 
  rename(species = taxon_canonical_name) 


# saving the list of all deployments
deployments |> 
  write_rds(here("Data", "1_downloadable_studies_deployments.rds"))
  # write_csv(here("Data", "1_downloadable_studies_deployments.csv"))

rm(movebank_filtered)

# filter deployments
deployments_filtered <- deployments |> 
  # contains species of interest
  filter(species %in% target_sp) |> 
  # contains sensor type of interest
  filter(str_detect(sensor_type_ids, str_c(tags_ids, collapse = "|"))) |> 
  # no manipulation with tracked animals
  filter(
    manipulation_type %in% c("none", "relocated") | is.na(manipulation_type)
  ) |>
  select(any_of(c("species", "account", col_deploy)))


# saving in two formats, so that when/if needed to be reloaded for download 
# we don't have incompatibility with data types 
# (e.g. deployment id as double or integer64)
deployments_filtered |> 
  write_rds(here("Data", "1_downloadable_studies_deployments_filtered.rds"))

deployments_filtered |> 
  as_tibble() |> 
  write_csv(here("Data", "1_downloadable_studies_deployments_filtered.csv"))

rm(deployments)

# warning x10:
# Warning messages:                                  
#   1: `vroom()` finds reading problems with the movebank specification.
# ℹ This might relate to the returned data not fitting the expectation of
#  the movebank data format specified in the package.
# ℹ For retrieving the specific problem you can enable `global_entrace` 
# using rlang::global_entrace() then run the command and use
# `rlang::last_warnings()[[1]]$problems` to retrieve the problems.
# ℹ The requested url can then be retrieved with:
# `rlang::last_warnings()[[1]]$url`
# ℹ Alternatively in some cases you might be able to retrieve the problems 
# calling `vroom::problems()` on the result of the function call that
# produced the warning. 


# 2 - Downloading studies per species -------------------------------------

# I first downloaded deployments, and the day after I ran the following code
# deployments_filtered <- here(
#   "Data", "1_downloadable_studies_deployments_filtered.rds"
#   ) |>
#   read_rds()

# 2.1. Creating folder for each species ----------------------------------

# creating directory for data download
if(!dir.exists(here("Data", "Studies"))){
  
  dir.create(here("Data", "Studies"))
  
}

# creating folder for every species that has downloadable data
deployments_filtered |> 
  distinct(species) |> 
  as_vector() |>
  map(~{
    
    sp_dir <- here("Data", "Studies", str_replace(.x, " ", "_"))
    
    if(!dir.exists(sp_dir)) {
      
      sp_dir |> dir.create()
       
    }
    
    if(!dir.exists(here(sp_dir, "1_deployments"))){
      
      here(sp_dir, "1_deployments") |> dir.create()
      
    }
    
  }
  )

# 2.2 - Downloading deployments of interest --------------------------------

# Define a custom function to handle multiple arguments and wrap with safely
# sensor types: 653, 2299894820 - gps, sixfox

study_download <- function(
    study_id = NULL, 
    individual_local_identifier = NULL, 
    sensor_type_id =  c("gps", "sigfox-geolocation"), # c(653, 2299894820)
    ...
    ) {

  # it reported errors with the rest of the parameters, so I gave up
  movebank_download_study(
    study_id, 
    sensor_type_id = sensor_type_id,
    individual_local_identifier = individual_local_identifier,
    remove_movebank_outliers = T,
    omit_derived_data = T,
    convert_spatial_columns = T,
    attributes = "all"
    ) 
  
}

safe_study_download <- safely(study_download)
  
# download studies 
studies <- unique(deployments_filtered$study_id)

download_report <- deployments_filtered |>
  group_split(study_id, individual_local_identifier) |>
  map(~{

    deployment_info <- .x
    
    sp <- unique(deployment_info$species) |> str_replace(" ", "_")
    sp_dir <- here("Data", "Studies", sp, "1_deployments")
    
    account <- unique(deployment_info$account)

    # specifying study and individual that we want to download
    study_id <- unique(deployment_info$study_id)
    
    print(paste(sp, study_id, which(study_id == studies), "|", length(studies)))

    ind_local <- unique(deployment_info$individual_local_identifier) |>
      as.character()
    
    ind_id <- unique(deployment_info$individual_id) |>
      bit64::as.integer64()

    print(paste("individual:", ind_local))
    
    downloaded <- tibble(file = list.files(sp_dir)) |> 
      mutate(
        study_id = bit64::as.integer64(str_split_i(file, "_", 1)), 
        individual_id = bit64::as.integer64(str_split_i(file, "_", 3)), 
        deployment_id = bit64::as.integer64(str_split_i(file, "_", 5))
      ) 

    if(
      !(study_id %in% unique(downloaded$study_id) & 
        ind_id %in% unique(downloaded$individual_id))
      ){
      
      options("move2_movebank_key_name" = account)
      
      # download deployments of interest
      res <- safe_study_download(
        study_id,
        individual_local_identifier = ind_local
      )
      
      df <- res$result
      
      if(!is.null(df)){
        
        # add additional deployment information
        df <- df |>
          left_join(deployment_info)
        
        df |>
          group_split(deployment_id) |>
          map(~{
            
            depl_id <- unique(.x$deployment_id)
            ind_id <- unique(.x$individual_id)
            
            study_file <- str_c(
              study_id, "_study_",
              ind_id, "_ind_",
              depl_id, "_depl.rds"
            )
            
            .x |>
              write_rds(file = here(sp_dir, study_file))
            
            print("----------------------saved!------------------------")
            
            tibble(
              species = sp,
              file = study_file,
              study_id = study_id,
              individual_local_identifier = ind_local,
              individual_id = ind_id,
              deployment_id = depl_id,
              account = account, 
              downloaded_at = Sys.time()
              # deploy_total = length(deployment_ids),
              # deploy_download = length(unique(df$deployment_id))
            )
            
          }) |>
          bind_rows()
        
        
      } else {
        
        
        print("-----------------an error occurred!-----------------")
        
        # if there is error with download, save the info
        # if there is error with download, save the info
        tibble(
          study_id = study_id,
          individual_local_identifier = ind_local, 
          error_text = paste(res$error, collapse = " "), 
          account = account, 
          downloaded_at = Sys.time()
        )
        
      }
      
    } else {
      
      print("-----------------already downloaded!-----------------")
      
      study_file <- downloaded |> 
        filter(study_id == study_id & individual_id == ind_id)
      
      tibble(
        species = sp,
        file = study_file$file,
        study_id = study_id,
        individual_local_identifier = ind_local,
        individual_id = study_file$individual_id,
        deployment_id = study_file$deployment_id,
        account = account, 
        downloaded_at = file.mtime(here(sp_dir, study_file$file))
        # deploy_total = length(deployment_ids),
        # deploy_download = length(unique(df$deployment_id))
      )
      
    }
    

  },
  .progess = T
  ) |> 
  bind_rows()


download_report |> 
  write_csv(here("Data", "Studies", "1_individuals_download_report.csv"))



# There were 50 or more warnings (use warnings() to see the first 50)
# 50: `vroom()` finds reading problems with the movebank specification.
# ℹ This might relate to the returned data not fitting the expectation of the
# movebank data format specified in the package.
# ℹ For retrieving the specific problem you can enable `global_entrace` using
# rlang::global_entrace() then run the command and use
# `rlang::last_warnings()[[1]]$problems` to retrieve the problems.
# ℹ The requested url can then be retrieved with: `rlang::last_warnings()[[1]]$url`
# ℹ Alternatively in some cases you might be able to retrieve the problems\
# calling `vroom::problems()` on the result of the function call that
# produced the warning.
