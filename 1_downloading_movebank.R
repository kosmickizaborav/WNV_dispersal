
# INFO --------------------------------------------------------------------

# 1 - Checking available deployments 
# - accessing all studies and filter those: 
# 1 - have registered deployments
# 2 - that deploy sensors of interest
# 3 - with download access
# 4 - not a test deployments
# >>> saved as downloadable_studies.csv
# 
# - download all the available deployment information from studies of interest
#     done like this because in study info sometimes there is no species info
# - if download failed keep the error info
# >>> saved as downloadable_studies_deployments.csv
# 
# - filtering deployments that include: 
# 1 - species of interest
# 2 - sensor type of interest
# 3 - no manipulation with the tracked animal
# >>> saved as downloadable_studies_deployments_filtered.csv
# 
# 2 - Downloading deployments of interest 
# - download the tracking data by specifying: individual_local_identifier, 
#     sensor_type_id, and study_id from before
# - keep track of errors
# >>> dowload report saved

# 0 - Defining parameters of interest -------------------------------------

library(move2)
library(tidyverse)
library(here)
library(units)
library(sf)


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
                "individual_number_of_deployments", 
                "deployment_local_identifier", "tag_local_identifier", 
                "sex", "animal_life_stage",  "manipulation_type", "error_text", 
                "account"
)


# defining columns to keep for each study
col_study <- c(
  "species", "individual_local_identifier", "deployment_id", "timestamp", 
  "geometry", "event_id", "sensor_type_id", "gps_hdop", "gps_dop", 
  "gps_vdop", "individual_id", 
  "study_id", "tag_id", "tag_local_identifier", "algorithm_marked_outlier", 
  "import_marked_outlier", "manually_marked_outlier"
)


# accessing data from two different movebank accounts
# additional account that is from the Rbook on movement
# key_name = "rbook_account"
# options("move2_movebank_key_name" = "movebank")
# for downlaoding pigeon study: 
# ID 897179972Ornitela_Pigeon_Columba livia_Spiegel_Israel 

movebank_access <- list(
  main = "movebank", 
  sub1 = "rbook_account"
)


# 1 - Checking available deployments --------------------------------------

# filtering only the studies that:
# 1 - have registered deployments
# 2 - that deploy sensors of interest
# 3 - with download access

# checking all the studies that are available - 6992 studies
movebank_filtered <- movebank_access |> 
  map(~ {
    
    options("move2_movebank_key_name" = .x)
    
    movebank_retrieve(entity_type = "study") |> 
      mutate(account = .x)
    
  }) |> 
  bind_rows() |> 
  drop_units() |>
  # at least one registered deployment
  filter(
    number_of_deployed_locations > 0 & !is.na(number_of_deployed_locations)
  ) |>
  # filter the sensors of interest
  filter(str_detect(sensor_type_ids, str_c(tags_ids, collapse = "|"))) |>
  # filter studies with download access
  filter(i_have_download_access == TRUE) |> 
  # filter out test deployments
  filter(is_test != TRUE) |> 
  # eliminate the studies that can be downloaded by multiple accounts
  filter(!duplicated(id))

movebank_filtered |>
  write_csv(here("Data", "downloadable_studies.csv"))

# define a safe version of the function to handle errors
safe_deployment_download <- safely(~ {
  movebank_download_deployment(
    .x, 'license-md5' = '51d5f89ddc94971109e50a17eb14f8be'
    )
})

# downloading all available deployments from studies of interest
deployments <- movebank_filtered |> 
  distinct(id, account) |>  # Select distinct study IDs
  group_split(id) |> 
  map(~ {
    
    options("move2_movebank_key_name" = unique(.x$account))
    
    res <- safe_deployment_download(.x$id)
    
    
    if (!is.null(res$result)) {  # if there are some results, keep them
      
      tibble(res$result)
      
    } else { # if there are no results, save the error and study id
      
      tibble(study_id = .x$id, error_text = as.character(res$error))
      
    }
    
  }, 
  .progress = T
  ) |> 
  bind_rows() |>  
  left_join(
    movebank_filtered |> 
      rename(study_id = id) |> 
      select(study_id, account)
  )

# saving the list of all deployments
deployments |> 
  write_csv(here("Data", "downloadable_studies_deployments.csv"))

rm(movebank_filtered)

# filter deployments that include: 
# 1 - species of interest
# 2 - sensor type of interest
# 3 - no manipulation with the tracked animal
deployments_filtered <- deployments |> 
  filter(taxon_canonical_name %in% target_sp) |> 
  filter(
    str_detect(sensor_type_ids, str_c(tags_ids, collapse = "|"))
  ) |> 
  filter(manipulation_type == "none" | is.na(manipulation_type))


# saving in two formats, so that when/if needed to be reloaded for download 
# we don't have incompatibility with data types 
# (e.g. deployment id as double or integer64)
deployments_filtered |> 
  saveRDS(
    here("Data", "downloadable_studies_deployments_filtered.rds"), 
    compress = T
  )

deployments_filtered |> 
  write_csv(here("Data", "downloadable_studies_deployments_filtered.csv"))

rm(deployments)

# 2 - Downloading deployments of interest --------------------------------

# Define a custom function to handle multiple arguments
# and wrap the custom function with safely
# sensor types: 653, 2299894820 - gps, sixfox
study_download <- function(
    study_id = NULL, 
    individual_local_identifier = NULL, 
    sensor_type_id = c(653, 2299894820),
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

if(!dir.exists(here("Data", "Studies"))){
  dir.create(here("Data", "Studies"))
}

download_report <- deployments_filtered|> 
  select(all_of(col_deploy)) |> # check section 0
  rename(species = taxon_canonical_name) |> 
  group_split(study_id) |> 
  map(~{
    
    # specifying study and individuals that we want to download
    deployment_info <- .x
    account <- unique(.x$account)
    study_id <- unique(deployment_info$study_id)
    individuals <- unique(deployment_info$individual_local_identifier) |> 
      as.character()
    deployment_ids <- unique(deployment_info$deployment_id)
    
    print(paste(study_id, which(study_id == studies), "|", length(studies), 
          account))
    
    options("move2_movebank_key_name" = account)
    
    # download deployments of interest
    res <- safe_study_download(
      study_id,
      individual_local_identifier = individuals
      )
      
    df <- res$result
   
     # if there is data, save it
      if(!is.null(df)){ 
        
        study_file <- str_c(study_id, "_study.rds")
        
        # add additional deployment information
       df <- df |> 
          left_join(deployment_info) 
       
       df |> 
          saveRDS(
            file = here("Data", "Studies", study_file), 
            compress = T
          )
        # tibble(df) |> 
        #   write_csv(study_file)
        
        tibble(
          study_id = study_id, 
          file = study_file,
          deploy_download = length(unique(df$deployment_id)),
          deploy_total = length(deployment_ids), 
          projection = as.character(st_crs(df$geometry))[1], 
          time_zone = tz(df$timestamp)
          )
              
      } else {
        
        # if there is error with download, save the info
        tibble(
          study_id = study_id, 
          error_text = paste(res$error, collapse = " "), 
          deploy_total = length(deployment_ids)
        )
        
      }
    
  }, 
  .progess = T
  ) |> 
  bind_rows() 

download_report |> 
  write_csv(here("Data", "Studies", "studies_download_report.csv"))


# Warning messages X10
# Warning messages:                                                                                                                    
#   1: `vroom()` finds reading problems with the movebank specification.
# ℹ This might relate to the returned data not fitting the expectation of 
# the movebank data format specified in the package.
# ℹ For retrieving the specific problem you can enable `global_entrace` 
# using rlang::global_entrace() then run the command and use
# `rlang::last_warnings()[[1]]$problems` to retrieve the problems.
# ℹ The requested url can then be retrieved with: 
# `rlang::last_warnings()[[1]]$url`
# ℹ Alternatively in some cases you might be able to retrieve the problems 
# calling `vroom::problems()` on the result of the
# function call that produced the warning. 

