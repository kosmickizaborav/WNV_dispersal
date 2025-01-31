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
#' done in this way, even if it is much slower, 
#' because in study info sometimes there is no species specified, 
#' so to make sure to include all potential studies
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
# because they are written in different ways
tags_ids <- c(
  "GPS", "Sigfox Geolocation", # sensor type ids from studies
  "gps", "sigfox-geolocation" # sensor type ids from deployments
  )

# list of columns that seemed to be relevant, selected by exploring the 
# dataframe downladed from movebank using movebank_download_deployment()
col_deploy <- c("taxon_canonical_name", "study_id", "deployment_id", 
                "individual_id", "individual_local_identifier", 
                "sensor_type_ids",
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
  # didn't work with just calling it within movebank_download_study_info
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
  write_csv(here("Data", "1_downloadable_studies.csv"))

movebank_filtered |>
  write_rds(here("Data", "1_downloadable_studies.rds"))


# 2 - Download deployment info and filter  --------------------------------

# accessing deployment information and filtering the deployments of interest

# downloading available deployments from studies of interest
# this is done first because in the study info in some cases there
# is no species defined, so to make sure to include all potential studies

# define a safe version of the function to handle errors
# in case there was an error during download save it
safe_deployment_download <- safely(~ {
  movebank_download_deployment(
    .x, 'license-md5' = '51d5f89ddc94971109e50a17eb14f8be'
    )
})

# in order to avoid downloading all deployment information, 
# we download only the deployments that contain either explicitly specified
# target species or have some other generic specification such as 
# "Animalia", NA value, or genus  

taxon_list <- movebank_filtered |> 
  filter(!is.na(taxon_ids)) |> 
  pull(taxon_ids) |> 
  str_split(",") |>
  unlist() |> 
  unique() 

# !str_detect(str_squish(taxon_list), " "), this is used to differentiate 
# from fully specified species and only genus, for the same reason needed to 
# add $ at the end of the string, to distinguish species from genus
# e.g. when using str_detect("Pelecanus "), it would include also
# "Pelecanus onocrotalus", but since in that case the species is fully specified, 
# we don't want to download the data, but only to check which species is exacly 
# under the deployment marked as "Pelecanus $"
taxon_filter <- str_c(
  c(
    str_c(taxon_list[!str_detect(str_squish(taxon_list), " ")], "$"), 
    target_sp
  ), 
  collapse = "|"
  )

# deployment download
deployments <- movebank_filtered |> 
  # filter taxons of interest or unclear taxons
  filter(str_detect(taxon_ids, taxon_filter) | is.na(taxon_ids)) |> 
  # select distinct study IDs
  distinct(id, account) |>  
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
        error_download_deployment = as.character(res$error), 
        account = acc
      ) 
      
    }
    
  }, 
  .progress = T
  ) |> 
  bind_rows()


# saving the list of all deployments
deployments |> 
  write_rds(here("Data", "1_downloadable_studies_deployments.rds"))

# remove study info, as it's not needed anymore
rm(movebank_filtered)
gc()

# filter deployments
deployments_filtered <- deployments |> 
  # wanted to keep the original taxon canonical name
  mutate(species = taxon_canonical_name) |> 
  # removing extra space just in case
  mutate(
    across(
      any_of(c("taxon_canonical_name", "species", "taxon_detail")), str_squish
    )
  ) |> 
  mutate(
    species = case_when(
      str_detect(species, " ") ~ species, 
      # in case family is specified seems like taxon detail has the species
      str_detect(species, "idae$|Aves") ~ str_to_sentence(taxon_detail),
      # in case genus was specified
      # seems like in taxon detail they put epitaph of the species
      !str_detect(taxon_detail, " ") ~ str_to_sentence(paste(species, taxon_detail)),
      .default = species
    )
  ) |>
  filter(species %in% target_sp) |> 
  # contains sensor type of interest
  filter(str_detect(sensor_type_ids, str_c(tags_ids, collapse = "|"))) |> 
  # no manipulation with tracked animals
  filter(
    manipulation_type %in% c("none", "relocated") | is.na(manipulation_type)
  ) |>
  select(
    any_of(c("species", "account", "error_download_deployment", col_deploy))
  )

# saving data
deployments_filtered |> 
  write_rds(here("Data", "1_downloadable_studies_deployments_filtered.rds"))

# proceed only with filtered deployments
rm(deployments)
gc()


# 3 - Create output directory for species with available data -------------

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


# 4 - Download and save deployments ---------------------------------------

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
# wraph with safely
safe_study_download <- safely(study_download)
  
# download studies 
studies <- unique(deployments_filtered$study_id)
ls <- length(studies)


# checking which data is already downloaded 
downloaded_df <- target_sp |> 
  map(~{
    
    sp <- .x 
    sp_dir <- here(
      "Data", "Studies",  str_replace(sp, " ", "_"), "1_deployments"
    )
    
    # check what deployments are already downloaded
    tibble(file = list.files(sp_dir)) |> 
      mutate(
        study_id = bit64::as.integer64(str_split_i(file, "_", 1)), 
        individual_id = bit64::as.integer64(str_split_i(file, "_", 3)), 
        deployment_id = bit64::as.integer64(str_split_i(file, "_", 5)), 
        species = sp
      ) 
    
  }) |> 
  bind_rows()


# downloading studies one by one
download_report <- deployments_filtered |>
  group_split(study_id, individual_local_identifier) |>
  map(~{

    deployment_info <- .x
    
    # folder where the data will be saved
    sp <- unique(deployment_info$species) 
    sp_dir <- here(
      "Data", "Studies",  str_replace(sp, " ", "_"), "1_deployments"
      )
    
    # check what deployments are already downloaded
    downloaded <- downloaded_df |> 
      filter(species == sp)
    
    # account to use when downloading data
    account <- unique(deployment_info$account)

    # specifying study and individual that we want to download
    study_id <- unique(deployment_info$study_id)
    
    print(paste(sp, which(study_id == studies), "|", ls))

    # getting individual ids
    ind_local <- unique(deployment_info$individual_local_identifier) |>
      as.character()
    ind_id <- unique(deployment_info$individual_id) |>
      bit64::as.integer64()

    print(paste("individual:", ind_local))
    
    # check if the individual and study are already downloaded
    if(
      !(study_id %in% unique(downloaded$study_id) & 
        ind_id %in% unique(downloaded$individual_id)
        )
      ){
      
      # set account to use for download
      options("move2_movebank_key_name" = account)
      
      # download deployments of interest
      res <- safe_study_download(
        study_id,
        individual_local_identifier = ind_local
      )
      
      df <- res$result
      
      # if there is some data to save
      if(!is.null(df)){
        
        # add additional deployment information
        df <- df |>
          left_join(deployment_info) |> 
          filter(!is.na(individual_id))
        
        df |>
          # if there is multiple deployments per individual download them 
          # to a separate file
          group_split(deployment_id) |>
          map(~{
            
            depl_id <- unique(.x$deployment_id)
            # ind_id <- unique(.x$individual_id) already exists above
            
            # define file name
            study_file <- str_c(
              study_id, "_study_",
              ind_id, "_ind_",
              depl_id, "_depl.rds"
            )
            
            # save the data
            .x |>
              write_rds(here(sp_dir, study_file))
            
            print("----------------------saved!------------------------")
            
            # save the basic information for download report
            tibble(
              species = sp,
              file = study_file,
              study_id = study_id,
              individual_local_identifier = ind_local,
              individual_id = ind_id,
              deployment_id = depl_id,
              n_locs = nrow(.x),
              account = account, 
              downloaded_at = Sys.time()
            )
            
          }) |>
          bind_rows()
        
        
      } else { # if there is no data to save / error occurred!
        
        
        print("-----------------an error occurred!-----------------")
        
        # if there is error with download, save the info
        tibble(
          species = sp, 
          study_id = study_id,
          individual_local_identifier = ind_local,
          individual_id = ind_id,
          error_download_study = paste(res$error, collapse = " "), 
          account = account, 
          downloaded_at = Sys.time()
        )
        
      }
      
    } else { # if individual and study are already downloaded 
      
      print("-----------------already downloaded!-----------------")
      
      # keep the info from the downloade files for the report
      
      # filter the studies that are already downloaded
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
        downloaded_at = file.mtime(here(sp_dir, study_file$file)), 
        already_downloadded = T
      )
      
    }
    

  },
  .progess = T
  ) |> 
  bind_rows()

# save the download report
download_report |> 
  write_csv(here("Data", "Studies", "1_individuals_download_report.csv"))


# There were 50 or more warnings (use warnings() to see the first 50)
# 50: `vroom()` finds reading problems with the movebank specification.
# ℹ This might relate to the returned data not fitting the expectation of the movebank data format specified in the package.
# ℹ For retrieving the specific problem you can enable `global_entrace` using rlang::global_entrace() then run the command and use
# `rlang::last_warnings()[[1]]$problems` to retrieve the problems.
# ℹ The requested url can then be retrieved with: `rlang::last_warnings()[[1]]$url`
# ℹ Alternatively in some cases you might be able to retrieve the problems calling `vroom::problems()` on the result of the function call
# that produced the warning.

# no download problems
# check <- download_report |>
#   filter(!is.na(error_download_study)) 
