
# FUNCTION: download_study_metadata ---------------------------------------

download_study_metadata <- function(
    movebank_accounts, 
    tag_ids = c("GPS", "Sigfox Geolocation"), 
    save_file = T, 
    file_name = NULL, 
    file_dir = getwd()
) {
  
  # names extracted from movebank_retrieve(entity_type = "tag_type")$name
  tag_name <- c(
    "Bird Ring", "GPS", "Radio Transmitter", "Argos Doppler Shift", 
    "Natural Mark", "Acceleration", "Solar Geolocator", 
    "Accessory Measurements", "Solar Geolocator Raw", "Barometer",
    "Magnetometer", "Orientation", "Solar Geolocator Twilight",
    "Acoustic Telemetry", "Gyroscope", "Heart Rate",
    "Sigfox Geolocation", "Proximity", "Geolocation API", "GNSS",
    "Derived", "TDR", "ATLAS Geolocation"
  )
  
  #cCheck if the input is valid
  if(!is.vector(movebank_accounts) || !is.character(movebank_accounts)) {
    stop("The 'movebank_accounts' parameter must be a character vector of Movebank account names.")
  }
  
  if(!is.character(tag_ids) || all(tag_ids %in% tag_name) == F) {
    stop("The 'tags_ids' parameter must be a character vector with the correct tag_type names.")
  }
  
  if(save_file == T && is.null(file_name)){
    stop("If 'save_file' is TRUE, please provide a file name.")
  }
  
  # initialize the result by combining filtered studies across accounts
  studies_list <- lapply(movebank_accounts, function(account) {
    
    message(sprintf("Downloading Movebank data from the account: %s", account))
    
    # set the Movebank API key for the current account
    options("move2_movebank_key_name" = account)
    
    # Download and filter study information
    as.data.table(movebank_download_study_info())[
      # ensure the account has download access
      i_have_download_access == TRUE &
        # exclude studies without deployed locations
        !is.na(number_of_deployed_locations) &  
        as.numeric(number_of_deployed_locations) > 0 &  
        # download only studies with the matching sensor type IDs
        grepl(paste(tag_ids, collapse = "|"), sensor_type_ids) &  
        # exclude test studies
        !is_test  
    ][, account := account]  # sdd the account name to the filtered data
    
  })
  
  studies <- rbindlist(studies_list)[!duplicated(id)]  # remove duplicate study IDs
  
  # save file if specifies
  if(save_file){
    
    file_path <- file.path(file_dir, file_name)
    fwrite(studies, file_path)
    
    message(sprintf("Study metadata saved to: %s", file_path))
    
  }
  
  # return the filtered studies as a data.table
  return(studies)
  
}


# FUNCTION: download_deployment_metadata ----------------------------------

download_deployment_metadata <- function(
    id_account_dt, 
    save_file = T, 
    tag_ids = c("gps", "sigfox-geolocation"), 
    accepted_manipulation =  c("none", "relocated"), 
    file_name = NULL, 
    file_dir = getwd()
){
  
  # Check if input is a data.table
  if (!is.data.table(id_account_dt)) {
    stop("The input must be a data.table containing study id and Movebank account.")
  }
  
  # Ensure the data.table has the required columns
  if (!all(c("id", "account") %in% names(id_account_dt))) {
    stop("The input data.table must contain 'id' (study_id) and 'account' columns.")
  }
  
  tag_external_id <- c(
    "bird-ring", "gps", "radio-transmitter", "argos-doppler-shift", 
    "natural-mark", "acceleration", "solar-geolocator", "accessory-measurements", 
    "solar-geolocator-raw", "barometer", "magnetometer", "orientation",
    "solar-geolocator-twilight", "acoustic-telemetry", "gyroscope", "heart-rate", 
    "sigfox-geolocation", "proximity", "geolocation-api", "gnss",
    "derived", "tdr", "atlas-geolocation"
  )
  
  if(!is.character(tag_ids) || all(tag_ids %in% tag_external_id) == F) {
    stop("The 'tags_ids' parameter must be a character vector with the correct tag exernal_id.")
  }
  
  manipulation_types <- c(
    "confined", "domesticated", "manipulated-other", "none", 
    "reintroduced", "relocated", "translocated"
  )
  
  if(any(!accepted_manipulation %in% manipulation_types)){
    stop("The 'accepted_manipulation' parameter must be a character vector with the correct manipulation types.\n Check Movebank attribute dictionary for correct values.")
  }
  
  if("none" %in% accepted_manipulation){
    accepted_manipulation <- c("", accepted_manipulation)
  }
  
  
  if(save_file == T && is.null(file_name)){
    stop("If 'save_file' is TRUE, please provide a file name.")
  }
  
  
  id_total <- nrow(id_account_dt)
  
  # Function to process each row
  process_by_row <- function(study_id, account, index) {
    
    # Set the Movebank API key for the account
    options("move2_movebank_key_name" = account)
    
    # Display progress message
    message(sprintf("Downloading deployment metadata for study %d | %d!", index, id_total))
    
    # Attempt to download the deployment data with error handling
    tryCatch(
      {
        # Call the Movebank download function
        data <- as.data.table(movebank_download_deployment(
          study_id, 'license-md5' = '51d5f89ddc94971109e50a17eb14f8be'
        ))[,account := account]
      }, 
      error = function(e) {
        # Return an error table if the download fails
        data.table(
          study_id = study_id,
          account = account,
          error = e$message
        )
      }
    )
  }
  
  # Apply the function to each row of the input data.table
  deployments_list <- lapply(seq_len(id_total), function(i) {
    process_by_row(
      study_id = id_account_dt[i, id],
      account = id_account_dt[i, account],
      index = i
    )
  })
  
  # # Combine the results into a single data.table
  deployments <- rbindlist(deployments_list, fill = TRUE)
  
  # there was a problem with saving list type columns and they seemed to 
  # be coming only from location data, so we replace NULL values
  # with empty geometry
  
  # identify the columns that are list type, and make sure they are all 
  # locational information
  loc_cols <- names(deployments)[sapply(deployments, is.list)]
  loc_cols <- grep("location", loc_cols, value = T)
  
  # replace NULL values with empty geometry (e.g., POINT EMPTY)
  deployments[, (loc_cols) := lapply(.SD, function(col) {
    lapply(col, function(x) if (is.null(x)) sf::st_point() else x)
  }), .SDcols = loc_cols]
  
  # filter deployments
  deployments <- deployments[grepl(paste(tag_ids, collapse = "|"), sensor_type_ids)]
  deployments <- deployments[manipulation_type %in% accepted_manipulation]
  
  # # save file if specifies
  if(save_file){
    
    file_path <- file.path(file_dir, file_name)
    fwrite(deployments, file_path)
    
    message(sprintf("Deployments metadata saved to: %s", file_path))
    
  }
  
  return(deployments)
}

# FUNCTION: download_individual_deployments -----------------------------------

download_individual_deployments <- function(
    deploy_info, 
    tag_ids = c("gps", "sigfox-geolocation"),
    studies_dir = here::here("Data", "Studies"), 
    species_col = "birdlife_name", 
    sub_dir = NA # "1_deployments"
) {
  
  col_check <- c("account", "study_id", "individual_id", "deployment_id")
  
  if(all(c(col_check, species_col) %in% names(deploy_info)) == F){
    stop(
      "The input data.table must contain the following columns: ", 
      paste(col_check, collapse = ", "), 
      " and the correct column with species names provided as a function parameter."
    )
  }
  
  # check if the tag_ids are valid
  tag_external_id <- c(
    "bird-ring", "gps", "radio-transmitter", "argos-doppler-shift", 
    "natural-mark", "acceleration", "solar-geolocator", "accessory-measurements", 
    "solar-geolocator-raw", "barometer", "magnetometer", "orientation",
    "solar-geolocator-twilight", "acoustic-telemetry", "gyroscope", "heart-rate", 
    "sigfox-geolocation", "proximity", "geolocation-api", "gnss",
    "derived", "tdr", "atlas-geolocation"
  )
  
  tag_numb <- c(397, 653, 673, 82798, 2365682, 2365683, 3886361, 7842954, 9301403,
                77740391, 77740402, 819073350, 914097241, 1239574236, 1297673380,
                2206221896, 2299894820, 2645090675, 3090218812, 3090218818, 3090218819,
                4264744764, 4342918458)
  
  if(any(!tag_ids %in% c(tag_external_id, tag_numb))){
    stop("The 'tags_ids' parameter must be a character or numeric vector with the correct tag exernal_id or id. Check Movebank specification")
  }
  
  # create a file name and file_path for each deployment
  deploy_info[, file := paste0(
    study_id,  "_stu_", individual_id, "_ind_", deployment_id, "_dep", ".rds")]
  deploy_info[, file_path := file.path(
    studies_dir, gsub(" ", "_", get(species_col)), sub_dir, file)]
  deploy_info[, downloaded_before := file.exists(file_path)]
  deploy_info[, file := NULL]
  
  deploy_info[, row_id := .I]
  
  rows_to_download <- deploy_info[downloaded_before == F, row_id]
  
  # Function to process each row
  process_by_row <- function(
    account, study_id, deployment_id, individual_id, file_path, index
  ) {
    
    # Set the Movebank API key for the account
    options("move2_movebank_key_name" = account)
    
    # Attempt to download the deployment data with error handling
    tryCatch(
      {
        # Call the Movebank download function
        track <- movebank_download_study(
          study_id = study_id,
          individual_id = individual_id, 
          deployment_id = deployment_id, 
          sensor_type_id = tag_ids,
          remove_movebank_outliers = TRUE,
          omit_derived_data = TRUE,
          convert_spatial_columns = TRUE,
          attributes = "all"
        ) |> 
          dplyr::select(where(~!all(is.na(.))))
        
        if(nrow(track) > 0){
          
          saveRDS(track, file_path, compress = F)
          
          rm(track)
          return(data.table(row_id = index, downloaded_on = Sys.time()))
          
          
        } else{
          
          rm(track)
          return(data.table(row_id = index, error = "no data for the deployment"))
        }
        
      }, error = function(e) {
        
        # Return an error table if the download fails
        message("An error occurred!")
        
        return(data.table(row_id = index, error = e$message))
      }
    )
  }
  
  total <- length(rows_to_download)
  
  # Apply the function to each row of the input data.table
  download_report <- rbindlist(
    lapply(seq_len(total), function(i) {
      
      message(sprintf("\nDownloading track %d of %d!", i, total))
      
      row_i <- rows_to_download[i]
      
      process_by_row(
        account = deploy_info[row_i, account],
        study_id = deploy_info[row_i, study_id],
        individual_id = deploy_info[row_i, individual_id],
        deployment_id = deploy_info[row_i, deployment_id],
        file_path = deploy_info[row_i, file_path],
        index = row_i
      )
      
    }), 
    fill = T
  )
  
  out_report <- merge(
    deploy_info, download_report, by = "row_id", all.x = TRUE
  )
  out_report[
    , downloaded_on := if("downloaded_on" %in% names(out_report))
      fifelse(downloaded_before == T, file.mtime(file_path), downloaded_on)
    else fifelse(downloaded_before == T, file.mtime(file_path), NA)
  ][, row_id := NULL]
  
  rm(download_report)
  
  return(out_report)
  
}
