# get day bursts
#  , day_burst := cumsum(c(1, diff(as.numeric(get(day_cycle_col))) > 1))
# , by = file]

# squish_base -------------------------------------------------------------

# Base R equivalent of str_squish
squish_base <- function(x) {
  gsub("\\s+", " ", trimws(x))
}

to_sentence_base <- function(x) {
  # Convert the first letter to uppercase and the rest to lowercase
  paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))
}

# create directories ------------------------------------------------------

#Utility function: Create directories if they don't exist
#Helper: Create directories

create_dir <- function(
    species, new_dir, studies_dir = here::here("Data", "Studies")
    ) {
  
  sp_dir <- file.path(studies_dir, gsub(" ", "_", species))
  
  created <- sapply(new_dir, function(nd){
    
    dir_path <- file.path(sp_dir, nd)
    
    check <- dir.exists(dir_path)
    
    if(any(check == F)) sapply(dir_path[!check], dir.create, recursive = T) 
    
    return(any(check == F))
  
    })
    
  if(any(created) == T){
    message("Directories created successfully!")
  } else {
    message("Directories already exist!")
  }

}


# FUNCTION 1: generate_file_name ------------------------------------------

make_file_name <- function(
    study_id, individual_id, deployment_id,
    sensor_id = NULL,
    file_type = ".rds"
    ){

  if(is.null(sensor_id)){

    fname <- paste0(
      paste(
        study_id,  "stu",
        individual_id, "ind",
        deployment_id, "dep",
        sep = "_"
      ),
      file_type
    )

  } else{

    fname <- paste0(
      paste(
        study_id,  "stu",
        individual_id, "ind",
        deployment_id, "dep",
        sensor_id, "sen",
        sep = "_"
      ),
      file_type
    )
  }


 return(fname)

}

# # FUNCTION 2: get_file_path -------------------------------------------

# get_file_path <- function(file, folder, species, data_dir = here::here("Data")){
# 
#   file_paths <- file.path(
#     data_dir, "Studies", gsub(" ", "_", species), folder, file
#     )
# 
#   return(file_paths)
# }


# # check_file_exists <-  ---------------------------------------------------
# 
# check_file_exists <- function(file, folder, species, data_dir = here("Data")){
#   
#   file_path <- get_file_path(file, folder, species, data_dir = data_dir)
#   
#   return(file.exists(file_path))
# }
# 


# FUNCTION: add_birdlife_phylogeny ---------------------------------------------

add_birdlife_phylogeny <- function(
    df = NULL, 
    species_name, 
    birdlife_file = here::here("Published_data", "00_birdlife_classification.csv")
) {
  
  # Validate inputs
  if (is.null(df) && !is.vector(species_name)) {
    stop("Either `df` must be provided or `species_name` must be a vector.")
  }
  
  if(!is.null(df) && !is.data.table(df)){
    stop("`df` must be a data.table or NULL.")
  }
  
  if (!is.null(df) && !species_name %in% names(df)) {
    stop("The specified `species_name` column does not exist in `df`.")
  }
  
  birdlife <- data.table::fread(birdlife_file)
  birdlife <- unique(birdlife[, .(scientific_name, family, order)])
    
  # If `df` is NULL and `species_name` is a vector, create a data.table
  if (is.null(df) && is.vector(species_name)) {
    df <- data.table(birdlife_name = species_name)
    species_name <- "birdlife_name"
  }
  
  
  # Merge with birdlife synonyms
  df <- merge(
    df,
    birdlife,
    by.x = species_name,
    by.y = "scientific_name",
    all.x = TRUE
  )
  
  return(df)
}


# FUNCTION: add_birdlife_phylo --------------------------------------------

rename_to_birdlife <- function(
    df = NULL, 
    species_name, 
    max_dist = 1, 
    birdlife_file = here::here("Published_data", "00_birdlife_classification.csv")
) {
  # Validate inputs
  if (is.null(df) && !is.vector(species_name)) {
    stop("Either `df` must be provided or `species_name` must be a vector.")
  }
  
  if(!is.null(df) && !is.data.table(df)){
    stop("`df` must be a data.table or NULL.")
  }
  
  if (!is.null(df) && !species_name %in% names(df)) {
    stop("The specified `species_name` column does not exist in `df`.")
  }
  
  birdlife <- fread(birdlife_file)
  
  # Prepare birdlife data in long format for matching
  bln <- melt(
    birdlife[, .(scientific_name, synonym, sp_status)],
    id.vars = c("sp_status"),
    measure.vars = c("scientific_name", "synonym"),
    variable.name = "name_type",
    value.name = "sci_name",
    na.rm = TRUE
  )[
    , name_type := ifelse(
      sp_status == "R",
      paste("recognized", gsub("_", " ", name_type)),
      paste("not recognized", gsub("_", " ", name_type))
    )
  ][!duplicated(sci_name)]
  
  # Matching function for approximate string matching
  match_names <- function(sp) {
    loc <- stringdist::amatch(
      sp, bln[, sci_name], maxDist = max_dist, matchNA = FALSE, method = "lv"
    )
    bln[loc, sci_name]
  }
  
  # If `df` is NULL and `species_name` is a vector, create a data.table
  if (is.null(df) && is.vector(species_name)) {
    df <- data.table(original_name = species_name)
    species_name <- "original_name"
  }
  
  # Add birdlife_name column and match with birdlife synonyms
  df[, birdlife_name := get(species_name)]
  df[, birdlife_name := fifelse(
    !birdlife_name %in% bln[, sci_name],
    match_names(birdlife_name),
    birdlife_name
  )]
  
  # Merge with birdlife synonyms
  df <- merge(
    df,
    bln[, .(sci_name, name_type)],
    by.x = "birdlife_name",
    by.y = "sci_name",
    all.x = TRUE
  )
  
  # Replace synonyms with official scientific names
  df[, birdlife_name := fifelse(
    grepl("synonym", name_type),
    birdlife$scientific_name[match(birdlife_name, birdlife$synonym)],
    birdlife_name
  )]
  
  return(df)
}



# FUNCTION: day_cycle_to_yd -----------------------------------------------

day_cycle_to_yd <- function(day_cycle){
  
  as.POSIXlt(as.Date(day_cycle))$yday + 1
  
}

# rename_to_birdlife <- function(df, species_name, add_phylo = F) {
#   library(tidyverse)
#   
#   # birdlife classification downloaded from: 
#   # https://datazone.birdlife.org/about-our-science/taxonomy
#   # and prepared to be used in this function in the script 
#   # 00_prepare_external_files.R
#   
#   birdlife <- here("Published_data", "00_birdlife_classification.csv") |> 
#     read_csv(show_col_types = F)
#   
#   # extract all the names and synonyms in one table
#   bln <- birdlife |> 
#     pivot_longer(
#       cols = c(scientific_name, synonym), 
#       names_to = "name_type", 
#       values_to = "sci_name", 
#       values_drop_na = T
#     ) |>  
#     # unique scientific names and synonyms
#     distinct(sci_name, name_type, .keep_all = T) |> 
#     mutate(
#       name_type = if_else(
#         sp_status == "R", 
#         paste("recognized" , str_replace(name_type, "_", " ")), 
#         paste("not recognized", str_replace(name_type, "_", " "))
#       )
#     ) 
#   
#   # copy the species name column from the original data for easier manipulation
#   df <- df |> 
#     mutate(birdlife_name = {{species_name}}) |> 
#     # add the name type
#     left_join(
#       bln |> 
#         select(sci_name, name_type), 
#       by = c("birdlife_name" = "sci_name")
#     ) |> 
#     mutate(
#       alternative_for_synonym = ifelse(
#         str_detect(name_type, "synonym"), 
#         birdlife$scientific_name[match(birdlife_name, birdlife$synonym)], 
#         NA
#       ), 
#       birdlife_name = if_else(
#         str_detect(name_type, "synonym"), 
#         alternative_for_synonym, 
#         birdlife_name
#       ) 
#     ) |> 
#     select(-alternative_for_synonym)
#   
#   if(add_phylo){
#     
#     df <- df |> 
#       mutate(
#         family = birdlife$family[match(birdlife_name, birdlife$scientific_name)],
#         order = birdlife$order[match(birdlife_name, birdlife$scientific_name)]
#       )
#     
#   }
#   
#   return(df)
#   
# }



