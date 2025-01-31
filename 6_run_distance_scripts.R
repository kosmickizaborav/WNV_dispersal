#' ---
#' title: "Resample tracks"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#'  **SECTION 1 - Distances per day period**
#'  


# 0 - Defining parameters and packages ---------------------------------------

library(here)
library(tidyverse)
library(amt)
library(sf)


# getting the species of interest
target_sp <- here("Data", "1_downloadable_studies_deployments_filtered.rds") |> 
  read_rds() |>
  distinct(species) |> 
  as_vector()


# create output directory
target_sp |> 
  map(~{
    
    sp_dir <- here("Data", "Studies", str_replace(.x, " ", "_"))
    out_dir <- here(sp_dir, "6_distances")
    
    if(!dir.exists(out_dir)){ dir.create(out_dir) }
    
  })

# keeping just columns of interest
cols_of_interest <- c(
  "individual_local_identifier", "sensor_type", "sex", "manipulation_type", 
  "country", "country_admin", "continent", "within_eubb", "study_site"
)


# 1 - Define parameters for calculating daily distances -------------------


# for night steps
resample_rate <- hours(24)
resample_tolerance <- hours(2)

# how do we define day and night (day limits)
day_limits <- list(
  c(day_start = "nightEnd", day_end =  "night"), 
  c(day_start = "nauticalDawn", day_end = "nauticalDusk"), 
  c(day_start = "dawn", day_end = "dusk")
)


#output files: 
# 1 - 
# 2 - 
# 3 -


# runs --------------------------------------------------------------------

for(day_lim in day_limits){
  
  dcp_file <- str_c(
    "1_all_tracks_dcp_distances_", 
    day_lim[["day_start"]], 
    "_", 
    day_lim[["day_end"]],
    ".rds"
  )
  
  night_file <- str_c(
    "2_all_tracks_night_steps_", 
    day_lim[["day_start"]],
    "_", 
    day_lim[["day_end"]], 
    ".rds"
  )
  
  day_file <- str_c(
    "3_all_tracks_max_day_steps_", 
    day_lim[["day_start"]],
    "_", 
    day_lim[["day_end"]], 
    ".rds"
  )
  
  for(sp in target_sp){
    
    
    # folder with species data
    sp_dir <- here("Data",  "Studies", str_replace(sp, " ", "_"))
    
    # list all deployments
    files <- here(sp_dir, "5_resampled") |> list.files()
    files <- files
    lfl <- length(files)
    
    source(here("6_2_get_night_and_day_steps.R"))
    cat("--------------------DONE: NIGHT and DAY STEPS----------------------\n")
    
    source(here("6_1_get_dcp_distances.R"))
    cat("-----------------DONE: DISTANCES DURING DAY/NIGHT-----------------\n")
    
    print(paste(sp, "DONE!"))
    
  }
  
  print(
    paste(
      "CALCULATION ENDED FOR: \n",
      "day starts at:",
      day_lim[["day_start"]],
      "day ends at:",
      day_lim[["day_end"]]
    )
  )

}
 



