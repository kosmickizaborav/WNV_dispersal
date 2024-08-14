#' ---
#' title: "Grouping tracking data by species"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' **SECTION 1 - Group by species** 
#' - load the tracking data and split by species
#' - filter empty locations
#' - split deployments per year
#' - label locations with problems: 
#'   1 - eliminate weird years (i.e. year < 1950)
#'   2 - and coordinates (e.g. long > 180 and lat < 90)
#'   3 - duplicated time-stamps
#'   4 - duplicated subsequent locations
#'   5 - only one location per track_id
#'   6 - season out (month from 4 to 10)
#'   7 - area out (limits of EU)
#'   
#' ** SECTION 2 - Deployment summaries **
#' quick summary of each track_id within deployments
#' - number of deployments
#' - timelags
#' - utm zones   

# 0 - Defining parameters and packages ---------------------------------------

library(move2)
library(atlastools)
library(here)
library(tidyverse)
library(sf)

source("0_helper_functions.R")

# getting the species of interest
target_sp <- here("Data", "downloadable_studies_deployments_filtered.csv") |> 
  read_csv(show_col_types = F) |> 
  distinct(taxon_canonical_name) |> 
  as_vector()

# making folders for each species
target_sp |>
  map(~{
    
    sp_dir <- here("Data", "Studies", str_replace(.x, " ", "_"))
    
    if(!dir.exists(sp_dir)) { sp_dir |> dir.create() }
    
  }
  )

# chat gpt said this is the boundry box for europe,
# added 5 more degrees for safety 
# Latitude range: 34.8° N to 71.2° N
# Longitude range: -31.3° W to 69.1° E
eu_bb <- st_as_sfc(
  st_bbox(c(xmin = -25, xmax = 75, ymax = 30, ymin = 75), crs = 4326)
  )

# focal months
fmonths <- 4:10

# defining units
uni <- list(time = "mins")


# 1 - Group by species ----------------------------------------------------

# ordering tracking data per species: if the study has multiple species, 
# we split it by species and save the data in the corresponding folder

# list of all studies
sfiles <- here("Data", "Studies") |> 
  list.files(pattern = "study.rds")

# loading each study
sfiles |> 
  map(~{
    
    fin <- .x
    
    print(str_c(fin, " - ", which(fin == sfiles), "|", length(sfiles)))
    
    here("Data", "Studies", fin) |> 
      readRDS() |> 
      # split study by species
      group_split(species) |>
      map(~ {
        
        sp <- unique(.x$species) |>
          str_replace(" ", "_")
        
        print(sp)
        
        fout <- str_c(sp, "_", str_replace(fin, "rds", "csv"))
        
        .x |> 
          # filter empty locations
          filter(!st_is_empty(geometry)) |> 
          mutate(
            month = month(timestamp), 
            year = year(timestamp)
          ) |> 
          # grouping data by year
          # track_id = individual_id + year
          mutate(
            deployments_per_individual = length(unique(deployment_id)),
            .by = individual_local_identifier
          ) |> 
          mutate(
            track_id = if_else(
              deployments_per_individual == 1, 
              str_c(individual_id, "_", year), 
              str_c(deployment_id, individual_id, year, sep = "_")
            )
          ) |> 
          group_by(track_id) |> 
          arrange(across(any_of(c("timestamp", "gps_hdop"))), .by_group = T) |>
          mutate(
            lon = st_coordinates(geometry)[, 1], 
            lat = st_coordinates(geometry)[, 2]
          ) |> 
          mutate(
            # eliminating duplicated timestamps
            duplicated_timestamp = duplicated(timestamp),
            n_locations = n(), 
            # eliminating duplicated subsequent locations
            duplicated_loc = lon == c(NA, lon[-n()]) & lat == c(NA, lat[-n()])
          ) |>
          ungroup() |> 
          mutate(
            track_problem = case_when(
              # date
              year < 1950 ~ "weird date",
              # coordinates outside of the range even though proj is EPSG:4326
              abs(lon) > 180 | abs(lat) > 90 ~ "weird coordinates",
              # duplicated timestamp
              duplicated_timestamp == T ~ "duplicated timestamp", 
              duplicated_loc == T ~ "duplicated subsequent location",
              n_locations == 1 ~ "only one point",
              # months outside of the mosquito season
              !month %in% fmonths ~ "season out",
              # points outside of europe
              !as.vector(st_intersects(geometry, eu_bb, sparse = FALSE)) ~ "area out",
              .default = NA
            )
          ) |>
          # drop geometry, since it is kept as lonlat
          st_drop_geometry() |>  
          as_tibble() |> 
          write_csv(here("Data", "Studies", sp, fout))
        
      }
      )
    print("study done!")
  },
  .progress = T
  )
      

# Warning message:
#   There were 2 warnings in `stopifnot()`.
# The first warning was:
#   ℹ In argument: `track_problem = case_when(...)`.
# Caused by warning in `st_is_longlat()`:
#   ! bounding box has potentially an invalid value range for longlat data
# ℹ Run dplyr::last_dplyr_warnings() to see the 1 remaining warning. 
# 
# [[1]]
# <warning/rlang_warning>
#   Warning in `stopifnot()`:
#   ℹ In argument: `track_problem = case_when(...)`.
# Caused by warning in `st_is_longlat()`:
#   ! bounding box has potentially an invalid value range for longlat data
# ---
# 
# [[2]]
# <warning/rlang_warning>
#   Warning in `stopifnot()`:
#   ℹ In argument: `track_problem = case_when(...)`.
# Caused by warning in `st_is_longlat()`:
#   ! bounding box has potentially an invalid value range for longlat data
# ---



# 02 - deployment summaries ----------------------------------------------------

target_sp |> 
  str_replace(" ", "_") |> 
  map(~ {
    
    sp <- .x
    
    sfiles <- here("Data", "Studies", sp) |> 
      list.files(pattern = "study.csv", full.names = F)
    
    sfiles |> 
      map(~{
        
        track_df <- here("Data", "Studies", sp, .x) |> 
          read_csv(show_col_types = F) |> 
          mutate(across(contains("_id"), as.character))
        
        studyid <- unique(track_df$study_id)
        
        # keep only tracks without problems
        track_df <- track_df |> 
          filter(is.na(track_problem)) 
        
        if(nrow(track_df) > 0){
          
          track_df |> 
            mutate(
              timestamp = as_datetime(timestamp),
              timelag = compute_timelags(timestamp), 
              calendar_date = date(timestamp),
              n_locations = n(),
              .by = track_id
            ) |> 
            # keep only track_ids with more than one location
            filter(n_locations > 1) |> 
            group_by(track_id) |> 
            summarize(
              across(
                any_of(
                  c("study_id", "individual_local_identifier", "deployment_id",
                    "sex")
                ),
                ~as.character(unique(.))
              ),
              n_locations = n(), 
              first_timestamp = min(timestamp), 
              last_timestamp = max(timestamp),
              across(
                c(timelag, lon, lat), #timestamp
                list(
                  min = ~min(., na.rm = T),
                  max = ~max(., na.rm = T),
                  median = ~median(., na.rm = T)
                ),
                .names = "{.col}_{.fn}"
              ),
              deploy_unique_dates = length(unique(calendar_date))
            ) |> 
            ungroup() 
          
        } else {
          
          tibble(
            study_id = studyid, 
            track_problem = "no track matches the filtering criteria",
            n_locations = 0
          )
          
          } 
          

        }
      ) |>  
      bind_rows() |>
      # checking in which utm quadrant the track falls
      mutate(
        utm_max = get_utmzone(lon = lon_max, lat = lat_max), 
        utm_min = get_utmzone(lon = lon_min, lat = lat_min), 
        utm_median = get_utmzone(lon = lon_median, lat = lat_median)
      ) |> 
      mutate(
        utm_check = utm_max == utm_min
      ) |> 
      write_csv(
        here("Data", "Studies", sp, str_c(sp, "_deployment_summary.csv"))
      )
    
    print(str_c(sp, " DONE!"))
    
  })



# checking the deployments available --------------------------------------

# 134 out of 1209 deployments span more than one utm zone
df <- target_sp |> 
  str_replace(" ", "_") |> 
  map(~ {
    
    sp <- .x
    
    sfiles <- here("Data", "Studies", sp, str_c(sp, "_deployment_summary.csv")) |> 
      read_csv(show_col_types = F) 
    
    }
  ) |> 
  bind_rows() |> 
  filter(utm_check == F)
