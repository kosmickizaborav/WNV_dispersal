#' ---
#' title: "visualizing daily distances"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

# this script just standardizes the data for different distances so that it
# can be plot using one script
# because some species were exhibiting extreme distances that repeat exactly, 
# explored the manipulation comments, and excluded the deployments that said
# 1: "animal owned by farm and may have been restricted in movement or 
# transported for sale (see Deployment Comments)"
# 2: "displacement" / "Displacement" 

# 0 - packages and files --------------------------------------------------

# to handle movement data
library(tidyverse)
library(here)
library(sf)
library(suncalc)

source("0_helper_functions.R")

# getting the species of interest
target_sp <- here("Data", "1_downloadable_studies_deployments_filtered.csv") |> 
  read_csv(show_col_types = F) |> 
  distinct(species) |> 
  as_vector() |> 
  str_replace(" ", "_")

dist_files <- c(
  "5.1_all_tracks_one_loc_per_day_bursts.rds", 
  "5.1_all_tracks_one_loc_per_day_morning_bursts.rds", 
  "5.2_all_tracks_max_daily_distance.rds"
)

deploy_path <- here("Data", "1_downloadable_studies_deployments_filtered.rds")

dcols <- c("study_id", "individual_id", "deployment_id", "species", "file")

sun_times <- c(start = "dawn", end = "dusk")

# chat gpt said this is the boundry box for europe,
# added 5 more degrees for safety 
# Latitude range: 34.8° N to 71.2° N
# Longitude range: -31.3° W to 69.1° E
# Define the bounding box for Europe correctly
eu_xmin <- -25
eu_xmax <- 75
eu_ymin <- 30
eu_ymax <- 75

# eu_bb <- st_as_sfc(
#   st_bbox(
#     c(
#       xmin = eu_xmin, xmax = eu_xmax, ymin = eu_ymin, ymax = eu_ymax
#     ), 
#     crs = st_crs(4326)
#     )
# )
# ORIGINALLY USED THIS, BUT GAVE UP, BECAUSE IT RETURNS WEIRD RESULTS
# e.g. for Turdus # POINT (29.70283 40.92096) returns FALSE
# df |> 
#   st_as_sf(coords = c("x1_", "y1_"), crs = st_crs(4326)) |>
#   mutate(europe = as.vector(st_intersects(geometry, eu_bb, sparse = FALSE)))



# 1 - Prepare data --------------------------------------------------------


# deployment file, used to add information about manipulation and sensor types
dep_df <- deploy_path |> 
  read_rds() |> 
  select(any_of(dcols), sensor_type_ids, manipulation_comments) |> 
  mutate(
    sensor = case_when(
      str_detect(sensor_type_ids, "sigfox") ~ "sigfox", 
      str_detect(sensor_type_ids, "radio") ~ "radio", 
      str_detect(sensor_type_ids, "argos") ~ "argos", 
      str_detect(sensor_type_ids, "gps") ~ "gps"
    )
  ) |> 
  mutate(
    problematic_deployment = str_detect(
      manipulation_comments, "restricted in movement|[Dd]isplacement"
    )
  )

# preparing data frames for the plots
target_sp |> 
  map(~{
    
    sp <- .x
    
    sp_dir <- here("Data", "Studies", sp)
    
    dist_files |> 
      map(~{
        
        fname <- .x
        fout <- fname |> 
          str_replace( ".rds", "_graph_data.rds") |> 
          str_replace("^5\\.[12]", "5.3")
        
        track <- here(sp_dir, "5_distances", fname) |> 
          read_rds() |> 
          mutate(row_id = str_c("r_", 1:n()))
        
        sun_df <- getSunlightTimes(
          data = tibble(
            date = date(track$t1_),
            lat = track$y1_,
            lon = track$x1_
          ),
          keep = sun_times,
          tz = "UTC"
          ) |>
          mutate(row_id = track$row_id) |> 
          rename_with(~"day_start", all_of(sun_times[["start"]])) |>
          rename_with(~"day_end", all_of(sun_times[["end"]]))
        # grouping days by light cycle
        
        track |> 
          left_join(sun_df, by = "row_id") |>
          mutate(
            day_cycle = if_else(
              t1_ < day_start, 
              str_c(yday(date-1), "_", year(date-1)),
              str_c(yday(date), "_", year(date))
            ),
            day_period = if_else(
              t1_ >= day_start & t1_ <= day_end, "day", "night"
            )
          ) |> 
          mutate(dup_days = n() > 1, .by = c(file, day_cycle)) |> 
          mutate(
            day_id = if_else(
              dup_days, str_c(day_cycle, "_", day_period), day_cycle
            )
          ) |> 
          select(
            any_of(c(dcols, "day_id", "n_locs")),
            any_of(matches("^x[12]_$|^t[12]_$|^y[12]_$|^sl_$|^dt_$"))
          ) |> 
          mutate(
            species = str_replace(sp, "_", " "), 
            file_id = str_remove_all(fname, "5\\.[12]_all_tracks_|_bursts|.rds") |> 
              str_replace_all("_", " "), 
            sl_ = as.numeric(sl_),
            sl_km = sl_/1000,
            sl_log = log10(ifelse(sl_ == 0, sl_ + 1e-5, sl_)),
            month = month(t1_, label = T)
          ) |> 
          filter(!is.na(day_id)) |> 
          left_join(
            dep_df, 
            by = c("study_id", "individual_id", "deployment_id", "species")
          ) |> 
          filter(is.na(problematic_deployment) | problematic_deployment == F) |> 
          mutate(
            in_europe = (x1_>eu_xmin & x1_<eu_xmax & y1_>eu_ymin & y1_<eu_ymax)
          ) |> 
          write_rds(here(sp_dir, "5_distances", fout))
        
        print(paste(fout, "saved!"))
        
      }) # map file
    
    print(paste(sp, "DONE!"))
    
  }) # map species


