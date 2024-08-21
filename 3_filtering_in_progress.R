# to handle movement data
library(data.table)
library(tidyverse)
library(here)
library(suncalc)
library(move2)
library(atlastools)
library(sf)
library(amt)
library(nngeo)

source("0_helper_functions.R")

main_cols <- c("track_id", "timestamp", "lon", "lat",
               "track_problem", "loc_problem")

speed_quantile <- 0.99
speed_in_quantile <- 0.99
d_quantile <- 0.99
turn_quantile <- 0.10
angle_th <- 35

# defining variables for the funcion getSunlightTimes, 
# the definition of each limit can be found here: 
# https://rdrr.io/cran/suncalc/man/getSunlightTimes.html
day_lim <- c(end = "nauticalDusk", start = "nauticalDawn")

target_sp <- here("Data", "downloadable_studies_deployments_filtered.csv") |> 
  read_csv(show_col_types = F) |> 
  distinct(taxon_canonical_name) |> 
  as_vector()


bird_speeds <- here("Data", "Bird_speeds_Alerstam_2007.csv") |> 
  read_csv() |> 
  select(2:4) |> 
  rename_with(~c("species", "speed", "sd"), everything()) |> 
  mutate(
    max_speed = if_else(is.na(sd), speed, speed + 2*sd), 
    species = str_squish(str_remove(species, "•")), 
    genus = str_split_i(species, " ", 1)
  ) 

speed_limits <- tibble(species = target_sp) |> 
  mutate(genus = str_split_i(species, " ", 1)) |> 
  left_join(
    bird_speeds |> 
      summarize(
        speed_avg_sp = mean(max_speed), 
        .by = species
      ) 
  ) |> 
  left_join(
    bird_speeds |> 
      summarize(
        speed_avg_gen = mean(max_speed), 
        .by = genus
      )
  ) |> 
  mutate(
    speed_lim = if_else(!is.na(speed_avg_sp), speed_avg_sp, speed_avg_gen)
  )
    

bird_speeds_sp <- 




# Filtering locations that might be gps errors ----------------------------



# i don't get the same values when calculating speeds and turning angles with 
# two different packages argosfilter and move2, don't know why. 

track_amt <- "/home/nina/R_projects/Bird_dispersal/Data/Studies/Anas_platyrhynchos/Anas_platyrhynchos_236953686_study.csv" |> 
  read_csv() |> 
  filter(is.na(track_problem)) |> 
  select(any_of(main_cols)) |> 
  filter(track_id == "436618240_2018") |> 
  mt_as_move2(
    coords = c("lon", "lat"),
    #sf_column_name = c("geometry"),
    time_column = "timestamp",
    track_id_column = "track_id", 
    crs = sf::st_crs(4326)
  ) |> 
  mutate(loc_n = 1:n()) |> 
  compute_move_param() |> 
  units::drop_units() |> 
  mutate(btw_distance = (in_distance+out_distance)/2) |> 
  mutate(
    speed_th = quantile(in_speed, probs = speed_quantile, na.rm = T), 
    dist_th = quantile(btw_distance, probs = d_quantile, na.rm = T), 
    .by = track_id
  ) |>
  mutate(
    speed_problem = case_when(
      in_speed > speed_th & out_speed > speed_th & abs(turn) > angle_th ~ "bad",
      in_speed > speed_th ~ "extreme in speed", 
      out_speed > speed_th ~ "extreme out speed", 
      .default = "good"
    ), 
    spatial_problem = if_else(btw_distance > dist_th, "spatial out", "good")
  ) 


track_amt_filtered <- track_amt |> 
  filter(speed_problem != "bad" & !(spatial_problem == "spatial out" & str_detect(speed_problem, "speed"))) |> 
  compute_move_param()


  filter(speed_problem != "bad") |> 
  compute_move_param() |> 
  mutate(
    lon = st_coordinates(geometry)[, 1], 
    lat = st_coordinates(geometry)[, 2]
  ) |> 
  #filter(track_id %in% id_check$track_id) |> 
  # select(any_of(main_cols)) |> 
  mutate(
    date = date(timestamp),
    yearday = yday(date), # used just for the day cycle calculations below
    getSunlightTimes(
      data = tibble(date = date, lat = lat, lon = lon), 
      keep = c(day_lim[["start"]], day_lim[["end"]]), 
      tz = "UTC"
    )
  ) |> 
  rename_with(
    ~c("day_start", "day_end"), 
    all_of(c(day_lim[["start"]], day_lim[["end"]]))
  ) |> 
  # grouping days by light cycle
  mutate(day_cycle = if_else(timestamp < day_start, yearday - 1, yearday)) |> 
  group_split(track_id, day_cycle) |> 
  map(~ {
    
    k <- ifelse(nrow(.x) < 3, nrow(.x), 3)
    
    if(k == 1){
      
      .x |> 
        mutate(
          locs_per_day = k,
          nn_ids = NA, 
          nn_dist = NA, 
          nn_mean_dist = NA
        )
      
    } else {
      
      distnn <- st_nn(.x, .x, k = k, returnDist = T) 
      
      distance_nn <- distnn[[2]] |> 
        map(~ {
          distance = sum(.x)/(k-1)
        }) |> 
        unlist()
      
      .x |> 
        mutate(
          locs_per_day = k,
          nn_ids = distnn[[1]], 
          nn_dist = distnn[[2]], 
          nn_mean_dist = distance_nn
        )
      
    }
    
  }, 
  .progress = T
  ) |> 
  bind_rows() |> 
  mutate(
    #speed_th = quantile(in_speed, probs = speed_quantile, na.rm = T), 
    dist_th = quantile(nn_mean_dist, probs = d_quantile, na.rm = T),
    #speed_in_th = quantile(in_speed, probs = speed_in_quantile, na.rm = T), 
    .by = track_id
    # turn_th = quantile(turn, probs = turn_quantile, na.rm = T),
  ) |> 
  units::drop_units() |> 
  mutate(
    space_problem = if_else(
      nn_mean_dist > dist_th, "spatial outlier", "good"
    ),
    loc_n = 1:n()
  ) 

track_amt_filtered <- track_amt |> 
  filter(!(str_detect(speed_problem, "extreme") & space_problem == "spatial outlier")) |> 
  arrange(timestamp) |> 
  compute_move_param()

see <- track_amt_filtered[54440:54450,]

track_filtered <- track |> 
  filter(track_id == "436618240_2018") |> 
  filter(!(speed_problem != "good" & space_problem == "spatial outlier")) |> 
  arrange(timestamp) |> 
  filter(in_speed > speed_th | out_speed > speed_th)
  compute_move_param()
  filter(timestamp >= as.POSIXct("2018-10-31 20:00:00" ))
  filter(month(timestamp) == 10 & day(timestamp) == 31) 
  group_by(track_id) |> 
  arrange("timestamp", .by_group = T) |> 
  ungroup() |> 
  mt_as_move2(
    coords = c("lon", "lat"),
    #sf_column_name = c("geometry"),
    time_column = "timestamp",
    track_id_column = "track_id", 
    crs = sf::st_crs(4326)
  ) |> 
  compute_move_param()
  
  
 
 track_fi
  
  
  

track_amt2 <- track_distance |> 
 filter(track_id == "679124349_2019" & day_cicle == 93)

  filter(loc_n > 333580 & loc_n < 333800)


check <-  st_nn(id, id, sparse = F, returnDist = T)

id <- track_distance |> 
  filter(track_id == "431819826_2018") |> 
  filter(loc_n > 333500 & loc_n < 333800) 
  mutate(loc_check = median(geometry))


track_amt <-"/home/nina/R_projects/Bird_dispersal/Data/Studies/Anas_platyrhynchos/Anas_platyrhynchos_236953686_study.csv" |> 
  read_csv() |> 
  filter(is.na(track_problem)) |> 
  filter(track_id == "678056862_2019") |> 
  select(
    all_of(
      c("lon", "lat", "timestamp", "track_id", "track_problem", "n_locations")
    )
  ) |> 

  
  

track_param_amt <- track_amt |> 
  mutate(
    in_speed = atl_get_speed(
      track_amt, x = "x_", y = "y_", time = "t_", type = "in"
    ), 
    out_speed = atl_get_speed(
      track_amt, x = "x_", y = "y_", time = "t_", type = "out"
    ),
    turn = atl_turning_angle(track_amt, x = "x_", y = "y_", time = "t_"), 
  ) |> 
  mutate(
    speed_th = quantile(in_speed, probs = speed_quantile, na.rm = T), 
    turn_th = quantile(turn, probs = turn_quantile, na.rm = T)
  ) |> 
  mutate(
    loc_problem = if_else(
      in_speed > speed_th & out_speed > speed_th & turn > turn_th, 
      "bad", "good"),
    loc_n = 1:n()
  ) |> 
  filter(loc_problem == "bad")


check <- track_filtered |> 
  group_by(track_id) |> 
  arrange(timestamp) |> 
  ungroup() |> 
  mutate(
    out_speed = mt_speed(track_filtered, units = "m/s"), 
    in_speed = c(NA, out_speed[-n()]), 
    turn = abs(mt_turnangle(track_filtered, units = "degree")), 
    distance = mt_distance(track_filtered, units = "m")
  )

check2 <- track_filtered |> 
  filter(track_id == "436619810_2018")

track_check <- check |> 
  filter(track_id == "436619810_2018")

check <- track_filtered |> 
  select(contains(c("track", "speed", "turn"))) |> 
  as_tibble() |> 
  distinct() |> 
  mutate_if(is.numeric, ~round(.))


filter(loc_problem != "bad") |> 
  
  
  id_check <- track_filtered |> 
  filter(in_speed > 100 | out_speed > 100) |> 
  st_drop_geometry() |> 
  as_tibble() |> 
  distinct(track_id)
