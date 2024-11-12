#' ---
#' title: "visualizing daily distances"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------


# 0 - packages and files --------------------------------------------------

# to handle movement data
library(data.table)
library(tidyverse)
library(here)
library(move2)
library(sf)
library(amt)
library(ggpubr)
library(suncalc)

source("0_helper_functions.R")


sun_times <- c(start = "dawn", end = "dusk")


# getting the species of interest
target_sp <- here("Data", "1_downloadable_studies_deployments_filtered.csv") |> 
  read_csv(show_col_types = F) |> 
  distinct(species) |> 
  as_vector() |> 
  str_replace(" ", "_")


# summary graph directory
ggraph_dir <- here("Data",  "Graphs")

dcols <- c("study_id", "individual_id", "deployment_id", "species", "file")

dep_df <- here("Data", "1_downloadable_studies_deployments_filtered.rds") |> 
  read_rds() |> 
  select(any_of(dcols), sensor_type_ids) |> 
  mutate(
    sensor = case_when(
      str_detect(sensor_type_ids, "sigfox") ~ "sigfox", 
      str_detect(sensor_type_ids, "radio") ~ "radio", 
      str_detect(sensor_type_ids, "argos") ~ "argos", 
      str_detect(sensor_type_ids, "gps") ~ "gps"
    )
  ) 



# checking the origin of different distances ------------------------------


files <- tibble(
  file = c(
    "5a_all_tracks_one_loc_per_day_bursts.rds", 
    "5a_all_tracks_one_loc_per_day_morning_bursts.rds", 
    "5b_all_tracks_max_daily_distance.rds"
  ),
  name = c("one", "morning", "max")
  )

dfs <- files |> 
  group_split(file) |> 
  map(~{
    
    fname <- .x$file
    n_fix <- .x$name
    
    target_sp |> 
      map(~{
        
        sp <- .x
        
        sp_dir <- here("Data", "Studies", sp)
        
        track <- here(sp_dir, "5_distances", fname) |> 
          read_rds() 
        
        if(n_fix == "max"){
          
          track <- track |> 
            rename(t1_ = timestamp_1, t2_ = timestamp_2, sl_ = max_dist) |> 
            mutate(
              x1_ = st_coordinates(location_1)[,1], 
              y1_ = st_coordinates(location_1)[,2],
              x2_ = st_coordinates(location_2)[,1], 
              y2_ = st_coordinates(location_2)[,2]
            ) |> 
            filter(!is.na(sl_))
          
        }
          
        track <- track |> 
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
            yearday = str_c(yday(date), "_", year(date)),
            before_dawn = t1_ < day_start, 
            day_cycle = if_else(
              t1_ < day_start, 
              str_c(yday(date-1), "_", year(date-1)),
              yearday
            ),
            day_period = if_else(
              t1_ >= day_start & t1_ <= day_end, "day", "night"
            ), 
            day_id = str_c(day_cycle, "_", day_period)
          ) |> 
          select(
            any_of(c(dcols, "day_id"))
            any_of(matches("^x[12]_$|^t[12]_$|^y[12]_$|^sl_$"))
          ) |> 
          rename_with(
            ~str_c(., n_fix), 
            any_of(matches("^x[12]_$|^t[12]_$|^y[12]_$|^sl_$"))
          ) |> 
          mutate(species = str_replace(sp, "_", " ")) |> 
          filter(!is.na(day_id)) 
          # mutate(idd = str_c(study_id, individual_id, deployment_id, species, day_id, sep = "_"))
        
      }) |> 
      bind_rows() |> 
      left_join(dep_df)
    
  }, .progress = T)


df <- dfs[[1]] |> 
  full_join(dfs[[2]]) |> 
  full_join(dfs[[3]]) 




step_df |> 
  ggplot() +
  geom_boxplot(
    aes(x = step, y = sensor, group = sensor),  
    fill = "gray66", color = "black"
  ) +
  facet_wrap(~ species, ncol = 1, scales = "free") +
  labs(
    x = "step length [m]", 
    title = "Tracks subset - one location per day | different sensors"
  ) +
  theme_bw() +
  scale_x_continuous(label = ~custom_scientific(.x, fixed_exp = 3))


ggsave(
  here(ggraph_dir, "5a_one_loc_per_day_steps_box_sensors.pdf"), 
  units = "cm"
)

step_df |> 
  group_split(species) |> 
  map(~{
    
    sp <- unique(.x$species)
    
    sp_dir <- here("Data", "Studies", str_replace(sp, " ", "_"))
    
    .x |> 
      ggplot() +
      geom_histogram(
        aes(x = step),  
        fill = "gray66", color = "black"
      ) +
      facet_wrap(~ sensor, ncol = 1, scales = "free") +
      labs(
        x = "step length [m]", 
        title = str_c(sp, "- one location per day | different sensors")
      ) +
      theme_bw() +
      scale_x_continuous(label = ~custom_scientific(.x, fixed_exp = 3))
    
    ggsave(
      here(sp_dir, "Graphs", "5a_one_loc_per_day_steps_sensors.pdf"), 
      units = "cm"
    )
    
  })


step_df |> 
  group_split(species) |> 
  map(~{
    
    sp <- unique(.x$species)
    
    sp_dir <- here("Data", "Studies", str_replace(sp, " ", "_"))
    
    .x |> 
      mutate(month = month(t1_)) |> 
      ggplot() +
      geom_histogram(
        aes(x = step),  
        fill = "gray66", color = "black"
      ) +
      facet_wrap(~ month, ncol = 3, scales = "free") +
      labs(
        x = "step length [m]", 
        title = str_c(sp, " - one location per day | different sensors")
      ) +
      theme_bw() +
      scale_x_continuous(label = ~custom_scientific(.x, fixed_exp = 3))
    
    ggsave(
      here(sp_dir, "Graphs", "5a_one_loc_per_day_steps_months.pdf"), 
      units = "cm", 
      height = 15
    )
    
  })





step_df |> 
  mutate(step_log = log10(ifelse(step == 0, step + 1e-10, step))) |> 
  group_split(species) |> 
  map(~{
    
    sp <- unique(.x$species)
    
    sp_dir <- here("Data", "Studies", str_replace(sp, " ", "_"))
    
    .x |> 
      mutate(month = month(t1_)) |> 
      ggplot() +
      geom_histogram(
        aes(x = step_log), 
        bins = 100, 
        fill = "gray66", color = "black"
      ) +
      facet_wrap(~ month, ncol = 3, scales = "free_y") +
      labs(
        x = "step length [m] - log scale | bins = 100",
        title = str_c(sp, " - one location per day")
      ) +
      theme_bw()
    
    ggsave(
      here(sp_dir, "Graphs", "5a_one_loc_per_day_steps_months_log.pdf"), 
      units = "cm", 
      height = 15
    )
    
  })





step_df |> 
  mutate(month = month(t1_)) |> 
  ggplot() +
  geom_boxplot(
    aes(x = step, y = species),  
    fill = "gray66", color = "black"
  ) +
  facet_wrap(~month, ncol = 3, scales = "free") +
  labs(
    x = "step length [m]", 
    title = "Distances by month - one location per day"
  ) +
  theme_bw() +
  scale_x_continuous(label = ~custom_scientific(.x, fixed_exp = 3))

ggsave(
  here(ggraph_dir, "5a_one_loc_per_day_steps_months_boxplot.pdf"), 
  units = "cm", 
  height = 30
)




