#' ---
#' title: "Calculating maximum distances per day cycle"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' **SECTION 1 -  Calculating sampling rates per track **
#' done in order to evaluate which resampling to use for the high resolution 
#' tracks. if the median sampling rate per track was higher than the 
#' predefined sampling rate, we resampled the track to coarser resolution
#' 
#' ** 1.1. - Historgram of the sampling rates per species **
#' 
#' **SECTION 2 -  Calculating maximum daily distance **
#' calculating distances between all locations that happen from dawn to dawn
#' and selecting the highest value per day
#' 
#' **SECTION 2 -  Calculating maximum daily distance **
#' First the tracks are resampled if the median deployment sampling rate was 
#' higher than predifined treshold. then for each daily cycle, distances
#' are calcuated between all available points. the highest distance is selected,
#' and saved for that day. 
#' 
#' **SECTION 3 -  Plot maximum daily distances **



# 0 - packages and files --------------------------------------------------

# to handle movement data
library(data.table)
library(tidyverse)
library(here)
library(move2)
library(sf)
library(amt)
library(ggpubr)

source("0_helper_functions.R")



# getting the species of interest
target_sp <- here("Data", "1_downloadable_studies_deployments_filtered.csv") |> 
  read_csv(show_col_types = F) |> 
  distinct(species) |> 
  as_vector() |> 
  str_replace(" ", "_")

# for calculating maximum distances during the day,
# for the data that has high resolution, we first resampled the track to
res_rate = minutes(30) 
res_tolerance = minutes(5)

target_sp |> 
  map(~{
    
    mdist_dir <- here("Data",  "Studies", .x, "5_distances", "max_distances")
    
    if(!dir.exists(mdist_dir)){ mdist_dir |>  dir.create()}
    
  })

# summary graph directory
ggraph_dir <- here("Data",  "Graphs")



# 1 - Sampling rates per track --------------------------------------------

# calculate the sampling rate per track for all species
sampling_rate <- target_sp |> 
  map(~{
    
    sp <- .x 
    
    # folder of the species data
    sp_dir <- here("Data",  "Studies", sp)
    
    # list all deployments
    files <- here(sp_dir, "4_filtered_speed") |> 
      list.files()
    
    lfl <- length(files)
    
    
    files |> 
      map(~{
        
        fname <- .x
        
        print(paste(sp, which(fname == files), "|", lfl))
        
        mtrack <- here(sp_dir, "4_filtered_speed", fname) |> 
          read_rds() 
        
        mtrack |> 
          track(
            x = st_coordinates(mtrack)[,1],
            y = st_coordinates(mtrack)[,2],
            t = move2::mt_time(mtrack),
            crs = sf::st_crs(mtrack),
            all_cols = F
          ) |> 
          summarize_sampling_rate(time_unit = "min") |> 
          mutate(file = fname)
        
        
      }) |> # map files
      bind_rows() |> 
      mutate(species = str_replace(sp, " ", "_"))
    
  }) |> 
  bind_rows() 

# saving sampling rate 
sampling_rate |> 
  write_rds(here("Data", "Studies", "5b_all_tracks_sampling_rate.rds"))


# 1.1 - Plot the sampling rates per species -------------------------------

# plotting sampling rate for all species
sampling_rate |> 
  mutate(species = str_replace(species, "_", " ")) |> 
  ggplot() +
  geom_histogram(aes(x = median), binwidth = 60) +
  facet_wrap(~ species, ncol = 1) +
  labs(
    x = "median time per track [min] | binwidth = 60"
  ) +
  theme_bw()

ggsave(
  here(ggraph_dir, "5b_sampling_rate_per_sp.pdf"), 
  height = 15,
  units = "cm"
)


# 2 - Calculating daily max distance --------------------------------------


# saving sampling rate 
# sampling_rate <- here("Data", "Studies", "5b_all_tracks_sampling_rate.rds") |>
#   read_rds()

id_cols <- c(
  "study_id", "deployment_id", "individual_id", "individual_local_identifier"
  )


target_sp |> 
  map(~{
    
    sp <- .x 
    # folder of the species data
    sp_dir <- here("Data",  "Studies", sp)
    m_dir <- here(sp_dir, "5_distances", "max_distances")
    
    
    # list all deployments
    files <- here(sp_dir, "4_filtered_speed") |> 
      list.files()
    lfl <- length(files)
    
    files |> 
      map(~{
        
        fname <- .x
        fout <- str_replace(fname, "_speed_filtered.rds", "_max_dist.rds")
        
        
        if(!file.exists(here(m_dir, fout))){
          
          # checking median sampling rate for that deployment
          og_sampling <- sampling_rate |> 
            filter(file == fname) |> 
            pull(median) |> 
            round() |> 
            as.period(unit = "min")
          
          print(paste(sp, which(fname == files), "|", lfl))
          
          # loading the track
          track <- here(sp_dir, "4_filtered_speed", fname) |> 
            read_rds() |> 
            select(all_of(id_cols) | contains("day"))
          
          
          # resampling the track if the sampling rate is smaller than the 
          # predefined treshold
          if( og_sampling < (res_rate - res_tolerance) ) {
            
            track <- track |> 
              # because this resample is faster than the move2 one
              track(
                x = st_coordinates(track)[,1],
                y = st_coordinates(track)[,2],
                t = mt_time(track),
                id = individual_local_identifier,
                crs = st_crs(track)
              ) |> 
              track_resample(
                rate = res_rate,
                tolerance = res_tolerance
              ) |> 
              mt_as_move2() |> 
              select(-timestamp, -individual_local_identifier, -burst_) |> 
              rename(timestamp = t_, individual_local_identifier = id) |> 
              mutate(track_subset = "yes") 
          }
          
          track |>  
            group_split(day_cycle) |> 
            map(~{
              
              day_track <- .x |> 
                mutate(row_id = str_c("row_", 1:n()))
              
              day_track_info <- day_track |>  
                st_drop_geometry() |> 
                summarize(
                  across(
                    any_of(c(id_cols, "track_subset", "day_cycle")), 
                    ~unique(.x)
                  ), 
                  locs_per_day = n()
                ) 
              
              if(nrow(day_track) > 1){
                
                names <- day_track$row_id
                
                dist <- day_track |>
                  st_distance()
                dist[lower.tri(dist, diag=TRUE)] <- NA
                
                dist_df <- dist |>
                  as_tibble(.name_repair = ~names) |>
                  mutate(id1 = names) |>
                  pivot_longer(
                    cols = all_of(names),
                    values_to = "dist",
                    names_to = "id2",
                    values_drop_na = T
                
                  ) 
                
                id1_df <- day_track |> 
                  filter(row_id %in% dist_df$id1) |> 
                  select(
                    row_id, timestamp, geometry, individual_local_identifier, 
                    deployment_id
                  ) |>  
                  rename(
                    id1 = row_id, 
                    timestamp_1 = timestamp, 
                    location_1 = geometry, 
                  )
                
                dist_df |> 
                  bind_cols(day_track_info) |> 
                  left_join(
                    id1_df, 
                    by = c(
                      "id1", "individual_local_identifier", "deployment_id"
                    )
                  ) |> 
                  arrange(dist, timestamp_1) |>
                  slice_tail(n = 1) |>
                  mutate(
                    timestamp_2 = day_track$timestamp[day_track$row_id == id2], 
                    location_2 = day_track$geometry[day_track$row_id == id2], 
                    file = fname
                  ) |> 
                  rename(max_dist = dist)  
                
                
              } else {
                
                day_track_info |>
                  mutate(comment = "only one location for day cycle")
                
              }
              
              
            }) |> # map day_cycle
            bind_rows()  |> 
            mutate(data_saved_on = Sys.time()) |> 
            write_rds(here(m_dir, fout))
          
          print("----------------saved!--------------------")
          
        } else { # close if file exists
        
          print("distances calculated previously")
          
        }
    
      }) # map_files

  }, .progress = T) # map species


# grouping data for all tracks to one file per species
target_sp |> 
  map(~{
    
    sp <- .x 
    # folder of the species data
    sp_dir <- here("Data",  "Studies", sp)
    m_dir <- here(sp_dir, "5_distances", "max_distances")
    
    files <- m_dir |> list.files()
    
    files |> 
      map(~{
        here(m_dir, .x) |> 
          read_rds() |> 
          filter(!is.na(day_cycle))
      }) |> 
      bind_rows() |> 
      write_rds(
        here(sp_dir, "5_distances", "5b_all_tracks_max_daily_distance.rds")
      )
    
    print(paste(sp, "DONE!"))
    
  }, .progress = T) 



# 3 - Data for graphs -----------------------------------------------------


dist_df <- target_sp |> 
  map(~{
    
    sp_dir <- here("Data", "Studies", .x)
    
    here(sp_dir, "5_distances", "5b_all_tracks_max_daily_distance.rds") |> 
      read_rds() |> 
      mutate(species = str_replace(.x, "_", " ")) 
    
  }) |> 
  bind_rows() |> 
  filter(is.na(comment)) |> 
  mutate(
    time_period = difftime(timestamp_2, timestamp_1, units = "hours") |> 
      as.numeric(), 
    hour_1 = format(timestamp_1, format = "%H:%M:%S"),
    hour_2 = format(timestamp_2, format = "%H:%M:%S")
  ) |> 
  units::drop_units()


dist_df |> 
  # plotting sampling rate for all species 
  ggplot() +
  geom_histogram(
    aes(x = max_dist), 
    binwidth = 10000, 
    fill = "gray66", color = "black"
  ) +
  facet_wrap(~ species, ncol = 1, scales = "free_y") +
  labs(
    x = bquote("step length [m] | binwidth =" ~ 10^4), 
    title = "Maximum distance per day"
  ) +
  theme_bw() +
  scale_x_continuous(label = ~custom_scientific(.x, fixed_exp = 3))


ggsave(
  here(ggraph_dir, "5b_max_distance_per_day.pdf"), 
  height = 15,
  units = "cm"
)

dist_df |> 
  mutate(dist_log = log10(ifelse(max_dist == 0, max_dist + 1e-10, max_dist))) |> 
  ggplot() +
  geom_histogram(
    aes(x = dist_log), 
    bins = 100, 
    fill = "gray66", color = "black"
  ) +
  facet_wrap(~ species, ncol = 1, scales = "free_y") +
  labs(
    x = "step length [m] - log scale | bins = 100",
    title = "Maximum distance per day"
  ) +
  theme_bw() 

ggsave(
  here(ggraph_dir, "5b_max_distance_per_day_log10.pdf"), 
  height = 15,
  units = "cm"
)


dist_df |> 
  # plotting sampling rate for all species 
  ggplot() +
  geom_boxplot(
    aes(x = max_dist, y = species, group = species),  
    fill = "gray66", color = "black", 
    notch = T
  ) +
  labs(
    x = "step length [m]", 
    title =  "Maximum distance per day"
  ) +
  theme_bw() +
  scale_x_continuous(label = ~custom_scientific(.x, fixed_exp = 3))

ggsave(
  here(ggraph_dir, "5b_max_distance_per_day_box.pdf"), 
  units = "cm"
)

hp <- dist_df |> 
  ggplot() +
  geom_histogram(
    aes(x = time_period), 
    binwidth = 1, 
    fill = "gray66", color = "black"
  ) +
  facet_wrap(~ species, ncol = 1, scales = "free_y") +
  labs(
    x = "time period [hour] | binwidth = 1",
    title = "Time period between the two locations"
  ) +
  theme_bw() 


ggsave(
  here(ggraph_dir, "5b_max_distance_per_day_time_period.pdf"), 
  height = 15,
  units = "cm"
)

dist_df |> 
  ggplot() + 
  geom_boxplot(
    aes(x = time_period, y = species, group = species),  
    fill = "gray66", color = "black", 
    notch = T
  ) +
  labs(x = "time period between the points [hour]") +
  theme_bw() 

ggsave(
  here(ggraph_dir, "5b_max_distance_per_day_time_period_boxplot.pdf"),
  units = "cm"
)


dist_df |> 
  ggplot() + 
  geom_histogram(
    aes(x = locs_per_day), 
    binwidth = 1, 
    fill = "gray66", color = "black"
  ) +
  facet_wrap(~ species, ncol = 1, scales = "free_y") +
  # geom_segment(
  #   aes(x = hour_1, xend = hour_2, y = locs_per_day, yend = locs_per_day), 
  #   position = "jitter"
  # ) +
  # geom_point(
  #   aes(x = hour_1, y = hour_2),  
  #   color = "orange", 
  #   position = "jitter"
  # ) +
  # geom_point(
  #   aes(x = hour_2, y = locs_per_day),
  #   color = "darkblue", 
  #   position = "jitter"
  # ) +
  labs(
    x = "locations per day", 
    title =  "Number of locatioons per day"
  ) +
  theme_bw() 

ggsave(
  here(ggraph_dir, "5b_max_distance_number_of_locations.pdf"), 
  height = 15,
  units = "cm"
)

# check -------------------------------------------------------------------
# 
# check_df <- target_sp |> 
#   map(~{
#     
#     sp <- .x 
#     # folder of the species data
#     sp_dir <- here("Data",  "Studies", sp)
#     
#     
#     # list all deployments
#     files <- here(sp_dir, "4_filtered_speed") |> 
#       list.files()
#     lfl <- length(files)
#     
#     files |> 
#       map(~{
#         
#         fname <- .x
#         
#         track <- here(sp_dir, "4_filtered_speed", fname) |> 
#           read_rds() |> 
#           select(all_of(id_cols) | contains("day")) |> 
#           filter(is.na(day_cycle))
#         
#         if(nrow(track) > 0){
#           tibble(
#             species = sp, 
#             file = fname, 
#             folder = here(sp_dir, "4_filtered_speed")
#           )
#         }
#           
#         }) |> 
#       bind_rows()
#     }) |> 
#   bind_rows()
# 
# check_df |> 
#   bind_rows()
# check_file <- here("Data",  "Studies", target_sp[1], )
# 


# correcting the files that had deployment_id.x and deployment_id.y column
# deploy_problem <- here("Data", "deployment_id_check.rds") |> 
#   read_rds() |> 
#   filter(id_col == "deployment") |> 
#   mutate(
#     file =  str_replace(file, "_speed_filtered.rds", "_max_dist.rds"), 
#     m_dir = here(sp_dir, "5_distances", "max_distances"), 
#     full_path = here(m_dir, file)
#     )
#   
# deploy_problem$full_path |> 
#   map(~file.remove(.x))
