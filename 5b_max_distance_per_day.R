#' ---
#' title: "Re-sampling tracking data and extracting basic track parameters"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' **SECTION 1 - Re-sampling tracks**




# 0 - packages and files --------------------------------------------------

# to handle movement data
library(data.table)
library(tidyverse)
library(here)
# library(suncalc)
library(move2)
# library(atlastools)
library(sf)
library(amt)
# library(nngeo)
library(ggpubr)



# getting species of interest
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


ggraph_dir <- here("Data",  "Graphs")

if(!dir.exists(ggraph_dir)) { ggraph_dir |> dir.create() }


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
  write_rds(here("Data", "Studies", "5_all_tracks_sampling_rate.rds"))


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


# 3 - MAX DISTANCE PER DAY-------------------------------------------------

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
          
          og_sampling <- sampling_rate |> 
            filter(file == fname) |> 
            pull(median) |> 
            round() |> 
            as.period(unit = "min")
          
          print(paste(sp, which(fname == files), "|", lfl))
          
          track <- here(sp_dir, "4_filtered_speed", fname) |> 
            read_rds()
          
          if(og_sampling < (res_rate + res_tolerance)) {
            
            track <- track |> 
              # because this resample is faster than the move2 one
              track(
                x = st_coordinates(track)[,1],
                y = st_coordinates(track)[,2],
                t = move2::mt_time(track),
                crs = sf::st_crs(track),
                all_cols = T
              ) |> 
              track_resample(
                rate = ,
                tolerance = minutes(5)
              ) |> 
              mt_as_move2() |> 
              # mt_filter_per_interval(
              #   criterion = "first", unit = str_c(resolution, " min")
              # ) |> 
              mutate(track_subset = "yes")
          }
          
          track |> 
            group_split(day_cycle) |> 
            map(~{
              
              day_track <- .x |> 
                mutate(row_id = str_c("row_", 1:n()))
              
              day_track_info <- day_track |> 
                #slice_head(n = 1) |> 
                st_drop_geometry() |> 
                summarize(
                  across(
                    any_of(c(id_cols, "day_cycle", "track_subset")), 
                    ~unique(as.character(.x))
                  ), 
                  locs_per_day = n()
                ) 
              
              if(nrow(day_track) > 1){
                
                names <- day_track$row_id
                
                dist <- day_track |>
                  st_distance()
                dist[lower.tri(dist, diag=TRUE)] <- NA
                
                dist |>
                  as_tibble(.name_repair = ~names) |>
                  mutate(id1 = names) |>
                  pivot_longer(
                    cols = all_of(names),
                    values_to = "dist",
                    names_to = "id2",
                    values_drop_na = T
                  ) |>
                  rowwise() |>
                  #filter(dist == max(dist)) |>
                  mutate(
                    timestamp_1 = day_track$timestamp[day_track$row_id == id1],
                    timestamp_2 = day_track$timestamp[day_track$row_id == id2],
                    location_1 = day_track$geometry[day_track$row_id == id1],
                    location_2 = day_track$geometry[day_track$row_id == id2]
                  ) |>
                  ungroup() |>
                  arrange(across(all_of(c("dist", "timestamp_1")), desc)) |>
                  slice_head(n = 1) |>
                  rename(max_dist = dist) |>
                  bind_cols(day_track_info)
                
              } else {
                
                day_track_info |>
                  mutate(comment = "only one location for day cycle")
                
              }
              
              
            }) |> # map day_cycle
            bind_rows() |> 
            mutate(data_saved_on = Sys.time()) |> 
            write_rds(here(m_dir, fout))
          
        } else { # close if file exists
        
          print("distances calculated previously")
          
        }
    
      }) # map_files

  }, .progress = T) # map species


