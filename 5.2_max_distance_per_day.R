#' ---
#' title: "Calculating maximum distances per day cycle"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' **SECTION 1 -  Calculate sampling rates per track **
#' done in order to evaluate which re-sampling to use for the high resolution 
#' tracks. if the median sampling rate per track was higher than the 
#' predefined sampling rate, we re-sampled the track to coarser resolution
#' 
#' ** 1.1. - Plot sampling rates per species **
#' 
#' **SECTION 2 - Calculate max daily distance **
#' calculating distances between all locations that happen from dawn to dawn
#' and selecting the highest value per day.
#' First the tracks are re-sampled if the median deployment sampling rate was 
#' higher than predefined threshold. then for each daily cycle, distances
#' are calculated between all available points. the highest distance is selected,
#' and saved for that day. 
#' 
#' **SECTION 3 - generate one file per species**
#' output: "5.2_all_tracks_max_daily_distance.rds"


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

target_sp |> 
  map(~{
    
    mdist_dir <- here("Data",  "Studies", .x, "5_distances", "max_distances")
    
    if(!dir.exists(mdist_dir)){ mdist_dir |>  dir.create()}
    
  })

# summary graphs directory
ggraph_dir <- here("Data",  "Graphs")

if(!dir.exists(ggraph_dir)) { ggraph_dir |> dir.create() }


# for calculating maximum distances during the day,
# for the data that has high resolution, we first re-sampled the track to
res_rate = minutes(30) 
res_tolerance = minutes(5)


# 1 - Calculate sampling rate per track -----------------------------------

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
  write_rds(here("Data", "Studies", "5.2_all_tracks_sampling_rate.rds"))


# 1.1 - Plot sampling rates per species -------------------------------

# plotting sampling rate for all species
sampling_rate |> 
  mutate(
    species = str_replace(species, "_", " "), 
    median = median/60, 
    mean = mean/60
  ) |>
  pivot_longer(
    cols = c("median", "mean"), values_to = "time", names_to = "var"
  ) |> 
  mutate(var = paste(var, "time lag")) |> 
  ggplot() +
  geom_boxplot(
    aes(x = time, y = species), fill = "#9BB655FF"
  ) +
  facet_wrap(~ var, nrow = 1, scales = "free") +
  labs(
    x = "median time lag per track [h]", 
    title = "Distrubution of time lags per track"
  ) +
  theme_bw()

ggsave(
  here(ggraph_dir, "5.2_sampling_rate_per_sp.pdf"), 
  width = 25,
  units = "cm"
)


# 2 - Calculate max daily distance ----------------------------------------


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
        
        # if the distance is already calculated skip the file
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
          
          # res-ampling the track if the sampling rate is smaller than the 
          # predefined treshold
          if( og_sampling < (res_rate - res_tolerance) ) {
            
            track <- track |> 
              # because this re-sample is faster than the move2 one
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
            # for each day cycle calculate max distance
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
                
                # distance matrix
                dist <- day_track |>
                  st_distance()
                dist[lower.tri(dist, diag=TRUE)] <- NA
                
                # distance matrix to tibble
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


# 3 - Generate one file per species ---------------------------------------

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
      rename(
        t1_ = timestamp_1, 
        t2_ = timestamp_2, 
        sl_ = max_dist, 
        n_locs = locs_per_day
      ) |> 
      mutate(
        x1_ = st_coordinates(location_1)[,1], 
        y1_ = st_coordinates(location_1)[,2],
        x2_ = st_coordinates(location_2)[,1], 
        y2_ = st_coordinates(location_2)[,2]
      ) |> 
      filter(!is.na(sl_)) |> 
      mutate(dt_ = difftime(t2_, t1_, units = "sec")) |> 
      filter(dt_ > res_rate) |> 
      write_rds(
        here(sp_dir, "5_distances", "5.2_all_tracks_max_daily_distance.rds")
      )
    
    print(paste(sp, "DONE!"))
    
  }, .progress = T) 


