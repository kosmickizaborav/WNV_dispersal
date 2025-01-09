#' ---
#' title: "Resample tracks"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#'  **SECTION 1 - Calculate sampling rate per track**
#'  for each track and species summarize the sampling rate
#'  
#'  **SECTION 2 - Plot sampling rates per species**
#'  plot the distribution of sampling rates per species using t
#'  the median sampling rate
#'  
#'  **SECTION 3 - Re-sample track to defined rate**
#'  re-sample the track to a defined rate, in this case 60 minutes
#'  add day_cycle and day_period columns 
#'  output saved in "5_resampled" in the species folder

# 0 - Defining parameters and packages ---------------------------------------

library(move2)
library(amt)
library(here)
library(tidyverse)
library(sf)

# getting the species of interest
# getting the species of interest
target_sp <- here("Data", "1_downloadable_studies_deployments_filtered.rds") |> 
  read_rds() |>
  distinct(species) |> 
  as_vector()

# main graph directory for summary plots across species
graphs_main <- here("Data", "Graphs")
if(!dir.exists(graphs_main)){
  dir.create(graphs_main)
}

# keeping just columns of interest
columns_of_interest <- c(
  "timestamp", "geometry", "individual_local_identifier", 
  "sensor_type_id", "sex", "manipulation_type", 
  "country", "country_admin", "continent", "within_eubb", "study_site", 
  "sensor_type"
  )

day_limits <- c(day_start = "nightEnd", day_end = "night")


# for the data that has high resolution, we first re-sampled the track to, 
# arbitrary decidison of 60 minutes 
resample_rate <- minutes(60)
resample_tolerance = minutes(30)

# sampling_rate |> 
#   summarize(
#     median_of_medians = median(median),
#     .by = species
#   )
# # A tibble: 6 × 2
# species            median_of_medians
# <chr>                          <dbl>
# 1 Anas_platyrhynchos                60
# 2 Columba_livia                      1
# 3 Turdus_merula                   1438
# 4 Circus_aeruginosus                 5
# 5 Sturnus_vulgaris                 733
# 6 Accipiter_gentilis                60


# create output directory
target_sp |> 
  map(~{
    out_dir <- here("Data", "Studies", str_replace(.x, " ", "_"), "5_resampled")
   
     if(!dir.exists(out_dir)){
      dir.create(out_dir)
     }
    
  })

# 1 - Calculate sampling rate per track -----------------------------------

# calculate the sampling rate per track for all species
sampling_rate <- target_sp |> 
  map(~{
    
    sp <- .x 
    # folder of the species data
    sp_dir <- here("Data",  "Studies", str_replace(sp, " ", "_"))
    
    # list all deployments
    files <- here(sp_dir, "4_filtered_speed") |> 
      list.files()
    lfl <- length(files)
    
    
    files |> 
      map(~{
        
        fin <- .x
        
        print(paste(sp, which(fin == files), "|", lfl))
        
        mtrack <- here(sp_dir, "4_filtered_speed", fin) |> 
          read_rds() |> 
          mutate(file = fin)
        
        mtrack |> 
          mutate(
            x = st_coordinates(mtrack)[,1],
            y = st_coordinates(mtrack)[,2], 
            t = timestamp
          ) |> 
          make_track(
            x, y, t,
            id = file,
            crs = sf::st_crs(mtrack), 
            all_cols = F
          ) |> 
          summarize_sampling_rate(time_unit = "min") |> 
          mutate(file = fin)
        
      }) |> # map files
      bind_rows() |> 
      mutate(species = sp)
    
  }) |> 
  bind_rows() 

# saving sampling rate 
sampling_rate |> 
  write_rds(here("Data", "Studies", "5_all_tracks_sampling_rate.rds"))


# 2 - Plot sampling rates per species -------------------------------------

# plotting sampling rate for all species
sampling_rate |> 
  mutate(
    species = str_replace(species, "_", " "), 
    median = median/60, 
    mean = mean/60
  ) |>
  ggplot() +
  geom_histogram(aes(x = mean, fill = species), color = "gray22") +
  facet_wrap(~species, ncol = 1, scales = "free") +
  labs(
    x = "median time lag per track [h]", 
    title = "Distrubution of median time lags per track"
  ) +
  theme_bw() + 
  theme(legend.position = "none")

ggsave(
  here(graphs_main, "5_sampling_rate_hist.pdf"), 
  height = 25,
  units = "cm"
)


# 3 - Re-sample & add new columns --------------------------------------


target_sp |> 
  map(~{
    
    sp <- .x 
    # folder of the species data
    sp_dir <- here("Data",  "Studies", str_replace(sp, " ", "_"))
    
    # list all deployments
    files <- here(sp_dir, "4_filtered_speed") |> 
      list.files()
    lfl <- length(files)
    
    files |> 
      map(~{
        
        fin <- .x
        fout <- str_replace(fin, "_speed_filtered.rds", "_resampled.rds")
        
        print(paste(sp, which(fin == files), "|", lfl))
        
        # loading the track, 
        # moved addition of columns here and not at the end, because it changed
        # the dataframe class from track to tibble and we don´t want that
        mtrack <- here(sp_dir, "4_filtered_speed", fin) |> 
          read_rds() |> 
          select(all_of(c(columns_of_interest, unname(day_limits)))) |> 
          rename_with(~"day_start", all_of(day_limits[["day_start"]])) |>
          rename_with(~"day_end", all_of(day_limits[["day_end"]])) 
        
        track <- mtrack |> 
          mutate(
            x = st_coordinates(mtrack)[,1],
            y = st_coordinates(mtrack)[,2], 
            t = timestamp
          ) |> 
          make_track(
            x, y, t,
            id = individual_local_identifier,
            crs = sf::st_crs(mtrack), 
            all_cols = T
          ) 
        
        # 3 - 1 - Resample tracks --------------------------------------------
        
        median_srate <- sampling_rate |> 
          filter(file == fin) |> 
          pull(median) |> 
          ceiling() |> 
          as.period(unit = "min")
        
        if( median_srate < resample_rate ) {
          
          track <- track |> 
            track_resample(
              rate = resample_rate,
              tolerance = resample_tolerance
            ) |> 
            mutate(track_resampled = T)
          
        } else {
          
          track <- track |> 
            mutate(track_resampled = F)
          
        } 
        
        if(nrow(track) > 3){
          
          track |>
            mutate(date = as.Date(timestamp)) |> 
            # in the northern latitudes sometimes the sun doesn't set so 
            # to filter out those locations if we are using the nautical dusk
            # filter(!is.na(day_start) & !is.na(day_end))
            mutate(
              day_cycle = if_else(
                timestamp < day_start,
                str_c(yday(date - 1), "_", year(date - 1)),
                str_c(yday(date), "_", year(date))
              ),
              day_period = if_else(
                timestamp <= day_start | timestamp >= day_end, 
                "night",
                "day", 
                missing = NA
              ), 
              dcp = str_c(day_cycle, "_", day_period)
            ) |> 
            write_rds(here(sp_dir, "5_resampled", fout))
          
        }
      
    }) # map files
    
    print(paste(sp, "done!"))
    
}) # map target_sp



# 4 - Calculate sampling rate after re-sample -----------------------------------


# calculate the sampling rate per track for all species
postsampling_rate <- target_sp |> 
  map(~{
    
    sp <- .x 
      
    # folder of the species data
    sp_dir <- here("Data",  "Studies", str_replace(sp, " ", "_"))
    
    # list all deployments
    files <- here(sp_dir, "5_resampled") |> 
      list.files()
    lfl <- length(files)
    
    
    files |> 
      map(~{
        
        fin <- .x
        print(fin)
        print(paste(sp, which(fin == files), "|", lfl))
        
        track <- here(sp_dir, "5_resampled", fin) |> 
          read_rds() |> 
          summarize_sampling_rate(time_unit = "min") |> 
          mutate(file = fin)
        
      }) |> # map files
      bind_rows() |> 
      mutate(species = sp)
    
  }) |> 
  bind_rows() 

# saving sampling rate 
postsampling_rate |> 
  write_rds(here("Data", "Studies", "5_all_tracks_resampled_sampling_rate.rds"))

# 
# # 5 - Plot sampling rates per species -------------------------------------
# 
# # plotting sampling rate for all species
# postsampling_rate |> 
#   mutate(
#     species = str_replace(species, "_", " "), 
#     median = median/60, 
#     mean = mean/60
#   ) |>
#   ggplot() +
#   geom_histogram(aes(x = mean, fill = species), color = "gray22") +
#   facet_wrap(~species, ncol = 1, scales = "free") +
#   labs(
#     x = "median time lag per track [h]", 
#     title = "Distrubution of median time lags per track", 
#     subtitle = "after re-sampling to 60 minutes"
#   ) +
#   theme_bw() + 
#   theme(legend.position = "none")
# 
# ggsave(
#   here(graphs_main, "5_sampling_rate_hist_post_resample.pdf"), 
#   height = 25,
#   units = "cm"
# )


