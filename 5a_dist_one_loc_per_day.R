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

# resample rate and tolerance
resample_rate = hours(24)
resample_tolerance = hours(2)

target_sp |> 
  map(~{
    
    dist_dir <- here("Data",  "Studies", .x, "5_distances")
    
    if(!dir.exists(dist_dir)) { dist_dir |> dir.create() }
    
  })



# 1 - Re-sampling tracks --------------------------------------------------

# re-sampling tracks with the predefined rate and tolerance

target_sp |> 
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
          # eliminating duplicated positions per individual 
          # to avoid distances 0,
          # this is a bit extreme as positions can be days spaced apart,
          # but couldn't think of a better way
          # distinct(x_, y_, .keep_all = T, .by = track_id) 
          track_resample(
            rate = resample_rate,
            tolerance = resample_tolerance
          ) |> 
          mutate(
            track_id = str_remove_all(fname, "[a-zA-Z_]") |> 
              str_replace_all("__", " ") |> 
              str_remove_all("_") |> 
              str_squish() |> 
              str_replace_all(" ", "_")
          )
        
      }) |> # map files
      bind_rows() |> 
      # saving re-sampled tracks
      write_rds(
        here(sp_dir, "5_distances", "all_tracks_one_loc_per_day.rds")
      )
    
    print(paste(sp, "DONE!"))
    
    })
    
# 2 - summary of the re-sampled tracks ----------------------------------------

target_sp |> 
  map(~{
    
    ddir <- here("Data", "Studies", .x, "5_distances")
    
    resampled_tracks <- here(ddir, "all_tracks_one_loc_per_day.rds") |> 
      read_rds() |> 
      mutate(n = n(), .by = "track_id")
    
    # it reports an error in summary if you pass these tracks,
    # because there is insufficient data to summarize
    # but I wanted to keep track of them, so saved them like this
    filtered <- resampled_tracks |>
      filter(n < 2) 

    resampled_tracks <- resampled_tracks |>
      filter(n >= 2) |>
      summarize_sampling_rate_many("track_id", time_unit = "hour") |>
      as_tibble() 
    
    if(nrow(filtered) > 0){
      
      filtered <- filtered |>
        mutate(comment  = "only one location after filtering")
          
      resampled_tracks <- resampled_tracks |> 
        bind_rows(filtered)
          
    }
    
    resampled_tracks |> 
      write_csv(here(ddir, "all_tracks_one_loc_per_day_summary.csv"))
    
    print(paste(.x, "DONE!"))
    
  }, .progress = T)



# 3 - Converting track to steps ---------------------------------------------


target_sp |> 
  map(~{
    
    ddir <- here("Data", "Studies", .x, "5_distances")
    
    here(ddir, "all_tracks_one_loc_per_day.rds") |> 
      read_rds() |> 
      group_split(track_id) |> 
      map(~{
        
        .x |> 
          # for transforming the coordinate system, for the future
          # crs_sub <- mt_aeqd_crs(
          # mt_as_move2(sub), center = "centroid", units = "m"
          # )
          # transform_coords(st_crs(crs_sub)) |>
          filter_min_n_burst(min_n = 3) |>
          steps_by_burst(lonlat = T, keep_cols = T)
        
      }) |> 
      bind_rows() |> 
      write_rds(here(ddir, "all_tracks_one_loc_per_day_bursts.rds"))
      
    print(str_c(.x, " DONE!"))
      
  })



# 4 - Plotting distances and turning angles ---------------------------------

# making plots of step length and turning angles

target_sp |> 
  map(~{
    
    sp <- .x |> 
      str_replace("_", " ")
    
    sp_dir <- here("Data", "Studies", .x)
    
    steps_df <- here(
      sp_dir, "5_distances", "all_tracks_one_loc_per_day_bursts.rds"
      ) |> 
      readRDS() |> 
      rename(step = sl_, turn = ta_) |> 
      mutate(
        step = step/1000, 
        dt_ = as.numeric(dt_, units = "hours"), 
        speed = step/dt_
      )
    
    # plotting step length
    pst <- steps_df |> 
      ggplot() +
      geom_histogram(
        aes(x = step), binwidth = 10, fill = "gray66", color = "black"
      ) +
      labs(
        x = "step length [km] | binwidth = 10"
      ) 
    
    pta <- steps_df |> 
      ggplot() +
      geom_histogram(
        aes(x = turn), binwidth = 0.1, fill = "gray66", color = "black"
      ) +
      labs(x = "turning angle [rad] | binwidth = 0.1") 
    
    psp <- steps_df |>
      ggplot() +
      geom_histogram(
        aes(x = speed), binwidth = 1, fill = "gray66", color = "black"
      ) +
      labs(x = "speed [km/h] | binwidth = 1") 
      # theme_bw(base_size = 14)
    
    
    ggarrange(pst, pta, psp, nrow = 1) |> 
      annotate_figure(
        top = text_grob(
          str_c(sp, " - one location per day"), face = "bold", size = 14
        )
      )
        
    ggsave(
      here(sp_dir, "Graphs","5a_one_loc_per_day.pdf"), 
      width = 30, 
      units = "cm"
    )
        
   
  })


# Warning messages:
# 1: Removed 9480 rows containing non-finite values (`stat_bin()`). 
# 2: Removed 1203 rows containing non-finite values (`stat_bin()`). 
# 3: Removed 658 rows containing non-finite values (`stat_bin()`). 
# 4: Removed 182 rows containing non-finite values (`stat_bin()`). 
# 5: Removed 960 rows containing non-finite values (`stat_bin()`). 

