#' ---
#' title: "MORNING - re-sampling tracking data and extracting basic track parameters"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' the script is identical to the 5.1 script, except that the tracks were first
#' filtered so that they encompass the morning hours of the day 
#' before resampling
#' 
#' **SECTION 1 - Re-sample tracks**
#' resample the tracks using predefined resample and tolerance rate, 
#' save all the resampled tracks in one file per species
#' output: "5.1_all_tracks_one_loc_per_day_morning.rds"
#' 
#' **SECTION 1.1 - Track summary**
#' saving the summary of resampled tracks
#' output: "5.1_all_tracks_one_loc_per_day_morning_summary.csv
#' 
#' **SECTION 2 - Converting track into steps**
#' converting track to steps with the minimal burst of 3 (3 location in a row)
#' to assure the calucation of the turning angle
#' output: "5.1_all_tracks_one_loc_per_day_morning_bursts.rds"
#' 
#' **SECTION 3 - Plot steps and turning angles**
#' output: "5.1_one_loc_per_day_morning.pdf"

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



# getting species of interest
target_sp <- here("Data", "1_downloadable_studies_deployments_filtered.csv") |> 
  read_csv(show_col_types = F) |> 
  distinct(species) |> 
  as_vector() |> 
  str_replace(" ", "_")

# for getting one location per day,
# we use the following sampling rate and tolerance
# resample rate and tolerance
resample_rate = hours(24)
resample_tolerance = hours(2)

# making output directories
target_sp |> 
  map(~{
    
    dist_dir <- here("Data",  "Studies", .x, "5_distances")
    
    if(!dir.exists(dist_dir)) { dist_dir |> dir.create() }
    
  })



# 1 - Re-sample tracks ----------------------------------------------------

# re-sampling tracks with the predefined rate and tolerance

target_sp |> 
  map(~{
    
    sp <- .x 
    
    # folder of the species data
    sp_dir <- here("Data",  "Studies", sp)
    
    # list all deployments previously filtered
    files <- here(sp_dir, "4_filtered_speed") |> 
      list.files()
    
    lfl <- length(files)
    
    # loading each file
    files |> 
      map(~{
        
        fname <- .x
        
        print(paste(sp, which(fname == files), "|", lfl))
        
        mtrack <- here(sp_dir, "4_filtered_speed", fname) |> 
          read_rds() |> 
          filter(
            timestamp <= day_start + resample_tolerance &
              timestamp >= day_start - resample_tolerance
          )
        
        if(nrow(mtrack) > 0){
          
          # resampling the track
          mtrack |> 
            track(
              x = st_coordinates(mtrack)[,1],
              y = st_coordinates(mtrack)[,2],
              t = move2::mt_time(mtrack),
              crs = sf::st_crs(mtrack)
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
            mutate(file = fname)
          
        }
        
      }) |> # map files
      bind_rows() |> 
      # saving re-sampled tracks
      write_rds(
        here(sp_dir, "5_distances", "5.1_all_tracks_one_loc_per_day_morning.rds")
      )
    
    print(paste(sp, "DONE!"))
    
  })



# 1.1 - Track summary -----------------------------------------------------

# providing the summary of resampled tracks

target_sp |> 
  map(~{
    
    ddir <- here("Data", "Studies", .x, "5_distances")
    
    resampled_tracks <- here(ddir, "5.1_all_tracks_one_loc_per_day_morning.rds") |> 
      read_rds() |> 
      mutate(n = n(), .by = file)
    
    # it reports an error in summary if you pass these tracks,
    # because there is insufficient data to summarize
    # but I wanted to keep track of them, so saved them like this
    filtered <- resampled_tracks |>
      filter(n < 2) 

    resampled_tracks <- resampled_tracks |>
      filter(n >= 2) |>
      summarize_sampling_rate_many("file", time_unit = "hour") |>
      as_tibble() 
    
    if(nrow(filtered) > 0){
      
      filtered <- filtered |>
        mutate(comment  = "only one location after filtering")
          
      resampled_tracks <- resampled_tracks |> 
        bind_rows(filtered)
          
    }
    
    resampled_tracks |> 
      write_csv(here(ddir, "5.1_all_tracks_one_loc_per_day_morning_summary.csv"))
    
    print(paste(.x, "DONE!"))
    
  }, .progress = T)


# 2 - Convert tracks to steps ---------------------------------------------

target_sp |> 
  map(~{
    
    sp <- .x 
    ddir <- here("Data", "Studies", sp, "5_distances")
    
    here(ddir, "5.1_all_tracks_one_loc_per_day_morning.rds") |> 
      read_rds() |> 
      group_split(file) |> 
      map(~{
        
        .x |> 
          # for transforming the coordinate system, for the future
          # crs_sub <- mt_aeqd_crs(
          # mt_as_move2(sub), center = "centroid", units = "m"
          # )
          # transform_coords(st_crs(crs_sub)) |>
          filter_min_n_burst(min_n = 3) |>
          steps_by_burst(lonlat = T) |>
          mutate(
            file = unique(.x$file), 
            study_id = unique(.x$study_id),
            individual_id = unique(.x$individual_id), 
            deployment_id = unique(.x$deployment_id)
          )
        
      }) |> 
      bind_rows() |> 
      write_rds(here(ddir, "5.1_all_tracks_one_loc_per_day_morning_bursts.rds"))
      
    print(str_c(.x, " DONE!"))
      
  }, .progress = T)



# 3 - Plot steps and turning angles ----------------------------------------

# making plots of step length and turning angles

target_sp |> 
  map(~{
    
    sp <- .x |> 
      str_replace("_", " ")
    
    sp_dir <- here("Data", "Studies", .x)
    
    steps_df <- here(
      sp_dir, "5_distances", "5.1_all_tracks_one_loc_per_day_morning_bursts.rds"
      ) |> 
      read_rds() |> 
      rename(step = sl_, turn = ta_) |> 
      mutate(
        #step = step/1000, 
        #dt_ = as.numeric(dt_, units = "hours"), 
        speed = step/as.numeric(dt_)
      )
    
    # plotting step length
    pst <- steps_df |> 
      ggplot() +
      geom_histogram(
        aes(x = step), binwidth = 1000, fill = "#9BB655FF", color = "gray22"
      ) +
      labs(
        x = bquote("step length [m] | binwidth =" ~ 10^3), 
      ) +
      scale_x_continuous(label = ~custom_scientific(.x, fixed_exp = 3))
    
    pta <- steps_df |> 
      ggplot() +
      geom_histogram(
        aes(x = turn), binwidth = 0.1, fill = "#9BB655FF", color = "gray22"
      ) +
      labs(x = "turning angle [rad] | binwidth = 0.1") 
    
    psp <- steps_df |>
      ggplot() +
      geom_histogram(
        aes(x = speed), binwidth = 1, fill = "#9BB655FF", color = "gray22"
      ) +
      labs(x = "speed [m/s] | binwidth = 1") 
      # theme_bw(base_size = 14)
    
    
    ggarrange(pst, pta, psp, nrow = 1) |> 
      annotate_figure(
        top = text_grob(
          str_c(sp, " - one location per day"), face = "bold", size = 14
        )
      )
        
    ggsave(
      here(sp_dir, "Graphs","5.1_one_loc_per_day_morning.pdf"), 
      width = 30, 
      units = "cm"
    )
        
   
  })

# Warning messages:
#   1: Removed 954 rows containing non-finite values (`stat_bin()`). 
# 2: Removed 512 rows containing non-finite values (`stat_bin()`). 
# 3: Removed 83 rows containing non-finite values (`stat_bin()`). 
# 4: Removed 193 rows containing non-finite values (`stat_bin()`). 
# 5: Removed 530 rows containing non-finite values (`stat_bin()`). 

