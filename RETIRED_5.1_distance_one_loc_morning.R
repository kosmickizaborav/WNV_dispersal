#' ---
#' title: "MORNING - re-sampling tracking data and extracting basic track parameters"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' **SECTION 1 - Convert track to steps**
#' 1 - filter tracks for position that were registered within the time span
#'     (dawn - resampe_tolerance, dawn + resample_tolerance)
#' 2 - re-sample the track so that we have one location per day with the 
#'     sampling frequency 24 hours +/- 2 hours
#' 3 - filter bursts by 3 locations and convert track to steps with the minimal 
#'     to assure the calculation of the turning angle
#'     
#' **SECTION 2 - generate one file per species**
#' Gather data from all tracks into one file per species
#' output: "5.1_all_tracks_one_loc_morning_steps.rds"
#' 
#' **SECTION 3 - Plot steps and turning angles**
#' output: "5.1_all_tracks_one_loc_morning_steps.pdf"

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


# making output directories
target_sp |> 
  map(~{
    
    ddir <- here("Data",  "Studies", .x, "5_distances")
    
    if(!dir.exists(ddir)) { ddir |> dir.create() }
    
    mdir <- here(ddir, "1_one_loc_morning")
    
    if(!dir.exists(mdir)) { mdir |> dir.create() }
    
  })


# 1 - Convert tracks to steps ---------------------------------------------

# the tracks were first filtered for the morning period and then
# they were resampled for one location per day
# to get one location per day we use the following sampling rate and tolerance:
resample_rate = hours(24)
resample_tolerance = hours(2)
 
# re-sampling tracks with the predefined rate and tolerance

target_sp |> 
  map(~{
    
    sp <- .x 
    
    # folder of the species data
    sp_dir <- here("Data",  "Studies", sp)
    ddir <- here(sp_dir, "5_distances")
    
    # list all deployments previously filtered
    files <- here(sp_dir, "4_filtered_speed") |> 
      list.files()
    
    lfl <- length(files)
    
    # loading each file
    files |> 
      map(~{
        
        fin <- .x
        fout <- str_replace(fin, "_speed_filtered.rds", "_one_loc_morning.rds")
        
        print(paste(sp, which(fin == files), "|", lfl))
        
        # taking only the points around the dawn
        mtrack <- here(sp_dir, "4_filtered_speed", fin) |> 
          read_rds() |> 
          filter(
            timestamp >= (day_start - resample_tolerance) &
              timestamp <= (day_start + resample_tolerance) 
          ) |> 
          select(-event_id, -account)
          
        if(nrow(mtrack) > 0){
          
          # re-sampling the track
          step_df <- mtrack |> 
            mutate(
              x = st_coordinates(mtrack)[,1],
              y = st_coordinates(mtrack)[,2], 
              t = timestamp,
              file = fin
            ) |> 
            make_track(
              x, y, t,
              id = file,
              crs = sf::st_crs(mtrack), 
              all_cols = T
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
            # for transforming the coordinate system, for the future
            # crs_sub <- mt_aeqd_crs(
            # mt_as_move2(sub), center = "centroid", units = "m"
            # )
            # transform_coords(st_crs(crs_sub)) |>
            filter_min_n_burst(min_n = 3) |>
            steps_by_burst(lonlat = T, keep_cols = 'start') 
          
          if(nrow(step_df) > 0){
            
            step_df |> 
              mutate(
                step_id = str_c("step_", 1: n()), 
                track_file = fin 
              ) |> 
              write_rds(here(ddir, "1_one_loc_morning", fout))
            
          } # close if(step_df)
          
        } # close if(nrow(mtrack) > 0)
        
      }) # close map files 
    
    print(paste(sp, "DONE!"))
  
  }) # close map species



# 2 - Generate one file per species -------------------------------------

target_sp |> 
  map(~{
    
    sp <- .x
    ddir <- here("Data", "Studies", sp, "5_distances")
    
    # list all deployments that are re-sampled
    files <- here(ddir, "1_one_loc_morning") |> 
      list.files(full.names = T) |> 
      map(read_rds) |> 
      bind_rows() |> 
      write_rds(here(ddir, "5.1_all_tracks_one_loc_morning_steps.rds"))
    
    print(paste(sp, "DONE!"))
      
  }, .progress = T)



# 3 - Plot steps and turning angles ----------------------------------------

# not useful for now

# making plots of step length and turning angles
# fin <- "5.1_all_tracks_one_loc_morning_steps.rds"
# fout <- str_replace(fin, ".rds", ".pdf")
# 
# target_sp |> 
#   map(~{
#     
#     sp <- .x 
#     sp_dir <- here("Data", "Studies", .x)
#     
#     steps_df <- here(sp_dir, "5_distances", fin) |> 
#       read_rds() |> 
#       rename(step = sl_, turn = ta_) |> 
#       mutate(
#         #step = step/1000, 
#         #dt_ = as.numeric(dt_, units = "hours"), 
#         speed = step/as.numeric(dt_)
#       )
#     
#     # plotting step length
#     pst <- steps_df |> 
#       ggplot() +
#       geom_histogram(
#         aes(x = step), binwidth = 1000, fill = "#9BB655FF", color = "gray22"
#       ) +
#       labs(
#         x = bquote("step length [m] | binwidth =" ~ 10^3), 
#       ) +
#       scale_x_continuous(label = ~custom_scientific(.x, fixed_exp = 3))
#     
#     pta <- steps_df |> 
#       ggplot() +
#       geom_histogram(
#         aes(x = turn), binwidth = 0.1, fill = "#9BB655FF", color = "gray22"
#       ) +
#       labs(x = "turning angle [rad] | binwidth = 0.1") 
#     
#     psp <- steps_df |>
#       ggplot() +
#       geom_histogram(
#         aes(x = speed), binwidth = 1, fill = "#9BB655FF", color = "gray22"
#       ) +
#       labs(x = "speed [m/s] | binwidth = 1") 
#       # theme_bw(base_size = 14)
#     
#     
#     ggarrange(pst, pta, psp, nrow = 1) |> 
#       annotate_figure(
#         top = text_grob(
#           str_c(str_replace(sp, "_", " "), " - one location per day"), 
#           face = "bold", size = 14
#         )
#       )
#         
#     ggsave(
#       here(sp_dir, "Graphs", fout), 
#       width = 30, 
#       units = "cm"
#     )
#     
#     print(str_c(.x, " DONE!"))
#         
#    
#   })

# Warning messages:
# 1: Removed 963 rows containing non-finite values (`stat_bin()`). 
# 2: Removed 529 rows containing non-finite values (`stat_bin()`). 
# 3: Removed 84 rows containing non-finite values (`stat_bin()`). 
# 4: Removed 191 rows containing non-finite values (`stat_bin()`). 
# 5: Removed 580 rows containing non-finite values (`stat_bin()`). 
# 6: Removed 1 rows containing non-finite values (`stat_bin()`). 


# Track summary -----------------------------------------------------

# skipped because it was never used, but kept code just in case

# providing the summary of re-sampled tracks
# 
# target_sp |> 
#   map(~{
#     
#     sp <- .x
#     ddir <- here("Data", "Studies", sp, "5_distances")
#     
#     # list all deployments that are re-sampled
#     files <- here(ddir, "1_one_loc_morning") |> 
#       list.files()
#     
#     lfl <- length(files)
#     
#     files |> 
#       map(~{
#         
#         fin <- .x
#         
#         print(paste(sp, which(fin == files), "|", lfl))
#         
#         track <- here(ddir, "1_one_loc_morning", fin) |> 
#           read_rds()
#         
#         if(nrow(track) > 1) {
#           
#           track |> 
#             summarize_sampling_rate(time_unit = "hour") |> 
#             mutate(file = fin) 
#           
#         } else {
#           
#           tibble(
#             file = fin, 
#             comment = "only one location after re-sampling",
#           )
#           
#         }
#         
#       }) |> 
#       bind_rows() |> 
#       write_csv(
#         here(ddir, "5.1_all_tracks_one_loc_morning_summary.csv")
#       )
#     
#     print(paste(sp, "DONE!"))
#     
#   }, .progress = T)


