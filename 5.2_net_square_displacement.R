#' ---
#' title: "Calculating maximum distances per day cycle"
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
library(ggpubr)
library(amt)
library(patchwork)

source("0_helper_functions.R")


# getting the species of interest
target_sp <- here("Data", "1_downloadable_studies_deployments_filtered.csv") |> 
  read_csv(show_col_types = F) |> 
  distinct(species) |> 
  as_vector() |> 
  str_replace(" ", "_")

# for calculating maximum distances during the day,
# for the data that has high resolution, we first re-sampled the track to
res_rate = minutes(30) 
res_tolerance = minutes(5)



# 1 - Calculating net square daily displacement ---------------------------


id_cols <- c(
  "study_id", "deployment_id", "individual_id", "individual_local_identifier"
)

sampling_rate <- here("Data", "Studies", "5.2_all_tracks_sampling_rate.rds") |> 
  read_rds()

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
        
        track <- track |> 
          track(
            x = st_coordinates(track)[,1],
            y = st_coordinates(track)[,2],
            t = mt_time(track),
            id = individual_local_identifier,
            crs = st_crs(track)
          ) 
        
        # res-ampling the track if the sampling rate is smaller than the 
        # predefined treshold
        if( og_sampling < (res_rate - res_tolerance) ) {
          
          track <- track |> 
            # because this re-sample is faster than the move2 one
            track_resample(
              rate = res_rate,
              tolerance = res_tolerance
            ) |> 
            select(-timestamp, -individual_local_identifier, -burst_) |> 
            mutate(track_subset = "yes") 
        }
        
        track |>  
          # for each day cycle calculate max distance
          group_split(day_cycle) |> 
          map(~{
              
            day_track <- .x |> 
              mutate(
                row_id = str_c("row_", 1:n()), 
                locs_per_day = n()
              ) |> 
              add_nsd() |>
              mutate(t1_ = t_[1], x1_ = x_[1], y1_ = y_[1]) |> 
              filter(nsd_ == max(nsd_)) |> 
              rename(t2_ = t_, x2_ = x_, y2_ = y_) 
           
          }) |> # map day_cycle
          bind_rows() |> 
          mutate(
            file = fname, 
            sqrt_nsd_ = sqrt(nsd_), 
            sl_ = sqrt_nsd_
          )
  
        
      }) |>  # map_files
      bind_rows() |> 
      write_rds(
        here(sp_dir, "5_distances", "5.2_all_tracks_net_square_displacement.rds")
      )
    
    print(paste(sp, "DONE!"))
    
  }, .progress = T) # map species




# Plot net square displacement --------------------------------------------

# dist_files <- c(
#   "5.1_all_tracks_one_loc_per_day_bursts.rds", 
#   "5.1_all_tracks_one_loc_per_day_morning_bursts.rds", 
#   "5.2_all_tracks_max_daily_distance.rds"
# )


# target_sp |> 
#   map(~{
#     
#     sp <- .x
#     sp_dir <- here("Data", "Studies", str_replace(sp, " ", "_"))
#     
#     files <- here(sp_dir, "5_distances") |> 
#       list.files(pattern = "graph_data.rds")
#     
#     net <- here(
#       sp_dir, "5_distances", "5.2_all_tracks_net_sqare_displace.rds"
#       ) |> 
#       read_rds() |> 
#       filter(row_id != "row_1")
#     
#     df <- files |> 
#       map(~{
#         
#         df <- here(sp_dir, "5_distances", .x) |> 
#           read_rds() |> 
#           rename(day_cycle = day_id) 
#         
#         t <- unique(df$file_id)
#         
#         df <- df |> 
#           left_join(
#             net, 
#             by = c("study_id", "individual_id", "deployment_id", 
#                    "file", "day_cycle")
#           ) |> 
#           select(sl_, nsd_) |> 
#           ggplot() +
#           geom_point(
#             aes(x = sl_, y = nsd_),
#             color = "#9BB655FF", alpha = 0.2, na.rm = T
#           ) +
#           labs(
#             x = "step length [m]", 
#             y = "net square displacement",
#             title = t
#           ) +
#           theme_bw()
#         
#       }) |> 
#       reduce(`+`) + 
#       plot_layout(ncol = 1) + 
#       plot_annotation(
#         title = "Daily distances vs. net square displacement", 
#         theme = theme(
#           plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
#         )
#       ) 
#     
#     # Save the combined plot
#     ggsave(
#       here(sp_dir, "Graphs", "5.2_net_disp_vs_dist.pdf"), 
#       height = 25, units = "cm"
#     )
#          
#       
#     
#     
#     print(paste(sp, "DONE!"))
#     
#     
#   }) # map species
# 
# 
# 
