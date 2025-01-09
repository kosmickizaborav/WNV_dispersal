# 0 - packages and files --------------------------------------------------

# to handle movement data
library(tidyverse)
library(here)
library(sf)
library(ggpubr)
library(patchwork)
library(move2)
library(paletteer)
library(amt)

source("0_helper_functions.R")

set.seed(666)

# getting the species of interest
target_sp <- here("Data", "1_downloadable_studies_deployments_filtered.csv") |> 
  read_csv(show_col_types = F) |> 
  distinct(species) |> 
  as_vector() |> 
  str_replace(" ", "_")

target_sp |> 
  map(~{
    
    tdir <- here("Data",  "Studies", .x, "Graphs", "track_samples")
    
    if(!dir.exists(tdir)){ tdir |>  dir.create()}
    
    ddir <- here("Data", "Studies", .x, "5_distances", "track_samples")
    
    if(!dir.exists(ddir)){ ddir |>  dir.create()}
    
  })

dist_files <- c(
  "5.3_all_tracks_one_loc_per_day_bursts_graph_data.rds", 
  "5.3_all_tracks_one_loc_per_day_morning_bursts_graph_data.rds", 
  "5.3_all_tracks_max_daily_distance_graph_data.rds"
  # "5.3_all_tracks_net_square_displacement_graph_data.rds"
)


# col_id <- c("study_id", "deployment_id", "individual_id", 
#             "individual_local_identifier",  "day_cycle", "species")


# 1 - Prepare track samples -----------------------------------------------


target_sp |> 
  map(~{
    
    sp <- .x 
    
    # folder of the species data
    sp_dir <- here("Data",  "Studies", sp)
    
    # sampling 10 trajectories of 3 days length
    plot_lim <- here(
      sp_dir, "5_distances", "5.1_all_tracks_one_loc_per_day_morning_bursts.rds"
      ) |> 
      read_rds() |> 
      summarize(
        date_start = date(min(t1_)),
        date_end = date(max(t2_)),
        n_days = length(unique(date(t1_))), 
        .by = c(file, burst_)
      ) |> 
      filter(n_days >= 4) |> 
      select(-burst_) |>
      slice_sample(n = 1, by = file) |>
      slice_sample(n = 10) |>
      mutate(
        plot_start = sample(seq(date_start, date_end - 3, 1), 1),
        plot_end = plot_start + 3,
        .by = file
      ) |>
      select(file, plot_start, plot_end)
    
    # list all deployments previously filtered
    track_files <- plot_lim |> 
      pull(file)
    
    # subsetting each type of distance 
    # and merging them all in one file
    dist_df <- dist_files |> 
      map(~{
        
        here(sp_dir, "5_distances", .x) |> 
          read_rds() |> 
          # keeping only the files and time range that are selected  above
          filter(file %in% track_files) |> 
          left_join(plot_lim, by = "file") |> 
          filter(date(t1_) >= plot_start, date(t2_) <= plot_end) |>
          mutate(step_id = 1:n(), .by = file) |>
          select(file, step_id, file_id, any_of(matches("^[txy][12]_$|sl_km$"))) |>
          pivot_longer(
            cols = any_of(matches("t[12]_")),
            values_to = "timestamp",
            names_to = "loc_type"
          ) |>
          mutate(
            x = case_when(
              loc_type == "t1_" ~ x1_,
              loc_type == "t2_" ~ x2_
            ),
            y = case_when(
              loc_type == "t1_" ~ y1_,
              loc_type == "t2_" ~ y2_
            ),
            loc_type = case_when(
              loc_type == "t1_" ~ str_c(step_id, "A"),
              loc_type == "t2_" ~ str_c(step_id, "B")
            )
          ) |>
          group_split(file, file_id, timestamp) |>
          map(~{

            .x |>
              mutate(point_id = str_c(.x$loc_type, collapse = "|"))

          }) |>
          bind_rows() |>
          distinct(file, file_id, timestamp, point_id, x, y, sl_km) |>
          st_as_sf(coords = c("x", "y"), crs = st_crs(4326))

      }) |> 
      bind_rows() |> 
      rename(loc_id = file_id)
      #reduce(full_join, by = c("timestamp", "file"))
    
    lfl <- length(track_files)
    
    # loading each file
    track_files |> 
      map(~{
        
        fname <- .x
        
        fout <- str_replace(
          fname, "depl_speed_filtered.rds", "track_sample.rds"
        )
        
        print(paste(sp, which(fname == track_files), "|", lfl))
        
        dist <- dist_df |> 
          filter(file == fname) 
        
        og_track <- here(sp_dir, "4_filtered_speed", fname) |> 
          read_rds() |> 
          # subset the track to the limits used 
          filter(
            timestamp >= min(dist$timestamp) & timestamp <= max(dist$timestamp)
          ) |> 
          mutate(
            day_period = if_else(
              timestamp >= day_start & timestamp <= day_end, "day", "night" 
            )
          ) |> 
          select(timestamp, geometry, day_period) |> 
          mutate(
            loc_id = "track data", 
            file = fname
          )
      
        crs_aeq <- mt_aeqd_crs(og_track, center = "centroid", units = "m")
        
        og_track <- og_track |> st_transform(crs_aeq) 
        
        dist |>
          st_transform(crs_aeq) |> 
          bind_rows(og_track) |> 
          mutate(
            yearday = yday(timestamp), 
            study_id = str_split_i(file, "_", 1), 
            individual_id = str_split_i(file, "_", 3), 
            deployment_id = str_split_i(file, "_", 5)
          ) |> 
          write_rds(
            here(sp_dir, "5_distances", "track_samples", fout)
          )
        
      }) # map files
    
    print(paste(sp, "DONE!"))
    
  })
    



# 2 - Plot track samples-------------------------------------------------------


target_sp |> 
  map(~{
    
    sp <- .x 
    
    # folder of the species data
    sp_dir <- here("Data",  "Studies", sp)
    
    files <- here(sp_dir, "5_distances", "track_samples") |> 
      list.files()
    
    files |> 
      map(~{
        
        fname <- .x 
        
        print(fname)
        
        fout <- fname |> 
          str_replace(".rds", ".pdf")
        
        track <- here(sp_dir, "5_distances", "track_samples", fname) |> 
          read_rds()
        
        # pal <- c("#4F3855FF", "#9BB655FF", "#D89873FF", "blue")
        # 
        # names(pal) <- levels(og_track$yearday)
        
        ptitle <- paste(
          str_replace(sp, "_", " "), 
          "- deployment ID:", 
          unique(track$deployment_id)
        )
        
        psub <- paste(
          "time period:",
          min(date(track$timestamp)), "-", max(date(track$timestamp))
        )
        
        og_track <- track |> 
          filter(loc_id == "track data") |> 
          select(-loc_id) |> 
          mutate(ddate = date(timestamp))
        
        track_lines <- st_sf(
          yearday = og_track$yearday[-1], # example attribute
          geometry = st_cast(st_combine(og_track), "LINESTRING")
        )
        
        tp <- track |> 
          filter(loc_id != "track data") |> 
          ggplot() +
          geom_sf(data  = track_lines, color = "gray88", alpha = 0.2) +
          geom_sf(
            data = og_track, 
            aes(
              shape = day_period, 
              color = ddate
            ), 
            size = 3, 
            alpha = 0.4
          ) +
          geom_sf(
            shape = 8,
            # aes(shape = loc_id), 
            color = "black",
            #shape = 2,
            alpha = 0.8,
            size = 3
          ) +
          # geom_sf(data  = mt_track_lines(og_track), alpha = 0.2) +
          ggrepel::geom_text_repel(
            aes(label = point_id, geometry = geometry),
            stat = "sf_coordinates",
            min.segment.length = Inf
          ) +
          labs(
            x = "", y = "",
            title = ptitle,
            subtitle = psub, 
            shape = "day period", 
            color = "date"
          ) +
          facet_wrap(~loc_id, nrow = 1) +
          theme_bw() +
          theme(
            legend.position = "bottom"
          ) +
          scale_colour_paletteer_c(
            "grDevices::Plasma", 
            breaks = c(min(og_track$ddate), max(og_track$ddate))
          )
          # scale_x_continuous(n.breaks = 2) +
          # scale_y_continuous(n.breaks = 2) +
          #paletteer::scale_colour_paletteer_d("ggthemes::excel_Aspect") 
          #paletteer_c("grDevices::rainbow", 30) 
          #paletteer::scale_colour_paletteer_c("ggthemes::Gold-Purple Diverging")
        
      
        dp <- track |> 
          filter(loc_id != "track data") |> 
          mutate(
            loc_id = str_replace_all(loc_id, "_", " "), 
            point_id = str_sub(point_id, 1, 1)
          ) |> 
          distinct(sl_km, loc_id, point_id) |> 
          ggplot() +
          geom_point(aes(x = sl_km, y = loc_id), position = "jitter") + 
          labs(
            x = "step length [km]", 
            y = "", 
            title = "relevant step length"
          ) +
          theme_bw()
        
        nsdp <- og_track |> 
          track(
            x = st_coordinates(og_track)[,1],
            y = st_coordinates(og_track)[,2],
            t = og_track$timestamp,
            id = og_track$file,
            crs = st_crs(og_track)
          ) |> 
          add_nsd() |>
          mutate(nsd_ = nsd_/1000000) |> 
          ggplot() +
          geom_line(aes(x = timestamp, y = nsd_)) +
          theme_bw() + 
          labs(
            y = "net square displacement"
          )
          
        
        
        ggarrange(
          tp, ggarrange(dp, nsdp, nrow = 1), nrow = 2, heights = c(1, 0.5)
        ) 
        
        ggsave(
          here("Data",  "Studies", sp, "Graphs", "track_samples", fout),
          #height = 10, width = 20, 
          units = "cm"
        )
        
        
      }) # map files
   print(paste(sp, "done!"))
    
  }) # map target sp

        
        
#       
#         
# 
#        
#         
#         og_track |> 
#           ggplot() + 
#           geom_sf(alpha = 0.2) + 
#           geom_sf(
#             data = track_lines, alpha = 0.2, lineend = "round") +
#           facet_wrap(~day_cycle)
#         
#         
#         dist_df |> 
#           filter(file == fname) |> 
#           group_split(file_id) |> 
#           map(~{
#             
#             n_fix <- unique(.x$file_id)
#             
#             dist <- dist_df |> 
#               select(timestamp, c, file_id)
#               rename_with
#             og_track |> 
#               right_join(, .by = "timestamp")
#           })
#           
#           
#         
#         
#         
#         
#         
#        
#         
#        
#           read_rds() |> 
#           mutate(
#             day_period = if_else(
#               timestamp >= day_start & day_end <= day_end, "day_time", "night_time"
#             )
#           ) |>  
#           mutate(n_day_period = n(), .by = day_period) |> 
#           select(-event_id) |> 
#           summarize(
#             across(contains(c("_id", "_identifier")), unique), 
#             track_start = min(timestamp), 
#             track_end = max(timestamp), 
#             n_days = length(unique(date(timestamp))), 
#             n_day_time = sum(day_period == "day_time"),
#             n_night_time = sum(day_period == "night_time")
#           ) |> 
#           mutate(
#             fname = fname, 
#             species = species
#           )
#       }) |> 
#       bind_rows() |> 
#       write_rds(
#         here(sp_dir, "5_distances", "5.5_track_summaries_for_day_plots")
#       )
#   }) 
# 
# 
# file |> 
#   mutate(
#     day_time = if_else(
#       timestamp >= day_start & day_end <= day_end, "day", "night"
#     )
#   ) |> 
#   ggplot() + 
#   geom_sf(data= mt_track_lines(file)) +
#   geom_sf(aes(color = day_time)) +
#   geom_text(aes(x = st_coordinates(file)[,1], y = st_coordinates(file)[,2], label = n_id))
# 
