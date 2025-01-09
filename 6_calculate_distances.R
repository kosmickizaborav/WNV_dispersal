#' ---
#' title: "Resample tracks"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#'  **SECTION 1 - Distances per day period**
#'  


# 0 - Defining parameters and packages ---------------------------------------

library(tidyverse)
library(move2)
library(amt)
library(here)
library(sf)
library(units)

# getting the species of interest
# getting the species of interest
target_sp <- here("Data", "1_downloadable_studies_deployments_filtered.rds") |> 
  read_rds() |>
  distinct(species) |> 
  as_vector()


# create output directory
target_sp |> 
  map(~{
    
    sp_dir <- here("Data", "Studies", str_replace(.x, " ", "_"))
    
    out_dir <- here(sp_dir, "6_distances")
    
    if(!dir.exists(out_dir)){
      dir.create(out_dir)
      # dir.create(here(out_dir, "night_steps"))
    }
    
    graph_dir <- here(sp_dir, "Graphs", "6_distances_plots")
    
    if(!dir.exists(graph_dir)){
      dir.create(graph_dir)
      # dir.create(here(out_dir, "night_steps"))
    }
    
  })

resample_rate <- hours(24)
resample_tolerance <- hours(2)

# output tracks
dcp_file <- "1_all_tracks_dcp_distances.rds"
night_file <- "2_all_tracks_night_steps.rds"
day_file <- "3_all_tracks_max_day_steps.rds"


# 1 - Distance between day/night points -----------------------------------


target_sp |> 
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
        
        print(paste(sp, which(fin == files), "|", lfl))
        
        track <- here(sp_dir, "5_resampled", fin) |> 
          read_rds() |> 
          filter(!is.na(dcp))
        
        if(nrow(track) > 0){
          
          track |> 
            group_split(dcp) |> 
            map(~{
              
              dcp_track <- .x 
              
              dcp_summary <- dcp_track |> 
                st_drop_geometry() |> 
                select(
                  all_of(
                    c(
                      "individual_local_identifier", 
                      "dcp", 
                      "day_cycle", 
                      "day_period"
                    )
                  )
                ) |>  
                summarize(
                  across(everything(), unique), 
                  dcp_n_locs = n()
                )
              
              if(nrow(dcp_track) > 1){
                
                # distance matrix
                dist <- dcp_track |> 
                  st_distance()
                dist[lower.tri(dist, diag = TRUE)] <- NA
                
                
                dcp_summary |> 
                  mutate(
                    dcp_d_min = min(dist, na.rm = T),
                    dcp_d_max = max(dist, na.rm = T),
                    dcp_d_mean = mean(dist, na.rm = T),
                    dcp_d_median = median(dist, na.rm = T)
                  ) |> 
                  mutate(
                    across(
                      starts_with("dcp_d"), ~ifelse(is.numeric(.x), .x, NA)
                    ), 
                    comment = "distances calculated"
                  ) 
                
              } 
              
            }) |>  # map period
            bind_rows() |> 
            mutate(track_file = fin)
          
              
          } else { # close track > 0
            
            tibble(
              track_file = fin, 
              comment = "insufficient data to calculate distances"
            )
            
          }
          
      }) |>  # close map files
      bind_rows() |> 
      write_rds(here(sp_dir, "6_distances", dcp_file))
    
    print(paste(sp, "DONE!"))
    
  }) # close map species                                                                                                                                                                                                                                                                                                                                                                                                                      


# 2 - Steps per night -----------------------------------------------------


# calculate the sampling rate per track for all species
target_sp |> 
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
        
        print(paste(sp, which(fin == files), "|", lfl))

        track <- here(sp_dir, "5_resampled", fin) |> 
          read_rds() |> 
          filter(
            day_period == "night" & timestamp == max(timestamp), 
            .by = dcp
          ) 
        
        if(nrow(track) >= 3){
          
         steps <- track |> 
          track_resample(
             rate = resample_rate,
             tolerance = resample_tolerance
           ) |> 
           filter_min_n_burst(min_n = 3) |>
           steps_by_burst(lonlat = T, keep_cols = 'start')  |> 
           mutate(track_file = fin) 
         
        } else(
          
          steps <- tibble()
          
        )
        
        if(nrow(steps) == 0){
          
          tibble(
            track_file = fin, 
            comment = "no night steps available"
          )
          
        } else {
          
          steps |> 
            mutate(
              step_id = str_c("step_", 1:n()), 
              comment = "night steps available"
            )
          
        }
      
      }) |> # close map files
      bind_rows() |> 
      write_rds(here(sp_dir, "6_distances", night_file))
    
    print(paste(sp, "DONE!"))
            
})



# 3 - Max day steps ------------------------------------------------------------

target_sp |> 
  map(~{
    
    sp <- .x 
    # folder of the species data
    sp_dir <- here("Data",  "Studies", str_replace(sp, " ", "_"))
    
    # loading the one loc per day morning steps 
    night_steps <- here(sp_dir, "6_distances", night_file) |> 
      read_rds() |> 
      filter(comment == "night steps available") # no night steps available
      # select(-comment)
    
    if(nrow(night_steps) > 0){
    
    # list all deployments
    files <- unique(night_steps$track_file)
    lfl <- length(files)
    
    files |> 
      map(~{
        
        fin <- .x
        
        print(paste(sp, which(fin == files), "|", lfl))
        
        # taking only the points around the dawn
        track <- here(sp_dir, "5_resampled", fin) |> 
          read_rds() 
        
        night_steps |> 
          filter(track_file == fin) |> 
          st_as_sf(coords = c("x1_", "y1_"), crs = st_crs(track)) |> 
          group_split(step_id) |> 
          map(~{
            
            step_df <- .x 
            
            strack <- track |> 
              select(t_, x_, y_, day_period) |> 
              filter(t_ >= step_df$t1_, t_ <= step_df$t2_) |> 
              rename_with(~str_replace(.x, "_$", "2_"))
            
            check <- strack |>
              mutate(sl_ = st_distance(strack, step_df)[,1]) |> 
              mutate(
                t1_ = step_df$t1_, 
                x1_ = st_coordinates(step_df)[,1], 
                y1_ = st_coordinates(step_df)[,2],
                step_id = step_df$step_id, 
                burst_ = step_df$burst_
              ) |> 
              filter(day_period == "day") |> 
              arrange(sl_, t2_) |> 
              slice_tail(n = 1) |> 
              st_drop_geometry()
            
          }) |>  # close map step_id
          bind_rows() |> 
          mutate(track_file = fin) |> 
          select(-day_period)
        
      }) |>  # close map files
      bind_rows() |> 
      write_rds(here(sp_dir, "6_distances", day_file))
  
    } else{
      
    print(paste(sp, "- no night steps available"))
    
    }
   
    print(paste(sp, "DONE!"))
    
  }) # close map species 



# 4 - Making graphs --------------------------------------------------------

# paletteer::paletteer_d("rcartocolor::Tropic")
colp <- c(paletteer::paletteer_d("rcartocolor::Geyser"))
pbreaks <- c(1000, 5000, 10000, 25000, 50000, 100000)

target_sp |> 
  map(~{
    
    sp <- .x 
    # folder of the species data
    sp_dir <- here("Data",  "Studies", str_replace(sp, " ", "_"))
    
    midp <- here(sp_dir, "6_distances", dcp_file) |>
      read_rds() |> 
      filter(day_period == "night") |> 
      summarize(mid = median(dcp_d_median, na.rm = T)) |> 
      pull(mid) |> 
      round()
    
    night_df <- here(sp_dir, "6_distances", night_file) |> 
      read_rds() |> 
      filter(comment == "night steps available") # no night steps available
    
    if(nrow(night_df) > 0){
      
      night_df <- night_df |> 
        filter(!is.na(step_id)) |> 
        mutate(sl_ = units::set_units(sl_, "m")) |> 
        # select(any_of(matches("step_id|burst_|track_|[txy][12]_|day_|sl_|step"))) |> 
        separate_wider_delim(
          col = day_cycle, delim = "_", names = c("yday", "year")
        ) |> 
        mutate(
          across(all_of(c("yday", "year")), as.numeric),
          yday_date = as.Date(yday, origin = str_c(year, "-01-01")), 
          month = month(yday_date), 
          day = day(yday_date)
        ) |> 
        drop_units() |> 
        select(
          t1_, track_file, step_id, month, day, day_period, year, sl_, 
          manipulation_type, country, within_eubb
        ) 
      
      
      day_df <- here(sp_dir, "6_distances", day_file) |>
        read_rds() |>
        drop_units() |>
        select(sl_, t1_, step_id, track_file) |> 
        left_join(night_df |> select(-sl_, -day_period)) |>
        mutate(
          month = month + 0.5,
          day_period = "day"
        ) 
      
      full_df <- night_df |>
        bind_rows(day_df) |> 
        mutate(
          country_plot = case_when(
            !is.na(country) ~ country, 
            is.na(country) & within_eubb ~ "within European boundary box",
            is.na(country) & within_eubb ~ "outside European boundary box"
          ), 
          day_period = factor(day_period, levels = c("night", "day"))
        )
      
      rm(day_df, night_df)
      
      # pbreaks <- unique(round(seq(
      #   1000, max(night_df$sl_, na.rm = T), length.out = length(colp)
      # )/1000)*1000)
      
      full_df |> 
        group_split(track_file) |>
        map(~{
          
          tdf <- .x
          
          fname <- tdf |> 
            pull(track_file) |>
            unique() 
          
          if(nrow(tdf) > 60){
            
            subp <- paste(
              "manipulation:",
              unique(tdf$manipulation_type),
              "| country:",
              unique(tdf$country_plot)
            )
            
            
            max_dist <- ifelse(max(tdf$sl_) > 100000, max(tdf$sl_), 150000) |> 
              ceiling()
            
            tdf <- tdf |> 
              mutate(
                pfill = cut(
                  sl_, 
                  breaks = c(0, midp, pbreaks, max_dist), 
                  labels = c(
                    str_c("< ", midp, "m"), 
                    "< 1km", 
                    "< 5km", 
                    "<10km", 
                    "<25km", 
                    "<50km", 
                    "<100km", 
                    str_c("<", round(max_dist/1000), "km")
                  ), 
                  right = FALSE
                )
              )
            
            colp <- c("gray22", colp)
            names(colp) <- levels(tdf$pfill)
            
            
            tdf |> 
              ggplot() + 
              geom_tile(
                aes(y = month, x = day, fill = pfill, color = day_period),
                # color = day_period excluded day for now
                na.rm = TRUE,
                linewidth = 0.5
                #color = "black"
              ) +
              labs(
                fill = "distance", 
                color = "day period",
                title = paste(sp, "- distance between steps"),
                subtitle = subp, 
                caption = paste("file name:", fname)
              ) +
              scale_y_continuous(trans = "reverse", breaks = 1:12) +
              scale_x_continuous(breaks = 1:31, limits = c(1, 31)) +
              scale_fill_manual(values = colp) +
              # scale_fill_stepsn(
              #   colours = c("black", colp), 
              #   breaks = c(0, midp, pbreaks)
              # ) +
              scale_colour_manual(values = c("day" = "orange", night = "black")) +
              coord_fixed(ratio = 1) +
              facet_wrap(~year, ncol = 1) +
              theme_bw() +
              theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())
            # theme(
            #   legend.position = "bottom",
            #   legend.text = element_text(angle = 90, vjust = 0.5)
            # )
            # 
            
            ggsave(
              here(
                sp_dir, 
                "Graphs", 
                "6_distances_plots", 
                str_replace(fname, "_resampled.rds",  "_distance.png")
              ), 
              width = 10, 
              height = 20, 
              units = "cm"
            )
            
          }
          
          
          print(fname)
          
        })
      
    } else{
      
      print(paste(sp, "- no night steps available"))
      
    }
    
    print(paste(sp, "DONE!"))

}) # close map species
