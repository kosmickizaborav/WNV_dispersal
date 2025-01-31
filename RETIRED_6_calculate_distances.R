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
library(paletteer)

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
    
    # folder with species data
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
    
    # folder with species data
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
            day_period == "night" & timestamp >= (max(timestamp)-hours(2)), 
            .by = dcp
          ) 
        
        if(nrow(track) >= 3){
          
         steps <- track |> 
          track_resample(
             rate = resample_rate,
             tolerance = resample_tolerance
           ) |> 
           filter_min_n_burst(min_n = 3) |>
           # keep columns from the start, so that we have the same day
           # cycle in night and days later, if we would keep info for the end
           # it would point to the next day
           steps_by_burst(lonlat = T, keep_cols = 'start')  |> 
           mutate(track_file = fin) 
         
        } else(
          
          steps <- tibble()
          
        )
        
        # if there is no data, return a tibble with the file name and a comment
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
      filter(comment == "night steps available") |> # no night steps available
      # remove all the columns that are associated with night steps
      select(
        -any_of(
          c(
            "direction_p", "ta_", "date", "day_period", "dcp", "dt_", 
            "sl_", "comment", "timestamp", "x2_", "y2_"
          )
        )
      ) |> 
      # geometry saved but without a coordinate system, so we redefine it later
      st_drop_geometry()
    
    if(nrow(night_steps) > 0){
    
    # list all deployments
    files <- unique(night_steps$track_file)
    lfl <- length(files)
    
    day_steps <- files |> 
      map(~{
        
        fin <- .x
        
        print(paste(sp, which(fin == files), "|", lfl))
        
        # taking only the points around the dawn
        track <- here(sp_dir, "5_resampled", fin) |> 
          read_rds() 
        
        df <- night_steps |> 
          filter(track_file == fin) |> 
          st_as_sf(coords = c("x1_", "y1_"), crs = st_crs(track)) |> 
          group_split(step_id, burst_) |> 
          map(~{
            
            step_df <- .x 
            
            track |> 
              select(t_, x_, y_, day_period, dcp) |> 
              filter(t_ > step_df$t1_, t_ <= step_df$t2_) |> 
              rename_with(~str_replace(.x, "_$", "2_")) |>
              mutate(sl_ = st_distance(geometry, step_df)[,1]) |> 
              mutate(
                t1_ = step_df$t1_, 
                x1_ = st_coordinates(step_df)[,1], 
                y1_ = st_coordinates(step_df)[,2],
                step_id = step_df$step_id, 
                burst_ = step_df$burst_
              ) |> 
              # remove the points that fall during the night
              # in case we selected the t2_ from the night step 
              filter(day_period == "day") |> 
              # odrder by distance and time
              arrange(sl_, desc(t2_)) |> 
              # take the last value (highest sl_ /and earliest t2_)
              slice_tail(n = 1) |> 
              st_drop_geometry() |> 
              # adding other characteristics of the step
              left_join(
                step_df |> select(-t2_) |> st_drop_geometry(), 
                by = c("t1_", "step_id", "burst_")
              )
            
          }) |>  # close map step_id
          bind_rows() |> 
          mutate(track_file = fin) 
        
      }) |>  # close map files
      bind_rows() |> 
      write_rds(here(sp_dir, "6_distances", day_file))
  
    } else{
      
    print(paste(sp, "- no night steps available"))
    
    }
   
    print(paste(sp, "DONE!"))
    
  }) # close map species 



# 4 - Sumarize number of steps per day ------------------------------------

# define palette for the graph
colp <- paletteer_d("MoMAColors::Lupi")
# define breaks for the colors 
col_break <- c(1, 5, 10, 25, 50, 75, 100)

step_summary <- target_sp |> 
  map(~{
    
    sp <- .x 
    # folder of the species data
    sp_dir <- here("Data",  "Studies", str_replace(sp, " ", "_"))
    
    night_steps <- here(sp_dir, "6_distances", night_file) |> 
      read_rds() |> 
      filter(comment == "night steps available")  # no night steps available
      
    
    if(nrow(night_steps) > 0){
      
      here(sp_dir, "6_distances", day_file) |> 
        read_rds() |> 
        bind_rows(
          night_steps |> 
            mutate(sl_ = units::set_units(sl_, "m"))
        ) |> 
        separate_wider_delim(
          col = day_cycle, delim = "_", names = c("yday", "year")
        ) |> 
        summarize(
          n_steps = n(), 
          n_individuals = length(unique(track_file)), 
          .by = yday
        ) |> 
        mutate(species = sp)
      
    } else{
      
      tibble(
        species = sp, 
        n_steps = NA
      )
      
    }
    
  }) |> # close map species
  bind_rows() |> 
  filter(!is.na(n_steps)) |> 
  mutate(
    yday_date = as.Date(as.numeric(yday), origin = str_c("2000-01-01")), 
    month = month(yday_date), 
    day = day(yday_date)
  ) |> 
  select(-yday_date) |> 
  pivot_longer(
    cols = c(n_steps, n_individuals), 
    names_to = "step_name", 
    values_to = "step_count"
  ) |> 
  mutate(
    pfill = cut(
      step_count, 
      breaks = if_else(
        max(step_count, na.rm = T) > max(col_break), 
        list(c(col_break, max(step_count, na.rm = T))),
        list(col_break)
      )[[1]],
      right = FALSE
    )
  )

# naming colors with the right categories
colp_names <- levels(step_summary$pfill)
colp <- colp[1:length(colp_names)] 
names(colp) <- colp_names

step_summary |> 
  mutate(
    step_name = case_when(
      step_name == "n_steps" ~ "number of steps", 
      step_name == "n_individuals" ~ "number of individuals"
    )
  ) |> 
  ggplot() + 
  geom_tile(
    aes(y = month, x = day, fill = pfill),
    color = "gray11",
    na.rm = TRUE,
    linewidth = 0.5
    #color = "black"
  ) +
  labs(
    fill = "count",
    title = "Distributuion of avaialble steps per species"
  ) +
  scale_y_continuous(trans = "reverse", breaks = seq(1,12, 2)) +
  scale_x_continuous(breaks = seq(1, 31, 2), limits = c(1, 31)) +
  scale_fill_manual(values = colp) +
  coord_fixed(ratio = 1) +
  facet_grid(species~step_name + day_period) +
  theme_bw() +
  guides(fill = guide_legend(nrow = 1)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    legend.position = "bottom"
  ) 


ggsave(
  here("Data", "Graphs", "6_available_steps_per_species.png"), 
  height = 20, 
  width = 25,
  units = "cm"
)


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
              theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank()
              )
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
              )
            )
            
          }
          
          
          print(fname)
          
        })
      
    } else{
      
      print(paste(sp, "- no night steps available"))
      
    }
    
    print(paste(sp, "DONE!"))

}) # close map species



# summary 2 ---------------------------------------------------------------


night_df <- target_sp |> 
  map(~{
    
    sp <- .x 
    # folder of the species data
    sp_dir <- here("Data",  "Studies", str_replace(sp, " ", "_"))
    
    night_steps <- here(sp_dir, "6_distances", night_file) |> 
      read_rds() |> 
      mutate(period = "night") |> # no night steps available
      units::drop_units() |> 
      mutate(species = sp)

  }) |> 
  bind_rows() |> 
  sf::st_drop_geometry() |>
  filter(comment == "night steps available") |> 
  select(t1_, t2_, day_cycle, sl_, period, track_file, step_id, within_eubb, species) 

info <- night_df |> 
  distinct(day_cycle, t1_, t2_, track_file, within_eubb) |> 
  rename(t2_night = t2_)

day_df <- target_sp |> 
  map(~{
    
    sp <- .x 
    # folder of the species data
    sp_dir <- here("Data",  "Studies", str_replace(sp, " ", "_"))
    
    if(file.exists(here(sp_dir, "6_distances", day_file))){
      
      here(sp_dir, "6_distances", day_file) |> 
        read_rds() |> 
        mutate(period = "day") |> 
        drop_units() |> 
        mutate(species = sp)
      
    } else{
      tibble()
    }
  }) |> 
  bind_rows() |> 
  left_join(info) |> 
  rename(t2_day = t2_) |> 
  filter(t2_day != t2_night)
  

df <- bind_rows(night_df, day_df) |> 
  select(sl_, species, period, day_cycle, within_eubb) |> 
  filter(within_eubb == T) |> 
  separate_wider_delim(
    col = day_cycle, delim = "_", names = c("yday", "year")
  ) |> 
  mutate(
    sl_ = round(sl_/1000),
    yday_date = as.Date(as.numeric(yday), origin = str_c("2000-01-01")), 
    month = month(yday_date), 
    day = day(yday_date)
  ) |> 
  summarise(
    sl_ = median(sl_, na.rm = T),
    .by = c(species, period, month, day, yday)
  ) |> 
  mutate(
    pfill = cut(
      sl_, 
      breaks = c(0, 0.1, 5, 10, 25, 50, 75, 100, 150, max(sl_)), 
      right = FALSE
    )
  )

length(levels(df$pfill))

colp_names <- levels(df$pfill)
colp <- c("gray", paletteer_d("MoMAColors::Lupi"))
names(colp) <- colp_names


df |> 
  mutate(period = if_else(period == "day", "dan", "noć") |> factor(levels = c("dan", "noć"))) |> 
  ggplot() +
  geom_tile(
    aes(y = month, x = day, fill = pfill),
    color = "gray11",
    # color = day_period excluded day for now
    na.rm = TRUE,
    linewidth = 0.5
    #color = "black"
  ) +
  labs(
    fill = "median distanca [km]", 
    title = "Srednje dnevne distance - Evropa"
  ) +
  scale_y_continuous(trans = "reverse", breaks = 1:12) +
  scale_x_continuous(breaks = seq(1,31, 2)) +
  scale_fill_manual(values = colp) +
  # scale_fill_stepsn(
  #   colours = c("black", colp), 
  #   breaks = c(0, midp, pbreaks)
  # ) +
  facet_grid(species~period) +
  theme_bw() +
  #guides(fill = guide_legend(nrow = 1)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    legend.position = "bottom"
  ) 


ggsave(
  here("Data", "Graphs", "6_available_night_steps_per_species.png"), 
  height = 25, 
  width = 20,
  units = "cm"
)
  
  theme(
  legend.position = "bottom"
)
# 

ggsave(
  here(
    "Data", 
    "Graphs", 
    "dan_noc.png"
  )
)


