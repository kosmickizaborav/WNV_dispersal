#' ---
#' title: "Plot probability stayed"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' here we plot the probability of staying at the same location for
#' the night steps and the median night locations (dcp) files using different
#' distance thresholds for different tracking devices. The procedure is the 
#' same for both files, the only difference is that in the dcp case we also print
#' the number of locations available for each night, that was used to calculate 
#' median position.
#' data is subset for EUROPE BBOX
#'  **SECTION 1 - Plot probability of staying for night steps file**
#'  **SECTION 2 - Plot probability of staying for median night locations (dcp) file**



# 0 - Load packages -------------------------------------------------------

library(here)
library(tidyverse)
library(amt)
source(here("6_2_plot_functions.R"))

target_sp <- here("Data", "1_downloadable_studies_deployments_filtered.rds") |> 
  read_rds() |>
  distinct(species) |> 
  as_vector()

# coordinates used to check if in EUbb
eu_coords <- list(
  ymin = 34.0,
  ymax = 81.0,
  xmin = -29.0,
  xmax = 69.0
)

# distance limits applied as gps/sigfox error, in meters
sigfox_lim <- c(1:10)*1000
gps_lim <- seq(5, 50, 5)

# 1 - Data for Fede -------------------------------------------------------

# ran for fede, afterwards abandoned, just to keep track of what i sent 

if(F){
 
  fin <- "3_all_tracks_max_day_steps_nauticalDawn_nauticalDusk.rds"
  
  fede_dir <- here("Data", "For Fede")
  if(!dir.exists(fede_dir)){ dir.create(fede_dir) }
  
  target_sp |> 
    map(~{
      
      sp <- .x
      sp_dir <- here("Data",  "Studies", str_replace(sp, " ", "_"))
      
      day_df <- here(sp_dir, "6_distances", fin) |> 
        read_rds() |> 
        filter(within_eubb == T) |> 
        select(
          track_file, individual_local_identifier, day_cycle,
          t1_, t2_, x1_, y1_, x2_, y2_, all_of(matches("sl_")), step_id, 
          manipulation_type, sensor_type
        ) 
      
      day_df |> 
        write_csv(
          here(
            fede_dir, 
            str_c(
              str_replace(sp, " ", "_"), 
              "_EU_max_day_steps_nauticalDawn_nauticalDusk.csv")
          )
        )
      
      
    }) # close map species
  
}

# FUNCTION: check_if_stays ------------------------------------------------

get_p_stayed <- function(df, dist_col = sl_, distance_thr){

  distance_thr |> 
    map(~{
      
      dlim <- .x
      
      df |> 
        mutate(
          stayed_night = {{dist_col}} <= dlim, 
          dist_thr = dlim 
        )
      
    }) |> 
    list_rbind() |> 
    summarize(
      n_tracks = n_distinct(track_file), 
      p_stay = sum(stayed_night)/n(), 
      .by = c(yd, dist_thr)
    )
  
}



# 2 - Night_steps ------------------------------------------------------

fin_ns <-  "2_all_tracks_night_steps_nauticalDawn_nauticalDusk.rds"
sig_title <- "sigfox data - probability of staying based on different thresholds"
gps_title <- "gps data - probability of staying based on different thresholds"
pname_ns <- "p_stayed_night_steps_nauticalDawn_nauticalDusk.png"

target_sp |> 
  map(~{
    
    sp <- .x
    sp_dir <- here("Data",  "Studies", str_replace(sp, " ", "_"))
    
    pname <- str_c("6_4_", str_replace(sp, " ", "_"), "_", pname_ns)
    
    night_df <- here(sp_dir, "6_distances", fin_ns) |> 
      read_rds() |> 
      filter(night_steps_available == T, within_eubb == T) |> 
      select(track_file, day_cycle, sensor_type, sl_, sensor_type) |> 
      mutate(
        day_cycle = as.Date(day_cycle, format = "%d%m%Y"), 
        yd = yday(day_cycle)
      )
    
    df_stayed <- night_df |> 
      filter(sensor_type == "gps") |> 
      get_p_stayed(distance_thr = gps_lim) |> 
      mutate(sensor_type = "gps")
    
    if(sum(night_df$sensor_type == "sigfox") > 0){
      
      sig_stayed <- night_df |> 
        filter(sensor_type == "sigfox") |> 
        get_p_stayed(distance_thr = sigfox_lim) |> 
        mutate(sensor_type = "sigfox")
      
      df_stayed <- df_stayed |> 
        bind_rows(sig_stayed)
      
    }
    
    # plot heatmap of the probability to say using different thresholds
    psp <- df_stayed |> 
      # change so that it is more explanatory in the facet plot titles
      mutate(
        sensor_type = case_when(
          sensor_type == "sigfox" ~ sig_title, 
          sensor_type == "gps" ~ gps_title,
        )
      ) |> 
      plot_tile_timeline(
        y = dist_thr, 
        fill = p_stay, 
        flab = "probability of staying", 
        ylab = "distance treshold [m]", 
        pal = c( "#EF7C12FF", "#DAD8A7FF", "#172869FF")
      ) +
      facet_wrap(~sensor_type, scales = "free_y", ncol = 1) +
      theme(
        plot.margin = margin(t = 0, r = 1, b = 0, l = 0, unit = "cm"), 
        panel.spacing = unit(0, "mm")
      )
    
    # plots counts per day and sensor type
    pcp <- df_stayed |> 
      distinct(yd, n_tracks, sensor_type) |> 
      bind_rows(
        expand.grid(
          yd = 1:366, 
          n_tracks = 0, 
          sensor_type = unique(df_stayed$sensor_type)
        )
      ) |>
      summarize(n_tracks = sum(n_tracks), .by = c(yd, sensor_type)) |>
      mutate(title = "number of tracks per day and sensor type") |> 
      plot_steps_count(
        counts = n_tracks, 
        color = sensor_type, 
        pal = c("#65323EFF", "#1BB6AFFF"), 
        legend_position = "bottom", 
        leglab = "sensor", 
        ylab = "number of tracks per day", 
        y_expand = expansion(mult = c(0, 0.2))
      ) +
      facet_wrap(~title) +
      theme(
        plot.margin = margin(t = 0, r = 1, b = 0, l = 0, unit = "cm"), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.spacing = unit(0, "mm"), 
      ) 
  
    # output plot
    pout <- (pcp/psp) + 
      plot_annotation(
        title = paste(
          str_replace(sp, "_", " "), 
          "in Europe - probability of staying at the same night spot"
        ), 
        subtitle = paste("file:", fin_ns),
        theme = theme(
          plot.title = element_text(hjust = 0.5), 
          plot.subtitle = element_text(hjust = 0.5)
        )
      ) +
      plot_layout(heights = c(1,2), ncol = 1, guides = "collect") & 
      theme(legend.position = "bottom")
    
    # save
    ggsave(
      here("Data", "Graphs", pname), 
      pout, 
      height = 20, width = 30, units = "cm"
    )
      
    
    cat(paste(str_replace(sp, "_", " "), " - DONE!\n"))
    
  }) # close map species

# 3 - DCP ------------------------------------------------------
 
fin_dcp <- "1_all_tracks_dcp_night_steps_nauticalDawn_nauticalDusk.rds"
sig_title <- "sigfox data - probability of staying based on different thresholds"
gps_title <- "gps data - probability of staying based on different thresholds"
pname_dcp <- "p_stayed_dcp_night_steps_nauticalDawn_nauticalDusk.png"

target_sp |> 
  map(~{
    
    sp <- .x
    sp_dir <- here("Data",  "Studies", str_replace(sp, " ", "_"))
    
    pname <- str_c("6_4_", str_replace(sp, " ", "_"), "_", pname_dcp)
    
    dcp_df <- here(sp_dir, "6_distances", fin_dcp) |> 
      read_rds() |> 
      mutate(
        within_eubb = (
          x1_ >= eu_coords$xmin & x1_ <= eu_coords$xmax & 
            y1_ >= eu_coords$ymin & y1_ <= eu_coords$ymax
        )
      ) |> 
      filter(night_steps_available == T, within_eubb == T) |> 
      select(track_file, day_cycle, sl_, n_locs) |> 
      mutate(
        day_cycle = as.Date(day_cycle, format = "%d%m%Y"), 
        yd = yday(day_cycle)
      )
    
    dcp_df <- dcp_df |> 
    left_join(
      dcp_df |> 
        distinct(track_file) |> 
        pull(track_file) |> 
        map(~{
          
          fin <- .x
          
          here(sp_dir, "5_resampled", fin) |> 
            read_rds() |> 
            distinct(sensor_type) |> 
            mutate(track_file = fin)
          
        }) |>
        list_rbind(), 
      by = "track_file"
    ) 
    
    df_stayed <- dcp_df |> 
      # adding info for sensor type that is not present in dcp file
      filter(sensor_type == "gps") |> 
      get_p_stayed(distance_thr = gps_lim) |> 
      mutate(sensor_type = "gps")
    
    if(sum(dcp_df$sensor_type == "sigfox") > 0){
      
      sig_stayed <- dcp_df |> 
        filter(sensor_type == "sigfox") |> 
        get_p_stayed(distance_thr = sigfox_lim) |> 
        mutate(sensor_type = "sigfox")
      
      df_stayed <- df_stayed |> 
        bind_rows(sig_stayed)
      
    }
    
    df_stayed <- df_stayed |> 
      left_join(
        dcp_df |> 
          summarize(n_locs = sum(n_locs), .by = c(yd, sensor_type)),
        by = c("yd", "sensor_type")
      )
    
    # plot heatmap of the probability to say using different thresholds
    psp <- df_stayed |> 
      mutate(
        sensor_type = case_when(
          sensor_type == "sigfox" ~ sig_title,
          sensor_type == "gps" ~ gps_title,
        )
      ) |> 
      plot_tile_timeline(
        y = dist_thr, 
        fill = p_stay, 
        flab = "probability of staying", 
        ylab = "distance treshold [m]", 
        pal = c( "#EF7C12FF", "#DAD8A7FF", "#172869FF")
      ) +
      facet_wrap(~sensor_type, scales = "free_y", ncol = 1) +
      theme(
        plot.margin = margin(t = 0, r = 1, b = 0, l = 0, unit = "cm"), 
        panel.spacing = unit(0, "mm")
      )
    
    # plots counts per day and sensor type
    pcp <- df_stayed |> 
      distinct(yd, n_tracks, n_locs, sensor_type) |> 
      bind_rows(
        expand.grid(
          yd = 1:366, 
          n_tracks = 0, 
          n_locs = 0,
          sensor_type = unique(df_stayed$sensor_type)
        )
      ) |>
      summarize(
        n_tracks = sum(n_tracks), 
        n_locs = sum(n_locs),
        .by = c(yd, sensor_type)
      ) |>
      pivot_longer(
        cols = c(n_tracks, n_locs), 
        names_to = "count_type", 
        values_to = "count"
      ) |>
      mutate(
        title = "number of tracks per day and sensor type + number of locations available", 
        count_type = case_when(
          count_type == "n_tracks" ~ "number of tracks", 
          count_type == "n_locs" ~ "number of points"
        )
      ) |> 
      plot_steps_count(
        counts = count, 
        color = sensor_type, 
        linetype = count_type, 
        pal = c("#65323EFF", "#1BB6AFFF"), 
        legend_position = "bottom", 
        leglab = "sensor", 
        ylab = "number of tracks per day", 
        y_expand = expansion(mult = c(0, 0.2)), 
        alpha = 0.6
      ) +
      facet_wrap(~title) +
      theme(
        plot.margin = margin(t = 0, r = 1, b = 0, l = 0, unit = "cm"), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.spacing = unit(0, "mm"), 
      ) 
    
    # output plot
    pout <- (pcp/psp) + 
      plot_annotation(
        title = paste(
          str_replace(sp, "_", " "), 
          "in Europe - probability of staying at the same night spot (median, when available)"
        ), 
        subtitle = paste("file:", fin_dcp),
        theme = theme(
          plot.title = element_text(hjust = 0.5), 
          plot.subtitle = element_text(hjust = 0.5)
        )
      ) +
      plot_layout(heights = c(1,2), ncol = 1, guides = "collect") & 
      theme(legend.position = "bottom")
    
    # save
    ggsave(
      here("Data", "Graphs", pname), 
      pout, 
      height = 20, width = 30, units = "cm"
    )
    
    
    cat(paste(str_replace(sp, "_", " "), " - DONE!\n"))
    
    
  }) # close map species

