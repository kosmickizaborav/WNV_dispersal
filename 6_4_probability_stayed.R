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

# palette for different sensor types
pals <- c("#862633FF", "#3CC8C0FF")
names(pals) <- c("gps", "sigfox")

# palette for probability of staying or leaving
# palp[1] - low values, palp[2] - mid values, palp[3] - high values
palp <- c( "#EF7C12FF", "#DAD8A7FF", "#172869FF")

# titles for facte plots that are more explanatory, not only sigfox/gps
sig_title <- "sigfox data - probability of staying based on different thresholds"
gps_title <- "gps data - probability of staying based on different thresholds"
title_main <- "in Europe - probability of staying at the same night spot"
count_title <- "number of tracks per day and sensor type"

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

# 1 - Night steps - p(stayed) different thresholds----------------------------

# input file
fin_ns <-  "2_all_tracks_night_steps_nauticalDawn_nauticalDusk.rds"
# plot name
pname_ns <- "p_stayed_night_steps_nauticalDawn_nauticalDusk.png"

target_sp |> 
  map(~{
    
    sp <- .x
    sp_dir <- here("Data",  "Studies", str_replace(sp, " ", "_"))
    
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
    
# PLOT 1: p(stayed) heatmap----------------------------------------------------
    
    pname <- str_c("6_4.1_", str_replace(sp, " ", "_"), "_", pname_ns)
    
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
        pal = palp
      ) +
      facet_wrap(~sensor_type, scales = "free_y", ncol = 1) +
      theme(
        plot.margin = margin(t = 0, r = 1, b = 0, l = 0, unit = "cm"), 
        panel.spacing = unit(0, "mm")
      )
    
    # plots counts per day and sensor type
    pcp <- df_stayed |> 
      distinct(yd, n_tracks, sensor_type) |> 
      # in order for count plots to be correct we need to add 0 values for the
      # days that don't exist in the data
      bind_rows(
        expand.grid(
          yd = 1:366, 
          n_tracks = 0, 
          sensor_type = unique(df_stayed$sensor_type)
        )
      ) |>
      summarize(n_tracks = sum(n_tracks), .by = c(yd, sensor_type)) |>
      mutate(title = count_title) |> 
      plot_steps_count(
        counts = n_tracks, 
        color = sensor_type, 
        pal = pals, 
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
        title = paste(sp, title_main), 
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
      
    
    cat(paste(str_replace(sp, "_", " "), "- plot 1 done!\n"))
    
  }) # close map species


# 2 - DCP steps - p(stayed) different thresholds --------------------------
 
fin_dcp <- "1_all_tracks_dcp_night_steps_nauticalDawn_nauticalDusk.rds"
pname_dcp <- "p_stayed_dcp_night_steps_nauticalDawn_nauticalDusk.png"

target_sp |> 
  map(~{
    
    sp <- .x
    sp_dir <- here("Data",  "Studies", str_replace(sp, " ", "_"))
    
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
      # adding sensor type to the data
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


# PLOT 2: p(stayed) heatmap----------------------------------------------------
    
    pname <- str_c("6_4.2_", str_replace(sp, " ", "_"), "_", pname_dcp)
    
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
        pal = palp
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
        title = paste(count_title, "+ number of locations available"), 
        count_type = case_when(
          count_type == "n_tracks" ~ "number of tracks", 
          count_type == "n_locs" ~ "number of points"
        )
      ) |> 
      plot_steps_count(
        counts = count, 
        color = sensor_type, 
        linetype = count_type, 
        pal = pals, 
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
        title = paste(sp, title_main, "(median, when available)"), 
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
    
    cat(paste(str_replace(sp, "_", " "), "- plot 2 done!\n"))
    
  }) # close map species



# 3- DCP steps - p(stayed) vs. turning angle --------------------------------

fin_dcp_night <- "1_all_tracks_dcp_night_steps_nauticalDawn_nauticalDusk.rds"
fin_dcp_max_day <- "4_all_tracks_dcp_max_day_steps_nauticalDawn_nauticalDusk.rds" 

# limits of staying 
sigfox_limit <- 5000
gps_limit <- 30
crs_dcp <- 4326

target_sp |> 
  map(~{
    
    sp <- .x
    sp_dir <- here("Data",  "Studies", str_replace(sp, " ", "_"))
    
    # prepare DCP night stayed 
    
    dcp_night_df <- here(sp_dir, "6_distances", fin_dcp_night) |> 
      read_rds() |> 
      filter(night_steps_available == T) |> 
      select(track_file, day_cycle, sl_, t1_, t2_) 
    
    dcp_night_stayed <- dcp_night_df |> 
      # adding sensor type to the data
      left_join(
        dcp_night_df |> 
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
      ) |> 
      mutate(
        stayed = case_when(
          sensor_type == "gps" &  sl_ <= gps_limit ~ T, 
          sensor_type  == "gps" &  sl_ > gps_limit ~ F,
          sensor_type == "sigfox" & sl_ <= sigfox_limit ~ T, 
          sensor_type == "sigfox" & sl_ > sigfox_limit ~ F, 
          .default = NA
        ), 
        stayed = as.numeric(stayed)
      ) |> 
      select(-sl_) |> 
      rename(t1_night = t1_) 
    
    rm(dcp_night_df)
    
    # prepare DCP night-day turn
    
    dcp_max_day_df <- here(sp_dir, "6_distances", fin_dcp_max_day) |> 
      read_rds() |> 
      select(
        track_file, 
        day_cycle, 
        day_period, 
        step_id, 
        all_of(matches("[xyt][12]_"))
      ) 
    
    dcp_day_turns <- bind_rows(
      # splitting day and night location from the steps
      # night locations 
      dcp_max_day_df |> 
        # correcting the day_period column to night, because in the whole 
        # dataframe it was set to "day"
        mutate(day_period = "night") |> 
        select(-all_of(matches("[xyt]2_"))) |> 
        rename_with(~str_remove(., "1_$"), all_of(matches("[xyt]1_"))) |> 
        filter(!is.na(t)), 
      # day locations
      dcp_max_day_df |> 
        select(-all_of(matches("[xyt]1_"))) |> 
        rename_with(~str_remove(., "2_$"), all_of(matches("[xyt]2_"))) |> 
        filter(!is.na(t))
    ) |> 
      make_track(x, y, t, id = track_file, crs = crs_dcp, all_cols = T) |> 
      # unnecessary since make track already makes sure they are ordered
      # group_by(track_file) |>  arrange(t_, by_group = T) |> ungroup()
      mutate(burst_ = str_remove(str_split_i(step_id, "_", 1), "b")) |> 
      group_split(track_file, burst_) |> 
      map(~{
        # FOR SOME REASON I DO NOT GET THE SANE VALUES FOR TURNING ANGLES
        # USING AMT AND MOVE2 PACKAGE. THEY VARY IN DECIMALS, BUT ALSO IN
        # THE SIGN without a clear rule
        tf <- unique(.x$track_file)
        
        .x |>
          steps_by_burst(longlat = T, keep_cols = "start") |>
          mutate(
            track_file = tf,
            ta_deg = ta_ |> as_degree() |> abs()
          )
        
      }) |>
      list_rbind() |> 
      # we just want the turning angles where the middle point is day
      # so that it is surrounded by two night points, and can represent 
      # returning to the same sleeping area
      filter(day_period == "day", !is.na(ta_deg)) |>
      rename(t1_day = t1_) |> 
      select(-x1_, -y1_, -sl_) 
    
    rm(dcp_max_day_df)
    
    df_ts <- left_join(
      dcp_day_turns, dcp_night_stayed, 
      by = c("track_file", "day_cycle", "t2_")
    ) |> 
      mutate(
        within_eubb = (
          x2_ >= eu_coords$xmin & x2_ <= eu_coords$xmax & 
            y2_ >= eu_coords$ymin & y2_ <= eu_coords$ymax
        ),
        day_cycle = as.Date(day_cycle, format = "%d%m%Y"), 
        yd = yday(day_cycle)
      ) |> 
      mutate(sensor_type = factor(sensor_type, levels = c("gps", "sigfox"))) |> 
      filter(within_eubb == T) 
    
    rm(dcp_day_turns, dcp_night_stayed)
    
    df_ts_med <- df_ts |> 
      summarize(
        step_count = n(),
        ta_deg_med = median(ta_deg, na.rm = T), 
        stayed_med = sum(stayed, na.rm = T)/step_count, 
        .by = c(yd, sensor_type)
      ) |> 
      bind_rows(
        expand_grid(
          step_count = 0, 
          yd = 1:366, 
          sensor_type = unique(df_ts$sensor_type)
        )
      ) |> 
      summarize(
        step_count = sum(step_count),
        across(
          c(ta_deg_med, stayed_med), 
          ~ifelse(all(is.na(.x)), NA, sum(.x, na.rm = T))
        ),
        .by = c(yd, sensor_type)
      ) 
    
    month_limits <- get_month_limits()
    
    # PLOT 3 - turns and stays separate ---------------------------------------
    
    pturn <- df_ts |> 
      ggplot() + 
      geom_vline(
        data = month_limits, aes(xintercept = last_yd), color = "gray33"
      ) +
      geom_boxplot(aes(x = yd, y = ta_deg, group = yd), alpha = 0.7) +
      geom_point(
        data = df_ts_med, 
        aes(x = yd, y = ta_deg_med, color = sensor_type), 
        size = 2, 
        alpha = 0.6
      ) +
      scale_x_continuous(
        breaks = seq(1, 366, 14), limits = c(1, 366), expand = c(0,0)
      ) +
      scale_y_continuous(limits = c(0, 180), breaks = c(0, 180, 20)) +
      scale_color_manual(values = pals) +
      theme_bw() + 
      labs(y = "turning angle", x = "day") +
      theme(
        plot.margin = margin(t = 1, r = 0, b = 0, l = 0, unit = "mm"), 
        panel.spacing = unit(0, "cm"), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        legend.position = "none"
      ) 
    
    pstayed <- df_ts |> 
      ggplot() + 
      geom_vline(
        data = month_limits, aes(xintercept = last_yd), color = "gray33"
      ) +
      geom_boxplot(aes(x = yd, y = stayed, group = yd), alpha = 0.7) +
      geom_point(
        data = df_ts_med, 
        aes(x = yd, y = stayed_med, color = sensor_type), 
        size = 2, 
        alpha = 0.6
      ) +
      scale_x_continuous(
        breaks = seq(1, 366, 14), limits = c(1, 366), expand = c(0,0)
      ) +
      scale_y_continuous(limits = c(0, 1)) +
      scale_color_manual(values = pals) +
      theme_bw() +
      theme(
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "mm"), 
        panel.spacing = unit(0, "cm"), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        legend.position = "none"
      ) 
    
    pcount <- df_ts_med |> 
      plot_steps_count(
        counts = step_count, 
        color = sensor_type, 
        legend_position = "bottom",
        leglab = "sensor",
        ylab = "steps linked with turns per day",
        pal = pals
      ) +
      theme(
        plot.margin = margin(t = 0, r = 0, b = 1, l = 0, unit = "mm"), 
        panel.spacing = unit(0, "cm")
      ) 
    
    pout <- pturn/pstayed/pcount +
      plot_annotation(
        title = paste(sp, title_main, "and related turning angle"), 
        subtitle = str_c(
          "applied thresholds: ", gps_limit, 
          "m for gps, ", sigfox_limit, "m for sigfox"
        )
      )
    
    pout |> 
      ggsave(
        filename = here("Data", "Graphs", str_c("6_4.3_", sp, "_turn_stayed.png")), 
        width = 30, height = 25, units = "cm"
      )
    
    cat(str_c(str_replace(sp, "_", " "), ", plot 3 done!\n"))
    
    
# PLOT 4: turn and stays boxplot related----------------------------------------
    
    pbox <- df_ts |> 
      mutate(
        stayed = case_when(
          stayed == 1 & sensor_type == "gps" ~ "yes, gps data",
          stayed == 0 & sensor_type == "gps" ~ "no, gps data", 
          stayed == 1 & sensor_type == "sigfox" ~ "yes, sigfox data",
          stayed == 0 & sensor_type == "sigfox" ~ "no, sigfox data"
        ) |> 
          factor(
            levels = c(
              "yes, gps data", 
              "no, gps data", 
              "yes, sigfox data", 
              "no, sigfox data"
            )
          )
      ) |> 
      ggplot() +
      geom_boxplot(
        aes(x = ta_deg, y = stayed, group = stayed, fill = sensor_type), 
        alpha = 0.8
      ) +
      scale_x_continuous(limits = c(0, 180)) + 
      scale_fill_manual(values = pals) +
      labs(
        x = "turning angle", 
        y = "used same night spot?", 
        title = paste(sp, "in Europe - distribution of turning angles and staying"), 
        #subtitle = "distribution of turning angles and staying", 
        legend = "sensor"
      ) +
      theme_bw() + 
      theme(legend.position = "none")
    
    pbox |> 
      ggsave(
        filename = here(
          "Data", "Graphs", str_c("6_4.4_", sp, "_turn_stayed_boxplot.png")
        ), 
        width = 20, height = 15, units = "cm"
      )
    
    
    cat(str_c(str_replace(sp, "_", " "), ", plot 4 done!\n"))
    
  })

