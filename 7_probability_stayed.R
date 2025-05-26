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
library(amt)
library(data.table)
source("6_plot_datatable_FUNCTIONS.R")

data_dir <- here::here("Data")

nfile <- "2_all_tracks_dcp_night_steps_nauticalDawn_nauticalDusk_continent.rds"

dist_dirs <- grep(
  "5_distances$", list.dirs(file.path(data_dir, "Studies")), value = T)

files <- unlist(lapply(dist_dirs, function(dir){
  grep(nfile, list.files(dir, full.names = T), value = T)
}))
n_files <- length(files)


# palette for different sensor types
pals <- c(GPS = "#862633FF", SigFox= "#3CC8C0FF")
sensor_labeller <- c(
  GPS = "probability of staying - GPS data", 
  SigFox = "probability of staying - SigFox data"
)

# palette for probability of staying or leaving
# palp[1] - low values, palp[2] - mid values, palp[3] - high values
palp <- c( "#EF7C12FF", "#DAD8A7FF", "#172869FF")

graph_dir <- file.path(data_dir, "Graphs", "7_probability_stayed")
if(!dir.exists(graph_dir)){ dir.create(graph_dir) }


# PLOT 1
p1_name <- "1_%s_probability_stayed_nauticalDawn_nauticalDusk.png"
# PLOT 2
p2_name <- "2_%s_probability_stayed_vs_turn_nauticalDawn_nauticalDusk.png"
# PLOT 3
p3_name <- "3_%s_probability_stayed_vs_turn_nauticalDawn_nauticalDusk_boxplot.png"


# FUNCTION: check_if_stayed ------------------------------------------------

check_if_stayed <- function(
    dt, dist_list, dist_col = "sl_", sensor_col = "sensor_name"){
  
  sensor_names <- names(dist_list)
  
  stayed_dt <- rbindlist(lapply(sensor_names, function(s){
    
    dist_lim <- dist_list[[s]]
    
    sensor_dt_dist <- dt[get(sensor_col) == s]
    
    rbindlist(lapply(dist_lim, function(d){
        
        dt_lim <- copy(sensor_dt_dist)
        dt_lim[
          , stayed := get(dist_col) <= d][
          , dist_thr := d]
        
        return(dt_lim)
        
      }))[, sensor_name := s]
    
  }))
  
  return(stayed_dt)
  
}


# FUNCTION: get_p_stayed --------------------------------------------------


get_p_stayed <- function(dt) {
  
  pst_dt <- dt[
    , .(p_stayed_id = sum(stayed) / .N), 
    by = .(individual_local_identifier, sensor_name, dist_thr, yd)][
    , .(
      p_stayed_med = median(p_stayed_id),  
      p_stayed_mean = mean(p_stayed_id),
      n_ind = uniqueN(individual_local_identifier)
    ), by = .(yd, dist_thr, sensor_name)]
  
  ppo_dt <- dt[
    , .(
      p_stayed_pooled = sum(stayed) / .N, 
      n_steps = .N
    ), by = .(yd, dist_thr, sensor_name)]
  
  pout_dt <- merge(
    pst_dt, ppo_dt, 
    by = c("yd", "dist_thr", "sensor_name"), all = T)
  
  return(pout_dt)
  
}


# FUNCTION: prepare_night_steps -------------------------------------------

deploys <- fread(file.path(data_dir, "2_deployments_cleaned.csv"))[
  , .(individual_local_identifier, file)][
    , file := gsub("2_cleaned", "4_resampled", file)]

prepare_night_steps <- function(
    night_file, deploy_info = deploys, 
    cols_out = c("yd", "sensor_name", "file", "individual_local_identifier", "sl_")
    ){
  
  # sensor info 
  sensors <- c(GPS = 653, SigFox = 2299894820)
  
  night_steps <- fread(night_file)[continent == "Europe"]
  
  if(nrow(night_steps) > 0){
    
    night_steps <- deploy_info[night_steps, on = "file"][
      , sensor_id := as.numeric(sub(".*dep_(\\d+)_sen.*", "\\1", file))][
      , sensor_name := as.character(factor(
        sensor_id, levels = sensors, labels = names(sensors)))][
      , yd := as.POSIXlt(as.Date(day_cycle_1))$yday+1]
    
    if (is.null(cols_out)){ return(night_steps) 
      } else{ return(night_steps[, ..cols_out]) } 
    
  } else{
    
    return(NULL)
    
  }
  
}


# FUNCTION: prepare_count_plot --------------------------------------------

prepare_count_plot <- function(p_stayed_dt){
  
  counts_df <- melt(
    unique(p_stayed_dt[, .(yd, n_ind, n_steps, sensor_name)]), 
    id.vars = c("yd", "sensor_name"), 
    measure.vars = c("n_ind", "n_steps"), 
    variable.name = "count_id", 
    value.name = "count")[
      , count_id := fcase(
        count_id == "n_ind", "individuals", 
        count_id == "n_steps", "steps")]
  
  
  counts_df <- rbindlist(list(
    CJ(yd = 1:366, count = 0, count_id = unique(counts_df$count_id),
       sensor_name = unique(counts_df$sensor_name)),  
    counts_df), 
    fill = T)[
      , .(count = sum(count)), by = c("yd", "sensor_name", "count_id")]
  
  # plots counts per day and sensor type
  pcp <- plot_steps_count(
    steps = counts_df, count = count, color = sensor_name, linetype = count_id, 
    pal = pals, 
    legend_position = "bottom", 
    collab = "sensor", 
    linelab = "count",
    ylab = "count \n per day"
  )+
    labs(color = "sensor") +
    theme(
      plot.margin = margin(t = 0, r = 1, b = 0, l = 0, unit = "cm"),
      panel.spacing = unit(0, "mm")
    )
  
}


# 1 - PLOT1: p(stayed) distance threshold --------------------------------------


# distance limits applied as gps/sigfox error, in meters
sensor_limits <- list(
  GPS = seq(5, 50, 5), 
  SigFox = c(1:10)*1000
)

lapply(seq_along(files), function(i){
  
  fin <- files[i]
  
  sp <- gsub(".*/Studies/([^/]+)/5_distances/.*", "\\1", fin)
  
  night_steps <- prepare_night_steps(night_file = fin, cols_out = NULL)
  
  if(!is.null(night_steps)){
    
    p_stayed_dt <- get_p_stayed(
      check_if_stayed(dt = night_steps, dist_list = sensor_limits))
    
    # plot heatmap of the probability to say using different thresholds
    
    pt <- p_stayed_dt |> 
      plot_tile_timeline(
        y = dist_thr, fill = p_stayed_med, 
        ylab = "distance \n treshold [m]", 
        flab = "p"
      ) +
      facet_wrap(
        ~sensor_name, scales = "free", ncol = 1, 
        labeller = labeller(sensor_name = sensor_labeller)
      ) +
      theme(
        plot.margin = margin(t = 0, r = 1, b = 0, l = 0, unit = "mm"), 
        panel.spacing = unit(0, "mm"), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank()
      ) 
    
    pcp <- prepare_count_plot(p_stayed_dt)
    
    # output plot
    pout <- (pt/pcp) + 
      plot_annotation(
        title = paste(gsub("_", " ", sp), "in Europe"),
        subtitle = "probability of staying at the same spot between consequtive nights,
        calculated with different distance thresholds",
        theme = theme(
          plot.title = element_text(hjust = 0.5), 
          plot.subtitle = element_text(hjust = 0.5)
        )
      ) +
      plot_layout(heights = c(2,1), ncol = 1, guides = "collect") & 
      theme(legend.position = "bottom") 
    
    # save
    ggsave(
      file.path(graph_dir, sprintf(p1_name, sp)), pout, 
      height = 18, width = 18, units = "cm"
    )
    
  }
  
  cat(sprintf("\nSEGMENT 1: %s - done %d|%d!", sp, i, n_files))

})

# CATCH WARNINGS 
# withCallingHandlers({
#   ---CODE TO EXECUTE---
# }, warning = function(w){
#   message("Warning caught: ", conditionMessage(w))
#   invokeRestart("muffleWarning")
# })

# 2 - Probability stayed vs. turning angles -----------------------------------

dfile <- "4_all_tracks_dcp_max_day_steps_nauticalDawn_nauticalDusk_continent.rds"

sensor_limit_one <- list(SigFox = 500, GPS = 25)


lapply(seq_along(files), function(i){
  
  fin <- files[i]
  
  sp <- gsub(".*/Studies/([^/]+)/5_distances/.*", "\\1", fin)
  
  night_steps <- prepare_night_steps(night_file = fin, cols_out = NULL)
  
  if(!is.null(night_steps)){
    
    night_stayed <- check_if_stayed(
      dt = night_steps, dist_list = sensor_limit_one)[
        , .(
          file, individual_local_identifier, sensor_name,
          t1_night = t1_, t2_, stayed, yd, dist_thr)]
    
    rm(night_steps)
    
    fin_d <- gsub(nfile, dfile, fin)
    
    if(file.exists(fin_d)){
      
      day_steps <- fread(fin_d)
      
      day_tracks <- unique(rbindlist(list(
        day_steps[
          , .(t_ = t1_, x_ = x1_, y_ = y1_, day_cycle = day_cycle_1, file)], 
        day_steps[
          , .(t_ = t2_, x_ = x2_, y_ = y2_, day_cycle = day_cycle_2, file)]
      )))[order(t_)][
        , burst_ := cumsum(c(1, diff(as.numeric(day_cycle)) > 1)), by = file]
      
      day_tracks <- split(day_tracks, by = "file")
      
      rm(day_steps)
      
      day_turns <- rbindlist(lapply(day_tracks, function(track){
        
        # id is the file name, which should be unique per split
        trk <- make_track(
          track, x_, y_, t_, 
          id = unique(track$file), 
          crs = sf::st_crs(4326), 
          all_cols = TRUE
        )
        
        steps <- steps_by_burst(trk, lonlat = TRUE, keep_cols = "start") |> 
          mutate(ta_deg = abs(as_degree(ta_)))
        
        return(as.data.table(steps))
        
      }))
      
      day_turns <- day_turns[, .(t1_day = t1_, t2_, file, ta_deg, sl_)]
      
      turn_stayed <- day_turns[night_stayed, on = .(file, t2_), nomatch = NA][
        , .(t1_night, t1_day, t2_, file, yd, ta_deg, stayed, sensor_name, 
            individual_local_identifier, dist_thr)][!is.na(ta_deg)]
      
      ps_file <- gsub("/4_", "/7_prob_stayed_", fin_d)
      fwrite(turn_stayed, ps_file)
      
      rm(day_turns, night_stayed)
      
     
      month_limits <- get_month_limits()
      label_dt <- data.table(
        id = c("turn", "p_stayed", "count"), 
        t = c(
          "median turning angle per individual", 
          "probability of staying summarized per day",
          "number of indiviudals/steps per day"
        ), 
        y = 0,
        x = 366
      )
      
      # PLOT turns ####
    
      turn_ind_med <- turn_stayed[
        , .(ta_deg_med = median(ta_deg)), 
        by = .(yd, individual_local_identifier, sensor_name)][
          , .(ta_deg_med = median(ta_deg_med)), by = .(yd, sensor_name)] 
      
      pturn <- turn_stayed |> 
        ggplot() + 
        geom_boxplot(aes(x = yd, y = ta_deg, group = yd), alpha = 0.7) +
        geom_vline(
          data = month_limits, aes(xintercept = last_yd), color = "gray33"
        ) +
        geom_point(
          data = turn_ind_med, 
          aes(x = yd, y = ta_deg_med, color = sensor_name), 
          size = 3, 
          alpha = 0.8
        ) +
        geom_label(
          data = label_dt[id == "turn",], 
          aes(x = x, y = y, label = t), hjust = 1, alpha = 0.7, label.size = 0
        ) +
        scale_x_continuous(
          breaks = seq(1, 366, 30), limits = c(1, 366), expand = c(0,0)
        ) +
        scale_y_continuous(limits = c(0, 180), breaks = seq(0, 180, 20)) +
        scale_color_manual(values = pals) +
        theme_bw() +
        labs(y = "turning\nangle", x = "day", color = "sensor") +
        theme(
          plot.margin = margin(t = 1, r = 0, b = 0, l = 0, unit = "mm"), 
          panel.spacing = unit(0, "cm"), 
          axis.text.x = element_blank(), 
          axis.title.x = element_blank(), 
          axis.ticks.x = element_blank(), 
          legend.position = "inside",
          legend.justification.inside =  c("left", "top")) 
      
      #rm(turn_ind_med)
      
      # PLOT count ####
      
      turn_p_stayed <- get_p_stayed(turn_stayed)
      
      pcount <- prepare_count_plot(turn_p_stayed) +
        geom_label(
          data = label_dt[id == "count",], 
          aes(x = x, y = y, label = t), hjust = 1, vjust = 0, 
          alpha = 0.7, label.size = 0
        ) +
        theme(
          legend.position = "inside",
          legend.justification.inside =  c("left", "top")
        )
      
      # PLOT stayed####
      
      turn_p_stayed <- melt(
        turn_p_stayed, 
        measure.vars = c("p_stayed_med", "p_stayed_mean", "p_stayed_pooled"),
        variable.name = "p_type",
        value.name = "p_value"
        )[
        , p_type := fcase(
          p_type == "p_stayed_med", "median",
          p_type == "p_stayed_mean", "mean",
          p_type == "p_stayed_pooled", "pooled")]
      
      pstayed <- turn_p_stayed |> 
        ggplot() + 
        geom_vline(
          data = month_limits, aes(xintercept = last_yd), color = "gray33"
        ) +
        geom_point(
          aes(x = yd, y = p_value, color = sensor_name, shape = p_type), 
          size = 3, 
          alpha = 0.6
        ) +
        geom_label(
          data = label_dt[id == "p_stayed",], 
          aes(x = x, y = y, label = t), hjust = 1, vjust = 0, 
          alpha = 0.7, label.size = 0
        ) +
        labs(
          y = "probability\nof staying",
          shape = "probability type", 
          color = "sensor"
        ) +
        scale_x_continuous(limits = c(1, 366), expand = c(0,0)) +
        scale_y_continuous(limits = c(0, 1)) +
        scale_color_manual(values = pals) +
        theme_bw() +
        theme(
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "mm"), 
          panel.spacing = unit(0, "cm"), 
          axis.text.x = element_blank(), 
          axis.title.x = element_blank(), 
          axis.ticks.x = element_blank(),
          legend.position = "inside",
          legend.justification.inside =  c("left", "top")
        ) 
      
      #rm(turn_p_stayed)
      
      # PLOT joined####
    
      pout <- pturn/pstayed/pcount +
        plot_annotation(
          title = paste(
            gsub("_", " ", sp), "in Europe", 
            "\nprobability of staying and median turning angles per individual"
          ),
          subtitle = "points represent median values per day",
          caption = sprintf(
            "applied thresholds for probability of staying: %dm [GPS], %dm [SigFox]",
            sensor_limit_one$GPS, sensor_limit_one$SigFox), 
          theme = theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0.5)
          )
        )
      
      ggsave(
        file.path(graph_dir, sprintf(p2_name, sp)), pout, 
        height = 20, width = 18, units = "cm"
      )
      
      # BOXPLOT 
      turn_stayed[
        , stayed_text := factor(fcase(
          stayed == 1 & sensor_name == "GPS", "yes [GPS]",
          stayed == 0 & sensor_name == "GPS", "no [GPS]", 
          stayed == 1 & sensor_name == "SigFox", "yes [SigFox]",
          stayed == 0 & sensor_name == "SigFox", "no [SigFox]"), 
          level = c("yes [GPS]", "no [GPS]", "yes [SigFox]",  "no [SigFox]")
        )]
      
      pbox <- turn_stayed |> 
        ggplot() +
        geom_boxplot(
          aes(x = ta_deg, y = stayed_text, group = stayed_text, fill = sensor_name), 
          alpha = 0.8
        ) +
        scale_x_continuous(limits = c(0, 180)) + 
        scale_fill_manual(values = pals) +
        labs(
          x = "turning angle", 
          y = "used same night spot?", 
          title = paste(gsub("_", " ", sp), "in Europe"), 
          subtitle = "distribution of turning angles and staying", 
          legend = "sensor"
        ) +
        theme_bw() + 
        theme(legend.position = "none")

      ggsave(
        file.path(graph_dir, sprintf(p3_name, sp)), pbox, 
        height = 15, width = 15, units = "cm"
      )
      
    }
    
  }
  
  cat(sprintf("\nSEGMENT 2: %s - done %d|%d!", sp, i, n_files))
  
})
