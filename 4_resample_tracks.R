#' ---
#' title: "Resample tracks"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#'  **SECTION 0 - Load packages and files**
#'  
#'  **SECTION 1 - Calculate sampling rate per track**
#'  summarize sampling rate for the cleaned tracks 
#'  
#'  **SECTION 2 - Plot sampling rates per species**
#'  plot the distribution of sampling rates across species using median 
#'  sampling rate per track in hours
#'  
#'  **SECTION 3 - Re-sample tracks**
#'  re-sample the track to a defined rate, in this case 60 minutes
#'  add day_cycle and day_period columns 
#'  output saved in "5_resampled" in the species folder

# 0 - Load packages and files --------------------------------------------------

library(data.table)
library(amt)
library(ggplot2)
library(patchwork)
source("color_palettes.R")
source("0_helper_functions.R")

data_dir <- here::here("Data")
study_dir <- file.path(data_dir, "Studies")
graphs_dir <- file.path(data_dir, "Graphs")

# create output folders
invisible(lapply(
  file.path(list.files(study_dir, full.names = T), "4_resampled"), 
  dir.create, showWarnings = FALSE))

# OUTPUT
file_sampl_rate <- "4_filtered_deployments_sampling_rate.csv"
file_resampled_report <- "4_resampled_report.csv"


# INPUT tracks
files <- list.files(
  file.path(list.files(study_dir, full.names = T), "3_filtered_speed"), 
  full.names = T, pattern = "sen.rds")
ntrk <- length(files)


# 1 - Calculate sampling rate per track ------------------------------------

# calculate the sampling rate per track for all species
if(!file.exists(file.path(data_dir, file_sampl_rate))){
  
  sampl_rate <- rbindlist(lapply(seq_along(files), function(i){
    
    fin <- files[i]
    
    cat(sprintf("\n Getting sampling rate: %d | %d!", i, ntrk))
    
    track_time <- fread(fin)[, timestamp]
    n_locs <- length(track_time)
    
    dt <- data.table(
      timelag = difftime(track_time[-1], track_time[-n_locs], units = "min"))
    
    dt <- dt[, c(
      as.list(unclass(summary(timelag))),
      sd = sd(timelag))]
    dt[, names(dt) := lapply(.SD, round, 2)]
    dt[, ':=' (
      n_locs = n_locs, 
      file = fin, 
      unit = "min")]
    
    return(dt)
    
  }))
  
  sampl_rate <- sampl_rate[, birdlife_name := gsub(
    "_", " ", gsub(".*Studies/(.*?)/3_filtered_speed/.*", "\\1", file))]
  
  setorder(sampl_rate, birdlife_name)
  
  id_cols <- c("birdlife_name", "file")
  setcolorder(sampl_rate, c(id_cols, setdiff(names(sampl_rate), id_cols)))
  
  setnames(
    sampl_rate, old = names(sampl_rate), new = tolower(names(sampl_rate)))
  
  sampl_rate <- add_birdlife_phylogeny(
    sampl_rate, species_name = "birdlife_name")
  
  fwrite(sampl_rate, file.path(data_dir, file_sampl_rate))
  
} 


# PLOT: sampling rates across species --------------------------------------

# OUTPUT FILE
pout <- "4_median_sampling_rate_hist.png"

if(!file.exists(file.path(graphs_dir, pout))){
  
  sampl_rate <- fread(file.path(data_dir, file_sampl_rate))
  
  # because we are only interested in daily sampling frequencies
  sampl_rate <- sampl_rate[, median_hours := median/60][median_hours <= 25]
  sampl_rate[, birdlife_name := factor(
    birdlife_name, levels = sort(unique(birdlife_name), decreasing = T))]
  
  trkcount <- sampl_rate[, .(n_tracks = uniqueN(file)), by = birdlife_name]
  trkcount[, n_tracks := sprintf("[N=%d]", n_tracks)]
  
  ggplot(sampl_rate) +
    geom_boxplot(
      aes(x = median_hours, y = birdlife_name), color = "gray22", fill = "gray80", 
      alpha = 0.8
    ) +
    geom_text(data = trkcount, aes(y = birdlife_name, x = 26, label = n_tracks)) +
    labs(
      x = "median sampling rate per track [h]", 
      y = "species",
      title = "Distrubution of median time lags per track"
    ) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) 
  
  ggsave(file.path(graphs_dir, pout), height = 35, width = 15)
  
}


# 3 - Resample tracks -----------------------------------------------------

# for the data that has high resolution, we first re-sampled the track to, 
# arbitrary decidison of 60 minutes 
resample_rate <- minutes(60)
resample_tolerance <- minutes(15)

if(!file.exists(file.path(data_dir, file_resampled_report))){
  
  # calculate the sampling rate per track for all species
  resampled_tracks <- rbindlist(lapply(seq_along(files), function(i){
    
    fin <- files[i]
    fout <-  gsub("3_filtered_speed", "4_resampled", fin)
    
    track <- fread(fin)
    n_org <- nrow(track)
    
    track <- track |>
      make_track(
        x, y, timestamp,
        crs = sf::st_crs(4326),
        all_cols = TRUE
      )
    
    track <- track |> 
      track_resample(
        rate = resample_rate,
        tolerance = resample_tolerance
      ) |> 
      select(-burst_)
    
    setDT(track)
    setorder(track, t_)
    n_resampled <- nrow(track)
    
    message(sprintf("Processed track: %d | %d!", i, ntrk))
    
    dt_out <- data.table(
      file = fout,
      n_before_resample = n_org,
      n_after_resample = n_resampled)
    
    
    if(n_resampled >=3){ 
      
      fwrite(track, fout)
      
    } else{
      
      fout <- gsub(".rds", "_nodata.rds", fout)
      
      dt_out[, file := fout]
      
      fwrite(dt_out, fout)
    }
    
    return(dt_out)
    
  }), fill = T)
  
  
  resampled_tracks <- resampled_tracks[, `:=` (
    species = gsub("_", " ", gsub(".*/Studies/(.*?)/4_resampled/.*", "\\1", file)), 
    resample_rate_min = as.numeric(resample_rate, units = "mins"),
    resample_tolerance_min = as.numeric(resample_tolerance, units = "mins"))]
  
  
  fwrite(resampled_tracks, file.path(data_dir, file_resampled_report))
  
}




# PLOT: sampling rates for report ----------------------------------------------

#INPUT
fin <- "4_filtered_deployments_sampling_rate.csv"
# OUTPUT FILE
pout <- "4_median_sampling_rate_order.png"

if(!file.exists(file.path(graphs_dir, pout))){
  
  sampl_rate <- fread(file.path(data_dir, fin))
  
  # because we are only interested in daily sampling frequencies
  sampl_rate <- sampl_rate[, median_hours := median/60][
    median_hours <= 25]
  
  n_trks <- nrow(sampl_rate)
  n_sps <- uniqueN(sampl_rate$birdlife_name)
  
  sampl_rate <- add_birdlife_phylogeny(
    sampl_rate, species_name = "birdlife_name")
  
  sampl_rate[, order_txt := sprintf(
    "%s [%d, %d]", order, uniqueN(birdlife_name), uniqueN(file)), 
    by = order]
  sampl_rate[, order_num := as.integer(
    factor(order, levels = sort(unique(order), decreasing = T)))]
  
  sampl_rate[, floor_hours := floor(median_hours)]

  
  sampl_rate_sum <- sampl_rate[, .(
    perc = round(.N/nrow(sampl_rate)*100, 1)), by = floor_hours]
  
  box_dt <- data.table(
    xmin = -5, 
    xmid = -3.5,
    xmax = -1, 
    ymin = -Inf, 
    ymax = Inf, 
    ymid_t = mean(c(0, max(sampl_rate_sum$perc) + 10)), 
    ymid_o = mean(c(0, max(sampl_rate$order_num) + 1)),
    txt_t = "Porportion of total\nwithin 1h-intervals [%]", 
    txt_o = "Distribution across\norders [N species, N tracks]")
  
 po <-  ggplot(sampl_rate) +
    geom_point(
      aes(x = median_hours, y = order_num, color = order),
      alpha = 0.5, position = position_jitter(height = 0.1, width = 0)
    ) +
    geom_boxplot(
      aes(x = median_hours, y = order_num - 0.5, fill = order),
      color = "gray22", outlier.shape = NA, alpha = 0.6
    ) +
   geom_rect(
     data = box_dt,
     aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
     alpha = 0.8, fill = "white", color = "black"
   ) +
   geom_text(
     data = box_dt,
     aes(x = xmid, y = ymid_o, label = txt_o),
     size = 4, fontface = "bold", color = "gray22", angle = 90
   ) +
    scale_color_manual(values = ord_col) +
    scale_fill_manual(values = ord_col) +
    scale_y_continuous(
      breaks = unique(sampl_rate[, order_num])-0.25,
      labels = unique(sampl_rate[, order_txt]), 
      position = "right", 
      limits = c(0, max(sampl_rate$order_num) + 1)
    ) +
   scale_x_continuous(
     breaks = seq(0, 24, by = 2), 
     limits = c(box_dt$xmin, 25), 
     expand = c(0,0)
   ) +
   labs(x = "median sampling rate per track [h]") +
   theme_bw() +
   theme(
     plot.title = element_blank(),
     axis.title.y = element_blank(),
     legend.position = "none", 
     plot.margin = margin(t = 0, r = 5, b = 5, l = 5)
   ) 
   

  pt <- sampl_rate_sum |> 
    ggplot() +
    geom_col(
      aes(x = floor_hours, y = perc),
      alpha = 0.5, fill = "gray88", color = "gray22", width = 0.7
    ) +
    geom_text(
      data = sampl_rate_sum[perc > 2],
      aes(x = floor_hours, y = perc, label = perc),
      vjust = -0.5, size = 3, color = "gray22"
    ) +
    geom_rect(
      data = box_dt,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
      alpha = 0.8, fill = "white", color = "black"
    ) +
    geom_text(
      data = box_dt,
      aes(x = xmid, y = ymid_t, label = txt_t),
      size = 4, fontface = "bold", color = "gray22", 
      angle = 90, vjust = 0.5, hjust = 0.5
    ) +
    scale_x_continuous(
      breaks = seq(0, 24, by = 2), 
      limits = c(box_dt$xmin, 25), 
      expand = c(0,0)
    ) +
    scale_y_continuous(
      limits = c(-5, max(sampl_rate_sum$perc)+10),
      labels = function(x) paste0(x, "%"),
      expand = c(0,0), 
      position = "right"
    ) +
    theme_bw() +
    theme(
      legend.position = "none", 
      axis.title = element_blank(),
      axis.text.x = element_blank(), 
      axis.ticks.x = element_blank(), 
      plot.title = element_blank(), 
      plot.margin = margin(t = 5, r = 5, b = 0, l = 5)
    ) 
  
  (pt/po) + plot_layout(heights = c(1, 2.5)) +
    plot_annotation(
      title = "Distribution of median sampling rates per track",
      subtitle = sprintf(
        "Totaling %d tracks from %d species", n_trks, n_sps)
    ) & 
    theme(
      plot.title = element_text(hjust = 0, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0, size = 12),
      plot.caption = element_text(hjust = 0, size = 10)
    )
  
  ggsave(file.path(graphs_dir, pout), height = 24, width = 18, unit = "cm")
  
}



