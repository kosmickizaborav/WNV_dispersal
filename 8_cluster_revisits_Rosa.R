library(data.table)
library(ggplot2)
library(patchwork)
source("color_palettes.R")

crs <- sf::st_crs(4326)

data_dir <- here::here("Data")
study_dir <- file.path(data_dir, "Studies")
graphs_dir <- file.path(data_dir, "Graphs")


cluster_dirs <- file.path(
  list.files(study_dir, full.names = T), "7_sleep_clusters")
# MAKE OUTPUT DIRECTORIES
revisit_dirs <- gsub("7_sleep_clusters", "8_cluster_revisits", cluster_dirs)
lapply(revisit_dirs, dir.create, showWarnings = F)

plots_dir <- file.path(graphs_dir, "8_cluster_revisits")
dir.create(plots_dir, showWarnings = F)


# parameters --------------------------------------------------------------

# window size
wdw <- 7 # days
min_wdw <- wdw
max_gap <- 0


# 1 - Prepare data -------------------------------------------------------------

fin_name <- "3_dbscan_clusters_npts_5_distthr_350_nauticalDawn_nauticalDusk.rds"
fout_name <- sprintf(
  "1_sleep_clusters_bursts_%d_window_%d_gap.rds", min_wdw, max_gap)

files <- list.files(cluster_dirs, pattern = fin_name, full.names = T)
files_out <- file.path(
  gsub("7_sleep_clusters", "8_cluster_revisits", dirname(files)), fout_name)

files <- files[
  !(file.exists(files_out) | file.exists(gsub(".rds", "_nodata.rds", files_out)))]
nf <- length(files)

if(nf > 0){
  
  lapply(seq_along(files), function(n){
    
    fin <- files[n]
    fout <- file.path(
      gsub("7_sleep_clusters", "8_cluster_revisits", dirname(fin)), fout_name)
    
    dcp_dt <- fread(fin)[, .(file, day_cycle, sleep_cluster)]
    
    # Remove the deployments that are duplicated because of the two sensors, 
    # keep GPS when possible
    to_keep <- data.table(file = unique(basename(dcp_dt$file)))
    to_keep <- to_keep[, ':=' (
      file_id = sub("^(.*dep)_\\d+.*$", "\\1", file), 
      sensor = as.numeric(sub("^.*dep_(\\d+)_sen.*", "\\1", file)))]
    setorder(to_keep, sensor)
    to_keep <- to_keep[, duplicate := duplicated(file_id)]
    to_keep <- to_keep[duplicate == FALSE][, file]
    dcp_dt <- dcp_dt[basename(file) %in% to_keep]
    
    # Order by individual and date
    setorder(dcp_dt, file, day_cycle)
    
    dcp_dt[
      , track_length := max(day_cycle) - min(day_cycle) + 1, by = file]
    
    # check continuity of the tracking data
    dcp_dt[
      , burst_gap := c(0, as.numeric(diff(day_cycle, units = "days"))-1), 
      by = file]
    # group track by bursts (tracking gap lower than max_gap
    dcp_dt[, burst := cumsum(burst_gap > max_gap)+1, by = file]
    dcp_dt[, ':=' (
      burst_start = min(day_cycle), 
      burst_end = max(day_cycle),
      burst_length = max(day_cycle) - min(day_cycle) + 1
    ), by = .(burst, file)]
    
    # select only the bursts that would have enough data for the revisiting analysis
    dcp_dt <- dcp_dt[burst_length >= min_wdw]
    
    if(nrow(dcp_dt) == 0){
      
      fout <- gsub(".rds", "_nodata.rds", fout)
      fwrite(data.table(burst_length = min_wdw), fout)
      
    } 
    
    setorder(dcp_dt, file, day_cycle)
    dcp_dt[, file_id := .GRP, by = file]
    dcp_dt[, burst_id := paste(file_id, burst, sep = "-")]
    dcp_dt[, burst_day := day_cycle - burst_start + 1, by = burst_id]
    dcp_dt[, sleep_cluster_id := paste(file_id, sleep_cluster, sep = "-")]
      
    fwrite(dcp_dt, fout)
    
    cat("DONE:", n, "|", nf, "\n")
    
  })
  
}

rm(fout_name, fin_name, files, files_out)


# 2 - Time windows -------------------------------------------------------------

fin_name <- sprintf(
  "1_sleep_clusters_bursts_%d_window_%d_gap.rds", min_wdw, max_gap)
files <- list.files(revisit_dirs, pattern = fin_name, full.names = T)

files_out <- gsub(".rds", "_all_time_windows.rds", files)


files <- files[!file.exists(files_out)]
nf <- length(files)


if(nf > 0){
  
  lapply(seq_along(files), function(n){
    
    fin <- files[n]
    sp <- gsub(".*/Studies/(.*)_(.*)/8_cluster_revisits/.*$", "\\1 \\2", fin)
    
    dcp_dt <- fread(fin)[
      , .(file, burst_id, burst_start, burst_end, burst_day,
          day_cycle, sleep_cluster)]
    
    mean_burst <- burst_dt[species == sp, mean_burst_length]
    
    max_wdw <- floor(mean_burst/wdw)
    
    dcp_dt[, sample_id := paste(burst_id, ceiling(burst_day/wdw), sep = "-"), 
           by = burst_id]
    dcp_dt[, sample_start := min(day_cycle), by = sample_id]
    
    sample_starts <- unique(
      dcp_dt[, .(file, burst_id, sample_id, sample_start)])
    
    dcp_dt[, sample_start := NULL][, burst_day := NULL]
    
    sample_full <- rbindlist(lapply(seq_len(nrow(sample_starts)), function(z){
      
      fs <- sample_starts$file[z]
      bs <- sample_starts$burst_id[z]
      ss <- sample_starts$sample_start[z]
      sid <- sample_starts$sample_id[z]
      
      sample_dt <- dcp_dt[
        file == fs & burst_id == bs & day_cycle >= ss]
      
      if(nrow(sample_dt) < wdw) { return(NULL) }
      
      sample_dt[, sample_id := sid]
      sample_dt[, sample_day := day_cycle - ss + 1]
      
      sample_wdw <- floor(nrow(sample_dt)/wdw)
      
      if(sample_wdw > max_wdw) { sample_wdw <- max_wdw }
      
      rbindlist(lapply(1:sample_wdw, function(nw){
        
        tw <- nw*wdw
        
        wdw_dt <- sample_dt[sample_day <= tw]
        
        wdw_dt[, sleep_cluster_id := paste(sample_id, sleep_cluster, sep = "-")]
        
        wdw_dt[, ":=" (
          sample_start = min(day_cycle),
          sample_end = max(day_cycle)), 
          by = sample_id]
        
        wdw_dt <- wdw_dt[
          , .(n_visits = .N), 
          by = .(file, burst_id, sample_id, sample_start, sample_end, 
                 sleep_cluster, sleep_cluster_id)]
        
        wdw_dt[, time_window := tw]
        
        
      }))
      
      
    }))
    
    fwrite(sample_full, gsub(".rds", "_all_time_windows.rds", fin))
    
    cat("DONE:", n, "|", nf, "\n")
    
  })
}

# 3 - time of returns -----------------------------------------------------


fin_name <- sprintf(
  "1_sleep_clusters_bursts_%d_window_%d_gap.rds", min_wdw, max_gap)
files <- list.files(revisit_dirs, pattern = fin_name, full.names = T)

files_out <- gsub(".rds", "_return_time.rds", files)


files <- files[!file.exists(files_out)]
nf <- length(files)

if(nf > 0){
  
  
  lapply(seq_along(files), function(n){
    
    fin <- files[n]
    sp <- gsub(".*/Studies/(.*)_(.*)/8_cluster_revisits/.*$", "\\1 \\2", fin)
    
    dt <- fread(fin)[, .(file, burst_id, burst_length, day_cycle, sleep_cluster)]
    dt[, sleep_cluster_id := paste(burst_id, sleep_cluster, sep = "-")]
    
    dt[, ':=' (
      revisit_day_cycle = c(0, diff(day_cycle)),
      visit_num = seq_len(.N)
    ), by = sleep_cluster_id]
    
    dt[, ':=' (
      revisit_day_cycle = fifelse(sleep_cluster == 0, NA, revisit_day_cycle),
      visit_num = fifelse(sleep_cluster == 0, NA, visit_num)
    )]
    
    fwrite(dt, gsub(".rds", "_return_time.rds", fin))
    
  })
  
}



# PLOT: burst length ------------------------------------------------------

fout <- file.path(data_dir, "8_summary_burst_length_min7_per_species.csv")

fin_name <- sprintf(
  "1_sleep_clusters_bursts_%d_window_%d_gap.rds", min_wdw, max_gap)
files <- list.files(revisit_dirs, pattern = fin_name, full.names = T)

burst_dir <- file.path(plots_dir, "3_burst_length")
dir.create(burst_dir, showWarnings = F)

if(!file.exists(fout)){
  
  burst_dt <- rbindlist(lapply(seq_along(files), function(n){
    
    fin <- files[n]
    sp <- gsub(".*/Studies/(.*)_(.*)/8_cluster_revisits/.*$", "\\1 \\2", fin)
    
    dcp_dt <- unique(fread(fin)[, .(file, burst, burst_length)])
    n_files <- uniqueN(dcp_dt$file)
    
    dcp_dt |> 
      ggplot() + 
      geom_histogram(
        aes(x = burst_length), 
        binwidth = 1, fill = "#caefd7", color = "gray33", linewidth = 0.2
      ) +
      scale_x_continuous(limits = c(1, max(dcp_dt$burst_length)+1)) +
      theme_bw() +
      labs(
        x = "burst length", 
        title = sprintf(
          "%s | N files = %d | N bursts = %d", sp, n_files, nrow(dcp_dt))
      )
    
    ggsave(
      file.path(burst_dir, sprintf("%s_burst_length.png", gsub(" ", "_", sp))), 
      width = 6, height = 4
    )
    
    dcp_dt[, species := sp]
    
  }))
  
  burst_dt <- burst_dt[, .(
    median_burst_length = as.numeric(median(burst_length)),
    mean_burst_length = as.numeric(mean(burst_length)),
    n_bursts = .N, 
    n_files = uniqueN(file)), 
    by = species]
  
  fwrite(burst_dt, fout)
  
}




# PLOT: return times ------------------------------------------------------


burst_dt <- fread(
  file.path(data_dir, "8_summary_burst_length_min7_per_species.csv"))

burst_dt <- burst_dt[floor(mean_burst_length/wdw*wdw) >= 35]
burst_dt <- burst_dt[n_files >= 10 | n_bursts >= 10]

selected_sp <- burst_dt[, species]

selected_dirs <- grep(
  paste(gsub(" ", "_", selected_sp), collapse = "|"), revisit_dirs, value = T)

fin_name <- sprintf(
  "1_sleep_clusters_bursts_%d_window_%d_gap_return_time.rds", min_wdw, max_gap)
files <- list.files(selected_dirs, pattern = fin_name, full.names = T)

burst_dir <- file.path(plots_dir, "6_return_time")
dir.create(burst_dir, showWarnings = F)

lapply(seq_along(files), function(n){
  
  fin <- files[n]
  sp <- gsub(".*/Studies/(.*)_(.*)/8_cluster_revisits/.*$", "\\1 \\2", fin)
  
  dt <- fread(fin)
  
  dt <- dt[
    !is.na(revisit_day_cycle) & sleep_cluster > 0 & visit_num > 1]
  
  if(nrow(dt) == 0) { return(NULL) }
  
  n_ind <- uniqueN(dt$file)
  n_bursts <- uniqueN(dt$burst_id)
  
  brst_mean <- burst_dt[species == sp, mean_burst_length]
  brst_median <- burst_dt[species == sp, median_burst_length]
  brst_max <- max(dt$burst_length)
  brst_min <- min(dt$burst_length)
  
  
  mean_rt <- round(mean(dt$revisit_day_cycle), 2)
  variance_rt <- round(var(dt$revisit_day_cycle), 2)
  
  dt |> 
    ggplot() + 
    geom_histogram(
      aes(x = revisit_day_cycle), bins = 100,
      fill = "#317873", color = "gray33", linewidth = 0.2, alpha = 0.5) +
    theme_bw() +
    labs(
      x = "return time (days)", 
      title = sprintf(
        "%s | N individuals = %d | N bursts = %d", sp, n_ind, n_bursts),
      subtitle = sprintf(
        "mean return time = %s days | variance = %s", 
        mean_rt, variance_rt), 
      caption = sprintf(
        "burst duration: range [%d, %d] | mean %1.f | median %1.f days", 
        brst_min, brst_max, brst_mean, brst_median)
    )
  
  pname <- sprintf("%s_return_time.png", gsub(" ", "_", sp))
  
  ggsave(file = file.path(burst_dir, pname), width = 10, height = 8)
  
  cat("DONE:", n, "|", length(files), "\n")
  
})


rosa_dir <- file.path(data_dir, "For_Rosa")
dir.create(rosa_dir, showWarnings = F)




# PLOTS -------------------------------------------------------------------

fin_name <- sprintf(
  "1_sleep_clusters_bursts_%d_window_%d_gap_all_time_windows.rds", min_wdw, max_gap)
files <- list.files(selected_dirs, pattern = fin_name, full.names = T)
nf <- length(files)

burst_dir <- file.path(plots_dir, "4_same_sample_start")
dir.create(burst_dir, showWarnings = F)


lapply(seq_along(files), function(n){
  
  fin <- files[n]
  sp <- gsub(".*/Studies/(.*)_(.*)/8_cluster_revisits/.*$", "\\1 \\2", fin)
  
  dt <- fread(fin)
  
  max_wdw <- max(dt$time_window)
  
  sample_dt <- unique(
    dt[time_window == max_wdw, .(sample_id, sample_start, sample_end, burst_id)])
  
  # Function for one burst
  select_nonoverlapping <- function(dt) {
    setorder(dt, sample_end)
    selected <- integer(0)
    last_end <- -Inf
    for (i in seq_len(nrow(dt))) {
      if (dt$sample_start[i] > last_end) {
        selected <- c(selected, i)
        last_end <- dt$sample_end[i]
      }
    }
    dt[selected]
  }
  
  # Apply per burst
  sample_dt <- sample_dt[, select_nonoverlapping(.SD), by = burst_id]
  
  dt_sample <- dt[sample_id %in% sample_dt$sample_id]
  
  lapply(1:(max_wdw/wdw), function(i){
    
    dt_plot <- dt_sample[time_window == i*wdw]
    dt_plot <- dt_plot[n_visits > 1 & sleep_cluster > 0]
    dt_plot[, med_revisit := sum(n_visits)/uniqueN(sleep_cluster_id)]
    dt_plot[, revisit_rate := n_visits/med_revisit]
    
    n_ind <- uniqueN(dt_plot$file)
    n_sample <- uniqueN(c(dt_plot$file, dt_plot$sample_id))
    n_cls <- uniqueN(dt_plot$sleep_cluster_id)
    mrr <- mean(dt_plot$revisit_rate)
    vrr <- var(dt_plot$revisit_rate)
    
    dt_plot |> 
      ggplot() + 
      geom_histogram(
        aes(x = revisit_rate), bins = 100,
        fill = "#682860", color = "gray33", linewidth = 0.2, alpha = 0.5) +
      theme_bw() +
      labs(
        x = "revisit rate", 
        title = sprintf(
          "%s | max t = %d days | displayed t = %d days", 
          sp, max_wdw, i*wdw
        ), 
        subtitle = sprintf(
          "revisit rate - mean: %.2f | variance: %.3f", mrr, vrr),
        caption = sprintf(
          "N individuals = %d | N samples = %d | N patches = %d", 
          n_ind, n_sample, n_cls)
      )
    
    pname <- sprintf(
      "%s_revisit_rate_max_window_%d_days.png", gsub(" ", "_", sp), i*wdw)
    
    ggsave(file = file.path(burst_dir, pname), width = 10, height = 8)
    
  })
  
  cat("DONE:", n, "|", length(files), "\n")
  
})


burst_dir <- file.path(plots_dir, "5_all_time_windows")
dir.create(burst_dir, showWarnings = F)


lapply(seq_along(files), function(n){
  
  fin <- files[n]
  sp <- gsub(".*/Studies/(.*)_(.*)/8_cluster_revisits/.*$", "\\1 \\2", fin)
  
  dt <- fread(fin)
  
  max_wdw <- max(dt$time_window)
  
  lapply(1:(max_wdw/wdw), function(i){
    
    dt_plot <- dt[time_window == i*wdw]
    dt_plot <- dt_plot[n_visits > 1 & sleep_cluster > 0]
    dt_plot[, med_revisit := sum(n_visits)/uniqueN(sleep_cluster_id)]
    dt_plot[, revisit_rate := n_visits/med_revisit]
    
    n_ind <- uniqueN(dt_plot$file)
    n_sample <- uniqueN(c(dt_plot$file, dt_plot$sample_id))
    n_cls <- uniqueN(dt_plot$sleep_cluster_id)
    mrr <- mean(dt_plot$revisit_rate)
    vrr <- var(dt_plot$revisit_rate)
    
    dt_plot |> 
      ggplot() + 
      geom_histogram(
        aes(x = revisit_rate), bins = 100,
        fill = "#f7d486", color = "gray33", linewidth = 0.2) +
      theme_bw() +
      labs(
        x = "revisit rate", 
        title = sprintf(
          "%s | displayed t = %d days", sp, i*wdw), 
        subtitle = sprintf(
          "revisit rate - mean: %.2f | variance: %.3f", mrr, vrr),
        caption = sprintf(
          "N individuals = %d | N samples = %d | N patches = %d", 
          n_ind, n_sample, n_cls)
      )
    
    pname <- sprintf(
      "%s_revisit_rate_all_windows_%d_days.png", gsub(" ", "_", sp), i*wdw)
    
    ggsave(file = file.path(burst_dir, pname), width = 10, height = 8)
    
    
  })
  
  cat("DONE:", n, "|", nf, "\n")
  
})



# Data summary ------------------------------------------------------------


fin_name <- sprintf(
  "1_sleep_clusters_bursts_%d_window_%d_gap_all_time_windows.rds", min_wdw, max_gap)
files <- list.files(selected_dirs, pattern = fin_name, full.names = T)

rosa_dir <- file.path(data_dir, "For_Rosa")
dir.create(rosa_dir, showWarnings = F)

lapply(seq_along(files), function(n){
  
  fin <- files[n]
  sp <- gsub(".*/Studies/(.*)/8_cluster_revisits/.*$", "\\1", fin)
  
  dt <- fread(fin)[, sleep_cluster_id := NULL]
  dt[, sample_start := as.Date(sample_start)]
  dt[, sample_end := as.Date(sample_end)]
  dt[, file := basename(file)]
  
  setnames(dt, "sleep_cluster", "patch_id")
  
  fout <- paste0(sp, "_totes_mostres_min_7_dies.csv")
  
  fwrite(dt, file.path(rosa_dir, fout))
  
})

