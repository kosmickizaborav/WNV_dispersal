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
max_wdw <- wdw
max_gap <- 0


# 1 - Prepare data -------------------------------------------------------------

fin_name <- "3_dbscan_clusters_npts_5_distthr_350_nauticalDawn_nauticalDusk.rds"
fout_name <- sprintf(
  "1_sleep_clusters_bursts_%d_window_%d_gap.rds", max_wdw, max_gap)

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
    dcp_dt <- dcp_dt[burst_length >= max_wdw]
    
    if(nrow(dcp_dt) == 0){
      
      fout <- gsub(".rds", "_nodata.rds", fout)
      fwrite(data.table(burst_length = max_wdw), fout)
      
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



# PLOT: burst length ------------------------------------------------------


fout <- file.path(data_dir, "8_summary_burst_length_min7_per_species.csv")

fin_name <- sprintf(
  "1_sleep_clusters_bursts_%d_window_%d_gap.rds", max_wdw, max_gap)
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




# 2 - Time windows -------------------------------------------------------------

burst_dt <- fread(
  file.path(data_dir, "8_summary_burst_length_min7_per_species.csv"))

burst_dt <- burst_dt[median_burst_length >= 21][n_files >= 10 | n_bursts >= 10]

selected_sp <- burst_dt[, species]

revisit_dirs <- grep(
  paste(gsub(" ", "_", selected_sp), collapse = "|"), revisit_dirs, value = T)

fin_name <- sprintf(
  "1_sleep_clusters_bursts_%d_window_%d_gap.rds", max_wdw, max_gap)
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
    
    n_wdw <- floor(mean_burst/wdw)
    
    sample_full <- rbindlist(lapply(seq_len(n_wdw), function(nw){
      
      setorder(dcp_dt, file, day_cycle)
      
      tw <- nw*wdw
      
      wdw_dt <- copy(dcp_dt)[
        , sample_id := paste(burst_id, ceiling(burst_day/tw), sep = "-"), 
        by = burst_id]
      wdw_dt[, burst_day := NULL]
      
      wdw_dt[, sample_length := max(day_cycle) - min(day_cycle) + 1, 
             by = sample_id]
      wdw_dt <- wdw_dt[sample_length == tw]
      
      if(nrow(wdw_dt) == 0) return(NULL)
      
      wdw_dt[, sleep_cluster_id := paste(sample_id, sleep_cluster, sep = "-")]
      
      wdw_dt[, ":=" (
        sample_start = min(day_cycle),
        sample_end = max(day_cycle)), 
        by = sample_id]
      
      wdw_dt <- wdw_dt[
        , .(n_visits = .N), 
        by = .(file, burst_id, sample_id, sample_start, sample_end, 
               sleep_cluster, sleep_cluster_id)]
      
      wdw_dt <- wdw_dt[sleep_cluster > 0 & n_visits > 1]
      
      wdw_dt[, time_window := tw]
      
      wdw_dt[, med_revisit := sum(n_visits)/uniqueN(sleep_cluster_id)]
      
      wdw_dt[, revisit_rate := n_visits/med_revisit]
      
    }))
    
    fwrite(sample_full, gsub(".rds", "_all_time_windows.rds", fin))
    
    cat("DONE:", n, "|", nf, "\n")
    
  })
}



# PLOTS -------------------------------------------------------------------

revisit_dirs <- grep(
  paste(gsub(" ", "_", selected_sp), collapse = "|"), revisit_dirs, value = T)

fin_name <- sprintf(
  "1_sleep_clusters_bursts_%d_window_%d_gap_all_time_windows.rds", max_wdw, max_gap)
files <- list.files(revisit_dirs, pattern = fin_name, full.names = T)

burst_dir <- file.path(plots_dir, "4_same_sample_start")
dir.create(burst_dir, showWarnings = F)


lapply(seq_along(files), function(n){
  
  fin <- files[n]
  sp <- gsub(".*/Studies/(.*)_(.*)/8_cluster_revisits/.*$", "\\1 \\2", fin)
  
  dt <- fread(fin)
  
  max_window <- max(dt$time_window)
  
  n_runs <- 4:(max_window/wdw)
  
  lapply(n_runs, function(nw){
    
    wdw_select <- nw*wdw
    
    sample_starts <- unique(
      dt[time_window == wdw_select, .(file, burst_id, sample_start)])
    sample_starts[, selected_sampled := T]
    
    dt_sample <- merge(
      copy(dt), 
      sample_starts, by = c("file", "sample_start", "burst_id"), all.x = T)
    
    dt_sample <- dt_sample[selected_sampled == T]
    
    lapply(1:nw, function(i){
      
      dt_plot <- dt_sample[time_window == i*wdw]
      
      n_ind <- uniqueN(dt_plot$file)
      n_sample <- uniqueN(c(dt_plot$file, dt_plot$sample_id))
      n_cls <- uniqueN(dt_plot$sleep_cluster_id)
      
      dt_plot |> 
        ggplot() + 
        geom_histogram(
          aes(x = revisit_rate), bins = 100,
          fill = "#caefd7", color = "gray33", linewidth = 0.2) +
        theme_bw() +
        labs(
          x = "revisit rate", 
          title = sprintf(
            "%s | max window = %d days | Time window = %d days", 
            sp, nw*wdw, i*wdw
          ), 
          subtitle = sprintf(
            "N individuals = %d | N samples = %d | N clusters = %d", 
            n_ind, n_sample, n_cls)
        )
      
      ggsave(
        file = file.path(
          burst_dir, sprintf(
            "%s_revisit_rate_max_window_%d_window_%d_days_.png", 
            gsub(" ", "_", sp), nw*wdw, i*wdw)
        )
      )
      
    })
    
  })
  
})


