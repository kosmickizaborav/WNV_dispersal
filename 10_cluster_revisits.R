library(data.table)
library(ggplot2)
library(patchwork)

crs <- sf::st_crs(4326)

data_dir <- here::here("Data")
study_dir <- file.path(data_dir, "Studies")
graphs_dir <- file.path(data_dir, "Graphs")

filtered <- fread(file.path(data_dir, 
  "6_overview_filter_30_max_active_steps_nauticalDawn_nauticalDusk_continent.csv"))
target_sp <- unique(filtered$species)
nsp <- length(target_sp)

deployments <- fread(file.path(data_dir, "1_deployments_to_download.csv"))
# deployments[, file := make_file_name(study_id, individual_id, deployment_id)]
# deployments <- deployments[, .(birdlife_name, file, manipulation_type)]

# OUTPUT
cluster_dirs <- file.path(
  study_dir, gsub(" ", "_", target_sp), "10_cluster_revisits")
lapply(cluster_dirs, dir.create, showWarnings = F)

plots_dir <- file.path(graphs_dir, "10_cluster_revisits")
dir.create(plots_dir, showWarnings = F)


traits_dt <- fread(here::here("Published_data", "00_bird_traits.csv"))[ 
  , .(species = birdlife_name, migration = migration_txt)]
traits_dt <- unique(traits_dt[species %in% target_sp])
birdlife <- fread(here::here("Birdlife_migration_status.csv"))[
  species %in% target_sp]
traits_dt <- merge(traits_dt, birdlife, by = "species")
rm(birdlife)

wdw <- 7
n_wdw <- 4
max_gap <- 2

quants <- c(0.25, 0.5, 0.75)


# 1 - Prepare data -------------------------------------------------------------

fin_name <- "3_dbscan_clusters_npts_5_distthr_350_nauticalDawn_nauticalDusk.rds"
fout_name <- "1_sleep_clusters_bursts.rds"

sp_to_do <- target_sp[!file.exists(file.path(cluster_dirs, fout_name))]
nsp <- length(sp_to_do)

if(nsp > 0){
 
  lapply(seq_along(sp_to_do), function(n){
    
    sp <- sp_to_do[n]
    sp_dir <- file.path(study_dir, gsub(" ", "_", sp))
    
    fout <-  file.path(sp_dir, "10_cluster_revisits", fout_name)
    
    dcp_dt <- fread(file.path(sp_dir, "7_sleep_clusters", fin_name))
    dcp_dt[, revisit_timelag := NULL]
    
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
    
    dcp_dt[, track_length := max(day_cycle) - min(day_cycle) + 1, by = file]
    
    # check continuity of the tracking data
    dcp_dt[
      , burst_gap := c(0, as.numeric(diff(day_cycle, units = "days"))-1), 
      by = file]
    # group track by bursts (tracking gap lower than max_gap
    dcp_dt[, burst := cumsum(burst_gap > max_gap), by = file]
    dcp_dt[, ':=' (
      burst_start = min(day_cycle), 
      burst_end = max(day_cycle),
      burst_length = max(day_cycle) - min(day_cycle) + 1
    ), by = .(file, burst)]
    
    # the number of visit 
    dcp_dt[, visit_num := seq_len(.N), by = .(file, sleep_cluster)]
    
    fwrite(dcp_dt, fout)
    
    dcp_dt <- dcp_dt[
      , c("revisit_day_cycle", "tracking_gap", "visit_num") := NULL] 
    
    # select only the bursts that would have enough data for the revisiting analysis
    dcp_dt <- dcp_dt[burst_length >= wdw*n_wdw]
    
    fout <- gsub(".rds", sprintf("_%d_days.rds", wdw*n_wdw), fout)
    
    if(nrow(dcp_dt) == 0){
      
      fout <- gsub(".rds", "_nodata.rds", fout)
      fwrite(data.table(burst_length = wdw*n_wdw), fout)
      
    } else{
      
      # get revisiting time within the burst
      dcp_dt[, ':=' (
        revisit_day_cycle = c(0, diff(day_cycle)),
        visit_num = seq_len(.N)
      ), by = .(file, burst, sleep_cluster)]
      
      fwrite(dcp_dt, fout)
      
    }
    
    cat("DONE:", n, "|", nsp, "\n")
    
  })
  
}
  
rm(fout_name, fin_name)

# PLOT: Time to first revisit---------------------------------------------------

fin_name <- "1_sleep_clusters_bursts.rds"

files_out <- file.path(
  plots_dir, paste0("1_", gsub(" ", "_", target_sp), "_burst_first_revisit.png"))

sp_to_do <- target_sp[!file.exists(files_out)]
nsp <- length(sp_to_do)
rm(files_out)

if(nsp > 0){
  
  lapply(seq_along(sp_to_do), function(n){
    
    sp <- target_sp[n]
    sp_dir <- file.path(study_dir, gsub(" ", "_", sp))
    
    migration_txt <- paste(
      traits_dt[species == sp, migration], "|",
      traits_dt[species == sp, birdlife_migration])
    
    fin <- file.path(sp_dir, "10_cluster_revisits", fin_name)
    
    dt_revisits <- fread(fin)
    
    # remove locations that are not within sleep clusters
    # and keep only the first revisit
    dt_revisits <- dt_revisits[sleep_cluster > 0 & visit_num == 2]
    
    if(nrow(dt_revisits) == 0) {
      
      message(sprintf("No revisits found for %s", sp))
      return(NULL)
      
    }
    
    n_ind <- uniqueN(dt_revisits$file)
    n_clst <- uniqueN(paste(dt_revisits$file, dt_revisits$sleep_cluster))
    
    # get the info on tracking gaps
    qgap <- quantile(dt_revisits$tracking_gap, probs = c(0, 0.5, 1), na.rm = T)
    qgap_txt <- if(n_ind == 1){
      paste(":", qgap[2]) } else{
        sprintf("- median [range]: %.0f [%.0f-%.0f]", qgap[2], qgap[1], qgap[3])
      } 
    
    # get the info on deployment duration
    qdpl <- quantile(dt_revisits$track_length, probs = quants, na.rm = T)
    qdpl_txt <- if(n_ind == 1){
      paste(":", qdpl[2]) } else{
        sprintf("- median [Q1-Q3]: %.0f [%.0f-%.0f]", qdpl[2], qdpl[1], qdpl[3])
      } 
    
    # if a sleep cluster appears in different bursts, save the longest one only
    cluster_dt <- dt_revisits[, .(n_cls = uniqueN(sleep_cluster)), by = file]
    qclst <- quantile(cluster_dt$n_cls, probs = quants, na.rm = T)
    qclst_txt <- if(n_ind == 1){ "" } else{
      sprintf(
        "| per individual - median [Q1-Q3]: %.0f [%.0f-%.0f]", 
        qclst[2], qclst[1], qclst[3])
    } 
    
    p_raw <- dt_revisits |> 
      ggplot() +
      geom_histogram(
        aes(x = revisit_day_cycle, y = after_stat(density)), 
        bins = 100, color = "gray44", fill = "pink") +
      labs(
        x = "days to first revisit", 
        title = sprintf(
          "Total patches [N = %d] %s", n_clst, qclst_txt), 
        subtitle = sprintf(
          "deployment duration [N = %d] %s days \ntracking gap %s days", 
          n_ind, qdpl_txt, qgap_txt)
      ) +
      theme_bw() 
    
    rm(dt_revisits)
    
    fin <- gsub(".rds", sprintf("_%d_days.rds", wdw*n_wdw), fin)
    
    if(!file.exists(fin)){
      
      p_fltr <- NULL
      h <- 4
      message(sprintf("No bursts found for %s", sp))
      
    } else{
      
      dt_revisits <- fread(fin)
      
      # Find the longest burst per file
      max_burst_per_file <- dt_revisits[
        , .SD[which.max(burst_length)], by = file][, .(file, burst)]
      
      # filter dcp_dt to keep only those bursts
      dt_revisits <- dt_revisits[max_burst_per_file, on = .(file, burst)]
      
      dt_revisits <- dt_revisits[sleep_cluster > 0 & visit_num == 2]
      
      n_ind <- uniqueN(dt_revisits$file)
      n_clst <- uniqueN(paste(dt_revisits$file, dt_revisits$sleep_cluster))
      
      # if a sleep cluster appears in different bursts, save the longest one only
      cluster_dt <- dt_revisits[, .(n_cls = uniqueN(sleep_cluster)), by = file]
      qclst <- quantile(
        cluster_dt$n_cls, probs = c(0.25, 0.5, 0.75), na.rm = T)
      qclst_txt <- if(n_ind == 1){ "" } else{
        sprintf(
          "| per individual - median [Q1-Q3]: %.0f [%.0f-%.0f]", 
          qclst[2], qclst[1], qclst[3])
      } 
      
      # get info for bursts lenght
      burst_dt <- dt_revisits[, .(file, burst_length, burst)]
      qbst <- quantile(
        burst_dt$burst_length, probs = c(0.25, 0.5, 0.75), na.rm = T)
      qbst_txt <- if(n_ind == 1){
        paste(":", qbst[2]) } else{
          sprintf("- median [Q1-Q3]: %.0f [%.0f-%.0f]", qbst[2], qbst[1], qbst[3])
        } 
      
      p_fltr <- dt_revisits |> 
        ggplot() +
        geom_histogram(
          aes(x = revisit_day_cycle, y = after_stat(density)), 
          bins = 100, color = "gray44", fill = "skyblue") +
        labs(
          x = "days to first revisit", 
          title = sprintf(
            "Total patches [N = %d] %s", n_clst, qclst_txt), 
          subtitle = sprintf(
            "deployment* duration [N = %d] %s days", n_ind, qbst_txt), 
          caption = sprintf(
            "*the longest continuous segment per deployment (maximum %d days gap)", 
            max_gap)
        ) +
        theme_bw() 
      
      h <- 8
      
    }
    
    pname <- paste0("1_", gsub(" ", "_", sp), "_burst_first_revisit.png")
    
    p_raw/p_fltr +
      plot_annotation(title = sprintf("%s - migration: %s", sp, migration_txt)) &
      theme(plot.title = element_text(face = "bold", hjust = 0.5))
    
    ggsave(
      filename = file.path(plots_dir, pname),
      width = 10, height = h, dpi = 300, bg = "white"
    )
    
    cat("DONE:", n, "|", nsp, "\n")
    
  })
  
}


rm(fin_name)



# 2 - Revisiting times --------------------------------------------------------

fin_name <- sprintf("1_sleep_clusters_bursts_%d_days.rds", wdw*n_wdw)
files <- list.files(cluster_dirs, pattern = fin_name, full.names = T)
files_out <- gsub(".rds", "_rolling_window.rds", files)

files <- files[!file.exists(files_out)]
nf <- length(files)
rm(files_out, fin_name)

n_wdws <- c(2, 3, 4)

if(nf > 0){
  
  
  lapply(seq_along(files), function(n){
    
    fin <- files[n]
    fout <- gsub(".rds", "_rolling_window.rds", fin)
    
    max_wdw <- max(n_wdws)*wdw
    
    dcp_dt <- fread(fin)
    
    dcp_dt <- dcp_dt[
      , .(file, day_cycle, burst, burst_start, burst_end, burst_length, 
          burst_gap, sleep_cluster)]
    setorder(dcp_dt, file, day_cycle)
    dcp_dt[, burst_day := day_cycle - burst_start + 1, by = .(file, burst)]
    
    dcp_dt[, burst_day := day_cycle - burst_start + 1, by = .(file, burst)]
    
    dcp_dt[, sample_id := paste(burst, ceiling(burst_day/max_wdw), sep = "-"), 
           by = .(file, burst)]
    
    dcp_dt <- dcp_dt[
      , .SD[max(day_cycle) - min(day_cycle) + 1 + max_gap >= max_wdw],
      by = .(file, sample_id)]
    
    if(nrow(dcp_dt) == 0){
      
      fout <- gsub(".rds", "_nodata.rds", fout)
      fwrite(data.table(sample_size = max_wdw), fout)
      return(NULL)
      
    }
    
    dcp_dt[, sample_start := min(day_cycle), by = .(file, sample_id)]
    dcp_dt[
      , sample_day := day_cycle - sample_start + 1, by = .(file, sample_id)]
    
    
    sample_df <- rbindlist(lapply(n_wdws, function(n_wdw){
      
      week_df <- dcp_dt[
        , .SD[sample_day <= (wdw*n_wdw)], by = .(file, sample_id)]
      
      week_df <- week_df[
        , .(times_visited = .N), 
        by = .(
          file, burst_start, burst_end, burst_length, sample_id, sleep_cluster)]
      
      week_df[, time_window := wdw*n_wdw]
      
    }))
    
    fwrite(sample_df, fout)
    
    cat("DONE:", n, "|", nf, "\n")
    
  })
  
  
}



# PLOT: Revisiting times -------------------------------------------------------

max_wdw <- max(n_wdws)*wdw

fin_name <- sprintf(
  "1_sleep_clusters_bursts_%d_days_rolling_window.rds", max_wdw)
files <- list.files(cluster_dirs, pattern = fin_name, full.names = T)

#files <- files[!file.exists(files_out)]
nf <- length(files)
rm(files_out, fin_name)


lapply(seq_along(files), function(n){
  
  fin <- files[n]
  sp <- sub(".*Studies/(.*?)_(.*?)/10_cluster_revisits.*", "\\1 \\2", fin)
  
  # migration_txt <- paste(
  #   traits_dt[species == sp, migration], "|",
  #   traits_dt[species == sp, birdlife_migration])
  
  sample_dt <- fread(fin)
  
  if(all(sample_dt$sleep_cluster == 0)){return(NULL)}
  
  # Find the longest burst per file
  max_burst <- sample_dt[
    , .SD[which.max(burst_length)], by = file][
    , .(file, burst_start, burst_end)]
  
  # filter dcp_dt to keep only those bursts
  sample_ids <- sample_dt[max_burst, on = .(file, burst_start, burst_end)][
    , .(file, sample_id)]
  n_individuals <- uniqueN(sample_ids$file)
  
  set.seed(12345)
  sample_ids <- sample_ids[, .SD[sample(.N, 1)], by = file]
  
  sample_dt <- sample_dt[sample_ids, on = .(file, sample_id)][sleep_cluster > 0]
  
  # if a sleep cluster appears in different bursts, save the longest one only
  info_dt <- sample_dt[
    , .(clst_per_file = uniqueN(sleep_cluster)), by = .(file, time_window)]
  info_dt <- info_dt[, .(
    clst_per_file_median = as.numeric(median(clst_per_file)), 
    n_files = uniqueN(file),
    n_tlt = sum(clst_per_file)), 
    by = time_window]
  info_dt[, txt := sprintf(
  "%d days | %d individuals with median number of patches %.1f [N total = %d]", 
  time_window, n_files, clst_per_file_median, n_tlt)]
  
  # for each time_window and times_visited, count number of unique clusters
  summary_dt <- sample_dt[, .(
    n_patches = uniqueN(paste0(basename(file), "-", sleep_cluster))
  ), by = .(time_window, times_visited)]
  summary_dt[, p_patches := n_patches/sum(n_patches), by = time_window]
  summary_dt <- merge(
    summary_dt, 
    info_dt[, .(time_window, txt)], 
    by = "time_window"
  )
  
  if(nrow(summary_dt) == 0){ return(NULL)}
  
  summary_dt |> 
    ggplot() + 
    geom_col(
      aes(x = times_visited, y = p_patches, fill = as.factor(time_window)), 
      width = 1, color = "gray66"
      ) +
    facet_wrap(~txt, ncol = 1) +
    theme_bw() +
    scale_x_continuous(
      breaks = seq(1, max_wdw, by = 1), limits = c(0, max_wdw+1)
    ) +
    theme(legend.position = "none") +
    scale_fill_manual(
      values = c("28" = "#B39DDB", "21" = "#D9B5CC", "14" = "#FFCCBC")
    ) +
    labs(
      x = "number of visits to a patch", 
      y = "number of patches / total number of patches", 
      title = sprintf(
        "%s - one random sample per individual [N total = %d]", sp, n_individuals)
    )
  
  pname <- paste0("2_", gsub(" ", "_", sp), "_sliding_window.png")
 
  ggsave(
    filename = file.path(plots_dir, pname),
    width = 10, height = 4, dpi = 300, bg = "white"
  )
  
})
