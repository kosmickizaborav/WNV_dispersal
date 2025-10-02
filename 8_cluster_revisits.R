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

# filtered <- fread(file.path(data_dir, 
#   "6_overview_filter_30_max_active_steps_nauticalDawn_nauticalDusk_continent.csv"))
# target_sp <- unique(filtered$species)
# nsp <- length(target_sp)

#deployments <- fread(file.path(data_dir, "1_deployments_to_download.csv"))
# deployments[, file := make_file_name(study_id, individual_id, deployment_id)]
# deployments <- deployments[, .(birdlife_name, file, manipulation_type)]

# plots_dir <- file.path(graphs_dir, "8_cluster_revisits")
# dir.create(plots_dir, showWarnings = F)

target_sp <- gsub("_", " ", list.files(study_dir))

traits_dt <- fread(here::here("Published_data", "00_bird_traits.csv"))[ 
  , .(species = birdlife_name, migration = migration_txt, order, family)]
traits_dt <- unique(traits_dt[species %in% target_sp])

# parameters --------------------------------------------------------------

# window size
wdw <- 7 # days
n_wdw <- 4 # weeks
max_wdw <- wdw*n_wdw
max_gap <- 0

quants <- c(0.25, 0.5, 0.75)


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
    
    dcp_dt <- fread(fin)
    dcp_dt[, revisit_timelag := NULL]
    
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
    
    dcp_dt[, track_length := max(day_cycle) - min(day_cycle) + 1, by = file]
    
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
    ), by = .(file, burst)]
    
    # select only the bursts that would have enough data for the revisiting analysis
    dcp_dt <- dcp_dt[burst_length >= max_wdw]
    
    if(nrow(dcp_dt) == 0){
      
      fout <- gsub(".rds", "_nodata.rds", fout)
      fwrite(data.table(burst_length = max_wdw), fout)
      
    } else{
      
      # get revisiting time within the burst
      dcp_dt[, ':=' (
        revisit_day_cycle = c(0, diff(day_cycle)),
        visit_num = seq_len(.N)
      ), by = .(file, burst, sleep_cluster)]
      
      dcp_dt[, ':=' (
        revisit_day_cycle = fifelse(sleep_cluster == 0, NA, revisit_day_cycle),
        visit_num = fifelse(sleep_cluster == 0, NA, visit_num)
      )]
      
      fwrite(dcp_dt, fout)
      
    }
    
    cat("DONE:", n, "|", nf, "\n")
    
  })
  
}
  
rm(fout_name, fin_name, files, files_out)




# Data summary ------------------------------------------------------------

fin_name <- sprintf(
  "1_sleep_clusters_bursts_%d_window_%d_gap.rds", max_wdw, max_gap)
files <- list.files(revisit_dirs, pattern = fin_name, full.names = T)

stats_dt <- rbindlist(lapply(seq_along(files), function(n){
  
  fin <- files[n]
  dcp_dt <- fread(fin)
  
  revisit_per_patch <- dcp_dt[sleep_cluster > 0][
    , .(n_revisits = sum(revisit_day_cycle > 0, na.rm = T)),
    by = .(file, burst, sleep_cluster)][
    , .(
      median_revisit_per_patch = median(n_revisits), 
      max_revisit_per_patch = max(n_revisits), 
      min_revisit_per_patch = min(n_revisits)
    ), by = .(file, burst)]
  
  
  dcp_track <- dcp_dt[, .(
    n_nights = uniqueN(day_cycle), 
    n_patches = uniqueN(sleep_cluster[sleep_cluster != 0]),
    n_first_visit = sum(sleep_cluster == 0 | revisit_day_cycle == 0),
    n_revisits = as.numeric(
      if(all(is.na(revisit_day_cycle))) 0 else sum(revisit_day_cycle > 0, na.rm = T))
  ), by = .(file, burst)]
  
  
  merge(dcp_track, revisit_per_patch, by = c("file", "burst"), all.x = T)
  
}))

# add species and order
stats_dt[, species := gsub(
  ".*Studies/(.*?)_(.*?)/5_flag_static/.*", "\\1 \\2", file)]

stats_dt <- merge(
  stats_dt, traits_dt[, .(species, order)], by = "species", all.x = T)

stats_dt[, file := basename(file)]

setcolorder(
  stats_dt, 
  neworder = c("order", "species", "file", "burst")
)

fwrite(
  setnames(copy(stats_dt), old = c("burst", "file"), new = c("segment_id", "track_id")), 
  file.path(data_dir, "8_cluster_revisits_segment_summary.csv")
)

stats_dt[, n_order := uniqueN(species), by = order]

stats_dt[, n_nights_log := log10(n_nights)]

stats_dt[, ':=' (
  median_n = as.numeric(median(n_nights)), 
  lower_n = quantile(n_nights, 0.25)-1.5*IQR(n_nights),
  upper_n = quantile(n_nights, 0.75)+1.5*IQR(n_nights)),
  by = species]


setorder(stats_dt, n_order, order, -median_n)
stats_dt[, sp_id := .GRP, by = species]



sample_max <- ceiling(max(stats_dt$n_nights_log))

order_dt <- stats_dt[, .(
  ymax = max(sp_id) + 0.5, 
  ymin = min(sp_id) - 0.5,
  n_order = uniqueN(species), 
  xmin = sample_max, 
  xmax = sample_max*1.3, 
  xmed = sample_max*1.15
), by = order][, ymed := (ymax + ymin)/2, by = order]
order_dt[, lab := ifelse(n_order > 2, order, "")]


total_dt <- stats_dt[, .(
  n_bursts = uniqueN(paste(burst, file)), 
  n_depl = uniqueN(file)), 
  by = .(order, species, sp_id)]
total_dt[, ':=' (
  n_bursts_log = log10(n_bursts)*sample_max*0.1+sample_max*1.31, 
  n_depl_log = log10(n_depl)*sample_max*0.1+sample_max*1.31)]


logs <- 0:floor(max(c(log10(total_dt$n_bursts), log10(total_dt$n_depl))))
logs_x <- logs*sample_max*0.1+sample_max*1.31
logs_lab <- 10^logs

logs_base <- min(floor(stats_dt$n_nights_log)):max(floor(stats_dt$n_nights_log))
logs_base_lab <- 10^logs_base

xmin <- floor(log10(min(stats_dt$n_nights)))
xmax <- ceiling(max(total_dt$n_bursts_log))

sp_max <- max(stats_dt$sp_id) + 0.5

lab_box <- data.table(
  xmin = c(xmin, sample_max, sample_max*1.3),
  xmax = c(sample_max, sample_max*1.3, xmax),
  ymin = sp_max, 
  ymax = sp_max + 10, 
  ymed = sp_max + 5, 
  lab = c(
    "duration of a continuous track segments\n[number of nights per segment]", 
    "bird order", 
    "total number of\ntrack segments (bars) and\ntracks (points)")
)[, xmed := (xmin+xmax)/2]



              
stats_dt |> 
  ggplot() + 
  geom_rect(
    data = order_dt,
    aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax, 
        fill = order),  color = "gray22",
    alpha = 0.3
  ) +
  geom_text(
    data = order_dt, 
    aes(y = ymed, x = xmed, label = lab), 
    vjust = 0.5, hjust = 0.5, 
    color = "gray22"
  ) +
  geom_boxplot(
    aes(y = sp_id, x = n_nights_log, group = sp_id, fill = order), 
    outlier.colour = "gray66", alpha = 0.8, color = "gray33", outliers = T
  ) +
  geom_rect(
    data = total_dt, 
    aes(xmin = sample_max*1.3, xmax = n_bursts_log,
        ymin = sp_id-0.5, ymax = sp_id+0.5, fill = order), alpha = 0.6
  ) +
  geom_vline(
    xintercept = logs_x, linetype = "dashed", color = "gray44"
  ) +
  geom_point(
    data = total_dt, 
    aes(x = n_depl_log, y = sp_id), 
    shape = 21, fill = "gray22", color = "white", size = 2
  ) +
  geom_rect(
    data = lab_box, 
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
    fill = "white", color = "gray22"
  ) +
  geom_text(
    data = lab_box,
    aes(x = xmed, y = ymed, label = lab),
    hjust = 0.5, vjust = 0.5, color = "gray22", size = 3, fontface = "bold"
  ) +
  scale_fill_manual(values = ord_col) +
  scale_y_continuous(
    breaks = total_dt$sp_id, labels = total_dt$species, expand = c(0,0)
  ) +
  scale_x_continuous(
    breaks = c(logs_base, logs_x),
    labels = c(logs_base_lab, logs_lab),
    expand = c(0,0), 
    limits = c(xmin, NA)
  ) +
  labs(
    x = "left: duration of track segment [number of nights]\nvs.\nright: total number of tracks (point) and segments (bars)",
    y = "species",
    title = "Duration of continuous track segments"
  ) +
  theme_bw() +
  theme(
    legend.position = "none", 
    plot.title = element_text(face = "bold", vjust = 0.5)
  )



ggsave(
  filename = file.path(graphs_dir, "8_segment_duration_overview.png"),
  width = 12, height = 15, dpi = 300, bg = "white"
)



# revisits ----------------------------------------------------------------


# REVISITS
total_dt <- stats_dt[, .(
  n_patches = sum(n_patches), 
  n_depl = uniqueN(file), 
  lower_n = quantile(n_patches, 0.25)-1.5*IQR(n_patches),
  upper_n = quantile(n_patches, 0.75)+1.5*IQR(n_patches)), 
  by = .(order, species, sp_id)]


sample_max <- ceiling(max(total_dt$upper_n))

total_dt[, ':=' (
  n_patches_log = log10(n_patches)*sample_max*0.1+sample_max*1.31, 
  n_depl_log = log10(n_depl)*sample_max*0.1+sample_max*1.31)]




order_dt <- stats_dt[, .(
  ymax = max(sp_id) + 0.5, 
  ymin = min(sp_id) - 0.5,
  n_order = uniqueN(species), 
  xmin = sample_max, 
  xmax = sample_max*1.3, 
  xmed = sample_max*1.15
), by = order][, ymed := (ymax + ymin)/2, by = order]
order_dt[, lab := ifelse(n_order > 2, order, "")]





logs <- 0:floor(max(log10(c(total_dt$n_patches, total_dt$n_depl))))
logs_x <- logs*sample_max*0.1+sample_max*1.31
logs_lab <- 10^logs

sp_max <- max(stats_dt$sp_id) + 0.5

lab_box <- data.table(
  xmin = c(0, sample_max, sample_max*1.3),
  xmax = c(
    sample_max, sample_max*1.3, max(total_dt$n_patches_log)),
  ymin = sp_max, 
  ymax = sp_max + 10, 
  ymed = sp_max + 5, 
  lab = c(
    "number of patches per track segment", 
    "bird order", 
    "total number of\npatches (bars) and\ntracks (points)")
)[, xmed := (xmin+xmax)/2]




stats_dt |> 
  ggplot() + 
  geom_rect(
    data = order_dt,
    aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax, 
        fill = order),  color = "gray22",
    alpha = 0.3
  ) +
  geom_text(
    data = order_dt, 
    aes(y = ymed, x = xmed, label = lab), 
    vjust = 0.5, hjust = 0.5, 
    color = "gray22"
  ) +
  geom_boxplot(
    aes(y = sp_id, x = n_patches, group = sp_id, fill = order), 
    outlier.colour = "gray88", alpha = 0.8, color = "gray33", outliers = F
  ) +
  geom_rect(
    data = total_dt, 
    aes(xmin = sample_max*1.3, xmax = n_patches_log,
        ymin = sp_id-0.5, ymax = sp_id+0.5, fill = order), alpha = 0.6
  ) +
  geom_vline(
    xintercept = logs_x, linetype = "dashed", color = "gray44"
  ) +
  geom_point(
    data = total_dt, 
    aes(x = n_depl_log, y = sp_id), 
    shape = 21, fill = "gray22", color = "white", size = 2
  ) +
  geom_rect(
    data = lab_box,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "white", color = "gray22"
  ) +
  geom_text(
    data = lab_box,
    aes(x = xmed, y = ymed, label = lab),
    hjust = 0.5, vjust = 0.5, color = "gray22", size = 3, fontface = "bold"
  ) +
  scale_fill_manual(values = ord_col) +
  scale_color_manual(values = ord_col) +
  scale_y_continuous(
    breaks = total_dt$sp_id, labels = total_dt$species, expand = c(0,0)
  ) +
  scale_x_continuous(
    breaks = c(seq(0, floor(sample_max), 5), logs_x),
    labels = c(seq(0, floor(sample_max), 5), logs_lab),
    expand = c(0,0)
  ) +
  labs(
    x = "left: number of patches per track segment\nvs.\nright: total number of patches (bars) and tracks (points)",
    y = "species",
    title = "Number of patches per track segment"
  ) +
  theme_bw() +
  theme(
    legend.position = "none", 
    plot.title = element_text(face = "bold", vjust = 0.5)
  )



ggsave(
  filename = file.path(graphs_dir, "8_patches.png"),
  width = 12, height = 15, dpi = 300, bg = "white"
)





# trial -------------------------------------------------------------------

stats_dt[, patch_vs_night := n_patches/n_nights]


stats_dt |> 
  ggplot() + 
  geom_boxplot(
    aes(y = sp_id, x = patch_vs_night, group = sp_id, fill = order), 
    outlier.colour = "gray88", alpha = 0.8, color = "gray33", outliers = F
  ) +
  scale_fill_manual(values = ord_col) +
  scale_color_manual(values = ord_col) +
  scale_y_continuous(
    breaks = total_dt$sp_id, labels = total_dt$species, expand = c(0,0)
  ) +
  labs(
    x = "number of patches / number of nights",
    y = "species",
    title = "Proportion [patches/nights] per track segment"
  ) +
  theme_bw() +
  theme(
    legend.position = "none", 
    plot.title = element_text(face = "bold", vjust = 0.5)
  )



ggsave(
  filename = file.path(graphs_dir, "8_patches_vs_night.png"),
  width = 12, height = 15, dpi = 300, bg = "white"
)





stats_dt |> 
  ggplot() + 
  geom_boxplot(
    aes(y = sp_id, x = max_revisit_per_patch, group = sp_id, fill = order), 
    outlier.colour = "gray88", alpha = 0.8, color = "gray33", outliers = F
  ) +
  scale_fill_manual(values = ord_col) +
  scale_color_manual(values = ord_col) +
  scale_y_continuous(
    breaks = total_dt$sp_id, labels = total_dt$species, expand = c(0,0)
  ) +
  labs(
    x = "maximum number of revisits per patch and segment",
    y = "species",
    title = "Maximum number of revisits to a patch"
  ) +
  theme_bw() +
  theme(
    legend.position = "none", 
    plot.title = element_text(face = "bold", vjust = 0.5)
  )



ggsave(
  filename = file.path(graphs_dir, "8_max_revisits.png"),
  width = 12, height = 15, dpi = 300, bg = "white"
)







# PLOT: Time to first revisit---------------------------------------------------

pout_dir <- file.path(plots_dir, "1_time_to_first_revisit")
dir.create(pout_dir, showWarnings = F)

fin_name <- sprintf(
  "1_sleep_clusters_bursts_%d_window_%d_gap.rds", max_wdw, max_gap)
files <- list.files(revisit_dirs, pattern = fin_name, full.names = T)

target_sp <- gsub(
  ".*Studies/(.*?)_(.*?)/8_cluster_revisits.*", "\\1_\\2", files)
files_out <- sprintf(
  "1_%s_longest_burst_first_revisit_%d_window_%d_gap.png", 
  target_sp, max_wdw, max_gap)

sp_to_do <- target_sp[!file.exists(file.path(pout_dir, files_out))]
nsp <- length(sp_to_do)
rm(files_out)

if(nsp > 0){
  
  lapply(seq_along(sp_to_do), function(n){
    
    sp <- target_sp[n]
    
    pname <- sprintf(
      "1_%s_longest_burst_first_revisit_%d_window_%d_gap.png", 
      sp, max_wdw, max_gap)
    
    sp_dir <- file.path(study_dir, sp)
    fin <- file.path(sp_dir, "8_cluster_revisits", fin_name)
    
   
    sp <- gsub("_", " ", sp)
    
    migration_txt <- paste(
      traits_dt[species == sp, migration])
    
    dt_revisits <- fread(fin)
    
    # Find the longest burst per file
    max_burst_per_file <- dt_revisits[
      , .SD[which.max(burst_length)], by = file][, .(file, burst)]
    
    # filter dcp_dt to keep only those bursts
    dt_revisits <- dt_revisits[max_burst_per_file, on = .(file, burst)]
    
    # remove locations that are not within sleep clusters
    # and keep only the first revisit
    dt_revisits <- dt_revisits[sleep_cluster > 0 & visit_num == 2]
    
    if(nrow(dt_revisits) == 0) {
      
      message(sprintf("No revisits found for %s", sp))
      return(NULL)
      
    }
    
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
    
    dt_revisits |> 
      ggplot() +
      geom_histogram(
        aes(x = revisit_day_cycle, y = after_stat(density)), 
        bins = 100, color = "gray44", fill = "skyblue") +
      labs(
        x = "days to first revisit", 
        title = sprintf("%s - migration: %s", sp, migration_txt),
        subtitle = sprintf(
          "total patches [N = %d] %s \ndeployment* duration [N = %d] %s days", 
          n_clst, qclst_txt, n_ind, qbst_txt), 
        caption = sprintf(
          "*the longest continuous segment per deployment (maximum gap of %d days)", 
          max_gap)
      ) +
      theme_bw() 
    

    ggsave(
      filename = file.path(pout_dir, pname),
      width = 10, height = 5, dpi = 300, bg = "white"
    )
    
    cat("DONE:", n, "|", nsp, "\n")
    
  })
  
}


rm(fin_name, files, sp_to_do)


# 2 - Roll window samples --------------------------------------------------


fin_name <- sprintf(
  "1_sleep_clusters_bursts_%d_window_%d_gap.rds", max_wdw, max_gap)
files <- list.files(revisit_dirs, pattern = fin_name, full.names = T)

files_out <- gsub(".rds", "_roll_window.rds", files)

files <- files[!file.exists(files_out)]
nf <- length(files)

n_wdws <- c(2, 3, 4)

if(nf > 0){
  
  lapply(seq_along(files), function(n){
    
    fin <- files[n]
    fout <- gsub(".rds", "_roll_window.rds", fin)
    
    dcp_dt <- fread(fin)
    
    dcp_dt <- dcp_dt[
      , .(file, day_cycle, burst, burst_start, burst_end, burst_length, 
          burst_gap, sleep_cluster)]
    setorder(dcp_dt, file, day_cycle)
    dcp_dt[, burst_day := day_cycle - burst_start + 1, by = .(file, burst)]
    
    dcp_dt[, sample_id := paste(burst, ceiling(burst_day/(max_wdw)), sep = "-"), 
           by = .(file, burst)]
    
    dcp_dt[, sample_lngth := max(day_cycle) - min(day_cycle) + 1, 
           by = .(file, sample_id)]
    
    dcp_dt <- dcp_dt[sample_lngth == max_wdw]
    
    if(nrow(dcp_dt) == 0){
      
      fout <- gsub(".rds", "_nodata.rds", fout)
      fwrite(data.table(sample_size = max_wdw), fout)
      return(NULL)
      
    }
    
    dcp_dt[, sample_start := min(day_cycle), by = .(file, sample_id)]
    dcp_dt[
      , sample_day := day_cycle - sample_start + 1, by = .(file, sample_id)]
    dcp_dt[
      , sleep_cluster_id := fifelse(
        sleep_cluster == 0, 
        paste(.GRP, sleep_cluster, 1:.N, sep = "-"),
        paste(.GRP, sleep_cluster, sep = "-")
      ), by = .(file)
    ]
    
    sample_dt <- rbindlist(lapply(n_wdws, function(n_wdw){
      
      week_dt <- dcp_dt[
        , .SD[sample_day <= (wdw*n_wdw)], by = .(file, sample_id)]
      
      week_dt <- week_dt[, .(
        times_visited = .N, 
        perc_visited = .N/(wdw*n_wdw)
      ), 
      by = .(
        file, burst_start, burst_end, burst_length, 
        sample_start, sample_id, sleep_cluster, sleep_cluster_id)]
      
      week_dt[, time_window := wdw*n_wdw]
      
    }))
    
    fwrite(sample_dt, fout)
    
    cat("DONE:", n, "|", nf, "\n")
    
  })
}



# PLOT: roll window overview ---------------------------------------------------

pname <- sprintf(
  "8_sleep_clusters_bursts_%d_window_%d_gap.png", max_wdw, max_gap)

if(!file.exists(file.path(graphs_dir, pname))){
  
  fin_name <- sprintf(
    "1_sleep_clusters_bursts_%d_window_%d_gap_roll_window.rds", 
    max_wdw, max_gap)
  files <- list.files(revisit_dirs, pattern = fin_name, full.names = T)
  
  sample_dt <- rbindlist(lapply(seq_along(files), function(n){
    
    fin <- files[n]
    sp <- sub(".*Studies/(.*?)_(.*?)/8_cluster_revisits.*", "\\1 \\2", fin)
    
    dcp_dt <- fread(fin)
    
    dcp_dt[, .(n_samples = uniqueN(sample_id)), by = file][, species := sp]
    
  }))
  
  sample_dt[, ':=' (
    median_n = as.numeric(median(n_samples)), 
    lower_n = quantile(n_samples, 0.25)-1.5*IQR(n_samples),
    upper_n = quantile(n_samples, 0.75)+1.5*IQR(n_samples)),
    by = species]
  
  sample_dt <- merge(
    sample_dt, traits_dt[, .(species, order)], by = "species", all.x = T)
  
  sample_dt[, n_order := uniqueN(species), by = order]
  
  sample_max <- ceiling(max(sample_dt$upper_n)/10)*10
  
  setorder(sample_dt, n_order, order, -median_n)
  sample_dt[, sp_id := .GRP, by = species]
  
  order_dt <- sample_dt[, .(
    ymax = max(sp_id) + 0.5, 
    ymin = min(sp_id) - 0.5,
    n_order = uniqueN(species), 
    xmin = sample_max, 
    xmax = sample_max*1.3, 
    xmed = sample_max*1.15
  ), by = order][, ymed := (ymax + ymin)/2, by = order]
  order_dt[, lab := ifelse(n_order > 2, order, "")]
  
  total_dt <- sample_dt[, .(
    sample_tot = sum(n_samples), 
    n_depl = uniqueN(file)), 
    by = .(order, species, sp_id)]
  total_dt[, ':=' (
    sample_tot_log = log10(sample_tot)*sample_max*0.1+sample_max*1.31, 
    n_depl_log = log10(n_depl)*sample_max*0.1+sample_max*1.31)]
  
  logs <- 0:floor(max(c(log10(total_dt$sample_tot), log10(total_dt$n_depl))))
  logs_x <- logs*sample_max*0.1+sample_max*1.31
  log_lab <- 10^logs
  
  sp_max <- max(sample_dt$sp_id) + 0.5
  
  lab_box <- data.table(
    xmin = c(0, sample_max, sample_max*1.3),
    xmax = c(
      sample_max, sample_max*1.3, max(c(total_dt$n_depl_log, total_dt$sample_tot_log))),
    ymin = sp_max, 
    ymax = sp_max + 10, 
    ymed = sp_max + 5, 
    lab = c(
      "number of samples per deployment", 
      "order", 
      "total number of\nsamples (bars) and\ndeployments (points)")
  )[, xmed := (xmin+xmax)/2]
  
  
  sample_dt |> 
    ggplot() + 
    geom_rect(
      data = order_dt,
      aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax, 
          fill = order),  color = "gray22",
      alpha = 0.3
    ) +
    geom_text(
      data = order_dt, 
      aes(y = ymed, x = xmed, label = lab), 
      vjust = 0.5, hjust = 0.5, 
      color = "gray22"
    ) +
    geom_boxplot(
      aes(y = sp_id, x = n_samples, group = sp_id, fill = order), 
      outlier.colour = "gray88", alpha = 0.8, color = "gray33", outliers = F
    ) +
    geom_rect(
      data = total_dt, 
      aes(xmin = sample_max*1.3, xmax = sample_tot_log,
          ymin = sp_id-0.5, ymax = sp_id+0.5, fill = order), alpha = 0.6
    ) +
    geom_vline(
      xintercept = logs_x, linetype = "dashed", color = "gray44"
    ) +
    geom_point(
      data = total_dt, 
      aes(x = n_depl_log, y = sp_id), 
      shape = 21, fill = "gray22", color = "white", size = 2
    ) +
    geom_rect(
      data = lab_box, 
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
      fill = "white", color = "gray22"
    ) +
    geom_text(
      data = lab_box, 
      aes(x = xmed, y = ymed, label = lab), 
      hjust = 0.5, vjust = 0.5, color = "gray22", size = 3, fontface = "bold"
    ) +
    scale_fill_manual(values = ord_col) +
    scale_y_continuous(
      breaks = total_dt$sp_id, labels = total_dt$species, expand = c(0,0)
    ) +
    scale_x_continuous(
      breaks = c(seq(0, sample_max, by = 10), logs_x), 
      labels = c(seq(0, sample_max, by = 10), log_lab), 
      expand = c(0,0)
    ) +
    labs(
      x = "number of samples and deployments per species", 
      y = "species", 
      title = sprintf(
        "Avialability of %d window samples across species", max_wdw)
    ) +
    theme_bw() +
    theme(legend.position = "none")

  ggsave(
    filename = file.path(graphs_dir, pname),
    width = 12, height = 15, dpi = 300, bg = "white"
  )
  
}

# PLOT: patch summary -----------------------------------------------------

n_smpl <- 150

pal_wdw <- c("28" = "#B39DDB", "21" = "#D9B5CC", "14" = "#FFCCBC")

fin_name <- sprintf(
  "1_sleep_clusters_bursts_%d_window_%d_gap_roll_window.rds", 
  max_wdw, max_gap)

files <- list.files(revisit_dirs, pattern = fin_name, full.names = T)

pout_dir <- file.path(plots_dir, "2_samples_patch_summary")
dir.create(pout_dir, showWarnings = F)


lapply(seq_along(files), function(n){
  
  fin <- files[n]
  sp <- sub(".*Studies/(.*?)_(.*?)/8_cluster_revisits.*", "\\1 \\2", fin)
  
  sample_dt <- fread(fin)
  
  sample_ids <- unique(sample_dt[, .(file, sample_id)])
  
  if(nrow(sample_ids) < n_smpl) {return(NULL)}
  
  set.seed(12345)
  sample_ids <- sample_ids[, .SD[sample(.N, n_smpl)]]
  n_depl <- uniqueN(sample_ids$file)

  sample_dt <- sample_dt[sample_ids, on = .(file, sample_id)]
  
  sample_ids <- sample_ids[, .(n_smpl = uniqueN(sample_id)), by = file]
  sample_ids <- sample_ids[, .(
    n_med = as.numeric(median(n_smpl)), 
    n_up = quantile(n_smpl, 0.25), 
    n_low = quantile(n_smpl, 0.75))]
  smpl_per_depl <- sprintf("median [Q1-Q3]: %.1f [%.0f-%.0f]",
    sample_ids$n_med, sample_ids$n_up, sample_ids$n_low)
  
  # making facet titles
  info_dt <- sample_dt[
    , .(
      cls_clustered = uniqueN(sleep_cluster_id[sleep_cluster > 0]),
      cls_unclustered = sum(sleep_cluster == 0)
      ), by = .(file, time_window, sample_id)]
  info_dt <- info_dt[, .(
    clst_per_file_median = as.numeric(median(cls_clustered)), 
    unclust_per_file_median = as.numeric(median(cls_unclustered)),
    n_tlt = sum(cls_clustered), 
    n_unc = sum(cls_unclustered)
    ), by = time_window]
  info_dt[, txt := sprintf(
    "%d days | %d revisited patches (%d visited only once) | median per sample %.1f (%.1f)", 
    time_window, n_tlt, n_unc, clst_per_file_median, unclust_per_file_median)]
  info_dt <- info_dt[, .(time_window, txt)]
  
  # plot titles 
  plt_tlt <- sprintf("%s - %d deployments", sp, n_depl)
  plt_stlt = sprintf(
    "%d random samples across deployments | per deployment - %s", 
    n_smpl, smpl_per_depl) 
  
  sample_dt <- merge(sample_dt, info_dt, by = "time_window")

  
  setorder(sample_dt, time_window, -perc_visited)
  sample_dt[, plotx := 1:.N, by = time_window]
  
  sample_dt |> 
    ggplot() +
    geom_col(
      aes(x = plotx, y = perc_visited, fill = as.factor(time_window)),
    ) +
    facet_wrap(~txt, ncol = 1) +
    theme_bw() +
    labs(
      x = "sleeping patch", 
      y = "frequency of visits\ntimes a patch was visited/time window", 
      title = plt_tlt,
      subtitle = plt_stlt
    ) +
    scale_fill_manual(values = pal_wdw) +
    theme(
      legend.position = "none", 
      axis.text.x = element_blank()
    )
  
  pname <- sprintf(
    "2_%s_%d_random_samples_%d_days_bypatch.png", sp, n_smpl, max_wdw)
  
  ggsave(
    file.path(pout_dir, pname),
    width = 10, height = 15, dpi = 300, bg = "white"
  )
  
  sample_dt <- sample_dt[sleep_cluster > 0]
  
  summary_dt <- sample_dt[, .(
    n_patches = uniqueN(sleep_cluster_id)
  ), by = .(time_window, times_visited)]
  summary_dt[, p_patches := n_patches/sum(n_patches), by = time_window]
  summary_dt <- merge(
    summary_dt, 
    info_dt[, .(time_window, txt)], 
    by = "time_window"
  )
  
  summary_dt |> 
    ggplot() + 
    geom_col(
      aes(x = times_visited, y = p_patches, fill = as.factor(time_window)), 
      width = 1, color = "gray33", linewidth = 0.2
    ) +
    facet_wrap(~txt, ncol = 1) +
    theme_bw() +
    scale_x_continuous(
      breaks = seq(1, max_wdw, by = 1), limits = c(0, max_wdw+1)
    ) +
    theme(legend.position = "none") +
    scale_fill_manual(values = pal_wdw) +
    labs(
      x = "number of visits to a patch", 
      y = "number of patches / total number of patches", 
      title = plt_tlt,
      subtitle = plt_stlt, 
      caption = "locations visited once removed, not included in the patch total"
    )
  
  pname <- sprintf(
    "2_%s_%d_random_samples_%d_days_byday.png", sp, n_smpl, max_wdw)
  
  ggsave(
    file.path(pout_dir, pname),
    width = 10, height = 15, dpi = 300, bg = "white"
  )
  
  
  
  cat("DONE:", n, "|", length(files), "\n")
  
})


