#' ---
#' title: "Download Movebank data"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' **SECTION 0 - Define parameters and packages**
#' 
#' **FUNCTION: flag_static**
#' calculate the step lengths within the track, check which are lower than the 
#' distance threshold (here 25m) and check for how long the distances stayed
#' like that. if the distances stayed lower than distance threshold longer than 
#' the period defined (here 5 days) label those spots as belonging to a static
#' cluster. 
#' 
#' **SECTION 1 - Flag static clusters**
#' Label the static clusters within the track
#' 
#' **SECTION 2 - Make a static cluter report**
#' 

# 0 - Define parameters and packages--------------------------------------------
library(data.table)

data_dir <- here::here("Data")
study_dir <- file.path(data_dir, "Studies")

# OUTPUT DIRECTORIES
invisible(lapply(
  file.path(list.files(study_dir, full.names = T), "5_flag_static"),
  dir.create, showWarnings = FALSE
))


# FUNCTION: flag_static ---------------------------------------------------

flag_static <- function(
    track, coords = c("x_", "y_"), time_col = "t_", 
    crs = sf::st_crs(4326), units = "days", 
    dist_thr = 25, min_duration = 5){
  
  in_cols <- names(copy(track))
  
  track <- track[
    , geometry := sf::st_as_sf(.SD, coords = coords, crs = crs),
    .SDcols = coords]
  
  track <- track[
    , sl := c(NA, sf::st_distance(geometry[-.N], geometry[-1], by_element = T)),
  ]
  
  # flag step lengths below threshold
  track[, small_step := sl < dist_thr]
  
  # identify clusters of consecutive small steps
  track[, cluster := rleid(small_step)]
  # if not in a cluster, set it to NA
  track[small_step == FALSE | is.na(small_step), cluster := NA]
  track[, cluster := ifelse(.N == 1, NA, cluster), by = cluster]
  
  # find problematic clusters
  clusters <- track[!is.na(cluster), .(
    cluster_duration = as.numeric(
      max(get(time_col)) - min(get(time_col)), units=units)
  ), by = cluster]
  
  # label only clusters that are longer than the minimum length
  clusters <- clusters[cluster_duration >= min_duration]
  
  # mark points belonging to static_clusters
  track[, flag_static := cluster %in% clusters$cluster]
  
  setnames(track, "cluster", "static_cluster_id")
  
  out_cols <- c(in_cols, "flag_static", "static_cluster_id")
  
  track <- track[, ..out_cols]
  
  return(track)
  
}


# 1 - Flag static clusters -----------------------------------------------

files <- list.files(
  file.path(list.files(study_dir, full.names = T), "4_resampled"), 
  full.names = T, pattern = "sen.rds")

# check if already exists
files <- files[!file.exists(gsub("4_resampled", "5_flag_static", files))]
nf <- length(files)

if(nf > 0){
  
  lapply(seq_along(files), function(i){
    
    fin <- files[i]
    fout <- gsub("4_resampled", "5_flag_static", fin)
    
    track <- fread(fin)
    
    setorder(track, t_)
    
    track <- flag_static(track)
    
    fwrite(track, fout)
    
    cat("\n Static cluster labelled:", i, "|", nf)
    
    return(invisible(NULL))
    
  })
  
}

rm(files, nf)

# 2 - Static report -------------------------------------------------------

# OUTPUT FILE
f_report <- "5_static_cluster_report.csv"

# INPUT FILES
files <- list.files(
  file.path(list.files(study_dir, full.names = T), "5_flag_static"), 
  full.names = T, pattern = "sen.rds")
nf <- length(files)

cluster_report <- rbindlist(lapply(seq_along(files), function(i){
  
  fin <- files[i]
  
  track <- fread(fin)[!is.na(static_cluster_id)]
  
  dt_out <- track[, .(
    n_locs = .N, 
    cluster_start = min(t_),
    cluster_end = max(t_), 
    static = unique(flag_static)), 
    by = static_cluster_id]
  
  dt_out[
    , duration_days := as.numeric(cluster_end - cluster_start, units = "days")]
  
  dt_out[, file := fin]
  
  cat("\n Cluster summary:", i, "|", nf)
  
  return(dt_out)
  
}), fill = T)


cluster_report[
  , species := gsub(
    "_", " ", gsub(".*/Studies/(.*?)/5_flag_static/.*", "\\1", file))] 
 
fwrite(cluster_report, file.path(data_dir, f_report))


