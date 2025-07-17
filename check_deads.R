
library(data.table)
library(amt)
source("0_helper_functions.R")

data_dir <- here::here("Data")

file_dep_filter <- "1_deployments_to_download.csv"
deployments_filtered <- fread(file.path(data_dir, file_dep_filter))

deployments_filtered <- deployments_filtered[grepl("dead", deployment_comments)]

deployments_filtered[
  , file_pattern := make_file_name(study_id, individual_id, deployment_id)]

patts <- unique(gsub(".rds", "", deployments_filtered$file_pattern))


sps <- unique(deployments_filtered$birdlife_name)

files <- list.files(
  file.path(data_dir, "Studies", gsub(" ", "_", sps), "2_cleaned"), full.names = T)

tfiles <- files[grepl(paste(patts, collapse = "|"), files)]
1586527995_stu_1718417073_ind_1718431546_dep.rds
tfiles <- files[grepl("14671003_stu_37112939_ind_37116076_dep", files)]

fin <- tfiles[1]


track <- fread(fin)
crs <- sf::st_crs(4326)

tamt <- track |>
  make_track(
    x, y, timestamp,
    crs = crs,
    all_cols = TRUE
  )

sl <- step_lengths(tamt, lonlat = T, append_last = FALSE)

dt <- track[, sl := c(sl, NA)]
# Assume your data.table is called dt and has x_, y_, t_, sl_ columns
threshold <- 10    # meters
min_duration <- 60*60   # seconds (e.g., 1 hour)

# 1. Flag step lengths below threshold
dt[, small_step := sl < threshold]

# 2. Identify clusters of consecutive small steps
dt[, cluster := rleid(small_step)]
dt[small_step == FALSE, cluster := NA]  # Set cluster to NA for non-small steps

# 3. Summarize clusters to get start and end times, duration, and number of points
clusters <- dt[!is.na(cluster), .(
  start_time = min(timestamp),
  end_time = max(timestamp),
  duration = as.numeric(max(timestamp) - min(timestamp), units="days"),
  n_points = .N
), by = cluster][duration >= min_duration]

# 4. Mark points belonging to "long stationary" clusters
dt[, flag_stationary := cluster %in% clusters$cluster]

# dt$flag_stationary is TRUE for rows in long stationary clusters

sl <- step_lengths(tamt, lonlat = T, append_last = FALSE)

segs <- rle(sl <= 25)

sf_points <- sf::st_as_sf(track, coords = c("x", "y"), crs = crs)

dist <- sf::st_distance(sf_points[-nrow(sf_points),], sf_points[-1,], by_element = TRUE)


dt <- track[, sl := c(sl)][, dist := c(as.numeric(dist), NA)]

library(ggplot2)

sf_points$col <- as.numeric(sf_points$dist) < 10

sf_points |> 
ggplot() + 
  geom_sf(aes( color = col))

flag_defunct_clusters.track_xyt <- function(x, zeta, eta, theta, ...) {
  
  ## Check inputs
  # Check that x has rows (could have already been filtered)
  if (nrow(x) == 0) {
    warning("'x' has 0 rows. Check previous filtering.")
    x$defunct_cluster_ <- logical(0)
    return(x)
  }
  # zeta
  checkmate::assert_numeric(zeta)
  # eta
  checkmate::assert_numeric(eta)
  # theta
  if(!is(theta, "Period")) {
    stop("'theta' should be a 'Period' from package 'lubridate'.")
  }
  
  # Sort x by time
  x <- x[order(x$t_), ]
  
  # Initialize flag column
  x$defunct_cluster_ <- FALSE
  
  # Calculate step lengths
  sl <- step_lengths(x, append_last = FALSE)
  
  # Get sequence of 0 steps using zeta as tolerance
  segs <- rle(sl <= zeta)
  
  if (any((segs$lengths + 1)[segs$values] > eta)) { 
    
    # +1 to include the end point of the step
    
    # start
    start <- cumsum(c(1, head(segs$lengths, -1)))
    end <- cumsum(segs$lengths)
    w <- which(segs$lengths >= eta & segs$values)
    
    # check all segments, starting with the last
    for (i in rev(w)) {
      # Start segs
      s <- start[i]
      # End segs
      e <- end[i] + 1 # Because
      # now get the time difference
      if (as.numeric(difftime(x$t_[e], x$t_[s], units = "secs")) >
          as.numeric(theta, units = "secs")) {
        # flag all locations from `s` to end
        x$defunct_cluster_ <- TRUE
        x$defunct_cluster_[1:s] <- FALSE
        break()
      }
    }
  }
  return(x)
}
