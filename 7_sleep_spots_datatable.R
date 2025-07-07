#' ---
#' title: "Find sleeping spots"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

# 0 - Load packages -------------------------------------------------------

library(data.table)
library(sf)
library(dbscan)
library(ggplot2)
source("5_distance_datatable_FUNCTIONS.R")
source("0_helper_functions.R")
# get the species of interest
data_dir <- here::here("Data")

crs <- st_crs(4326)

# direcyories with median distances
dist_dirs <- grep(
  "5_distances$", list.dirs(file.path(data_dir, "Studies")), value = T)
sleep_dirs <- gsub("5_distances$", "7_sleep_spots", dist_dirs)
lapply(sleep_dirs, dir.create)

# 1 - Classify sleep spots -------------------------------------------------------

dcp_files <- list.files(
  dist_dirs, pattern = "1_all_tracks_dcp_distances", full.names = T)
n_files <- length(dcp_files)

lapply(seq_along(dcp_files), function(n){

  # print(paste("started:", n))

  fin <- dcp_files[n]

  fout <- gsub(
    "5_distances", "7_sleep_spots",
    gsub("1_all_tracks_dcp_distances", "1_all_tracks_dcp_sleep_spots", fin))

  if(!file.exists(fout)){

    # load the data
    dcp_locs <- fread(fin)
    dcp_locs <- dcp_locs[
      , .(day_cycle, day_period, x_median, y_median, t_median, file)]
    dcp_locs <- dcp_locs[!is.na(x_median) & !is.na(y_median)]

    # group by deployment and day_period
    sleeps <- dcp_locs[, {
      # copy .SD to avoid modifying by reference
      p_locs <- copy(.SD)
      # order by timestamp
      setorder(p_locs, t_median)

      # if there are more than 1 point calculate the distances between all
      if (nrow(p_locs) > 1) {
        # convert to sf object and calculate distances
        p_sf <- st_as_sf(
          p_locs, coords = c("x_median", "y_median"), crs = crs, remove = FALSE)
        distm <- as.dist(st_distance(p_sf))
        # run DBSCAN clustering
        # assign the same cluster to all the points that have distance < eps
        clust_dbs <- dbscan(distm, eps = 50, minPts = 1)
        p_locs$sleep_cluster_dbscan <- as.integer(clust_dbs$cluster)
        
        clust_hcl <- hclust(distm, method = "complete")
        clust_hclc <- cutree(clust_hcl, h = 50) # 50 meters
        p_locs$sleep_cluster_hclust <- as.integer(clust_hclc)
        
      } else {

        p_locs$sleep_cluster_hclust <- as.integer(0)
        p_locs$sleep_cluster_dbscan <- as.integer(0)

      }

      # return datatable
      as.data.table(p_locs)

    }, by = .(file, day_period)]
    
    
    sleeps <- melt(
      sleeps,
      measure.vars = c("sleep_cluster_dbscan", "sleep_cluster_hclust"), 
      variable.name = "method",
      value.name = "sleep_cluster"
    )[, method := gsub("sleep_cluster_", "", method)]
    

    sleeps[
      , `:=`(
        revisit_day_cycle = c(0, diff(day_cycle)),
        revisit_timelag = c(NA, diff(t_median, units = "days"))
      ), by = .(file, day_period, sleep_cluster, method)
    ]

    sleeps[
      , tracking_gap := {
        if (revisit_day_cycle > 1) {
          gap_days <- (day_cycle - revisit_day_cycle + 1):(day_cycle - 1)
          track_days <- sleeps[file == .BY$file & method == .BY$method &
                                 day_period == .BY$day_period &
                                 day_cycle %in% gap_days, day_cycle]
          length(gap_days) - length(track_days)
        } else 0
      }, by = .(file, day_period, day_cycle, method)
    ]

    fwrite(sleeps, fout)

    cat(paste("\nProcessed:", n, "|", n_files))

  }

})



# 2 - Add europe ----------------------------------------------------------

sleep_files <- grep(
  "continent", list.files(sleep_dirs, full.names = T), value = T, invert = T)
n_files <- length(sleep_files)

target_sp <- gsub(
  "_", " ",
  unique(gsub(".*/Studies/([^/]+)/7_sleep_spots/.*", "\\1", sleep_files)))

elton <- fread(here::here("Published_data", "BirdFuncDat.txt"))[
  Scientific %in% target_sp][, .(species = Scientific, nocturnal = Nocturnal)][
    , nocturnal := fifelse(species == "Nycticorax nycticorax", 1, nocturnal)]

lapply(seq_along(sleep_files), function(n){
  
  # print(paste("started:", n))
  
  fin <- sleep_files[n]
  fout <- gsub(".rds", "_continent.rds", fin)
  
  if(file.exists(fout)){
   
    sp <- gsub("_", " ", gsub(".*/Studies/([^/]+)/7_sleep_spots/.*", "\\1", fin))
    
    nocturn <- elton[species == sp, nocturnal] 
    
    # load steps and add continent information
    sleeps <- fread(fin)[sleep_cluster != 0][, nocturnal := nocturn]
    
    if(nocturn == 1){ sleeps <- sleeps[day_period == "day"] 
    } else{ sleeps <- sleeps[day_period == "night"] }
    
    sleeps <- add_worldmap_data(sleeps, coord_cols = c("x_median", "y_median"))
    
    setnames(
      sleeps, old = c("sovereignt", "admin"), new = c("country", "country_admin"))
    
    sleeps[ , ':=' (
      n_points_per_track = .N, 
      n_sleep_cluster = uniqueN(sleep_cluster)), # it was called sleep_spots
      by = c("file", "method")]
    
    fwrite(sleeps, fout)
    
    cat(paste("\nProcessed:", n, "|", n_files)) 
    
  }
  
})


# 3 - Plot sleep spots -----------------------------------------------------

sleep_files <- grep(
  "continent", list.files(sleep_dirs, full.names = T), value = T)

spots <- fread(fout)


# Calculate pairwise distances within each group (file, sleep_spot)
dist_dt <- spots[revisit_day_cycle > 0][, {
  # Convert to sf object for distance calculation
  sf_pts <- st_as_sf(.SD, coords = c("x_median", "y_median"), crs = crs) # set CRS to match your data!
  # Calculate pairwise distances (returns a matrix in meters if projected CRS)
  dist_mat <- st_distance(sf_pts)
  # Get lower triangle indices (unique pairs)
  inds <- which(lower.tri(dist_mat), arr.ind = TRUE)
  if (nrow(inds) > 0) {
    data.table(
      file = file[1],
      sleep_cluster = sleep_spot[1],
      idx1 = inds[,1],
      idx2 = inds[,2],
      dist = as.numeric(dist_mat[inds]) # convert units to numeric
    )
  } else {
    NULL
  }
  
  
  by = .(file, sleep_cluster, method)]

dist_dt[, dist := as.numeric(dist)] |> 
  ggplot() + 
  geom_histogram(
    aes(x = dist, fill = method), 
    alpha = 0.5, bins = 100, position = "identity"
  ) + 
  labs(
    title = "Distance between locations within sleep clusters",
    x = "distance [m]"
  ) +
  theme_bw()


spoints <- st_as_sf(
  spots, coords = c("x_median", "y_median"), crs = crs, remove = FALSE)

spots[revisit_day_cycle > 0] |> 
  ggplot() +
  geom_point(aes(x = revisit_day_cycle, y = tracking_gap, color = method), alpha = 0.5) +
  theme_bw() + 
  facet_wrap(~method, free = T) +
  labs(
    x = "sleep cluster revisited after [days]", 
    y = "tracking gap [days]"
  ) +
  theme(legend.position = "none")

spots[revisit_day_cycle < 25] |> 
  ggplot() + 
  geom_violin(
    aes(x = revisit_day_cycle, y = method,  fill = method), 
    alpha = 0.5
  ) 

spots[, yd := day_cycle_to_yd(day_cycle)]

spots_med <- spots[, revis_med := median(revisit_day_cycle, na.rm = TRUE), 
                   by = .(file, method, yd)]

spots |> 
  ggplot() + 
  geom_line(aes(x = yd, y = revis_med, color = method))

spoints <- st_as_sf(
  spots, coords = c("x_median", "y_median"), crs = crs, remove = FALSE)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") 

spoints |> 
  ggplot() + 
  geom_sf(aes(color = file)) + 
  theme(legend.position = "none")



ns_bbox <- sf::st_bbox(spoints) |> expand_bbox(5)

# main plot
mm <- ggplot() + 
  geom_sf(data = world, fill = "white") + 
  geom_sf(data = spoints, aes(color = file), alpha = 0.5, size = 1.5) +
  coord_sf(
    xlim = c(ns_bbox$xmin, ns_bbox$xmax),
    ylim = c(ns_bbox$ymin, ns_bbox$ymax), 
    expand = F
  ) +
  theme_void() + 
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "none", 
    plot.margin =  margin(0, 0, 0, 0, unit = "cm"), 
    panel.spacing = unit(0, "cm"),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

# generate the polygon that will "zoom in from the world map
zoom_pol <- sf::st_polygon(
  list(
    matrix(
      c(
        ns_bbox$xmax, ns_bbox$ymax,  
        180, 90,  
        180, -90, 
        ns_bbox$xmax, ns_bbox$ymin, 
        ns_bbox$xmax, ns_bbox$ymax 
      ),
      ncol = 2, 
      byrow = TRUE
    )
  ))






# plot sleep spots --------------------------------------------------------

#' 
#' # GET TRAITS DATA
#' #' AVONET:
#' #' - migration:
#' #'   1 = Sedentary.
#' #'   2 = Partially migratory - minority of population migrates long distances,
#' #'   or most of population undergoes short-distance migration,
#' #'   nomadic movements, distinct altitudinal migration, etc.
#' #'   3 = Migratory - majority of population undertakes long-distance migration
#' avonet <- setDT(readxl::read_xlsx(
#'   file.path(here::here("Published_data", "AVONET1_Birdlife.xlsx")),
#'   sheet = 2))[Species1 %in% target_sp][
#'     , .(species = Species1, migration = Migration)]
#' # ELTON
#' # nocturnal or not, Nycticorax nycticorax seemed erroneous
#' 
#' 
#' lapply(seq_along(sleep_files), function(n){
#' 
#'   # print(paste("started:", n))
#' 
#'   fin <- sleep_files[n]
#'   
#' 
#' 
#'   cat(paste("\nProcessed:", n, "|", n_files))
#' 
#'   
#' })
#' 
#' fin <- sleep_files[10]
#' 
#' sp <- gsub("_", " ", gsub(".*/Studies/([^/]+)/7_sleep_spots/.*", "\\1", fin))
#' 
#' slp <- fread(fin)[
#'   , deploy_id := paste0("dpl_", .GRP), by = file][
#'   , revisit_day_cycle := fifelse(
#'     is.na(revisit_day_cycle), 0, revisit_day_cycle)][
#'   , file := NULL]
#' setorder(slp, deploy_id, day_period, t_median)
#' 
#' 
#' 
#' if(traits[species == sp, nocturnal] == 1){ 
#'   slp <- slp[day_period == "day"] } else{ slp <- slp[day_period == "night"] }
#' 
#' trial <- slp[
#'   , tracking_gap := {
#'     
#'     if(revisit_day_cycle > 1) {
#'       # define days between previous visit and this visit
#'       gap_days <- (day_cycle - revisit_day_cycle + 1):(day_cycle)
#'       # check how many tracking days there are in this period
#'       track_days <- slp[
#'         deploy_id == .BY$deploy_id & day_cycle %in% gap_days, day_cycle]
#'       length(gap_days) - length(track_days)
#'     } else { 0 }
#'     
#'   }, by = .(deploy_id, day_cycle) ]
#' 
#' # We'll make a helper function:
#' check_gaps <- function(dt) {
#'   dt[, has_data_gap := {
#'     if(revisit_day_cycle > 1) {
#'       # Define days between previous visit and this visit (exclusive)
#'       gap_days <- (day_cycle - revisit_day_cycle + 1):(day_cycle - 1)
#'       # Check if there are any records for this animal/sleep_cluster in the gap
#'       any_data <- DT[
#'         animal_id == .BY$animal_id & sleep_cluster == .BY$sleep_cluster & day_cycle %in% gap_days
#'       ]
#'       # If no data in the gap, TRUE (there is a gap), else FALSE
#'       !nrow(any_data)
#'     } else {
#'       NA  # Not relevant for revisit_day_cycle <= 1
#'     }
#'   }, by = .(animal_id, sleep_cluster, day_cycle)]
#' }
#' 
#' # Run the function (modify to your column names as needed)
#' check_gaps(DT)
#' 
#' check <- slp[deploy_id == "dpl_1" & sleep_cluster == 12]
#' 
#' spoints <- st_as_sf(check, coords = c("x_median", "y_median"), crs = crs, remove = F)
#' 
#' library(ggplot2)
#' 
#' ggplot(spoints) + 
#'   geom_sf(aes(color = as.factor(sleep_cluster), size = 2, alpha = 0.5))
#' 
#' 
#'                