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
# source("6_distance_datatable_FUNCTIONS.R")
# source("0_helper_functions.R")

# OUTPUT directories -------------------------------------------------------

# MAIN DATA DIRECTORY
data_dir <- here::here("Data")
study_dir <- file.path(data_dir, "Studies")
graphs_dir <- file.path(data_dir, "Graphs")

# DATA OUTPUT
# directories with median distances
dist_dirs <- file.path(list.files(study_dir, full.names = T), "6_distances")
sleep_dirs <- gsub("6_distances$", "7_sleep_spots", dist_dirs)
invisible(lapply(sleep_dirs, dir.create, showWarnings = F))

# PLOT OUTPUT
plots_dir <- file.path(graphs_dir, "7_sleep_spots")
dir.create(plots_dir, showWarnings = F)

# Create folder names from day_limits
dl_folders <- c("nightEnd_night", "nauticalDawn_nauticalDusk", "dawn_dusk")
regions <- c("World", "Europe")
# Create all combinations
all_plots_dirs <- file.path(
  plots_dir, rep(regions, each=length(dl_folders)), dl_folders)

invisible(lapply(all_plots_dirs, dir.create, showWarnings = F, recursive = T))


# INPUT variables --------------------------------------------------------------

# GET WHETHER THE ANIMAL IS NOCTURNAL OR DIURNAL FOR THE SLEEP SPOTS
target_sp <- gsub("_", " ", list.files(study_dir))
nsp <- length(target_sp)

nocturnal_dt <- fread(here::here("Published_data", "00_bird_traits.csv"))[
  birdlife_name %in% target_sp]
# there were duplicates because of some synonyms
nocturnal_dt <- unique(nocturnal_dt[, .(birdlife_name, nocturnal)])
# missing information for this species
nocturnal_dt[birdlife_name == "Larus smithsonianus", nocturnal := 0]

# coordinate system to use when calculating distances
crs <- st_crs(4326)

# 1 - Classify sleep spots -------------------------------------------------------

# listing all dcp files
files <- list.files(dist_dirs, 
  pattern = "1_all_tracks_dcp_distances.*continent.rds", full.names = T)

spots_files <- gsub(
  "6_distances", "7_sleep_spots", 
  gsub("dcp_distances", "dcp_sleep_spots", files))

files <- files[!file.exists(spots_files)]
nf <- length(files)
rm(sspots_files)

if(nf > 0){
  
  # only columns of interest
  cols_of_interest <- c(
    "day_cycle", "day_period", "x_median", "y_median", "t_median", 
    "file", "continent", "country", "country_admin")
  
  lapply(seq_along(files), function(n){
    
    fin <- files[n]
    
    fout <- gsub(
      "6_distances", "7_sleep_spots", 
      gsub("dcp_distances", "dcp_sleep_spots", fin))
    
    sp <- gsub(".*/Studies/(.*)_(.*)/6_distances/.*", "\\1 \\2", fin)
    
    sleep_time <- ifelse(
      nocturnal_dt[birdlife_name == sp, nocturnal] == 1, "day", "night")
    
    # load the data
    dcp_locs <- fread(fin)
    
    # subsetting dcp just for the sleeping points
    dcp_locs <- dcp_locs[, ..cols_of_interest][day_period == sleep_time]
    # just in case
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
      
    }, by = file]
    
    
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
      ), by = .(file, sleep_cluster, method)]
    
    sleeps[
      , tracking_gap := {
        if (revisit_day_cycle > 1) {
          gap_days <- (day_cycle - revisit_day_cycle + 1):(day_cycle - 1)
          track_days <- sleeps[file == .BY$file & method == .BY$method &
                                 day_cycle %in% gap_days, day_cycle]
          length(gap_days) - length(track_days)
        } else 0
      }, by = .(file, day_cycle, method)
    ]
    
    fwrite(sleeps, fout)
    
    cat("\nProcessed:", n, "|", nf)
  })
  
  
}

rm(files, nf)

# 2 - Distances within clusters -----------------------------------

files <- list.files(sleep_dirs, 
    pattern = "1_all_tracks_dcp_sleep_spots.*_continent.rds", full.names = T)

out_files <- gsub("1_all_tracks_dcp_", "2_distances_between_", files)

files <- files[!file.exists(out_files)]
rm(out_files)
nf <- length(files)

lapply(seq_along(files), function(i){
  
  fin <- files[i]
  fout <- gsub("1_all_tracks_dcp_", "2_distances_between_", fin)
  
  spots <- fread(fin)
  spots[, nclust := .N, by = .(file, method, sleep_cluster)]
  
  # keep only clusters with more than 1 point
  spots <- spots[nclust > 1][, .(
    file, method, sleep_cluster, 
    x_median, y_median, day_cycle, continent, country, country_admin,
    revisit_day_cycle, tracking_gap)]
  
  spots <- split(spots, by = c("file", "method", "sleep_cluster"))
  
  dist_clust <- rbindlist(lapply(spots, function(dt){
    
    
    if(nrow(dt)){
      
    # convert to sf object for distance calculation
    sf_pts <- st_as_sf(dt, coords = c("x_median", "y_median"), crs = crs)
    
    # calculate pairwise distances (returns a matrix in meters if projected CRS)
    dist_mat <- st_distance(sf_pts)
    
    # get lower triangle indices (unique pairs)
    inds <- which(lower.tri(dist_mat), arr.ind = TRUE)
    
    data.table(
      file = dt$file[1],
      sleep_cluster = dt$sleep_cluster[1],
      method = dt$method[1],
      idx1 = inds[,1],
      idx2 = inds[,2],
      dist = as.numeric(dist_mat[inds]) # convert units to numeric
    )
    
    } 
    
  }), fill = T)
  
  cat("\nProcessed:", i, "|", nf)
  
  fwrite(dist_clust, fout)
  
})



# 3 - Plot distances within clusters ------------------------------------------

plots_dir <- file.path(
  graphs_dir, "7_sleep_spots", "2_distances_between_sleep_spots")
dir.create(plots_dir, showWarnings = F)

files <- list.files(sleep_dirs, 
  pattern = "2_distances_between_sleep_spots.*_continent.rds", full.names = T)

target_sp <- unique(gsub(".*/Studies/(.*?)/7_sleep_spots/.*", "\\1", files))
bfiles <- unique(basename(files))
out_plots <- unlist(lapply(target_sp, function(sp) {
  file.path(plots_dir, gsub(".rds", paste0("_", sp, ".png"), bfiles))
}))

files <- files[!file.exists(out_plots)]
nf <- length(files)
rm(out_plots, target_sp, bfiles)

lapply(seq_along(files), function(i){
  
  fin <- files[i]
  sp <- gsub(".*/Studies/(.*?)/7_sleep_spots/.*", "\\1", fin)

  fout <- file.path(
    plots_dir, gsub(".rds", paste0("_", sp, ".png"), basename(fin)))
  
  spots <- fread(fin)
  spots[, method := factor(method, levels = c("dbscan", "hclust"))]
  
  spots |> 
    ggplot() + 
    geom_histogram(
      aes(x = dist, fill = method), alpha = 0.6, bins = 100, color = "gray33"
    ) + 
    labs(
      title = sprintf(
        "%s - distances within sleep clusters", gsub("_", " ", sp)),
      x = "distance [m]"
    ) +
    theme_bw() +
    theme(legend.position = "bottom")
  
  ggsave(filename = fout, width = 17, units = "cm")
  
  cat("\nProcessed:", i, "|", nf)
})

rm(files, nf)


# Spatial plots of large clusters -----------------------------------------

files <- list.files(sleep_dirs,
   pattern = "2_distances_between_sleep_spots.*_continent.rds", full.names = T)
nf <- length(files)

lapply(seq_along(files), function(i){
  
  fin <- files[i]
  sp <- gsub(".*/Studies/(.*?)_(.*?)/7_sleep_spots/.*", "\\1 \\2", fin)
  dist_spots <- fread(fin)[method == "dbscan"]
  
  # Calculate max distance per cluster in each file
  max_dist <- dist_spots[
    , .(max_distance = max(dist)), by = .(file, sleep_cluster)]
  max_dist <- max_dist[which.max(max_distance)]
  rm(dist_spots)
  
  fout <- file.path(
    plots_dir, sprintf("%s_cluster_with_highest_distance.png", gsub(" ", "_", sp)))
  
  org_fin <- gsub(
    "2_distances_between_sleep_spots", "1_all_tracks_dcp_sleep_spots", fin)
  
  spots <- fread(org_fin)
  db_cluster <- spots[method == "dbscan" & 
      file == max_dist$file & sleep_cluster == max_dist$sleep_cluster]
  h_cluster <- spots[method == "hclust" & 
      file == max_dist$file & day_cycle %in% db_cluster$day_cycle][
        , .(day_cycle, hclust_sleep_cluster = sleep_cluster)]
  
  db_track <- spots[method == "dbscan" & file == max_dist$file]
  
  db_cluster <- merge(db_cluster, h_cluster, by = "day_cycle", all.x = TRUE)
  
  source("6_distance_PLOT_FUNCTIONS.R")
  
  max_clust <- plot_on_world_map(
    db_cluster, 
    as_steps = F, 
    coord_cols = c("x_median", "y_median"), 
    exp_deg = 0.0001, 
    color = "hclust_sleep_cluster", 
    title = sprintf("Max dist - dbscan sleep cluster %d", max_dist$sleep_cluster)) +
    guides(color = guide_legend(nrow = 2, title = "hclust")) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  
  all_clust <- plot_on_world_map(
    db_track, 
    as_steps = F, 
    coord_cols = c("x_median", "y_median"), 
    exp_deg = 0.0001, 
    title = sprintf("%s - dbscan  all sleep clusters", sp), 
    color_by = "sleep_cluster") +    
    guides(color = guide_legend(nrow = 2, title = "dbscan")) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  
  all_clust / max_clust
  
  ggsave(filename = fout, width = 18, height = 18, units = "cm")
  
  cat("\nProcessed:", i, "|", nf)
})


# 3 - Plot revisit time vs. tracking gap----------------------------------------


sleep_files <- grep(
  "continent", list.files(sleep_dirs, full.names = T), value = T)
nsf <- length(sleep_files)

lapply(seq(nsf), function(i){
  
  
  fin <- sleep_files[i]
  spots <- fread(fin)
  
  sp <- gsub("_", " ", gsub(".*/Studies/([^/]+)/7_sleep_spots/.*", "\\1", fin))
  dl_folder <- sub(".*spots_(.*)_(.*)_continent.*", "\\1_\\2", fin)
  
  
  lapply(regions, function(regi){
    
    spots_regi <- spots
    
    if(regi == "Europe"){ spots_regi <- spots_regi[continent == "Europe"] }
    
    spots_regi <- spots_regi[, nights_per_file := .N, by = file][nights_per_file > 10]
    spots_regi[, date := as.Date(day_cycle)]
    
    spots_regi[
      , method := paste0(method, ": ", uniqueN(sleep_cluster), " sleep clusters"), 
      by = method]
    
    nights_total <- uniqueN(spots_regi$date)
    ind_total <- uniqueN(spots_regi$file)
    
    sp_tit <- sprintf(
      "%s | %s - revisited sleeping spots \n for %d nights and %d individuals", 
      sp, regi, nights_total, ind_total)
    
    if(nrow(spots_regi) > 0){
      
      spots_regi[
        , facet_1 := sprintf(
          "%s - %d times at new and %d at revisited one",
          method, sum(revisit_day_cycle == 0), sum(revisit_day_cycle > 0)), 
        by = method]
      
      spots_regi |> 
        ggplot() + 
        geom_histogram(
          aes(x = revisit_day_cycle, y = after_stat(count), fill = method), 
          position = "identity", color = "gray33", alpha = 0.5, bins = 100) + 
        facet_wrap(~facet_1, scales = "free", ncol = 1) +
        theme_bw() +
        labs(
          x = "sleep cluster revisit times [days]",
          title = sp_tit
        ) +
        theme(
          legend.position = "none", 
          plot.title = element_text(hjust = 0.5)) 
      
      pname <- sprintf("1_%s_sleep_spots_revisit_times.png", gsub(" ", "_", sp))
      
      ggsave(
        filename = file.path(plots_dir, regi, dl_folder, pname), 
        width = 15, units = "cm")
      
    }
    
    
    # checking only revisits
    
    spots_regi <- spots_regi[revisit_day_cycle > 0]
    
    spots_regi <- spots_regi[
      , facet_2 := paste0(
        method, ", of which ", uniqueN(sleep_cluster), " revisited"), by = method]
    
    if(nrow(spots_regi) > 0){
      
      spots_regi |> 
        ggplot() + 
        geom_point(
          aes(x = revisit_day_cycle, y = tracking_gap, color = method), 
          size = 2, alpha = 0.6
        ) +
        theme_bw() + 
        scale_y_log10() +
        scale_x_log10() +
        facet_wrap(~facet_2, scales = "free_y", ncol = 1) +
        labs(
          x = "sleep cluster revisited after [days]", 
          y = "tracking gap [days]", 
          title = sp_tit
        ) +
        theme(
          legend.position = "none", 
          plot.title = element_text(hjust = 0.5))
      
      pname <- sprintf(
        "2_%s_revisited_sleep_spots_vs_tracking_gap.png", gsub(" ", "_", sp))
      
      ggsave(
        filename = file.path(plots_dir, regi, dl_folder, pname), 
        width = 15, units = "cm")
      
      
    }
   
    
    
  })
  
  cat("\n Plot 1 done:", i , "|", nsf)
  
})




# revisit time across species ---------------------------------------------

target_sp <- gsub("_", " ", list.files(file.path(data_dir, "Studies")))


methods <- c("dbscan", "hclust")

lapply(unique(basename(sleep_files)), function(ftype){
  
  dl_files <- grep(ftype, sleep_files, value = T)
  
  dl <- sub(".*spots_(.*)_(.*)_continent.*", "\\1_\\2", ftype)
  
  dldt <- rbindlist(lapply(dl_files, function(fin){
    
    spots <- fread(fin)
    
    spots[, nights_per_file := .N, by = file]
    
    spots <- spots[
      nights_per_file > 10 & revisit_day_cycle > 0 & revisit_day_cycle <= 366]
    
    spots <- spots[
      , .(revisit_day_cycle, sleep_cluster, method, continent, file)]
    spots[, species :=  gsub(
      "_", " ", gsub(".*/Studies/([^/]+)/7_sleep_spots/.*", "\\1", fin))]
  }))
  
  lapply(regions, function(regi){
    
    dldt_regi <- dldt
    
    if(regi == "Europe"){ dldt_regi <- dldt[continent == "Europe"] }
    
    
    if(nrow(dldt_regi) > 0){
      
      lapply(methods, function(m){
        
        dldtm <- dldt[method == m]
        
        dldtm[, species := factor(species, levels = target_sp)]
        
        slepcount <- dldtm[
          , .(n_clust = max(sleep_cluster)), by = .(species, file)]
        slepcount <- slepcount[, .(n_clust = sum(n_clust)), by = species]
        slepcount[, n_clust := sprintf("[N=%d]", n_clust)]
        
        p <- dldtm |> 
          ggplot() + 
          geom_boxplot(aes(y = species, x = revisit_day_cycle)) +
          geom_text(
            data = slepcount, aes(y = species, x = 367, label = n_clust)) +
          labs(
            title = sprintf( "Revisit time across species - %s - %s", m, dl), 
            x = "sleep cluster revisit time [days]", 
            caption = "revisit time limited to 366 days",
            y = "species"
          ) +
          theme_bw() 
        
        pname <- sprintf(
          "3_%s_%s_revisit_time_across_species_366.png", m, dl)
        
        ggsave(
          file.path(plots_dir, regi, pname), 
          height = 35,
          width = 15
        )
        
        p30 <- dldtm[revisit_day_cycle <= 30] |> 
          ggplot() + 
          geom_boxplot(aes(y = species, x = revisit_day_cycle)) +
          geom_text(
            data = slepcount, aes(y = species, x = 31, label = n_clust)) +
          labs(
            title = sprintf( "Revisit time across species - %s - %s", m, dl), 
            x = "sleep cluster revisit time [days]", 
            caption = "revisit time limited to 30 days",
            y = "species"
          ) +
          theme_bw()
        
        pname <- sprintf(
          "3_%s_%s_revisit_time_across_species_30.png", m, dl)
        
        ggsave(
          file.path(plots_dir, regi, pname), 
          height = 35,
          width = 15
        )
        
        
      })
      
      
    }
    
    })
  
})
    


# # Calculate pairwise distances within each group (file, sleep_spot)
# dist_dt <- spots[revisit_day_cycle > 0][, {
#   # Convert to sf object for distance calculation
#   sf_pts <- st_as_sf(.SD, coords = c("x_median", "y_median"), crs = crs) # set CRS to match your data!
#   # Calculate pairwise distances (returns a matrix in meters if projected CRS)
#   dist_mat <- st_distance(sf_pts)
#   # Get lower triangle indices (unique pairs)
#   inds <- which(lower.tri(dist_mat), arr.ind = TRUE)
#   if (nrow(inds) > 0) {
#     data.table(
#       file = file[1],
#       sleep_cluster = sleep_spot[1],
#       idx1 = inds[,1],
#       idx2 = inds[,2],
#       dist = as.numeric(dist_mat[inds]) # convert units to numeric
#     )
#   } else {
#     NULL
#   }
#   
#   
#   by = .(file, sleep_cluster, method)]
# 
# dist_dt[, dist := as.numeric(dist)] |> 
#   ggplot() + 
#   geom_histogram(
#     aes(x = dist, fill = method), 
#     alpha = 0.5, bins = 100, position = "identity"
#   ) + 
#   labs(
#     title = "Distance between locations within sleep clusters",
#     x = "distance [m]"
#   ) +
#   theme_bw()
# 
# 
# spoints <- st_as_sf(
#   spots, coords = c("x_median", "y_median"), crs = crs, remove = FALSE)
# 
# spots[revisit_day_cycle > 0] |> 
#   ggplot() +
#   geom_point(aes(x = revisit_day_cycle, y = tracking_gap, color = method), alpha = 0.5) +
#   theme_bw() + 
#   facet_wrap(~method, free = T) +
#   labs(
#     x = "sleep cluster revisited after [days]", 
#     y = "tracking gap [days]"
#   ) +
#   theme(legend.position = "none")
# 
# spots[revisit_day_cycle < 25] |> 
#   ggplot() + 
#   geom_violin(
#     aes(x = revisit_day_cycle, y = method,  fill = method), 
#     alpha = 0.5
#   ) 
# 
# spots[, yd := day_cycle_to_yd(day_cycle)]
# 
# spots_med <- spots[, revis_med := median(revisit_day_cycle, na.rm = TRUE), 
#                    by = .(file, method, yd)]
# 
# spots |> 
#   ggplot() + 
#   geom_line(aes(x = yd, y = revis_med, color = method))
# 
# spoints <- st_as_sf(
#   spots, coords = c("x_median", "y_median"), crs = crs, remove = FALSE)
# 
# world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") 
# 
# spoints |> 
#   ggplot() + 
#   geom_sf(aes(color = file)) + 
#   theme(legend.position = "none")
# 
# 
# 
# ns_bbox <- sf::st_bbox(spoints) |> expand_bbox(5)
# 
# # main plot
# mm <- ggplot() + 
#   geom_sf(data = world, fill = "white") + 
#   geom_sf(data = spoints, aes(color = file), alpha = 0.5, size = 1.5) +
#   coord_sf(
#     xlim = c(ns_bbox$xmin, ns_bbox$xmax),
#     ylim = c(ns_bbox$ymin, ns_bbox$ymax), 
#     expand = F
#   ) +
#   theme_void() + 
#   theme(
#     axis.text = element_blank(),
#     axis.title = element_blank(),
#     legend.position = "none", 
#     plot.margin =  margin(0, 0, 0, 0, unit = "cm"), 
#     panel.spacing = unit(0, "cm"),
#     panel.grid = element_blank(),
#     panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
#   )
# 
# # generate the polygon that will "zoom in from the world map
# zoom_pol <- sf::st_polygon(
#   list(
#     matrix(
#       c(
#         ns_bbox$xmax, ns_bbox$ymax,  
#         180, 90,  
#         180, -90, 
#         ns_bbox$xmax, ns_bbox$ymin, 
#         ns_bbox$xmax, ns_bbox$ymax 
#       ),
#       ncol = 2, 
#       byrow = TRUE
#     )
#   ))
# 





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