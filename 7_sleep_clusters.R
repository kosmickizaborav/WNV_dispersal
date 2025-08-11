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
library(patchwork)
#source("6_distance_FUNCTIONS.R")
source("6_distance_PLOT_FUNCTIONS.R")
# source("0_helper_functions.R")

# OUTPUT directories -------------------------------------------------------

# MAIN DATA DIRECTORY
data_dir <- here::here("Data")
study_dir <- file.path(data_dir, "Studies")
graphs_dir <- file.path(data_dir, "Graphs")

# DATA OUTPUT
# directories with median distances
sleep_dirs <- file.path(
  list.files(study_dir, full.names = T), "7_sleep_clusters")
invisible(lapply(sleep_dirs, dir.create, showWarnings = F))

# PLOT OUTPUT
plots_dir <- file.path(graphs_dir, "7_sleep_clusters")
dir.create(plots_dir, showWarnings = F)

# # Create folder names from day_limits
# dl_folders <- c("nightEnd_night", "nauticalDawn_nauticalDusk", "dawn_dusk")
# regions <- c("World", "Europe")
# # Create all combinations
# all_plots_dirs <- file.path(
#   plots_dir, rep(regions, each=length(dl_folders)), dl_folders)
# 
# invisible(lapply(all_plots_dirs, dir.create, showWarnings = F, recursive = T))


# INPUT variables --------------------------------------------------------------

# GET WHETHER THE ANIMAL IS NOCTURNAL OR DIURNAL FOR THE SLEEP SPOTS
target_sp <- gsub("_", " ", list.files(study_dir))
nsp <- length(target_sp)

traits_dt <- fread(here::here("Published_data", "00_bird_traits.csv"))[
  birdlife_name %in% target_sp]
# there were duplicates because of some synonyms
traits_dt <- unique(traits_dt[, .(birdlife_name, nocturnal, migration_txt)])


# coordinate system to use when calculating distances
crs <- st_crs(4326)


# 1 - Explore parameter influence -----------------------------------------

fin_dcp <- "1_all_tracks_dcp_distances_nauticalDawn_nauticalDusk_continent.rds"

# CALCULATE DISTANCES BETWEEN SLEEPING POSITIONS

dist_dirs <- file.path(list.files(study_dir, full.names = T), "6_distances")

# listing all dcp files
files <- list.files(dist_dirs, fin_dcp, full.names = T)
files_out <- gsub("/6_distances/", "/7_sleep_clusters/", files)

files <- files[
  !(file.exists(gsub(".rds", "_nodata.rds", files_out)) | 
      file.exists(gsub(".rds", "_done.rds", files_out)))]
rm(files_out)

nf <- length(files)

if(nf > 0){
  
  # only columns of interest
  cols_of_interest <- c(
    "day_cycle", "day_period", "x_median", "y_median", "t_median",
    "file", "continent", "subregion")
  
  lapply(seq_along(files), function(n){
    
    fin <- files[n]
    
    out_dir <- gsub("6_distances", "7_sleep_clusters", dirname(fin))
    
    sp <- gsub(".*/Studies/(.*)_(.*)/6_distances/.*", "\\1 \\2", fin)
    
    sleep_time <- ifelse(
      traits_dt[birdlife_name == sp, nocturnal] == 1, "day", "night")
    
    # load the data
    dcp_locs <- fread(fin)[ , ..cols_of_interest]
    
    dcp_locs[, n_sleep := sum(day_period == sleep_time), by = file]
    
    if(!any(dcp_locs$day_period == sleep_time) | all(dcp_locs$n_sleep <= 1)){
      
      fout <- gsub(".rds", "_nodata.rds", basename(fin))
      
      fwrite(unique(dcp_locs[, .(file, n_sleep)]), file.path(out_dir, fout))
      rm(dcp_locs)
      
      return(NULL)
      
    }
    
    dir.create(file.path(out_dir, "1_raw_distances"), showWarnings = F)
    
    # sub-setting dcp just for the sleeping points
    dcp_locs <- dcp_locs[day_period == sleep_time & n_sleep > 1]
    
    files_done <- list.files(file.path(out_dir, "1_raw_distances"))
    
    dcp_locs[, file_done := basename(file) %in% files_done]
    
    if(all(dcp_locs$file_done == T)){ 
      
      fout <- gsub(".rds", "_done.rds", basename(fin))
      fwrite(unique(dcp_locs[, .(file, n_sleep)]), file.path(out_dir, fout))
      
      rm(dcp_locs)
      
      return(NULL)
    }
    
    dcp_locs <- dcp_locs[file_done == F]
    
    tracks <- unique(dcp_locs$file)
    
    lapply(seq_along(tracks), function(ti){
      
      trk <- tracks[ti]
      
      fout <- basename(trk)
      
      p_locs <- dcp_locs[file == trk]
      
      setorder(p_locs, t_median)
      
      # convert to sf object and calculate distances
      p_sf <- st_as_sf(
        p_locs, coords = c("x_median", "y_median"), crs = crs, remove = FALSE)
      dist <- st_distance(p_sf)
      
      saveRDS(dist, file = file.path(out_dir, "1_raw_distances", fout))
      
      rm(p_locs, p_sf, dist)
      
    })
    
    
    cat("\nProcessed:", n, "|", nf, " | ", sp)
    
    rm(dcp_locs, tracks)
    
    gc(verbose = F)
    
    return(NULL)
    
  })
  
  
}


# EXPLORE DIFFERENT PARAMETERS
par_dt <- CJ(
  slp_dir = gsub("1_raw_distances", "2_dbscan_clusters", 
    list.files(sleep_dirs, pattern = "1_raw_distances", full.names = T)),
  n_pts = 1:7, 
  dist_thr = seq(50, 1000, 50)
)

lapply(unique(par_dt$slp_dir), dir.create, showWarnings = F)

par_dt[, fname := sprintf(
  "dbscan_clusters_npts_%d_distthr_%d.rds", n_pts, dist_thr)]

# check which files already exist
par_dt[, fout := file.path(slp_dir, fname)]
par_dt[, done := file.exists(fout)]
par_dt[, species := gsub(".*/Studies/(.*?)/7_sleep_clusters/.*", "\\1", slp_dir)]
par_dt[, check := all(done == T), by = "species"]
par_dt <- par_dt[check == F]
par_dt <- par_dt[done == F]

sleep_dirs_dist <- gsub(
  "2_dbscan_clusters", "1_raw_distances", unique(par_dt$slp_dir))
ndirs <- length(sleep_dirs_dist)

par_dt[, c("slp_dir", "fname", "check") := NULL]


if(ndirs > 0){

  lapply(seq_along(sleep_dirs_dist), function(i){

    slp_dst_dir <- sleep_dirs_dist[i]

    files <- list.files(slp_dst_dir, full.names = T, pattern = "sen.rds")
    nf <- length(files)

    odir <- gsub("1_raw_distances", "2_dbscan_clusters", slp_dst_dir)

    sp <- gsub(".*/Studies/(.*?)/7_sleep_clusters/.*", "\\1", odir)

    to_do <- par_dt[done == F & species == sp]
    npar <- nrow(to_do)

    lapply(1:npar, function(np){

      mpts <- to_do[np, n_pts]
      eps_thr <- to_do[np, dist_thr]

      fout <- to_do[np, fout]

      clust_dt_all <- rbindlist(lapply(seq_along(files), function(n){

        fin <- files[n]
        distm <- readRDS(fin)

        # run DBSCAN clustering
        # assign the same cluster to all the points that have distance < eps
        clust_dbs <- dbscan(as.dist(distm), eps = eps_thr, minPts = mpts)

        clust_dt <- data.table(
          cluster = as.integer(clust_dbs$cluster),
          file = fin
        )

        rm(clust_dbs, distm)

        cat("\nProcessed:", i, "|", ndirs, "|", np, "|", npar, "|", n, "|", nf)

        return(clust_dt)

      }))

      fwrite(clust_dt_all, fout)

      rm(clust_dt_all)

    })

  })


}

rm(par_dt)



# PLOT: parameter influence -----------------------------------------------
# 
# clust_dirs <- list.files(
#   sleep_dirs, pattern = "2_dbscan_clusters", full.names = T)
# 
# dir.create(file.path(plots_dir, "1_paremeters"), showWarnings = F)
# 
# files_out <- file.path(
#   plots_dir, "1_paremeters", sprintf("1_%s_dbscan_clusters_sum.png", 
#   gsub(".*/Studies/(.*?)/7_sleep_clusters/.*", "\\1", clust_dirs)))
# 
# clust_dirs <- clust_dirs[!file.exists(files_out)]
# rm(files_out)
# 
# ndirs <- length(clust_dirs)
# 
# 
# col <- paletteer_d("tvthemes::bigHero6")[1:7]
# names(col) <- factor(1:7)
# 
# 
# 
# lapply(seq_along(clust_dirs), function(i){
#   
#   clsdir <- clust_dirs[i]
#   sp <- gsub(".*/Studies/(.*?)/7_sleep_clusters/.*", "\\1", clsdir)
#   
#   files <- list.files(clsdir, full.names = T)
#   
#   dt <- rbindlist(
#     lapply(files, function(fin){
#       fread(fin)[, clust_file := basename(fin)]})
#     )
#   
#   dt[, n_per_file := .N, by = .(file, clust_file)]
#   
#   dt <- dt[n_per_file >= 10]
#   
#   if(nrow(dt) == 0){ return(NULL) }
#   
#   nf <- uniqueN(dt$file)
#   
#   dt[, npts := as.numeric(gsub(".*npts_(\\d+)_distthr_.*", "\\1", clust_file))]
#   dt[, dist_thr := as.numeric(gsub(".*distthr_(\\d+).rds", "\\1", clust_file))]
#   
#   dt <- dt[npts > 1]
# 
#   
#   dt_summary <- dt[, .(
#     n_total = .N,
#     n_clusters = uniqueN(cluster[cluster != 0]), 
#     n_unassigned = sum(cluster == 0)
#     ), by = .(file, npts, dist_thr)]
#   
#   dt_summary[, npts := factor(npts)]
#   
#   dt_sum <- dt_summary[
#     , .(
#       n_total = sum(n_total), 
#       n_clusters = sum(n_clusters),
#       n_unassigned = sum(n_unassigned),
#       perc_unassigned = sum(n_unassigned)/sum(n_total) * 100
#     ), by = .(npts, dist_thr)]
#   
#   nt <- unique(dt_sum$n_total)
#   
#   pc <- dt_sum |> 
#     ggplot() +
#     geom_line(
#       aes(x = dist_thr, y = n_clusters, color = npts),
#       alpha = 0.5, linewidth = 2
#     ) +
#     theme_bw() +
#     scale_color_manual(values = col) +
#     guides(color = guide_legend(nrow = 1)) +
#     labs(
#       x = "distance threshold [m]",
#       y = "number of clusters", 
#       color = "min points in a cluster", 
#       title = sprintf("Total number of clusters across tracks [N = %d]", nf)
#     ) + 
#     theme(
#       legend.position = "none", 
#       plot.title = element_text(hjust = 0.5)
#       #axis.title.x = element_blank(), 
#       #axis.text.x = element_blank()
#     )
#   
#   pp <- dt_sum |> 
#     ggplot() +
#     geom_line(
#       aes(x = dist_thr, y = perc_unassigned, color = npts), 
#       alpha = 0.5, linewidth = 2) +
#     theme_bw() +
#     scale_color_manual(values = col) +
#     guides(color = guide_legend(nrow = 1)) +
#     labs(
#       x = "distance threshold [m]",
#       y = "unassigned locations [%]", 
#       color = "min points in a cluster", 
#       title = sprintf("Percentage of unassigned locations [N = %d]", nt),
#     ) + 
#     theme(
#       legend.position = "bottom", 
#       plot.title = element_text(hjust = 0.5))
#   
#   pc + pp +
#     plot_annotation(
#       title = sprintf("%s - sleep clusters", gsub("_", " ", sp))
#     ) +
#     plot_layout(guides = "collect") & 
#     theme(
#       plot.title = element_text(hjust = 0.5, face = "bold"), 
#       legend.position = "bottom"
#   )
#   
#   ggsave(
#     filename = file.path(
#       plots_dir, "1_paremeters", sprintf("1_%s_dbscan_clusters_sum.png", sp)),
#     height = 10, width = 20)
# 
#   rm(pc, pp, dt_sum, dt_summary)
#   
#   cat("\nProcessed:", i, "|", ndirs, "\n")
#   
# })





# Revisits ----------------------------------------------------------------

file_dcp <- "1_all_tracks_dcp_distances_nauticalDawn_nauticalDusk_continent.rds"

# only columns of interest
cols_of_interest <- c(
  "day_cycle", "day_period", "x_median", "y_median", "t_median",
  "file", "continent", "subregion")

clust_dirs <- list.files(
  sleep_dirs, pattern = "2_dbscan_clusters", full.names = T)

return_dirs <- gsub("2_dbscan_clusters", "3_return_times", clust_dirs)
invisible(lapply(return_dirs, dir.create, showWarnings = F))

files <- list.files(clust_dirs, full.names = T)
files <- files[!file.exists(gsub("2_dbscan_clusters", "3_return_times", files))]


clust_dirs <- unique(dirname(files))
ncls <- length(clust_dirs)

rm(files, return_dirs)

lapply(seq_along(clust_dirs), function(i){
  
  clsdir <- clust_dirs[i]
  
  sp <- gsub(".*/Studies/(.*?)_(.*?)/7_sleep_clusters/.*", "\\1 \\2", clsdir)
  sleep_time <- ifelse(
    traits_dt[birdlife_name == sp, nocturnal] == 1, "day", "night")
  
  tracks <- list.files(
    gsub("2_dbscan_clusters", "1_raw_distances", clsdir))
  
  dcp_stp <- fread(file.path(
    gsub("7_sleep_clusters", "6_distances", dirname(clsdir)), fin_dcp))
  dcp_stp <- dcp_stp[, ..cols_of_interest]
  
  dcp_stp[, bfile := basename(file)]
  
  dcp_stp <- dcp_stp[day_period == sleep_time & bfile %in% tracks]
  setorder(dcp_stp, file, t_median)
  
  dcp_stp[, file := NULL]
  
  files <- list.files(clsdir, full.names = T)
  files <- files[
    !file.exists(gsub("2_dbscan_clusters", "3_return_times", files))]
  nf <- length(files)
  
  lapply(seq_along(files), function(n){
    
    fin <- files[n]
    
    dbsc <- fread(fin)
    dbsc[, bfile := basename(file)]
    
    dbsc <- split(dbsc, by = "bfile")
    
    dbsc <- rbindlist(lapply(dbsc, function(dt){
      
      bfile_dt <- unique(dt$bfile)
      
      trk_data <- dcp_stp[bfile == bfile_dt][, bfile := NULL]
      
      cbind(trk_data, dt)[, bfile := NULL]
      
    }))
    
    
    dbsc[
      , `:=`(
        revisit_day_cycle = c(0, diff(day_cycle)),
        revisit_timelag = c(NA, diff(t_median, units = "days"))
      ), by = .(file, cluster)]
    
    dbsc[
      , tracking_gap := {
        if (revisit_day_cycle > 1) {
          gap_days <- (day_cycle - revisit_day_cycle + 1):(day_cycle - 1)
          track_days <- dbsc[
            file == .BY$file & day_cycle %in% gap_days, day_cycle]
          length(gap_days) - length(track_days)
        } else 0
      }, by = .(file, day_cycle)
    ]
    
    
    fwrite(dbsc, gsub("2_dbscan_clusters", "3_return_times", fin))
    
    cat("DONE:", n, "|", nf, "-", i, "|", ncls, "\n")
    
    rm(dbsc)
    
    return(invisible(NULL))
    
  })
  
  rm(dcp_stp, files)
  gc(verbose = F)
  
  return(NULL)
  
  
})





# MEDIAN PLOTS
#
# if(nf > 1){
#   
#   par_overview <- dt_summary[, .(
#     clust_75 = quantile(n_clusters, 0.75),
#     clust_25 = quantile(n_clusters, 0.25),
#     clust_90 = quantile(n_clusters, 0.90),
#     clust_10 = quantile(n_clusters, 0.10),
#     clust_50 = median(n_clusters)
#   ), by = .(npts, dist_thr)]
#   
#   par_overview[, npts_lab := sprintf("min %d points in a cluster", npts)]
#   
#   par_overview <- split(par_overview, by = "npts")
#   
#   lapply(par_overview, function(prw){
#     
#     labs <- unique(prw[
#       dist_thr == 500, 
#       .(npts, clust_75, clust_25, clust_90, clust_10, clust_50)])
#     
#     labs <- melt(
#       labs, 
#       id.vars = "npts", 
#       variable.name = "quantile", 
#       value.name = "n_clust")
#     labs[, quantile := paste(gsub("clust_", "", quantile), "%")]
#     
#     
#     prw |> 
#       ggplot() +
#       geom_ribbon(
#         aes(x = dist_thr, ymin = clust_25, ymax = clust_75, fill = npts),
#         alpha = 0.3) +
#       geom_ribbon(
#         aes(x = dist_thr, ymin = clust_10, ymax = clust_90, fill = npts), 
#         alpha = 0.3) +
#       geom_line(
#         aes(x = dist_thr, y = clust_50, color = as.factor(npts))) +
#       ggrepel::geom_text_repel(
#         data = labs, 
#         aes(x = 500, y = n_clust, label = quantile, color = npts), 
#         size = 4, 
#         fontface = "bold",
#         nudge_x = 20, 
#         segment.size = 0,
#         show.legend = F) +
#       theme_bw() +
#       scale_color_manual(values = col) +
#       scale_fill_manual(values = col) +
#       facet_grid(~npts_lab, scales = "free_y") + 
#       labs(
#         x = "distance threshold [m]",
#         y = "number of clusters"
#       ) + 
#       theme(legend.position = "none")
#     
#     pname <- sprintf("1_%s_dbscan_clusters_npts_%d.png", sp, unique(prw$npts))
#     
#     ggsave(filename = file.path(plots_dir, "1_paremeters", pname))
#     
#   })
#   
# } 







#' # 1 - Classify sleep spots -------------------------------------------------------
#' 
#' # problesm: strnus vulgaris, streptopellia turtur, phoeniconaias minor, 
#' # bubulcus ibis, mergus merganser
#' 
#' # listing all dcp files
#' files <- list.files(dist_dirs, 
#'   pattern = "1_all_tracks_dcp_distances.*continent.rds", full.names = T)
#' 
#' clust_files <- gsub(
#'   "6_distances", "7_sleep_clusters", 
#'   gsub("dcp_distances", "dcp_sleep_clusters", files))
#' 
#' files <- files[!(file.exists(clust_files) | 
#'       file.exists(gsub(".rds", "_nodata.rds", clust_files)))]
#' nf <- length(files)
#' rm(clust_files)
#' 
#' if(nf > 0){
#'   
#'   # only columns of interest
#'   cols_of_interest <- c(
#'     "day_cycle", "day_period", "x_median", "y_median", "t_median", 
#'     "file", "continent", "subregion")
#'   
#'   lapply(seq_along(files), function(n){
#'     
#'     fin <- files[n]
#'     
#'     fout <- gsub(
#'       "6_distances", "7_sleep_clusters", 
#'       gsub("dcp_distances", "dcp_sleep_spots", fin))
#'     
#'     sp <- gsub(".*/Studies/(.*)_(.*)/6_distances/.*", "\\1 \\2", fin)
#'     
#'     sleep_time <- ifelse(
#'       nocturnal_dt[birdlife_name == sp, nocturnal] == 1, "day", "night")
#'     
#'     # load the data
#'     dcp_locs <- fread(fin)
#'     
#'     dcp_locs <- dcp_locs[, ..cols_of_interest]
#'     
#'     if(!any(dcp_locs$day_period == sleep_time)){
#'       
#'       fout <- gsub(".rds", "_nodata.rds", fout)
#'       
#'       fwrite(dcp_locs, fout)
#'       
#'       return(NULL)
#'       
#'     }
#'     
#'     # subsetting dcp just for the sleeping points
#'     dcp_locs <- dcp_locs[day_period == sleep_time]
#'     
#'     # group by deployment and day_period
#'     sleeps <- dcp_locs[, {
#'       # copy .SD to avoid modifying by reference
#'       p_locs <- copy(.SD)
#'       # order by timestamp
#'       setorder(p_locs, t_median)
#'       
#'       # if there are more than 1 point calculate the distances between all
#'       if (nrow(p_locs) > 1) {
#'         # convert to sf object and calculate distances
#'         p_sf <- st_as_sf(
#'           p_locs, coords = c("x_median", "y_median"), crs = crs, remove = FALSE)
#'         distm <- as.dist(st_distance(p_sf))
#'         # run DBSCAN clustering
#'         # assign the same cluster to all the points that have distance < eps
#'         clust_dbs <- dbscan(distm, eps = 50, minPts = 1)
#'         p_locs$sleep_cluster_dbscan <- as.integer(clust_dbs$cluster)
#'         
#'         clust_hcl <- hclust(distm, method = "complete")
#'         clust_hclc <- cutree(clust_hcl, h = 50) # 50 meters
#'         p_locs$sleep_cluster_hclust <- as.integer(clust_hclc)
#'         
#'       } else {
#'         
#'         p_locs$sleep_cluster_hclust <- as.integer(0)
#'         p_locs$sleep_cluster_dbscan <- as.integer(0)
#'         
#'       }
#'       
#'       # return datatable
#'       as.data.table(p_locs)
#'       
#'     }, by = file]
#'     
#'     
#'     sleeps <- melt(
#'       sleeps,
#'       measure.vars = c("sleep_cluster_dbscan", "sleep_cluster_hclust"), 
#'       variable.name = "method",
#'       value.name = "sleep_cluster"
#'     )[, method := gsub("sleep_cluster_", "", method)]
#'     
#'     
#'     sleeps[
#'       , `:=`(
#'         revisit_day_cycle = c(0, diff(day_cycle)),
#'         revisit_timelag = c(NA, diff(t_median, units = "days"))
#'       ), by = .(file, sleep_cluster, method)]
#'     
#'     sleeps[
#'       , tracking_gap := {
#'         if (revisit_day_cycle > 1) {
#'           gap_days <- (day_cycle - revisit_day_cycle + 1):(day_cycle - 1)
#'           track_days <- sleeps[file == .BY$file & method == .BY$method &
#'                                  day_cycle %in% gap_days, day_cycle]
#'           length(gap_days) - length(track_days)
#'         } else 0
#'       }, by = .(file, day_cycle, method)
#'     ]
#'     
#'     fwrite(sleeps, fout)
#'     
#'     
#'     
#'     cat("\nProcessed:", n, "|", nf)
#'   })
#'   
#'   
#' }
#' 
#' rm(files, nf)
#' 
#' # 2 - Distances within clusters ------------------------------------------------
#' 
#' files <- list.files(
#'   sleep_dirs, full.names = T,
#'   pattern = "1_all_tracks_dcp_sleep_spots.*_continent.rds")
#' 
#' out_files <- gsub("1_all_tracks_dcp_", "2_distances_between_", files)
#' 
#' files <- files[
#'   !(file.exists(out_files)|file.exists(gsub(".rds", "_nodata.rds", out_files)))]
#' rm(out_files)
#' nf <- length(files)
#' 
#' if(nf > 0){
#'   
#'   lapply(seq_along(files), function(i){
#'     
#'     fin <- files[i]
#'     fout <- gsub("1_all_tracks_dcp_", "2_distances_between_", fin)
#'     
#'     spots <- fread(fin)
#'     
#'     # keep only clusters with more than 1 point
#'     spots <- spots[, .(file, method, sleep_cluster, x_median, y_median,continent)]
#'     spots <- spots[, n_locs_clust := .N, by = .(file, method, sleep_cluster)]
#'     
#'     if(any(spots$n_locs_clust > 1)){
#'       
#'       spots <- spots[n_locs_clust > 1]
#'       
#'       spots <- split(spots, by = c("file", "method", "sleep_cluster"))
#'       
#'       dist_clust <- rbindlist(lapply(spots, function(dt){
#'         
#'         # convert to sf object for distance calculation
#'         sf_pts <- st_as_sf(dt, coords = c("x_median", "y_median"), crs = crs)
#'         
#'         # calculate pairwise distances (returns a matrix in meters if projected CRS)
#'         dist_mat <- st_distance(sf_pts)
#'         
#'         # get lower triangle indices (unique pairs)
#'         inds <- which(lower.tri(dist_mat), arr.ind = TRUE)
#'         
#'         data.table(
#'           file = dt$file[1],
#'           sleep_cluster = dt$sleep_cluster[1],
#'           method = dt$method[1],
#'           continent = dt$continent[1],
#'           n_locs_clust = dt$n_locs_clust[1],
#'           idx1 = inds[,1],
#'           idx2 = inds[,2],
#'           dist = as.numeric(dist_mat[inds]) # convert units to numeric
#'         )
#'         
#'       }), fill = T)
#'       
#'       fwrite(dist_clust, fout)
#'       
#'       rm(dist_clust, spots)
#'       
#'     } else {
#'       
#'       nodata_dt <- spots[
#'         , .(
#'           n_clusters = uniqueN(sleep_cluster), 
#'           n_locs = .N), 
#'         by = .(file, method)]
#'       
#'       fwrite(nodata_dt, gsub(".rds", "_nodata.rds", fout))
#'       
#'       rm(nodata_dt, spots)
#'       
#'     }
#'     
#'     cat("\nProcessed:", i, "|", nf)
#'     
#'   })
#'   
#' }
#' 
#' 
#' 
#' # PLOT 1: Plot distances within clusters --------------------------------------
#' 
#' files <- list.files(sleep_dirs, full.names = T,
#'   pattern = "2_distances_between_sleep_spots.*_continent.rds")
#' 
#' out_files <- unlist(lapply(files, function(fin){
#'   sp <- gsub(".*/Studies/(.*?)/7_sleep_clusters/.*", "\\1", fin)
#'   dl <- gsub(
#'     ".*/2_distances_between_sleep_spots_(.*?)_continent.rds", "\\1", fin)
#'   pname <- sprintf("1_%s_distances_within_clusters.png", sp)
#'   regi <- "World"
#'   fout <- file.path(graphs_dir, "7_sleep_clusters", regi, dl, pname)
#' }))
#' 
#' files <- files[!file.exists(out_files)]
#' nf <- length(files)
#' rm(out_files)
#' 
#' if(nf > 0){
#'   
#'   lapply(seq_along(files), function(i){
#'     
#'     fin <- files[i]
#'     sp <- gsub(".*/Studies/(.*?)/7_sleep_clusters/.*", "\\1", fin)
#'     dl <- gsub(
#'       ".*/2_distances_between_sleep_spots_(.*?)_continent.rds", "\\1", fin)
#'     
#'     pname <- sprintf("1_%s_distances_within_clusters.png", sp)
#'     
#'     spots <- fread(fin)
#'     # spots[, method := factor(method, levels = c("dbscan", "hclust"))]
#'     
#'     lapply(regions, function(regi){
#'       
#'       spot_regi <- copy(spots)
#'       
#'       if(regi == "Europe"){ spot_regi <- spot_regi[continent == "Europe"] }
#'       
#'       if(nrow(spot_regi) == 0){ return(NULL) }
#'       
#'       n_depl <- uniqueN(spot_regi$file)
#'       n_clust <- uniqueN(paste0(spot_regi$file, "_", spot_regi$sleep_cluster))
#'       
#'       pal <- c("dbscan" = "#440154FF", "hclust"	= "#FDE725FF")
#'       
#'       pp <- spot_regi |> 
#'         ggplot() + 
#'         geom_histogram(
#'           aes(x = dist, fill = method), 
#'           alpha = 0.6, bins = 50, #color = "gray44", 
#'           position = "dodge"
#'         ) + 
#'         scale_fill_manual(values = pal) +
#'         labs(
#'           title = sprintf(
#'             "%s, %s - deployments: %d, clusters: %d", 
#'             gsub("_", " ", sp), regi, n_depl, n_clust),
#'           x = "distance [m]",
#'           subtitle = "Distances within sleep clusters"
#'         ) +
#'         theme_bw() +
#'         theme(legend.position = "bottom")
#'       
#'       max_dt <- spot_regi[
#'         , .(dist = max(dist)), by = .(file, sleep_cluster, method)]
#'       
#'       pmax <- max_dt |> 
#'         ggplot() + 
#'         geom_histogram(
#'           aes(x = dist, fill = method), 
#'           alpha = 0.6, bins = 50, # color = "gray44", 
#'           position = "dodge"
#'         ) + 
#'         scale_fill_manual(values = pal) +
#'         labs(
#'           subtitle = "Maximum distances",
#'           x = "distance [m]"
#'         ) +
#'         theme_bw() +
#'         theme(legend.position = "bottom")
#'       
#'       fout <- file.path(graphs_dir, "7_sleep_clusters", regi, dl, pname)
#'       
#'       pout <- (pp + pmax) + 
#'         plot_layout(guides = "collect") & 
#'         theme(legend.position = "bottom")
#'       
#'       ggsave(filename = fout, plot = pout, width = 17, height = 12, units = "cm")
#'       
#'       
#'       rm(pp, spot_regi, pmax, pout)
#'       gc(verbose = F)
#'       
#'     })
#'     
#'     rm(spots)
#'     
#'     cat("\nProcessed:", i, "|", nf, "\n")
#'   })
#'   
#' }
#' 
#' rm(files, nf)
#' 
#' 
#' # PLOT 2: Spatial plots of large clusters --------------------------------------
#' 
#' files <- list.files(sleep_dirs,
#'    pattern = "2_distances_between_sleep_spots.*_continent.rds", full.names = T)
#' nf <- length(files)
#' 
#' out_plots <- unlist(lapply(files, function(fin){
#' 
#'   sp <- unique(gsub(".*/Studies/(.*?)/7_sleep_clusters/.*", "\\1", fin))
#'   dl <- gsub(
#'     ".*/2_distances_between_sleep_spots_(.*?)_continent.rds", "\\1", fin)
#'   pname <- sprintf("1_%s_distances_within_clusters.png", sp)
#'   regi <- "World"
#'   fout <- file.path(graphs_dir, "7_sleep_clusters", regi, dl, pname)
#' 
#'   return(fout)
#' 
#' }))
#' 
#' files <- files[!file.exists(out_plots)]
#' nf <- length(files)
#' rm(out_plots)
#' 
#' if(nf > 0){
#'   
#'   lapply(seq_along(files), function(i){
#'     
#'     fin <- files[i]
#'     sp <- gsub(".*/Studies/(.*?)/7_sleep_clusters/.*", "\\1", fin)
#'     dl <- gsub(
#'       ".*2_distances_between_sleep_spots_(.*?)_continent.rds", "\\1", fin)
#'     
#'     dbclust_dist <- fread(fin)[method == "dbscan"]
#'     
#'     lapply(regions, function(regi){
#'       
#'       pname <- sprintf("2_%s_cluster_with_highest_distance.png", sp)
#'       dbdist_regi <- dbclust_dist
#'       
#'       fout <- file.path(graphs_dir, "7_sleep_clusters", regi, dl, pname)
#'       
#'       if(regi == "Europe"){ 
#'         dbdist_regi <- dbdist_regi[continent == "Europe"] }
#'       
#'       if(nrow(dbdist_regi) == 0){ return(NULL) }
#'       
#'       # Calculate max distance per cluster in each file
#'       max_dist <- dbdist_regi[
#'         , .(max_distance = max(dist)), by = .(file, sleep_cluster)]
#'       max_dist <- max_dist[which.max(max_distance)]
#'       # get all distances
#'       max_dist_dt <- dbdist_regi[
#'         sleep_cluster == max_dist$sleep_cluster & file == max_dist$file]
#'       
#'       rm(dbdist_regi)
#'       
#'       fin <- gsub(
#'         "2_distances_between_sleep_spots", "1_all_tracks_dcp_sleep_spots", fin)
#'       
#'       spots <- fread(fin)[file == max_dist$file]
#'       
#'       db_track <- spots[method == "dbscan"]
#'       
#'       if(regi == "Europe"){ db_track <- db_track[continent == "Europe"] }
#'       
#'       pmap <- plot_on_world_map(
#'         db_track, 
#'         as_steps = F, 
#'         coord_cols = c("x_median", "y_median"), 
#'         exp_deg = 0.01, 
#'         color = "sleep_cluster", 
#'         title = sprintf("%s - dbscan sleep clusters", gsub("_", " ", sp))) +
#'         geom_text(
#'           data = db_track[sleep_cluster == max_dist$sleep_cluster][1,],
#'           aes(x = x_median, y = y_median, label = sleep_cluster)
#'         ) +
#'         labs(caption = paste("file:", basename(max_dist$file)))
#'       
#'       db_cluster <- db_track[sleep_cluster == max_dist$sleep_cluster]
#'       h_cluster <- spots[method == "hclust" & day_cycle %in% db_cluster$day_cycle][
#'         , .(day_cycle, hclust_sleep_cluster = sleep_cluster)]
#'       
#'       db_cluster <- merge(db_cluster, h_cluster, by = "day_cycle", all.x = TRUE)
#'       
#'       db_cluster <- sf::st_as_sf(
#'         db_cluster, coords = c("x_median", "y_median"), crs = 4326)
#'       
#'       clp <- db_cluster |> 
#'         ggplot() + 
#'         geom_sf(aes(color = factor(hclust_sleep_cluster)), size = 2) +
#'         theme_bw() +
#'         scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
#'         labs(
#'           title = sprintf("Cluster %d - sleep locations and distances", max_dist$sleep_cluster),
#'           caption = "colored by hclust classification"
#'         ) +
#'         theme(legend.position = "none")
#'       
#'       
#'       cdist <- max_dist_dt |> 
#'         ggplot() + 
#'         geom_histogram(
#'           aes(x = dist), color = "gray33", fill = "gray90", bins = 100
#'         ) + 
#'         theme_bw() +
#'         labs(x = "distance [m]") 
#'       
#'       pout <- pmap / (clp + cdist) +
#'         plot_layout(heights = c(2, 1))
#'       
#'       rm(pmap, clp, cdist, max_dist_dt, db_cluster, h_cluster, db_track, spots, 
#'          dbdist_regi, max_dist)
#'       
#'       ggsave(fout, plot = pout, width = 18, height = 17, units = "cm")
#'       
#'       
#'     })
#'     
#'     cat("\nProcessed:", i, "|", nf, "\n")
#'     
#'   })
#'   
#' }
#'   
#'  
#' # PLOT 3: Revisit time vs. tracking gap----------------------------------------
#' 
#' files <- list.files(
#'   sleep_dirs, full.names = T,
#'   pattern = "1_all_tracks_dcp_sleep_spots.*_continent.rds")
#' 
#' lapply(seq_along(nf), function(i){
#'   
#'   
#'   fin <- sleep_files[i]
#'   spots <- fread(fin)
#'   
#'   sp <- gsub("_", " ", gsub(".*/Studies/([^/]+)/7_sleep_clusters/.*", "\\1", fin))
#'   dl_folder <- sub(".*spots_(.*)_(.*)_continent.*", "\\1_\\2", fin)
#'   
#'   
#'   lapply(regions, function(regi){
#'     
#'     spots_regi <- spots
#'     
#'     if(regi == "Europe"){ spots_regi <- spots_regi[continent == "Europe"] }
#'     
#'     spots_regi <- spots_regi[, nights_per_file := .N, by = file][nights_per_file > 10]
#'     spots_regi[, date := as.Date(day_cycle)]
#'     
#'     spots_regi[
#'       , method := paste0(method, ": ", uniqueN(sleep_cluster), " sleep clusters"), 
#'       by = method]
#'     
#'     nights_total <- uniqueN(spots_regi$date)
#'     ind_total <- uniqueN(spots_regi$file)
#'     
#'     sp_tit <- sprintf(
#'       "%s | %s - revisited sleeping spots \n for %d nights and %d individuals", 
#'       sp, regi, nights_total, ind_total)
#'     
#'     if(nrow(spots_regi) > 0){
#'       
#'       spots_regi[
#'         , facet_1 := sprintf(
#'           "%s - %d times at new and %d at revisited one",
#'           method, sum(revisit_day_cycle == 0), sum(revisit_day_cycle > 0)), 
#'         by = method]
#'       
#'       spots_regi |> 
#'         ggplot() + 
#'         geom_histogram(
#'           aes(x = revisit_day_cycle, y = after_stat(count), fill = method), 
#'           position = "identity", color = "gray33", alpha = 0.5, bins = 100) + 
#'         facet_wrap(~facet_1, scales = "free", ncol = 1) +
#'         theme_bw() +
#'         labs(
#'           x = "sleep cluster revisit times [days]",
#'           title = sp_tit
#'         ) +
#'         theme(
#'           legend.position = "none", 
#'           plot.title = element_text(hjust = 0.5)) 
#'       
#'       pname <- sprintf("1_%s_sleep_spots_revisit_times.png", gsub(" ", "_", sp))
#'       
#'       ggsave(
#'         filename = file.path(plots_dir, regi, dl_folder, pname), 
#'         width = 15, units = "cm")
#'       
#'     }
#'     
#'     
#'     # checking only revisits
#'     
#'     spots_regi <- spots_regi[revisit_day_cycle > 0]
#'     
#'     spots_regi <- spots_regi[
#'       , facet_2 := paste0(
#'         method, ", of which ", uniqueN(sleep_cluster), " revisited"), by = method]
#'     
#'     if(nrow(spots_regi) > 0){
#'       
#'       spots_regi |> 
#'         ggplot() + 
#'         geom_point(
#'           aes(x = revisit_day_cycle, y = tracking_gap, color = method), 
#'           size = 2, alpha = 0.6
#'         ) +
#'         theme_bw() + 
#'         scale_y_log10() +
#'         scale_x_log10() +
#'         facet_wrap(~facet_2, scales = "free_y", ncol = 1) +
#'         labs(
#'           x = "sleep cluster revisited after [days]", 
#'           y = "tracking gap [days]", 
#'           title = sp_tit
#'         ) +
#'         theme(
#'           legend.position = "none", 
#'           plot.title = element_text(hjust = 0.5))
#'       
#'       pname <- sprintf(
#'         "2_%s_revisited_sleep_spots_vs_tracking_gap.png", gsub(" ", "_", sp))
#'       
#'       ggsave(
#'         filename = file.path(plots_dir, regi, dl_folder, pname), 
#'         width = 15, units = "cm")
#'       
#'       
#'     }
#'    
#'     
#'     
#'   })
#'   
#'   cat("\n Plot 1 done:", i , "|", nsf)
#'   
#' })
#' 
#' 
#' 
#' 
#' # revisit time across species ---------------------------------------------
#' 
#' target_sp <- gsub("_", " ", list.files(file.path(data_dir, "Studies")))
#' 
#' 
#' methods <- c("dbscan", "hclust")
#' 
#' lapply(unique(basename(sleep_files)), function(ftype){
#'   
#'   dl_files <- grep(ftype, sleep_files, value = T)
#'   
#'   dl <- sub(".*spots_(.*)_(.*)_continent.*", "\\1_\\2", ftype)
#'   
#'   dldt <- rbindlist(lapply(dl_files, function(fin){
#'     
#'     spots <- fread(fin)
#'     
#'     spots[, nights_per_file := .N, by = file]
#'     
#'     spots <- spots[
#'       nights_per_file > 10 & revisit_day_cycle > 0 & revisit_day_cycle <= 366]
#'     
#'     spots <- spots[
#'       , .(revisit_day_cycle, sleep_cluster, method, continent, file)]
#'     spots[, species :=  gsub(
#'       "_", " ", gsub(".*/Studies/([^/]+)/7_sleep_clusters/.*", "\\1", fin))]
#'   }))
#'   
#'   lapply(regions, function(regi){
#'     
#'     dldt_regi <- dldt
#'     
#'     if(regi == "Europe"){ dldt_regi <- dldt[continent == "Europe"] }
#'     
#'     
#'     if(nrow(dldt_regi) > 0){
#'       
#'       lapply(methods, function(m){
#'         
#'         dldtm <- dldt[method == m]
#'         
#'         dldtm[, species := factor(species, levels = target_sp)]
#'         
#'         slepcount <- dldtm[
#'           , .(n_clust = max(sleep_cluster)), by = .(species, file)]
#'         slepcount <- slepcount[, .(n_clust = sum(n_clust)), by = species]
#'         slepcount[, n_clust := sprintf("[N=%d]", n_clust)]
#'         
#'         p <- dldtm |> 
#'           ggplot() + 
#'           geom_boxplot(aes(y = species, x = revisit_day_cycle)) +
#'           geom_text(
#'             data = slepcount, aes(y = species, x = 367, label = n_clust)) +
#'           labs(
#'             title = sprintf( "Revisit time across species - %s - %s", m, dl), 
#'             x = "sleep cluster revisit time [days]", 
#'             caption = "revisit time limited to 366 days",
#'             y = "species"
#'           ) +
#'           theme_bw() 
#'         
#'         pname <- sprintf(
#'           "3_%s_%s_revisit_time_across_species_366.png", m, dl)
#'         
#'         ggsave(
#'           file.path(plots_dir, regi, pname), 
#'           height = 35,
#'           width = 15
#'         )
#'         
#'         p30 <- dldtm[revisit_day_cycle <= 30] |> 
#'           ggplot() + 
#'           geom_boxplot(aes(y = species, x = revisit_day_cycle)) +
#'           geom_text(
#'             data = slepcount, aes(y = species, x = 31, label = n_clust)) +
#'           labs(
#'             title = sprintf( "Revisit time across species - %s - %s", m, dl), 
#'             x = "sleep cluster revisit time [days]", 
#'             caption = "revisit time limited to 30 days",
#'             y = "species"
#'           ) +
#'           theme_bw()
#'         
#'         pname <- sprintf(
#'           "3_%s_%s_revisit_time_across_species_30.png", m, dl)
#'         
#'         ggsave(
#'           file.path(plots_dir, regi, pname), 
#'           height = 35,
#'           width = 15
#'         )
#'         
#'         
#'       })
#'       
#'       
#'     }
#'     
#'     })
#'   
#' })
#'     
#' 
#' 
#' # # Calculate pairwise distances within each group (file, sleep_spot)
#' # dist_dt <- spots[revisit_day_cycle > 0][, {
#' #   # Convert to sf object for distance calculation
#' #   sf_pts <- st_as_sf(.SD, coords = c("x_median", "y_median"), crs = crs) # set CRS to match your data!
#' #   # Calculate pairwise distances (returns a matrix in meters if projected CRS)
#' #   dist_mat <- st_distance(sf_pts)
#' #   # Get lower triangle indices (unique pairs)
#' #   inds <- which(lower.tri(dist_mat), arr.ind = TRUE)
#' #   if (nrow(inds) > 0) {
#' #     data.table(
#' #       file = file[1],
#' #       sleep_cluster = sleep_spot[1],
#' #       idx1 = inds[,1],
#' #       idx2 = inds[,2],
#' #       dist = as.numeric(dist_mat[inds]) # convert units to numeric
#' #     )
#' #   } else {
#' #     NULL
#' #   }
#' #   
#' #   
#' #   by = .(file, sleep_cluster, method)]
#' # 
#' # dist_dt[, dist := as.numeric(dist)] |> 
#' #   ggplot() + 
#' #   geom_histogram(
#' #     aes(x = dist, fill = method), 
#' #     alpha = 0.5, bins = 100, position = "identity"
#' #   ) + 
#' #   labs(
#' #     title = "Distance between locations within sleep clusters",
#' #     x = "distance [m]"
#' #   ) +
#' #   theme_bw()
#' # 
#' # 
#' # spoints <- st_as_sf(
#' #   spots, coords = c("x_median", "y_median"), crs = crs, remove = FALSE)
#' # 
#' # spots[revisit_day_cycle > 0] |> 
#' #   ggplot() +
#' #   geom_point(aes(x = revisit_day_cycle, y = tracking_gap, color = method), alpha = 0.5) +
#' #   theme_bw() + 
#' #   facet_wrap(~method, free = T) +
#' #   labs(
#' #     x = "sleep cluster revisited after [days]", 
#' #     y = "tracking gap [days]"
#' #   ) +
#' #   theme(legend.position = "none")
#' # 
#' # spots[revisit_day_cycle < 25] |> 
#' #   ggplot() + 
#' #   geom_violin(
#' #     aes(x = revisit_day_cycle, y = method,  fill = method), 
#' #     alpha = 0.5
#' #   ) 
#' # 
#' # spots[, yd := day_cycle_to_yd(day_cycle)]
#' # 
#' # spots_med <- spots[, revis_med := median(revisit_day_cycle, na.rm = TRUE), 
#' #                    by = .(file, method, yd)]
#' # 
#' # spots |> 
#' #   ggplot() + 
#' #   geom_line(aes(x = yd, y = revis_med, color = method))
#' # 
#' # spoints <- st_as_sf(
#' #   spots, coords = c("x_median", "y_median"), crs = crs, remove = FALSE)
#' # 
#' # world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") 
#' # 
#' # spoints |> 
#' #   ggplot() + 
#' #   geom_sf(aes(color = file)) + 
#' #   theme(legend.position = "none")
#' # 
#' # 
#' # 
#' # ns_bbox <- sf::st_bbox(spoints) |> expand_bbox(5)
#' # 
#' # # main plot
#' # mm <- ggplot() + 
#' #   geom_sf(data = world, fill = "white") + 
#' #   geom_sf(data = spoints, aes(color = file), alpha = 0.5, size = 1.5) +
#' #   coord_sf(
#' #     xlim = c(ns_bbox$xmin, ns_bbox$xmax),
#' #     ylim = c(ns_bbox$ymin, ns_bbox$ymax), 
#' #     expand = F
#' #   ) +
#' #   theme_void() + 
#' #   theme(
#' #     axis.text = element_blank(),
#' #     axis.title = element_blank(),
#' #     legend.position = "none", 
#' #     plot.margin =  margin(0, 0, 0, 0, unit = "cm"), 
#' #     panel.spacing = unit(0, "cm"),
#' #     panel.grid = element_blank(),
#' #     panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
#' #   )
#' # 
#' # # generate the polygon that will "zoom in from the world map
#' # zoom_pol <- sf::st_polygon(
#' #   list(
#' #     matrix(
#' #       c(
#' #         ns_bbox$xmax, ns_bbox$ymax,  
#' #         180, 90,  
#' #         180, -90, 
#' #         ns_bbox$xmax, ns_bbox$ymin, 
#' #         ns_bbox$xmax, ns_bbox$ymax 
#' #       ),
#' #       ncol = 2, 
#' #       byrow = TRUE
#' #     )
#' #   ))
#' # 
#' 
#' 
#' 
#' 
#' 
#' # plot sleep spots --------------------------------------------------------
#' 
#' #' 
#' #' # GET TRAITS DATA
#' #' #' AVONET:
#' #' #' - migration:
#' #' #'   1 = Sedentary.
#' #' #'   2 = Partially migratory - minority of population migrates long distances,
#' #' #'   or most of population undergoes short-distance migration,
#' #' #'   nomadic movements, distinct altitudinal migration, etc.
#' #' #'   3 = Migratory - majority of population undertakes long-distance migration
#' #' avonet <- setDT(readxl::read_xlsx(
#' #'   file.path(here::here("Published_data", "AVONET1_Birdlife.xlsx")),
#' #'   sheet = 2))[Species1 %in% target_sp][
#' #'     , .(species = Species1, migration = Migration)]
#' #' # ELTON
#' #' # nocturnal or not, Nycticorax nycticorax seemed erroneous
#' #' 
#' #' 
#' #' lapply(seq_along(sleep_files), function(n){
#' #' 
#' #'   # print(paste("started:", n))
#' #' 
#' #'   fin <- sleep_files[n]
#' #'   
#' #' 
#' #' 
#' #'   cat(paste("\nProcessed:", n, "|", n_files))
#' #' 
#' #'   
#' #' })
#' #' 
#' #' fin <- sleep_files[10]
#' #' 
#' #' sp <- gsub("_", " ", gsub(".*/Studies/([^/]+)/7_sleep_clusters/.*", "\\1", fin))
#' #' 
#' #' slp <- fread(fin)[
#' #'   , deploy_id := paste0("dpl_", .GRP), by = file][
#' #'   , revisit_day_cycle := fifelse(
#' #'     is.na(revisit_day_cycle), 0, revisit_day_cycle)][
#' #'   , file := NULL]
#' #' setorder(slp, deploy_id, day_period, t_median)
#' #' 
#' #' 
#' #' 
#' #' if(traits[species == sp, nocturnal] == 1){ 
#' #'   slp <- slp[day_period == "day"] } else{ slp <- slp[day_period == "night"] }
#' #' 
#' #' trial <- slp[
#' #'   , tracking_gap := {
#' #'     
#' #'     if(revisit_day_cycle > 1) {
#' #'       # define days between previous visit and this visit
#' #'       gap_days <- (day_cycle - revisit_day_cycle + 1):(day_cycle)
#' #'       # check how many tracking days there are in this period
#' #'       track_days <- slp[
#' #'         deploy_id == .BY$deploy_id & day_cycle %in% gap_days, day_cycle]
#' #'       length(gap_days) - length(track_days)
#' #'     } else { 0 }
#' #'     
#' #'   }, by = .(deploy_id, day_cycle) ]
#' #' 
#' #' # We'll make a helper function:
#' #' check_gaps <- function(dt) {
#' #'   dt[, has_data_gap := {
#' #'     if(revisit_day_cycle > 1) {
#' #'       # Define days between previous visit and this visit (exclusive)
#' #'       gap_days <- (day_cycle - revisit_day_cycle + 1):(day_cycle - 1)
#' #'       # Check if there are any records for this animal/sleep_cluster in the gap
#' #'       any_data <- DT[
#' #'         animal_id == .BY$animal_id & sleep_cluster == .BY$sleep_cluster & day_cycle %in% gap_days
#' #'       ]
#' #'       # If no data in the gap, TRUE (there is a gap), else FALSE
#' #'       !nrow(any_data)
#' #'     } else {
#' #'       NA  # Not relevant for revisit_day_cycle <= 1
#' #'     }
#' #'   }, by = .(animal_id, sleep_cluster, day_cycle)]
#' #' }
#' #' 
#' #' # Run the function (modify to your column names as needed)
#' #' check_gaps(DT)
#' #' 
#' #' check <- slp[deploy_id == "dpl_1" & sleep_cluster == 12]
#' #' 
#' #' spoints <- st_as_sf(check, coords = c("x_median", "y_median"), crs = crs, remove = F)
#' #' 
#' #' library(ggplot2)
#' #' 
#' #' ggplot(spoints) + 
#' #'   geom_sf(aes(color = as.factor(sleep_cluster), size = 2, alpha = 0.5))
#' #' 
#' #' 
#' #'                