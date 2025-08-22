library(data.table)
library(geosphere)
library(zoo)
library(ggplot2)
library(EMbC)
library(sf)
source("0_helper_functions.R")
source("6_distance_PLOT_FUNCTIONS.R")

crs <- sf::st_crs(4326)

data_dir <- here::here("Data")
study_dir <- file.path(data_dir, "Studies")
graphs_dir <- file.path(data_dir, "Graphs")

filtered <- fread(file.path(data_dir, 
  "6_overview_filter_30_max_active_steps_nauticalDawn_nauticalDusk_continent.csv"))
target_sp <- unique(filtered$species)

nocturnal_dt <- fread(here::here("Published_data", "00_bird_traits.csv"))[
  birdlife_name %in% target_sp]
# there were duplicates because of some synonyms
nocturnal_dt <- unique(nocturnal_dt[, .(birdlife_name, nocturnal)])


finn <- "1_all_tracks_dcp_distances_nauticalDawn_nauticalDusk_continent.rds"
# find <- "1_all_tracks_dcp_distances_dawn_dusk_continent.rds"
#fins <- "2_all_tracks_dcp_sleep_steps_nauticalDawn_nauticalDusk_continent.rds"
#find <- "4_all_tracks_max_active_steps_nauticalDawn_nauticalDusk_continent.rds"


#bc_labs <- data.table(value = 1:4, name = c("LL", "LH", "HL", "HH"))

#cat_names <- c("LLL", "LLH", "LHL", "LHH", "HLL", "HLH", "HHL", "HHH")
#full_dt[, embc_cls_lab := cat_names[embc_cls]]


plots_dir <- file.path(graphs_dir, "8_detecting_migration")
dir.create(plots_dir, showWarnings = F)

min_days <- 2
wdw <- 7



library(zoo)





# 0 - Load packages -------------------------------------------------------

library(data.table)
library(ggplot2)
library(patchwork)
#library(parallel) - never used it in the end
#load the functions for distance calculations
source("6_distance_FUNCTIONS.R")
source("6_distance_PLOT_FUNCTIONS.R")
source("0_helper_functions.R")

# ALL OUTPUT DIRECTORIES
data_dir <- here::here("Data")
study_dir <- file.path(data_dir, "Studies")
graph_dir <- file.path(data_dir, "Graphs")

# OUTPUT DIRECTORIES FOR DATA
dist_dirs <- file.path(list.files(study_dir, full.names = T), "6_distances")
# make output directories
invisible(lapply(dist_dirs, dir.create, showWarnings = FALSE))

# OUTPUT DIRECTORIES FOR PLOTS
plots_dir <- file.path(graph_dir, "6_distances")
dir.create(plots_dir, showWarnings = FALSE)

# how do we define day and night (day limits)
day_limits <- list(
  c("nightEnd", "night"), 
  c("nauticalDawn", "nauticalDusk"),
  c("dawn", "dusk")
)

# distinguishing between world and europe for plots
regions <- c("World", "Europe")

# Create folder names from day_limits
dl_folders <- sapply(day_limits, function(x) paste(x, collapse="_"))

# Create all combinations
all_plots_dirs <- file.path(
  plots_dir, rep(regions, each=length(dl_folders)), dl_folders)
# Create the directories (including parents)
invisible(sapply(all_plots_dirs, dir.create, recursive = T, showWarnings = F))


# GET WHETHER THE ANIMAL IS NOCTURNAL OR DIURNAL FOR THE SLEEP SPOTS
target_sp <- gsub("_", " ", list.files(study_dir))
nsp <- length(target_sp)

nocturnal_dt <- fread(here::here("Published_data", "00_bird_traits.csv"))[
  birdlife_name %in% target_sp]
# there were duplicates because of some synonyms
nocturnal_dt <- unique(nocturnal_dt[, .(birdlife_name, nocturnal)])


# 1 - DCP distances -------------------------------------------------------

lapply(seq_along(target_sp), function(n){

  sp <- target_sp[n]
  # output directory
  out_dir <- grep(gsub(" ", "_", sp), dist_dirs, value = T)
  
  # get available tracks for the species
  sp_files <- list.files(
    gsub("6_distances", "5_flag_static", out_dir), full.names = T)
  
  if(length(sp_files) == 0){ return(NULL) }
  
  lapply(day_limits, function(dl){

    # OUTPUT file with day-cycle-period (DCP) distances
    fout <- paste0(
      "1_all_tracks_dcp_distances_", dl[1], "_", dl[2], ".rds")

    if(!file.exists(file.path(out_dir, fout))){

      dcp_dist_all <- rbindlist(lapply(seq_along(sp_files), function(i){

        # load the track
        fin <- sp_files[i]
        track <- fread(fin)

        # calculate DCP distances and median DCP positions
        dcp_dist <- get_dcp_dist(track, day_limits = dl)[, file := fin]

        return(dcp_dist)

      }), fill = T)

      # save data if exists
      fwrite(dcp_dist_all, file.path(out_dir, fout)) 
      
      rm(dcp_dist_all)
      
      cat("\n Species DCP distances done:", n, "|", nsp)

    }
    
    return(NULL)
    
  })

})


dcp_files <- list.files(
  dist_dirs, "1_all_tracks_dcp_distances", full.names = T)
dcp_files <- grep("continent", dcp_files, value = T, invert = T)








lapply(seq_along(target_sp), function(i) {
  
  sp <- target_sp[i]
  
  cat(i, sp, "\n")
 
  sp_dir <- file.path(study_dir, gsub(" ", "_", sp), "6_distances")
  
  dcp_dt <- fread(file.path(sp_dir, finn))[
    , .(file, x_ = x_median, y_ = y_median, t_ = t_median, day_cycle, day_period)]
  setorder(dcp_dt, file, t_)
  
  dcp_dt <- dcp_dt[, n_steps := .N, by = file][n_steps >= 30]
  
  # stype <- ifelse(
  #   nocturnal_dt[birdlife_name == sp, nocturnal] == 1, "day", "night")
  # 
  # dcp_dt <- dcp_dt[day_period == stype]
  
  dcp_dt[
    , time_gap := c(0, as.numeric(diff(t_), units = "days")), by = file]
  dcp_dt[
    , weekly_bursts := cumsum(time_gap > min_days), by = file]
  
  dcp_dt[
    , geometry := sf::st_as_sf(.SD, coords = c("x_", "y_"), crs = crs), 
    .SDcols = c("x_", "y_")]
  
  dcp_dt[, ':=' (
    sl_med_loc = c(NA, as.numeric(sf::st_distance(
      geometry[-.N], geometry[-1], by_element = TRUE))), 
    direction = bearing(p1 = cbind(shift(x_), shift(y_)), p2 = cbind(x_, y_))
    ), by = .(file, weekly_bursts)]
 dcp_dt[, direction := abs(direction)]
  
  dcp_dt[, to_n := direction <= 60]
  dcp_dt[, to_s := direction >= 130]
  
  dcp_dt[, c("y_medb", "y_meda", "sl_sum", "to_ns", "to_ss") := .(
    # frollsum(sl_med_loc*to_n, n = wdw, align = "center"),
    # frollsum(sl_med_loc*to_s, n = wdw, align = "center"),
    frollmean(y_, n = wdw, align = "left"),
    frollmean(y_, n = wdw, align = "right"),
    frollsum(sl_med_loc, n = wdw, align = "left"),
    frollsum(to_n, n = wdw, align = "left"),
    frollsum(to_s, n = wdw, align = "left")
  ), by = .(file, weekly_bursts)]
  
  dcp_dt[, direction_tendency := abs(to_ns - to_ss)/wdw]
  dcp_dt[, y_mid_diff := abs(abs(y_meda) - abs(y_medb))]
  #dcp_dt[, sl_sum_direction := abs(sl_north - sl_south)/sl_sum]



  #  robust_scale <- function(x) {
  #    (x - median(x, na.rm = T)) / IQR(x, na.rm = T)
  #  }
  # 
  # dcp_dt[, sl_sum_scaled := robust_scale(sl_sum), by = file]
  
  dcp_dt <- dcp_dt[!is.na(direction_tendency) & !is.na(sl_sum)]
  
  embc_clst <- embc(
    as.matrix(dcp_dt[, .(direction_tendency, sl_sum)]),
    maxItr = 1000)
  embc_clst <- smth(embc_clst, dlta = 1)
  
  dcp_dt[, embc_clst := embc_clst@A]
  
  cat_names <- c("LL", "LH", "HL", "HH")
  #cat_names <- c("LLL", "LLH", "LHL", "LHH", "HLL", "HLH", "HHL", "HHH")
  
  dcp_dt[, embc_clst_lab := cat_names[embc_clst]]
  
  
  dcp_dt[, month := month(as.Date(day_cycle))]
  
  sf::st_as_sf(
    dcp_dt, 
    coords = c("x_", "y_"), 
    crs = 4326
  ) |> 
    ggplot() + 
    geom_sf(aes(color = embc_clst_lab), alpha = 0.5) +
    theme_bw() +
    scale_color_manual(
      values = c(
        "LL" = "#0072B2", "LH" = "lightblue", 
        "HL" = "#E69F00", "HH" = "#D55E00"),
      name = "EMbC Clusters"
    ) +
    labs(title = sp) +
    facet_wrap(~month, ncol = 4) +
    theme(legend.position = "bottom") 
  
  
  ggsave(
    filename = file.path(
      plots_dir, paste0(gsub(" ", "_", sp), "_migration_clusters.png")
    ),
    width = 10, height = 6, dpi = 300
  )

})

# scale_color_manual(
#   values = c(
#     "LLL" = "#0072B2", "LLH" = "lightblue", 
#     "LHL" = "purple", "LHH" = "pink", 
#     "HLL" = "gray64", "HLH" = "gray78",
#     "HHL" = "#E69F00", "HHH" = "#D55E00"),
#   name = "EMbC Clusters"
# ) +
 

 
# sf::st_as_sf(
#       full_dt, 
#       coords = c("x1_", "y1_"), 
#       crs = 4326
#     ) |> 
#     ggplot() + 
#     geom_sf(aes(color = as.factor(embc_cls_lab)), alpha = 0.5) +
#     theme_bw() +
#     labs(title = sp) +
#     facet_wrap(~month, ncol = 4) +
#     theme(legend.position = "bottom") +
#     scale_color_manual(
#       values = c("LLL" = "gray", "LLH" = "gray23", "LHL" = "gray88", 
#                  "LHH" = "gray74", "HLL" = "gray64", "HLH" = "gray78", 
#                  "HHL" = "blue", "HHH" = "blue"), 
#       name = "EMbC Clusters"
#     ) 
#   
#   full_dt |> 
#     ggplot() + 
#     geom_point(aes(x = sls_wmean, y = sls_med), alpha = 0.5) +
#     #geom_histogram(aes(x = nsd_diff)) +
#     theme_bw()


# 
# 
# active_dt <- fread(file.path(sp_dir, find))[
#   , .(file, day_cycle_1, t1_, x1_, y1_, sl_active = sl_)]
# 
# 
# active_dt <- rbindlist(list(
#   active_dt[]
# ))
# 
# 
# sleep_dt <- fread(file.path(sp_dir, fins))[
#   , .(file, day_cycle_1, t1_, x1_, y1_, sl_sleep = sl_)]
# 
# 
# 
# setnames(sleep_dt, c("x_", "y_", "t_"), c("x1_", "y1_", "t1_"))
# 
# 
# full_dt <- merge(
#   active_dt, sleep_dt, 
#   by = c("file", "day_cycle_1", "t1_", "x1_", "y1_")
# )
# full_dt[, month := month(as.Date(day_cycle_1))]
# 
# # Function to check max gap in each rolling window
# check_gap <- function(days, max_allowed_gap = 2){
#   max(diff(sort(days))) <= max_allowed_gap
# } 
# 
# # # Apply rolling gap check
# # full_dt[, rolla := frollapply(
# #   day_cycle_1, 7, check_gap, align = "left", fill = F), by = file]
# # full_dt[, rollb := frollapply(
# #   day_cycle_1, 7, check_gap, align = "right", fill = F), by = file]
# # 
# # # Calculate rolling means only where ok_for_roll is TRUE; otherwise set NA
# # full_dt[, c("sla_wmean", "sls_wmean", "y1_wa", "y1_wb", "nsd_a", "nsd_b") := .(
# #   fifelse(rolla == 1, frollmean(sl_active, n = 7, align = "left"), NA_real_),
# #   fifelse(rolla == 1, frollmean(sl_sleep, n = 7, align = "left"), NA_real_), 
# #   fifelse(rolla == 1, frollmean(nsd_, n = 7, align = "left"), NA_real_),
# #   fifelse(rollb == 1, frollmean(nsd_, n = 7, align = "right"), NA_real_),
# #   fifelse(rolla == 1, frollmean(y1_, n = 7, align = "left"), NA_real_),
# #   fifelse(rollb == 1, frollmean(y1_, n = 7, align = "right"), NA_real_)
# # ), by = file]
# # 
# 
# 
# full_dt[, c("sla_med", "sls_med", "sla_wmean", "sls_wmean") := .(
#   runmed(sl_active, k = 7), 
#   runmed(sl_sleep, k = 7),  
#   frollmean(sl_active, n = 7, align = "left"),
#   frollmean(sl_sleep, n = 7, align = "left")
# )]
# 
# 
# full_dt[, c("sla_wmean", "sls_wmean", "y1_wa", "y1_wb", "nsd_a", "nsd_b") := .(
#   runmed(sl_active, k = 7), 
#   runmed(sl_sleep, k = 7),  
#   frollmean(sl_active, n = 7, align = "left"),
#   frollmean(sl_sleep, n = 7, align = "left"),
#   frollmean(nsd_, n = 7, align = "left"),
#   frollmean(nsd_, n = 7, align = "right"),
#   frollmean(y1_, n = 7, align = "left"),
#   frollmean(y1_, n = 7, align = "right")
# ), by = file]
# 
# full_dt[, y1_diff := (y1_wb - y1_wa)^2]
# full_dt[, nsd_diff := abs(nsd_a - nsd_b)]
# full_dt <- full_dt[!is.na(y1_diff) & !is.na(nsd_diff) & !is.na(sls_wmean)]
# 
# if (nrow(full_dt) == 0) {
#   message(paste("No data for", sp, "after filtering. Skipping..."))
#   return(NULL)
# }
# 
# 
# 
# full_dt[, embc_cls := embc_clst@A]
# 
# cat_names <- c("LLL", "LLH", "LHL", "LHH", "HLL", "HLH", "HHL", "HHH")
# full_dt[, embc_cls_lab := cat_names[embc_cls]]
# 
# 
# 
