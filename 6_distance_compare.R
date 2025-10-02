library(data.table)
library(ggplot2)
library(patchwork)
source("color_palettes.R")
source("0_helper_functions.R")
source("6_distance_PLOT_FUNCTIONS.R")

data_dir <- here::here("Data")
study_dir <- file.path(data_dir, "Studies")
graphs_dir <- file.path(data_dir, "Graphs")

dist_dirs <- file.path(list.files(study_dir, full.names = T), "6_distances")
target_sp <- gsub(".*/Studies/(.*?)_(.*?)/6_distances.*", "\\1 \\2", dist_dirs)

traits_dt <- fread(here::here("Published_data", "00_bird_traits.csv"))
traits_dt <- unique(traits_dt[
  , .(species = birdlife_name, migration = migration_txt, order)])
traits_dt <- traits_dt[species %in% target_sp]


fmed <- "3_all_tracks_median_active_steps_nauticalDawn_nauticalDusk_continent.rds"

fmax <- "4_all_tracks_max_active_steps_nauticalDawn_nauticalDusk_continent.rds"


files <- list.files(dist_dirs, pattern = fmax, full.names = T)
n_fltr <- 30



# 1 - Differences in daily distances --------------------------------------


# #mlvl <- c("sedentary", "partial", "migrant")
# mlbl <- c("sedentary", "partial migrant", "migrant")
# move_col <- c("#A3DA8D", "#F4A460", "#781D42")
# names(move_col) <- mlbl

# species_dt <- rbindlist(lapply(seq_along(files), function(n){
#   
#   fin <- files[n]
#   sp <- gsub(".*/Studies/(.*?)_(.*?)/6_distances.*", "\\1 \\2", fin)
#   
#   dt <- fread(fin)
#   
#   dt[, sl_median := fifelse(
#     sl_n_steps == 1, as.numeric(sl_), as.numeric(sl_median))]
#   
#   dt <- dt[, .(file, day_cycle_1, sl_max = sl_, sl_median_max = sl_median)]
#   
#   # filter tracks that have less than 10 steps
#   dt <- dt[, n_steps := .N, by = file][n_steps >= n_fltr]
#   
#   if(nrow(dt) == 0){ return(NULL) }
#   
#   dt_median <- fread(gsub(fmax, fmed, fin))[
#     , .(file, day_cycle_1, sl_median = sl_)]
#   
#   
#   dt <- merge(dt, dt_median, by = c("file", "day_cycle_1"))
#   
#   dt[, diff_sl_max := abs(sl_max - sl_median_max)]
#   dt[, diff_sl_median := abs(sl_median_max - sl_median)]
#   
#   # extract sensor
#   dt[, sensor := sub(".*dep_(.*?)_sen.*", "\\1", file)][
#     , sensor := fcase(sensor == 653, "gps", sensor == 2299894820, "sigfox")]
#   
#   n_deploy_tot <- dt[, uniqueN(file)]
#   
#   # get track lengths and sensor
#   dt_per_file <- dt[, .(
#     n_days = uniqueN(day_cycle_1),
#     sensor = unique(sensor)
#   ), by = file]
#   
#   # remove duplicated trakcs - deployments with both gps and sigfox
#   dt_per_file[, track_id := sub("_dep_\\d+.*$", "", basename(file))]
#   
#   setorder(dt_per_file, sensor)
#   
#   dt_per_file[, duplicated_track := duplicated(track_id)]
#   dpltrks <- dt_per_file[duplicated_track == T, file]
#   
#   # select duplicated deployments
#   dt_per_file <- dt_per_file[duplicated_track == F]
#   
#   dt <- dt[!file %in% dpltrks]
#   
#   dt_summary <- dt[, .(
#     diff_sl_max_25 = round(quantile(diff_sl_max, 0.25, na.rm = T)/1000, 2),
#     diff_sl_max_50 = round(median(diff_sl_max, na.rm = T)/1000, 2),
#     diff_sl_max_75 = round(quantile(diff_sl_max, 0.75, na.rm = T)/1000, 2), 
#     diff_sl_median_25 = round(quantile(diff_sl_median, 0.25, na.rm = T)/1000, 2), 
#     diff_sl_median_50 = round(median(diff_sl_median, na.rm = T)/1000, 2), 
#     diff_sl_median_75 = round(quantile(diff_sl_median, 0.75, na.rm = T)/1000, 2)
#   )]
#   
#   dt_summary[, species := sp]
#   
# }), fill = T)
# 
# 
# # add traits and phylogeny
# species_dt <- merge(species_dt, traits_dt, by = "species", all.x = T)
#   
# species_dt[, migration := fcase(
#   # migration == "altitudinal migrant", "altitude migrant",
#   migration == "partially migratory", "partial migrant",
#   migration %in% c("migratory", "full migrant"), "migrant",
#   migration == "not a migrant", "sedentary",
#   default = migration)]
# species_dt[
#   , migration := factor(migration, levels = mlbl)]
# 
# # set order of 
# species_dt[, sl_order := max(diff_sl_max_50), by = order]
# 
# 
# # set order and assign a number to the species name
# setorder(species_dt, migration, sl_order, order, diff_sl_max_50)
# species_dt[, sp_id := .I]
# 
# order_dt <- species_dt[, .(
#   ymax = max(sp_id), 
#   ymin = min(sp_id), 
#   ymed = as.numeric(median(sp_id)), 
#   count = max(sp_id) - min(sp_id) + 1), 
#   by = .(migration, order)]
# order_dt[, lab := ifelse(count > 2, order, "")]
# 
# move_dt <- species_dt[, .(
#   ymax = max(sp_id), 
#   ymin = min(sp_id), 
#   ymed = median(sp_id)), 
#   by = migration]
# 
# max75 <- max(species_dt$diff_sl_median_75)
# 
# if(max75 < 100) {
#   
#   pscl <- c(-2, -8, -30)
#   
# } else{ 
#   
#   pscl <- c(-max75*0.02, -max75*0.08, -max75*0.33)
#   
# }
# # 
# # subtit <- ifelse(
# #   grepl("median", fin), "median daily distance", "maximum daily distance")
# # 
# # subtit <- ifelse(
# #   grepl("median_active", fin), 
# #   paste(subtit, "- median sleep to median active locations"), 
# #   paste(subtit, "- median sleep to all active locations")
# # )
# 
# species_dt |> 
#   ggplot() +
#   geom_point(
#     aes(y = sp_id, x = diff_sl_median_50, color = migration), size = 3, shape = 15,
#     alpha = 0.6
#   ) +
#   geom_errorbar(
#     aes(y = sp_id, xmin = diff_sl_median_25, xmax = diff_sl_median_75, color = migration), 
#     linewidth = 1.2, alpha = 0.6
#   ) +
#   geom_rect(
#     data = move_dt,
#     aes(xmin = pscl[2], xmax = pscl[1], ymin = ymin - 0.5, ymax = ymax + 0.5, 
#         fill = migration), alpha = 0.6, color = "gray33"
#   ) +
#   geom_rect(
#     data = order_dt,
#     aes(xmin = pscl[3], xmax = pscl[2], ymin = ymin - 0.5, ymax = ymax + 0.5, 
#         fill = order),  color = "gray33",
#     alpha = 0.4
#   ) +
#   geom_text(
#     data = order_dt, 
#     aes(y = ymed, x = pscl[2]+(pscl[3]-pscl[2])/2, label = lab), 
#     vjust = 0.5, hjust = 0.5, 
#     color = "gray33"
#   ) +
#   scale_y_continuous(
#     breaks = species_dt$sp_id, 
#     labels = species_dt$species, 
#     expand = c(0, 0)
#   ) +
#   scale_x_continuous(expand = c(0,0)) +
#   # scale_x_continuous(
#   #   expand = c(0, 0),
#   #   limits = c(pscl[3], max75+10),
#   #   breaks = c(
#   #     seq(0, 100, 10), if(max75 > 100) seq(100, max75+10, 20)),
#   #   guide = guide_axis(check.overlap = T)
#   # ) +
#   theme_bw() +
#   scale_fill_manual(values = c(ord_col, move_col)) +
#   scale_color_manual(values = move_col, labels = mlbl) +
#   labs(
#     x = "distance difference [km]",
#     title = "Difference between median distance and distance between medians",
#     subtitle = "median value across species with [Q1-Q3] error bars"
#   ) +
#   theme(
#     axis.title.y = element_blank(), 
#     axis.ticks.y = element_blank(),
#     legend.position = "none"
#   ) +
#   geom_text(
#     data = move_dt, 
#     aes(y = ymed, x = pscl[1]+(pscl[2]-pscl[1])/2, label = migration), 
#     angle = 90, hjust = 0.5, vjust = 0.5, color = "gray11"
#   )
# 
# pname <- "6_difference_sl_median_and_median_by_species_fltr30.png"
# 
# ggsave(
#   file.path(graphs_dir, pname), 
#   width = 12, height = 16, dpi = 300
# )
# 
# 


# daily speed -------------------------------------------------------------

month_dt <- get_month_limits()

pout_dir <- file.path(graphs_dir, "6_speed_distributions")
dir.create(pout_dir, showWarnings = F)

sp_limits <- fread(file.path(data_dir, "3_speed_limits.csv"))

sp_max_dt <- rbindlist(lapply(seq_along(files), function(n){
  
  fin <- files[n]
  
  sp <- gsub(".*/Studies/(.*?)_(.*?)/6_distances.*", "\\1 \\2", fin)
  
  sp_lim <- sp_limits[birdlife_name == sp, speed_limit]
  
  dt <- fread(fin)
  
  
  dt <- dt[, .(file, day_cycle_1, sl_, x1_, y1_, t1_, x2_, y2_, t2_)]
  
  dt[, t_diff := difftime(t2_, t1_, unit = "sec")]
  
  dt[, speed := sl_ / as.numeric(t_diff)]
  
  dt[, yd := day_cycle_to_yd(day_cycle_1)]

  dt[, above_limit := speed > sp_lim]
  
  dt |> 
    ggplot() +
    geom_vline(
      data = month_dt, aes(xintercept = last_yd), color = "gray33"
    ) +
    geom_text(
      data = month_dt, 
      aes(x = mid_yd, y = 0, label = month), 
      vjust = 1, color = "gray33"
    ) +
    geom_point(
      aes(
        y = speed, x = yd, color = above_limit), 
      alpha = 0.5, position = "jitter") +
    scale_x_continuous(
      expand = c(0, 0)
    ) +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(
      x = "day of the year", 
      y = "speed [m/s]",
      colour = "above theoretical limit", 
      title = sprintf("%s - unfiltered speed distribution", sp)
    )
    
  
  ggsave(file.path(pout_dir, paste0(sp, ".png")), 
         width = 7, height = 7, dpi = 300)
  
  data.table( 
    speed_max = max(dt$speed, na.rm = T),
    speed_95 = quantile(dt$speed, 0.95, na.rm = T),
    speed_75 = quantile(dt$speed, 0.75, na.rm = T),
    species = sp, 
    speed_limit = sp_lim
  )
  
  
}))


# Diomedea_antipodensis
