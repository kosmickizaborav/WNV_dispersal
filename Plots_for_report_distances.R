library(data.table)
library(ggplot2)
source("6_distance_PLOT_FUNCTIONS.R")
source("0_helper_functions.R")
source("color_palettes.R")

data_dir <- here::here("Data")
study_dir <- file.path(data_dir, "Studies")
graphs_dir <- file.path(data_dir, "Graphs")


dist_col <- c(
  "<1" = "#ffee88", 
  "[1, 10)" = "#73d2de", 
  "[10, 50)" = "#218380", 
  "[50, 1000)" = "#D17D98", 
  ">100" = "#8f2d56")



# set order of the species ------------------------------------------------

f_types <- c(
  "4_all_tracks_max_active_steps_nauticalDawn_nauticalDusk_continent.rds", 
  "2_all_tracks_dcp_sleep_steps_nauticalDawn_nauticalDusk_continent.rds", 
  "3_all_tracks_median_active_steps_nauticalDawn_nauticalDusk_continent.rds"
)

n_file_filter = 30 
n_yd_filter = 10

dist_dirs <- file.path(list.files(study_dir, full.names = T), "6_distances")
files <- list.files(
  dist_dirs, paste0(f_types, collapse = "|"), full.names = T)

# get species order
sp_order_all <- rbindlist(lapply(files, function(fin){
  
  dt <- fread(fin)[
    , n_track := .N, by = file][n_track > n_file_filter][
      , yd := day_cycle_to_yd(day_cycle_1)][
        , n_yd := .N, by = yd][n_yd > n_yd_filter]
  
  if(nrow(dt) == 0) return(NULL)
  
  dt[, .(n_tracks = uniqueN(file))][
    , species := gsub(".*Studies/(.*)_(.*)/6_distances.*", "\\1 \\2", fin)]
  
}))

sp_order_all <- sp_order_all[, .SD[which.max(n_tracks)], by = species]
sp_order_all <- add_birdlife_phylogeny(sp_order_all, species_name = "species")

sp_order_all[, ord_max := max(n_tracks), by = .(order)]
setorder(sp_order_all, ord_max, order, n_tracks, species)
sp_order_all[, sp_id := .I]

sp_order_all <- sp_order_all[, .(species, sp_id, order)]

# PLOT FUNCTION -----------------------------------------------------------

plot_distances <- function(
    file_type = NULL, 
    study_dir = here::here("Data", "Studies"),
    sum_fun = median,
    n_file_filter = 30, 
    n_yd_filter = 10, 
    days_space = 366,
    order_space = 90,
    wdth = 40,
    dist_type = "Median distance between roost positions"
    ){
  
  dist_dirs <- file.path(list.files(study_dir, full.names = T), "6_distances")
  files <- list.files(dist_dirs, file_type, full.names = T)
  
  # get species order
  sp_order <- rbindlist(lapply(files, function(fin){
    
    dt <- fread(fin)[
      , n_track := .N, by = file][n_track > n_file_filter][
        , yd := day_cycle_to_yd(day_cycle_1)][
          , n_yd := .N, by = yd][n_yd > n_yd_filter]
    
    if(nrow(dt) == 0) return(NULL)
    
    dt[
      , .(
        n_tracks = uniqueN(file), 
        n_tracks_eu = ifelse(
          any(continent== "Europe"), 
          uniqueN(.SD[continent == "Europe", file]), NA))][
            , species := gsub(".*Studies/(.*)_(.*)/6_distances.*", "\\1 \\2", fin)]
    
  }))
  
  sp_order[, n_track_log := days_space+order_space+wdth*log10(n_tracks)]
  sp_order[, n_track_eu_log := days_space+order_space+wdth*log10(n_tracks_eu)]
  
  sp_order <- merge(
    sp_order, sp_order_all, by = "species")
  
  order_dt <- sp_order[, .(
    sp_max = max(sp_id) + 0.5, 
    sp_min = min(sp_id) - 0.5, 
    sp_med = as.numeric(median(sp_id)), 
    n_order = .N, 
    xmin = days_space, 
    xmax = days_space+order_space, 
    xmed = days_space + order_space/2
  ), by = .(order)]
  order_dt[, lab := ifelse(n_order > 2, order, "")]
  
  
  dist_dt <- rbindlist(lapply(files, function(fin){
    spc <- gsub(".*Studies/(.*)_(.*)/6_distances.*", "\\1 \\2", fin)
    
    fread(fin)[
      , n_track := .N, by = file][n_track > n_file_filter][
        , yd := day_cycle_to_yd(day_cycle_1)][ # 
          , .(
            med_dist = as.numeric(sum_fun(sl_)), 
            n_dist = .N, 
            n_files = uniqueN(file)), 
          by = yd][, species := spc]
    
  }))[n_dist > n_yd_filter]
  
  
  dist_dt[, dist_cat := cut(
    med_dist, 
    breaks = c(0, 1000, 10000, 50000, 100000, Inf), 
    labels = c("<1", "[1, 10)", "[10, 50)", "[50, 1000)", ">100"), 
    include.lowest = T, right = F)]
  
  dist_dt <- merge(dist_dt, sp_order[, .(species, sp_id)], by = "species")
  
  sp_max <- max(dist_dt$sp_id) + 0.5
  yminmin <- -4
  
  month_dt <- get_month_limits()
  
  month_box <- data.table(
    xmin = min(month_dt$first_yd), 
    xmax = max(month_dt$last_yd), 
    ymin = yminmin, 
    ymax = sp_max
  )[, xmed := (xmin+xmax)/2]
  
  dep_bb <- seq(0, floor(max(log10(sp_order$n_tracks), 1)))
  
  count_box <- data.table(
    xmin = days_space + order_space, 
    xmax = max(sp_order$n_track_log, na.rm = T), 
    ymin = 0.5, 
    ymax = sp_max, 
    vline = days_space+order_space+wdth*dep_bb, 
    lab = 10^dep_bb
  )
  
  lab_box <- data.table(
    xmin = c(1, days_space, days_space + order_space),
    xmax = c(days_space, days_space + order_space, max(sp_order$n_track_log)),
    ymin = sp_max, 
    ymax = sp_max + 6, 
    ymed = sp_max + 3,
    lab = c(
      sprintf("%s per calendar day and species", dist_type),  
      "order", 
      "number of tracks\n[log scale]\ndotted line = in Europe")
  )[, xmed := (xmin+xmax)/2]
  
  # move_dt <- sp_order[, .(
  #   ymax = max(sp_id) + 0.5, 
  #   ymin = min(sp_id) - 0.5, 
  #   ymed = as.numeric(median(sp_id)), 
  #   xmax = 0.5, 
  #   xmin = -1, 
  #   xmed = -0.25), 
  #   by = .(migration)]
  
  
  
  ggplot() +
    geom_rect(
      data = month_box,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "white", color = "gray22",  linewidth = 0.5
    ) +
    geom_tile(
      data = dist_dt, aes(x = yd, y = sp_id, fill = dist_cat), alpha = 0.8
    ) +
    geom_segment(
      data = month_dt, 
      aes(x = first_yd, xend = first_yd, y = month_box$ymin, yend = sp_max), 
      color = "gray22", linewidth = 0.2
    ) +
    geom_text(data = month_dt, aes(y = yminmin/2, x = mid_yd, label = month)) +
    geom_segment(
      data = count_box, 
      aes(x = vline, xend = vline, y = ymin, yend = ymax), 
      color = "gray22", linewidth = 0.3
    ) +
    geom_rect(
      data = unique(count_box[, .(xmin, xmax, ymin, ymax)]),
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = NA, color = "gray22", linewidth = 0.5
    ) +
    geom_rect(
      data = sp_order,
      aes(
        ymin = sp_id-0.5, ymax = sp_id+0.5, 
        xmin = days_space + order_space, xmax = n_track_log, fill = order), 
      alpha = 0.6
    ) + 
    geom_segment(
      data = sp_order,
      aes(
        x = days_space + order_space, xend = n_track_eu_log, 
        y = sp_id, yend = sp_id), 
      color = "white", linewidth = 1, linetype = "dotted"
    ) +
    geom_text(
      data = count_box, 
      aes(x = vline, y = yminmin/2, label = lab), color = "gray22", size = 3
    ) +
    geom_rect(
      data = lab_box, 
      aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax), 
      fill = "white", color = "gray22", linewidth = 0.5
    ) +
    geom_text(
      data = lab_box, 
      aes(y = ymed, x = xmed, label = lab), 
      color = "gray22", size = 3, fontface = "bold"
    ) +
    geom_rect(
      data = order_dt, 
      aes(ymin = sp_min, ymax = sp_max, xmin = xmin, xmax = xmax, fill = order), 
      alpha = 0.4, color = "gray22"
    ) +
    geom_text(
      data = order_dt, 
      aes(y = sp_med, x = xmed, label = lab), color = "gray22"
    ) +
    scale_fill_manual(
      values = c(ord_col, dist_col), 
      breaks = names(dist_col)
    ) +
    scale_y_continuous(
      breaks = sp_order$sp_id, 
      labels = sp_order$species,
      expand = c(0, 0)
    ) +
    scale_x_continuous(
      breaks = month_dt$first_yd, 
      expand = c(0, 0)
    ) +
    labs(
      x = "", 
      y = "species", 
      fill = "daily distance [km]", 
      title = sprintf("%s throughout a year", dist_type)
    ) +
    theme_bw() +
    theme(
      axis.ticks = element_blank(), 
      axis.text.y = element_text(family = "FreeSans", size = 6),
      panel.border = element_blank(),
      axis.title.x = element_text(hjust = 0), 
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.5, "lines"),
      legend.position = "bottom",
      legend.direction = "horizontal", 
      plot.title = element_text(hjust = 0, size = 16, face = "bold")
    ) 
}




f_types <- c(
  "4_all_tracks_max_active_steps_nauticalDawn_nauticalDusk_continent.rds", 
  "2_all_tracks_dcp_sleep_steps_nauticalDawn_nauticalDusk_continent.rds", 
  "3_all_tracks_median_active_steps_nauticalDawn_nauticalDusk_continent.rds"
)

lapply(f_types, function(ftp){
  
  if(grepl("sleep", ftp)){ 
    dstt <- "distance between roost positions" 
    } else {
    dstt <- ifelse(
      grepl("max", ftp), "daily distance", "distance to median daily position")
    }
  
  plot_distances(
    file_type = ftp, sum_fun = min, dist_type = paste("Min", dstt))
  
  pname <- sprintf(
    "6_min_%s_nauticalDawn_nauticalDusk.png", gsub(" ", "_", dstt))
  
  ggsave(
    file.path(graphs_dir, pname),  width = 25, height = 30, units = "cm")
  
  plot_distances(
    file_type = ftp, sum_fun = median, dist_type = paste("Median", dstt))
  
  pname <- sprintf(
    "6_median_%s_nauticalDawn_nauticalDusk.png", gsub(" ", "_", dstt))
  
  ggsave(
    file.path(graphs_dir, pname),  width = 25, height = 30, units = "cm")
  
  plot_distances(
    file_type = ftp, sum_fun = mean, dist_type = paste("Mean", dstt))
  
  pname <- sprintf(
    "6_mean_%s_nauticalDawn_nauticalDusk.png", gsub(" ", "_", dstt))
  
  ggsave(
    file.path(graphs_dir, pname),  width = 25, height =30, units = "cm")
  
  plot_distances(
    file_type = ftp, sum_fun = max, dist_type = paste("Max", dstt))
  
  pname <- sprintf(
    "6_max_%s_nauticalDawn_nauticalDusk.png", gsub(" ", "_", dstt))
  
  ggsave(
    file.path(graphs_dir, pname),  width = 25, height = 30, units = "cm")
  
})






