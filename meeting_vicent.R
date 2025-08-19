library(data.table)
library(patchwork)
library(ggplot2)
library(sf)


conti <- tryCatch(
  rnaturalearth::ne_load(
    type = "geography_regions_polys",  scale = "medium", category = "physical"), 
  error = function(e) {
    rnaturalearth::ne_download(
      type = "geography_regions_polys",  scale = "medium", category = "physical")
  }
)


data_dir <- here::here("Data")
study_dir <- file.path(data_dir, "Studies")
graphs_dir <- file.path(data_dir, "Graphs", "Vicent")
dir.create(graphs_dir, showWarnings = F)

dist_dirs <- file.path(list.files(study_dir, full.names = T), "6_distances")

finm <- "4_all_tracks_max_active_steps_nauticalDawn_nauticalDusk_continent.rds"

finf <- list.files(
  data_dir, "6_overview_filter_30_max.*_continent.csv", full.names = T)

filtered <- fread(finf)


species <- c(
  "Larus melanocephalus", 
  "Anas platyrhynchos",
  "Ardea alba", 
  "Circus aeruginosus", 
  "Corvus corone", 
  "Columba livia"
  )



# 1 - Distances -----------------------------------------------------------



lapply(seq_along(species), function(i){
  
  sp <- species[i]
  
  sp_dir <- grep(gsub(" ", "_", sp), dist_dirs, value = T)
  
  sl_dt <- fread(file.path(sp_dir, finm))
  
  # filter tracks that have less than 10 steps
  sl_dt <- sl_dt[, n_steps := .N, by = file][n_steps >= 30]
  sl_dt[, sl_ := sl_/1000] # convert to kilometers
  
  pn <- sl_dt |> 
    ggplot() +
    geom_histogram(
      aes(x = sl_), bins = 100, fill = "#A3DA8D", color = "gray44", alpha = 0.6
    ) + 
    theme_bw() +
    labs(
      x = "Daily distance [km]",
      y = "Count"
    ) 
  
  pl <- sl_dt |> 
    ggplot() +
    geom_histogram(
      aes(x = sl_), bins = 100, fill = "#A3DA8D", color = "gray44", alpha = 0.6
    ) + 
    theme_bw() +
    labs(
      x = "Daily distance [km] \n - logharitmic scale -",
      y = ""
    ) + 
    scale_x_log10()
  
  pn + pl + 
    plot_annotation(title = sprintf("%s - daily distances", sp)) &
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
    )
  
  
  ggsave(
    file.path(graphs_dir, sprintf("%s_daily_distances.png", gsub(" ", "_", sp))), 
    width = 10)
  
  sps <- rbindlist(list(
    sl_dt[, .(file, x_ = x1_, y_ = y1_)][, day_period := "night"],
    sl_dt[, .(file, x_ = x2_, y_ = y2_)][, day_period := "day"]
  ))
  
  sps <- st_as_sf(sps, coords = c("x_", "y_"), crs = 4326, remove = F)
  
  sps |> 
    ggplot() +
    geom_sf(data = conti, fill = "grey95", color = "grey80", inherit.aes = FALSE) +
    geom_sf(aes(color = day_period), alpha = 0.2) +
    scale_color_manual(values = c(day = "#F0A202", night = "darkblue")) +
    facet_wrap(~day_period) +
    coord_sf(
      xlim = range(sps$x_), 
      ylim = range(sps$y_)
    ) +
    theme_bw() +
    theme(
      legend.position = "none", 
      plot.title = element_text(face = "bold", hjust = 0.5)
    ) +
    labs(title = sprintf("%s - night and day positions", sp))
  
  
  ggsave(
    file.path(graphs_dir, sprintf("%s_dtrack_map.png", gsub(" ", "_", sp))), 
    width = 16, height = 10)
  
})


# 2 - Sleep clusters ------------------------------------------------------

fsp <- "3_dbscan_clusters_npts_5_distthr_350_nauticalDawn_nauticalDusk.rds"


lapply(seq_along(species), function(i){
  
  sp <- species[i]
  
  fin <- file.path(study_dir, gsub(" ", "_", sp), "7_sleep_clusters", fsp)
  
  cls_dt <- fread(fin)
  
  cls_dt <- cls_dt[, n_steps := .N, by = file][n_steps >= 30]
  
  pn <- cls_dt |> 
    ggplot() +
    geom_histogram(
      aes(x = revisit_day_cycle), 
      bins = 100, fill = "#0F5B78", color = "gray44", alpha = 0.6
    ) + 
    theme_bw() +
    labs(
      x = "Returning times [days]",
      y = "Count"
    ) 
  
  pl <- cls_dt |> 
    ggplot() +
    geom_histogram(
      aes(x = revisit_day_cycle), 
      bins = 100, fill = "#0F5B78", color = "gray44", alpha = 0.6
    ) + 
    theme_bw() +
    labs(
      x = "Returning times \n - logharitmic scale -",
      y = ""
    ) + 
    scale_x_log10()
  
  pn + pl + 
    plot_annotation(title = sprintf("%s - returning times", sp)) &
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
    )
  
  
  ggsave(
    file.path(graphs_dir, sprintf("%s_return_times.png", gsub(" ", "_", sp))), 
    width = 10)
  
  
  
  cls_dt[, visit_time := 1:.N, by = .(file, sleep_cluster)]
  cls_dt[sleep_cluster == 0, visit_time := 1]
  
  cls_dt[, location_type := fifelse(
    sleep_cluster == 0, "new visits", "repeated visit")]
  
  
})




# plot --------------------------------------------------------------------


fsp <- "3_dbscan_clusters_npts_5_distthr_350_nauticalDawn_nauticalDusk.rds"


lapply(seq_along(species), function(i){
  
  
  sp <- species[i]
  
  fin <- file.path(study_dir, gsub(" ", "_", sp), "7_sleep_clusters", fsp)
  
  cls_dt <- fread(fin)
  
  cls_dt <- cls_dt[, n_steps := .N, by = file][n_steps >= 30]
  
  cls_dt[, visit_time := 1:.N, by = .(file, sleep_cluster)]
  cls_dt[sleep_cluster == 0, visit_time := 1]
  
  cls_dt[, location_type := fifelse(
    sleep_cluster == 0, "new visits", "repeated visit")]
  
  # n_trk <- uniqueN(cls_dt$file)
  # n_cls <- uniqueN(
  #   paste0(cls_dt$file, cls_dt$sleep_cluster[cls_dt$sleep_cluster != 0]))
  # n_obs <- uniqueN(paste0(cls_dt$file, cls_dt$day_cycle)) 
  # n_yd <- uniqueN(cls_dt$yd)
  # n_zero <- round(sum(cls_dt$sleep_cluster == 0)/nrow(cls_dt)*100)
  # n_cluster <- round(sum(cls_dt$sleep_cluster != 0)/nrow(cls_dt)*100)
  
  
   cls_dt |> 
     ggplot() + 
     geom_histogram(aes(x = visit_time, fill = location_type), bins = 100) + 
     theme_bw() +
     theme(legend.position = "bottom") + 
     labs(
       x = "Times visited",
       y = "Count", 
       fill = "location type"
     ) + 
     scale_fill_manual(
       values = c("new visits" = "#F0A202", "repeated visit" = "#0F5B78")
     )
   
   
   
   ggsave(
     file.path(graphs_dir, sprintf("%s_times_visited.png", gsub(" ", "_", sp))), 
     width = 10)
   
   
   cls_sum <- cls_dt[ , .(n_visits = .N), by = .(file, sleep_cluster)]

   cls_sum[, p_visits := n_visits / sum(n_visits), by = file]
   cls_sum[, location_type := fifelse(
     sleep_cluster == 0, "new visits", "cluster")]
   
  
   pn <- cls_sum |>
     ggplot() +
     geom_histogram(aes(x = n_visits, fill = location_type), bins = 100) +
     theme_bw() +
     labs(
       x = "number of visits",
       y = "", 
       fill = "location type"
     ) +
     facet_wrap(~location_type, ncol = 1) +
     theme(legend.position = "bottom") + 
     scale_fill_manual(
       values = c("new visits" = "#F0A202", "cluster" = "#0F5B78")
     )
   
  
  
   
   ggsave(
     file.path(graphs_dir, sprintf("%s_n_visits.png", gsub(" ", "_", sp))), 
     width = 10)
   

  
})

       