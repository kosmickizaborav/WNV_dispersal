library(data.table)
library(ggplot2)
library(patchwork)
library(sf)

# ALL OUTPUT DIRECTORIES
data_dir <- here::here("Data")
study_dir <- file.path(data_dir, "Studies")
graph_dir <- file.path(data_dir, "Graphs")
mig_man <- file.path(data_dir, "Migration_manual")
dir.create(mig_man)

target_sp <- c("Circus aeruginosus", "Falco naumanni", "Milvus migrans",
               "Neophron percnopterus", "Ciconia ciconia")

lapply(target_sp, function(sp){
  
  
  sp_dir <- file.path(study_dir, gsub(" ", "_", sp), "6_distances") 
  
  
  ddt <- fread(
    list.files(sp_dir, "max_.*_nauticalDawn_nauticalDusk_continent", full.names = T))
  
  ddt[, m := month(t1_)]
  
  p_ddt <- st_as_sf(ddt, coords = c("x1_", "y1_"), crs = 4326)
  
  p_ddt |> 
    ggplot() +
    geom_sf(alpha = 0.2) +
    facet_wrap(~m) +
    theme_bw() + 
    theme(legend.position = "none") +
    labs(title = sp)
  
  ggsave(
    file.path(mig_man, paste0(gsub(" ", "_", sp), "_all.png")),
    width = 10, height = 6
  )
  
  fin_sel <- ddt[, .(n_per_track = .N), by = "file"][which.max(n_per_track)][, file]
  
  
  # fin_sel <- "/home/nina/R_projects/WNV_dispersal/Data/Studies/Circus_aeruginosus/5_flag_static/1278021460_stu_1296952627_ind_1332844116_dep_653_sen.rds"
  
  track <- ddt[file == fin_sel]
  
  y_range_dt <- track[, .(y_range = max(y1_) - min(y1_)), by = "m"]
  mm <- y_range_dt[y_range > quantile(y_range, 0.75), m]
  # track[, migration := fcase(
  #   m %in% c(3, 9), T, 
  #   m %in% c(1,2, 4:7, 10:12), F, 
  #   default = NA)]
  
  track[, migration := m %in% mm]
  
  p <- st_as_sf(track, coords = c("x1_", "y1_"), crs = 4326)
  
  pp <- p |> 
    ggplot() +
    geom_sf(aes(color = migration)) +
    facet_wrap(~m) +
    theme_bw() + 
    theme(legend.position = "none")
  
  
  
  pb <- track |>  
    ggplot() + 
    geom_boxplot(aes(x = sl_median, y = migration, fill = migration)) + 
    theme_bw() + 
    theme(legend.position = "none") + 
    labs(x = "maximum daily distance [m] - log scale") + 
    scale_x_log10()
  
  pp + pb + 
    plot_annotation(
      title = sp, 
      subtitle = basename(fin_sel)
    )
  
  ggsave(
    file.path(mig_man, paste0(gsub(" ", "_", sp), basename(fin_sel), ".png")),
    width = 10, height = 6
  )
  
  
})





