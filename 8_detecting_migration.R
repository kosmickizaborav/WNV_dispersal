library(data.table)
library(geosphere)
library(zoo)
library(ggplot2)
library(EMbC)
library(sf)
source("0_helper_functions.R")
source("6_distance_PLOT_FUNCTIONS.R")
source("6_distance_FUNCTIONS.R")

crs <- sf::st_crs(4326)

data_dir <- here::here("Data")
study_dir <- file.path(data_dir, "Studies")
graphs_dir <- file.path(data_dir, "Graphs")

filtered <- fread(file.path(data_dir, 
  "6_overview_filter_30_max_active_steps_nauticalDawn_nauticalDusk_continent.csv"))
target_sp <- unique(filtered$species)
nsp <- length(target_sp)


# OUTPUT
out_dirs <- file.path(study_dir, gsub(" ", "_", target_sp), "8_migration")
lapply(out_dirs, dir.create, showWarnings = F)


clst_names <- c("LL", "LH", "HL", "HH")


min_days <- 2
window_size <- 7



# Run for each file -------------------------------------------------------

in_dirs <- file.path(study_dir, gsub(" ", "_", target_sp), "5_flag_static")
files <- list.files(in_dirs, full.names = T, pattern = "sen.rds")
files_out <- gsub("5_flag_static", "8_migration", files)

files <- files[
  !(file.exists(files_out) | 
    file.exists(gsub(".rds", "_nodata.rds", files_out)) |
      file.exists(gsub(".rds", "_error.rds", files_out)))]
nf <- length(files)

rm(in_dirs, files_out)

if(nf > 0){
  
  
  lapply(seq_along(files), function(n){
    
    fin <- files[n]
    
    # output directory
    fout <- gsub("5_flag_static", "8_migration", fin)
    
    track <- fread(fin)[, .(x_, y_, t_)]
    
    track <- add_day_cycle(track, day_limits = c("nauticalDawn", "nauticalDusk"))
    
    if(is.null(track)){
      
      out_dt <- data.table(comment = "no nautical dawn")
      fwrite(out_dt, gsub(".rds", "_nodata.rds", fout))
      return(NULL)
      
    }
    
    track[, n_locs_per_day := .N, by = day_cycle]
    
    wdw <- median(track[, n_locs_per_day])*window_size
    
    track[, time_gap := c(0, as.numeric(diff(t_), units = "days"))]
    track[, weekly_bursts := cumsum(time_gap > min_days)]
    
    track[, geometry := sf::st_as_sf(.SD, coords = c("x_", "y_"), crs = crs), 
          .SDcols = c("x_", "y_")]
    
    track[, ':=' (
      sl_h = c(NA, as.numeric(sf::st_distance(
        geometry[-.N], geometry[-1], by_element = TRUE))), 
      direction = bearing(p1 = cbind(shift(x_), shift(y_)), p2 = cbind(x_, y_))
    ), by = weekly_bursts]
    track[, direction := abs(direction)]
    
    track[, to_n := direction <= 60]
    track[, to_s := direction >= 130]
    
    track[, c("sl_sum_n", "sl_sum_s", "to_ns", "to_ss") := .(
      frollsum(sl_h*to_n, n = wdw, align = "center"),
      frollsum(sl_h*to_s, n = wdw, align = "center"),
      frollsum(to_n, n = wdw, align = "center"),
      frollsum(to_s, n = wdw, align = "center")
    ), by = weekly_bursts]
    
    track[, direction_tendency := abs(to_ns - to_ss)/wdw]
    track[, sl_sum := abs(sl_sum_n - sl_sum_s)]
    
    track <- track[!is.na(direction_tendency) & !is.na(sl_sum)]
    
    
    if(nrow(track) < 2*wdw){
      
      out_dt <- data.table(comment = "not enough data for embc")
      fwrite(out_dt, gsub(".rds", "_nodata.rds", fout))
      
      return(NULL)
      
    }
    
    
    embc_clst <- tryCatch(
      embc(
        as.matrix(track[, .(direction_tendency, sl_sum)]),
        maxItr = 1000),
      error = function(e) return(NULL)
    )
    
    if(is.null(embc_clst)){
      
      fwrite(track, gsub(".rds", "_error.rds", fout))
      return(NULL)
      
    }
    
    embc_clst <- smth(embc_clst, dlta = 1)
    
    saveRDS(embc_clst, gsub(".rds", "_embc_clst.rds", fout))
    
    track[, embc_clst := embc_clst@A]
    
    track[, embc_clst_lab := clst_names[embc_clst]]
    
    
    track[, month := month(as.Date(day_cycle))]
    
    # save data if exists
    fwrite(track, file.path(out_dir, fout))
    
    
    rm(track, embc_clst)
    
    cat("\n DONE", n, "|", nf)
    
    
  })
  
  
  
}


# summarize ---------------------------------------------------------------

nsp <- length(target_sp)

lapply(seq_along(target_sp), function(n){
  
  sp <- target_sp[n]
  
  sp_dir <-  file.path(study_dir, gsub(" ", "_", sp), "8_migration")
  
  files <- list.files(sp_dir, full.names = T, pattern = "sen.rds")
  
  if(length(files) == 0) return(NULL)
  
  sum_dt <- rbindlist(lapply(seq_along(files), function(i){
    
    fin <- files[i]
    
    track <- fread(fin)
    
    track <- track[
      , .(n_clst = .N), 
      by = .(day_cycle, day_period, embc_clst_lab)]
    
    track[, file := fin]
    
  }), fill = T)

  
  fwrite(sum_dt, file.path(sp_dir, "8_migration_summary.rds"))
  
  cat("\n DONE", n, "|", nsp)
  
})



# migration check  --------------------------------------------------------

fin_dcp <- "1_all_tracks_dcp_distances_nauticalDawn_nauticalDusk_continent.rds"
plots_dir <- file.path(graphs_dir, "8_detecting_migration")

lapply(seq_along(target_sp), function(n){

  sp <- target_sp[n]
  sp_dir <- file.path(study_dir, gsub(" ", "_", sp))
  
  dcp_dt <-  fread(file.path(sp_dir, "6_distances", fin_dcp))
  dcp_dt[, file := basename(file)]
  
  mig_dt <- fread(file.path(sp_dir, "8_migration", "8_migration_summary.rds"))
  mig_dt[, cls_prop := n_clst/sum(n_clst), by = .(file, day_cycle)]
  mig_dt <- mig_dt[, .SD[which.max(cls_prop)], by = .(file, day_cycle)]
  mig_dt[, file := basename(file)]
  
  dcp_dt <- merge(
    dcp_dt, 
    mig_dt, 
    by = c("file", "day_cycle"))
  dcp_dt[, month := month(as.Date(day_cycle))]
  
  rm(mig_dt)
  
  st_as_sf(
    dcp_dt,
    coords = c("x_median", "y_median"), crs = crs
  ) |>
  ggplot() +
  geom_sf(aes(color = embc_clst_lab), size = 0.5) +
  scale_color_manual(
    values = c("HH" = "blue", "HL" = "orange", "LH" = "red", "LL" = "green")
  ) +
  theme_minimal() +
  facet_wrap(~month, ncol = 4) +
  theme(legend.position = "bottom")
  
  ggsave(
    filename = file.path(
      plots_dir, paste0(gsub(" ", "_", sp), "_migration_clusters.png")
    ),
    width = 10, height = 6, dpi = 300, bg = "white"
  )
  
  })



