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
plots_dir <- file.path(graphs_dir, "8_migration")
dir.create(plots_dir, showWarnings = F)


clst_names <- c("LL", "LH", "HL", "HH")


min_days <- 3
window_size <- 7




# 1 - Get matrices --------------------------------------------------------

in_dirs <- file.path(study_dir, gsub(" ", "_", target_sp), "5_flag_static")
files <- list.files(in_dirs, full.names = T, pattern = "sen.rds")
files_out <- gsub("5_flag_static", "8_migration", files)

files <- files[!(
  file.exists(files_out) | file.exists(gsub(".rds", "_nodata.rds", files_out)))]
nf <- length(files)

rm(in_dirs, files_out)

if(nf > 0){
  
  lapply(seq_along(files), function(n){
    
    fin <- files[n]
    
    # output directory
    fout <- gsub("5_flag_static", "8_migration", fin)
    
    track <- fread(fin)[, .(x_, y_, t_)]
    
    track <- add_day_cycle(
      track, day_limits = c("nauticalDawn", "nauticalDusk"))
    
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
    track[, direction_abs := abs(direction)]
    
    track[, to_n := direction_abs <= 60]
    track[, to_s := direction_abs >= 130]
    
    track[, c("sl_sum_n", "sl_sum_s", "to_ns", "to_ss") := .(
      frollsum(sl_h*to_n, n = wdw, align = "center"),
      frollsum(sl_h*to_s, n = wdw, align = "center"),
      frollsum(to_n, n = wdw, align = "center"),
      frollsum(to_s, n = wdw, align = "center")
    ), by = weekly_bursts]
    
    track[, direction_tendency := abs(to_ns - to_ss)/wdw]
    track[, sl_sum := abs(sl_sum_n - sl_sum_s)/wdw]
    
    
    track[, month := month(as.Date(day_cycle))]
    
    # save data if exists
    fwrite(track, fout)
    
    rm(track)
    
    cat("\n DONE", n, "|", nf)
    
  })
  
}



# 2 - EmBc --------------------------------------------------------------------

sp_to_do <- target_sp[
  !file.exists(file.path(out_dirs, "1_all_tracks_embc_clusters.rds"))]
nsp <- length(sp_to_do)

if(nsp > 0){
  
  
  lapply(seq_along(sp_to_do), function(n){
    
    sp <- sp_to_do[n]
    
    sp_dir <-  file.path(study_dir, gsub(" ", "_", sp), "8_migration")
    
    files <- list.files(sp_dir, full.names = T, pattern = "sen.rds")
    
    full_dt <- rbindlist(lapply(seq_along(files), function(i){
      
      fin <- files[i]
      track <- fread(fin)
      track[, file := fin]
      
    }), fill = T)
    
    full_dt <- full_dt[!is.na(direction_tendency) & !is.na(sl_sum)]
    
    embc <- embc(
      as.matrix(full_dt[, .(direction_tendency, sl_sum)]),
      maxItr = 1000)
    
    saveRDS(embc, file.path(sp_dir, "1_all_tracks_embc_clusters.rds"))
    
    embc <- smth(embc, dlta = 1)
    full_dt[, embc_clst := embc@A]
    
    full_dt[, embc_clst_lab := clst_names[embc_clst]]
    full_dt[, month := month(as.Date(day_cycle))]
    
    
    fwrite(full_dt, file.path(sp_dir, "2_all_tracks_embc_summary_dt.rds"))
    
  })
  
  
}




# PLOT: map  -------------------------------------------------------------------


fin_dcp <- "1_all_tracks_dcp_distances_nauticalDawn_nauticalDusk_continent.rds"

get_mode <- function(x) {
  ux <- unique(na.omit(x))
  if (length(ux) == 0) return(NA)
  ux[which.max(tabulate(match(x, ux)))]
}



lapply(seq_along(target_sp), function(n){
  
  sp <- target_sp[n]
  sp_dir <- file.path(study_dir, gsub(" ", "_", sp))
  
  dcp_dt <-  fread(file.path(sp_dir, "6_distances", fin_dcp))
  dcp_dt <- dcp_dt[
    , .(x_median, y_median, t_median, file, day_cycle, day_period, dist_max)]
  dcp_dt[, file := basename(file)]
  
  mig_dt <- fread(
    file.path(sp_dir, "8_migration", "2_all_tracks_embc_summary_dt.rds"))
  
  mig_dt |>
    ggplot() +
    geom_point(
      aes(y = sl_sum, x = direction_tendency, color = as.factor(embc_clst_lab)),
      size = 0.5, alpha = 0.3
    ) +
    scale_color_manual(
      values = c("HH" = "blue", "HL" = "orange", "LH" = "red", "LL" = "green"), 
      na.value = "gray"
    ) +
    scale_y_log10() +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave(
    filename = file.path(
      plots_dir, paste0("1_", gsub(" ", "_", sp), "_migration_clusters.png")
    ),
    width = 10, height = 6, dpi = 300, bg = "white"
  )
  
  
  
  mig_dt <- mig_dt[
    , .(n_clst = .N), by = .(file, weekly_bursts, day_cycle, embc_clst_lab)]
  mig_dt[
    , embc_prop := n_clst/sum(n_clst), by = .(file, weekly_bursts, day_cycle)]
  mig_dt <- mig_dt[
    , .SD[which.max(embc_prop)], by = .(file, weekly_bursts, day_cycle)]
  mig_dt[, file := basename(file)]
  
  dcp_dt <- merge(
    dcp_dt, 
    mig_dt, 
    by = c("file", "day_cycle"), 
    all.x = T)
  
  rm(mig_dt)
  
  dcp_dt[, month := month(as.Date(day_cycle))]
  
  dcp_dt[
    , time_gap := c(0, as.numeric(diff(day_cycle), units = "days")), by = file]
  dcp_dt[, window_bursts := cumsum(time_gap > window_size), by = file]
  
  dcp_dt[, c("embc_filled", "embc_filled_prop_mean") := .(embc_clst_lab, NA_real_)]
  
  nas <- is.na(dcp_dt$embc_clst_lab)
  dcp_dt[, embc_filled := embc_clst_lab]
  dcp_dt[nas, embc_filled := rollapply(
    embc_clst_lab, window_size, get_mode, align = "right", fill = NA
  )[nas]]
  
  # Step 2 & 3: Compute rolling mean proportion for the imputed value
  dcp_dt[, matched := embc_clst_lab == embc_filled]
  dcp_dt[, prop_for_filled := ifelse(matched, embc_prop, 0)]
  
  dcp_dt[
    , embc_filled_prop_mean := rollmean(
      prop_for_filled, window_size, align = "right", fill = NA), 
    by = .(file, window_bursts)]
  
  
  st_as_sf(
    dcp_dt,
    coords = c("x_median", "y_median"), crs = crs
  ) |>
    ggplot() +
    geom_sf(aes(color = embc_filled), size = 0.5, alpha = 0.3) +
    scale_color_manual(
      values = c("HH" = "blue", "HL" = "orange", "LH" = "red", "LL" = "green"), 
      na.value = "gray"
    ) +
    theme_minimal() +
    facet_wrap(~month, ncol = 4) +
    theme(legend.position = "bottom")
  
  ggsave(
    filename = file.path(
      plots_dir, paste0("2_", gsub(" ", "_", sp), "_map_embc.png")
    ),
    width = 10, height = 6, dpi = 300, bg = "white"
  )
  
  
  # You may want to return dcp_dt here if you want to collect results in your lapply
  # return(dcp_dt)
})

 
 
  
# chekdhfsfo --------------------------------------------------------------



fin_dcp <- "4_all_tracks_max_active_steps_nauticalDawn_nauticalDusk_continent.rds" 
plots_dir <- file.path(graphs_dir, "8_detecting_migration")


lapply(seq_along(target_sp), function(n){
  
  sp <- target_sp[n]
  sp_dir <- file.path(study_dir, gsub(" ", "_", sp))
  
  dcp_dt <-  fread(file.path(sp_dir, "6_distances", fin_dcp))
  dcp_dt[, file := basename(file)]
  setnames(dcp_dt, old = "day_cycle_1", new = "day_cycle")
  
  mig_dt <- fread(file.path(sp_dir, "8_migration", "8_migration_summary.rds"))
  mig_dt[, cls_prop := n_clst/sum(n_clst), by = .(file, day_cycle)]
  mig_dt <- mig_dt[, .SD[which.max(cls_prop)], by = .(file, day_cycle)]
  mig_dt[, file := basename(file)]
  
  dcp_dt <- merge(
    dcp_dt, 
    mig_dt, 
    by = c("file", "day_cycle"), 
    all.x = T)
  dcp_dt[, month := month(as.Date(day_cycle))]
  
  rm(mig_dt)
  
  dcp_dt |>
    ggplot() +
    geom_boxplot(
      aes(x = sl_, y = embc_clst_lab, fill = embc_clst_lab), size = 0.5, alpha = 0.3) +
    scale_fill_manual(
      values = c("HH" = "blue", "HL" = "orange", "LH" = "red", "LL" = "green"), 
      na.value = "gray"
    ) +
    theme_minimal() +
    scale_x_log10() +
    #facet_wrap(~month, ncol = 4, scale = "free") +
    theme(legend.position = "bottom")
  
  ggsave(
    filename = file.path(
      plots_dir, paste0("2_boxplot_", gsub(" ", "_", sp), "_migration.png")
    ),
    width = 10, height = 6, dpi = 300, bg = "white"
  )
  
})



# check distribution ------------------------------------------------------

lapply(seq_along(target_sp), function(n){
  
  sp <- target_sp[n]
  
  sp_dir <-  file.path(study_dir, gsub(" ", "_", sp), "8_migration")
  
  files <- list.files(sp_dir, full.names = T, pattern = "sen.rds")
  
  if(length(files) == 0) return(NULL)
  
  sum_dt <- rbindlist(lapply(seq_along(files), function(i){
    
    fin <- files[i]
    
    track <- fread(fin)
    
    track[, file := fin]
    
  }), fill = T)
  
  sum_dt |> 
    ggplot() + 
    geom_point(
      aes(x = direction_tendency, y = sl_sum, color = as.factor(embc_all)), 
      alpha = 0.3
    ) 
  
  embc_all <- embc(
    as.matrix(sum_dt[, .(direction_tendency, sl_sum)]),
    maxItr = 1000)
  error = function(e) return(NULL)
  
  sum_dt[, embc_all := embc_all@A]
  
  fwrite(sum_dt, file.path(sp_dir, "8_migration_summary.rds"))
  
  cat("\n DONE", n, "|", nsp)
  
})



