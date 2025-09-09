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

filtered <- fread(
  list.files(data_dir, "filter_30_max_active.*_continent.csv", full.names = T))
target_sp <- unique(filtered$species)
nsp <- length(target_sp)


traits_dt <- fread(here::here("Published_data", "00_bird_traits.csv"))[
  birdlife_name %in% target_sp]
# there were duplicates because of some synonyms
traits_dt <- unique(traits_dt[, .(birdlife_name, nocturnal)])

embc_fldr <- "8_migration_one"

# OUTPUT
out_dir <- file.path(data_dir, embc_fldr)
dir.create(out_dir, showWarnings = F)
plots_dir <- file.path(graphs_dir, embc_fldr)
dir.create(plots_dir, showWarnings = F)


clst_names <- c("LL", "LH", "HL", "HH")

wdw <- 7


# 1 - Get matrices --------------------------------------------------------

dist_dirs <- file.path(study_dir, gsub(" ", "_", target_sp), "6_distances")
files <- list.files(
  dist_dirs, "1.*_nauticalDawn_nauticalDusk_continent.rds", full.names = T)

# output directory
files_out <- gsub(".rds", "", basename(files))
files_out <- paste0(
  files_out, sub(".*Studies/(.*?)/6_distances/.*", "_\\1", files), ".rds") 
files_out <- file.path(out_dir, files_out)

files <- files[!file.exists(files_out)]
rm(files_out)
nf <- length(files)


if(nf > 0){
  
  lapply(seq_along(files), function(n){
    
    fin <- files[n]
    sp <- sub(".*Studies/(.*?)_(.*?)/6_distances/.*", "\\1 \\2", fin)
    
    # output directory
    fout <- gsub(".rds", paste0("_", gsub(" ", "_", sp), ".rds"), basename(fin))
    fout <- file.path(out_dir, fout)
    
    track <- fread(fin)
    
    sleep_time <- ifelse(
      traits_dt[birdlife_name == sp, nocturnal] == 1, "day", "night")
    
    track <- track[day_period == sleep_time][, .(
      file, x_ = x_median, y_ = y_median, t_ = t_median, day_cycle, continent)]
  
    setorder(track, file, t_)
    
    track[
      , time_gap := c(0, as.numeric(diff(day_cycle), units = "days")), 
      by = file]
    track[, burst := cumsum(time_gap > 1), by = file]
    
    track[, geometry := sf::st_as_sf(.SD, coords = c("x_", "y_"), crs = crs), 
          .SDcols = c("x_", "y_")]
    
    track[, ':=' (
      sl_ = c(NA, as.numeric(sf::st_distance(
        geometry[-.N], geometry[-1], by_element = TRUE))), 
      t_lag = c(NA, as.numeric(diff(t_, units = "secs")))
    ), by = .(file, burst)]
    
    track[, speed_d := sl_/t_lag]
    
    
    track[, c("sl_sum", "speed_mean", "x_start", "x_end", "y_start", "y_end") := .(
      frollsum(sl_, n = wdw, align = "center"),
      frollmean(speed_d, n = wdw, align = "center"), 
      frollapply(x_, n = wdw, FUN = function(x) x[1], align = "center"),
      frollapply(x_, n = wdw, FUN = function(x) x[length(x)], align = "center"),
      frollapply(y_, n = wdw, FUN = function(x) x[1], align = "center"),
      frollapply(y_, n = wdw, FUN = function(x) x[length(x)], align = "center")
    ), by = .(file, burst)]
    
    track <- track[!is.na(sl_sum) & !is.na(speed_mean)]
    if(nrow(track) == 0){
      return(NULL)
    }
    
    track[, dist_direct := as.numeric(sf::st_distance(
      sf::st_as_sf(.SD, coords = c("x_start", "y_start"), crs = crs),
      sf::st_as_sf(.SD, coords = c("x_end", "y_end"), crs = crs),
      by_element = TRUE
    )), by = .(file, burst)]
    
    track <- track[
      , .(file, burst, x_, y_, t_, day_cycle, sl_, sl_sum, speed_d, 
          speed_mean, dist_direct)]
    
    track[, tortuosity := fifelse(dist_direct == 0, 0, sl_sum/dist_direct)]
    
    embc <- embc(as.matrix(track[, .(tortuosity, speed_mean)]), maxItr = 1000)
    saveRDS(embc, gsub(".rds", "_embc_clustering.rds", fout))
    
    embc <- smth(embc, dlta = 1)
    track[, embc_clst := embc@A]
    
    track[, embc_clst_lab := clst_names[embc_clst]]
    
    track[, month := month(as.Date(day_cycle))]
    
    # save data if exists
    fwrite(track, fout)
    
    rm(track)
    
    cat("\n DONE", n, "|", nf)
    
  })
  
}



# PLOT: map  -------------------------------------------------------------------


files <- grep(
  "embc_clustering", list.files(out_dir, full.names = T), value = T, invert = T)
nf <- length(files)

lapply(seq_along(files), function(n){
  
  fin <- files[n]
  
  sp <- gsub(".*_(.*?)_(.*?).rds", "\\1 \\2", basename(fin))
  dcp_dt <- fread(fin)
  
  dcp_dt |>
    ggplot() +
    geom_point(
      aes(x = tortuosity, y = speed_mean, color = as.factor(embc_clst_lab)),
      size = 0.5, alpha = 0.3
    ) +
    scale_color_manual(
      values = c("HH" = "blue", "HL" = "orange", "LH" = "red", "LL" = "green"), 
      na.value = "gray"
    ) +
    scale_x_log10() +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave(
    filename = file.path(
      plots_dir, paste0("1_", gsub(" ", "_", sp), "_migration_clusters.png")
    ),
    width = 10, height = 6, dpi = 300, bg = "white"
  )
  
  st_as_sf(
    dcp_dt,
    coords = c("x_", "y_"), crs = crs
  ) |>
    ggplot() +
    geom_sf(aes(color = embc_clst_lab), size = 0.5, alpha = 0.3) +
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


# 3 - Step length in migration --------------------------------------------

fin_name <- "4_all_tracks_max_active_steps_nauticalDawn_nauticalDusk_continent.rds"

lapply(seq_along(target_sp), function(n){
  
  sp <- target_sp[n]
  sp_dir <- file.path(study_dir, gsub(" ", "_", sp))
  
  dcp_dt <-  fread(file.path(sp_dir, "6_distances", fin_name))
  dcp_dt <- dcp_dt[, .(file, day_cycle = day_cycle_1, sl_)]
  dcp_dt[, file := basename(file)]
  
  dcp_dt[, month := month(as.Date(day_cycle))]
  
  mig_dt <- fread(
    file.path(sp_dir, embc_fldr, "2_all_tracks_embc_summary_dt.rds"))
  
  mig_dt <- mig_dt[, .(n_clst = .N), by = .(file, day_cycle, embc_clst_lab)]
  mig_dt[, month := month(as.Date(day_cycle))]
  mig_dt[, embc_prop := n_clst/sum(n_clst), by = .(file, day_cycle)]
  mig_dt <- mig_dt[
    , embc_prop_m := mean(embc_prop, na.rm = T), by = .(file, month)]
  mig_dt[, day_cycle := NULL]
  mig_dt <- mig_dt[, .SD[which.max(embc_prop_m)], by = .(file, month)]
  mig_dt[, file := basename(file)]
  
  dcp_dt <- merge(
    dcp_dt, 
    mig_dt, 
    by = c("file", "month"), 
    all.x = T
  )
  
  dcp_dt |> 
    ggplot() +
    geom_violin(
      aes(x = sl_, y = embc_clst_lab, fill = embc_clst_lab), alpha = 0.3) +
    scale_fill_manual(
      values = c("HH" = "blue", "HL" = "orange", "LH" = "red", "LL" = "green"), 
      na.value = "gray"
    ) +
    scale_x_log10() +
    theme_bw() +
    labs(
      x = "max daily distance\n-logarithmic scale-", 
      y = "EMbC cluster",
      title = paste(sp, "- daily distance vs. EMbC clusters")
    ) +
    theme(legend.position = "none")
  
  ggsave(
    filename = file.path(
      plots_dir, paste0("4_", gsub(" ", "_", sp), "_embc_max_active_distance.png")),
    width = 10, height = 6, dpi = 300, bg = "white")
  
})
