library(data.table)
library(sf)
library(ggplot2)
library(patchwork)
# library(ggplot2)
# library(patchwork)
#library(parallel) - never used it in the end
#load the functions for distance calculations
# source("6_distance_FUNCTIONS.R")
source("6_distance_PLOT_FUNCTIONS.R")
source("0_helper_functions.R")

data_dir <- here::here("Data")
main_out_dir <- file.path(data_dir, "9_distance_explore")
dir.create(main_out_dir, showWarnings = FALSE)

crs <- st_crs(4326) # WGS84

regi_file <- "6_distances_in_Europe_summary.csv"

regi_dt <- fread(file.path(data_dir, regi_file))

regi_dt <- regi_dt[day_limits == "nauticalDawn-nauticalDusk"]

regi_dt <- regi_dt[, ':=' (
  sensors = uniqueN(sensor), 
  n_locs_north = sum(n_locs_north), 
  n_locs_south = sum(n_locs_south)), 
  by = species]

regi_dt <- regi_dt[, yearly_coverage := any(year_days > 200), 
                   by = .(species, file)]

regi_dt <- regi_dt[yearly_coverage == T & n_tracks > 10]

sp_select <- regi_dt[, .(
  order = unique(order), 
  n_eu_total = sum(n_locs_eu),
  n_world_total = sum(n_locs_noneu), 
  n_north = sum(as.numeric(n_locs_north)), 
  n_south = sum(as.numeric(n_locs_south)), 
  migration_txt = unique(migration_txt), 
  nocturnal = unique(nocturnal)), 
  by = species]

sp_select <- sp_select[
  , hemisphere := fifelse(n_south > n_north, "south", "north")]




# 1 - sleep_distances -------------------------------------------------------

inputf <- "1_all_tracks_dcp_distances_nauticalDawn_nauticalDusk_continent.rds"
outputf <- "1_nauticalDawn_nauticalDusk_seasonal_sleep_distances_%s.rds"

# output directory
out_dir <- file.path(main_out_dir, "1_sleep_distances")
dir.create(out_dir, showWarnings = F)

# check only for selected species and if the output files don't already exist
selected_sp <- sp_select[, species]
# output files
out_files <- sprintf(outputf, gsub(" ", "_", selected_sp))
# filter out species for which the output files already exist
selected_sp <- selected_sp[!file.exists(file.path(out_dir, out_files))]
nsp <- length(selected_sp)
rm(out_files)


if(nsp > 0){
  
  lapply(seq_along(selected_sp), function(i) {

    # get relevant info for the species
    sp <- selected_sp[i]
    # sleep time
    sleep_time <- ifelse(
      sp_select[species == sp, nocturnal] == 1, "day", "night")
    # norhtern or southern hemisphere
    hemisphere <- sp_select[species == sp, hemisphere]
    
    # load dcp sleep file
    sp_dir <- file.path(data_dir, "Studies", gsub(" ", "_", sp), "6_distances")
    # get the data
    fin <- file.path(sp_dir, inputf)
    fout <- file.path(out_dir, sprintf(outputf, gsub(" ", "_", sp)))
    
    # LOAD AND FILTER THE TRACK

    # read the data
    slocs <- fread(fin)[day_period == sleep_time][
      , .(file, x_median, y_median, t_median, day_cycle)]
    
    # only allow one hemisphere because of the season calculations
    if(hemisphere == "north"){ slocs <- slocs[y_median >= 0]
    } else{ slocs <- slocs[y_median < 0]}
    
    # add season
    slocs <- add_season(slocs, t_col = "t_median", y_col = "y_median") 
    
    setorder(slocs, file, t_median)
    
    # filter tracks that have less than 30 days per season
    slocs[, nlocs_file_season := .N, by = .(file, season)]
    slocs <- slocs[nlocs_file_season > 30]
    
    # calculate distances per deployment and season
    dist_sleep <- slocs[, {
      
      p_locs <- .SD
      
      # order just in case
      setorder(p_locs, t_median)
      
      # convert to sf object and calculate distances
      p_sf <- st_as_sf(
        p_locs, coords = c("x_median", "y_median"), crs = crs)
      distm <- st_distance(p_sf)
      
      # get lower triangle indices (unique pairs)
      inds <- which(lower.tri(distm), arr.ind = TRUE)
      
      data.table(
        dist = as.numeric(distm[inds]),
        nlocs = nrow(p_locs), 
        day_cycle_1 = p_locs$day_cycle[inds[,1]],
        day_cycle_2 = p_locs$day_cycle[inds[,2]]
      )
      
    }, by = .(file, season)]
    
    
    # save the results
    fwrite(dist_sleep, fout)
    
    rm(slocs, dist_sleep)
    
    cat("Sleep:", i, "|", nsp, "\n")
    
  })
  
}

rm(inputf, outputf, out_dir, selected_sp, nsp)

# 2 - day distances ---------------------------------------------------------

inputf <- "4_all_tracks_max_active_steps_nauticalDawn_nauticalDusk_continent.rds"
outputf <- "1_nauticalDawn_nauticalDusk_max_active_steps_%s.rds"

out_dir <- file.path(main_out_dir, "2_daily_distances")
dir.create(out_dir, showWarnings = FALSE)

selected_sp <- sp_select[, species]

out_files <- sprintf(outputf, gsub(" ", "_", selected_sp))

selected_sp <- selected_sp[!file.exists(file.path(out_dir, out_files))]
nsp <- length(selected_sp)

if(nsp > 0){
  
  lapply(seq_along(selected_sp), function(i) {
    
    # info for the species
    sp <- selected_sp[i]
    hemisphere <- sp_select[species == sp, hemisphere]
    
    sp_dir <- file.path(data_dir, "Studies", gsub(" ", "_", sp), "6_distances")
    # get the data
    fin <- file.path(sp_dir, inputf)
    fout <- 
    
    
    # LOAD AND FILTER THE TRACK
    # read the data
    dlocs <- fread(fin)[
      , .(file, sl_, x1_, y1_, t1_, day_cycle_1, 
          x2_, y2_, t2_, day_cycle_2, continent)]
    
    # only allow one hemisphere because of the season calculations
    if(hemisphere == "north"){ dlocs <- dlocs[y1_ >= 0]
    } else{ dlocs <- dlocs[y1_ < 0]}
    
    # add season
    dlocs <- add_season(dlocs, t_col = "t1_", y_col = "y1_") 
    
    setorder(dlocs, file, t1_)
    
    # filter tracks that have less than 30 days per season
    dlocs[, nlocs_file_season := .N, by = .(file, season)]
    dlocs <- dlocs[nlocs_file_season > 30]
    
    # save the results
    fwrite(dlocs, fout)
    
    cat("Active:", i, "|", nsp, "\n")
    
  }) 
  
}

rm(outputf, inputf, out_files, out_dir, selected_sp, nsp)

# 3 - sleep locations ---------------------------------------------------------

inputf <- "1_all_tracks_dcp_distances_nauticalDawn_nauticalDusk_continent.rds"
outputf <- "1_nauticalDawn_nauticalDusk_sleep_spots_%s.rds"

out_dir <- file.path(main_out_dir, "1_sleep_spots")
dir.create(out_dir, showWarnings = FALSE)

selected_sp <- sp_select[, species]

out_files <- sprintf(outputf, gsub(" ", "_", selected_sp))

selected_sp <- selected_sp[!file.exists(file.path(out_dir, out_files))]
nsp <- length(selected_sp)

if(nsp > 0){
  
  lapply(seq_along(selected_sp), function(i) {
    
    # species info
    sp <- selected_sp[i]
    sleep_time <- ifelse(
      sp_select[species == sp, nocturnal] == 1, "day", "night")
    hemisphere <- sp_select[species == sp, hemisphere]
    
    sp_dir <- file.path(data_dir, "Studies", gsub(" ", "_", sp), "6_distances")
    # get the data
    fin <- file.path(sp_dir, inputf)
    fout <- file.path(out_dir, sprintf(outputf, gsub(" ", "_", sp)))
    
    # load files
    slocs <- fread(fin)[day_period == sleep_time][
      , .(file, x_median, y_median, t_median, day_cycle)]
    
    if(hemisphere == "north"){ slocs <- slocs[y_median >= 0]
    } else{ slocs <- slocs[y_median < 0]}
    
    slocs <- add_season(slocs, t_col = "t_median", y_col = "y_median") 
    
    setorder(slocs, file, t_median)
    
    slocs[, nlocs_file_season := .N, by = .(file, season)]
    
    # at least 30 days per season
    slocs <- slocs[nlocs_file_season > 30]
    
    # save the results
    fwrite(slocs, fout)
    
    rm(slocs)
    
    cat("Sleep spots:", i, "|", nsp, "\n")
    
  })
  
  
}

rm(selected_sp, nsp, inputf, outputf, out_dir)

# PLOT 1: distance histograms--------------------------------------------------

outputf <- "1_active_vs_passive_seaonal_%s.png"

out_dir <- file.path(main_out_dir, "3_seasonal_distances_plots")
dir.create(out_dir, showWarnings = FALSE)

files <- list.files(
  file.path(main_out_dir, "1_sleep_distances"), full.names = TRUE)

out_filenames <- sprintf(
  outputf, sub(".*seasonal_sleep_distances_(.*)_(.*).rds", "\\1_\\2", files))

files <- files[!file.exists(file.path(out_dir, out_filenames))]
nf <- length(files)

if(nf > 0){
  
  lapply(seq_along(files), function(i) {
    
    # load sleep steps
    fin <- files[i]
    # read the data
    sdist <- fread(fin)[, .(season, sl_ = dist)][, type := "sleep"]
    
    # load day steps
    find <- gsub(
      "seasonal_sleep_distances", "max_active_steps", 
      gsub("1_sleep_distances", "2_daily_distances", fin))
    # read the data
    ddist <- fread(find)[, .(season, sl_)][, type := "active"]
    
    # combine the two step types
    distances_dt <- rbindlist(list(sdist, ddist))
    rm(sdist, ddist)
    
    distances_dt[, season := factor(
      season, levels = c("winter", "spring", "summer", "autumn"))]
    
    distances_dt[, colori := paste(type, season, sep = "-")]
    
    distances_dt[, sl_ := ifelse(sl_ == 0, 0.001, sl_)]
    
    # species info
    sp <- sub(
      ".*seasonal_sleep_distances_(.*)_(.*).rds", "\\1 \\2", basename(fin))
    migration <- sp_select[species == sp, migration_txt] 
    hemisphere <- sp_select[species == sp, hemisphere]
    ord <- sp_select[species == sp, order]
    
    p <- distances_dt |> 
      ggplot() +
      geom_histogram(
        aes(x = sl_, fill = colori, y = after_stat(density)), bins = 100
      ) + 
      facet_grid(season ~ type, scales = "free") +
      scale_x_log10() +
      theme_bw() +
      theme(legend.position = "none") +
      labs(
        x = "distance [m]", 
        title = "Distance between sleep locations vs. active steps", 
        subtitle = sprintf(
          "%s [%s] | %s | %s hemisphere", sp, ord, migration, hemisphere)
      ) + 
      scale_fill_manual(values = pal)
    
    # save the plot
    fout <- file.path(out_dir, sprintf(outputf, gsub(" ", "_", sp)))
    ggsave(fout, p, width = 10, height = 12)
    
    cat("Processed:", i, "|", nf, "\n")
    
  })
  
}

rm(outputf, out_dir, files, out_filenames, nf)


# PLOT 2: distances within a year -----------------------------------------

outputf <- "2_night_within_vs_across_years_%s.png"

out_dir <- file.path(main_out_dir, "3_seasonal_distances_plots")

files <- list.files(
  file.path(main_out_dir, "1_sleep_distances"), full.names = TRUE)

out_filenames <- sprintf(
  outputf, sub(".*seasonal_sleep_distances_(.*).rds", "\\1", files))

files <- files[!file.exists(file.path(out_dir, out_filenames))]
nf <- length(files)
rm(out_filenames)

if(nf > 0){
  
  for(i in seq_along(files)){
    
    # get the file name and species
    fin <- files[i]
    sp <- sub(
      ".*seasonal_sleep_distances_(.*)_(.*).rds", "\\1 \\2", basename(fin))
    
    # read the data
    sdist <- fread(fin)
    
    sdist[, ":=" (
      y1  = year(as.Date(day_cycle_1)), 
      y2 = year(as.Date(day_cycle_2)))]
    
    sdist[, ":=" (
      y1 = ifelse(
        season == "winter" & month(as.Date(day_cycle_1)) == 12, y1+1, y1),
      y2 = ifelse(
        season == "winter" & month(as.Date(day_cycle_2)) == 12, y2+1, y2)
    )]
    
    #sdist[, date1 := as.Date(day_cycle_1)][, date2 := as.Date(day_cycle_2)]
    
    sdist[, same_year := fifelse(y1 == y2, "within year", "across years")]
    
    sdist <- sdist[, .(season, sl_ = dist, same_year)]
    
    migration <- sp_select[species == sp, migration_txt] 
    hemisphere <- sp_select[species == sp, hemisphere]
    ord <- sp_select[species == sp, order]
    
    pal <- c("across years" = "#2166ac", "within year" = "#b2182b") 
    
    sdist[, sl_ := ifelse(sl_ == 0, 0.001, sl_)]
    
    sdist[, season := factor(
      season, levels = c("winter", "spring", "summer", "autumn"))]
    
    p <- sdist |> 
      ggplot() +
      geom_histogram(
        aes(x = sl_, fill = same_year, y = after_stat(density)), bins = 100, 
        position = "identity", alpha = 0.5
      ) + 
      facet_wrap(~season) +
      scale_x_log10() +
      theme_bw() +
      theme(legend.position = "none") +
      scale_fill_manual(values = pal) +
      labs(x = "distance [m]") 
    
    pb <- sdist |> 
      ggplot(aes(
        x = sl_, 
        y = factor(season), 
        fill = same_year, 
        group = interaction(season, same_year)
      )) +
      geom_boxplot(
        position = "dodge", 
        alpha = 0.5, outliers = F
      ) +
      scale_x_log10() +
      theme_bw() +
      scale_fill_manual(values = pal) +
      labs(x = "distance [m]", y = "season", fill = "") + 
      theme(legend.position = "bottom")
    
    p / pb + 
      plot_layout(ncol = 1, heights = c(1, 1)) +
      plot_annotation(
        title = "Distances between sleep locations",
        subtitle = sprintf(
          "%s [%s] | %s | %s hemisphere", sp, ord, migration, hemisphere)
      ) &
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold")
      )
    
    
    # save the plot
    fout <- file.path(out_dir, sprintf(outputf, gsub(" ", "_", sp)))
    ggsave(fout, width = 10, height = 12)
    
    rm(pb, p, sdist, fout, migration, hemisphere, ord, pal)
    gc(verbose = F, reset = T)
    
    cat("Processed:", i, "|", nf, "\n")
   
    
     
  }
  
}
  

# PLOT 3: maps ---------------------------------------------------------------


pal <- c(
  "active-spring" = "#006837", "sleep-spring" = "#ACD98DFF", 
  "active-summer" = "#FF7F0FFF", "sleep-summer" = "#FFB977FF",
  "active-autumn" = "#6A3D9A", "sleep-autumn" = "#CAB2D6",
  "active-winter" = "#386CB0", "sleep-winter" = "#A6CEE3"
)


outputf <- "3_active_vs_passive_map_%s.png"

out_dir <- file.path(main_out_dir, "3_seasonal_distances_plots")

files <- list.files(
  file.path(main_out_dir, "1_sleep_spots"), full.names = TRUE)

out_filenames <- sprintf(
  outputf, sub(".*_sleep_spots_(.*).rds", "\\1", files))

files <- files[!file.exists(file.path(out_dir, out_filenames))]
nf <- length(files)


if(nf > 0){
  
  conti <- tryCatch(
    rnaturalearth::ne_load(
      type = "geography_regions_polys",  scale = "medium", category = "physical"), 
    error = function(e) {
      rnaturalearth::ne_download(
        type = "geography_regions_polys",  scale = "medium", category = "physical")
    }
  )
  
  for(i in seq_along(files)) {
    
    fin <- files[i]
    
    # read the data
    slocs <- fread(fin)[, .(season, x_ = x_median, y_ = y_median)]
    slocs[, type := "sleep"]
    
    find <- gsub(
      "sleep_spots", "max_active_steps", 
      gsub("1_sleep_spots", "2_daily_distances", fin))
    
    dlocs <- fread(find)[, .(season, x_ = x2_, y_ = y2_)]
    dlocs[, type := "active"]
    
    locs_dt <- rbindlist(list(slocs, dlocs))
    rm(slocs, dlocs)
    
    locs_dt[, season := factor(
      season, levels = c("winter", "spring", "summer", "autumn"))]
    
    locs_dt[, colori := paste(type, season, sep = "-")]
    
    locs_dt <- st_as_sf(
      locs_dt, coords = c("x_", "y_"), crs = crs, remove = FALSE)
    
    # get species data
    sp <- sub(".*_sleep_spots_(.*)_(.*).rds", "\\1 \\2", basename(fin))
    migration <- sp_select[species == sp, migration_txt] 
    hemisphere <- sp_select[species == sp, hemisphere]
    ord <- sp_select[species == sp, order]
    
    pout <- ggplot() +
      geom_sf(data = conti, fill = "grey95", color = "grey80", inherit.aes = FALSE) +
      geom_sf(data = locs_dt, aes(color = colori), alpha = 0.4, size = 2) +
      coord_sf(
        xlim = range(locs_dt$x_), 
        ylim = range(locs_dt$y_)
      ) +
      scale_color_manual(values = pal) +
      facet_grid(season ~ type) +
      theme_bw() +
      theme(
        legend.position = "none", 
        plot.title = element_text(hjust = 0.5)
      ) +
      labs(
        title = "Locations of sleep and active steps",
        subtitle = sprintf(
          "%s [%s] | %s | %s hemisphere", sp, ord, migration, hemisphere)
      )
    
    
    # save the plot
    fout <- file.path(out_dir, sprintf(outputf, gsub(" ", "_", sp)))
    ggsave(fout, pout, width = 12, height = 15)
    
    cat("Processed:", i, "|", nf, "\n")
    
  }
  
  
}


# Night vs day ------------------------------------------------------------
# 
# input_act <- "4_all_tracks_max_active_steps_nauticalDawn_nauticalDusk_continent.rds"
# input_slp <- "2_all_tracks_dcp_sleep_steps_nauticalDawn_nauticalDusk_continent.rds"
# 
# out_dir <- file.path(main_out_dir, "4_day_vs_night_distances")
# dir.create(out_dir, showWarnings = F)
# 
# selected_sp <- sp_select[, species]
# 
# # pal <-  c("spring" = "#006837", "summer" = "#FF7F0FFF", 
# #           "autumn" = "#6A3D9A", "winter" = "#386CB0")
# 
# lapply(seq_along(selected_sp), function(i) {
#   
#   # get relevant info for the species
#   sp <- selected_sp[i]
#   # norhtern or southern hemisphere
#   hemisphere <- sp_select[species == sp, hemisphere]
#   migration <- sp_select[species == sp, migration_txt] 
#   ord <- sp_select[species == sp, order]
#   
#   # load dcp sleep file
#   sp_dir <- file.path(data_dir, "Studies", gsub(" ", "_", sp), "6_distances")
#   
#   act_dt <- fread(file.path(sp_dir, input_act))
#   act_dt <- act_dt[, .(file, day_cycle_1, x1_, y1_, t1_, sl_active = sl_)]
#   slp_dt <- fread(file.path(sp_dir, input_slp))
#   slp_dt <- slp_dt[, .(file, day_cycle_1, x1_, y1_, t1_, sl_sleep = sl_)]
#   
#   dndt <- merge(
#     act_dt, slp_dt, by = c("file", "day_cycle_1", "x1_", "y1_", "t1_"))
#   
#   rm(slp_dt, act_dt)
#   
#   # only allow one hemisphere because of the season calculations
#   if(hemisphere == "north"){ dndt <- dndt[y1_ >= 0]
#   } else{ dndt <- dndt[y1_ < 0]}
#    
#   dndt <- add_season(dndt, t_col = "t1_", y_col = "y1_")
#   
#   dndt <- melt(
#     dndt, id.vars = c("file", "day_cycle_1", "x1_", "y1_", "t1_", "season"), 
#     measure.vars = c("sl_active", "sl_sleep"), 
#     variable.name = "type", value.name = "sl_"
#   )
#   
#   dndt[, type := gsub("sl_", "", type)]
#   dndt[, colori := paste(type, season, sep = "-")]
#   
#   dndt |> 
#     ggplot() + 
#     geom_histogram(
#       aes(x = sl_, fill = colori, y = after_stat(density)), bins = 100
#     ) + 
#     facet_grid(season~type) +
#     scale_x_log10() +
#     theme_bw() +
#     theme(legend.position = "none") +
#     labs(
#       x = "distance [m]", 
#       title = "Distance between sleep locations vs. active steps", 
#       subtitle = sprintf(
#         "%s [%s] | %s | %s hemisphere", sp, ord, migration, hemisphere)
#     ) + 
#     scale_fill_manual(values = pal)
#   
#   fout <- sprintf("4_day_vs_night_distances_%s.png", gsub(" ", "_", sp))
#   
#   ggsave(file.path(out_dir, fout), width = 10, height = 8)
#   
# 
#   })
# 
# 
# 
# # night bursts ------------------------------------------------------------
# 
# input <- "2_all_tracks_dcp_sleep_steps_nauticalDawn_nauticalDusk_continent.rds"
# 
# out_dir <- file.path(main_out_dir, "5_night_bursts")
# dir.create(out_dir, showWarnings = F)
# 
# selected_sp <- sp_select[, species]
# 
# # pal <-  c("spring" = "#006837", "summer" = "#FF7F0FFF", 
# #           "autumn" = "#6A3D9A", "winter" = "#386CB0")
# 
# lapply(seq_along(selected_sp), function(i) {
#   
#   # get relevant info for the species
#   sp <- selected_sp[i]
#   # norhtern or southern hemisphere
#   hemisphere <- sp_select[species == sp, hemisphere]
#   migration <- sp_select[species == sp, migration_txt] 
#   ord <- sp_select[species == sp, order]
#   
#   # load dcp sleep file
#   sp_dir <- file.path(data_dir, "Studies", gsub(" ", "_", sp), "6_distances")
#   
#   sdt <- fread(file.path(sp_dir, input))
#   
#   # only allow one hemisphere because of the season calculations
#   if(hemisphere == "north"){ sdt <- sdt[y1_ >= 0]
#   } else{ sdt <- sdt[y1_ < 0]}
#   
#   sdt <- add_season(sdt, t_col = "t1_", y_col = "y1_")
#   
#   sdt <- sdt[, .(file, day_cycle_1, sl_, season)]
#   
#   sdt[
#     , day_burst := cumsum(c(1, diff(as.numeric(day_cycle_1) > 1))), by = file]
#   sdt[, day_in_burst := 1:.N, by = .(file, day_burst)]
#   
#   # Assume DT already has columns: day_burst, day_in_burst, sl_
#   
#   # Step 1: Get all unique (burst, burst_length)
#   burst_info <- sdt[, .(burst_length = .N), by = .(file, day_burst)]
#   
#   # Step 2: Create all combinations of (day_burst, window_size) for k = 2:burst_length
#   all_windows <- burst_info[
#     , .(window_size = 2:burst_length), by = .(file, day_burst)]
#   
#   # Step 3: Join with DT to get all rows within the window for each burst and window_size
#   sdt_expanded <- sdt[all_windows, on = .(file, day_burst), allow.cartesian = T]
#   
#   sdt_expanded <- sdt_expanded[day_in_burst <= window_size]
#   
#   # Step 4: Calculate mean sl_ for each (day_burst, window_size)
#   window_bursts <- sdt_expanded[
#     , .(
#       median_sl = median(sl_), 
#       season = season[.N]
#     ), by = .(file, day_burst, window_size)]
#   
#   
#   
#   window_bursts[median_sl < 100] |> 
#     ggplot() + 
#     geom_point(
#       aes(x = window_size, y = median_sl, color = season), 
#       alpha = 0.5, position = position_jitter(width = 0.2, height = 0)
#     ) + 
#     scale_y_log10() +
#     facet_wrap(~season)
#     geom_histogram(
#       aes(x = sl_, fill = colori, y = after_stat(density)), bins = 100
#     ) + 
#     facet_grid(season~type) +
#     scale_x_log10() +
#     theme_bw() +
#     theme(legend.position = "none") +
#     labs(
#       x = "distance [m]", 
#       title = "Distance between sleep locations vs. active steps", 
#       subtitle = sprintf(
#         "%s [%s] | %s | %s hemisphere", sp, ord, migration, hemisphere)
#     ) + 
#     scale_fill_manual(values = pal)
#   
#   fout <- sprintf("4_day_vs_night_distances_%s.png", gsub(" ", "_", sp))
#   
#   ggsave(file.path(out_dir, fout), width = 10, height = 8)
#   
#   
# })
# 



# Median active -----------------------------------------------------------

fins <- c(
  active = "4_all_tracks_max_active_steps_nauticalDawn_nauticalDusk_continent.rds", 
  sleep = "2_all_tracks_dcp_sleep_steps_nauticalDawn_nauticalDusk_continent.rds"
)

out_dir <- file.path(main_out_dir, "5_step_distribution")
dir.create(out_dir, showWarnings = FALSE)

month_limits <- get_month_limits()
# Top plot (month labels)
mb <- ggplot(month_limits) +
  geom_text(aes(x = mid_yd, y = 1, label = month), hjust = 0.5, vjust = 0.5) +
  geom_vline(aes(xintercept = last_yd), color = "gray33", linetype = "dashed") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 396)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2)) +
  theme_void() +  # Remove everything
  theme(
    plot.margin = margin(t = 1, r = 1, b = 0, l = 1, unit = "cm"), 
    panel.spacing = unit(0, "mm"), 
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  ) 

selected_sp <- sp_select[, species]

lapply(seq_along(selected_sp), function(i){
  
  sp <- selected_sp[i]
  migration <- sp_select[species == sp, migration_txt] 
  hemisphere <- sp_select[species == sp, hemisphere]
  ord <- sp_select[species == sp, order]
  
  sp_dir <- file.path(data_dir, "Studies", gsub(" ", "_", sp), "6_distances")
  
  plots <- lapply(names(fins), function(fin_name){
    
    fin <- fins[[fin_name]]
    
    dt <- fread(file.path(sp_dir, fin))[, .(file, sl_, day_cycle_1)]
    #med_act <- fread(file.path(sp_dir, fmed))[, .(file, sl_, day_cycle_1)]
    #dt[is.na(sl_median), sl_median := sl_max]
    dt <- dt[, yd := day_cycle_to_yd(day_cycle_1)]
    #dt <- merge(max_act, med_act, by = c("file", "day_cycle_1"))
    
    #yd <- CJ(yd = 1:366, file = unique(dt$file))
    #dt_expanded <- merge(yd, dt, by = c("file", "yd"), all.x = TRUE)
    
    dt[, sl_ := ifelse(sl_ < 1, 1, sl_)]
    
    med_sl <- dt[
      , .(
        sl_min = min(sl_, 0.25),
        sl_ = median(sl_),
        sl_max = quantile(sl_, 0.75)
      ), by = yd]
    
    med_sl <- merge(data.table(yd = 1:366), med_sl, all.x = T)
    
    med_txt <- med_sl[yd == max(yd)]
    med_txt <- melt(
      med_txt, id.var = "yd", measure.vars = c("sl_min", "sl_", "sl_max"))
    med_txt[,lab := fcase(
      grepl("min", variable), "25%", 
      grepl("sl_$", variable), "50%", 
      grepl("max", variable), "75%")]
    
    bgc <- ifelse(fin_name == "active", "#009E73", "#0072B2")
    
    p <- med_sl |> 
      ggplot() +
      geom_ribbon(
        aes(x = yd, ymin = sl_min, ymax = sl_max), 
        alpha = 0.2, fill = bgc
      ) +
      geom_line(aes(x = yd, y = sl_, color = "sl_", linewidth = "sl_")) +
      geom_line(aes(x = yd, y = sl_max, color = "sl_max", linewidth = "sl_max")) +
      geom_line(aes(x = yd, y = sl_min, color = "sl_min", linewidth = "sl_min")) +
      scale_color_manual(
        values = c("sl_" = "#D55E00", "sl_max" = bgc, "sl_min" = bgc)
      ) +
      geom_segment(
        data = med_txt, 
        aes(x = yd, xend = yd+20, y = value, yend = value, color = variable, linewidth = variable)
      ) + 
      scale_linewidth_manual(
        values = c("sl_" = 1.2, "sl_max" = 0.8, "sl_min" = 0.8), 
        guide = "none"
      ) +
      geom_text(
        data = med_txt, aes(x = yd+5, y = value, label = lab, color = variable), 
        hjust = 0, vjust = 0, size = 4
      ) +
      geom_vline(
        xintercept = month_limits$last_yd, color = "gray33", linetype = "dashed"
      ) +
      scale_y_log10() +
      scale_x_continuous(
        breaks = seq(1, 366, by = 30), limits = c(0, 396), expand = c(0, 0)
      ) +
      labs(
        y = "daily distances [m]", 
        x = "day of the year", 
        caption = fin_name
      ) +
      theme_bw() +
      theme(
        legend.position = "none", 
        plot.margin = margin(t = 0, r = 1, b = 1, l = 1, unit = "cm"), 
        panel.spacing = unit(0, "cm"), 
        axis.ticks = element_blank()
      ) 
    
    
    mb / p + patchwork::plot_layout(heights =  c(0.1, 1)) 
    
  })
  
  wrap_plots(plots) +
    plot_annotation(
      title = sprintf(
        "%s [%s] | %s | %s hemisphere", sp, 
        sp_select[species == sp, order], 
        sp_select[species == sp, migration_txt], 
        sp_select[species == sp, hemisphere]), 
      subtitle = "Active vs. sleep steps",
      theme = theme(
        plot.title = element_text(hjust = 0.5, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, face = "bold")
      )
    )
  
  pname <- sprintf("5_step_distribution_%s.png", gsub(" ", "_", sp))
  
  ggsave(file.path(out_dir, pname), width = 15)
  
  
  
})
