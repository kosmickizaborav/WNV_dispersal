#' ---
#' title: "Get daily distances"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' using different definitions of day and night periods, this script calculates
#' the distances during each period. day cycle is defined as the period between
#' day start of one day until the day start of the next day, e.g. from dawn to 
#' dawn of the next day. day period is defined as night if the timestamp falls
#' between the end of the day and the start of the next one (e.g. dusk to dawn);
#' day is defined as the period between the start of the day and the end of the
#' day (e.g. dawn to dusk).
#' 
#'  **SECTION 1 - Distances per day period - DCP distances**
#' splits the track into day-cycle-periods (DCP) and calculates distances between all
#' points available in each period, as well as the median point for each period.
#' the median of these positions are then used to calculate the distances
#' between the sleeping locations and the median active locations. 
#' 
#' **SECTION 2 - Sleep steps - DCP**
#' because there are some nocturnal species in the dataset, we calculated the 
#' steps between sleeping positions for both night and days. sleeping positions
#' are represented as the median position per day period (for nocturnal species
#' we are going to use the day, and for diurnal night). these median position
#' come from the calculations done in section 1
#' 
#'  **SECTION 2 - Active steps - DCP -> DCP; DCP -> track**
#'  to calculate the active steps, we use the sleeping poistion as the starting 
#'  point of the day, and then calculated two types of distances: 
#'   1 - sleep position to median position of the active period (e.g. for 
#'       diurnal species it would be from a median night poisiton to the median 
#'       day position)
#'   2 - sleep position to all the position available during the day period, 
#'       with extraction of the point that produces the biggest distance, 
#'       and preservation of minimum, median and mean values of the distances
#'       calculated (e.g. for diurnal species, it would be from the median 
#'       night poison to all the day position available, preserving the point 
#'       that constitutes the maximum distance)
#'       
#' **SECTION 3 - Add continent/country information**
#' for each exported dataset, continent and country fields are added using
#' spatial overlays. only steps with sufficient data are retained, for 
#' easier calculation later. 

# 0 - Load packages -------------------------------------------------------

library(data.table)
library(ggplot2)
library(patchwork)
#library(parallel) - never used it in the end
#load the functions for distance calculations
source("6_distance_FUNCTIONS.R")
source("6_distance_PLOT_FUNCTIONS.R")
source("0_helper_functions.R")
source("color_palettes.R")

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


# 2 - DCP sleep steps ---------------------------------------------------------

# check if output files already exist
sleep_files <- gsub(
  "1_all_tracks_dcp_distances", "2_all_tracks_dcp_sleep_steps", dcp_files)

files <- dcp_files[!file.exists(sleep_files)] 
nf <- length(files)
rm(sleep_files)

if(nf > 0){
  
  invisible(lapply(seq_along(files), function(n){
    
    fin <- files[n]
    fout <- gsub(
      "1_all_tracks_dcp_distances", "2_all_tracks_dcp_sleep_steps", fin)
    sp <- gsub("_", " ", sub(".*/Studies/([^/]+)/6_distances/.*", "\\1", fin))
    
    # if the species is nocturnal take day locations 
    # otherwise take night locations
    stype <- ifelse(
      nocturnal_dt[birdlife_name == sp, nocturnal] == 1, "day", "night")
    
    dcp_tracks <- fread(fin)
    
    nodata_dt <- data.table(
      file = unique(dcp_tracks$file),
      sleep_steps_available = FALSE)
    
    if(all(dcp_tracks$day_period_distance_available == F)){
      
      fwrite(nodata_dt, fout)
      
      return(NULL)
      
    }
    
    # get only the median location, day cycle and period
    dcp_tracks <- dcp_tracks[
      , .(x_ = x_median, y_ = y_median, t_ = t_median, 
          day_cycle, day_period, file)]
    
    files_log <- unique(dcp_tracks$file)
    
    dcp_tracks <- dcp_tracks[day_period == stype]
    
    # if there is no locations during the selected day period
    if(nrow(dcp_tracks) == 0){ 
      
      fwrite(nodata_dt, fout)
      
      return(NULL)
      
    }
    
    # split into data for each track
    dcp_tracks <- split(dcp_tracks, by = "file")
    
    # get sleep steps for each DCP track
    sleep_steps <- rbindlist(lapply(dcp_tracks, function(dcp_track){
      return(get_sleep_steps(dcp_track))
      }), idcol = "file", fill = T)
    
    sleep_steps <- sleep_steps[, step_type := stype]
    
    fwrite(sleep_steps, fout)
    
    cat("\n Sleep steps done:", n, "|", nf)
    
    return(NULL)
    
  }))
  
}

# 3 - Active steps ---------------------------------------------------------

# checking if the output files already exist
median_active_files <- gsub(
  "1_all_tracks_dcp_distances", "3_all_tracks_median_active_steps", dcp_files)
max_active_files <- gsub(
  "1_all_tracks_dcp_distances", "4_all_tracks_max_active_steps", dcp_files)

files <- dcp_files[
  !file.exists(median_active_files) | !file.exists(max_active_files)]
nf <- length(files)

rm(median_active_files, max_active_files)

if(nf > 0){
 
  invisible(lapply(seq_along(files), function(n){
    
    fin <- files[n]
    # OUTPUT 1: median sleep to median active
    fout1 <- gsub(
      "1_all_tracks_dcp_distances", "3_all_tracks_median_active_steps", fin)
    # OUTPUT 2: median sleep to max active
    fout2 <- gsub(
      "1_all_tracks_dcp_distances_", "4_all_tracks_max_active_steps_", fin)
    
    # get species name
    sp <- gsub("_", " ", sub(".*/Studies/([^/]+)/6_distances/.*", "\\1", fin))
    
    # if the species is nocturnal take day locations otherwise take night locations
    ntd <- nocturnal_dt[birdlife_name == sp, nocturnal] == 0
    stpt <- ifelse(ntd, "night_to_day", "day_to_night")
    
    # get only the median location, and day cycle and period
    dcp_tracks <- fread(fin)
    
    if(all(dcp_tracks$day_period_distance_available == F)){
      
      out_dt <- data.table(
        file = unique(dcp_tracks$file),
        step_type = ifelse(ntd, "night_day", "day_night"),
        active_steps_available = FALSE)
      
      fwrite(out_dt, fout1)
      fwrite(out_dt, fout2)
      
      return(NULL)
      
    }
    
    dcp_tracks <- dcp_tracks[
      , .(x_ = x_median, y_ = y_median, t_ = t_median, 
          day_cycle, day_period, file)]
    
    # split into data for each track
    dcp_tracks <- split(dcp_tracks, by = "file")
    
    # OUTPUT 1: median sleep to median active
    if(!file.exists(fout1)){
      
      median_active <- rbindlist(lapply(dcp_tracks, function(dcp_track){
        
        return(get_active_steps(dcp_track, night_to_day = ntd))
        
      }), idcol = "file", fill = T)
      
      fwrite(median_active, fout1)
      
      rm(median_active)
      
      cat(paste("\n Median active steps done:", n, "|", nf))
      
    }
    
    # median to max
    if(!file.exists(fout2)){
      
      # extract day limits because we need to assign day period to the 
      # re-sampled track again
      dl <- sub(".*dcp_distances_([^_]+)_([^_]+)\\.rds", "\\1 \\2", fin)
      dl <- strsplit(dl, " ")[[1]]
      
      max_active <- rbindlist(lapply(dcp_tracks, function(dcp_track){
        
        # load the original tracking data, re-sampled
        tfin <- unique(dcp_track$file)
        track <- fread(tfin)
        
        max_steps <- get_active_steps(
          track = track, 
          sleep_locs = dcp_track, 
          day_limits = dl, 
          night_to_day = ntd)
        
        rm(track)
        
        return(max_steps)
        
      }), idcol = "file", fill = T)
      
      fwrite(max_active, fout2)
      
      rm(max_active)
      
      cat("\n Max active steps done:", n, "|", nf)
      
      return(NULL)
      
    }
    
  }))
  
}


# 4 - Add Europe  ---------------------------------------------------------

dist_files <- grep(
  "continent", list.files(dist_dirs, full.names = T), value = T, invert = T)

files <- dist_files[
  !(file.exists(gsub(".rds", "_continent.rds", dist_files)) | 
      file.exists(gsub(".rds", "_continent_nodata.rds", dist_files)))]
nf <- length(files)

if(nf > 0){
  
  invisible(lapply(seq_along(files), function(n){
    
    fin <- files[n]
    fout <- gsub(".rds", "_continent.rds", fin)
    
    # laod steps
    dist_locs <- fread(fin)
    
    # check which column indicates whether the step is available, and only load
    # the ones that have it
    all_cols <- names(dist_locs)
    avlb_col <- grep("available", all_cols, value = T)
    
    available_vec <- dist_locs[, get(avlb_col)]
    
    if(all(available_vec == F)){
      
      fout <- gsub(".rds", "_nodata.rds", fout) 
      
      fwrite(dist_locs, fout)
      
      return(NULL)
      
    }
    
    
    if("x_median" %in% all_cols){ 
      
      xy_cols <- c("x_median", "y_median") 
      
      } else{ xy_cols <- c("x1_", "y1_") }
    
    dist_locs <- dist_locs[get(avlb_col) == T]
    
    if(nrow(dist_locs) > 0){
      
      dist_locs <- dist_locs[!is.na(get(xy_cols[1])) & !is.na(get(xy_cols[2]))][
        , nlocs_per_track := .N, by = file]
      
      dist_locs <- add_continents(
        dist_locs, scale = "medium", coord_cols = xy_cols)
      
     
      fwrite(dist_locs, fout)
      
    } 
    
    cat("\n Continent info added:", n, "|", nf, "\n")
    
  }))
  
}


# 5 - Check if only in Europe ---------------------------------------------

regi_file <- "6_distances_in_Europe_summary.csv"

if(!file.exists(file.path(data_dir, regi_file))){
  
  files <- list.files(
    dist_dirs, pattern = ".*_continent.rds", full.names = T)
  nf <- length(files)
  
  regi_dt <- rbindlist(lapply(seq_along(files), function(i){
    
    fin <- files[i]
    
    cat("\n", i, "|", nf, "DONE!")
    
    dt <- fread(fin)
    dt_cols <- colnames(dt)
    
    dc_col <- ifelse("day_cycle" %in% dt_cols, "day_cycle", "day_cycle_1")
    y_col <- ifelse("y_median" %in% dt_cols, "y_median", "y1_")
    
    # get day bursts
    dt[, ":=" (
      dcp_burst = cumsum(c(1, diff(as.numeric(get(dc_col))) > 1)),
      yd = day_cycle_to_yd(get(dc_col))
      ), by = file]
    
    # check how long is the continuous track
    burst_dt <- dt[, .N, by = c("file", "dcp_burst")]
    burst_stat <- burst_dt[, .(
      burst_max = max(N),
      burst_median = median(N), 
      burst_mean = mean(N)
    ), by = file]
    
    by_track <- dt[, .(
      n_locs = .N, 
      n_locs_eu = sum(continent == "Europe"), 
      n_locs_noneu = sum(continent != "Europe"), 
      n_locs_north = sum(get(y_col) > 0), 
      n_locs_south = sum(get(y_col) < 0),
      n_days = uniqueN(get(dc_col)),
      dc_start = min(get(dc_col)),
      dc_end = max(get(dc_col)),
      period_days = diff(range(get(dc_col))),
      year_days = uniqueN(yd),
      continents = paste(sort(unique(continent)), collapse = ", ")
      ), by = file]
    
    by_track <- merge(by_track, burst_stat, by = "file", all.x = T)
    
    rm(dt, burst_stat)
    
    by_track[, file := basename(file)]
    setnames(by_track, old = "file", new = "track")
    
    by_track[, file := fin]
    
  }), fill = T)
  
  regi_dt <- regi_dt[, ':=' (
    species = gsub(".*/Studies/(.*)_(.*)/6_distances/.*", "\\1 \\2", file),
    day_limits = sub(
      ".*[distances|steps]_(.*)_(.*)_continent.*", "\\1-\\2", file), 
    file = basename(file), 
    sensor = gsub(".*dep_(.*)_sen.rds", "\\1", track))]
  
  regi_dt[, n_tracks := uniqueN(track), by = species]
  
  traits <- unique(fread(here::here("Published_data", "00_bird_traits.csv"))[
    , .(species = birdlife_name, family, order, migration_txt, nocturnal)])
  
  regi_dt <- merge(regi_dt, traits, by = "species", all.x = T)
  setorder(regi_dt, 
    day_limits, file, order, n_locs_eu, period_days, burst_median, n_tracks)
  
  fwrite(regi_dt, file.path(data_dir, regi_file))
  
}




# # 6 - Plot distance availibility --------------------------------------------

cap <- "color of lines in timeline and points in map indicate the year of tracking"

files <- list.files(dist_dirs, pattern = "continent.rds", full.names = T)

# which plots already exist
existing_plots <- unlist(list.files(
  list.dirs(file.path(graph_dir, "6_distances"), recursive = T)))

# check if the plots already exist, if not, return the file names
to_plot <- unlist(lapply(files, function(fin){
  bfin <- basename(fin)
  sp <- gsub(".*/Studies/(.*)/6_distances/.*", "\\1", fin)
  pname <- gsub(".rds", paste0("_", sp, ".png"), bfin)
  
  if(sum(existing_plots %in% pname) == 2){
    return(NULL)
  } else { return(fin) }
  
}))
nf <- length(to_plot)


rm(files, existing_plots)

files <- to_plot 
rm(to_plot)

invisible(lapply(seq_along(files), function(i){
  
  fin <- files[i]
  
  sp <- gsub(".*/Studies/(.*)_(.*)/6_distances/.*", "\\1 \\2", fin)
  dl <- gsub(".*[distances|steps]_(.*)_(.*)_continent.*", "\\1_\\2", fin)
  dtype <- gsub(
    "_", " ", gsub(sprintf(".*all_tracks_([^/]+)_%s.*", dl), "\\1", fin))
  
  fout <- gsub(
    ".rds", paste0("_", gsub(" ", "_", sp), ".png"), basename(fin))
  
  all_plots <- file.path(plots_dir, regions, dl, fout)
  all_plots <- all_plots[!file.exists(all_plots)]
  
  regi_todo <- gsub(
    paste0(".*/6_distances/(.*)/", dl, ".*"), "\\1", all_plots)
  
  if(length(all_plots) == 0){
    
    cat("\n Plot exists!", i, nf)
    
    return(NULL)
    
  }
  
  dl <- gsub("_", "-", dl)
  
  data <- fread(fin)
  
  # generate subtitle for the plot
  n_steps <- nrow(data)
  n_eu <- sum(data$continent == "Europe")
  
  sub <- paste0("[", dl, "] | ", "in Europe: ", n_eu, "/", n_steps)
  
  # check if it's night or day step
  if(grepl("step", fin)){
    sub <- paste(gsub("_", " to ", unique(data$step_type)), "|", sub) } 
  
    # generate
  tit_full <- paste(sp, "-", dtype)
  
  if("x_median" %in% colnames(data)){
    
    setnames(
      data, 
      old = c("x_median", "y_median", "t_median"), 
      new = c("x_", "y_", "t_"))
    
    as_steps <- F
    
  } else { as_steps <- T}
  
  
  lapply(regi_todo, function(regi){
    
    steps_df <- data
    
    if(regi == "Europe"){
      steps_df <- steps_df[continent == "Europe"]
      pname <- grep("Europe", all_plots, value = T)
      
    } else{ 
      pname <- grep("World", all_plots, value = T)
    }
    
    if(nrow(steps_df) > 0){
      
      # plots
      wmap <- plot_on_world_map(steps_df, as_steps = as_steps)
      tl <- plot_steps_timeline(steps_df)
      
      pout <- wmap/tl + 
        plot_annotation(
          title = tit_full, 
          subtitle = sub, 
          caption = cap,
          theme = theme(
            plot.title = element_text(face = "bold", hjust = 0.5), 
            plot.subtitle = element_text(face = "bold", hjust = 0.5)
          )
        ) +
        plot_layout(heights = c(1.2, 1))
      
      # save the plots
      ggsave(pname, pout, width = 18, height = 18, units = "cm")
      
      rm(pout, wmap, tl)
      
    } 
    
    
  })
  
  cat("\n Plot done:", i, "|", nf)
  
  return(NULL)
  
}))



# 7 - Check speed distribution --------------------------------------------

fout <- "6_maximum_sl_speed_summary_per_species.csv"

if(!file.exists(file.path(data_dir, fout))){
  
  
  speed_dir <- file.path(graph_dir, "6_distances", "Max_sl_speed")
  dir.create(speed_dir, showWarnings = F)
  
  speed_limits_dt <- fread(file.path(data_dir, "3_speed_limits.csv"))
  
  files <- list.files(
    dist_dirs, pattern = "4_.*_max_.*nauticalDusk_continent.rds", full.names = T)
  
  speeds_sp <- rbindlist(lapply(seq_along(files), function(i){
    
    fin <- files[i]
    sp <- sub(".*/Studies/(.*)_(.*)/6_distances/.*", "\\1 \\2", fin)
    
    dt <- fread(fin)[, .(file, t1_, t2_, sl_)]
    dt[, delta_t := as.numeric(difftime(t2_, t1_, units = "secs"))]
    
    dt[, speed := sl_/delta_t]
    
    speed_limit_sp <- speed_limits_dt[birdlife_name == sp, speed_limit]
    
    dt[, above_spl := ifelse(speed > speed_limit_sp, "yes", "no")]
    
    dt |> 
      ggplot() + 
      geom_histogram(aes(x = speed, fill = above_spl), bins = 100) + 
      theme_bw() + 
      scale_fill_manual(values = c("no" = "#219ebc", "yes" = "#fb8500")) +
      labs(
        title = paste(sp, "- speed distribution"),
        subtitle = "maximum daily distances", 
        x = "speed [m/s]",
        y = "number of steps",
        fill = paste0("above speed limit [", speed_limit_sp, " m/s]")
      ) +
      theme(
        legend.position = "top"
      )
    
    pname <- sprintf("%s_max_active_sl_speed.png", gsub(" ", "_", sp))
    
    ggsave(file.path(speed_dir, pname), width = 20, height = 15, units = "cm")
    
    dt[, .(
      min_speed = min(speed, na.rm = T),
      max_speed = max(speed, na.rm = T),
      mean_speed = mean(speed, na.rm = T),
      median_speed = median(speed, na.rm = T)
    )][, birdlife_name := sp]
    
  }))
  
  speeds_sp <- merge(speeds_sp, speed_limits_dt, by = "birdlife_name")
  
  fwrite(speeds_sp, file.path(data_dir, fout))
  
}

speeds_sp <- fread(file.path(data_dir, fout))

legend_dt <- data.table(
  y = 5,
  xmin = 1,
  xmax = 10,
  x_ppt = c(NA, 6, NA,  13),
  y_txt = c(4, 2, 4, 2),
  y_arrow = c(NA, 3, NA, 3),
  x = c(1, 6, 10, 13),
  ppt = c(NA, "med", NA, "lim"),
  ppt_txt = c(
    "min", 
    "mean",
    "max", 
    "seed\nlimit"
  )
)

# Legend plot: points and text
legend <- legend_dt |> 
  ggplot() +
  geom_segment(
    aes(x = x_ppt, xend = x, y = 5, yend = y_arrow), color = "gray33", 
    arrow = arrow(length = unit(0.1, "cm"), type = "closed"), 
    linetype = "dashed", position = position_nudge(y = -0.3)
  ) +
  geom_errorbar(
    aes(y = y, xmin = xmin, xmax = xmax),
    linewidth = 1.2, color = "gray33", alpha = 0.6
  ) +
  geom_point(
    aes(x = x_ppt, y = y, shape = ppt), size = 8, fill = "white"
  ) + 
  geom_text(
    aes(x = x, y = y_txt, label = ppt_txt), hjust = 0.5, size = 4
  ) +
  scale_y_continuous(expand = c(0.2, 0.2)) + 
  scale_x_continuous(expand = c(0.3, 0.3)) +
  scale_shape_manual(values = c("lim" = 16, "med" = 18), na.value = NA) +
  theme_void() + 
  labs(title = "Legend") +
  theme(
    legend.position = "none",
    plot.background = element_rect(
      fill = "white", color = "gray22", linewidth = 1.2), 
    plot.title = element_text(
      hjust = 0.5, vjust = 0, size = rel(1), face = "bold")
  )

# set order of 
speeds_sp[, sl_order := max(speed_limit), by = order]

setorder(speeds_sp, sl_order, order, median_speed)

speeds_sp[, sp_id := .I]

order_dt <- speeds_sp[, .(
  ymax = max(sp_id), 
  ymin = min(sp_id), 
  ymed = as.numeric(median(sp_id)), 
  count = max(sp_id) - min(sp_id) + 1), 
  by = order]
order_dt[, lab := ifelse(count > 2, order, "")]

max75 <- max(speeds_sp$speed_limit)+1

pscl <- c(-max75*0.02, -max75*0.08, -max75*0.33)

speeds_sp |> 
  ggplot() +
  geom_errorbar(
    aes(y = sp_id, xmin = min_speed, xmax = max_speed, color = order), 
    linewidth = 1.2, alpha = 0.6
  ) +
  geom_point(
    aes(y = sp_id, x = mean_speed, color = order), size = 3, shape = 18,
    alpha = 0.6
  ) +
  geom_point(
    aes(y = sp_id, x = speed_limit, color = order), size = 3, shape = 16,
    alpha = 0.6
  ) +
  geom_rect(
    data = order_dt,
    aes(xmin = pscl[3], xmax = pscl[2], ymin = ymin - 0.5, ymax = ymax + 0.5, 
        fill = order),  color = "gray33",
    alpha = 0.4
  ) +
  geom_text(
    data = order_dt, 
    aes(y = ymed, x = pscl[2]+(pscl[3]-pscl[2])/2, label = lab), 
    vjust = 0.5, hjust = 0.5, 
    color = "gray33"
  ) +
  scale_y_continuous(
    breaks = speeds_sp$sp_id, 
    labels = speeds_sp$birdlife_name, 
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    expand = c(0, 0), 
    limits = c(pscl[3], max75+10), 
    breaks = c(
      seq(0, 100, 10), if(max75 > 100) seq(100, max75+10, 20)), 
    guide = guide_axis(check.overlap = T)
  ) +
  theme_bw() +
  scale_fill_manual(values = ord_col) +
  scale_color_manual(values = ord_col) +
  labs(
    x = "speed [m/s]", 
    title = "Speed distribution obtained from daily distances vs. speed limit",
  ) +
  theme(
    axis.title.y = element_blank(), 
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(size = rel(0.8)),
    legend.position = "none"
  ) + 
  inset_element(
    legend,
    left = 0.70,   # fraction of plot width from left (0 to 1)
    bottom = 0.01, # fraction of plot height from bottom (0 to 1)
    right = 0.99,  # fraction of plot width from left (0 to 1)
    top = 0.16  # fraction of plot height from bottom (0 to 1)
  )

pname <- "6_speed_distribution_thresholds.png"

ggsave(
  file.path(graph_dir, "6_distances", pname), 
  width = 12, height = 16, dpi = 300
)


# Plot data availability for report---------------------------------------------

# INPUT
files <- list.files(
  dist_dirs, 
  pattern = ".*max_active_steps_nauticalDawn_nauticalDusk_continent.rds", 
  full.names = T)

# OUTPUT
fout <- file.path(graph_dir, "7_daily_distances_per_species_timeline.png")

if(!file.exists(fout)){
  
  
  month_dt <- get_month_limits()
  
  days_space <- 366
  order_space <- 100
  wdth <- 50
  
  depl_count <- rbindlist(lapply(seq_along(files), function(i){
    
    fin <- files[i]
    
    dt <- fread(fin)
    
    cat("\n", i, "of", length(files), "DONE!")
    
    data.table(
      n_tracks = uniqueN(dt$file), 
      n_tracks_eu = ifelse(
        any(dt$continent== "Europe"), uniqueN(dt[continent == "Europe", file]), NA),  
      species = gsub(".*/Studies/(.*)_(.*)/6_distances/.*", "\\1 \\2", fin)
    )
    
  }))
  
  depl_count[, n_track_log := days_space+order_space+wdth*log10(n_tracks)]
  depl_count[, n_track_eu_log := days_space+order_space+wdth*log10(n_tracks_eu)]
  
  depl_count <- add_birdlife_phylogeny(depl_count, species_name = "species")
  depl_count[, ord_max := max(n_tracks), by = order]
  
  setorder(depl_count, ord_max, order, n_tracks, species)
  depl_count[, sp_id := .I]
  
  order_dt <- depl_count[, .(
    sp_max = max(sp_id) + 0.5, 
    sp_min = min(sp_id) - 0.5, 
    sp_med = as.numeric(median(sp_id)), 
    n_order = .N, 
    xmin = days_space, 
    xmax = days_space+order_space
  ), by = .(order)]
  order_dt[, lab := ifelse(n_order > 2, order, "")]
  
  # DAILY COUNTS 
  
  daily_dt <- rbindlist(lapply(seq_along(files), function(i){
    
    fin <- files[i]
    
    dt <- fread(fin)[, yd := day_cycle_to_yd(day_cycle_1)][
      , .(steps_per_day = .N), by = yd]
    
    cat("\n", i, "of", length(files), "DONE!")
    
    dt[
      , species := gsub(".*/Studies/(.*)_(.*)/6_distances/.*", "\\1 \\2", fin)]
    
  }))
  
  daily_dt[, steps_cat := cut(steps_per_day, breaks = c(0, 1, 9, 30, Inf))]
  daily_dt <- merge(daily_dt, depl_count, by = "species")
  
  sp_max <- max(depl_count$sp_id) + 0.5
  
  steps_col <- c("#BFBFBF", "#808080", "#595959", "#333333")
  names(steps_col) <- levels(daily_dt$steps_cat)
  yminmin <- -5
  
  month_box <- data.table(
    xmin = min(month_dt$first_yd), 
    xmax = max(month_dt$last_yd), 
    ymin = yminmin, 
    ymax = sp_max
  )[, xmed := (xmin+xmax)/2]
  
  dep_bb <- days_space+order_space+wdth*c(0, 1, 2, 3)
  
  count_box <- data.table(
    xmin = days_space + order_space, 
    xmax = max(depl_count$n_track_log, na.rm = T), 
    ymin = 0.5, 
    ymax = sp_max, 
    vline = dep_bb, 
    lab = c(1, 10, 100, 1000)
  )
  
  lab_box <- data.table(
    xmin = c(1, days_space, days_space + order_space),
    xmax = c(days_space, days_space + order_space, max(depl_count$n_track_log)),
    ymin = sp_max, 
    ymax = sp_max + 10, 
    lab = c("number of daily distances per calendar day and species", "order", "number of deployments [log scale]\ndotted line = deployments in Europe")
  )[, xmed := (xmin+xmax)/2]
  
  ggplot() +
    geom_rect(
      data = month_box,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "white", color = "gray22",  linewidth = 0.5
    ) +
    geom_tile(
      data = daily_dt, aes(x = yd, y = sp_id, fill = steps_cat), alpha = 0.8
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
      data = order_dt, 
      aes(ymin = sp_min, ymax = sp_max, xmin = xmin, xmax = xmax, fill = order), 
      alpha = 0.4, color = "gray22"
    ) +
    geom_text(
      data = order_dt, 
      aes(y = sp_med, x = days_space+wdth, label = lab), color = "gray22"
    ) +
    geom_rect(
      data = depl_count,
      aes(
        ymin = sp_id-0.5, ymax = sp_id+0.5, 
        xmin = days_space + order_space, xmax = n_track_log, fill = order), 
      alpha = 0.6
    ) + 
    geom_segment(
      data = depl_count,
      aes(
        x = days_space + order_space, xend = n_track_eu_log, 
        y = sp_id, yend = sp_id), 
      color = "white", linewidth = 1, linetype = "dotted"
    ) +
    geom_text(
      data = count_box, 
      aes(x = vline, y = -2.5, label = lab), color = "gray22", size = 3
    ) +
    geom_rect(
      data = lab_box, 
      aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax), 
      fill = "white", color = "gray22", linewidth = 0.5
    ) +
    geom_text(
      data = lab_box, 
      aes(y = ymax - 5, x = xmed, label = lab), 
      color = "gray22", size = 3, fontface = "bold", vjust = c(0, 0.5, 0.5)
    ) +
    scale_fill_manual(
      values = c(ord_col, steps_col), 
      breaks = names(steps_col), 
      labels = c("1", names(steps_col)[-1])
    ) +
    scale_y_continuous(
      breaks = depl_count$sp_id, 
      labels = depl_count$species,
      expand = c(0, 0)
    ) +
    scale_x_continuous(
      breaks = month_dt$first_yd, 
      expand = c(0, 0)
    ) +
    labs(
      x = "", 
      y = "species", 
      fill = "", 
      title = "Number of deployments per species and data availability throughout a year",
    ) +
    coord_cartesian(clip = "off") +
    theme_bw() +
    theme(
      axis.ticks = element_blank(), 
      axis.text.y = element_text(family = "FreeSans", size = 6),
      panel.border = element_blank(),
      axis.title.x = element_text(hjust = 0), 
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.5, "lines"), 
      legend.position = c(0.4, 0.97),
      legend.justification = c(1, 0.5),
      legend.direction = "horizontal", 
      plot.title = element_text(hjust = 0, size = 16, face = "bold")
    ) 
  
  ggsave(fout, width = 35, height = 45, units = "cm") 
  
}



# n_steps_limit <- 10
# dist_limit <- 50
# 
# lapply(seq_along(target_sp), function(n){
#   
#   # get species directory
#   sp <- target_sp[n]
#   dist_dir <- file.path(data_dir, "Studies", gsub(" ", "_", sp), "5_distances")
#   
#   files <- list.files(dist_dirs, "^[234]_.*_continent.rds", full.names = T)
#   
#   lapply(day_limits, function(dl){
#     
#     # OUTPUT file with day-cycle-period (DCP) distances
#     dl_str <- paste(dl, collapse = "_") 
#     dl_files <- grep(dl_str, files, value = T)
#     
#     lapply(c("day_to_night", "night_to_day"), function(ntd){
#       
#       sstart <- sub("_.*", "", ntd)
#       send <- sub(".*_to_", "", ntd)
#       
#       sfin <- grep(paste0("2_all_tracks_dcp_", sub("_.*", "", ntd), "_steps"),
#                    dl_files, value = T)
#       
#       afin <- grep(paste0("4_all_tracks_max_active_steps_", ntd),
#                    dl_files, value = T)
#       
#       if(length(sfin) > 0 & length(afin) > 0){
# 
#         sleep_steps <- fread(file.path(dist_dir, sfin))[
#           , .(file, sl_, continent, day_cycle = day_cycle_1, n_steps_per_track)][
#             , step_type := paste("sleep:", sstart)]
#         
#         active_steps <- fread(file.path(dist_dir, afin))[
#           , .(file, sl_, continent, day_cycle = day_cycle_1, n_steps_per_track)][
#             , step_type := paste("active:", send)]
#         
#         all_steps <- rbindlist(list(sleep_steps, active_steps))[
#           n_steps_per_track >= n_steps_limit][
#             , yd := day_cycle_to_yd(day_cycle)]
#         rm(sleep_steps, active_steps)
#         
#         
#         lapply(regions, function(regi){
#           
#           if(regi == "Europe"){ 
#             for_stats <- all_steps[continent == "Europe"] } else{
#               for_stats <- all_steps }
#           
#           # setNames(
#           # lapply(quants, function(q) quantile(sl_km, q, na.rm = TRUE)), 
#           # paste0("q", qts * 100)),
#           steps_stat <- for_stats[, .(
#             sl_km_median = median(sl_, na.rm = TRUE)/1000,
#             n_steps = .N
#           ),  by = c("yd", "step_type")]
#           rm(for_stats)
#           
#           if(nrow(steps_stat) > 0){
#             
#             step_list <- CJ(
#               yd = 1:366,
#               step_type = c(paste("sleep:", sstart), paste("active:", send)),
#               unique = TRUE)
#             
#             # 4. Merge to ensure all combinations present, missing get NA
#             steps_stat <- steps_stat[step_list, on = .(yd, step_type)][
#               , step_type := factor(
#                 step_type, 
#                 levels = c(paste("sleep:", sstart), paste("active:", send)))][
#                   , n_steps := fifelse(is.na(n_steps), 0, n_steps)]              
#             
#             pout <- plot_median_distance(dt = steps_stat) +
#               plot_annotation(
#                 title = paste(sp, "-", regi), 
#                 subtitle = sprintf(
#                   "sleep: median %s to median %s, active: median %s to maximum %s distance", 
#                   sstart, sstart, sstart, send),
#                 caption = paste0(
#                   "day definition: [", gsub("_", ", ", dl_str), "]"),
#                 theme = theme(
#                   plot.title = element_text(hjust = 0.5), 
#                   plot.subtitle = element_text(hjust = 0.5)
#                 )
#               )
#             
#             pname <- paste0(gsub(" ", "_", sp), "_", ntd, "_steps.png")
#             
#             ggsave(
#               file.path(plots_dir, regi, dl_str, pname),
#               pout, height = 18, width = 18, units = "cm", bg = "white")
#             
#             
#           }
#           
#         })
#         
#         }
#     
#       
#     }) # close day_night
#     
#   }) # close day limits
#   
#   cat(paste0(n, " of ", nsp, ": ", sp, "\n"))
#   
# }) # close species
# 
# 



