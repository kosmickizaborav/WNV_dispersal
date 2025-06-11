library(data.table)
library(ggplot2)
library(stringr)

source("0_helper_functions.R")
source("6_plot_datatable_FUNCTIONS.R")

data_dir <- here::here("Data")
graph_dir <- file.path(data_dir, "Graphs")

gif_dir <- file.path(graph_dir, "SIBECOL_gif")


# FUNCTION: get_gif -------------------------------------------------------

if(!dir.exists(gif_dir)){ dir.create(file.path(gif_dir)) }

get_gif <- function(steps, id = "night", species = NULL, file = NULL, 
                    gif_dir = file.path(graph_dir, "SIBECOL_gif")){
  
  out_file <- paste(
    gsub(" ", "_", species), id, 
    gsub(".rds", ".gif", basename(file)), sep = "_")
  
  if(file.exists(file.path(gif_dir, out_file))){
    
    return(NULL)
    
  } else{
    
    cat(paste(species, "\n"))
    
    track <- unique(rbindlist(list(
      steps[, .(x_ = x1_, y_ = y1_, t_ = t1_)], 
      steps[, .(x_ = x2_, y_ = y2_, t_ = t2_)]
    )))[, t_ := as.POSIXct(t_, tz = "UTC")][, id := id]
    track <- as.data.frame(track)
    track <- track[order(track$t_), ]
    
    rm(steps)
    
    track <- move(
      x = track$x_,
      y = track$y_,
      time = track$t_,
      data = track,
      proj = CRS("+proj=longlat +datum=WGS84"), 
      animal = track$id 
    )
    
    r <- ifelse(id == "night", 1, 12)
    u <- ifelse(id == "night", "days", "hours")
    col <- ifelse(id == "night", "#481A6CFF", "#FDE725FF")
    track <-  align_move(track, res = r, unit = u)
    
    frm <- frames_spatial(
      track, path_colours = col,
      map_service = "osm", map_type = "streets",  alpha = 0.8) |> 
      add_labels(x = "Longitude", y = "Latitude", title = species) |> 
      add_northarrow() |> 
      add_scalebar() |>  
      add_timestamps(type = "label") |> 
      add_progress()
    
    rm(track)
    
    out_file <- paste(
      gsub(" ", "_", species), id, 
      gsub(".rds", ".gif", basename(file)), sep = "_")
    
    animate_frames(frm, out_file = file.path(gif_dir, out_file), overwrite = T)
    
    gc()
    
    
  }
  
 
  
}

# FUNCTION: plot_median_distance ------------------------------------------


plot_distance <- function(dt){
  
  month_limits <- get_month_limits()
  
  mb <- ggplot(month_limits) +
    geom_text(aes(x = mid_yd, y = 1, label = month), hjust = 0.5, vjust = 0.5) +
    geom_vline(aes(xintercept = last_yd), color = "gray33") +
    scale_x_continuous(expand = c(0, 0), limits = c(1, 366)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 2)) +
    theme_void() + # Remove everything
    theme(
      plot.margin = margin(0, 0, 0, 0, "mm"),
      panel.spacing = unit(0, "mm"), 
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
    ) 
  
  p <- ggplot(dt) +
    geom_vline(
      data = month_limits, aes(xintercept = last_yd), color = "gray33"
    ) +
    geom_line(
      aes(x = yd, y = sl_day, color = "day"), 
      linewidth = 2, alpha = 0.7
    ) +
    geom_line(
      aes(x = yd, y = sl_night, color = "night"), linewidth = 1.2, alpha = 0.7
    ) +
    labs(y = "steps [m]", x = "year day") +
    scale_color_manual(
      values = c("#FDE725FF", "#481A6CFF"), name = "", 
      labels = c("day", "night")
    ) +
    scale_x_continuous(
      expand = c(0, 0), limits = c(1, 366), breaks = seq(1, 366, 30)) +
    theme_bw() + 
    theme(
      legend.position = "bottom", 
      plot.margin = margin(0, 0, 0, 0, "mm"),
      panel.spacing = unit(0, "mm"), 
      axis.ticks = element_blank()
    )
  
  nsdp <- ggplot(dt) +
    geom_vline(
      data = month_limits, aes(xintercept = last_yd), color = "gray33"
    ) +
    geom_line(
      aes(x = yd, y = nsd_), color = "#481A6CFF", linewidth = 1.2, alpha = 0.7
    ) +
    scale_x_continuous(
      expand = c(0, 0), limits = c(1, 366), breaks = seq(1, 366, 30)) +
    labs(y = "NSD") +
    theme_void() + 
    theme(
      plot.margin = margin(0, 0, 0, 0, "mm"),
      panel.spacing = unit(0, "mm"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      axis.title.y = element_text(angle = 90)
    )
  
  mb / nsdp / p   + patchwork::plot_layout(heights =  c(0.1, 0.25, 1)) +
    plot_layout(guides = "collect") & 
    theme(legend.position = "bottom")
  
}



# track visualization -----------------------------------------------------


if(F){ 
  
  library(move)
  library(moveVis)
  
  # DONE:
  # "Turdus merula", "Anser anser", "Sturnus vulgaris", "Columba livia", 
  # "Anas platyrhynchos"
  # NOT POSSIBLE
  # "Bubulcus ibis", 
  sps <- c("Ciconia ciconia", "Larus argentatus", 
           "Buteo buteo", "Falco tinnunculus", "Corvus corone")

  nfin <- "2_all_tracks_dcp_night_steps_nauticalDawn_nauticalDusk_continent.rds"
  dfin <- "4_all_tracks_dcp_max_day_steps_nauticalDawn_nauticalDusk_continent.rds"

  colz <- c("#481A6CFF", "#FDE725FF")

  sl_plot_list <- list()

  for(i in seq_along(sps)){

    sp <- sps[i]

    nsteps <- fread(get_file_path(nfin, folder = "5_distances", species = sp))[
      continent == "Europe"][, yd := day_cycle_to_yd(day_cycle_1)][
        , yr := year(t1_)][, n_yd := uniqueN(yd), by = .(file, yr)][
        n_yd == max(n_yd)][file == file[1] & yr == yr[1]]

    sel_file <- unique(nsteps$file)
    sel_y <- unique(nsteps$yr)
    ts1 <- min(nsteps$t1_)
    ts2 <- max(nsteps$t1_)

    local(get_gif(nsteps, id = "night", species = sp, file = sel_file))

    rm(nsteps)

    dsteps <- fread(get_file_path(dfin, folder = "5_distances", species = sp))[
      continent == "Europe"][file == sel_file & year(t1_) == sel_y][
        t1_ >= ts1 & t1_ <= ts2]

    local(get_gif(dsteps, id = "day", species = sp, file = sel_file))


    rm(dsteps)

    gc(verbose = F)

  }
  
}


# 2 - plot distances ----------------------------------------------------------

library(amt)

nfin <- "2_all_tracks_dcp_night_steps_nauticalDawn_nauticalDusk_continent.rds"
dfin <- "4_all_tracks_dcp_max_day_steps_nauticalDawn_nauticalDusk_continent.rds"

sp_list <- data.table(gif_file = list.files(gif_dir))
sp_list[, species := str_replace(
  gif_file, "^([A-Za-z]+)_([a-z]+)_.*", "\\1 \\2")]
#sp_list[, label := str_extract(gif_file, "(?<=_)day(?=_)|(?<=_)night(?=_)")]
# sp_list[, file := paste0(
#   str_extract(gif_file, "(?<=_)([0-9]+_stu_[0-9]+_ind_[0-9]+_dep_[0-9]+_sen)(?=\\.gif)"), ".rds")]

sps <- unique(sp_list$species)
#sp_list <- unique(sp_list[, gif_file := NULL])

if(F){
  
  lapply(seq_along(sps), function(i){
    
    sp <- sps[i]
    
    nsteps <- fread(get_file_path(nfin, folder = "5_distances", species = sp))[
      continent == "Europe"][, yd := day_cycle_to_yd(day_cycle_1)][
        , yr := year(t1_)][, n_yd := uniqueN(yd), by = .(file, yr)][
          n_yd == max(n_yd)][file == file[1] & yr == yr[1]]
    
    sel_file <- unique(nsteps$file)
    sel_y <- unique(nsteps$yr)
    ts1 <- min(nsteps$t1_)
    ts2 <- max(nsteps$t1_)
    
    trk <- make_track(
      nsteps, x1_, y1_, t1_, 
      id = "night track", 
      crs = sf::st_crs(4326), 
      all_cols = TRUE
    ) |> 
      add_nsd(units = "m")
    
    nsteps <- as.data.table(trk)[, .(yd, sl_night = sl_, t1_ = t_, file, nsd_)]
    
    rm(trk)
    
    dsteps <- fread(get_file_path(dfin, folder = "5_distances", species = sp))[
      continent == "Europe"][file == sel_file & year(t1_) == sel_y][
        t1_ >= ts1 & t1_ <= ts2]
    
    dsteps <- dsteps[, .(sl_day = sl_, t1_)]
    
    sl_full <- merge(nsteps, dsteps, by = "t1_", all = TRUE)
    rm(dsteps, nsteps)
    
    distp <- plot_distance(sl_full) + 
      plot_annotation(
        title = sp, 
        theme = theme(plot.title = element_text(hjust = 0.5)))
    
    out_file <- paste(
      gsub(" ", "_", sp), "_dist_",
      gsub(".rds", ".png", basename(sel_file)), sep = "_")
    
    ggsave(file = file.path(gif_dir, out_file), width = 20, units = "cm")
    
  })
  
  
}



# FUNCTION: plot_median_distance ------------------------------------------


plot_median_distance <- function(dt, limit = NULL){
  
  month_limits <- get_month_limits()
  
  mb <- ggplot(month_limits) +
    geom_text(aes(x = mid_yd, y = 1, label = month), hjust = 0.5, vjust = 0.5) +
    geom_vline(aes(xintercept = last_yd), color = "gray33") +
    scale_x_continuous(expand = c(0, 0), limits = c(1, 366)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 2)) +
    theme_void() + # Remove everything
    theme(
      plot.margin = margin(0, 0, 0, 0, "mm"),
      panel.spacing = unit(0, "mm"), 
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
    ) 
  
  # Day steps: ribbon for IQR, line for median
  pd <- ggplot(dt) +
    geom_vline(
      data = month_limits, aes(xintercept = last_yd), color = "gray33") +
    geom_ribbon(
      aes(x = yd, ymin = q25_day, ymax = q75_day), 
      fill = "#FDE725FF", alpha = 0.3
    ) +
    geom_line(aes(x = yd, y = q50_day), color = "#FDE725FF", linewidth = 2) +
    labs(y = "steps [m]", x = "year day") +
    scale_x_continuous(
      expand = c(0, 0), limits = c(1, 366), breaks = seq(1, 366, 30)) +
    theme_bw() +
    theme(
      legend.position = "none", 
      plot.margin = margin(0, 0, 0, 0, "mm"),
      panel.spacing = unit(0, "mm"), 
      axis.ticks = element_blank(), 
      axis.text.x = element_blank(), 
      axis.title = element_blank()
    )
  
  pn <- ggplot(dt) +
    geom_vline(
      data = month_limits, aes(xintercept = last_yd), color = "gray33") +
    geom_ribbon(
      aes(x = yd, ymin = q25_night, ymax = q75_night), 
      fill = "#481A6CFF", alpha = 0.3) +
    geom_line(aes(x = yd, y = q50_night), color = "#481A6CFF", linewidth = 2) +
    geom_hline(aes(yintercept = limit), color = "#ed5426", linewidth = 0.7) +
    labs(y = "steps [m]", x = "year day") +
    scale_x_continuous(
      expand = c(0, 0), limits = c(1, 366), breaks = seq(1, 366, 30)) +
    theme_bw() +
    theme(
      plot.margin = margin(0, 0, 0, 0, "mm"),
      panel.spacing = unit(0, "mm"), 
      axis.ticks = element_blank(), 
      axis.text.x = element_blank(),
      axis.title = element_blank()
    )
  
  counts_df <- unique(dt[, .(yd, count = n_file)])
  counts_df <- rbindlist(list(
    CJ(yd = 1:366, count = 0), counts_df), fill = T)[
      , .(count = sum(count)), by = "yd"]
  
  pc <- ggplot(counts_df, aes(y = count, x = yd)) + 
    geom_step(color = "#009580", linewidth = 2, alpha = 0.7) +
    geom_vline(
      data = month_limits, aes(xintercept = last_yd), color = "gray33"
    ) +
    scale_x_continuous(
      expand = c(0, 0), limits = c(1, 366), breaks = seq(1, 366, 30)) +
    theme_bw() +
    labs(x = "year day")
  theme(
    plot.margin = margin(0, 0, 0, 0, "mm"),
    panel.spacing = unit(0, "mm"), 
    axis.ticks = element_blank(), 
    axis.title.y = element_blank()
  ) 
  
  
  mb / pd /pn /pc  + patchwork::plot_layout(heights =  c(0.1, 1, 1, 0.4)) +
    plot_layout(guides = "collect") & 
    theme(legend.position = "bottom")
  
}


# 2 - plot median distance ------------------------------------------------

month_limits <- get_month_limits()

lapply(seq_along(sps), function(i){
  
  sp <- sps[i]
  
  sp_dir <- get_file_path("", folder = "5_distances", species = sp)
  
  nsteps <- fread(file.path(sp_dir, nfin))[
    , .(t1_, file, continent, sl_night = sl_, day_cycle = day_cycle_1)]
  
  dsteps <- fread(file.path(sp_dir, dfin))[, .(t1_, file, sl_day = sl_)]
  
  full_dt <- merge(nsteps, dsteps, by = c("t1_", "file"), all = TRUE)
  
  rm(dsteps, nsteps)
  
  full_dt <- full_dt[sl_day > 50 & continent == "Europe"][
    , stayed := sl_night <= 50][, yd := day_cycle_to_yd(day_cycle)]
  
  median_dt <- full_dt[
    , .(
      q25_day = quantile(sl_day, 0.25, na.rm = TRUE),
      q50_day = quantile(sl_day, 0.5,  na.rm = TRUE),
      q75_day = quantile(sl_day, 0.75, na.rm = TRUE),
      q25_night = quantile(sl_night, 0.25, na.rm = TRUE),
      q50_night = quantile(sl_night, 0.5,  na.rm = TRUE),
      q75_night = quantile(sl_night, 0.75, na.rm = TRUE)
    ),
    by = yd
  ]
  
  median_dt <- median_dt[
    full_dt[
      , .(
        p = sum(stayed)/.N,
        n_file = uniqueN(file)),
      by = yd], 
    on = .(yd), nomatch = 0]
  
  rm(full_dt)
  
  pout <- plot_median_distance(median_dt, limit = 50) +
    plot_annotation(
      title = sp, 
      theme = theme(plot.title = element_text(hjust = 0.5)))
  
  out_file <- paste0(gsub(" ", "_", sp), "_median_distance_all_tracks.png")
  
  ggsave(
    file.path(gif_dir, out_file), 
    pout, height = 25, width = 20, units = "cm", bg = "white"
  )
  
  dist_dens <- ggplot(median_dt, aes(x = q50_day)) +
    geom_histogram(
      aes(y = after_stat(density)), fill = "#FDE725FF", color = "gray33", 
      bins = 100, alpha = 0.3) +
    geom_density(color = "#FDE725FF", linewidth = 2) +
    theme_bw() +
    labs(x = "day step [m]", y = "density", title = sp)
  
  ggsave(
    file.path(gif_dir, paste0(gsub(" ", "_", sp), "_median_day_kernel.png")), 
    dist_dens, height = 10, width = 15, units = "cm", bg = "white"
  )
  
  mb <- ggplot(month_limits) +
    geom_text(aes(x = mid_yd, y = 1, label = month), hjust = 0.5, vjust = 0.5) +
    geom_vline(aes(xintercept = last_yd), color = "gray33") +
    scale_x_continuous(expand = c(0, 0), limits = c(1, 366)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 2)) +
    theme_void() + # Remove everything
    theme(
      plot.margin = margin(0, 0, 0, 0, "mm"),
      panel.spacing = unit(0, "mm"), 
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
    ) 
  
  prs <- ggplot(median_dt, aes(x = yd, y = p)) +
    geom_vline(
      data = month_limits, aes(xintercept = last_yd), color = "gray33"
    ) +
    geom_line(color = "#481A6CFF", linewidth = 2, alpha = 0.7) +
    scale_x_continuous(
      expand = c(0, 0), limits = c(1, 366), breaks = seq(1, 366, 30)) +
    scale_y_continuous(expand = c(0,0), limits = c(0, 1)) +
    labs(x = "year day", y = "probability stayed") +
    theme_bw() +
    theme(
      plot.margin = margin(0, 0, 0, 0, "mm"),
      panel.spacing = unit(0, "mm"), 
      axis.ticks = element_blank()
    )
  
  prstay <- mb / prs  + patchwork::plot_layout(heights =  c(0.1, 1))+
    plot_annotation(
      title = sp, 
      theme = theme(plot.title = element_text(hjust = 0.5))
    )
  
  ggsave(
    file.path(gif_dir, paste0(gsub(" ", "_", sp), "_probability_stayed.png")), 
    prstay, height = 10, width = 18, units = "cm", bg = "white")
  
  
  
})
  

