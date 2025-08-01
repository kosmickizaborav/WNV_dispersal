library(data.table)
library(ggplot2)
library(patchwork)
source("0_helper_functions.R")


data_dir <- here::here("Data")
main_out_dir <- file.path(data_dir, "10_sigfox_gps")
dir.create(main_out_dir, showWarnings = FALSE)


# 1 - Find deployments with both sensors ----------------------------------

# INPUT
file_deploy_clean <- "2_deployments_cleaned.csv"

deploy_info <- fread(file.path(data_dir, file_deploy_clean))[excluded == F]
deploy_info[, bfile := basename(file)]
deploy_info[
  , deployN := .N, by = .(species, study_id, individual_id, deployment_id)]

# get deployments files and species only
deploy_info <- deploy_info[deployN == 2][, .(species, bfile)]

deploy_info[, track_id := sub("^(.*dep)_.*$", "\\1", bfile)]

# 2 - Track overview ------------------------------------------------------

in_act <- "4_all_tracks_max_active_steps_nauticalDawn_nauticalDusk_continent.rds"

out_dir <- file.path(main_out_dir, "Tracks_overview")
dir.create(out_dir, showWarnings = F)

# continent plot for the background
conti <- tryCatch(
  rnaturalearth::ne_load(
    type = "geography_regions_polys",  scale = "medium", category = "physical"), 
  error = function(e) {
    rnaturalearth::ne_download(
      type = "geography_regions_polys",  scale = "medium", category = "physical")
  }
)

pal <- c("median-sigfox" = "#D55E00", "track-sigfox" =  "#E69F00", 
  "median-gps" = "#0072B2", "track-gps" = "#56B4E9")

pal_sens <- c(sigfox = "#E69F00", gps = "#0072B2")

# check which species are done
pn_dt <- unique(deploy_info[, .(species, track_id)])
pn_dt[, fout := sprintf(
     "%s_%s_sigfox_vs_gps.png", gsub(" ", "_", species), track_id)]
pn_dt <- pn_dt[!file.exists(file.path(out_dir, fout))]


target_sp <- unique(pn_dt$species)
rm(pn_dt)
nsp <- length(target_sp)


if(nsp > 0){
  
  lapply(seq_along(target_sp), function(i){
    
    # species name
    sp <- target_sp[i]
    # species directory
    sp_dir <- file.path(data_dir, "Studies", gsub(" ", "_", sp), "6_distances")
    
    # check which deployments have both sensors
    bfiles <- deploy_info[species == sp, bfile]
    
    # read in the max active steps file
    act_dt <- fread(file.path(sp_dir, in_act))
    # subset only for the files with both sensors
    act_dt <- act_dt[, bfile := basename(file)][bfile %in% bfiles]
    act_dt <- act_dt[
      , .(file, sl_, day_cycle = day_cycle_1, x_ = x1_, y_ = y1_, t_ = t1_)]
    act_dt[, type := "median"]
    
    # load original tracking data 
    # for comparison where is median and where are the points
    og_track <- rbindlist(lapply(unique(act_dt$file), function(fin){
      
      track <- fread(fin)[, .(x_, y_, t_)]
      track[, file := fin]
      track[, type := "track"]
      
    }))
    
    # put all in one file - easier to manage
    dt_full <- rbindlist(list(act_dt, og_track), fill = T)
    rm(act_dt, og_track)
    
    # extract sensor name
    dt_full[, sensor := as.numeric(gsub(".*_dep_(.*?)_sen.rds", "\\1", file))][
      , sensor := fcase(
        sensor == 653, "gps",
        sensor == 2299894820, "sigfox"
      )]
    
    # for colors in the plot
    dt_full[, type_sensor := paste(type, sensor, sep = "-")]
    
    # to group data by deployment id - same deployment different sensors
    dt_full[, track_id := sub("^(.*dep)_.*$", "\\1", basename(file))]
    dt_full <- split(dt_full, by = "track_id")
    
    # clean data so that it includes only the segments of track that 
    # have data for both sensors simultaneously
    dt_full <- lapply(dt_full, function(dt){
      
      dt[, t_round := lubridate::round_date(t_, "hour")]
      
      dc_common <- intersect(
        unique(dt[type_sensor == "median-gps", day_cycle]), 
        unique(dt[type_sensor == "median-sigfox", day_cycle])
      )
      
      day_common <- intersect(
        unique(dt[type_sensor == "track-gps", t_round]),
        unique(dt[type_sensor == "track-sigfox", t_round]
        ))
      
      dt <- dt[(type == "median" & day_cycle %in% dc_common) |
                 (type == "track" & t_round %in% day_common)]
      
      dt[, t_round := NULL]
      
    })
    
    # plot
    lapply(dt_full, function(dt){
      
      # plot points on the map
      pmap <- sf::st_as_sf(dt, coords = c("x_", "y_"), crs = 4326) |> 
        ggplot() + 
        geom_sf(data = conti, fill = "grey95", color = "grey80") +
        geom_sf(aes(color = type_sensor, shape = type_sensor), alpha = 0.3) + 
        scale_color_manual(values = pal) +
        scale_shape_manual(values = c(
          "median-sigfox" = 16, "median-gps" = 16, 
          "track-sigfox" = 21, "track-gps" = 21)
        ) +
        coord_sf(
          xlim = range(dt$x_), 
          ylim = range(dt$y_),
        ) +
        scale_y_continuous(guide = guide_axis(check.overlap = T)) +
        scale_x_continuous(guide = guide_axis(check.overlap = T)) +
        theme_bw() +
        labs(shape = "", color = "", title = "Original track") 
      
      # step data
      dt <- dt[!is.na(sl_)][, sl_ := sl_ / 1000]  # convert to km
      
      if(nrow(dt) == 0) { return(NULL) }
      
      phist <- dt |> 
        ggplot() + 
        geom_histogram(
          aes(x = sl_, fill = sensor),
          position = "dodge", bins = 50, alpha = 0.8
        ) +
        scale_fill_manual(values = pal_sens) +
        theme_bw() +
        labs(
          fill = "", 
          x = "step [km]", 
          title = "Max active step distribution"
        ) + 
        theme(legend.position = "right")
      
      dt_wide <- dcast(
        dt,
        day_cycle ~ sensor,          
        value.var = c("sl_", "x_", "y_"),
        sep = "" 
      )
      
      if(sum(c("sl_gps", "sl_sigfox") %in% colnames(dt_wide)) < 2) {
        return(NULL) }
      
      dt_wide <- dt_wide[!is.na(sl_gps) & !is.na(sl_sigfox)]
      
      # make points
      dt_wide[
        , geometry_sigfox := sf::st_as_sf(.SD,
                                          coords = c("x_sigfox", "y_sigfox"), crs = 4326), 
        .SDcols = c("x_sigfox", "y_sigfox")]
      
      dt_wide[
        , geometry_gps := sf::st_as_sf(.SD,
                                       coords = c("x_gps", "y_gps"), crs = 4326), 
        .SDcols = c("x_gps", "y_gps")
      ]
      
      dt_wide[, dist_btw_median := as.numeric(
        sf::st_distance(geometry_sigfox, geometry_gps, by_element = T))]
      
      pp <- dt_wide |> 
        ggplot() + 
        geom_point(
          aes(x = sl_gps, y = sl_sigfox), size = 2, alpha = 0.3, color = "gray33"
        ) +
        geom_abline(
          slope = 1, intercept = 0, linetype = "dashed", color = "black"
        ) +
        theme_bw() +
        labs(
          x = "gps steps [km]", 
          y = "sigfox steps [km]", 
          title = "Max active steps"
        ) 
      
      pdistm <- dt_wide |> 
        ggplot() + 
        geom_histogram(
          aes(x = dist_btw_median/1000), fill = "gray78", bins = 50, alpha = 0.8, 
          color = "gray44"
        ) +
        theme_bw() +
        labs(
          title = "Distance between median positions",
          x = "distance [km]") + 
        theme(legend.position = "bottom")
      
      
      (pmap + phist + pdistm + pp ) +
        plot_annotation(
          title = sprintf("%s - gps vs. sigfox", sp), 
          caption = sprintf("track ID: %s", dt$track_id[1])
        ) &
        theme(plot.title = element_text(hjust = 0.5))
      
      pname <- sprintf(
        "%s_%s_sigfox_vs_gps.png", gsub(" ", "_", sp), dt$track_id[1])
      
      ggsave(
        filename = file.path(out_dir, pname), 
        width = 12, height = 8, units = "in", dpi = 300
      )
      
      rm(dt_wide, pp, pdistm)
      
    })
    
    cat("Done:", i, "|", nsp)
    
  })
  
}


  # 3 - Explore median steps ---------------------------------------------------

in_files <- list(
  max_active = "4_all_tracks_max_active_steps_nauticalDawn_nauticalDusk_continent.rds",
  median_active = "3_all_tracks_median_active_steps_nauticalDawn_nauticalDusk_continent.rds",
  sleep = "2_all_tracks_dcp_sleep_steps_nauticalDawn_nauticalDusk_continent.rds"
)

pal <- c(
  max_active = "#D55E00", sleep = "#009E73", median_active = "#CC79A7", 
  gps = "#0072B2", sigfox = "#E69F00"
  )

target_sp <- unique(deploy_info$species)
nsp <- length(target_sp)

out_dir <- file.path(main_out_dir, "Steps_compare")
dir.create(out_dir, showWarnings = F)


lapply(seq_along(target_sp), function(i){
  
  # species name
  sp <- target_sp[i]
  
  # species directory
  sp_dir <- file.path(data_dir, "Studies", gsub(" ", "_", sp), "6_distances")
  
  # check which deployments have both sensors
  bfiles <- deploy_info[species == sp, bfile]
  
  fmdt <- rbindlist(lapply(names(in_files), function(nfin){
    
    fin <- in_files[[nfin]]
    
    # read in the max active steps file
    mdt <- fread(file.path(sp_dir, fin))
    # subset only for the files with both sensors
    mdt <- mdt[, bfile := basename(file)][bfile %in% bfiles]
    mdt <- mdt[
      , .(bfile, sl_, day_cycle_1, day_cycle_2, x1_, y1_, x2_, y2_, t1_, t2_)]
    
    mdt[, type := nfin]
    
  }), fill = T)
  
  # get sensor and deployment id
  fmdt[, sensor := as.numeric(gsub(".*_dep_(.*?)_sen.rds", "\\1", bfile))][
    , sensor := fcase(sensor == 653, "gps", sensor == 2299894820, "sigfox")]
  fmdt[, track_id := sub("^(.*dep)_.*$", "\\1", bfile)]
  
  ndp <- length(unique(fmdt$track_id))
  
  fmdt <- split(fmdt, by = c("track_id", "type"))
  
  fmdt <- rbindlist(lapply(fmdt, function(dt){
    
    dc_common <- intersect(
      unique(dt[sensor == "gps", day_cycle_1]), 
      unique(dt[sensor == "sigfox", day_cycle_1])
    )
    
    dt[day_cycle_1 %in% dc_common]
    
  }))
  
  if(nrow(fmdt) == 0) {
    return(NULL)
  }
  
  # convert to km 
  fmdt[, sl_km := fifelse(sl_ < 1, 1, sl_) / 1000]
  
  fmdt[, type_txt := fcase(
    type == "max_active", "active: median to max",
    type == "median_active", "active: median to median",
    type == "sleep", "sleep: median to median"
  )]
  
  
  sl_hist <- fmdt |> 
    ggplot() + 
    geom_histogram(
      aes(x = sl_km, fill = sensor), position = "dodge", bins = 50, alpha = 0.8
    ) +
    scale_fill_manual(values = c(sigfox = "#E69F00", gps = "#0072B2")) +
    theme_bw() +
    labs(x = "step [km]", fill = "") + 
    theme(legend.position = "right") + 
    facet_wrap(~type_txt, ncol = 1, scales = "free") + 
    theme(legend.position = "bottom")
  
  fmdt_wide <- dcast(
    fmdt[, .(day_cycle_1, type, type_txt, track_id, sensor, sl_km)],
    day_cycle_1 + type + type_txt + track_id ~ sensor,          
    value.var = "sl_km",
    sep = "" 
  )
  
  pp <- fmdt_wide |> 
    ggplot() + 
    geom_point(
      aes(x = gps, y = sigfox, color = type, shape = type), 
      size = 2, alpha = 0.3, shape = 21
    ) +
    geom_abline(
      slope = 1, intercept = 0, linetype = "dashed", color = "black"
    ) +
    scale_color_manual(values = pal) +
    theme_bw() +
    labs(
      x = "gps steps [km]", 
      y = "sigfox steps [km]", 
      color = ""
    ) +
    scale_y_log10() + 
    scale_x_log10() + 
    facet_wrap(~type_txt, ncol = 1, scales = "free") + 
    theme(legend.position = "none")
  
  rm(fmdt_wide)
  
  fmdt_points <- melt(
    fmdt, 
    id.vars = c("type", "type_txt", "track_id", "bfile", "day_cycle_1", "sensor"), 
    measure = list(
      y_ = c("y1_", "y2_"),
      x_ = c("x1_", "x2_")
    ), 
    variable.name = "point"
    )[, point := factor(
      fcase(point == 1, "start", point == 2, "end"), levels = c("start", "end"))]
  
  sig_gps_dist <- dcast(
    fmdt_points, 
    day_cycle_1 + type + type_txt + track_id + point ~ sensor,          
    value.var = c("x_", "y_"),
    sep = "" 
  )
  
  sig_gps_dist[
    , geometry_sigfox := sf::st_as_sf(
      .SD, coords = c("x_sigfox", "y_sigfox"), crs = 4326), 
    .SDcols = c("x_sigfox", "y_sigfox")]
  sig_gps_dist[
    , geometry_gps := sf::st_as_sf(
      .SD, coords = c("x_gps", "y_gps"), crs = 4326), 
    .SDcols = c("x_gps", "y_gps")
  ]
  sig_gps_dist[, dist_km := as.numeric(
    sf::st_distance(geometry_sigfox, geometry_gps, by_element = T))/1000]
  
  sig_gps_dist[
    , type := factor(type, levels = c("sleep", "max_active", "median_active"))]
  
  setorder(sig_gps_dist, point, type)
  
  coords_cols <- c("day_cycle_1", "x_gps", "y_gps", "x_sigfox", "y_sigfox")
  
  sig_gps_dist[, is_duplicate := duplicated(.SD), .SDcols  = coords_cols]
  sig_gps_dist <- sig_gps_dist[is_duplicate == F]
  sig_gps_dist[, tit := "distances gps-sigfox"]
  
  phist <- sig_gps_dist |> 
    ggplot() + 
    geom_histogram(
      aes(x = dist_km, fill = type), bins = 50, alpha = 0.6, position = "dodge"
    ) +
    theme_bw() +
    facet_wrap(~tit, ncol = 1, scales = "free") +
    scale_fill_manual(values = pal) +
    labs(x = "distance [km]", fill = "") + 
    theme(legend.position = "bottom")
  
  rm(sig_gps_dist)
  
  fmdt_points <- split(fmdt_points, by = "type")
  
  mplots <- lapply(fmdt_points, function(dt){
    
    box_col <- pal[[unique(dt$type)]]
    
    sf::st_as_sf(dt, coords = c("x_", "y_"), crs = 4326) |> 
      ggplot() + 
      geom_sf(data = conti, fill = "grey95", color = "grey80") +
      geom_sf(aes(color = sensor, shape = sensor), alpha = 0.3) +
      scale_color_manual(values = pal) +
      scale_shape_manual(values = c(gps = 3, sigfox = 22)) +
      coord_sf(
        xlim = range(dt$x_), 
        ylim = range(dt$y_),
      ) +
      scale_y_continuous(guide = guide_axis(check.overlap = T)) +
      scale_x_continuous(guide = guide_axis(check.overlap = T)) +
      theme_bw() +
      labs(shape = "", color = "") +
      theme(legend.position = "none") +
      theme(panel.border = element_rect(colour = box_col, fill=NA, linewidth=2))
    
  })
  
  wrap_plots(mplots) + sl_hist + phist + pp +
    plot_layout(heights = c(1, 1.3)) + 
    plot_annotation(
      title = sprintf("%s - %d deployments", sp, length(bfiles)), 
      subtitle = "Different step type comparison - sigfox vs. gps"
    )
  
  rm(mplots, fmdt_points, fmdt, phist, pp, sl_hist)
  
  pname <- sprintf("%s_sigfox_vs_gps_steps_compare.png", gsub(" ", "_", sp))
  
  ggsave(filename = file.path(out_dir, pname), width = 12, height = 10)
  
  cat("Done:", i, "|", nsp, "\n")
  
})


# check how many sigfox ---------------------------------------------------


sp_dirs <- list.files(file.path(data_dir, "Studies"), full.names = T)




# wrap_plots(mplts) +
#   plot_layout(guides = "collect") +
#   plot_annotation(
#     title = sprintf("%s - %d deployments", sp, ntrk), 
#     subtitle = "median night positions - sigfox vs. gps"
#   )  &
#   theme(
#     legend.position = "bottom", 
#     plot.title = element_text(hjust = 0.5), 
#     plot.subtitle = element_text(hjust = 0.5)
#   ) 
# 
# ggsave(
#   filename = file.path(
#     main_out_dir, sprintf("%s_map_sigfox_vs_gps.png", gsub(" ", "_", sp)))
# )
# 

