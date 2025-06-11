library(data.table)
library(ggplot2)
source("0_helper_functions.R")
source("6_plot_datatable_FUNCTIONS.R")

data_dir <- here::here("Data")
graph_dir <- file.path(data_dir, "Graphs")


# Get point per deployment ------------------------------------------------


if(!file.exists(file.path(data_dir, "SIBECOL_deploy_location.csv"))){
  

  file_down_report <- "1_download_report.csv"
  deployments_down <- fread(file.path(data_dir, file_down_report))[error == ""][
    , .(file = file_path, birdlife_name)]
  
  n_total <- nrow(deployments_down)
  files <- deployments_down$file
  
  deploy_location <- rbindlist(lapply(sequence(n_total), function(i) {
    fin <- files[i]
    dt <- as.data.table(readRDS(fin))[.N %/% 2 + 1, .(geometry)]
    
    
    coords <- as.data.table(sf::st_coordinates(dt$geometry))[
      , file := fin]
    
    rm(dt); gc()
    
    cat(sprintf("Processed %d | %d\n", i, n_total))
    
    coords
    
  }))
  
  
  result <- deployments_down[deploy_location, on = .(file)][
    !is.na(X) & !is.na(Y)]
  
  fwrite(result, file.path(data_dir, "SIBECOL_deploy_location.csv"))
  
}



# PLOT: MAP DEPLOYMENTS ---------------------------------------------------


if(F){
  
  result <- fread(file.path(data_dir, "SIBECOL_deploy_location.csv"))
  result <- add_birdlife_phylogeny(result, species_name = "birdlife_name")
  setorder(result, order)
  result[, order := factor(order, levels = unique(counts$order))]
  
  dlocs <- sf::st_as_sf(result, coords = c("X", "Y"), crs = sf::st_crs(4326))
  rm(result)
  
  
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") 
  world_id <- sf::st_nearest_feature(dlocs$geometry, world$geometry)
  dlocs$continent <- world$continent[world_id]
  
  
  p <- ggplot() + 
    geom_sf(data = world, fill = "gray99") + 
    geom_sf(
      data = dlocs, 
      aes(color = order), 
      alpha = 0.4, size = 1.5
    ) +
    theme_void() +
    viridis::scale_color_viridis(discrete = T) +
    theme(
      legend.position = "none", 
      plot.title = element_text(hjust = 0.5)) + 
    labs(title = "Available deployments") 
    
  
  ggsave(
    file.path(graph_dir, "SIBECOL_deployment_locations.png"), p, height = 15, units = "cm")
  
}


# PLOT: DEPLOYMENTS PER SPECIES -------------------------------------------

if(F){
  counts <- as.data.table(dlocs)[, .(birdlife_name, order, continent)][
    , .(
      world = .N,
      europe = sum(continent == "Europe"), 
      order = unique(order)), 
    by = birdlife_name][
      , ':=' (
        lab = paste0(europe, "|", world), 
        world_limit = fifelse(world > 100, 100, world), 
        europe_limit = fifelse(europe > 100, 100, europe))]
  
  setorder(counts, order, birdlife_name)
  
  counts[, birdlife_name := factor(birdlife_name, 
                                   levels = unique(birdlife_name))]
  
  # counts <- melt(
  #   counts, 
  #   id.vars = c("birdlife_name", "lab"), 
  #   variable.name = "continent", 
  #   value.name = "count"
  # )[, count_limit := fifelse(count > 100, 100, count)]
  
  
  ggplot(counts) +
    geom_segment(
      aes(x = 0, xend = world_limit, 
          y = birdlife_name, yend = birdlife_name, color = order),
      size = 3
    ) +
    geom_point(
      aes(x = europe_limit, y = birdlife_name), 
      color = "#ed5426", shape = 15, linewidth = 3
    ) +
    scale_color_viridis_d(option = "viridis") +
    scale_x_continuous(limits = c(-1, 101), expand = c(0,0)) +
    labs(y = "", x = "number of deployments [limited at 100]") +
    theme_minimal() +
    theme(legend.position = "none") 
  
  
  ggsave(
    file.path(graph_dir, "SIBECOL_deployment_counts.png"), 
    height = 20, width = 15, units = "cm", bg = "white")
  
}



# CHECK ---------------------------------------------------------

# file_dep_filter <- "1_deployments_to_download.csv"
# deployments_filtered <- fread(file.path(data_dir, file_dep_filter))[
#   , file := make_file_name(study_id, individual_id, deployment_id)
#   ][file %in% basename(deployments_down$file)]
# 
# length(unique(deployments_filtered$study_id))


# PLOT: SPEED QUANTILES ---------------------------------------------------

if(F){
  
  pal <- c("#009580", "#481A6CFF")
  
  file_speed_quantiles <- "3_target_sp_speed_quantiles.csv"
  speed_quant_df <- fread(file.path(data_dir, file_speed_quantiles))[
    , birdlife_name := factor(
      birdlife_name, levels = levels(counts$birdlife_name))][
    , order := factor(order, levels = unique(counts$order))]
  
  
  df_plot <- melt(
    speed_quant_df, 
    id.vars = c("birdlife_name", "speed_limit", "order"),  
    measure.vars = grep("%", names(speed_quant_df), value = T), 
    variable.name = "quantile",
    value.name = "speed"
  )[, limit_check := speed > speed_limit]
 
  df_plot[, birdlife_name := factor(
    birdlife_name, levels = levels(counts$birdlife_name))][                        , order := factor(order, levels = unique(counts$order))]
  
  ggplot(speed_quant_df) +
    geom_point(
      aes(x = speed_limit, y = birdlife_name, color = order), 
      size = 5, shape = 15
    ) +
    scale_color_viridis_d(option = "viridis") +
    theme_minimal() +
    theme(legend.position = "none")
  
  ggsave(
    file.path(graph_dir, "SIBECOL_speed_limits.png"), 
    height = 10, bg = "white"
  )
  
  qp <- ggplot(df_plot) +
    geom_tile(
      aes(x = quantile, y = birdlife_name, fill = limit_check),
      color = "gray33", alpha = 0.8
    ) + 
    theme_minimal() + 
    scale_fill_manual(values = pal) +
    #scale_y_discrete(limits = rev(unique(df_plot$birdlife_name))) +
    theme(
      legend.position = "top", 
      plot.title = element_text(hjust = 0.5), 
      plot.background = element_rect(fill = "white", color = NA),  
      panel.background = element_rect(fill = "white", color = NA)  
    ) +
    labs(
      title = "Distribution of speed vs. speed limit applied",
      y = "species",
      fill = "speed > speed limit"
    )
  
  ggsave(
    file.path(graph_dir, "SIBECOL_speed_quantiles.png"), 
    height = 10
  )
  
}



# 2 - prepare night day steps and turns data -------------------------------------

if(F){
  
  anas_dir <- get_file_path("", folder = "5_distances", species = "Anas platyrhynchos")
  nfin <- "2_all_tracks_dcp_night_steps_nauticalDawn_nauticalDusk_continent.rds"
  dfin <- "4_all_tracks_dcp_max_day_steps_nauticalDawn_nauticalDusk_continent.rds"
  dmfin <- "3_all_tracks_dcp_day_steps_nauticalDawn_nauticalDusk_continent.rds"
  tfin <- "7_prob_stayed_all_tracks_dcp_max_day_steps_nauticalDawn_nauticalDusk_continent.rds"
  
  
  
  turns <- fread(file.path(anas_dir, tfin))
  night_steps <- fread(file.path(anas_dir, nfin))[
    , .(t1_night = t1_, t2_, file, continent, 
        sl_night = sl_, day_cycle = day_cycle_1, x1_night = x1_, x2_night = x2_, 
        y1_night = y1_, y2_night = y2_)]
  day_steps_med <- fread(file.path(anas_dir, dmfin))[
    , .(t1_night = t1_, t1_day_median = t2_, file, sl_day_med = sl_)]
  day_steps <- fread(file.path(anas_dir, dfin))[
    , .(t1_night = t1_, t1_day = t2_, file, sl_day = sl_, 
        x1_day = x1_, x2_day = x2_, y1_day = y1_, y2_day = y2_)]
  
  full_dt <- turns[
    night_steps, on = .(t1_night, file, t2_), nomatch = 0]
  full_dt <- full_dt[
    day_steps, on = .(t1_night, file, t1_day), nomatch = 0]
  full_dt <- full_dt[
    day_steps_med, on = .(t1_night, file), nomatch = 0]
  
  full_dt <- full_dt[sl_day > 50 & sl_day_med > 0 & continent == "Europe"][
    , stayed := sl_night <= 50][, yw := ceiling(yd / 7)][
      , yr := year(t1_night)][
        , n_yd := uniqueN(yd), by = .(file, yr)][n_yd > 30]
  
  rm(day_steps_med, day_steps, night_steps, turns)
  
}






plot_distance <- function(dt, limit = 50){
  
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
  
  pd <- ggplot(dt) +
    geom_vline(
      data = month_limits, aes(xintercept = last_yd), color = "gray33"
    ) +
    geom_line(
      aes(x = yd, y = sl_day, color = "day"), 
      linewidth = 2, alpha = 0.7
    ) +
    labs(y = "steps [m]", x = "year day") +
    scale_color_manual(values = c("#FDE725FF")) +
    scale_x_continuous(
      expand = c(0, 0), limits = c(1, 366), breaks = seq(1, 366, 30)) +
    theme_bw() + 
    theme(
      legend.position = "bottom", 
      plot.margin = margin(0, 0, 0, 0, "mm"),
      panel.spacing = unit(0, "mm"), 
      axis.ticks = element_blank(), 
      axis.text.x = element_blank(), 
      axis.title = element_blank()
    )
  
  pn <- ggplot(dt) +
    geom_vline(
      data = month_limits, aes(xintercept = last_yd), color = "gray33"
    ) +
    geom_line(
      aes(x = yd, y = sl_night, color = "night"), 
      linewidth = 2, alpha = 0.7
    ) +
    geom_vline(aes(yintercept = limit), color = "#ed5426") +
    labs(y = "steps [m]", x = "year day") +
    scale_color_manual(values = c("#481A6CFF")) +
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
  
  counts_df <- unique(median_dt[, .(yd, count = n_file)])
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




# PLOT: median distance ---------------------------------------------------

if(F){
  
  median_dt <- full_dt[
    , lapply(.SD, median, na.rm = TRUE), by = yd,
    .SDcols = c("sl_day", "sl_day_med", "sl_night", "ta_deg")]
  
  median_dt <- median_dt[
    full_dt[
      , .(
        p = sum(stayed)/.N,
        n_file = uniqueN(file)),
      by = yd], 
    on = .(yd), nomatch = 0]
  
  # [ , p_cat := cut(
  #   p,
  #   breaks = c(-Inf, 0.25, 0.75, Inf),
  #   labels = c("low [< 0.25]", "medium", "high [> 0.75]"),
  #   right = FALSE
  # )]
  
  
  plot_distance(median_dt)
  
  
  pt <- ggplot(median_dt) +
    geom_tile(aes(y = 1, x = yd, fill = p), color = "gray33") + 
    geom_vline(
      data = month_limits, aes(xintercept = last_yd), color = "gray33"
    ) +
    scale_x_continuous(
      limits = c(0, 367), breaks = seq(1, 366, 30), expand = c(0,0)
    ) +
    scale_y_continuous(expand = c(0,0)) +
    theme_void() +
    scale_fill_gradient2(
      midpoint = 0.5, low = "#FDE725FF", mid = "#009580", high = "#440154FF") +
    theme(
      axis.ticks = element_blank(),
      plot.margin = margin(0, 0, 0, 0, "mm"),
      panel.spacing = unit(0, "mm"),
      legend.position = "none", 
      legend.title = element_text(vjust = 0.75)
    )
  
  mb / pt/ p + patchwork::plot_layout(heights =  c(0.1, 0.1, 1))
  
  
}





