library(data.table)
library(ggplot2)
library(patchwork)
source("color_palettes.R")

data_dir <- here::here("Data")
study_dir <- file.path(data_dir, "Studies")
graph_dir <- file.path(data_dir, "Graphs")

# PLOT 1: speed limit ----------------------------------------------------------

# INPUT
fin_sl <- file.path(data_dir, "3_speed_limits.csv")
fin_sq <- file.path(data_dir, "3_target_sp_speed_quantiles.csv")

# OUTPUT 
fout <- file.path(graph_dir, "3_speed_limits_complete.png")

if(!file.exists(fout)){

  
  # speed limits
  sl_dt <- fread(fin_sl)
  # set order and assign a number to the species name
  sl_dt[, genus := tstrsplit(birdlife_name, " ", fixed = T)[[1]]]
  sl_dt[, sp_order := max(speed_limit), by = order]
  sl_dt[, sp_fam := max(speed_limit), by = family]
  setorder(sl_dt, sp_order, order, sp_fam, genus) # sp_fam
  sl_dt[, sp_id := .I]
  
  min_sp <- floor(min(sl_dt$speed_limit)/5) * 5
  max_sp <- ceiling(max(sl_dt$speed_limit)/5) * 5
  
  order_dt <- sl_dt[, .(
    ymax = max(sp_id) + 0.5, 
    ymin = min(sp_id) - 0.5, 
    ymed = as.numeric(median(sp_id)), 
    count = max(sp_id) - min(sp_id) + 1, 
    xmin = max_sp, 
    xmax = max_sp + 10,
    xmed = max_sp + 5), 
    by = .(sp_order, order)]
  order_dt[, lab := ifelse(count > 2, order, "")]
  
  
  sq_dt <- fread(fin_sq)
  sq_dt <- melt(
    sq_dt, 
    id.vars = c("birdlife_name", "speed_limit", "order"),  
    measure.vars = grep("%", names(sq_dt), value = T), 
    variable.name = "quantile",
    value.name = "speed"
  )[, quant := as.numeric(gsub("%", "", quantile))]
  sq_dt[, limit_exceeded := speed > speed_limit]
  
  min_quant <- min(sq_dt$quant[sq_dt$limit_exceeded])-5
  
  # all speeds fall below 70% quantile so not plotting below
  sq_dt <- sq_dt[quant >= min_quant]
  setorder(sq_dt, birdlife_name, quantile)
  
  sq_dt[, add := 1:.N, by = birdlife_name]
  sq_dt[, ':=' (
    xmin = max_sp + 10 + (add-1)*2, 
    xmax = max_sp + 10 + add*2)]
  sq_dt[, xmed := xmin + 1]
  # sq_dt[, qtxt := sprintf("(%d, %d%%]", quantile-5, quantile)]
  sq_dt[, order := fifelse(limit_exceeded, "none", order)]
  
  sq_dt <- merge(
    sq_dt, 
    sl_dt[, .(birdlife_name, sp_id)], 
    by = "birdlife_name", 
    all.x = T)
  
  # Numeric speed breaks
  speed_breaks <- seq(min_sp, max_sp, 5)
  speed_labels <- as.character(speed_breaks)
  
  # Quantile breaks and labels
  quantile_breaks <- unique(sq_dt$xmax)
  quantile_labels <- as.character(unique(sq_dt$quantile))
  
  # Combine them
  all_breaks <- c(speed_breaks, quantile_breaks[-length(quantile_breaks)])
  all_labels <- c(speed_labels, quantile_labels[-length(quantile_labels)])
  
  labs_dt <- data.table(
    ymin = max(sl_dt$sp_id) + 0.5,
    xmin = c(min_sp, max_sp, max_sp + 10),
    xmax = c(max_sp, max_sp + 10, max(sq_dt$xmax)), 
    lab = c("speed limit [m/s]", "order", "speed quantiles\nbelow the limit [%]")
  )
  
  sl_dt |> 
    ggplot() +
    geom_vline(
      xintercept = seq(min_sp, max_sp, 5), 
      color = "gray22", linetype = "dashed"
    ) +
    geom_point(
      aes(y = sp_id, x = speed_limit, color = order), 
      size = 3, shape = 15, alpha = 0.6
    ) +
    geom_rect(
      data = order_dt,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
          fill = order),  color = "gray22",
      alpha = 0.4
    ) +
    geom_text(
      data = order_dt, 
      aes(y = ymed, x = 40, label = lab), 
      vjust = 0.5, hjust = 0.5, 
      color = "gray22"
    ) +
    geom_rect(
      data = sq_dt,
      aes(xmin = xmin, xmax = xmax, ymin = sp_id - 0.5, ymax = sp_id + 0.5, 
          fill = order), 
      color = NA, alpha = 0.6
    ) +
    geom_vline(
      data = sq_dt, 
      aes(xintercept = xmax), color = "gray22", linetype = "dashed"
    ) +
    geom_rect(
      data = labs_dt, 
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymin +10),
      fill = "white", color = "gray22"
    ) +
    geom_text(
      data = labs_dt, 
      aes(y = ymin + 5, x = (xmin + xmax)/2, label = lab), 
      vjust = 0.5, hjust = 0.5, 
      color = "gray22", size = 4, fontface = "bold"
    ) +
    scale_y_continuous(
      breaks = sl_dt$sp_id, 
      labels = sl_dt$birdlife_name, 
      expand = c(0, 0)
    ) +
    scale_x_continuous(
      breaks = all_breaks,
      labels = all_labels, 
      guide = guide_axis(check.overlap = TRUE),
      expand = c(0, 0)
    ) +
    theme_bw() +
    scale_fill_manual(values = c(ord_col, "none" = "white")) +
    scale_color_manual(values = ord_col) +
    theme(
      axis.ticks.y = element_blank(),
      axis.text.y = element_text(family = "FreeSans", size = 6),
      legend.position = "none", 
      plot.title = element_text(hjust = 0, size = 16, face = "bold")
    ) +
    labs(
      title = "Speed limits per species and their relation to speed distributions", 
      y = "species", 
      x = "speed limits vs. speed distribution"
    ) 
  
  ggsave(filename = fout, width = 25, height = 45, units = "cm" )
  
  
}

# PLOT 2: sampling rates -------------------------------------------------------

# code also in 4_resample_tracks.R

#INPUT
fin <- file.path(data_dir, "4_filtered_deployments_sampling_rate.csv")
# OUTPUT FILE
fout <- file.path(graph_dir, "4_median_sampling_rate_order.png")

if(!file.exists(fout)){
  
  sampl_rate <- fread(fin)
  
  # because we are only interested in daily sampling frequencies
  sampl_rate <- sampl_rate[, median_hours := median/60][
    median_hours <= 25]
  
  n_trks <- nrow(sampl_rate)
  n_sps <- uniqueN(sampl_rate$birdlife_name)
  
  sampl_rate[, order_txt := sprintf(
    "%s [%d, %d]", order, uniqueN(birdlife_name), uniqueN(file)), 
    by = order]
  sampl_rate[, order_num := as.integer(
    factor(order, levels = sort(unique(order), decreasing = T)))]
  
  sampl_rate[, floor_hours := floor(median_hours)]
  
  
  sampl_rate_sum <- sampl_rate[, .(
    perc = round(.N/nrow(sampl_rate)*100, 1)), by = floor_hours]
  
  box_dt <- data.table(
    xmin = -5, 
    xmid = -3.5,
    xmax = -1, 
    ymin = -Inf, 
    ymax = Inf, 
    ymid_t = mean(c(0, max(sampl_rate_sum$perc) + 10)), 
    ymid_o = mean(c(0, max(sampl_rate$order_num) + 1)),
    txt_t = "Porportion of deployments\nwithin 1h-intervals [%]", 
    txt_o = "Distribution across\norders [N species, N tracks]")
  
  po <-  ggplot(sampl_rate) +
    geom_point(
      aes(x = median_hours, y = order_num, color = order),
      alpha = 0.5, position = position_jitter(height = 0.1, width = 0)
    ) +
    geom_boxplot(
      aes(x = median_hours, y = order_num - 0.5, fill = order),
      color = "gray22", outlier.shape = NA, alpha = 0.6
    ) +
    geom_rect(
      data = box_dt,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
      alpha = 0.8, fill = "white", color = "black"
    ) +
    geom_text(
      data = box_dt,
      aes(x = xmid, y = ymid_o, label = txt_o),
      size = 3, fontface = "bold", color = "gray22", angle = 90
    ) +
    scale_color_manual(values = ord_col) +
    scale_fill_manual(values = ord_col) +
    scale_y_continuous(
      breaks = unique(sampl_rate[, order_num])-0.25,
      labels = unique(sampl_rate[, order_txt]), 
      position = "right", 
      limits = c(0, max(sampl_rate$order_num) + 1)
    ) +
    scale_x_continuous(
      breaks = seq(0, 24, by = 2), 
      limits = c(box_dt$xmin, 25), 
      expand = c(0,0)
    ) +
    labs(x = "median sampling rate per deployment [h]") +
    theme_bw() +
    theme(
      plot.title = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none", 
      plot.margin = margin(t = 0, r = 5, b = 5, l = 5)
    ) 
  
  
  pt <- sampl_rate_sum |> 
    ggplot() +
    geom_col(
      aes(x = floor_hours, y = perc),
      alpha = 0.5, fill = "gray88", color = "gray22", width = 0.7
    ) +
    geom_text(
      data = sampl_rate_sum[perc > 2],
      aes(x = floor_hours, y = perc, label = perc),
      vjust = -0.5, size = 3, color = "gray22"
    ) +
    geom_rect(
      data = box_dt,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
      alpha = 0.8, fill = "white", color = "black"
    ) +
    geom_text(
      data = box_dt,
      aes(x = xmid, y = ymid_t, label = txt_t),
      size = 3, fontface = "bold", color = "gray22", 
      angle = 90, vjust = 0.5, hjust = 0.5
    ) +
    scale_x_continuous(
      breaks = seq(0, 24, by = 2), 
      limits = c(box_dt$xmin, 25), 
      expand = c(0,0)
    ) +
    scale_y_continuous(
      limits = c(-5, max(sampl_rate_sum$perc)+10),
      labels = function(x) paste0(x, "%"),
      expand = c(0,0), 
      position = "right"
    ) +
    theme_bw() +
    theme(
      legend.position = "none", 
      axis.title = element_blank(),
      axis.text.x = element_blank(), 
      axis.ticks.x = element_blank(), 
      plot.title = element_blank(), 
      plot.margin = margin(t = 5, r = 5, b = 0, l = 5)
    ) 
  
  (pt/po) + plot_layout(heights = c(1, 2.5)) +
    plot_annotation(
      title = "Distribution of median sampling rates across deployments",
      subtitle = sprintf(
        "Totaling %d tracks from %d species", n_trks, n_sps)
    ) & 
    theme(
      plot.title = element_text(hjust = 0, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0, size = 10),
      plot.caption = element_text(hjust = 0, size = 10)
    )
  
  ggsave(filename = fout, height = 24, width = 18, unit = "cm")
  
}


# PLOT 3: distance availability -------------------------------------------

# code originally appears in 6_distance.R script

# INPUT
files <- list.files(
  file.path(list.files(study_dir, full.names = T), "6_distances"), 
  pattern = ".*max_active_steps_nauticalDawn_nauticalDusk_continent.rds", 
  full.names = T)

# OUTPUT 
fout <- file.path(graph_dir, "6_daily_distances_per_species_timeline.png")

if(!file.exists(fout)){
  
  source("6_distance_PLOT_FUNCTIONS.R")
  source("0_helper_functions.R")
  
  month_dt <- get_month_limits()
  
  days_space <- 366
  order_space <- 100
  wdth <- 50
  
  # GET DEPLOYMENTS COUNT
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
  
  steps_col <- c("#FFE3A9", "#8CCDEB", "#725CAD", "#0B1D51")
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
    lab = c("number of daily distances per calendar day and species", "order", "number of deployments [log scale]\ndotted line = in Europe")
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
    geom_rect(
      data = order_dt, 
      aes(ymin = sp_min, ymax = sp_max, xmin = xmin, xmax = xmax, fill = order), 
      alpha = 0.4, color = "gray22"
    ) +
    geom_text(
      data = order_dt, 
      aes(y = sp_med, x = days_space+wdth, label = lab), color = "gray22"
    ) +
    scale_fill_manual(
      values = c(ord_col, steps_col), 
      breaks = names(steps_col), 
      labels = c("1", names(steps_col)[-c(1, 4)], "30+")
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
      fill = "N per day", 
      title = "Number of daily distances measured throughout a year",
    ) +
    coord_cartesian(clip = "off") +
    theme_bw() +
    theme(
      axis.ticks = element_blank(), 
      axis.text.y = element_text(family = "FreeSans", size = 6),
      panel.border = element_blank(),
      axis.title.x = element_text(hjust = 0), 
      # legend.text = element_text(size = 8),
      # legend.key.size = unit(0.5, "lines"), 
      # legend.position = c(0.4, 0.97),
      # legend.justification = c(1, 0.5),
      legend.position = "bottom",
      legend.direction = "horizontal", 
      plot.title = element_text(hjust = 0, size = 16, face = "bold")
    ) 
  
  ggsave(fout,  width = 35, height = 45, units = "cm") 
  
  
}


# PLOT 4: filtering -------------------------------------------------------

# INPUT
files <- list.files(
  file.path(list.files(study_dir, full.names = T), "6_distances"), 
  pattern = ".*max_active_steps_nauticalDawn_nauticalDusk_continent.rds", 
  full.names = T)

pname <- "6_data_loss_with_filtering.png"

if(file.exists(file.path(graph_dir, pname))){
  
  source("0_helper_functions.R")
  
  full_dt <- rbindlist(lapply(seq_along(files), function(i){
    
    fin <- files[i]
    fread(fin)[, .(n_file = .N), by = file][
      , species := gsub(".*/Studies/(.*)_(.*)/6_distances/.*", "\\1 \\2", fin)]
    
  }))
  
  filters <- c(0, 10, 30)
  
  filter_col <- c(
    "no filter" = "#BFC0C0", 
    "min N = 10 per track" = "#006E90",
    "min N = 30 per track" = "#011638")
  
  filter_dt <- rbindlist(lapply(filters, function(f){
  
    full_dt[n_file > f][
      , .(n_dpl = uniqueN(file)), by = species][, filter := f]
    
  }))[, filter := paste0("n_dpl_", filter)]
  
  remove(full_dt)
  
  depl_cols <- unique(filter_dt$filter)
  
  filter_wide <- dcast(filter_dt, species ~ filter, value.var = "n_dpl")
  filter_wide[
    , (depl_cols) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), 
    .SDcols = depl_cols]
  filter_wide[, ':=' (
    perc_depl_30 = round(n_dpl_30/n_dpl_0*100, 1), 
    perc_depl_10 = round((n_dpl_10 - n_dpl_30)/n_dpl_0*100, 1), 
    perc_depl_0 = round((n_dpl_0 - n_dpl_10)/n_dpl_0*100, 1)
  )]
  
  p_dt <- melt(
    filter_wide, 
    id.vars = "species",
    measure.vars = c("perc_depl_10", "perc_depl_30", "perc_depl_0"), 
    variable.name = "filter", 
    value.name = "percent"
  )
  
  p_dt[, filter := gsub("perc_depl_", "", filter)]
  p_dt[, filter := factor(
    filter, 
    levels = c("0", "10", "30"),
    labels = names(filter_col))]
  
  wdth <- 10
  
  depl_count <- rbindlist(lapply(seq_along(files), function(i){
    
    fin <- files[i]
    dt <- fread(fin)
    
    data.table(
      n_tracks = uniqueN(dt$file), 
      n_tracks_eu = ifelse(
        any(dt$continent== "Europe"), uniqueN(dt[continent == "Europe", file]), NA),  
      species = gsub(".*/Studies/(.*)_(.*)/6_distances/.*", "\\1 \\2", fin)
    )
    
  }))[, ':=' (
    n_track_log = 120+wdth*log10(n_tracks), 
    n_track_log_eu = 120+wdth*log10(n_tracks_eu))]
  
  logs <- 120 + wdth*c(0, 1, 2, 3)
  log_lab <- c(1, 10, 100, 1000)
    
  depl_count <- add_birdlife_phylogeny(depl_count, species_name = "species")
  depl_count[, ord_max := max(n_tracks), by = order]
  
  
  setorder(depl_count, ord_max, order, n_tracks, species)
  depl_count[, sp_id := .I]
  
  order_dt <- depl_count[, .(
    xmax = max(sp_id) + 0.5, 
    xmin = min(sp_id) - 0.5, 
    xmed = as.numeric(median(sp_id)), 
    n_order = uniqueN(species), 
    ymin = 100, 
    ymax = 120, 
    ymed = 110
  ), by = order]
  order_dt[, lab := ifelse(n_order > 2, order, "")]
  
  p_dt <- merge(p_dt, depl_count[, .(species, sp_id)], by = "species")
  
  dpl_lbl <- data.table(
    xmin = max(p_dt$sp_id), 
    xmax = max(p_dt$sp_id)+10,
    xmed = max(p_dt$sp_id)+5,
    ymin = c(0, 100, 120), 
    ymax = c(100, 120, max(depl_count$n_track_log)), 
    ymed = c(50, 110, mean(c(120, max(depl_count$n_track_log)))),
    lab = c(
      "deployments after filtering [%]", 
      "order", 
      "total number\nof deployments\n[log scale]")
  )
  
  ggplot() +
    geom_rect(
      data = order_dt,
      aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax, 
          fill = order),  color = "gray22",
      alpha = 0.3
    ) +
    geom_text(
      data = order_dt, 
      aes(y = ymed, x = xmed, label = lab), 
      vjust = 0.5, hjust = 0.5, 
      color = "gray22"
    ) +
    geom_rect(
      data = depl_count, 
      aes(ymin = 120, ymax = n_track_log,
          xmin = sp_id-0.5, xmax = sp_id+0.5, fill = order), alpha = 0.6
    ) +
    geom_bar(
      data = p_dt,
      aes(x = sp_id, y = percent, fill = filter), 
      stat = "identity", width = 1, alpha = 0.7
    ) +
    geom_hline(
      aes(yintercept = c(seq(10, 100, 10), logs)), color = "gray22"
    ) +
    geom_vline(
      data = order_dt, aes(xintercept = xmin), color = "gray22"
    ) +
    geom_rect(
      data = dpl_lbl,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "white", color = "gray22"
    ) +
    geom_text(
      data = dpl_lbl,
      aes(x = xmed, y = ymed, label = lab), 
      vjust = c(0, 0.5, 0.5), fontface = "bold", size = 5
    ) +
    coord_flip() +
    scale_fill_manual(
      values = c(ord_col, filter_col), 
      breaks = names(filter_col) 
    ) +
    scale_x_continuous(
      breaks = depl_count$sp_id, 
      labels = depl_count$species, 
      expand = c(0, 0)) +
    scale_y_continuous(
      expand = c(0, 0),
      breaks = c(seq(0, 100, 10), logs), 
      labels = c(paste0(seq(0, 100, 10), "%"), log_lab)
    ) +
    theme_bw() +
    theme(
      axis.ticks = element_blank(), 
      legend.text = element_text(size = 8),
      #legend.key.size = unit(1, "lines"), 
      legend.position = "bottom",
      # legend.position = c(0.45, 0.965),
      # legend.justification = c(1, 0.5),
      legend.direction = "horizontal", 
      plot.title = element_text(hjust = 0, size = 16, face = "bold")
    ) +
    labs(
      x = "", 
      y = "", 
      fill = "filter", 
      title = "Loss of daployments with filtering", 
      subtitle = "filtering criteria = min number of distance measures per deployment"
    )
  
  ggsave(file.path(graph_dir, pname), height = 24, width = 18)
  
  
}



# P-5: SigFox vs. GPS -------------------------------------------------------


# INPUT
files <- list.files(
  file.path(list.files(study_dir, full.names = T), "6_distances"), 
  pattern = ".*max_active_steps_nauticalDawn_nauticalDusk_continent.rds", 
  full.names = T)

pname <- "6_sigfox_vs_gps.png"


if(!file.exists(file.path(graph_dir, pname))){
  
  scl <- 0.5
  perc_space <- 100*scl
  order_space <- 20
  wdth <- 10
  
  source("0_helper_functions.R")
  
  full_dt <- rbindlist(lapply(seq_along(files), function(i){
    
    fin <- files[i]
    
    file_list <- unique(fread(fin)[, file])

    data.table(
      n_tracks = length(file_list),
      n_gps = sum(grepl("dep_653_sen", file_list)),
      n_sigfox = sum(grepl("dep_2299894820_sen", file_list)),
      species = gsub(".*/Studies/(.*)_(.*)/6_distances/.*", "\\1 \\2", fin)
    )
    
  }))
  
  sensor_col <- c("SigFox" = "#FFD35A", "GPS" = "#26355D")
  
  full_dt[, ':=' (
      perc_sigfox = round(n_sigfox/n_tracks*100, 1), 
      perc_gps = round(n_gps/n_tracks*100, 1)
  )]
  
  p_dt <- melt(
    full_dt,
    id.vars = "species",
    measure.vars = c("perc_gps", "perc_sigfox"),
    variable.name = "sensor",
    value.name = "percent"
  )
  p_dt[, sensor := factor(
    sensor,
    levels = c("perc_gps", "perc_sigfox"),
    labels = c("GPS", "SigFox")
  )]
  p_dt[, percent := percent*scl]
  
  depl_count <- rbindlist(lapply(seq_along(files), function(i){
    
    fin <- files[i]
    dt <- fread(fin)
    
    data.table(
      n_tracks = uniqueN(dt$file), 
      n_tracks_eu = ifelse(
        any(dt$continent== "Europe"), uniqueN(dt[continent == "Europe", file]), NA),  
      species = gsub(".*/Studies/(.*)_(.*)/6_distances/.*", "\\1 \\2", fin)
    )
    
  }))[, ':=' (
    n_track_log = perc_space+order_space+wdth*log10(n_tracks), 
    n_track_eu_log = perc_space+order_space+wdth*log10(n_tracks_eu))]
  
  dep_bb <- seq(0, floor(max(log10(depl_count$n_tracks))), 1)
  
  logs <- perc_space+order_space + wdth*dep_bb
  log_lab <- 10^dep_bb
  
  depl_count <- add_birdlife_phylogeny(depl_count, species_name = "species")
  depl_count[, ord_max := max(n_tracks), by = order]
  
  
  setorder(depl_count, ord_max, order, n_tracks, species)
  depl_count[, sp_id := .I]
  
  order_dt <- depl_count[, .(
    xmax = max(sp_id) + 0.5, 
    xmin = min(sp_id) - 0.5, 
    xmed = as.numeric(median(sp_id)), 
    n_order = uniqueN(species), 
    ymin = perc_space, 
    ymax = perc_space+order_space, 
    ymed = perc_space+ order_space/2
  ), by = order]
  order_dt[, lab := ifelse(n_order > 2, order, "")]
  
  p_dt <- merge(p_dt, depl_count[, .(species, sp_id)], by = "species")
  
  dpl_lbl <- data.table(
    xmin = max(p_dt$sp_id), 
    xmax = max(p_dt$sp_id)+10,
    xmed = max(p_dt$sp_id)+5,
    ymin = c(0, perc_space, perc_space+order_space), 
    ymax = c(perc_space, perc_space+order_space, max(depl_count$n_track_log)), 
    ymed = c(
      perc_space/2, 
      perc_space+order_space/2, 
      perc_space+order_space+max(log10(depl_count$n_tracks))*wdth/2
    ),
    lab = c(
      "deployments GPS vs. SigFox [%]", 
      "order", 
      "total number\nof deployments\n[log scale]")
  )
  
  ggplot() +
    geom_rect(
      data = order_dt,
      aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax, 
          fill = order),  color = "gray22", alpha = 0.3
    ) +
    geom_text(
      data = order_dt, 
      aes(y = ymed, x = xmed, label = lab), 
      vjust = 0.5, hjust = 0.5, 
      color = "gray22"
    ) +
    geom_rect(
      data = depl_count, 
      aes(ymin = perc_space+order_space, ymax = n_track_log,
          xmin = sp_id-0.5, xmax = sp_id+0.5, fill = order), alpha = 0.6
    ) +
    geom_segment(
      data = depl_count, 
      aes(y = perc_space+order_space, yend = n_track_eu_log, x = sp_id), 
      alpha = 0.8, color = "white", linewidth = 1, linetype = "dotted"
    ) +
    geom_bar(
      data = p_dt,
      aes(x = sp_id, y = percent, fill = sensor), 
      stat = "identity", width = 1, alpha = 0.7
    ) +
    geom_hline(
      aes(yintercept = c(seq(10, 100, 10)*scl, logs)), color = "gray22"
    ) +
    geom_vline(
      data = order_dt, aes(xintercept = xmin), color = "gray22"
    ) +
    geom_rect(
      data = dpl_lbl,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "white", color = "gray22"
    ) +
    geom_text(
      data = dpl_lbl,
      aes(x = xmed, y = ymed, label = lab), 
      vjust = c(0, 0.5, 0.5), fontface = "bold", size = 5
    ) +
    coord_flip() +
    scale_fill_manual(
      values = c(ord_col, sensor_col), 
      breaks = names(sensor_col) 
    ) +
    scale_x_continuous(
      breaks = depl_count$sp_id, 
      labels = depl_count$species, 
      expand = c(0, 0)) +
    scale_y_continuous(
      expand = c(0, 0),
      breaks = c(seq(0, 100, 10)*scl, logs), 
      labels = c(paste0(seq(0, 100, 10), "%"), log_lab)
    ) +
    theme_bw() +
    theme(
      axis.ticks = element_blank(), 
      #legend.text = element_text(size = 8),
      #legend.key.size = unit(1, "lines"), 
      legend.position = "bottom",
      legend.direction = "horizontal", 
      plot.title = element_text(hjust = 0, size = 16, face = "bold")
    ) +
    labs(
      x = "", 
      y = "", 
      fill = "sensor", 
      title = "Deployments with GPS vs. SigFox"
    )
  
  ggsave(file.path(graph_dir, pname), height = 26, width = 15)
  
  
}



# relocations -------------------------------------------------------------

file_dep_filter <- "1_deployments_to_download.csv"
deployments <- fread(file.path(data_dir, file_dep_filter))[
  , file_id := paste0(
    study_id, "_stu_", individual_id, "_ind_", deployment_id)][
  , .(file_id, manipulation_type)]

# INPUT
files <- list.files(
  file.path(list.files(study_dir, full.names = T), "6_distances"), 
  pattern = ".*max_active_steps_nauticalDawn_nauticalDusk_continent.rds", 
  full.names = T)

pname <- "6_relocations.png"


if(!file.exists(file.path(graph_dir, pname))){
  
  scl <- 0.5
  perc_space <- 100*scl
  order_space <- 20
  wdth <- 10
  
  source("0_helper_functions.R")
  
  full_dt <- rbindlist(lapply(seq_along(files), function(i){
    
    fin <- files[i]
    
    dt <- fread(fin)[, file_id := gsub("_dep_.*$", "", basename(file))]
    
    dt <- merge(dt, deployments, by = "file_id", all.x = T)
    
    dt <- unique(dt[, .(file, manipulation_type)])
    
    dt[, .(
        n_tracks = uniqueN(file), 
        n_relocated = sum(manipulation_type == "relocated"),
        n_none = sum(manipulation_type == "none"))][
      , species := gsub(".*/Studies/(.*)_(.*)/6_distances/.*", "\\1 \\2", fin)]
    
  }))
  
  manipulation_col <- c("relocation" = "#f7aef8", "none" = "#2e294e")
  
  full_dt[, ':=' (
    perc_relocated = round(n_relocated/n_tracks*100, 1), 
    perc_none = round(n_none/n_tracks*100, 1)
  )]
  
  p_dt <- melt(
    full_dt,
    id.vars = "species",
    measure.vars = c("perc_relocated", "perc_none"),
    variable.name = "manipulation",
    value.name = "percent"
  )
  p_dt[, manipulation := factor(
    manipulation,
    levels = c("perc_relocated", "perc_none"),
    labels = c("relocation", "none")
  )]
  p_dt[, percent := percent*scl]
  
  depl_count <- rbindlist(lapply(seq_along(files), function(i){
    
    fin <- files[i]
    dt <- fread(fin)
    
    data.table(
      n_tracks = uniqueN(dt$file), 
      n_tracks_eu = ifelse(
        any(dt$continent== "Europe"), uniqueN(dt[continent == "Europe", file]), NA),  
      species = gsub(".*/Studies/(.*)_(.*)/6_distances/.*", "\\1 \\2", fin)
    )
    
  }))[, ':=' (
    n_track_log = perc_space+order_space+wdth*log10(n_tracks), 
    n_track_eu_log = perc_space+order_space+wdth*log10(n_tracks_eu))]
  
  dep_bb <- seq(0, floor(max(log10(depl_count$n_tracks))), 1)
  
  logs <- perc_space+order_space + wdth*dep_bb
  log_lab <- 10^dep_bb
  
  depl_count <- add_birdlife_phylogeny(depl_count, species_name = "species")
  depl_count[, ord_max := max(n_tracks), by = order]
  
  
  setorder(depl_count, ord_max, order, n_tracks, species)
  depl_count[, sp_id := .I]
  
  order_dt <- depl_count[, .(
    xmax = max(sp_id) + 0.5, 
    xmin = min(sp_id) - 0.5, 
    xmed = as.numeric(median(sp_id)), 
    n_order = uniqueN(species), 
    ymin = perc_space, 
    ymax = perc_space+order_space, 
    ymed = perc_space+ order_space/2
  ), by = order]
  order_dt[, lab := ifelse(n_order > 2, order, "")]
  
  p_dt <- merge(p_dt, depl_count[, .(species, sp_id)], by = "species")
  
  dpl_lbl <- data.table(
    xmin = max(p_dt$sp_id), 
    xmax = max(p_dt$sp_id)+10,
    xmed = max(p_dt$sp_id)+5,
    ymin = c(0, perc_space, perc_space+order_space), 
    ymax = c(perc_space, perc_space+order_space, max(depl_count$n_track_log)), 
    ymed = c(
      perc_space/2, 
      perc_space+order_space/2, 
      perc_space+order_space+max(log10(depl_count$n_tracks))*wdth/2
    ),
    lab = c(
      "deployments relocation vs. none [%]", 
      "order", 
      "total number\nof deployments\n[log scale]")
  )
  
  ggplot() +
    geom_rect(
      data = order_dt,
      aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax, 
          fill = order),  color = "gray22", alpha = 0.3
    ) +
    geom_text(
      data = order_dt, 
      aes(y = ymed, x = xmed, label = lab), 
      vjust = 0.5, hjust = 0.5, 
      color = "gray22"
    ) +
    geom_rect(
      data = depl_count, 
      aes(ymin = perc_space+order_space, ymax = n_track_log,
          xmin = sp_id-0.5, xmax = sp_id+0.5, fill = order), alpha = 0.6
    ) +
    geom_segment(
      data = depl_count, 
      aes(y = perc_space+order_space, yend = n_track_eu_log, x = sp_id), 
      alpha = 0.8, color = "white", linewidth = 1, linetype = "dotted"
    ) +
    geom_bar(
      data = p_dt,
      aes(x = sp_id, y = percent, fill = manipulation), 
      stat = "identity", width = 1, alpha = 0.7
    ) +
    geom_hline(
      aes(yintercept = c(seq(10, 100, 10)*scl, logs)), color = "gray22"
    ) +
    geom_vline(
      data = order_dt, aes(xintercept = xmin), color = "gray22"
    ) +
    geom_rect(
      data = dpl_lbl,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "white", color = "gray22"
    ) +
    geom_text(
      data = dpl_lbl,
      aes(x = xmed, y = ymed, label = lab), 
      vjust = c(0, 0.5, 0.5), fontface = "bold", size = 5
    ) +
    coord_flip() +
    scale_fill_manual(
      values = c(ord_col, manipulation_col), 
      breaks = names(manipulation_col) 
    ) +
    scale_x_continuous(
      breaks = depl_count$sp_id, 
      labels = depl_count$species, 
      expand = c(0, 0)) +
    scale_y_continuous(
      expand = c(0, 0),
      breaks = c(seq(0, 100, 10)*scl, logs), 
      labels = c(paste0(seq(0, 100, 10), "%"), log_lab)
    ) +
    theme_bw() +
    theme(
      axis.ticks = element_blank(), 
      #legend.text = element_text(size = 8),
      #legend.key.size = unit(1, "lines"), 
      legend.position = "bottom",
      legend.direction = "horizontal", 
      plot.title = element_text(hjust = 0, size = 16, face = "bold")
    ) +
    labs(
      x = "", 
      y = "", 
      fill = "manipulation", 
      title = "Deployments with relocation vs. none"
    )
  
  ggsave(file.path(graph_dir, pname), height = 26, width = 15)
  
  
}




