library(data.table)
library(ggplot2)
library(ggdist)
library(patchwork)
source("0_helper_functions.R")
source("6_distance_PLOT_FUNCTIONS.R")
source("color_palettes.R")

data_dir <- here::here("Data")
study_dir <- file.path(data_dir, "Studies")
graphs_dir <- file.path(data_dir, "Graphs")

dist_dirs <- file.path(list.files(study_dir, full.names = T), "6_distances")
target_sp <- gsub(".*/Studies/(.*?)_(.*?)/6_distances.*", "\\1 \\2", dist_dirs)

traits_dt <- fread(here::here("Published_data", "00_bird_traits.csv"))
traits_dt[, activity := ifelse(nocturnal == 1, "nocturnal", "diurnal")]
traits_dt <- unique(traits_dt[
  , .(species = birdlife_name, migration = migration_txt, 
      activity, order, family)])
traits_dt <- traits_dt[species %in% target_sp]

rm(target_sp)

# in case we want to check manipulations
# deployments <- fread(file.path(data_dir, "1_deployments_to_download.csv"))
# 
# deployments[, track_id := make_file_name(
#   study_id, individual_id, deployment_id, file_type =  "")]


# 1 - Make overview table ------------------------------------------------------

fout <- "6_overview_filter_20_max_active_steps_nauticalDawn_nauticalDusk_continent.csv"

finname <- "4_.*_max_active_steps_nauticalDawn_nauticalDusk_continent.rds"

n_fltr <- 20

files <- list.files(dist_dirs, pattern = finname, full.names = TRUE)
nf <- length(files)

if(!file.exists(file.path(data_dir, fout))){
  
  species_dt <- rbindlist(lapply(seq_along(files), function(n){
    
    fin <- files[n]
    
    dt <- fread(fin)
    
    # only keep the days in which maximum daily distance was at least 50m
    # double the average GPS error, otherwise we assume no movement
    dt <- dt[sl_ >= 50 & (sl_median >= 50 | is.na(sl_median))]
    
    # filter tracks that have less than 10 steps
    dt <- dt[, n_steps := .N, by = file][n_steps >= n_fltr]
    
    if(nrow(dt) == 0){ return(NULL) }
    
    fwrite(dt, gsub(".rds", "_fltr_20_dist_50m.rds", fin))
    
    dt[, yd := day_cycle_to_yd(day_cycle_1)]
    # extract sensor
    dt[, sensor := sub(".*dep_(.*?)_sen.*", "\\1", file)][
      , sensor := fcase(sensor == 653, "gps", sensor == 2299894820, "sigfox")]
    
    # get number of deployments
    n_deploy_tot <- dt[, uniqueN(file)]
    
    # get track lengths and sensor
    dt_per_file <- dt[, .(
      n_days = uniqueN(day_cycle_1),
      n_year_days = uniqueN(yd), 
      sensor = unique(sensor), 
      n_steps = unique(n_steps)
    ), by = file]
    
    # remove duplicated trakcs - deployments with both gps and sigfox
    dt_per_file[, track_id := sub("_dep_\\d+.*$", "", basename(file))]
    
    setorder(dt_per_file, sensor)
    
    dt_per_file[, duplicated_track := duplicated(track_id)]
    dpltrks <- dt_per_file[duplicated_track == T, file]
    
    dt_per_file <- dt_per_file[duplicated_track == F]
    dt <- dt[!file %in% dpltrks]
    
    n_steps_tot <- sum(dt_per_file$n_steps)
    
    cls <- c("n_days", "n_year_days")
    # extract stats for tracking times - days of tracking
    dt_per_file <- dt_per_file[, c(
      as.list(setNames(lapply(.SD, median), paste0("median_", cls))),
      as.list(setNames(lapply(.SD, function(x) {
        if(n_deploy_tot == 1) NA else paste(range(x), collapse = " - ") }),
        paste0("range_", cls))),
      N_gps = sum(sensor == "gps"),
      N_sigfox = sum(sensor == "sigfox"),
      Shennon_n_steps = -sum(n_steps/n_steps_tot * log10(n_steps/n_steps_tot))
    ), .SDcols = cls]
    
    dt[, yd := day_cycle_to_yd(day_cycle_1)]
    # extract sensor
    dt[, sensor := sub(".*dep_(.*?)_sen.*", "\\1", file)]
    
    setnames(dt, old = "sl_median", new = "sl_median_old")
    
    # when there was only one distance available no stats was calculated
    dt[, sl_median := fifelse(
      sl_n_steps == 1, as.numeric(sl_), as.numeric(sl_median_old))] 
    
   
    dt_out <- dt[, c(
      species = gsub(".*/Studies/(.*?)_(.*?)/6_distances.*", "\\1 \\2", fin),
      n_steps = .N, 
      n_days = uniqueN(day_cycle_1),
      n_year_days = uniqueN(yd),
      n_deployments = uniqueN(file), 
      max_sl_mean = mean(sl_),
      as.list(setNames(
        round(quantile(
          sl_, probs = c(0.05, 0.25, 0.5, 0.75, 0.95)), 2), 
        paste0("max_sl_", c("05", "25", "50", "75", "95")))), 
      median_sl_mean = mean(sl_),
      as.list(setNames(
        round(quantile(
          sl_median, probs = c(0.05, 0.25, 0.5, 0.75, 0.95)), 2), 
        paste0("sl_", c("q05", "q25", "median", "q75", "q95")))), 
      continents = paste(unique(continent), collapse = ", ")
    )]
    
    cbind(dt_out, dt_per_file)
    
  }), fill = T)
  
  # add traits and phylogeny
  species_dt <- merge(species_dt, traits_dt, by = "species", all.x = T)
  
  cls <- c(
    "species", "family", "order", "migration", "activity", "n_deployments")
  
  setcolorder(species_dt, neworder = cls)
  
  fwrite(species_dt, file.path(data_dir, fout))
  
}

cat(finm, "DONE!\n")

invisible(return(NULL))

gc(verbose = F)

rm(fdt)






# # 2 - Stat param overview -----------------------------------------------------
# 
# # INPUT
# fin_name <- "4_all_tracks_max_active_steps_nauticalDawn_nauticalDusk_continent.rds"
# 
# 
# 
# species_dt <- rbindlist(lapply(seq_along(files), function(n){
#   
#   fin <- files[n]
#   
#   dt <- fread(fin)
#   
#   # filter tracks that have less than 10 steps
#   dt <- dt[, n_steps := .N, by = file][n_steps >= n_fltr]
#   
#   if(nrow(dt) == 0){ return(NULL) }
#   
#   dt[, yd := day_cycle_to_yd(day_cycle_1)]
#   # extract sensor
#   dt[, sensor := sub(".*dep_(.*?)_sen.*", "\\1", file)][
#     , sensor := fcase(sensor == 653, "gps", sensor == 2299894820, "sigfox")]
#   
#   n_deploy_tot <- dt[, uniqueN(file)]
#   
#   # get track lengths and sensor
#   dt_per_file <- dt[, .(
#     n_days = uniqueN(day_cycle_1),
#     n_year_days = uniqueN(yd), 
#     sensor = unique(sensor), 
#     n_steps = unique(n_steps)
#   ), by = file]
#   
#   # remove duplicated trakcs - deployments with both gps and sigfox
#   dt_per_file[, track_id := sub("_dep_\\d+.*$", "", basename(file))]
#   
#   setorder(dt_per_file, sensor)
#   
#   dt_per_file[, duplicated_track := duplicated(track_id)]
#   dpltrks <- dt_per_file[duplicated_track == T, file]
#   
#   dt_per_file <- dt_per_file[duplicated_track == F]
#   dt <- dt[!file %in% dpltrks]
#   
#   cls <- c("n_days", "n_year_days")
#   
#   n_steps_tot <- sum(dt_per_file$n_steps)
#   
#   # extract stats for tracking times
#   dt_per_file <- dt_per_file[, c(
#     as.list(setNames(lapply(.SD, median), paste0("median_", cls))),
#     as.list(setNames(lapply(.SD, function(x) {
#       if(n_deploy_tot == 1) NA else paste(range(x), collapse = " - ") }),
#       paste0("range_", cls))),
#     N_gps = sum(sensor == "gps"),
#     N_sigfox = sum(sensor == "sigfox"), 
#     Shennon_n_steps = -sum(n_steps/n_steps_tot * log10(n_steps/n_steps_tot))
#   ), .SDcols = cls]
#   
#   dt[, yd := day_cycle_to_yd(day_cycle_1)]
#   # extract sensor
#   dt[, sensor := sub(".*dep_(.*?)_sen.*", "\\1", file)]
#   
#   
#   if(dist_col == "sl_median"){ 
#     setnames(dt, old = "sl_median", new = "sl_median_old")
#     # when there was only one distance available no stats was calculated
#     dt[, sl_median := fifelse(
#       sl_n_steps == 1, as.numeric(sl_), as.numeric(sl_median_old))] 
#   }
#   
#   dt_out <- dt[, c(
#     species = gsub(".*/Studies/(.*?)_(.*?)/6_distances.*", "\\1 \\2", fin),
#     N_steps = .N, 
#     N_days = uniqueN(day_cycle_1),
#     N_year_days = uniqueN(yd),
#     N_deployments = uniqueN(file), 
#     sl_mean = mean(get(dist_col)),
#     as.list(setNames(
#       round(quantile(
#         get(dist_col), probs = c(0.05, 0.25, 0.5, 0.75, 0.95)), 2), 
#       paste0("sl_", c("05", "25", "50", "75", "95")))), 
#     continents = paste(unique(continent), collapse = ", "), 
#     sensors = paste(unique(sensor), collapse = ", ")
#   )]
#   
#   cbind(dt_out, dt_per_file)
#   
# }), fill = T)
# 
# 
# files <- list.files(data_dir, pattern = "6_overview.*\\.csv", full.names = T)
# 
# dt_overview <- fread(
#   list.files(
#     data_dir, 
#     pattern = ".*filter_30_max_active_steps.*continent.csv", 
#     full.names = T)
# )
# n_fltr <- 30
# 
# target_sp <- dt_overview[, species]
# 
# 
# disp_fit_dir <- file.path(data_dir, "Disp_fits")
# dir.create(disp_fit_dir, showWarnings = F)

# Fit dispersal -----------------------------------------------------------

# never complated
# 
# dt_overview <- fread(
#   list.files(
#     data_dir, 
#     pattern = ".*filter_30_max_active_steps.*continent.csv", 
#     full.names = T)
# )
# n_fltr <- 30
# 
# target_sp <- dt_overview[, species]
# rm(dt_overview)
# 
# dist_dirs <- file.path(study_dir, gsub(" ", "_", target_sp), "6_distances")
# files <- list.files(
#   dist_dirs, ".*max_active.*nauticalDawn.*continent.rds", full.names = T)
# rm(target_sp)
# 
# dt_steps <- rbindlist(lapply(seq_along(files), function(i){
#   
#   fin <- files[i]
#   sp <- gsub(".*Studies/(.*)_(.*)/6_distances/.*", "\\1 \\2", fin)
#   
#   dt <- fread(fin)
#   
#   # filter tracks that have less than 10 steps
#   dt <- dt[, n_steps := .N, by = file][n_steps >= n_fltr]
#   
#   # extract sensor
#   dt[, sensor := sub(".*dep_(.*?)_sen.*", "\\1", file)][
#     , sensor := fcase(sensor == 653, "gps", sensor == 2299894820, "sigfox")]
#   
#   # get track lengths and sensor
#   dt_per_file <- dt[, .(sensor = unique(sensor)), by = file]
#   
#   # remove duplicated trakcs - deployments with both gps and sigfox
#   dt_per_file[, track_id := sub("_dep_\\d+.*$", "", basename(file))]
#   
#   setorder(dt_per_file, sensor)
#   
#   dt_per_file[, duplicated_track := duplicated(track_id)]
#   dpltrks <- dt_per_file[duplicated_track == T, file]
#   rm(dt_per_file)
#   
#   dt <- dt[, .(file, sl_, day_cycle_1)]
#   
#   dt[, .(
#     mean_sl = mean(sl_),
#     median_sl = median(sl_),
#     sd_sl = sd(sl_),
#     cv_sl = sd(sl_)/mean(sl_),
#     IQR_sl = IQR(sl_), 
#     skewness_sl = e1071::skewness(sl_), 
#     kurtosis_sl = e1071::kurtosis(sl_), 
#     unimodal = ifelse(diptest::dip.test(sl_)$p.value < 0.05, 0, 1), 
#     mode_kde = {
#       dens <- density(sl_, na.rm=TRUE)
#       dens$x[which.max(dens$y)]
#     },
#     n_steps = .N, 
#     skewness_sl_log = e1071::skewness(log10(sl_)), 
#     kurtosis_sl_log = e1071::kurtosis(log10(sl_)), 
#     unimodal_log = ifelse(diptest::dip.test(sl_)$p.value < 0.05, 0, 1), 
#     mode_kde_log = density(log10(sl_))$x[which.max(dens$y)]
#   )][, species = sp]
# 
# }))
# 




# Compare with Fandos -----------------------------------------------------

# fin <- "6_overview_filter_30_max_active_steps_nauticalDawn_nauticalDusk_continent_sl_median.csv"
# 
# 
# dt <- fread(file.path(data_dir, fin))
# 
# fandos_dt <- fread(here::here(
#   "Published_data", "Fandos2023_Table_S14_ species_dispersal_distances_v1_0_2.csv"))
# 
# fandos_dt <- rename_to_birdlife(fandos_dt, "species")
# 
# 
# fandos_dt <- fandos_dt[
#   birdlife_name %in% dt$species, .(median, birdlife_name, function_id)]
# fandos_avr <- fandos_dt[
#   , .(sl_50_avr = mean(median, na.rm = TRUE)), by = birdlife_name]
# 
# 
# merged_dt <- merge(fandos_avr, dt, by.x = "birdlife_name", by.y = "species")
# 
# merged_dt[, order_txt := paste0(order, " [", .N, "]"), by = order]
# 
# matched_idx <- match(names(ord_col), merged_dt$order)
# names(ord_col) <- merged_dt$order_txt[matched_idx]
# 
# 
# merged_dt |> 
#   ggplot() +
#   geom_point(
#     aes(x = sl_50_avr, y = sl_50/1000, color = order_txt), size = 3, alpha = 0.5
#   ) + 
#   ggrepel::geom_text_repel(
#     aes(x = sl_50_avr, y = sl_50/1000, label = birdlife_name), size = 3
#   ) +
#   scale_color_manual(values = ord_col) +
#   theme_bw() + 
#   scale_y_log10() + 
#   scale_x_log10() + 
#   labs(
#     x = "Fandos et al. (2023) median dispersal distance [km]",
#     y = "Median daily movement [km]", 
#     color = "order [number of species]", 
#     title = "Comparison of median daily movement estimates with Fandos et al. (2023) dispersal distances",
#     subtitle = "dispersal distances were represented by mean over median estimates for all models"
#   ) 
# 
# 
# ggsave(
#   filename = file.path(graphs_dir, "6_median_sl_compare_Fandos2023.png"),
#   width = 10, height = 6
# )



# PLOT: median sl_ per species -------------------------------------------------

files <- list.files(data_dir, pattern = "6_overview.*\\.csv", full.names = T)
grupings <- c("migration", "order")

# check which already done
files <- unique(unlist(lapply(grupings, function(o){
  
  pname <- sprintf(gsub(".csv", "_%s.png", basename(files)), o)
  
  return(files[!file.exists(file.path(graphs_dir, "6_distances", pname))])
  
})))

nf <- length(files)

# AVONET migration categories
#mlvl <- c("sedentary", "partial", "migrant")
mlbl <- c("sedentary", "partial migrant", "migrant")
move_col <- c("#A3DA8D", "#F4A460", "#781D42")
names(move_col) <- mlbl


if(nf > 0){
  
  legend_dt <- data.table(
    y = 5,
    xmin = 1,
    xmax = 10,
    x_ppt = c(NA, 6, NA,  13),
    y_txt = c(4, 2, 4, 2),
    y_arrow = c(NA, 3, NA, 3),
    x = c(1, 6, 10, 13),
    ppt = c(NA, "med", NA, NA),
    ppt_txt = c(
      "Q1 [25%]", 
      "median \nQ2 [50%]", 
      "Q3 [75%]", 
      "number of\ntracks"
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
    annotate(
      "text", x = 13, y = 5, label = "N", size = 5, color = "gray11"
    ) +
    scale_y_continuous(expand = c(0.2, 0.2)) + 
    scale_x_continuous(expand = c(0.3, 0.3)) +
    scale_shape_manual(values = c("mod" = 23, "med" = 15), na.value = NA) +
    theme_void() + 
    labs(title = "Legend") +
    theme(
      legend.position = "none",
      plot.background = element_rect(
        fill = "white", color = "gray22", linewidth = 1.2), 
      plot.title = element_text(
        hjust = 0.5, vjust = 0, size = rel(1), face = "bold")
    )
  
  
  lapply(seq_along(files), function(n){
    
    fin <- files[n]
    
    species_dt <- fread(fin)
    
    species_dt <- species_dt[
      , .(species, N_steps, N_deployments, order, 
          sl_50, sl_25, sl_75, migration)]
    
    # make factors - it included birdlife migration 
    species_dt[, migration := fcase(
      # migration == "altitudinal migrant", "altitude migrant",
      migration == "partially migratory", "partial migrant",
      migration %in% c("migratory", "full migrant"), "migrant",
      migration == "not a migrant", "sedentary",
      default = migration)]
    species_dt[
      , migration := factor(migration, levels = mlbl)]
    
    
    sl_cols <- c("sl_50", "sl_25", "sl_75")
    species_dt[
      , (sl_cols) := lapply(.SD, function(x) x / 1000), .SDcols = sl_cols]
    
    # set order of 
    species_dt[, sl_order := max(sl_50), by = order]
    
    setorder(species_dt, sl_order)
    
    lapply(grupings, function(o){
      
      if(o == "migration"){
        ordv <- c("migration", "sl_order") 
        by1 <- grupings
        by2 <- o
      } else {
        ordv <- c("sl_order", "migration")
        by1 <- o
        by2 <- grupings
      } 
      
      ordv <- c(ordv, "sl_50")
      
      # set order and assign a number to the species name
      setorderv(species_dt, cols = ordv)
      species_dt[, sp_id := .I]
      
      order_dt <- species_dt[, .(
        ymax = max(sp_id), 
        ymin = min(sp_id), 
        ymed = as.numeric(median(sp_id)), 
        count = max(sp_id) - min(sp_id) + 1), 
        by = by1]
      order_dt[, lab := ifelse(count > 2, order, "")]
      
      move_dt <- species_dt[, .(
        ymax = max(sp_id), 
        ymin = min(sp_id), 
        ymed = median(sp_id)), 
        by = by2]
      
      max75 <- max(species_dt$sl_75)
      
      if(max75 < 100) {
        
        pscl <- c(-2, -8, -30)
        
      } else{ 
        
        pscl <- c(-max75*0.02, -max75*0.08, -max75*0.33)
        
      }
      
      subtit <- ifelse(
        grepl("median", fin), "median daily distance", "maximum daily distance")
      
      subtit <- ifelse(
        grepl("median_active", fin), 
        paste(subtit, "- median sleep to median active locations"), 
        paste(subtit, "- median sleep to all active locations")
      )
      
      p <- species_dt |> 
        ggplot() +
        geom_point(
          aes(y = sp_id, x = sl_50, color = migration), size = 3, shape = 15,
          alpha = 0.6
        ) +
        geom_errorbar(
          aes(y = sp_id, xmin = sl_25, xmax = sl_75, color = migration), 
          linewidth = 1.2, alpha = 0.6
        ) +
        geom_text(
          aes(y = sp_id, x = sl_75-pscl[1], label = N_deployments), 
          vjust = 0.5, hjust = 0.5, color = "gray33", size = 2
        ) +
        geom_rect(
          data = move_dt,
          aes(xmin = pscl[2], xmax = pscl[1], ymin = ymin - 0.5, ymax = ymax + 0.5, 
              fill = migration), alpha = 0.6, color = "gray33"
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
          breaks = species_dt$sp_id, 
          labels = species_dt$species, 
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
        scale_fill_manual(values = c(ord_col, move_col)) +
        scale_color_manual(values = move_col, labels = mlbl) +
        labs(
          x = "distance [km]", 
          title =  sprintf(
            "Median daily distance with [Q1-Q3] range across species, by %s", o), 
          subtitle = subtit, 
          caption = sprintf("file: %s", basename(fin))
        ) +
        theme(
          axis.title.y = element_blank(), 
          axis.ticks.y = element_blank(),
          legend.position = "none"
        )
      
      
      if(o == "migration"){
        
        p <- p + geom_text(
          data = move_dt, 
          aes(y = ymed, x = pscl[1]+(pscl[2]-pscl[1])/2, label = migration), 
          angle = 90, hjust = 0.5, vjust = 0.5, color = "gray11"
        )
        
      }
      
      
      p +  inset_element(
        legend,
        left = 0.70,   # fraction of plot width from left (0 to 1)
        bottom = 0.01, # fraction of plot height from bottom (0 to 1)
        right = 0.99,  # fraction of plot width from left (0 to 1)
        top = 0.16  # fraction of plot height from bottom (0 to 1)
      )
      
      pname <- sprintf(gsub(".csv", "_%s.png", basename(fin)), o)
      
      ggsave(
        file.path(graphs_dir, "6_distances", pname), 
        width = 12, height = 16, dpi = 300
      )
      
      return(NULL)
      
    })
    
  })
  
  
}

rm(files, nf, grupings)

# PLOT: median sl_ per migration ---------------------------------------------------

filters <- c("filter_30", "filter_10")

files_out <- sprintf("6_%s_species_median_by_migration_halfeye.png", filters)

filters <- filters[
  !file.exists(file.path(graphs_dir, "6_distances", files_out))]
nf <- length(filters)

if(nf > 0){
  
  lapply(filters, function(f){
    
    files <- list.files(
      data_dir, 
      pattern = sprintf("6_overview_%s.*\\.csv", f), full.names = T)
    
    full_dt <- rbindlist(lapply(seq_along(files), function(n){
      
      fin <- files[n]
      
      species_dt <- fread(fin)
      
      species_dt <- species_dt[, .(species, sl_50, migration)]
      
      species_dt[, sl_50 := sl_50/1000]
      
      # make factors
      species_dt[, migration := fcase(
        migration == "partially migratory", "partial", 
        migration == "migratory", "migrant", 
        default = migration)]
      species_dt[
        , migration := factor(
          migration, 
          levels = c("sedentary", "partial", "migrant"), 
          labels = c("sedentary", "partial migrant", "migrant"))]
      
      subtit <- ifelse(
        grepl("median", fin), "median daily distance", "maximum daily distance")
      
      subtit <- ifelse(
        grepl("median_active", fin), 
        paste(subtit, "- median sleep to median active locations"), 
        paste(subtit, "- median sleep to all active locations")
      )
      
      tst <- kruskal.test(sl_50 ~ migration, data = species_dt)
      
      species_dt[, step_type := subtit]
      
      species_dt[, kruskal_p := sprintf(
        "Kruskal-Wallis test:\n p = %f", tst$p.value)]
      
    }))
    
    lbl_dt <- unique(full_dt[, .(step_type, kruskal_p, migration)])
    
    full_dt[, migration_txt := fifelse(
      grepl("partial", migration), 
      paste0(migration, " migrant [N = ", .N, "]"), 
      paste0(migration, " [N = ", .N, "]")),
      by = .(step_type, migration)]
    
    mtxt <- unique(full_dt$migration_txt)
    
    
    full_dt |> 
      ggplot(aes(x = sl_50, y = migration, fill = migration)) + 
      stat_slabinterval(alpha = 0.8) +
      geom_text(
        data = lbl_dt, 
        aes(x = 60, y = 2, label = kruskal_p), 
        hjust = 0, vjust = 0.5, size = 3.5, color = "gray33"
      ) +
      scale_y_discrete(
        limits = c("sedentary", "partial", "migrant"), 
        labels = c(
          grep("sedentary", mtxt, value = T), 
          grep("partial", mtxt, value = T), 
          grep("^migrant", mtxt, value = T))
      ) +
      scale_fill_manual(values = move_col) + 
      theme_bw() + 
      theme(legend.position = "none") +
      facet_wrap(~step_type, ncol = 1) +
      labs(
        x = "median daily distance per species [km]", 
        y = "", 
        title = sprintf(
          "Distribution of median values per species\nseparated by migration status - %s", f), 
        subtitle = "with different ways of calculating and summarizing daily distance"
      )
    
    pname <- sprintf("6_%s_species_median_by_migration_halfeye.png", f)
    
    ggsave(
      file.path(graphs_dir, "6_distances", pname), 
      width = 8, height = 8, dpi = 300)
    
    
    full_dt |> 
      ggplot(aes(x = sl_50, y = migration, fill = migration)) + 
      geom_violin(alpha = 0.8) +
      geom_text(
        data = lbl_dt, 
        aes(x = 60, y = 2, label = kruskal_p), 
        hjust = 0, vjust = 0.5, size = 3.5, color = "gray33"
      ) +
      scale_y_discrete(
        limits = c("sedentary", "partial", "migrant"), 
        labels = c(
          grep("sedentary", mtxt, value = T), 
          grep("partial", mtxt, value = T), 
          grep("^migrant", mtxt, value = T))
      ) +
      scale_fill_manual(values = move_col) + 
      theme_bw() + 
      theme(legend.position = "none") +
      facet_wrap(~step_type, ncol = 1) +
      labs(
        x = "median daily distance per species [km]", 
        y = "", 
        title = sprintf(
          "Distribution of median values per species\nseparated by migration status - %s", f), 
        subtitle = "with different ways of calculating and summarizing daily distance"
      ) 
    
    pname <- sprintf("6_%s_species_median_by_migration_violin.png", f)
    
    ggsave(
      file.path(graphs_dir, "6_distances", pname), 
      width = 8, height = 8, dpi = 300)
    
    
  })
  
  
  # kruskal.test(sl_50 ~ migration, data = species_dt)
  # 
  # # Pairwise comparisons
  # # "BH" uses the Benjamini-Hochberg correction for multiple testing
  # pairwise.wilcox.test(
  #   species_dt$sl_50, species_dt$migration, p.adjust.method = "BH")
  
  
}

rm(filters, files_out, nf)



# PLOT: Correlation between median values -----------------------------------

# correlation shown for all steps, without filtering the number of steps per 
# species

file_max <- "4_all_tracks_max_active_steps_nauticalDawn_nauticalDusk_continent.rds"
file_med <- "3_all_tracks_median_active_steps_nauticalDawn_nauticalDusk_continent.rds"

files <- list.files(dist_dirs, pattern = file_max, full.names = T)
nf <- length(files)

full_dt <- rbindlist(lapply(seq_along(files), function(i){
  
  fin_max <- files[i]
  fin_med <- gsub(file_max, file_med, fin_max)
  
  # prepare maximum file 
  dt_max <- fread(fin_max)
  dt_max[, sl_median := fifelse(
    sl_n_steps == 1, as.numeric(sl_), as.numeric(sl_median))]
  dt_max <- dt_max[, .(sl_max = sl_median, day_cycle_1, file, sl_n_steps)]
  
  # prepare median file
  dt_med <- fread(fin_med)
  dt_med <- dt_med[, .(sl_med = sl_, day_cycle_1, file)]
  
  # merge the two files
  dt <- merge(dt_max, dt_med, by = c("day_cycle_1", "file"))

  rm(dt_med, dt_max)
  
  return(dt[, .(sl_max, sl_med, sl_n_steps, day_cycle_1, file)])
  
}))


full_dt[
  , species := gsub(".*/Studies/(.*?)_(.*?)/5_flag_static.*", "\\1 \\2", file)]
full_dt <- merge(full_dt, traits_dt, by = "species")
full_dt[, migration := fcase(
  migration == "partially migratory", "partial", 
  migration == "migratory", "migrant", 
  default = migration)]

max_dist <- round(max(full_dt[, sl_max]) + 100)

full_dt[, range_mode_100m := cut(
  sl_max, breaks = c(-1, seq(0, max_dist, 100)), right = T)]


full_dt[, sl_max := fifelse(sl_max < 1, 0, sl_max)/1000]
full_dt[, sl_med := fifelse(sl_med < 1, 0, sl_max)/1000]

filters <- c(30, 10)

pname <- sprintf("6_correlation_filter_%d_median_distance_raw.png", filters)

filters <- filters[!file.exists(file.path(graphs_dir, "6_distances", pname))]
nf <- length(filters)
rm(pname)

if(nf > 0){
  
  lapply(filters, function(f){
    
    p_dt <- full_dt[sl_n_steps > 1]
    
    p_dt <- p_dt[, n_file := .N, by = file][n_file > f]
    
    pr <- p_dt |> 
      ggplot() +
      geom_point(aes(
        x = sl_max, y = sl_med, color = migration), alpha = 0.3) +
      geom_abline(
        aes(slope = 1, intercept = 0), color = "gray33", linetype = "dashed"
      ) +
      scale_color_manual(values = move_col) +
      labs(
        x = "median distance to all active locations [km]", 
        y = "distance to median active location [km]"
      ) +
      theme_bw() +
      theme(legend.position = "bottom") 
    
    pl <- pr +
      scale_y_log10() +
      scale_x_log10() +
      theme(legend.position = "bottom") +
      labs(
        x = "median distance to all active locations [km]\n- logarithmic scale -", 
        y = "distance to median active location [km]\n- logarithmic scale -"
      )
    
    (pl | pr) +
      plot_annotation(
        title = "Correlation between median distance across species",
        subtitle = sprintf("for tracks that had at least %d distances", f)
      )  +
      plot_layout(guides = "collect") &
      theme(
        legend.position = "bottom", 
        title.position = element_text(hjust = 0.5),
        subtitle.position = element_text(hjust = 0.5)
      ) 
    
    pname <- sprintf("6_correlation_filter_%d_median_distance_raw.png", f)
    
    ggsave(
      file.path(graphs_dir, "6_distances", pname),
      width = 12, height = 6, dpi = 300)
    
    rm(pl, pr, pdt)
    
    return(NULL)
    
  })
  
  
}


# PLOT: hist sl_n_steps ---------------------------------------------------

pname <- "6_histogram_sl_n_steps.png"

if(!file.exists(file.path(graphs_dir, "6_distances", pname))){
  
  
  full_dt |> 
    ggplot() + 
    geom_histogram(
      aes(x = sl_n_steps), binwidth =  1, fill = "gray88", color = "gray22"
    ) +
    theme_bw() +
    labs(
      x = "number of locations during the active period",
      title = "Distribution of number of locations used to get maximum daily distances",
      subtitle = "no filter applied"
    )
  
  ggsave(
    file.path(graphs_dir, "6_distances", pname), width = 8, height = 6
  )
  
  
}

# PLOT: mode ranges -------------------------------------------------------

pname <- "6_distance_ranges_for_mode.png"

if(file.exists(file.path(graphs_dir, "6_distances", pname))){
  
  modes_dt <- full_dt[, .(n_mode = .N), by = range_mode_100m]
  modes_dt[, perc_mode := round(n_mode/sum(n_mode)*100, 1)]
  
  modes_dt[, mode_lim := as.numeric(
    sub(".*,\\s*([0-9\\.e\\+]+)\\]", "\\1", range_mode_100m))/1000]
  
  modes_dt |> 
    ggplot() + 
    geom_point(aes(x = perc_mode, y = mode_lim)) +
    theme_bw() +
    scale_y_log10() +
    labs(
      x = "percentage of the distances [%]\nc(count of distances in the range)/(total count of distances)",
      y = "upper limit of the range [km]\n- logarithmic scale -",
      title = "Distribution of distance ranges used to obtain modes across all species",
      subtitle = "no filter applied"
    ) 
  
  
  ggsave(
    file.path(graphs_dir, "6_distances", pname), width = 8, height = 6
  )
  
  
  
}



# PLOT: data loss with filtering -----------------------------------------------

pname <- "6_data_loss_with_filtering.png"

if(file.exists(file.path(graphs_dir, "6_distances", pname))){
  
  filters <- c(0, 10, 30)
  
  filter_col <- c(
    "none" = "gray88", 
    "10 steps per track" = "#FFC900",
    "30 steps per track" =  "#781D42")
  
  
  full_dt[, n_file := .N, by = file]
  full_dt[, yd := day_cycle_to_yd(day_cycle_1)]
  
  filter_dt <- rbindlist(lapply(filters, function(f){
    
    species_dt <- unique(full_dt[, .(species, order)])
    
    fdt <- full_dt[n_file > f][, .(
      n_deployments = uniqueN(file), 
      n_steps = .N, 
      n_yd = uniqueN(yd)
    ), by = species]
    
    fdt <- merge(species_dt, fdt, by = "species", all.x = TRUE)
    
    fdt[, filter := f]
    
    return(fdt)
    
  }))
  
  # Replace NA with 0 in columns a, b, c
  cols <- c("n_deployments", "n_steps", "n_yd")
  filter_dt[
    , (cols) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols = cols]
  
  
  filter_wide <- dcast(filter_dt, species + order ~ filter, value.var = cols)
  filter_wide[, n_order := uniqueN(species), by = order]
  filter_wide[, ':=' (
    perc_depl_30 = round(n_dpl_30/n_dpl_0*100, 1), 
    perc_depl_10 = round((n_dpl_10 - n_dpl_30)/n_dpl_0*100, 1), 
    perc_depl_0 = round((n_dpl_0 - n_dpl_10)/n_dpl_0*100, 1)
  )]
  
  setorder(filter_wide, n_order, order, perc_depl_30, perc_depl_10)
  
  filter_wide[, sp_id := .I]
  
  p_dt <- melt(
    filter_wide, 
    id.vars = c("species", "sp_id", "order"),
    measure.vars = c("perc_depl_10", "perc_depl_30", "perc_depl_0"), 
    variable.name = "filter", 
    value.name = "percent"
  )
  
  p_dt[, filter := gsub("perc_depl_", "", filter)]
  p_dt[, filter := factor(
    filter, 
    levels = c("0", "10", "30"),
    labels = c("none", "10 steps per track", "30 steps per track"))]
  
  order_dt <- filter_wide[, .(
    xmax = max(sp_id) + 0.5, 
    xmin = min(sp_id) - 0.5, 
    xmed = as.numeric(median(sp_id))
  ), by = .(order, n_order)]
  order_dt[, lab := ifelse(n_order > 2, order, "")]
  
  
  count_dt <- filter_wide[, .(species, sp_id, n_deployments_0)]
  
  dpl_lbl <- data.table(
    xmin = max(p_dt$sp_id)+0.5, 
    ymin = c(-20, 0, 100), 
    ymax = c(0, 100, 105), 
    ymed = c(-10, 50, 102.5),
    lab = c("order", "deployments after filtering [%]", "N")
  )
  
  ggplot() +
    geom_bar(
      data = p_dt,
      aes(x = sp_id, y = percent, fill = filter), 
      stat = "identity", width = 1, alpha = 0.8
    ) +
    geom_hline(aes(yintercept = seq(10, 100, 10)), color = "gray22") +
    geom_vline(aes(xintercept = c(1, unique(order_dt$xmax))), color = "gray22") +
    geom_text(
      data = count_dt, 
      aes(x = sp_id, y = 102.5, label = n_deployments_0), size = 2
    ) +
    geom_rect(
      data = order_dt,
      aes(ymin = -20, ymax = 0, xmin = xmin, xmax = xmax, 
          fill = order),  color = "gray22",
      alpha = 0.3
    ) +
    geom_text(
      data = order_dt, 
      aes(y = -10, x = xmed, label = lab), 
      vjust = 0.5, hjust = 0.5, 
      color = "gray22"
    ) +
    geom_rect(
      data = dpl_lbl, 
      aes(xmin = xmin, xmax = xmin+6, ymin = ymin, ymax = ymax), 
      fill = "white", color = "gray22"
    ) +
    geom_text(
      data = dpl_lbl, 
      aes(x = xmin+3, y = ymed, label = lab)
    ) +
    coord_flip() +
    scale_fill_manual(
      values = c(ord_col, filter_col), 
      breaks = names(filter_col) 
    ) +
    scale_x_continuous(
      breaks = filter_wide$sp_id, 
      labels = filter_wide$species, 
      expand = c(0, 0)) +
    scale_y_continuous(
      expand = c(0, 0), 
      limits = c(-20, 105), 
      breaks = seq(0, 100, 10)
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom", 
      axis.ticks = element_blank()
    ) +
    labs(
      x = "", 
      y = "", 
      fill = "filter applied", 
      title = "Loss of data [deployments] with filtering per species"
    )
  
  ggsave(file.path(graphs_dir, "6_distances", pname), height = 20, width = 12)
  

  }



# PLOT: calendar -----------------------------------------------------

finm <- "4_all_tracks_max_active_steps_nauticalDawn_nauticalDusk_continent.rds" 

files <- list.files(dist_dirs, pattern = finm, full.names = T)

filters <- c(10, 30)

lapply(filters, function(n_fltr){
  
  fovw <- gsub(
    ".rds", ".csv", 
    gsub("[43]_all_tracks", sprintf("6_overview_filter_%d", n_fltr), finm))
  
  speed_dt <- fread(list.files(data_dir, pattern = fovw, full.names = T))

  sl_cols <- c("sl_50", "sl_25", "sl_75")
  speed_dt <- speed_dt[, c("species", "order", "migration", ..sl_cols)]
  
  speed_dt[, (sl_cols) := lapply(.SD, function(x) x / 1000), .SDcols = sl_cols]
  
  speed_dt[, speed_cat := cut(
    sl_50, breaks = c(0, 5, 10, 20, 30, round(max(sl_50)+1)), right = T)]
  speed_dt[, speed_cat := factor(
    speed_cat, 
    levels = levels(speed_cat), 
    labels = gsub("]", " km]", levels(speed_cat)))]
  
  speed_dt[, migration := fcase(
    migration == "partially migratory", "partial", 
    migration == "migratory", "migrant", 
    default = migration)]
  
  speed_dt[
    , migration := factor(
      migration, 
      levels = c("sedentary", "partial", "migrant"), 
      labels = c("sedentary", "partial migrant", "migrant"))]
  
  
  setorder(speed_dt, speed_cat, migration, sl_50)
  speed_dt[, sp_id := .I]
  
  species_dt <- rbindlist(lapply(seq_along(files), function(n){
    
    fin <- files[n]
    sp <- gsub(".*/Studies/(.*?)_(.*?)/6_distances.*", "\\1 \\2", fin)
    
    dt <- fread(fin)
    
    dt <- dt[, n_steps := .N, by = file][n_steps >= n_fltr]
    
    if(nrow(dt) == 0){ return(NULL) }
    
    dt[, yd := day_cycle_to_yd(day_cycle_1)]
    
    dt <- dt[, .(steps_per_day = .N), by = yd]
    
    dt[
      , species := gsub(".*/Studies/(.*?)_(.*?)/6_distances.*", "\\1 \\2", fin)]
    
  }))
  
  
  species_dt[, steps_cat := cut(steps_per_day, breaks = c(0, 1, 9, 30, Inf))]
  
  species_dt <- merge(
    species_dt, speed_dt[, .(species, sp_id, migration)], by = "species")
  
  species_dt <- species_dt[species %in% speed_dt$species][
    , species := factor(species, levels = unique(speed_dt$species))]
  
  count_col <-  c("#DD5129", "#E4B731", "#0F5B78", "#688A53") 
  names(count_col) <- levels(species_dt$steps_cat)
  dist_col <- c("#F5F5F5", "#CCCCCC", "#999999", "#666666", "#222222")
  #dist_col <- c("#E6DAF8", "#C3AEEA", "#A084CA", "#7A539C", "#421C52")
  names(dist_col) <- levels(speed_dt$speed_cat)
  move_col <- c("#A3DA8D", "#F4A460", "#781D42")
  names(move_col) <- levels(speed_dt$migration)
  
  move_dt<- speed_dt[, .(
    ymax = max(sp_id) + 0.5, 
    ymin = min(sp_id) - 0.5, 
    ymed = as.numeric(median(sp_id))),
    by = .(speed_cat, migration)]
  move_dt[, txt := fifelse(
    speed_cat == "(0,5 km]", as.character(migration), "")]
  
  speed_cat_dt <- speed_dt[, .(
    ymax = max(sp_id) + 0.5, 
    ymin = min(sp_id) - 0.5, 
    ymed = as.numeric(median(sp_id))),
    by = speed_cat]
  speed_cat_dt[, txt := fifelse(
    speed_cat == "(0,5 km]", 
    paste("median distance:", speed_cat), as.character(speed_cat))]

  
  month_limits <- get_month_limits()
  month_limits[, ymin := max(speed_dt$sp_id) + 0.5]
  month_limits[, ymax := ymin + ymin*0.02]
  month_limits[, ymed := ymin + ymin*0.01]

  species_dt |> 
    ggplot() + 
    geom_tile(
      aes(x = yd, y = sp_id, fill = steps_cat), alpha = 0.8
    ) +
    geom_vline(
      data = month_limits, aes(xintercept = last_yd), color = "gray33") +
    geom_hline(
      data = speed_cat_dt, 
      aes(yintercept = ymax), color = "gray33"
    ) +
    geom_vline(aes(xintercept = 0.5), color = "gray33") +
    geom_rect(
      data = speed_cat_dt,
      aes(
        xmin = -20, xmax = -10, ymin = ymin, ymax = ymax, fill = speed_cat
      ), alpha = 0.6, color = "gray33"
    ) +
    geom_text(
      data = speed_cat_dt, 
      aes(y = ymed, x = -15, label = txt), angle = 90, hjust = 0.5, vjust = 0.5
    ) +
    geom_rect(
      data = move_dt,
      aes(
        xmin = -10, xmax = 0.5, ymin = ymin, ymax = ymax, fill = migration
      ), alpha = 0.6, color = "gray33"
    ) +
    geom_text(
      data = move_dt, 
      aes(y = ymed, x = -5, label = txt), angle = 90, hjust = 0.5, vjust = 0.5
    ) +
    geom_rect(
      data = month_limits,
      aes(xmin = first_yd, xmax = last_yd, ymin = ymin, ymax = ymax), 
      fill = "white", alpha = 0.2
    ) +
    geom_text(
      data = month_limits, 
      aes(x = mid_yd, y = ymed, label = month), 
      hjust = 0.5, vjust = 0.5, size = 3.5, color = "gray33"
    ) +
    scale_fill_manual(
      values = c(count_col, dist_col, move_col), 
      breaks = names(count_col)
    ) +
    scale_x_continuous(
      expand = c(0, 0), limits = c(-20, 366.5), breaks = seq(1, 366, 30)
    ) +
    scale_y_continuous(
      expand = c(0, 0), 
      breaks = speed_dt$sp_id, 
      labels = speed_dt$species
    ) +
    theme_bw()  +
    theme(
      axis.ticks = element_blank(), 
      legend.position = "bottom"
    ) + 
    labs(
      y = "", 
      x = "day of a year", 
      fill = "number of steps per day", 
      title = sprintf(
        "Distribution of data througout a year - filter: minimum %d tracking days per deployment", 
        n_fltr), 
      subtitle = "grouped by species median distance and migration status"
    ) 
  
  pname <- gsub(".csv", ".png", gsub("6_overview_", "6_calendar_", fovw))
  
  ggsave(
    file.path(graphs_dir, "6_distances", pname), width = 12, height = 18 )
  
})

