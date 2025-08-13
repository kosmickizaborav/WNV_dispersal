library(data.table)
library(ggplot2)
library(ggdist)
library(patchwork)
source("0_helper_functions.R")
source("color_palettes.R")

data_dir <- here::here("Data")
study_dir <- file.path(data_dir, "Studies")
graphs_dir <- file.path(data_dir, "Graphs")

dist_dirs <- file.path(list.files(study_dir, full.names = T), "6_distances")
target_sp <- gsub(".*/Studies/(.*?)_(.*?)/6_distances.*", "\\1 \\2", dist_dirs)

traits_dt <- fread(here::here("Published_data", "00_bird_traits.csv"))
traits_dt[, activity := ifelse(nocturnal == 1, "nocturnal", "diurnal")]
traits_dt <- unique(traits_dt[
  , .(species = birdlife_name, migration = migration_txt, activity, order, family)])
traits_dt <- traits_dt[species %in% target_sp]


deployments <- fread(file.path(data_dir, "1_deployments_to_download.csv"))

deployments[, track_id := make_file_name(
  study_id, individual_id, deployment_id, file_type =  "")]

# INPUT
fdt <- data.table(
  finm = c(
    "4_all_tracks_max_active_steps_nauticalDawn_nauticalDusk_continent.rds", 
    "4_all_tracks_max_active_steps_nauticalDawn_nauticalDusk_continent.rds", 
    "3_all_tracks_median_active_steps_nauticalDawn_nauticalDusk_continent.rds" 
  ), 
  dist_var = c("sl_", "sl_median", "sl_")
)

fdt <- fdt[, .(n_filter = c(10, 30)), by = .(finm, dist_var)]



# 1 - Make overview table ------------------------------------------------------

lapply(1:nrow(fdt), function(i){
  
  finm <- fdt$finm[i]
  dist_col <- fdt$dist_var[i]
  n_fltr <- fdt$n_filter[i]
  
  files <- list.files(dist_dirs, pattern = finm, full.names = TRUE)
  nf <- length(files)
  
  foverview <- gsub(
    ".rds", ".csv", 
    gsub("[43]_all_tracks",sprintf("6_overview_filter_%d", n_fltr), finm))
  
  if(dist_col == "sl_median"){
    foverview <- gsub(".csv", "_sl_median.csv", foverview)
  } 
  
  if(!file.exists(file.path(data_dir, foverview))){
    
    species_dt <- rbindlist(lapply(seq_along(files), function(n){
      
      fin <- files[n]
      
      dt <- fread(fin)
      
      # filter tracks that have less than 10 steps
      dt <- dt[, n_steps := .N, by = file][n_steps >= n_fltr]
      
      if(nrow(dt) == 0){ return(NULL) }
      
      dt[, yd := day_cycle_to_yd(day_cycle_1)]
      # extract sensor
      dt[, sensor := sub(".*dep_(.*?)_sen.*", "\\1", file)][
        , sensor := fcase(sensor == 653, "gps", sensor == 2299894820, "sigfox")]
      
      n_deploy_tot <- dt[, uniqueN(file)]
      
      # get track lengths and sensor
      dt_per_file <- dt[, .(
        n_days = uniqueN(day_cycle_1),
        n_year_days = uniqueN(yd), 
        sensor = unique(sensor)
      ), by = file]
      
      # remove duplicated trakcs - deployments with both gps and sigfox
      dt_per_file[, track_id := sub("_dep_\\d+.*$", "", basename(file))]
      
      setorder(dt_per_file, sensor)
      
      dt_per_file[, duplicated_track := duplicated(track_id)]
      dpltrks <- dt_per_file[duplicated_track == T, file]
      
      dt_per_file <- dt_per_file[duplicated_track == F]
      dt <- dt[!file %in% dpltrks]
      
      cls <- c("n_days", "n_year_days")
      
      # extract stats for tracking times
      dt_per_file <- dt_per_file[, c(
        as.list(setNames(lapply(.SD, median), paste0("median_", cls))),
        as.list(setNames(lapply(.SD, function(x) {
          if(n_deploy_tot == 1) NA else paste(range(x), collapse = " - ") }),
          paste0("range_", cls))),
        N_gps = sum(sensor == "gps"),
        N_sigfox = sum(sensor == "sigfox")
      ), .SDcols = cls]
      
      dt[, yd := day_cycle_to_yd(day_cycle_1)]
      # extract sensor
      dt[, sensor := sub(".*dep_(.*?)_sen.*", "\\1", file)]

      
      if(dist_col == "sl_median"){ 
        setnames(dt, old = "sl_median", new = "sl_median_old")
        # when there was only one distance available no stats was calculated
        dt[, sl_median := fifelse(
          sl_n_steps == 1, as.numeric(sl_), as.numeric(sl_median_old))] 
      }
      
      max_dist <- round(max(dt[, get(dist_col)])+100)
      if(max_dist < 100) max_dist <- 100
      
      dt_out <- dt[, c(
        species = gsub(".*/Studies/(.*?)_(.*?)/6_distances.*", "\\1 \\2", fin),
        N_steps = .N, 
        N_days = uniqueN(day_cycle_1),
        N_year_days = uniqueN(yd),
        N_deployments = uniqueN(file), 
        sl_mean = mean(get(dist_col)),
        as.list(setNames(
          round(quantile(
            get(dist_col), probs = c(0.05, 0.25, 0.5, 0.75, 0.95)), 2), 
          paste0("sl_", c("05", "25", "50", "75", "95")))), 
        range_mode_100m = as.character(get_mode(cut(
          get(dist_col), breaks = c(-1, seq(0, max_dist, 100)), right = T)
        )), 
        continents = paste(unique(continent), collapse = ", "), 
        sensors = paste(unique(sensor), collapse = ", ")
      )]
      
      cbind(dt_out, dt_per_file)
      
    }), fill = T)
    
    # add traits and phylogeny
    species_dt <- merge(species_dt, traits_dt, by = "species", all.x = T)
    
    cls <- c("species", "family", "order", "migration", "activity", "N_deployments")
    
    setcolorder(species_dt, neworder = cls)
    
    species_dt <- species_dt[!(sl_50 == 0 & sl_75 == 0 & sl_25 == 0)]
    
    fwrite(species_dt, file.path(data_dir, foverview))
    
  }
  
  cat(finm, "DONE!\n")
  
  invisible(return(NULL))
  
})

gc(verbose = F)




# 2 - Plot ----------------------------------------------------------------

move_col <- c("#A3DA8D", "#F4A460", "#781D42")
names(move_col) <- c("sedentary", "partial", "migrant")

legend_dt <- data.table(
  y = 5,
  xmin = 1,
  xmax = 10,
  x_ppt = c(NA, 2, 6, NA,  13),
  y_txt = c(4, 7, 2, 4, 2),
  y_arrow = c(NA, 6, 3, NA, 3),
  x = c(1, 2, 6, 10, 13),
  ppt = c(NA, "mod", "med", NA, NA),
  ppt_txt = c(
    "Q1 [25%]", 
    "mode range \n(upper limit)", 
    "median \nQ2 [50%]", 
    "Q3 [75%]", 
    "number of\ndeployments"
  )
)

# Legend plot: points and text
legend <- legend_dt |> 
  ggplot() +
  geom_segment(
    aes(x = x_ppt, xend = x, y = 5, yend = y_arrow), color = "gray33", 
    arrow = arrow(length = unit(0.1, "cm"), type = "closed"), 
    linetype = "dashed"
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
  geom_text(
    aes(x = 13, y = 5, label = "N"), size = 5, color = "gray11"
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

files <- list.files(data_dir, pattern = "6_overview.*\\.csv", full.names = T)


lapply(seq_along(files), function(n){
  
  fin <- files[n]
  
  species_dt <- fread(fin)
  
  species_dt <- species_dt[
    , .(species, N_steps, N_deployments, order, 
        sl_50, sl_25, sl_75, range_mode_100m, migration)]
  
  # make factors
  species_dt[, migration := fcase(
    migration == "partially migratory", "partial", 
    migration == "migratory", "migrant", 
    default = migration)]
  species_dt[, migration := factor(
    migration, levels = c("sedentary", "partial", "migrant"))]
  
  species_dt[, mode_lim := as.numeric(
      sub(".*,\\s*([0-9\\.e\\+]+)\\]", "\\1", range_mode_100m))]
  
  
  sl_cols <- c("sl_50", "sl_25", "sl_75", "mode_lim")
  species_dt[
    , (sl_cols) := lapply(.SD, function(x) x / 1000), .SDcols = sl_cols]
  
  # set order of 
  species_dt[, sl_order := max(sl_50), by = order]
  
  setorder(species_dt, sl_order)
  
  grupings <- c("migration", "order")
  
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
    move_dt[, migration_txt := fifelse(
      migration == "partial", "partial migrant", as.character(migration))]
    
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
      geom_point(
        aes(y = sp_id, x = mode_lim, color = migration), shape = 23, 
        alpha = 0.6, size = 2, fill = "white", stroke = 1.2
      ) +
      geom_text(
        aes(y = sp_id, x = sl_75-pscl[1], label = N_deployments), 
        vjust = 0.5, hjust = 0.5, color = "gray33", size = 3
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
      scale_color_manual(values = move_col) +
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
        aes(y = ymed, x = pscl[1]+(pscl[2]-pscl[1])/2, label = migration_txt), 
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



# Box plot distances-------------------------------------------------


filters <- c("filter_30", "filter_10")

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
    species_dt[, migration := factor(
      migration, levels = c("sedentary", "partial", "migrant"))]
    
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




kruskal.test(sl_50 ~ migration, data = species_dt)

# Pairwise comparisons
# "BH" uses the Benjamini-Hochberg correction for multiple testing
pairwise.wilcox.test(
  species_dt$sl_50, species_dt$migration, p.adjust.method = "BH")



# Migration callendar -----------------------------------------------------
# 
# 
# 
# species_dt <- rbindlist(lapply(seq_along(files), function(n){
#   
#   fin <- files[n]
#   sp <- gsub(".*/Studies/(.*?)_(.*?)/6_distances.*", "\\1 \\2", fin)
#   
#   dt <- fread(fin)
#   
#   dt <- dt[, n_steps := .N, by = file][n_steps > 10]
#   
#   if(nrow(dt) == 0){ return(NULL) }
#   
#   dt[, yd := day_cycle_to_yd(day_cycle_1)]
#   
#   dt <- dt[, .(steps_per_day = .N), by = yd]
#   
#   dt <- rbindlist(list()
#     CJ(steps_per_day = 0, yd = 1:366), 
#     dt)
#   dt[, .(steps_per_day := sum(steps_per_day)), by = yd]
#   
#   cat("DONE:", n, "|", nf, "\n")
#   
#   dt[, species := gsub(".*/Studies/(.*?)_(.*?)/6_distances.*", "\\1 \\2", fin)]
#   
#   
#   
#   }))
# 
# 
# 
# 
# dt[, `:=`(
#   id = frank(paste0(y, "-", file), ties.method = "dense"),
#   year = as.factor(y)  # Convert year to factor
# )]
# 
# id_min <- 0
# id_max <- max(dt$id) + 1
# 
# 
# 
# sp_limits <- dt[, .(
#   n_tracks = uniqueN(file),  
#   id_min = min(id)
# ), by = species][order(id_min)][
#   , `:=`(
#     id_max = c(id_min[-1], id_max)
#   )][
#     , id_min := ifelse(id_min == 1, 0, id_min)][
#       , id_mid := id_min + (id_max - id_min) / 2]
# 
# 
#  ggplot() +
#   # geom_hline(
#   #   aes(yintercept = c(0, year_limits$id_max)), color = "gray33"
#   # ) +
#   # geom_vline(
#   #   aes(xintercept = c(1, month_limits$last_yd, 396)), color = "gray33"
#   # ) +
#   geom_segment(
#     data = dt,
#     aes(
#       x = brs_start, xend = brs_end, 
#       y = id, yend = id,
#       color = species
#     ),
#     linewidth = 2, 
#     alpha = 0.6
#   ) +
#   # geom_rect(
#   #   data = year_limits,
#   #   aes(
#   #     xmin = 366, xmax = 396,
#   #     ymin = id_min, ymax = id_max,
#   #     fill = year
#   #   ),
#   #   color = "gray33",
#   #   alpha = 0.6
#   # ) +
#   # geom_text(
#   #   data = year_limits,
#   #   aes(x = 381, y = id_mid, label = y_lab),
#   #   color = "black",
#   #   hjust = 0.5,
#   #   vjust = 0.5
#   # ) +
#   scale_x_continuous(expand = c(0,0)) +
#   scale_y_continuous(expand = c(0,0)) +
#   #scale_color_manual(values = pal) +
#   #scale_fill_manual(values = pal) +
#   theme_bw() +
#   labs(
#     y = "track bursts \n per deployment", 
#     title = "Distribution of tracking data in time"
#   ) + 
#   theme(legend.position = "none")
# 
# 
# 
# 
# 
# plot_steps_timeline <- function(steps = NULL) {
#   
#   df_timeline <- copy(steps)
#   
#   if("day_cycle_1" %in% names(df_timeline)){
#     setnames(df_timeline, old = "day_cycle_1", new = "day_cycle", skip_abs = T)
#   }
#   
# 
#   
#   count_df <- df_timeline[, .(step_count = .N), by = yd]
#   count_df <- rbindlist(list(
#     count_df,
#     data.table(yd = 1:366, step_count = 0)  # Add days with 0 steps
#   ))[, .(step_count = sum(step_count)), by = yd]  # Aggregate by yd
#   
#   rm(df_timeline)
#   
#   id_min <- 0
#   id_max <- max(df_bursts$id) + 1
#   
#   month_limits <- get_month_limits()
#   
#   year_limits <- df_bursts[, .(
#     n_tracks = uniqueN(file),  
#     id_min = min(id)
#   ), by = year][order(id_min)][
#     , `:=`(
#       id_max = c(id_min[-1], id_max),  
#       y_lab = ifelse(n_tracks > 0.1 * id_max, as.character(year), "")  # Filter sparse years
#     )][
#     , id_min := ifelse(id_min == 1, 0, id_min)][
#     , id_mid := id_min + (id_max - id_min) / 2]
#   
#   pal <- get_palette(years = year_limits$year)
#   
#   ptl <- ggplot() +
#     geom_hline(
#       aes(yintercept = c(0, year_limits$id_max)), color = "gray33"
#     ) +
#     geom_vline(
#       aes(xintercept = c(1, month_limits$last_yd, 396)), color = "gray33"
#     ) +
#     geom_segment(
#       data = df_bursts,
#       aes(
#         x = brs_start, xend = brs_end, 
#         y = id, yend = id,
#         color = year
#       ),
#       linewidth = 2, 
#       alpha = 0.6
#     ) +
#     geom_rect(
#       data = year_limits,
#       aes(
#         xmin = 366, xmax = 396,
#         ymin = id_min, ymax = id_max,
#         fill = year
#       ),
#       color = "gray33",
#       alpha = 0.6
#     ) +
#     geom_text(
#       data = year_limits,
#       aes(x = 381, y = id_mid, label = y_lab),
#       color = "black",
#       hjust = 0.5,
#       vjust = 0.5
#     ) +
#     scale_x_continuous(expand = c(0,0)) +
#     scale_y_continuous(expand = c(0,0)) +
#     scale_color_manual(values = pal) +
#     scale_fill_manual(values = pal) +
#     theme_void() +
#     labs(
#       y = "track bursts \n per deployment", 
#       title = "Distribution of tracking data in time"
#     ) +
#     theme(
#       axis.title.y = element_text(angle = 90, vjust = 0.5),
#       plot.title = element_text(hjust = 0),
#       plot.margin = margin(t = 1, r = 0, b = 0, l = 0, unit = "mm"), 
#       panel.spacing = unit(0, "cm"), 
#       legend.position = "none"
#     ) 
#   
#   mb <- ggplot() + 
#     geom_segment(aes(x = 0, xend = 367, y = 1, yend = 1), color = "gray33") +
#     geom_segment(aes(x = 0, xend = 367, y = 10, yend = 10), color = "gray33") +
#     geom_vline(
#       aes(xintercept = c(0, month_limits$last_yd)), color = "gray33"
#     ) +
#     scale_x_continuous(expand = c(0,0), limits = c(0, 396)) +
#     scale_y_continuous(expand = c(0, 0)) +
#     geom_text(
#       data = month_limits, aes(x = mid_yd, y = 5, label = month),
#       hjust = 0.5, vjust = 0.5
#     ) +
#     theme_void()
#     
#   
#   psc <- ggplot() + 
#     geom_vline(
#       aes(xintercept = month_limits$last_yd), color = "gray33"
#     ) +
#     geom_line(data = count_df, aes(x = yd, y = step_count)) +
#     geom_segment(aes(x = 0, xend = 367, y = 0, yend = 0), color = "gray33") +
#     scale_x_continuous(
#       expand = c(0,0), limits = c(0, 396), breaks = seq(1, 366, 30)
#     ) +
#     scale_y_continuous(
#       expand = c(0, NA), limits = c(0, max(count_df$step_count)+1)
#     ) +
#     theme_bw() +
#     labs(x = "day of the year", y = "count \n per day") +
#     theme(
#       plot.margin = margin(t = 0, r = 0, b = 1, l = 1, unit = "mm"), 
#       axis.ticks = element_blank(),
#       panel.grid.minor = element_blank(),
#       panel.grid.major = element_blank(),
#       panel.spacing = unit(0, "cm"), 
#       panel.border = element_blank(),
#       axis.line.y = element_line(color = "gray33")
#     )
#   
#   # Dynamically adjust the relative heights of the plots
#   relative_heights <- c(2, 0.3, 1)  # Adjust as needed
#   
#   pout <- ptl /mb/ psc + 
#     plot_layout(heights = relative_heights)  # Use relative_heights here
#   
#   return(pout)
#   
# }
# 
# 
# 
# 
