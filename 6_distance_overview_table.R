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



# 1 - Prepare files -------------------------------------------------------


finname <- "4_.*_max_active_steps_nauticalDawn_nauticalDusk_continent.rds"

n_fltr <- 20

files <- list.files(dist_dirs, pattern = finname, full.names = TRUE)

files_out <- gsub(".rds", "_fltr_20_dist_50m.rds", files)

files <- files[
  !(file.exists(files_out)|file.exists(gsub(".rds", "nodata.rds", files_out)))]
nf <- length(files)

if(nf > 0){
  
  lapply(seq_along(files), function(n){
    
    fin <- files[n]
    
    dt <- fread(fin)
    
    dt_summary <- dt[, .(
      n_days = uniqueN(day_cycle_1),
      n_deployments = uniqueN(file), 
      mean_sl = mean(sl_)
    ), by = file]
    
    # only keep the days in which maximum daily distance was at least 50m
    # double the average GPS error, otherwise we assume no movement
    dt <- dt[sl_ >= 50 & (sl_median >= 50 | is.na(sl_median))]
    
    # filter tracks that have less than 10 steps
    dt <- dt[, n_steps := .N, by = file][n_steps >= n_fltr]
    
    if(nrow(dt) == 0){ 
      
      fwrite(dt_summary, gsub(".rds", "_fltr_20_dist_50m_nodata.rds", fin))
      
      return(NULL)
      
    }
    
    dt[, sensor := sub(".*dep_(.*?)_sen.*", "\\1", file)][
      , sensor := fcase(sensor == 653, "gps", sensor == 2299894820, "sigfox")]
    
    # get track lengths and sensor
    dt_per_file <- dt[, .(sensor = unique(sensor)), by = file]
    
    # remove duplicated trakcs - deployments with both gps and sigfox
    dt_per_file[, track_id := sub("_dep_\\d+.*$", "", basename(file))]
    
    setorder(dt_per_file, sensor)
    
    dt_per_file[, duplicated_track := duplicated(track_id)]
    dpltrks <- dt_per_file[duplicated_track == T, file]
    
    dt <- dt[!file %in% dpltrks]
    
    # save tracks
    setnames(dt, old = "sl_median", new = "sl_median_old")
    
    # when there was only one distance available no stats was calculated
    dt[, sl_median := fifelse(
      sl_n_steps == 1, as.numeric(sl_), as.numeric(sl_median_old))] 
    
    dt[, sl_median_old := NULL]
    
    fwrite(dt, gsub(".rds", "_fltr_20_dist_50m.rds", fin))
    
    cat(n, "|", nf , "DONE! \n")
    
  })
  
}

gc(verbose = F)
rm(files, files_out)



# 1 - Make overview table ------------------------------------------------------

fout <- "6_overview_filter_20_max_active_steps_nauticalDawn_nauticalDusk_continent.csv"

if(!file.exists(file.path(data_dir, fout))){
  
  files <- list.files(
    dist_dirs, pattern ="_fltr_20_dist_50m.rds", full.names = T)
  
  species_dt <- rbindlist(lapply(seq_along(files), function(n){
    
    fin <- files[n]
    
    dt <- fread(fin)
    
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
        paste0("max_sl_", c("q05", "q25", "q50", "q75", "q95")))), 
      median_sl_mean = mean(sl_),
      as.list(setNames(
        round(quantile(
          sl_median, probs = c(0.05, 0.25, 0.5, 0.75, 0.95)), 2), 
        paste0("median_sl_", c("q05", "q25", "q50", "q75", "q95")))), 
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

gc(verbose = F)
rm(fout, finname)



# Compare with Fandos -----------------------------------------------------

# load data overview
fin <- "6_overview_filter_20_max_active_steps_nauticalDawn_nauticalDusk_continent.csv"
dt <- fread(file.path(data_dir, fin))
dt <- dt[, median_sl_q50 := median_sl_q50/1000]

fandos_dt <- fread(here::here(
  "Published_data", "Fandos2023_Table_S14_ species_dispersal_distances_v1_0_2.csv"))

# convert to birdlife names
fandos_dt <- rename_to_birdlife(fandos_dt, "species")
fandos_dt <- fandos_dt[
  birdlife_name %in% dt$species, .(median, birdlife_name, function_id)]
fandos_avr <- fandos_dt[
  , .(sl_50_avr = mean(median, na.rm = TRUE)), by = birdlife_name]


merged_dt <- merge(fandos_avr, dt, by.x = "birdlife_name", by.y = "species")

merged_dt[, order_txt := paste0(order, " [", .N, "]"), by = order]


ord_col_c <- ord_col
matched_idx <- match(names(ord_col_c), merged_dt$order)
names(ord_col_c) <- merged_dt$order_txt[matched_idx]


merged_dt |>
  ggplot() +
  geom_point(
    aes(x = sl_50_avr, y = median_sl_q50, color = order_txt), size = 3, alpha = 0.5
  ) +
  ggrepel::geom_text_repel(
    aes(x = sl_50_avr, y = median_sl_q50, label = birdlife_name), size = 3
  ) +
  scale_color_manual(values = ord_col_c) +
  theme_bw() +
  scale_y_log10() +
  scale_x_log10() +
  labs(
    x = "Fandos et al. (2023) median dispersal distance [km]",
    y = "Median daily movement [km]",
    color = "order [number of species]",
    title = "Comparison of median daily movement estimates with Fandos et al. (2023) dispersal distances",
    subtitle = "dispersal distances were represented by mean over median estimates for all models"
  )


ggsave(
  filename = file.path(graphs_dir, "6_median_sl_compare_Fandos2023.png"),
  width = 10, height = 6
)

rm(merged_dt, fandos_dt, fandos_avr, dt)



# P: SL - median [Q1, Q3]-------------------------------------------------------

fin <- "6_overview_filter_20_max_active_steps_nauticalDawn_nauticalDusk_continent.csv"
full_dt <- fread(file.path(data_dir, fin))

sl_vars <- c("median_sl", "max_sl")
grupings <- c("migration", "order")

files_out <- file.path(graphs_dir, "6_distances", sprintf(
  gsub(".csv", "_%s.png", basename(fin)), 
  paste(rep(sl_vars, each = length(grupings)), grupings, sep = "_"))
)

# check which files are missing
files <- files_out[!file.exists(files_out)]

nf <- length(files)

mlbl <- c("sedentary", "partial migrant", "migrant")
move_col <- c("#A3DA8D", "#F4A460", "#781D42")
names(move_col) <- mlbl

# make factors - it included birdlife migration 
full_dt[, migration := fcase(
  migration == "partially migratory", "partial migrant",
  migration == "migratory", "migrant",
  default = migration)]
full_dt[
  , migration := factor(migration, levels = mlbl)]


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
  
  lapply(seq_along(files), function(i){
    
    fout <- files[i]
    
    # check variables
    var <- if(grepl("median_sl", fout)) "median_sl" else "max_sl"
    o <- if(grepl("migration", fout)) "migration" else "order"
    
    # get the relevant columns
    col_n <- c(
      "species", "n_steps", "n_deployments", "order", "migration", 
      paste0(var, c("_q50", "_q25", "_q75")))
    species_dt <- full_dt[, ..col_n]
    
    # rename the sl column for plotting
    sl_cols <- c("sl_q50", "sl_q25", "sl_q75")
    setnames(
      species_dt, 
      old = paste0(var, c("_q50", "_q25", "_q75")), 
      new = sl_cols
      )
  
    # distance to km
    species_dt[
      , (sl_cols) := lapply(.SD, function(x) x / 1000), .SDcols = sl_cols]
    
    # set order of species 
    species_dt[, sl_order := max(sl_q50), by = order]
    
    if(o == "migration"){
      ordv <- c("migration", "sl_order") 
      by1 <- grupings
      by2 <- o
    } else {
      ordv <- c("sl_order", "migration")
      by1 <- o
      by2 <- grupings
    } 
    
    ordv <- c(ordv, "sl_q50")
    
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
    
    max75 <- max(species_dt$sl_q75)
    
    if(max75 < 100) {
      
      pscl <- c(-2, -8, -30)
      
    } else{ 
      
      pscl <- c(-max75*0.02, -max75*0.08, -max75*0.33)
      
    }
    
    dist_type <- ifelse(grepl("median", var), "Median", "Maximum")
    
    p <- species_dt |> 
      ggplot() +
      geom_point(
        aes(y = sp_id, x = sl_q50, color = migration), 
        size = 3, shape = 15, alpha = 0.6
      ) +
      geom_errorbar(
        aes(y = sp_id, xmin = sl_q25, xmax = sl_q75, color = migration), 
        linewidth = 1.2, alpha = 0.6
      ) +
      geom_text(
        aes(y = sp_id, x = sl_q75-pscl[1], label = n_deployments), 
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
        x = "daily distance [km]", 
        title = paste(dist_type, "daily distances across species"), 
        subtitle = "median values with the first [Q1] and third [Q3] quartiles", 
        caption = "only distances greater than 50m and tracks with at least 20 days included"
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
    
    ggsave(fout, width = 12, height = 16, dpi = 300)
    
  })
  
}

rm(full_dt, files, files_out, nf, grupings, legend_dt)



# # PLOT: data loss with filtering -----------------------------------------------
# 
# pname <- "6_data_loss_with_filtering.png"
# 
# finname <- "4_.*_max_active_steps_nauticalDawn_nauticalDusk_continent.rds"
# files <- list.files(dist_dirs, pattern = finname, full.names = T)
# nf <- length(files)
# 
# 
# full_dt <- rbindlist(lapply(files, function(fin){
#   dt <- fread(fin)
#   sp <- gsub(".*/Studies/(.*?)_(.*?)/6_distances.*", "\\1 \\2", fin)
#   
#   dt <- dt[sl_ >= 50 & (sl_median >= 50 | is.na(sl_median))]
#   if(nrow(dt) < 20 ){ return(NULL) }
#   
#   dt <- dt[, .(n_steps = .N), by = file]
#   dt[, species := sp]
#   dt[, order := traits_dt[species == sp, order]]
#   
#   }))
# 
# 
# 
# if(file.exists(file.path(graphs_dir, "6_distances", pname))){
#   
#   # filters and colors
#   filters <- c(0, 10, 20, 30)
#   
#   filter_col <- c(
#     "none" = "gray88",
#     "10 days/track" = "#f2f3ae",
#     "20 days/track" = "#68a691",
#     "30 days/track" = "#781D42"
#   )
#   
#   # Build a long table giving counts at each filter threshold
#   # short column names to keep downstream code compact
#   
#   filter_dt <- rbindlist(lapply(filters, function(f) {
#     # species + order reference to keep species with zero after filtering
#     species_ref <- unique(full_dt[, .(species, order)])
#     # select tracks with more than f files and compute summary per species
#     tmp <- full_dt[n_steps >= f, .(n_dep = uniqueN(file)), by = species]
#     
#     tmp <- merge(species_ref, tmp, by = "species", all.x = TRUE)
#     tmp[, flt := f]
#     
#     return(tmp)
#   
#     }))
#   
#   # Replace NA with 0 for our summary columns
#   filter_dt[, n_dep := fifelse(is.na(n_dep), 0, n_dep)]
#   
#   # for changing the names of columns
#   filter_dt[, flt := paste0("n_dep_", flt)]
#   filter_wide <- dcast(
#     filter_dt, species + order ~ flt, value.var = "n_dep")
#   
#   # number of species per order for plotting grouping
#   filter_wide[, n_order := uniqueN(species), by = order]
#   
#   # dynamic percent calculations (loss between successive filters) ---
#   # dase column - no filter
#   base_col <- "n_dep_0"
#   
#   # create perc_steps_<filter> columns:
#   # for intermediate filters: percent lost between current and next (relative to base)
#   # for last filter: percent remaining after last threshold (relative to base)
#   for (i in seq_along(filters)) {
#     cur <- filters[i]
#     cur_col <- paste0("n_dep_", cur)
#     perc_col <- paste0("perc_dep_", cur)
#     if (i < length(filters)) {
#       next_col <- paste0("n_dep_", filters[i + 1])
#       filter_wide[
#         , (perc_col) := round((get(cur_col) - get(next_col))/get(base_col) * 100, 1)]
#     } else {
#       # last: remaining proportion relative to original
#       filter_wide[
#         , (perc_col) := round(get(cur_col) / get(base_col) * 100, 1)]
#     }
#   }
#   
#   # Replace any NA percents (division by zero etc.) with 0
#   perc_cols <- paste0("perc_dep_", filters)
#   filter_wide[
#     , (perc_cols) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), 
#     .SDcols = perc_cols]
#   
#   # Order rows for plotting: by number of species in order, order name, and perc columns
#   # Build ordering columns dynamically (largest filter perc first likely more informative)
#   order_cols <- c("n_order", "order", rev(perc_cols))
#   setorderv(filter_wide, order_cols)
#   
#   # assign plotting ids
#   filter_wide[, sp_id := .I]
#   
#   # Melt percent columns to long format for stacked bar plot
#   p_dt <- melt(
#     filter_wide,
#     id.vars = c("species", "sp_id", "order"),
#     measure.vars = perc_cols,
#     variable.name = "filter",
#     value.name = "percent"
#   )
#   
#   # simplify filter column: "perc_depl_10" -> "10"
#   p_dt[, filter := gsub("^perc_dep_", "", filter)]
#   
#   # convert to factor with levels matching filters and labels coming from filter_col
#   p_dt[, filter := factor(
#     filter,
#     levels = as.character(filters),
#     labels = names(filter_col)
#   )]
#   
#   # rectangles showing taxonomic order at bottom
#   order_dt <- filter_wide[, .(
#     xmax = max(sp_id) + 0.5,
#     xmin = min(sp_id) - 0.5,
#     xmed = as.numeric(median(sp_id))
#   ), by = .(order, n_order)]
#   order_dt[, lab := ifelse(n_order > 2, order, "")]
#   
#   # small table with original deployment counts to print above bars
#   count_dt <- filter_wide[, .(n_dep_0, species, sp_id)]
#   
#   # little legend box labels (keeps previous layout)
#   dpl_lbl <- data.table(
#     xmin = max(p_dt$sp_id) + 0.5,
#     ymin = c(-20, 0, 100),
#     ymax = c(0, 100, 105),
#     ymed = c(-10, 50, 102.5),
#     lab = c("order", "deployments after filtering [%]", "N")
#   )
#   
#   # Plot
#   ggplot() +
#     geom_bar(
#       data = p_dt,
#       aes(x = sp_id, y = percent, fill = filter),
#       stat = "identity", width = 1, alpha = 0.8
#     ) +
#     geom_hline(yintercept = seq(10, 100, 10), color = "gray22") +
#     geom_vline(xintercept = c(1, unique(order_dt$xmax)), color = "gray22") +
#     geom_text(
#       data = count_dt,
#       aes(x = sp_id, y = 102.5, label = n_dep_0),
#       size = 2
#     ) +
#     geom_rect(
#       data = order_dt,
#       aes(ymin = -20, ymax = 0, xmin = xmin, xmax = xmax, fill = order),
#       color = "gray22",
#       alpha = 0.3
#     ) +
#     geom_text(
#       data = order_dt,
#       aes(y = -10, x = xmed, label = lab),
#       vjust = 0.5, hjust = 0.5,
#       color = "gray22"
#     ) +
#     geom_rect(
#       data = dpl_lbl,
#       aes(xmin = xmin, xmax = xmin + 6, ymin = ymin, ymax = ymax),
#       fill = "white", color = "gray22"
#     ) +
#     geom_text(
#       data = dpl_lbl,
#       aes(x = xmin + 3, y = ymed, label = lab)
#     ) +
#     coord_flip() +
#     scale_fill_manual(
#       # ord_col should be defined elsewhere in your script (order colors),
#       # we combine order colors and filter colors so both appear in legend mapping
#       values = c(ord_col, filter_col),
#       breaks = names(filter_col)
#     ) +
#     scale_x_continuous(
#       breaks = filter_wide$sp_id,
#       labels = filter_wide$species,
#       expand = c(0, 0)
#     ) +
#     scale_y_continuous(
#       expand = c(0, 0),
#       limits = c(-20, 105),
#       breaks = seq(0, 100, 10)
#     ) +
#     theme_bw() +
#     theme(
#       legend.position = "bottom",
#       axis.ticks = element_blank()
#     ) +
#     labs(
#       x = "",
#       y = "",
#       fill = "filter applied",
#       title = "Loss of data (deployments) with filtering per species"
#     )
#   
#   # Save plot (uses your variables graphs_dir and pname from surrounding code)
#   ggsave(file.path(graphs_dir, "6_distances", pname), height = 20, width = 12)
#   
# 
#   }
# 
# 
# 
# # PLOT: calendar -----------------------------------------------------
# 
# finm <- "4_all_tracks_max_active_steps_nauticalDawn_nauticalDusk_continent.rds" 
# 
# files <- list.files(dist_dirs, pattern = finm, full.names = T)
# 
# filters <- c(10, 30)
# 
# lapply(filters, function(n_fltr){
#   
#   fovw <- gsub(
#     ".rds", ".csv", 
#     gsub("[43]_all_tracks", sprintf("6_overview_filter_%d", n_fltr), finm))
#   
#   speed_dt <- fread(list.files(data_dir, pattern = fovw, full.names = T))
# 
#   sl_cols <- c("sl_50", "sl_25", "sl_75")
#   speed_dt <- speed_dt[, c("species", "order", "migration", ..sl_cols)]
#   
#   speed_dt[, (sl_cols) := lapply(.SD, function(x) x / 1000), .SDcols = sl_cols]
#   
#   speed_dt[, speed_cat := cut(
#     sl_50, breaks = c(0, 5, 10, 20, 30, round(max(sl_50)+1)), right = T)]
#   speed_dt[, speed_cat := factor(
#     speed_cat, 
#     levels = levels(speed_cat), 
#     labels = gsub("]", " km]", levels(speed_cat)))]
#   
#   speed_dt[, migration := fcase(
#     migration == "partially migratory", "partial", 
#     migration == "migratory", "migrant", 
#     default = migration)]
#   
#   speed_dt[
#     , migration := factor(
#       migration, 
#       levels = c("sedentary", "partial", "migrant"), 
#       labels = c("sedentary", "partial migrant", "migrant"))]
#   
#   
#   setorder(speed_dt, speed_cat, migration, sl_50)
#   speed_dt[, sp_id := .I]
#   
#   species_dt <- rbindlist(lapply(seq_along(files), function(n){
#     
#     fin <- files[n]
#     sp <- gsub(".*/Studies/(.*?)_(.*?)/6_distances.*", "\\1 \\2", fin)
#     
#     dt <- fread(fin)
#     
#     dt <- dt[, n_steps := .N, by = file][n_steps >= n_fltr]
#     
#     if(nrow(dt) == 0){ return(NULL) }
#     
#     dt[, yd := day_cycle_to_yd(day_cycle_1)]
#     
#     dt <- dt[, .(steps_per_day = .N), by = yd]
#     
#     dt[
#       , species := gsub(".*/Studies/(.*?)_(.*?)/6_distances.*", "\\1 \\2", fin)]
#     
#   }))
#   
#   
#   species_dt[, steps_cat := cut(steps_per_day, breaks = c(0, 1, 9, 30, Inf))]
#   
#   species_dt <- merge(
#     species_dt, speed_dt[, .(species, sp_id, migration)], by = "species")
#   
#   species_dt <- species_dt[species %in% speed_dt$species][
#     , species := factor(species, levels = unique(speed_dt$species))]
#   
#   count_col <-  c("#DD5129", "#E4B731", "#0F5B78", "#688A53") 
#   names(count_col) <- levels(species_dt$steps_cat)
#   dist_col <- c("#F5F5F5", "#CCCCCC", "#999999", "#666666", "#222222")
#   #dist_col <- c("#E6DAF8", "#C3AEEA", "#A084CA", "#7A539C", "#421C52")
#   names(dist_col) <- levels(speed_dt$speed_cat)
#   move_col <- c("#A3DA8D", "#F4A460", "#781D42")
#   names(move_col) <- levels(speed_dt$migration)
#   
#   move_dt<- speed_dt[, .(
#     ymax = max(sp_id) + 0.5, 
#     ymin = min(sp_id) - 0.5, 
#     ymed = as.numeric(median(sp_id))),
#     by = .(speed_cat, migration)]
#   move_dt[, txt := fifelse(
#     speed_cat == "(0,5 km]", as.character(migration), "")]
#   
#   speed_cat_dt <- speed_dt[, .(
#     ymax = max(sp_id) + 0.5, 
#     ymin = min(sp_id) - 0.5, 
#     ymed = as.numeric(median(sp_id))),
#     by = speed_cat]
#   speed_cat_dt[, txt := fifelse(
#     speed_cat == "(0,5 km]", 
#     paste("median distance:", speed_cat), as.character(speed_cat))]
# 
#   
#   month_limits <- get_month_limits()
#   month_limits[, ymin := max(speed_dt$sp_id) + 0.5]
#   month_limits[, ymax := ymin + ymin*0.02]
#   month_limits[, ymed := ymin + ymin*0.01]
# 
#   species_dt |> 
#     ggplot() + 
#     geom_tile(
#       aes(x = yd, y = sp_id, fill = steps_cat), alpha = 0.8
#     ) +
#     geom_vline(
#       data = month_limits, aes(xintercept = last_yd), color = "gray33") +
#     geom_hline(
#       data = speed_cat_dt, 
#       aes(yintercept = ymax), color = "gray33"
#     ) +
#     geom_vline(aes(xintercept = 0.5), color = "gray33") +
#     geom_rect(
#       data = speed_cat_dt,
#       aes(
#         xmin = -20, xmax = -10, ymin = ymin, ymax = ymax, fill = speed_cat
#       ), alpha = 0.6, color = "gray33"
#     ) +
#     geom_text(
#       data = speed_cat_dt, 
#       aes(y = ymed, x = -15, label = txt), angle = 90, hjust = 0.5, vjust = 0.5
#     ) +
#     geom_rect(
#       data = move_dt,
#       aes(
#         xmin = -10, xmax = 0.5, ymin = ymin, ymax = ymax, fill = migration
#       ), alpha = 0.6, color = "gray33"
#     ) +
#     geom_text(
#       data = move_dt, 
#       aes(y = ymed, x = -5, label = txt), angle = 90, hjust = 0.5, vjust = 0.5
#     ) +
#     geom_rect(
#       data = month_limits,
#       aes(xmin = first_yd, xmax = last_yd, ymin = ymin, ymax = ymax), 
#       fill = "white", alpha = 0.2
#     ) +
#     geom_text(
#       data = month_limits, 
#       aes(x = mid_yd, y = ymed, label = month), 
#       hjust = 0.5, vjust = 0.5, size = 3.5, color = "gray33"
#     ) +
#     scale_fill_manual(
#       values = c(count_col, dist_col, move_col), 
#       breaks = names(count_col)
#     ) +
#     scale_x_continuous(
#       expand = c(0, 0), limits = c(-20, 366.5), breaks = seq(1, 366, 30)
#     ) +
#     scale_y_continuous(
#       expand = c(0, 0), 
#       breaks = speed_dt$sp_id, 
#       labels = speed_dt$species
#     ) +
#     theme_bw()  +
#     theme(
#       axis.ticks = element_blank(), 
#       legend.position = "bottom"
#     ) + 
#     labs(
#       y = "", 
#       x = "day of a year", 
#       fill = "number of steps per day", 
#       title = sprintf(
#         "Distribution of data througout a year - filter: minimum %d tracking days per deployment", 
#         n_fltr), 
#       subtitle = "grouped by species median distance and migration status"
#     ) 
#   
#   pname <- gsub(".csv", ".png", gsub("6_overview_", "6_calendar_", fovw))
#   
#   ggsave(
#     file.path(graphs_dir, "6_distances", pname), width = 12, height = 18 )
#   
# })
# 
