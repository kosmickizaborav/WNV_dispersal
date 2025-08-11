library(data.table)
library(ggplot2)
source("0_helper_functions.R")

data_dir <- here::here("Data")
study_dir <- file.path(data_dir, "Studies")
graphs_dir <- file.path(data_dir, "Graphs")

dist_dirs <- file.path(list.files(study_dir, full.names = T), "6_distances")

finm <- "4_all_tracks_max_active_steps_nauticalDawn_nauticalDusk_continent.rds"
files <- list.files(dist_dirs, pattern = finm, full.names = TRUE)

traits_dt <- fread(here::here("Published_data", "00_bird_traits.csv"))



# 1 - Make overview table -------------------------------------------------

foverview <- "6_distance_overview_table.csv"

if(!file.exists(file.path(data_dir, foverview))){
  
  species_dt <- rbindlist(lapply(seq_along(files), function(n){
    
    fin <- files[n]
    sp <- gsub(".*/Studies/(.*?)_(.*?)/6_distances.*", "\\1 \\2", fin)
    
    dt <- fread(fin)
    
    dt <- dt[, n_steps := .N, by = file][n_steps > 10]
    
    if(nrow(dt) == 0){ return(NULL) }
    
    dt[, yd := day_cycle_to_yd(day_cycle_1)]
    dt[, sensor := sub(".*dep_(.*?)_sen.*", "\\1", file)][
      , sensor := fcase(sensor == 653, "gps", sensor == 2299894820, "sigfox")]
    
    n_deploy_tot <- dt[, uniqueN(file)]
    
    cls <- c("n_days", "n_year_days")
    
    dt_per_file <- dt[, .(
      n_days = uniqueN(day_cycle_1),
      n_year_days = uniqueN(yd), 
      sensor = unique(sensor)
    ), by = file]
    
    dt_per_file <- dt_per_file[, c(
      as.list(setNames(lapply(.SD, median), paste0("median_", cls))),
      as.list(setNames(lapply(.SD, function(x) {
        if(n_deploy_tot == 1) NA else paste(range(x), collapse = " - ") }),
        paste0("range_", cls))),
      N_gps = sum(sensor == "gps"),
      N_sigfox = sum(sensor == "sigfox")
    ), .SDcols = cls]
    
    dt_out <- dt[, c(
      species = gsub(".*/Studies/(.*?)_(.*?)/6_distances.*", "\\1 \\2", fin),
      N_steps = .N, 
      N_days = uniqueN(day_cycle_1),
      N_year_days = uniqueN(yd),
      N_deployments = uniqueN(file), 
      as.list(setNames(
        round(quantile(sl_, probs = c(0.05, 0.25, 0.5, 0.75, 0.95)), 2), 
        paste0("sl_", c("05", "25", "50", "75", "95")))), 
      continents = paste(unique(continent), collapse = ", "), 
      sensors = paste(unique(sensor), collapse = ", ")
    )]
    
    cbind(dt_out, dt_per_file)
    
  }), fill = T)
  
  species_dt <- add_birdlife_phylogeny(species_dt, species_name = "species")
  
  
  traits_dt <- unique(traits_dt[birdlife_name %in% species_dt$species][
    , .(species = birdlife_name, migration_txt, nocturnal)][
      , activity := ifelse(nocturnal == 1, "nocturnal", "diurnal")])
  
  setnames(traits_dt, old = "migration_txt", new = "migration")
  
  species_dt <- merge(species_dt, traits_dt, by = "species")
  
  cls <- c("species", "family", "order", "migration", "nocturnal", "N_deployments")
  
  setcolorder(species_dt, neworder = cls)
  
  fwrite(species_dt, file.path(data_dir, foverview))
  
}


# 2 - Plot ----------------------------------------------------------------

species_dt <- fread(file.path(data_dir, foverview))
species_dt <- species_dt[, .(species, order, sl_50, migration)]

ord_col <- c(
  "#F0A3FFFF", "#0075DCFF", "#993F00FF", "#4C005CFF", "#005C31FF",  "#2BCE48FF", 
  "#FFCC99FF", "#808080FF", "#94FFB5FF", "#8F7C00FF", "#9DCC00FF",  "#003380FF", 
  "#19A405FF", "#FFA8BBFF", "#426600FF", "#5EF1F2FF", "#E0FF66FF",  "#990000FF", 
  "#FFFF80FF", "#FFE100FF", "#FF5000FF")

move_col <- c("#008B8B", "#F4A460", "#8B008B")

# make factors
species_dt[, migration := fcase(
  migration == "partially migratory", "partial", 
  migration == "migratory", "migrant", 
  default = migration)]
species_dt[, migration := factor(
  migration, levels = c("sedentary", "partial", "migrant"))]

species_dt[, sl_order := max(sl_50), by = order]

# order columns
cord <- c("migration", "sl_order", "sl_50")

setorderv(species_dt, cols = cord)

species_dt[, sp_id := .I]

order_dt <- species_dt[, .(
  xmax = max(sp_id), 
  xmin = min(sp_id), 
  xmed = as.numeric(median(sp_id)), 
  count = max(sp_id) - min(sp_id) + 1), 
  by = .(order, migration)]
order_dt[, lab := ifelse(count > 3, order, "")]

move_dt <- species_dt[, .(
  xmax = max(sp_id), 
  xmin = min(sp_id), 
  xmed = median(sp_id)), 
  by = migration]

names(ord_col) <- unique(order_dt$order)
names(move_col) <- unique(move_dt$migration)

species_dt |> 
  ggplot() +
  geom_line(aes(x = sp_id, y = sl_50/1000, color = migration), linewidth = 1.2) +
  geom_rect(
    data = order_dt,
    aes(ymin = -30, ymax = -5, xmin = xmin - 0.5, xmax = xmax + 0.5, 
        fill = order),  color = "gray33",
    alpha = 0.4
  ) +
  geom_rect(
    data = move_dt,
    aes(ymin = -5, ymax = 0, xmin = xmin - 0.5, xmax = xmax + 0.5, 
        fill = migration), alpha = 0.8, color = "gray33"
  ) +
  geom_text(
    data = move_dt, 
    aes(x = xmed, y = -2.5, label = migration), angle = 90, vjust = 0.5, 
    color = "gray33"
  ) +
  geom_text(
    data = order_dt, 
    aes(x = xmed, y = -18, label = lab), vjust = 0.5, 
    color = "gray33"
  ) +
  scale_x_continuous(
    breaks = species_dt$sp_id, 
    labels = species_dt$species, 
    expand = c(0, 0)
  ) +
  theme_bw() +
  scale_fill_manual(values = c(ord_col, move_col)) +
  scale_color_manual(values = move_col) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip(clip = "off") + 
  labs(
    y = "distance [km]", 
    title = "Median daily distance across species"
  ) +
  theme(
    axis.title.y = element_blank(), 
    axis.ticks.y = element_blank(),
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5)
  )

ggsave(file.path(graphs_dir, "6_distance_overview_table_migration.png"), 
       width = 10, height = 15, dpi = 300)


  

# order columns
cord <- c("sl_order", "migration", "sl_50")

setorderv(species_dt, cols = cord)

species_dt[, sp_id := .I]

order_dt <- species_dt[, .(
  xmax = max(sp_id), 
  xmin = min(sp_id), 
  xmed = as.numeric(median(sp_id)), 
  count = max(sp_id) - min(sp_id) + 1), 
  by = order]
order_dt[, lab := ifelse(count > 2, order, "")]

move_dt <- species_dt[, .(
  xmax = max(sp_id), 
  xmin = min(sp_id), 
  xmed = median(sp_id)), 
  by = .(migration, order)]

names(ord_col) <- unique(order_dt$order)
names(move_col) <- unique(move_dt$migration)

species_dt |> 
  ggplot() +
  geom_point(aes(x = sp_id, y = sl_50/1000, color = migration), size = 3, shape = 15) +
  geom_rect(
    data = order_dt,
    aes(ymin = -30, ymax = -5, xmin = xmin - 0.5, xmax = xmax + 0.5, 
        fill = order),  color = "gray33",
    alpha = 0.4
  ) +
  geom_rect(
    data = move_dt,
    aes(ymin = -5, ymax = 0, xmin = xmin - 0.5, xmax = xmax + 0.5, 
        fill = migration), alpha = 0.8, color = "gray33"
  ) +
  geom_text(
    data = order_dt, 
    aes(x = xmed, y = -18, label = lab), vjust = 0.5, 
    color = "gray33"
  ) +
  scale_x_continuous(
    breaks = species_dt$sp_id, 
    labels = species_dt$species, 
    expand = c(0, 0)
  ) +
  theme_bw() +
  scale_fill_manual(values = c(ord_col, move_col)) +
  scale_color_manual(values = move_col) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0,100, 10)) +
  coord_flip(clip = "off") + 
  labs(
    y = "distance [km]", 
    title = "Median daily distance across species"
  ) +
  theme(
    axis.title.y = element_blank(), 
    axis.ticks.y = element_blank(),
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5)
  )

ggsave(file.path(graphs_dir, "6_distance_overview_table_order.png"), 
       width = 10, height = 15, dpi = 300)




