library(data.table)
library(ggplot2)
source("0_helper_functions.R")

data_dir <- here::here("Data")
study_dir <- file.path(data_dir, "Studies")

dist_dirs <- file.path(list.files(study_dir, full.names = T), "6_distances")

finm <- "4_all_tracks_max_active_steps_nauticalDawn_nauticalDusk_continent.rds"
files <- list.files(dist_dirs, pattern = finm, full.names = TRUE)

traits_dt <- fread(here::here("Published_data", "00_bird_traits.csv"))

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

species_dt[, migration := factor(
  migration, levels = c("sedentary", "partially migratory", "migratory"))]

species_dt[, sl_order := max(sl_50), by = order]

setorder(species_dt, migration, sl_order, N_steps)

species_dt[, sl_order := NULL]

species_dt[, sp_id := .I]


order_dt <- species_dt[, .(
  xmax = max(sp_id), 
  xmin = min(sp_id)), 
  by = order]

species_dt |> 
  ggplot() +
  geom_line(aes(x = sp_id, y = sl_50/1000, color = migration)) +
  geom_rect(
    data = order_dt,
    aes(ymin = -10, ymax = 0, xmin = xmin - 0.5, xmax = xmax + 0.5, 
        fill = order),  color = "gray33",
    alpha = 0.2
  ) +
  theme_bw() +
  coord_flip()
