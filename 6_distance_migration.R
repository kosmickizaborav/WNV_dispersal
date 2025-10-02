library(data.table)

traits_dt <- fread(here::here("Published_data", "00_bird_traits.csv"))
traits_dt <- unique(traits_dt[
  , .(species = birdlife_name, migration = migration_txt, order)])
traits_dt <- traits_dt[species %in% target_sp]


fmed <- "3_all_tracks_median_active_steps_nauticalDawn_nauticalDusk_continent.rds"

fmax <- "4_all_tracks_max_active_steps_nauticalDawn_nauticalDusk_continent.rds"


files <- list.files(dist_dirs, pattern = fmax, full.names = T)
n_fltr <- 30



# 1 - Differences in daily distances --------------------------------------


# #mlvl <- c("sedentary", "partial", "migrant")
# mlbl <- c("sedentary", "partial migrant", "migrant")
# move_col <- c("#A3DA8D", "#F4A460", "#781D42")
# names(move_col) <- mlbl

species_dt <- rbindlist(lapply(seq_along(files), function(n){

  fin <- files[n]
  sp <- gsub(".*/Studies/(.*?)_(.*?)/6_distances.*", "\\1 \\2", fin)

  dt <- fread(fin)

  dt[, sl_median := fifelse(
    sl_n_steps == 1, as.numeric(sl_), as.numeric(sl_median))]

  dt <- dt[, .(file, day_cycle_1, sl_max = sl_, sl_median_max = sl_median)]

  # filter tracks that have less than 10 steps
  dt <- dt[, n_steps := .N, by = file][n_steps >= n_fltr]
  
  dt[, yd := ]
  
  data.table(species = sp, n_depl = uniqueN(dt$file))

}))
#   