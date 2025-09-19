library(data.table)
library(EMbC)
library(ggplot2)

data_dir <- here::here("Data")
study_dir <- file.path(data_dir, "Studies")
graphs_dir <- file.path(data_dir, "Graphs")

file_in <- "4_all_tracks_max_active_steps_nauticalDawn_nauticalDusk_continent.rds"
file_inn <- "1_all_tracks_dcp_distances_nauticalDawn_nauticalDusk_continent.rds"


dist_dirs <- file.path(list.files(study_dir, full.names = T), "6_distances")

files <- list.files(dist_dirs, file_in, full.names = T)

sp_dt <- rbindlist(lapply(files, function(fin){
  fread(fin)[
    , m := month(t1_)][
    , .(n_month = .N), by = m][
    , species := gsub(".*Studies/(.*)_(.*)/6_distances.*", "\\1 \\2", fin)]
}))[n_month > 20][, monthsN := uniqueN(m), by = species]


target_sp <- sp_dt[monthsN == 12, unique(species)]

sp <- "Ciconia ciconia"

fin <- files[grepl(gsub(" ", "_", sp), files)]

ddt <- fread(fin)[, m := month(day_cycle_2)]

"/home/nina/R_projects/WNV_dispersal/Data/Studies/Ciconia_ciconia/5_flag_static/10157679_stu_10157692_ind_10174292_dep_653_sen.rds"

sf::st_as_sf(ddt, coords = c("x2_", "y2_"), crs = 4326) |> 
  ggplot() +
  geom_sf( size = 0.5) +
  facet_wrap(~m)

ndt <- fread(gsub(file_in, file_inn, fin))
ndt <- ndt[day_period == "night"][, day_cycle_2 := day_cycle]

ndt <- ndt[, .(x3_ = x_median, y3_ = y_median, day_cycle_2 = day_cycle, 
               file, t3_ = t_median)]

ddt <- merge(ddt, ndt, by = c("file", "day_cycle_2"))

setorder(ddt, file, t1_)
setcolorder(ddt, c("day_cycle_1", "day_cycle_2", "t1_", "t2_", "t3_", "x1_", "y1_", "x2_", "y2_", "x3_", "y3_"))

ddt[, ':='(
  # Bearings: A->B and B->C
  bearing1 = geosphere::bearing(cbind(x1_, y1_), cbind(x2_, y2_)),
  bearing2 = geosphere::bearing(cbind(x2_, y2_), cbind(x3_, y3_))
)]

ddt[, turning_angle := {
  # Angle difference
  angle = (bearing2 - bearing1) %% 360
  # Make sure angle is between 0 and 180
  angle = ifelse(angle > 180, 360 - angle, angle)
  angle
}]

crs <- sf::st_crs(4326)

ddt[, geometry_1 := sf::st_as_sf(.SD, coords = c("x1_", "y1_"), crs = crs), 
    .SDcols = c("x1_", "y1_")]
ddt[, geometry_2 := sf::st_as_sf(.SD, coords = c("x2_", "y2_"), crs = crs), 
  .SDcols = c("x2_", "y2_")]
ddt[, geometry_3 := sf::st_as_sf(.SD, coords = c("x3_", "y3_"), crs = crs), 
    .SDcols = c("x3_", "y3_")]

ddt[, dist_go := as.numeric(sf::st_distance(geometry_1, geometry_2, by_element = T))]
ddt[, dist_return := as.numeric(sf::st_distance(geometry_2, geometry_3, by_element = T))]
ddt[, dist_abs := as.numeric(sf::st_distance(geometry_1, geometry_3, by_element = T))]
ddt[, dist_return := ifelse(dist_return<=0, 1, dist_return)]
ddt[, passage := dist_go/dist_return]

ddt_trial <- ddt[, .(dist_return, turning_angle)]


ccls <- embc(as.matrix(ddt_trial), maxItr = 1000)

ddt[, cls := ccls@A]

p <- ddt[, .(x2_, y2_, file, day_cycle_2)][, cls := ccls@A][, m := month(day_cycle_2)]

sf::st_as_sf(
  trk[, m := month(t2_)], coords = c("x2_", "y2_"), crs = 4326
  ) |> 
  ggplot() +
  geom_sf(aes(color = t2_), size = 0.5) +
  facet_wrap(~m)


ddt |> 
  ggplot() + 
  geom_point(aes(x = dist_go, y = dist_return, color = as.factor(cls)), alpha = 0.3) +
  theme_bw() + 
  facet_wrap(~m)

ddt[file == fl] |> 
  ggplot() + 
  geom_histogram(aes(x = dist_go, y = after_stat(density))) +
  theme_bw() + 
  scale_x_log10() +
  facet_wrap(~m)

angle <- move2::mt_turnangle(dpm)*180/pi

ddt <- merge(ddt, ndt, by = c("file", "day_cycle_2"), all.x = T)

na.omit(ddt$day_cycle_3[!ddt$day_cycle_3 %in% ddt$day_cycle_1])


trial <- dist_dirs <- file.path(list.files(study_dir, full.names = T), "6_distances")




dist_dirs <- file.path(
  list.files(study_dir, full.names = T), "7_sleep_clusters", "1_raw_distances")

files <- list.files("/home/nina/R_projects/WNV_dispersal/Data/Studies/Ciconia_ciconia/7_sleep_clusters/1_raw_distances", full.names = T)

dist_mat <- rbindlist(lapply(files, function(fin){
 
  mm <- readRDS(fin)
   
}))