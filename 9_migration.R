library(data.table)
library(sf)


data_dir <- here::here("Data")
study_dir <- file.path(data_dir, "Studies")
graphs_dir <- file.path(data_dir, "Graphs")

dist_dirs <- file.path(list.files(study_dir, full.names = T), "6_distances")

fin_name <- "1_all_tracks_dcp_distances_nauticalDawn_nauticalDusk_continent.rds"


files <- list.files(dist_dirs, fin_name, full.names = T, recursive = T)

fin <- files[10]

dcp_dt <- fread(fin)

dcp_dt <- dcp_dt[day_period == "night"]

dcp_dt[, wk := paste(year(day_cycle), week(day_cycle))]


points_by_week <- split(dcp_dt, by = c("wk", "file"))

# Example: Convert points to an sf object for each month
sf_points_by_month <- lapply(points_by_week, function(df) {
  st_as_sf(df, coords = c("x_median", "y_median"), crs = 4326) # WGS84
})

sf_polygons_by_month <- lapply(sf_points_by_month, function(sfg) {
  st_convex_hull(st_union(sfg)) # convex hull
})

n_months <- length(sf_polygons_by_month)
overlap_matrix <- matrix(FALSE, n_months, n_months)

for (i in 1:(n_months-1)) {
  for (j in (i+1):n_months) {
    overlap_matrix[i, j] <- st_intersects(sf_polygons_by_month[[i]], sf_polygons_by_month[[j]], sparse = FALSE)
  }
}
