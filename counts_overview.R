library(data.table)
source("0_helper_functions.R")
library(ggplot2)

data_dir <- here::here("Data")
study_dir <- file.path(data_dir, "Studies")

dist_dirs <- file.path(list.files(study_dir, full.names = T), "6_distances")

finm <- "4_all_tracks_max_active_steps_nauticalDawn_nauticalDusk_continent.rds"
files <- list.files(dist_dirs, pattern = finm, full.names = TRUE)

tr


species_dt <- rbindlist(lapply(seq_along(files), function(n){
  
  fin <- files[n]
  
  dt <- fread(fin)
  
  dt <- dt[, n_steps := .N, by = file][n_steps > 10]
  dt[, yd := day_cycle_to_yd(day_cycle_1)]
  dt[, continent_check := ifelse(continent == "Europe", "Europe", "Outside Europe")]
  
  dt <- dt[, .(n_depl = uniqueN(file)), by = .(continent_check, yd)]
    
  dt[, species := gsub(".*/Studies/(.*?)_(.*?)/6_distances.*", "\\1 \\2", fin)]
  
}))

species_dt <- add_birdlife_phylogeny(species_dt, species_name = "species")

peu <- species_dt[continent_check == "Europe"] |>  
  ggplot() +
  geom_tile(aes(x = yd, y = species, fill = n_depl)) +
  labs(x = "Year Day", y = "Number of Deployments") +
  theme_bw() +
  facet_wrap(~continent_check, strip.position = "right") +
  scale_x_continuous(breaks = seq(0, 367)) +
  theme(
    axis.title.y.right = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank()
  )


traits_dt <- fread(here::here("Published_data", "00_bird_traits.csv"))[
  birdlife_name %in% species_dt$species]
