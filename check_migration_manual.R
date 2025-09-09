library(data.table)
library(ggplot2)
library(sf)

# ALL OUTPUT DIRECTORIES
data_dir <- here::here("Data")
study_dir <- file.path(data_dir, "Studies")
graph_dir <- file.path(data_dir, "Graphs")

target_sp <- c("Circus aeruginosus", "Falco naumanni", "Milvus migrans",
               "Neophron percnopterus")

sp <- target_sp[1]

sp_dir <- file.path(study_dir, gsub(" ", "_", sp), "6_distances") 


ddt <- fread(
  list.files(sp_dir, "max_.*_nauticalDawn_nauticalDusk_continent", full.names = T))


track <- ddt[file == "/home/nina/R_projects/WNV_dispersal/Data/Studies/Circus_aeruginosus/5_flag_static/1278021460_stu_1296952627_ind_1332844116_dep_653_sen.rds"]

track[, m := month(t1_)]


p <- st_as_sf(track, coords = c("x1_", "y1_"), crs = 4326)

track[, migration := fcase(
  m %in% c(3, 9), T, 
  m %in% c(1,2, 4:7, 10:12), F, 
  default = NA)]

pp <- p |> 
  ggplot() +
  geom_sf(aes(color = migration)) +
  facet_wrap(~m) +
  theme_bw() + 
  theme(legend.position = "none")



pb <- track |>  
  ggplot() + 
  geom_boxplot(aes(x = sl_median, y = migration, fill = migration)) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  labs(x = "maximum daily distance [m] - log scale") + 
  scale_x_log10()

pp + pb + 
  plot_annotation(
    title = sp, 
    subtitle = 
  )

ggsave()

