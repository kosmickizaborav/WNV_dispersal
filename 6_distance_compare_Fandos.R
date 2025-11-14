library(data.table)
library(ggplot2)
library(ggdist)
library(ggrepel)
library(patchwork)
source("0_helper_functions.R")
source("6_distance_PLOT_FUNCTIONS.R")
source("color_palettes.R")


data_dir <- here::here("Data")
study_dir <- file.path(data_dir, "Studies")

graphs_dir <- file.path(data_dir, "Graphs")

fin <- "6_overview_filter_30_max_active_steps_nauticalDawn_nauticalDusk_continent_sl_median.csv"


dt <- fread(file.path(data_dir, fin))

fandos_dt <- fread(here::here(
  "Published_data", "Fandos2023_Table_S14_ species_dispersal_distances_v1_0_2.csv"))

fandos_dt <- rename_to_birdlife(fandos_dt, "species")


fandos_dt <- fandos_dt[
  birdlife_name %in% dt$species, .(median, birdlife_name, function_id)]
fandos_avr <- fandos_dt[
  , .(sl_50_avr = mean(median, na.rm = TRUE)), by = birdlife_name]


merged_dt <- merge(fandos_avr, dt, by.x = "birdlife_name", by.y = "species")

merged_dt[, order_txt := paste0(order, " [", .N, "]"), by = order]

matched_idx <- match(names(ord_col), merged_dt$order)
names(ord_col) <- merged_dt$order_txt[matched_idx]
  
  
merged_dt |> 
  ggplot() +
  geom_point(
    aes(x = sl_50_avr, y = sl_50/1000, color = order_txt), size = 3, alpha = 0.5
  ) + 
  ggrepel::geom_text_repel(
    aes(x = sl_50_avr, y = sl_50/1000, label = birdlife_name), size = 3
  ) +
  scale_color_manual(values = ord_col) +
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
