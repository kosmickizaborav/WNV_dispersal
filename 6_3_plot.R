
# 0 - Load packages -------------------------------------------------------

library(tidyverse)
library(here)
library(paletteer)
library(rnaturalearth)
library(sf)
library(ggmagnify)
library(patchwork)

# getting the species of interest
target_sp <- here("Data", "1_downloadable_studies_deployments_filtered.rds") |> 
  read_rds() |>
  distinct(species) |> 
  as_vector()

sp <- target_sp[2]

sp_dir <- here("Data",  "Studies", str_replace(sp, " ", "_"))
files <- here(sp_dir, "6_distances_for") |> 
  list.files()


night_files <- files[str_detect(files, "2_all_tracks_night_steps_")]

night_steps <- night_files[1] |> 
  map(~{
    
    here(sp_dir, "6_distances", .x) |> 
      read_rds() |> 
      mutate(day_lim = str_remove_all(.x, "2_all_tracks_night_steps_|\\.rds")) |> 
      filter(night_steps_available == T) |> 
      mutate(
        day_cycle = as.Date(day_cycle, format = "%d%m%Y"), 
        yd = yday(day_cycle),
        m = month(day_cycle), 
        d = day(day_cycle), 
        y = year(day_cycle)
      )
    
  }) |> 
  list_rbind()






# 1 - Function: get_palette -----------------------------------------------

# get palette colors for years
get_palette <- function(df, y = y){
  
  years <- df |> 
    distinct({{y}}) |> 
    pull()
  years <- years[order(years)]
  
  pall <- paletteer_c("grDevices::Spectral", n = length(years))  
  names(pall) <- years
  
  return(pall)
}

pal <- get_palette(df = night_steps, y = y)
# 2 - Function: plot_steps_timeline ---------------------------------------

plot_steps_timeline <- function(df = NULL, pal = NULL){

  # summarize tracks by bursts_ and year
  df_bursts <- df |> 
    # extract only columns of interest
    select(y, yd, track_file, burst_) |> 
    # find begining adn end of each burst
    summarize(
      brs_start = min(yd), 
      brs_end = max(yd), 
      .by = c(burst_, y, track_file)
    ) |> 
    arrange(y, track_file, burst_) |> 
    # generate track id just for plotting
    mutate(
      id = dense_rank(str_c(y, "-", track_file)), 
      # convert to factor for coloring
      y = as.factor(y)
    )
  
  count_df <- df |> 
    summarize(step_count = n(), .by = yd)
  
  rm(df)

  id_max <- max(df_bursts$id)
  id_buff <- round(id_max*0.2)
  
  # generate dataframe for plotting edges of each month
  month_limits <- tibble(year = 2020, month = 1:12) |> 
    mutate(
      #last_yd = yday(ceiling_date(first_day, "month")),
      first_yd = yday(as.Date(paste(year, month, "01", sep = "-"))), 
      mid_yd = yday(as.Date(paste(year, month, "15", sep = "-"))),
      last_yd = c(first_yd[-1], 366),
      m_lab = month(month, label = T, abbr = T), 
      # for horizontal lines in the plot
      id_min = min(df_bursts$id), 
      id_max = max(df_bursts$id)
    ) |> 
    select(-year, -month, first_yd)
  
  # find the first track within each year to give limits to years
  year_limits <- df_bursts |> 
    summarize(
      n_tracks = length(unique(track_file)), 
      id_min = min(id), 
      .by = y
    ) |>
    mutate(
      # using this instead of max(id) to avoid white gaps
      id_max = c(id_min[-1], id_max),
      id_mid = id_min + (id_max - id_min)/2,
      y_lab = if_else(n_tracks > id_buff, y, "")
    )
  
  ptl <- ggplot() +
    geom_hline(aes(yintercept = id_max)) +
    geom_vline(aes(xintercept = 1)) +
    geom_segment(
      data = df_bursts,
      aes(
        x = brs_start, xend = brs_end, 
        y = id, yend = id,
        color = y
      ),
      linewidth = 2, 
      alpha = 0.7
    ) +
    geom_rect(
      data = year_limits,
      aes(
        xmin = 366, xmax = 386,
        ymin = id_min, ymax = id_max,
        fill = y
      ),
      color = "gray33",
      alpha = 0.7
    ) +
    geom_text(
      data = year_limits,
      aes(x = 376, y = id_mid, label = y_lab),
      color = "black",
      hjust = 0.5,
      vjust = 0.5
    ) +
    geom_segment(
      data = month_limits, 
      aes(x = last_yd, xend = last_yd, y = id_min, yend = id_max), 
      color = "gray33"
    ) +
    geom_rect(
      data = month_limits,
      aes(xmin = first_yd, xmax = last_yd, ymin = -id_buff, ymax = 1), 
      color = "gray33", 
      fill = "white"
    ) +
    geom_text(
      data = month_limits, aes(x = mid_yd, y = -id_buff/2, label = m_lab),
      hjust = 0.5, vjust = 0
    ) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0), lim = c(-id_buff, id_max)) +
    scale_color_manual(values = pal) +
    scale_fill_manual(values = pal) +
    theme_void() +
    labs(
      y = "available track bursts per deployment", 
      title = "Distribution of night steps in time"
    ) +
    theme(
      axis.title.y = element_text(angle = 90, vjust = 0.5),
      plot.title = element_text(hjust = 0),
      plot.margin = margin(t = 1, r = 0, b = 0, l = 0, unit = "mm"), 
      panel.spacing = unit(0, "cm"), 
      legend.position = "none"
    ) 
   
  
  psc <- ggplot() + 
    geom_segment(aes(x = 1, xend = 366, y = 0, yend = 0), color = "gray33") +
    geom_vline(data = month_limits, aes(xintercept = last_yd), color = "gray33") +
    geom_step(data = count_df, aes(y = step_count, x = yd), ) +
    scale_x_continuous(
      expand = c(0,0), limits = c(1, 386), breaks = seq(1, 366, 14)
    ) +
    scale_y_continuous(
      expand = c(0, 0), limits = c(0, max(count_df$step_count)+1)
    ) +
    theme_bw() +
    labs(x = "day of the year", y = "total step count") +
    theme(
      plot.margin = margin(t = 0, r = 0, b = 1, l = 1, unit = "mm"), 
      axis.ticks = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.spacing = unit(0, "cm"), 
      panel.border = element_blank(),
      axis.line.y = element_line(color = "gray33")
    )
  
  pout <- ptl/psc + 
    plot_layout(heights = c(2, 1))
    # plot_annotation(
    #   title = "Distribution of night steps in time", 
    #   theme = theme(plot.title = element_text(hjust = 0.5))
    # )
  
  return(pout)
  
}  



# 4 - Function: expand_bbox -----------------------------------------------

# Function to expand bbox by a given degree
expand_bbox <- function(bbox, deg) {
  bbox["xmin"] <- bbox["xmin"] - deg
  bbox["ymin"] <- bbox["ymin"] - deg
  bbox["xmax"] <- bbox["xmax"] + deg
  bbox["ymax"] <- bbox["ymax"] + deg
  return(bbox)
}


# 5 - Function: plot_on_world_map -----------------------------------------


plot_on_world_map <- function(df, in_crs = 4326, pal = NULL){
  
  world <- ne_countries(scale = "medium", returnclass = "sf") 
  
  # covert steps to sf objects of points and tracks so that we can plot them
  ns_points <- df |> 
    mutate(tb_id = str_c(dense_rank(track_file), "_", burst_)) |> 
    group_split(tb_id) |> 
    map(~{
      tibble(
        lon = c(.x$x1_, .x$x2_[nrow(.x)]),
        lat = c(.x$y1_, .x$y2_[nrow(.x)]),
        tb_id = unique(.x$tb_id),
        y = as.factor(year(c(.x$t1_, .x$t2_[nrow(.x)])))
      )
    }) |>  
    list_rbind() |> 
    st_as_sf(coords = c("lon", "lat"), crs = in_crs)
  
  ns_bbox <- st_bbox(ns_points) |> expand_bbox(5)

  # main plot
  mm <- ggplot() + 
    geom_sf(data = world, fill = "white") + 
    # geom_sf(
    #   data = ns_tracks, 
    #   aes(group = tb_id, color = y), 
    #   alpha = 0.5,
    #   linewidth = 2
    # ) +
    geom_sf(data = ns_points, aes(color = y), alpha = 0.5, size = 1.5) +
    coord_sf(
      xlim = c(ns_bbox$xmin, ns_bbox$xmax),
      ylim = c(ns_bbox$ymin, ns_bbox$ymax), 
      expand = F
    ) +
    theme_void() + 
    scale_color_manual(values = pal) +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      legend.position = "none", 
      plot.margin =  margin(0, 0, 0, 0, unit = "cm"), 
      panel.spacing = unit(0, "cm"),
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
    )
  
  # generate the poligon that will "zoom in from the world map
  zoom_pol <- matrix(
    c(
      ns_bbox$xmax, ns_bbox$ymax,  # First point (X, Y)
      180, 90,  # Second point (X, Y)
      180, -90, # Third point (X, Y)
      ns_bbox$xmax, ns_bbox$ymin,  # Fourth point (X, Y)
      ns_bbox$xmax, ns_bbox$ymax # close the loop
    ),
    ncol = 2, 
    byrow = TRUE
    ) |> 
    list() |> 
    st_polygon()
  
  zoom_pol <- st_sf(geometry = st_sfc(zoom_pol), crs = in_crs)
  
  ns_bbox <- st_as_sfc(ns_bbox)
  
  wm <- world |>  
    ggplot() +
    geom_sf(fill = "white") +
    geom_sf(
      data = ns_bbox, 
      fill = "gray33", 
      color = "black", 
      linewidth = 0.5, 
      alpha = 0.5
    ) +
    geom_sf(
      data = zoom_pol, 
      fill = "gray33", 
      alpha = 0.5, 
      color = "black", 
      linewidth = 0.5
    ) +
    coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = F) +
    theme_void() +
    labs(title = "Distribution of night steps in space") +
    theme(
      plot.margin = margin(0, 0, 0, 0, unit = "cm"), 
      panel.spacing = unit(0, "cm"), 
      plot.title = element_text(hjust = 0)
    )

  pout <- (wm + mm) 
    # plot_annotation(
    #   title = "Spatial distribution of night steps", 
    #   theme = theme(plot.title = element_text(hjust = 0.5))
    # )
  
  return(pout)
}



# 5 - Function: plot_step_count -------------------------------------------




pall <- get_palette(night_steps$y)

wmap <- night_steps |> plot_on_world_map(pal = pal)

tl <- night_steps |> 
  plot_steps_timeline(pal = pal)


wmap/tl + plot_layout(widths = c(2, 1))

# define palette for the graph
colp <- paletteer_d("MoMAColors::Lupi")
# define breaks for the colors 
col_break <- c(1, 5, 10, 25, 50, 75, 100)




night_steps |> 
  plot_steps_timeline(pall = pall)

# original plot to save just in case
# 
# ptl <- ggplot() +
#   geom_hline(aes(yintercept = id_max)) +
#   geom_segment(
#     data = df_bursts,
#     aes(
#       x = brs_start, xend = brs_end, 
#       y = id, yend = id,
#       color = y
#     ),
#     linewidth = 2, 
#     alpha = 0.7
#   ) +
#   geom_rect(
#     data = year_limits,
#     aes(
#       xmin = -20, xmax = 1,
#       ymin = id_min, ymax = id_max,
#       fill = y
#     ),
#     color = "gray3333",
#     alpha = 0.7
#   ) +
#   geom_text(
#     data = year_limits,
#     aes(x = -10, y = id_mid, label = y_lab),
#     color = "gray3333",
#     hjust = 0.5,
#     vjust = 0.5
#   ) +
#   geom_segment(
#     data = month_limits, 
#     aes(x = last_yd, xend = last_yd, y = id_min, yend = id_max), 
#     color = "gray3333"
#   ) +
#   geom_rect(
#     data = month_limits,
#     aes(xmin = first_yd, xmax = last_yd, ymin = -id_buff, ymax = 1), 
#     color = "gray3333", 
#     fill = "white"
#   ) +
#   geom_text(
#     data = month_limits, aes(x = mid_yd, y = -id_buff/2, label = m_lab),
#     hjust = 0.5, vjust = 0
#   ) +
#   scale_x_continuous(expand = c(0,0)) +
#   scale_y_continuous(expand = c(0,0)) +
#   scale_color_manual(values = pal) +
#   scale_fill_manual(values = pal) +
#   # labs(
#   #   x = "time of the year", 
#   #   y = "available night steps per deployment"
#   # ) +
#   theme_minimal() +
#   theme(
#     axis.text = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.grid.major = element_blank(),
#     legend.position = "none", 
#     plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
#   ) +
#   coord_cartesian(clip = "off")
# 

# yb <- year_limits |> 
#   ggplot() +
#   geom_rect(
#     aes(
#       xmin = -20, xmax = 1, 
#       ymin = id_min, ymax = id_max,
#       fill = y
#     ), 
#     color = "gray33", 
#     alpha = 0.7
#   ) +
#   geom_text(
#     data = year_limits,
#     aes(x = -10, y = id_mid, label = y_lab),
#     color = "black",
#     hjust = 0.5,
#     vjust = 0.5
#   ) +
#   scale_fill_manual(values = pall) +
#   scale_y_continuous(expand = c(0,0), lim = c(-id_buff, id_max)) +
#   scale_x_continuous(expand = c(0,0)) +
#   theme_void() +
#   theme(
#     legend.position = "none", 
#     plot.margin = margin(t = 1, r = 0, b = 0, l = 0, unit = "mm"), 
#     panel.spacing = unit(0, "cm")
#   ) 

# plot_step_count <- function(df){
#   
#   df |> 
#     summarize(step_count = n(), .by = yd) |> 
#     ggplot() + 
#     geom_step(aes(y = step_count, x = yd)) +
#     scale_x_continuous(expand = c(0,0), limits = c(1, 366)) +
#     theme_bw() +
#     labs(x = "day of the year", y = "total step count")
#   
# }