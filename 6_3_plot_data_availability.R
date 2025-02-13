#' ---
#' title: "Plotting steps in time and space"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#'  **SECTION 1 - Function: get_palette**
#'  helper function to getting and standardizing the colors across years
#'  **SECTION 2 - Function: plot_steps_timeline**
#'  plots the timeline of the bursts that were used to calculate the steps. 
#'  in this way we see the distribution of the daya trhough time and seasons.
#'  under the timeline, is ploted total number of steps available per day of 
#'  a year. 
#'  **SECTION 3 - Function: expand_bbox**
#'  helper function for expanding the bbox of a map
#'  **SECTION 4 - Function: plot_on_world_map**
#'  plots the points used to calculate steps on the world map. in this way
#'  we see the spatial distribution of the points used
#'  **SECTION 5 - Plot for every species**
#'  generate plot for every species and every night steps file we have 


# 0 - Load packages -------------------------------------------------------

library(tidyverse)
library(here)
library(paletteer)
library(rnaturalearth)
library(sf)
library(patchwork)

# getting the species of interest
target_sp <- here("Data", "1_downloadable_studies_deployments_filtered.rds") |> 
  read_rds() |>
  distinct(species) |> 
  as_vector()


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
  id_buff <- round(id_max/5)
  
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
      y_lab = if_else(n_tracks > id_buff | n_tracks > mean(n_tracks), y, "")
    )
  
  ptl <- ggplot() +
    geom_hline(aes(yintercept = id_max)) +
    geom_vline(aes(xintercept = 1)) +
    # plotting bursts from start to end day of the year
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
    # show year limits in color boxes on the right side
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
    # draw lines deliniating each month
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
      y = "track bursts \n per deployment", 
      title = "Distribution of data across years"
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
    labs(x = "day of the year", y = "steps \nper day") +
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
  
  return(pout)
  
}  



# 3 - Function: expand_bbox -----------------------------------------------

# Function to expand bbox by a given degree
expand_bbox <- function(bbox, deg) {
  bbox["xmin"] <- bbox["xmin"] - deg
  bbox["ymin"] <- bbox["ymin"] - deg
  bbox["xmax"] <- bbox["xmax"] + deg
  bbox["ymax"] <- bbox["ymax"] + deg
  return(bbox)
}


# 4 - Function: plot_on_world_map -----------------------------------------

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
  
  # generate the polygon that will "zoom in from the world map
  zoom_pol <- matrix(
    c(
      ns_bbox$xmax, ns_bbox$ymax,  
      180, 90,  
      180, -90, 
      ns_bbox$xmax, ns_bbox$ymin, 
      ns_bbox$xmax, ns_bbox$ymax 
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
    labs(
      title = "Distribution of data in space") +
    theme(
      plot.margin = margin(0, 0, 0, 0, unit = "cm"), 
      panel.spacing = unit(0, "cm"), 
      plot.title = element_text(hjust = 0)
    )

  pout <- (wm + mm) 
  
  return(pout)
}


# 5 - Plot for every species ----------------------------------------------


target_sp |> 
  map(~{
    
    sp <- .x
    
    sp_dir <- here("Data",  "Studies", str_replace(sp, " ", "_"))
    files <- here(sp_dir, "6_distances") |> 
      list.files(pattern = "2_all_tracks_night_steps_")
    
    files |> 
      map(~{
        
        fin <- .x
        fout <- str_c("6_3_plot_", str_remove(fin, ".rds"), ".pdf")
        
        night_steps <- here(sp_dir, "6_distances", fin) |> 
          read_rds() |> 
          filter(night_steps_available == T) |> 
          mutate(
            day_cycle = as.Date(day_cycle, format = "%d%m%Y"), 
            yd = yday(day_cycle),
            m = month(day_cycle), 
            d = day(day_cycle), 
            y = year(day_cycle)
          )
        
        # palette that will indicate different years
        pal <- get_palette(night_steps, y = y)
        
        # plots
        wmap <- plot_on_world_map(night_steps, pal = pal)
        tl <- plot_steps_timeline(night_steps, pal = pal)
        
        pout <- wmap/tl + 
          plot_annotation(
            title = str_c(
              sp, "\n",
              "spatial and temporal distribution of tracking data used to obtain night steps"
            ), 
            caption = str_c(
              "file: ", fin, "\n",
              "color of lines in timeline and points in map indicate year, see timeline for years with the most data"
            ),
            theme = theme(plot.title = element_text(face = "bold", hjust = 0.5))
          ) +
          plot_layout(heights = c(1.5, 1))
          
        # save the plots
        ggsave(
          here(sp_dir, "Graphs", fout), 
          pout, 
          width = 25, height = 20, units = "cm"
        )
        

      })
    
    print(paste(sp, "DONE!"))
      
    
  })
