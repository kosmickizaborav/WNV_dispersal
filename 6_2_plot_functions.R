#' ---
#' title: "Functions used to visualize data"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' all functions used to visualize data are moved to one script because I 
#' had a need to reuse them more often.
#'  **FUNCTION 1: get_palette**
#'  helper function to getting and standardizing the colors across years
#'  **FUNCTION 2: is_color**
#'  helper function, checks if color is provided or a variable name
#'  **FUNCTION 3: get_month_limits**
#'  helper function, gets the table with month limit including year-days
#'  **FUNCTION 4: plot_steps_count**
#'  geom_step showing the number of steps per day of the year
#'  **FUNCTION 5: plot_steps_timeline**
#'  plots the timeline of the bursts that were used to calculate the steps. 
#'  in this way we see the distribution of the daya trhough time and seasons.
#'  under the timeline, is ploted total number of steps available per day of 
#'  a year. 
#'  **FUNCTION 6: expand_bbox**
#'  helper function for expanding the bbox of a map
#'  **FUNCTION 7 - FUNCTION: plot_on_world_map**
#'  plots the points used to calculate steps on the world map. in this way
#'  we see the spatial distribution of the points used
#'  **FUNCTION 8: plot_bars**
#'  plots the bars of the data to avoid repeating code for different summaries

# 0 - Load packages -------------------------------------------------------

library(rlang)
library(tidyverse)
library(paletteer)
library(rnaturalearth)
library(sf)
library(patchwork)

# FUNCTION 1: get_palette  --------------------------------------------------

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

# FUNCTION 2: is_color ------------------------------------------------------

is_color <- function(cstr) {
  
  # regex for hexadecimal color code
  hex_regex <- "^#(?:[0-9a-fA-F]{3}){1,2}(?:[0-9a-fA-F]{2})?$"
  # regex for RGB color code
  rgb_regex <- "^rgb\\(\\s*(?:[0-9]{1,3}\\s*,\\s*){2}[0-9]{1,3}\\s*\\)$"
  # vector of named colors
  named_colors <- colors()
  
  # check if the string matches any of the patterns
  check <- str_detect(
    cstr, 
    str_c(c(hex_regex, rgb_regex, named_colors), collapse = "|")
  )
  
  return(check)
}


# FUNCTION 3: get_month_limits -----------------------------------------------

get_month_limits <- function(
    year = 2020, month = 1:12, 
    last_day = "first", keep_yd = T, keep_dates = F, abbr = T
    ){
  
  dc <- ifelse(keep_dates, c("_date"), " ")
  ydc <- ifelse(keep_yd, "_yd", " ")
  
  month_limits <- tibble(year = year, month = month) |> 
    mutate(
      first_date = as.Date(paste(year, month, "01", sep = "-")),
      last_date = ceiling_date(first_date, "month")-1,
      mid_date = first_date + (last_date-first_date)/2-1,
      first_yd = yday(first_date), 
      mid_yd = yday(mid_date),
      last_yd = if(last_day == "first") 
        c(first_yd[-1], yday(last_date[n()])) else yday(last_date),
      month_lab = month(month, label = T, abbr = abbr)
    ) |> 
    select(ends_with(c(dc, ydc)), month_lab)
  
  return(month_limits)
  
}

# FUNCTION 4: plot_steps_count ----------------------------------------------


plot_steps_count <- function(
    df, counts, color, 
    linetype = "solid", 
    linewidth = 1.2, 
    alpha = 0.8,
    title = NULL, 
    xlab = "day of the year", 
    ylab = "steps per day",
    pal = NULL, 
    month_limits = NULL,
    month_labels = T
){
  
  y_max <-  df |> pull({{counts}}) |> max()
  
  if(is.null(month_limits)){ month_limits <- get_month_limits() }
  
  month_limits <- month_limits |> 
    mutate(y_max = y_max)
  
  p <- df |> 
    ggplot(aes(y = {{counts}}, x = yd)) + 
    geom_vline(
      data = month_limits, aes(xintercept = last_yd), color = "gray33"
    ) +
    scale_x_continuous(
      expand = c(0,0), limits = c(1, 366), breaks = seq(1, 366, 14)
    ) +
    scale_y_continuous(
      limits = c(0, NA), expand = expansion(mult = c(0, 0.1))
    ) +
    theme_bw() +
    labs(x = xlab, y = ylab, title = title) +
    theme(legend.position = "none") 
  
  if(month_labels == T){
    p <- p +
      geom_text(
        data = month_limits, 
        aes(x = mid_yd, y = y_max, label = month_lab),
        hjust = 0.5, vjust = -1
      ) 
  }
  
  if(is_color(as.character(ensym(color)))) {

    # Use a single color for the line
    p <- p +
      geom_step(
        color = color, linewidth = linewidth, alpha = alpha, linetype = linetype
      )

  } else {
    # Use the color aesthetic with a palette
    p <- p +
      geom_step(
        aes(y = {{counts}}, color = {{color}}, x = yd),
        linewidth = linewidth, alpha = alpha, linetype = linetype
      )

    if(!is.null(pal)){ p <- p + scale_color_manual(values = pal) }

  }

  return(p)
  
}

# FUNCTION 5: plot_steps_timeline -------------------------------------------

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
  
  # sumarize the number of steps per day, including days with 0 steps
  # needed to be done explicitly, because otherwise geom_steps just plots the 
  # last available value, until it changes
  count_df <- df |> 
    summarize(step_count = n(), .by = yd) |> 
    bind_rows(tibble(yd = 1:366, step_count = 0)) |> 
    summarise(step_count = sum(step_count), .by = yd)
  
  rm(df)

  id_max <- max(df_bursts$id)
  id_buff <- round(id_max/5)
  
  # generate dataframe for plotting edges of each month
  month_limits <- get_month_limits() |> 
    mutate(
      id_min = min(df_bursts$id), 
      id_max = max(df_bursts$id)
    )
  
  # find the first track within each year to give limits to years
  year_limits <- df_bursts |> 
    summarize(
      n_tracks = length(unique(track_file)), 
      id_min = min(id), 
      .by = y
    ) |>
    mutate(
      # using this instead of max(id) to avoid white gaps in between years
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
      data = month_limits, aes(x = mid_yd, y = -id_buff/2, label = month_lab),
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
    geom_step(data = count_df, aes(x = yd, y = step_count)) +
    scale_x_continuous(
      expand = c(0,0), limits = c(1, 386), breaks = seq(1, 366, 14)
    ) +
    scale_y_continuous(expand = c(0, NA)) +
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

# FUNCTION 6: expand_bbox ---------------------------------------------------

# Function to expand bbox by a given degree
expand_bbox <- function(bbox, deg) {
  bbox["xmin"] <- bbox["xmin"] - deg
  bbox["ymin"] <- bbox["ymin"] - deg
  bbox["xmax"] <- bbox["xmax"] + deg
  bbox["ymax"] <- bbox["ymax"] + deg
  return(bbox)
}

# FUNCTION 7: plot_on_world_map ---------------------------------------------

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


# FUNCTION 8: plot_bars -----------------------------------------------------

plot_bars <- function(
    df, x, y, fill, pal, xlab = NULL, ylab = NULL, title = NULL
){
  
  df |> 
    ggplot() +
    geom_bar(
      aes(x = {{x}}, y = {{y}}, fill = {{fill}}), 
      stat = "identity", 
      show.legend = F, 
      color = "black"
    ) + 
    geom_text(
      aes(x = {{x}}, y = {{y}}, label = {{x}}),
      hjust = -1
    ) +
    scale_fill_manual(values = pal) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.3))) +
    labs(x = xlab, y = ylab, title = title) +
    theme_minimal() +
    theme(
      axis.line.y = element_line(color = "black"), 
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank()
    )
  
}



# FUNCTION 9: plot_tile_timeline ------------------------------------------

plot_tile_timeline <- function(
    df, y, fill, month_limits = NULL, 
    cols = c("#A2DA3CFF", "#EADD17FF", "#8B4F82FF"), 
    midp = 0.5, 
    ylab = NULL, 
    flab = NULL, 
    title = NULL
    ){
  
  if(is.null(month_limits)){ month_limits <- get_month_limits() }
  if(is.null(ylab)){ ylab <- ensym(y) }
  if(is.null(flab)){ flab <- ensym(fill) }
  
  y_lim <- df |> pull({{y}}) |> unique()
  
  df |> 
    ggplot() +
    geom_tile(aes(y = {{y}}, x = yd, fill = {{fill}}), color = "black") +
    geom_vline(
      data = month_limits, aes(xintercept = last_yd), color = "gray33"
    ) +
    scale_x_continuous(breaks = seq(1, 366, 14), expand = c(0,0)) +
    scale_y_continuous(breaks = y_lim, expand = c(0,0), labels = y_lim) +
    scale_fill_gradient2(
      midpoint = midp, high = cols[3], mid = cols[2], low = cols[1], 
    ) +
    theme_bw() +
    labs(y = ylab, x = "day of the year", fill = flab, title = title) +
    theme(
      axis.ticks = element_blank(),
      panel.spacing = unit(0, "mm"),
      legend.position = "bottom", 
      legend.title = element_text(vjust = 0.75)
    )
}
