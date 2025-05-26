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

# library(rlang)
library(paletteer)
library(ggplot2)
library(patchwork)
# library(rnaturalearth)
# library(sf)
# library(patchwork)


# FUNCTION 1: get_palette  --------------------------------------------------

# get palette colors for years
get_palette <- function(years){
  
  years <- sort(unique(years))
  
  pall <- paletteer::paletteer_c("grDevices::Spectral", n = length(years))  
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
  check <- grepl(paste(c(hex_regex, rgb_regex), collapse = "|"), cstr) ||
    cstr %in% named_colors

  return(check)
}


# FUNCTION 3: get_month_limits -----------------------------------------------

get_month_limits <- function(year = 2020) {
  
  # Create a data.table with months of the year
  month_limits <- data.table(month = 1:12)
  
  # Add columns for the first and last day of each month
  month_limits[, `:=`(
    first_date = as.Date(sprintf("%d-%02d-01", year, month)),  # Start of the month
    last_date = lubridate::ceiling_date(
      as.Date(sprintf("%d-%02d-01", year, month)), "month") - 1
  )]
  
  # Calculate the yearday for the start and end of each month
  month_limits[, `:=`(
    first_yd = as.POSIXlt(first_date)$yday + 1, 
    last_yd = as.POSIXlt(last_date)$yday + 1, 
    month = lubridate::month(month, label = T))][
  , mid_yd := first_yd + (last_yd - first_yd)/2]
  
  return(month_limits[, .(month, first_yd, mid_yd, last_yd)])
}


# FUNCTION 4: plot_steps_count ----------------------------------------------

library(ggplot2)

plot_steps_count <- function(
    steps, count, color = "#803342FF", 
    linetype = "solid", 
    linewidth = 1.5, 
    alpha = 0.8,
    title = NULL, 
    ylab = "steps per day",
    linelab = NULL, 
    collab = NULL,
    pal = NULL, 
    month_labels = TRUE, 
    legend_position = "none"
){
  
  month_limits <- get_month_limits()
  # Top plot (month labels)
  mb <- ggplot(month_limits) +
    geom_text(aes(x = mid_yd, y = 1, label = month), hjust = 0.5, vjust = 0.5) +
    geom_vline(aes(xintercept = last_yd), color = "gray33") +
    scale_x_continuous(expand = c(0, 0), limits = c(1, 366)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 2)) +
    theme_void() +  # Remove everything
    theme(
      plot.margin = margin(0, 0, 0, 0, "mm"),
      panel.spacing = unit(0, "mm"), 
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
    ) +
    labs(title = title)
  
  # check which variables are provided
  # for checking if linetype is a variable or type of the line
  line_types <- c(
    "blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash", 
    as.character(1:6)
  )
  color_is_fixed <- is_color(as.character(ensym(color)))
  line_is_fixed <- as.character(ensym(linetype)) %in% line_types
  
  if(is.null(ylab)){ ylab <- ensym(counts) }
  if(is.null(collab)){
    collab <- ifelse(color_is_fixed == F, ensym(color), ensym(counts))
  }
  
  p <- steps |> 
    ggplot(aes(y = {{count}}, x = yd)) + 
    geom_vline(
      data = month_limits, aes(xintercept = last_yd), color = "gray33"
    ) +
    scale_x_continuous(expand = c(0, 0), limits = c(1, 366), breaks = seq(1, 366, 30)) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
    theme_bw() +
    labs(
      x = "day of year", y = ylab, title = title, 
      color = collab, linetype = linelab) +
    theme(legend.position = legend_position) 
  
  if(color_is_fixed == T & line_is_fixed == T){
    
    p <- p +
      geom_step(
        color = color, linetype = linetype,
        linewidth = linewidth, alpha = alpha, 
      )
  }
  
  if(color_is_fixed == T & line_is_fixed == F){
    
    p <- p +
      geom_step(
        aes(linetype = {{linetype}}), color = color, 
        linewidth = linewidth, alpha = alpha
      )
  }
  
  if(color_is_fixed == F & line_is_fixed == T){
    
    p <- p +
      geom_step(
        aes(color = {{color}}), linetype = linetype,
        linewidth = linewidth, alpha = alpha
      )
  }
  
  if(color_is_fixed == F & line_is_fixed == F){
  
    p <- p +
      geom_step(
        aes(color = {{color}}, linetype = {{linetype}}), 
        linewidth = linewidth, alpha = alpha
      )
    
  }
  
  if(!is.null(pal)){ p <- p + scale_color_manual(values = pal) }
    
  pout <- mb / p + patchwork::plot_layout(heights =  c(0.1, 1)) 
    
  return(pout)
  
}


# plot_steps_count <- function(
#     steps, count, color = "#803342FF", 
#     linetype = "solid", 
#     linewidth = 1.5, 
#     alpha = 0.8,
#     title = NULL, 
#     ylab = "steps per day",
#     leglab = NULL,
#     pal = NULL, 
#     month_labels = TRUE, 
#     legend_position = "none"
# ){
#   
#   line_types <- c(
#     "blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash", 1:6
#   )
#   
#   month_limits <- get_month_limits()
#   # Top plot (month labels)
#   mb <- ggplot(month_limits) +
#     geom_text(aes(x = mid_yd, y = 1, label = month), hjust = 0.5, vjust = 0.5) +
#     geom_vline(aes(xintercept = last_yd), color = "gray33") +
#     scale_x_continuous(expand = c(0, 0), limits = c(1, 366)) +
#     scale_y_continuous(expand = c(0, 0), limits = c(0, 2)) +
#     theme_void() +  # Remove everything
#     theme(
#       plot.margin = margin(0, 0, 0, 0, "mm"),
#       panel.spacing = unit(0, "mm"), 
#       panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
#     ) +
#     labs(title = title)
#   
#   count_quo <- enquo(count)
#   color_quo <- enquo(color)
#   linetype_quo <- enquo(linetype)
#   
#   # Is color a fixed value or a variable?
#   color_is_fixed <- is.character(rlang::quo_get_expr(color_quo)) && 
#     is_color(as.character(rlang::quo_get_expr(color_quo)))
#   
#   linetype_is_fixed <- rlang::quo_get_expr(linetype_quo) %in% line_types
#   
#   # Build aes mapping
#   mapping <- aes(x = yd, y = !!count_quo)
#   if (!color_is_fixed) {
#     mapping <- aes(x = yd, y = !!count_quo, color = !!color_quo) }
#   if (!linetype_is_fixed && !color_is_fixed) {
#     mapping <- aes(
#       x = yd, y = !!count_quo, color = !!color_quo, linetype = !!linetype_quo) }
#   if(!linetype_is_fixed && color_is_fixed){
#     mapping <- aes(x = yd, y = !!count_quo, linetype = !!linetype_quo)
#   } 
#   
#   # Build geom_step args
#   geom_args <- list(linewidth = linewidth, alpha = alpha)
#   if (color_is_fixed){
#     geom_args$color <- as.character(rlang::quo_get_expr(color_quo))}
#   if (linetype_is_fixed) {
#     geom_args$linetype <- as.character(rlang::quo_get_expr(linetype_quo))}
#   
#   p <- ggplot(steps, mapping = mapping) +
#     do.call(geom_step, geom_args) +
#     geom_vline(data = month_limits, aes(xintercept = last_yd), color = "gray33") +
#     scale_x_continuous(expand = c(0, 0), limits = c(1, 366), breaks = seq(1, 366, 30)) +
#     scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
#     theme_bw() +
#     labs(x = "day of a year", y = ylab, color = leglab) +
#     theme(
#       legend.position = legend_position,
#       axis.ticks = element_blank(),
#       plot.margin = margin(t = 0, unit = "mm"),
#       panel.spacing = unit(0, "mm")
#     )
#   
#   if (!color_is_fixed && !is.null(pal)) {
#     p <- p + scale_color_manual(values = pal)
#   }
#   
#   pout <- mb / p + patchwork::plot_layout(heights =  c(0.1, 1)) 
#   
#   return(pout)
# }




# FUNCTION 5: plot_steps_timeline -------------------------------------------

plot_steps_timeline <- function(steps = NULL) {
  
  df_timeline <- steps[, .(day_cycle = day_cycle_1, file)][
    , `:=`(
      y = year(day_cycle),
      yd = as.POSIXlt(as.Date(day_cycle))$yday  + 1)][
        , day_burst := cumsum(c(1, diff(as.numeric(day_cycle)) > 1)), by = file]
  
  df_bursts <- df_timeline[, .(
    brs_start = min(yd), 
    brs_end = max(yd)
  ), by = .(y, file, day_burst)][, `:=`(
    id = frank(paste0(y, "-", file), ties.method = "dense"),
    year = as.factor(y)  # Convert year to factor
  )]
  
  count_df <- df_timeline[, .(step_count = .N), by = yd]
  count_df <- rbindlist(list(
    count_df,
    data.table(yd = 1:366, step_count = 0)  # Add days with 0 steps
  ))[, .(step_count = sum(step_count)), by = yd]  # Aggregate by yd
  
  rm(df_timeline)
  
  id_min <- 0
  id_max <- max(df_bursts$id) + 1
  
  month_limits <- get_month_limits()
  
  year_limits <- df_bursts[, .(
    n_tracks = uniqueN(file),  
    id_min = min(id)
  ), by = year][order(id_min)][
    , `:=`(
      id_max = c(id_min[-1], id_max),  
      y_lab = ifelse(n_tracks > 0.1 * id_max, as.character(year), "")  # Filter sparse years
    )][
    , id_min := ifelse(id_min == 1, 0, id_min)][
    , id_mid := id_min + (id_max - id_min) / 2]
  
  pal <- get_palette(years = year_limits$year)
  
  ptl <- ggplot() +
    geom_hline(
      aes(yintercept = c(0, year_limits$id_max)), color = "gray33"
    ) +
    geom_vline(
      aes(xintercept = c(1, month_limits$last_yd, 396)), color = "gray33"
    ) +
    geom_segment(
      data = df_bursts,
      aes(
        x = brs_start, xend = brs_end, 
        y = id, yend = id,
        color = year
      ),
      linewidth = 2, 
      alpha = 0.6
    ) +
    geom_rect(
      data = year_limits,
      aes(
        xmin = 366, xmax = 396,
        ymin = id_min, ymax = id_max,
        fill = year
      ),
      color = "gray33",
      alpha = 0.6
    ) +
    geom_text(
      data = year_limits,
      aes(x = 381, y = id_mid, label = y_lab),
      color = "black",
      hjust = 0.5,
      vjust = 0.5
    ) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    scale_color_manual(values = pal) +
    scale_fill_manual(values = pal) +
    theme_void() +
    labs(
      y = "track bursts \n per deployment", 
      title = "Distribution of tracking data in time"
    ) +
    theme(
      axis.title.y = element_text(angle = 90, vjust = 0.5),
      plot.title = element_text(hjust = 0),
      plot.margin = margin(t = 1, r = 0, b = 0, l = 0, unit = "mm"), 
      panel.spacing = unit(0, "cm"), 
      legend.position = "none"
    ) 
  
  mb <- ggplot() + 
    geom_segment(aes(x = 1, xend = 366, y = 1, yend = 1), color = "gray33") +
    geom_segment(aes(x = 1, xend = 366, y = 10, yend = 10), color = "gray33") +
    geom_vline(
      aes(xintercept = c(1, month_limits$last_yd)), color = "gray33"
    ) +
    scale_x_continuous(expand = c(0,0), limits = c(1, 396)) +
    scale_y_continuous(expand = c(0, 0)) +
    geom_text(
      data = month_limits, aes(x = mid_yd, y = 5, label = month),
      hjust = 0.5, vjust = 0.5
    ) +
    theme_void()
    
  
  psc <- ggplot() + 
    geom_vline(
      aes(xintercept = month_limits$last_yd), color = "gray33"
    ) +
    geom_segment(aes(x = 1, xend = 366, y = 0, yend = 0), color = "gray33") +
    geom_step(data = count_df, aes(x = yd, y = step_count)) +
    scale_x_continuous(
      expand = c(0,0), limits = c(1, 396), breaks = seq(1, 366, 30)
    ) +
    scale_y_continuous(
      expand = c(0, NA), limits = c(0, max(count_df$step_count)+1)
    ) +
    theme_bw() +
    labs(x = "day of the year", y = "steps \n per day") +
    theme(
      plot.margin = margin(t = 0, r = 0, b = 1, l = 1, unit = "mm"), 
      axis.ticks = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.spacing = unit(0, "cm"), 
      panel.border = element_blank(),
      axis.line.y = element_line(color = "gray33")
    )
  
  # Dynamically adjust the relative heights of the plots
  relative_heights <- c(2, 0.3, 1)  # Adjust as needed
  
  pout <- ptl /mb/ psc + 
    plot_layout(heights = relative_heights)  # Use relative_heights here
  
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

plot_on_world_map <- function(steps, crs = sf::st_crs(4326), pal = NULL){
  
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") 
  
  locs <-  unique(
    rbindlist(list(
      steps[, .(x_ = x1_, y_ = y1_, t_ = t1_, day_cycle = day_cycle_1, file)], 
      steps[, .(x_ = x2_, y_ = y2_, t_ = t2_, day_cycle = day_cycle_2, file)])))[
    , y := as.factor(year(day_cycle))]
  
  locs <- sf::st_as_sf(locs, coords = c("x_", "y_"), crs = crs)
  
  ns_bbox <- sf::st_bbox(locs$geometry) |> expand_bbox(5)
  
  pal <- get_palette(years = locs$y)

  # main plot
  mm <- ggplot() + 
    geom_sf(data = world, fill = "white") + 
    geom_sf(data = locs, aes(color = y), alpha = 0.5, size = 1.5) +
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
  zoom_pol <- sf::st_polygon(
    list(
      matrix(
        c(
          ns_bbox$xmax, ns_bbox$ymax,  
          180, 90,  
          180, -90, 
          ns_bbox$xmax, ns_bbox$ymin, 
          ns_bbox$xmax, ns_bbox$ymax 
        ),
        ncol = 2, 
        byrow = TRUE
      )
    ))
  
  zoom_pol <- sf::st_sf(geometry = sf::st_sfc(zoom_pol), crs = crs)
  
  ns_bbox <- sf::st_as_sfc(ns_bbox)
  
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
      title = "Distribution of tracking data in space") +
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
    df, x, y, fill, pal, alpha = 0.6, xlab = NULL, ylab = NULL, title = NULL
){
  
  df |> 
    ggplot() +
    geom_bar(
      aes(x = {{x}}, y = {{y}}, fill = {{fill}}), 
      stat = "identity", show.legend = F, alpha = alpha, 
      color = "black"
    ) + 
    geom_text(aes(x = {{x}}, y = {{y}}, label = {{x}}), hjust = -1) +
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
    dt, y, fill, yd = yd,
    pal = c("#A2DA3CFF", "#EADD17FF", "#8B4F82FF"), 
    pal_type = "gradient",
    midp = 0.5, 
    ylab = NULL, 
    flab = NULL, 
    title = NULL
    ){
  
  if(is.null(ylab)){ ylab <- ensym(y) }
  if(is.null(flab)){ flab <- ensym(fill) }
  
  month_limits <- get_month_limits()
  
  pt <- ggplot(dt) +
    geom_tile(aes(y = {{y}}, x = {{yd}}, fill = {{fill}}), color = "gray33") + 
    geom_vline(
      data = month_limits, aes(xintercept = last_yd), color = "gray33"
    ) +
    scale_x_continuous(
      limits = c(0, 367), breaks = seq(1, 366, 30), expand = c(0,0)
    ) +
    scale_y_discrete(expand = c(0,0), breaks = unique(dt[,get(ensym(y))])) +
    theme_bw() +
    labs(y = ylab, x = "day of the year", fill = flab, title = title) +
    theme(
      axis.ticks = element_blank(),
      #plot.margin = margin(0, 0, 0, 0, "mm"),
      panel.spacing = unit(0, "mm"),
      legend.position = "bottom", 
      legend.title = element_text(vjust = 0.75)
    )
  
  if(pal_type == "gradient"){
    
    pt <- pt + scale_fill_gradient2(
      midpoint = midp, high = pal[3], mid = pal[2], low = pal[1])
  }
  
  if(pal_type == "manual"){
    
    pt <- pt + scale_fill_manual(values = pal)
  }
  
  return(pt)
  
}

