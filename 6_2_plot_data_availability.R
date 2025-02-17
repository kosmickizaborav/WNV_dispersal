#' ---
#' title: "Plotting steps in time and space"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#'  **SECTION 1 - FUNCTION: get_palette**
#'  helper function to getting and standardizing the colors across years
#'  **SECTION 2 - FUNCTION: plot_steps_timeline**
#'  helper function to get limits of the months for plotting
#'  **SECTION 2 - FUNCTION: plot_steps_timeline**
#'  plots the timeline of the bursts that were used to calculate the steps. 
#'  in this way we see the distribution of the daya trhough time and seasons.
#'  under the timeline, is ploted total number of steps available per day of 
#'  a year. 
#'  **SECTION 3 - FUNCTION: expand_bbox**
#'  helper function for expanding the bbox of a map
#'  **SECTION 4 - FUNCTION: plot_on_world_map**
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


# FUNCTION: get_palette  --------------------------------------------------

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

# FUNCTION: get_month_limits -----------------------------------------------

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

# FUNCTION: plot_steps_timeline -------------------------------------------

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
    geom_step(data = count_df, aes(y = step_count, x = yd), ) +
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

# FUNCTION: expand_bbox ---------------------------------------------------

# Function to expand bbox by a given degree
expand_bbox <- function(bbox, deg) {
  bbox["xmin"] <- bbox["xmin"] - deg
  bbox["ymin"] <- bbox["ymin"] - deg
  bbox["xmax"] <- bbox["xmax"] + deg
  bbox["ymax"] <- bbox["ymax"] + deg
  return(bbox)
}

# FUNCTION: plot_on_world_map ---------------------------------------------

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


# FUNCTION: plot_bars -----------------------------------------------------

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

# 1 - Plot map overview for every species --------------------------------------

EU <- c(T, F)
tit <- "spatial and temporal distribution of tracking data used to obtain night steps"
cap <- "color of lines in timeline and points in map indicate year, see timeline for years with the most data"
  
target_sp |> 
  map(~{
    
    sp <- .x
    
    sp_dir <- here("Data",  "Studies", str_replace(sp, " ", "_"))
    files <- here(sp_dir, "6_distances") |> 
      list.files(pattern = "2_all_tracks_night_steps_")
    
    files |> 
      map(~{
        
        fin <- .x
        fout <- str_c("6_2_plot_", str_remove(fin, ".rds"), ".png")

        tit_full <- str_c(sp, "\n", tit)
        cap_full <- str_c("file: ", fin, "\n", cap)
        
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
        
        EU |> 
          map(~{
            
            eu <- .x
            
            if(eu == T){
              
              night_steps <- night_steps |> 
                filter(within_eubb == T)
              
              fout <- str_replace(fout, ".png", "_EU.png")
              tit_full <- paste(
                sp,  
                "- within the Europe bbox [34°N-81°N, 29°W-69°E]", "\n", 
                tit
              )
              
            } 
            
            # palette that will indicate different years
            pal <- get_palette(night_steps, y = y)
            
            # plots
            wmap <- plot_on_world_map(night_steps, pal = pal)
            tl <- plot_steps_timeline(night_steps, pal = pal)
            
            pout <- wmap/tl + 
              plot_annotation(
                title = tit_full, 
                caption = cap_full,
                theme = theme(
                  plot.title = element_text(face = "bold", hjust = 0.5)
                )
              ) +
              plot_layout(heights = c(1.5, 1))
            
            # save the plots
            ggsave(
              here(sp_dir, "Graphs", fout), 
              pout, 
              width = 35, height = 25, units = "cm"
            )
            
            print(fout)
      
          }) # close eu map

      }) # close file map
    
    print(paste(sp, "DONE!"))
      
    
  })


# 2 - Plot different day defintions ---------------------------------------

month_limits <- get_month_limits()

pal <- c("gray33", "#E9E4A6FF", "#E9A17CFF", "#1BB6AFFF")
names(pal) <- c(
  "all deployments", "[dawn, dusk]", 
  "[nauticalDawn, nauticalDusk]", "[nightEnd, night]"
)

target_sp |> 
  map(~{
    
    sp <- .x
    sp_dir <- here("Data",  "Studies", str_replace(sp, " ", "_"))
    files <- here(sp_dir, "6_distances") |> 
      list.files(pattern = "2_all_tracks_night_steps_")
    
    full_df <- files |> 
      map(~{
        
        day_lim = str_remove_all(.x, "2_all_tracks_night_steps_|\\.rds")
        
        here(sp_dir, "6_distances", .x) |> 
          read_rds() |> 
          mutate(
            day_cycle = as.Date(day_cycle, format = "%d%m%Y"), 
            yd = yday(day_cycle),
            day_lim = day_lim
          ) |> 
          select(track_file, yd, day_lim, night_steps_available, within_eubb)
        
      }) |> 
      list_rbind() |> 
      # modify day limits for the plot
      mutate(
        day_lim = if_else(
          str_detect(day_lim, "_"),
          str_c("[", str_replace(day_lim, "_", ", "), "]"), 
          day_lim
        ) |>  factor(levels = names(pal))
      ) 
    
    fout <- str_c("6_2_", str_replace(sp, " ", "_"), "_steps_availability.png")
    title_out <- str_c(sp, " - data availability")
    subtitle_out <- "night steps obtained using different day definitions"
    
    EU |> 
      map(~{
        
        eu <- .x 
        
        if(eu == T){
          
          full_df <- full_df |> 
            filter(within_eubb == T)
          
          fout <- str_replace(fout, ".png", "_EU.png")
          title_out <- paste(title_out, "- European bbox")
          
        }
        
        n_total <- n_distinct(full_df$track_file)
        
        full_df <- full_df |> 
          filter(night_steps_available == T)
        
        # getting counts per dataset 
        counts_df <- full_df |>
          summarize(
            n_deploys = n_distinct(track_file),
            n_steps = n(),
            n_days = n_distinct(yd, na.rm = T), 
            day_lim = unique(day_lim),
            .by = c(night_steps_available, day_lim)
          ) |> 
          bind_rows(tibble(day_lim = "all deployments", n_deploys = n_total))
         
        pdc <- counts_df |> 
          plot_bars(
            x = n_deploys, y = day_lim, fill = day_lim, pal = pal, 
            ylab = "day definition", 
            title = "Deployments used to obtain night steps"
          )
        
        psc <- counts_df |>
          mutate(
            day_lim = if_else(day_lim == "all deployments", "", day_lim)
          ) |> 
          plot_bars(
            x = n_steps, y = day_lim, fill = day_lim, pal = pal, 
            title = "Number of steps obtained"
          ) + 
          theme(axis.text.y = element_blank())
        
       
        # getting counts of steps per day
        step_times <- full_df |> 
          summarize(n_steps = n(), .by = c(yd, day_lim)) 
        
        month_limits <- month_limits |> 
          mutate(y_max = max(step_times$n_steps))
        
        
        pst <- step_times |> 
          ggplot() + 
          geom_step(
            aes(y = n_steps, color = day_lim, x = yd), 
            size = 1.5, alpha = 0.8, linetype = "twodash"
          ) +
          geom_segment(
            data = month_limits, 
            aes(x = last_yd, xend = last_yd, y = 0, yend = y_max), 
            color = "gray33"
          ) +
          geom_text(
            data = month_limits, aes(x = mid_yd, y = y_max, label = month_lab),
            hjust = 0.5, vjust = -1
          ) +
          scale_x_continuous(
            expand = c(0,0), limits = c(1, 366), breaks = seq(1, 366, 14)
          ) +
          scale_y_continuous(
            limits = c(0, NA), expand = expansion(mult = c(0, 0.1))
          ) +
          scale_color_manual(values = pal) +
          theme_bw() +
          labs(
            x = "day of the year", 
            y = "steps per day", 
            title = "Number of steps per day of the year"
          ) +
          theme(legend.position = "none") 
        
        # output plot 
        pout <-  (pdc + psc) / pst +
          plot_annotation(
            title = title_out, 
            subtitle = subtitle_out, 
            theme = theme(
              plot.title = element_text(face = "bold", hjust = 0.5), 
              plot.subtitle = element_text(hjust = 0.5)
            )
          ) +
          plot_layout(heights = c(1, 1))
        
        # save the plots
        ggsave(
          here("Data", "Graphs", fout), 
          pout, 
          width = 35, height = 20, units = "cm"
        )
        
        
      }) # close EU map
      
     print(paste(sp, "DONE!"))
     
  }) # close species map


# trial to make unifying timeline plot------------------------------------

# plot_steps_yearday <- function(
#     df, y = NULL, x = yd, color = NULL,
#     month_limits = NULL,
# ){
#   
#   # get month limits if not provided
#   if(is.null(month_limits)){ month_limits <- get_month_limits() }
#   
#   month_limits <- month_limits |> 
#     mutate(y_max = max(df$n_steps))
#   
#   df |> 
#     ggplot() + 
#     geom_step(aes(y = {{y}}, x = {{x}}), color = {{color}}) +
#     geom_vline(
#       data = month_limits, aes(xintercept = last_yd), color = "gray33"
#     ) +
#     
#     scale_x_continuous(
#       expand = c(0,0), limits = c(1, 386), breaks = seq(1, 366, 14)
#     ) +
#     scale_y_continuous(
#       expand = c(0, 0), limits = c(0, max(count_df$step_count)+1)
#     ) +
#     theme_bw() +
#     labs(x = "day of the year", y = "steps \nper day") +
#     theme(
#       plot.margin = margin(t = 0, r = 0, b = 1, l = 1, unit = "mm"), 
#       axis.ticks = element_blank(),
#       panel.grid.minor = element_blank(),
#       panel.grid.major = element_blank(),
#       panel.spacing = unit(0, "cm"), 
#       panel.border = element_blank(),
#       axis.line.y = element_line(color = "gray33")
#     )
#   
#   
#   
# }

