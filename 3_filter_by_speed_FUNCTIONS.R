#' ---
#' title: "function used to filter by speed"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' **FUNCTION 1 - get_speed_limit**
#' using the file "00_bird_wing_data_speed_mean.csv", that is generated within
#' a script 00_prepare_external_files.R, find the speed limit for the species
#' and its sex. in case it is not available, use the mean value for the family
#' or order. 
#' 
#' **FUNCTION 2 -  plot_speed_turns**
#' plot histograms of speed and turning angle, and label in which speed quantile
#' we can find the speed limit applied.
#' 
#' **FUNCTION 3 - get_speeds**
#' function that calculates the speeds from the track data in m/s in the format
#' of datatable. basically adapted the function for speed calculation that 
#' is found in amt package
#'     
#' **FUNCTION 4 - filter_speed_limit**
#' function that calculates the speeds in the tracking data, removes a point if
#' it generates a speed above the applied speed limit, and continues to do it
#' until all the speeds are below the speed limit.


# FUNCTION: get_speed_limit ---------------------------------------------

get_speed_limit <- function(
    species, summary_func = mean, wind_speed = 0, 
    family = NULL, order = NULL,
    birdspeed_file = here::here(
      "Published_data", "00_bird_wing_data_speed_mean.csv")
    # sex = NULL
    ) {
  
  #s <- ifelse(is.null(sex), c(NA, ""), sex)
  
  # Load bird_speeds data
  bird_speeds <- fread(birdspeed_file)[
    , genus := tstrsplit(scientific_name, " ", fixed = TRUE)[[1]]
    ][ , .(scientific_name, genus, family, order, speed) ] # sex
  
  # Check if species exists in bird_speeds
  if (species %in% bird_speeds$scientific_name) {
    
    speed_sp <- bird_speeds[scientific_name == species]
    #speed_sp <- speed_sp[if(s %in% sex) sex %in% s else sex %in% c(NA, "")]
    
    return(speed_sp[, speed] + wind_speed)
   
  } else {
    
    species_phylo <- data.table(
      genus = tstrsplit(species, " ", fixed = TRUE)[[1]], 
      family = if(!is.null(family)) family else NULL, 
      order = if(!is.null(order)) order else NULL
    )
    
  }
  
  # if there is no species that matches take average for the genus,
  # excluding sex differences
  # bird_speeds <- bird_speeds[sex %in% c(NA, "")]
  
  # helper function to match and compute speed for different phylogeny levels
  match_phylo_and_compute <- function(level) {
    
    if(species_phylo[[level]] %in% bird_speeds[[level]]) {
      
      speed <- ceiling(
        bird_speeds[get(level) == species_phylo[[level]], summary_func(speed)]
      )
      
      return(data.table(phylo_level = level, speed = speed + wind_speed))
    }
    
    return(NULL)
  }
  
  # calculate the speed for genus, family and order levels in sequence
  speed_phylo <- rbindlist(
    lapply(names(species_phylo), match_phylo_and_compute), 
    fill = T
  )
  
  # return the first valid match e.g. genus if it exists, then family, and then 
  # order
  speed <- if(nrow(speed_phylo) > 0) {
    speed <- speed_phylo[
      order(match(phylo_level, c("genus", "family", "order")))
      ][1, speed]
  } else {
    NA
  }
  
  # Return NA if no match found
  return(speed + wind_speed)
}

# FUNCTION: plot_speed_turns -----------------------------------------------

plot_speed_turns <- function(
    df, speed_limit,
    speed_col = "speed", turn_col = "turn", 
    cols = c("#088F8F", "#6e1354"), 
    q_seq = seq(0.5, 1, 0.01), 
    min_step = 0.001
) {
  
  # check within which quantile the speed limit falls
  q <- quantile(df$speed, q_seq, na.rm = TRUE)
  n <- which.min(abs(q - speed_limit))  # Find the closest quantile value
  q_big <- q_seq[n]
  q_value <- q[n]
  
  # refine quantiles if the quantile is below 1
  if (q_big < 1) {
    fq <- quantile(df$speed, seq(q_big, q_big + 0.01, min_step), na.rm = TRUE)
    n <-  which.min(abs(fq - speed_limit))
    q_value <- fq[n]
  }
  
  # create a data.table for speed limits to be plotted
  sp_limits_graph <- data.table(
    speed = c(unname(q_value), speed_limit),
    name = c(paste(names(q_value), "quantile"), "applied speed limit")
  )[, text := paste(name, ": ", round(speed, 1), " m/s")]
  
  # Plot 1: Speed distribution with quantiles
  ps <- ggplot(df[!is.na(speed)]) +
    geom_histogram(aes(speed), fill = "grey55", color = "black", bins = 50) +
    geom_vline(
      data = sp_limits_graph, 
      aes(xintercept = speed, color = text), 
      linewidth = 1.2
    ) +
    labs(x = "speed [m/s]", color = "") +
    scale_color_manual(values = cols) +
    theme_bw() +
    theme(legend.position = "bottom")
  
  # Plot 2: Turning angle distribution
  pta <- ggplot(df[!is.na(ta_)]) +
    geom_histogram(aes(ta_), fill = "grey55", color = "black", bins = 50) + 
    labs(x = "turning angle [rad]") +
    theme_bw()
  library(patchwork)
  # Combine both plots with shared legend and layout
  pout <- (ps + pta) + 
    plot_layout(guides = "collect") & theme(
      legend.position = 'top', 
      plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 12)
    )
  
  return(pout)
}


# FUNCTION: get_speed -----------------------------------------------------


get_speeds <- function(track, t_col = t_col, units = "secs"){
  
  n <- nrow(track)
  
  # checked that they give the same results as step_lengths from amt
  sls <- sf::st_distance(
    track$geometry[-n], track$geometry[-1], by_element = TRUE)
  
  ttime <- track[, get(t_col)]
  dtime <- difftime(ttime[-1], ttime[-n], units = "secs")
  
  speed <- as.numeric(sls) / as.numeric(dtime, unit = units)
  
  return(c(NA, speed))
  
}


# FUNCTION: filter_speed_limit ------------------------------------------------

# calculate the maximum speed from the tracking data and 
# keep filtering out the points that produce max speed higher than
# the speed limit

filter_speed_limit <- function(
    track, coords = c("x", "y"), t_col = "timestamp", 
    speed_limit = NULL, crs = NULL, units = "secs"){
  
  if(is.null(speed_limit)) {
    stop("Speed limit is NULL. Please provide a speed limit.")
  }
  
  track <- track[
    , geometry := sf::st_as_sf(.SD, coords = coords, crs = crs), 
    .SDcols = coords]
  
  repeat{
    # Calculate speeds (custom function, returns vector)
    speeds <- get_speeds(track, t_col = t_col, units = units)
    
    # Find indexes to exclude (either leading or trailing point per segment)
    to_exclude <- which(speeds > speed_limit)
    
    if (length(to_exclude) == 0) break
   
    track <- track[!seq_len(.N) %in% to_exclude]
    
  }
  
  track <- track[, geometry := NULL]
  
  return(track)
}



