# FUNCTION: get_speed_limit ---------------------------------------------

get_speed_limit <- function(
    species, sex = NULL, summary_func = mean, wind_speed = 0, 
    family = NULL, order = NULL,
    birdspeed_file = here::here("Published_data", "00_bird_wing_data_speed_mean.csv")
    ) {
  
  s <- ifelse(is.null(sex), c(NA, ""), sex)
  
  # Load bird_speeds data
  bird_speeds <- fread(birdspeed_file)[
    , speed := start_min_speed
    ][ , genus := tstrsplit(scientific_name, " ", fixed = TRUE)[[1]]
    ][ , .(scientific_name, sex, genus, family, order, speed) ]
  
  # Check if species exists in bird_speeds
  if (species %in% bird_speeds$scientific_name) {
    
    speed_sp <- bird_speeds[scientific_name == species]
    speed_sp <- speed_sp[if(s %in% sex) sex %in% s else sex %in% c(NA, "")]
    
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
  bird_speeds <- bird_speeds[sex %in% c(NA, "")]
  
  # helper function to match and compute speed for different phylogenetic levels
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
    cols = c("#A7DBD8FF", "#F38630FF"), 
    q_seq = seq(0.5, 1, 0.01), 
    min_step = 0.001
) {
  
  # Combine quantile calculation and closest index lookup
  q <- quantile(df$speed, q_seq, na.rm = TRUE)
  n <- which.min(abs(q - speed_limit))  # Find the closest quantile value
  q_big <- q_seq[n]
  q_value <- q[n]
  
  # Refine quantiles if the quantile is below 1
  if (q_big < 1) {
    fq <- quantile(df$speed, seq(q_big, q_big + 0.01, min_step), na.rm = TRUE)
    n <-  which.min(abs(fq - speed_limit))
    q_value <- fq[n]
  }
  
  # Create a data.table for speed limits to be plotted
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


get_speeds <- function(track, lonlat = T, units = "secs"){
  
  n_ <- nrow(track)
  
  speed <- data.table(
    step_lengths = step_lengths(track, lonlat = lonlat, append_last = F), 
    dt_ = difftime(track$t_[-1], track$t_[-n_], units = "secs")
  )[, speed := step_lengths/as.numeric(dt_, unit = "secs")][, speed]
  
  return(c(speed, NA))
  
}


# FUNCTION: filter_speed_limit ------------------------------------------------

# calculate the maximum speed from the tracking data and 
# keep filtering out the points that produce max speed higher than
# the speed limit

filter_speed_limit <- function(
    track, speed_limit = NULL, lonlat = T, units = "secs"){
  
  if(is.null(speed_limit)) {
    stop("Speed limit is NULL. Please provide a speed limit.")
  }
  
  speeds <- get_speeds(track, lonlat = lonlat, units = units)
  to_exclude <- which(speeds > speed_limit)
  
  while(length(to_exclude) > 0) {
    
    track <- track |> 
      mutate(row_id = 1:nrow(track)) |> 
      filter(!row_id %in% to_exclude)
    
    speeds <- get_speeds(track, lonlat = lonlat, units = units)
    
    to_exclude <- which(speeds > speed_limit)
    
  }
  
  track <- track |> select(-any_of("row_id"))
  
  return(track)
}
