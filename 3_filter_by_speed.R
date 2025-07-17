#' ---
#' title: "filter tracks by speed"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' **SECTION 0 - Load packages and files**
#' 
#' **SECTION 1 - Plot speed and turning angles - unfiltered**
#' load all the tracks and plot the distribution of speed and turning angle for
#' all of the unfiltered tracks. and compare it with the theoretical speed limit.
#' save the information of the speed quantiles.
#' 
#' **SECTION 2 - Plot speed quantiles**
#' plot the quantiles of the speed distribution for each species that are above
#' and below the applied theoretical speed limit
#' 
#' **SECTION 3 - Filter track for speed**
#' using the theoretical speed limit for each species, filter the tracks
#' and save them.
#'     
#' **SECTION 4 - Get filter report**
#' load filtered and unfiltered data and record how many locations there are
#' before and after filtering.

# 0 - Load packages and files --------------------------------------------------

library(data.table)
library(amt)
library(ggplot2)
library(patchwork)
source("0_helper_functions.R")
source("3_filter_by_speed_FUNCTIONS.R")

# INPUT
file_deploy_clean <- "2_deployments_cleaned.csv"

# OUTPUT DIRECTORIES
data_dir <- here::here("Data")
study_dir <- file.path(data_dir, "Studies")
graphs_dir <- file.path(data_dir, "Graphs")

# get cleaned deployment list
cleaned_tracks <- fread(file.path(data_dir, file_deploy_clean))[excluded == F][
  , .(file, species)]

# data output directories
target_sp <- cleaned_tracks[, unique(species)]
create_dir(target_sp, new_dir = "3_filtered_speed")

# plot output directories
if(!dir.exists(file.path(graphs_dir, "3_filtered_speed"))){ 
  dir.create(file.path(graphs_dir, "3_filtered_speed"), recursive = T)
}

# 1 - Find speed limits ---------------------------------------------------

# get birdlife phylogeny and if there is no speed limit specified for the 
# species, infer one from the family or the order
speed_limits_dt <- add_birdlife_phylogeny(species_name = target_sp)
speed_limits_dt[
  , speed_limit := get_speed_limit(
    species = birdlife_name, family = family, order = order),
  by = birdlife_name]
# ostrich - source https://www.speedofanimals.com/animals/ostrich
# but similar values found in general googling 
# https://www.academia.edu/8538642/
# Wild_ostrich_Struthio_camelus_ecology_and_physiology#loswp-work-container
speed_limits_dt[birdlife_name == "Struthio camelus", speed_limit := 20] 

# 2 - Plot speed and turning angles ---------------------------------------

# OUTPUT FILE
file_speed_quantiles <- "3_target_sp_speed_quantiles.csv"

if(!file.exists(file.path(data_dir, file_speed_quantiles))){

  # for progress reporting
  dc <- nrow(speed_limits_dt)

  # get the speed and turns distribution for the species across all tracks
  speed_quant_df <- rbindlist(lapply(seq(dc), function(i){

    sp_name <- speed_limits_dt[i, birdlife_name]

    # read the track
    files <- cleaned_tracks[species == sp_name, file]
    ntrk <- length(files)

    df_speed <- rbindlist(lapply(seq_along(files), function(t){

      track <- fread(files[t])
      
      track <- track |>
        make_track(
          x, y, timestamp,
          crs = sf::st_crs(4326),
          all_cols = TRUE
        )

      # calculate speed and turn - did it like this 
      # for the turning angle consistency
      dfs <- as.data.table(steps(track, lonlat = T))[, .(dt_, sl_, ta_)][
        ,  speed := sl_/as.numeric(dt_, unit = "secs")][, .(speed, ta_)]

      cat(sprintf("\n Species - %s: %d | %d of total %d | %d.",
         sp_name, t, ntrk, i, dc))

      return(dfs)

    }))

    # plot speed and turning angle
    p <- plot_speed_turns(
      df_speed,
      speed_limit = speed_limits_dt[i, speed_limit],
      speed_col = "speed", turn_col = "ta_"
    ) + plot_annotation(
      title =  paste(sp_name, "- speed and turning angles"),
      subtitle = paste(ntrk, "cleaned deployments unfiltered")
    )

    p_name <- paste0(gsub(" ", "_", sp_name),  "_speed_turn.png")

    # save the plots
    ggsave(
      filename = file.path(graphs_dir, "3_filtered_speed", p_name),
      plot = p, width = 16, height = 10, units = "cm",
    )

    # save the quantiles of speed distribution
    quant_df <- as.data.table(as.list(round(
        quantile(df_speed$speed, seq(0.5, 1, 0.05), na.rm = TRUE), 3)))
    
    quant_df <- cbind(speed_limits_dt[i,], quant_df)

    return(quant_df)

  }))

  fwrite(speed_quant_df, file.path(data_dir, file_speed_quantiles))

}
  

# 3 - Plot speed quantiles ----------------------------------------------------

# OUTPUT FILE
graph_speed_quantiles <- "3_speed_quantiles_vs_limit.png"

if(!file.exists(file.path(graphs_dir, graph_speed_quantiles))){
  
  pal <- c("#088F8F", "#6e1354")
  
  speed_quant_df <- fread(file.path(data_dir, file_speed_quantiles))
    
  df_plot <- melt(
    speed_quant_df, 
    id.vars = c("birdlife_name", "speed_limit", "order", "family"),  
    measure.vars = grep("%", names(speed_quant_df), value = T), 
    variable.name = "quantile",
    value.name = "speed"
  )[, limit_check := speed > speed_limit]
  
  
  p <- ggplot(df_plot) +
    geom_tile(
      aes(x = quantile, y = birdlife_name, fill = limit_check),
      color = "gray33", alpha = 0.8
    ) + 
    theme_minimal() + 
    scale_fill_manual(values = pal) +
    scale_y_discrete(limits = rev(unique(df_plot$birdlife_name))) +
    theme(
      legend.position = "top", 
      plot.title = element_text(hjust = 0.5), 
      plot.background = element_rect(fill = "white", color = NA),  
      panel.background = element_rect(fill = "white", color = NA)  
    ) +
    labs(
      title = "Distribution of speed vs. speed limit applied",
      y = "species",
      fill = "speed > speed limit"
    )
  
  ggsave(file.path(graphs_dir, graph_speed_quantiles), height = 30, width = 10)
  
  df_plot <- split(df_plot, by = "order")
  
  lapply(df_plot, function(df){
    
    ord <- unique(df$order)
    n <- uniqueN(df$birdlife_name)
    
    p <- ggplot(df) +
      geom_tile(
        aes(x = quantile, y = birdlife_name, fill = limit_check),
        color = "gray33", alpha = 0.8
      ) + 
      theme_minimal() + 
      scale_y_discrete(limits = rev(unique(df$birdlife_name))) +
      scale_fill_manual(values = pal) +
      theme(
        legend.position = "top", 
        plot.title = element_text(hjust = 0.5), 
        plot.background = element_rect(fill = "white", color = NA),  
        panel.background = element_rect(fill = "white", color = NA)  
      ) +
      labs(
        title = paste0(ord, ": distribution of speed vs. speed limit applied"),
        y = "species",
        fill = "speed > speed limit"
      ) 
    
    pout <- gsub(".png", paste("_", ord, ".png"), graph_speed_quantiles)
    
    ggsave(file.path(graphs_dir, pout), height = 10, width = 8)
    
    
  })

}


# 4 - Filter track for speed ---------------------------------------------------

# add the spedd limit to the track info
cleaned_tracks <- merge(
  cleaned_tracks, speed_limits_dt, 
  by.x = "species", by.y = "birdlife_name",
  all.x = T
)

rm(speed_limits_dt)

# check which speed limits were already filtered, and continue the process
# for the ones that were not
to_filter_dt <- cleaned_tracks[
  , fout := gsub("2_cleaned", "3_filtered_speed", file)][
    file.exists(fout) == F][
  , .(file, fout, speed_limit)]


rm(cleaned_tracks)

ntrk <- nrow(to_filter_dt)

if(ntrk > 0){
  
  lapply(seq(ntrk), function(i){
    
    # get speed limit input and output file
    sp_limit <- to_filter_dt[i, speed_limit]
    fin <- to_filter_dt[i, file]
    fout <- to_filter_dt[i, fout]
    
    cat(sprintf("\n Filtering speeds: %d | %d!", i, ntrk))
    
    track <- fread(fin)
    nlocs <- nrow(track)
    
    track <- filter_speed_limit(
      track = track, 
      coords = c("x", "y"), 
      t_col = "timestamp", 
      crs = sf::st_crs(4326),
      speed_limit = sp_limit, 
      lonlat = T, units = "secs"
    )
    
    
    if(nrow(track) >= 3){ 
      
      fwrite(track, fout) 
      
    } else { 
      
      out <- data.table(
        nlocs_before_filter = nlocs, 
        nlocs_after_filter = nrow(track), 
        speed_limit = sp_limit
      )
      
      fout <- gsub(".rds", "_nodata.rds", fout)
      
      fwrite(out, fout)
      
    }
    
    return(invisible(NULL))
    
  })
  
}



# 5 - Get filter report -------------------------------------------------------

# OUTPUT FILE 
file_filter_report <- "3_filtered_speed_report.csv"

files <- list.files(
  file.path(list.files(study_dir, full.names = T), "3_filtered_speed"), 
  full.names = T)
  
nsf <- length(files)

filtered_report <- rbindlist(lapply(seq(nsf), function(i){

  fin <- files[i]
  
  if(grepl("_nodata.rds", fin)){ return(fread(fin)[, file := fin]) } 
  
  n_filtered <- nrow(fread(fin))
  n_unfiltered <- nrow(fread(gsub("3_filtered_speed", "2_cleaned", fin)))
  
  cat(sprintf("\nReading files: %d | %d!", i, nsf))
  
  dt_out <- data.table(
    nlocs_before_filter = n_unfiltered,
    nlocs_after_filter = n_filtered, 
    file = fin
  )
  
  return(dt_out)
  
}), fill = T)


filtered_report[, ':=' (
  species = gsub(
    "_", " ", gsub(".*/Studies/(.*?)/3_filtered_speed/.*", "\\1", file)), 
  perc_remained = round(
    nlocs_after_filter/nlocs_before_filter * 100, 2), 
  track_saved = !grepl("nodata", file))]

fwrite(filtered_report, file.path(data_dir, file_filter_report))

