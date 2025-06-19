#' ---
#' title: "filter tracks by speed using literature limits"
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
#' **SECTION 3 - Filter speeds**
#' using the theoretical speed limit for each species, filter the tracks
#' and save them.
#'     
#' **SECTION 4 - Filter report**
#' load filtered and unfiltered data and record how many locations there are
#' before and after filtering.

# 0 - Load packages and files --------------------------------------------------

library(data.table)
library(amt)
library(ggplot2)
library(patchwork)
source("0_helper_functions.R")
source("3_filter_by_speed_datatable_FUNCTIONS.R")

data_dir <- here::here("Data")

# INPUT
file_deploy_clean <- "2_deployments_cleaned.csv"

# OUTPUT
file_speed_quantiles <- "3_target_sp_speed_quantiles.csv"
file_filter_report <- "3_filtered_speed_report.csv"
graph_speed_quantiles <- "3_speed_quantiles_vs_limit.png"


# get cleaned deployment list
cleaned_tracks <- fread(file.path(data_dir, file_deploy_clean))[excluded == F][
  , .(file, sex, birdlife_name)]

target_sp <- cleaned_tracks[, unique(birdlife_name)]

# Create output directories
graphs_dir <- file.path(data_dir, "Graphs")
if(!dir.exists(file.path(graphs_dir, "3_filtered_speed"))){ 
  dir.create(file.path(graphs_dir, "3_filtered_speed"), recursive = T)
}

create_dir(target_sp, new_dir = "3_filtered_speed")

# 1 - Plot speed and turning angles ---------------------------------------

if(!file.exists(file.path(data_dir, file_speed_quantiles))){

  # get birdlife phylogeny and if there is no speed limit specified for the 
  # species, infer one from the family or the order
  target_sp <- add_birdlife_phylogeny(species_name = target_sp)
  target_sp[
    , speed_limit := get_speed_limit(
      species = birdlife_name, family = family, order = order),
    by = birdlife_name]

  # for progress reporting
  dc <- nrow(target_sp)

  # get the speed and turns distribution for the species across all tracks
  speed_quant_df <- rbindlist(lapply(seq(1, dc), function(i){

    sp_name <- target_sp[i, birdlife_name]

    # read the track
    tracks <- cleaned_tracks[birdlife_name == sp_name, file]
    n_tracks <- length(tracks)

    df_speed <- rbindlist(lapply(seq_along(tracks), function(t){

      track <- readRDS(tracks[t])

      # calculate speed and turn
      dfs <- as.data.table(steps(track, lonlat = T))[, .(dt_, sl_, ta_)][
        ,  speed := sl_/as.numeric(dt_, unit = "secs")][, .(speed, ta_)]

      message(paste(sp_name, "- got speed and turns for track", t, "|", n_tracks))

      return(dfs)

    }))

    # plot speed and turning angle
    p <- plot_speed_turns(
      df_speed,
      speed_limit = target_sp[1, speed_limit],
      speed_col = "speed", turn_col = "ta_"
    ) + plot_annotation(
      title =  paste(sp_name, "- speed and turning angles"),
      subtitle = paste(n_tracks, "cleaned deployments unfiltered")
    )

    p_name <- paste0(gsub(" ", "_", sp_name),  "_speed_turn.png")

    # save the plots
    ggsave(
      filename = file.path(graphs_dir, "3_filtered_speed", p_name),
      plot = p, width = 16, height = 10, units = "cm",
    )

    # save the quantiles of speed distribution
    quant_df <- as.data.table(
      as.list(
        round(
          quantile(df_speed$speed, seq(0.5, 1, 0.05), na.rm = TRUE), 3)
      )
    )
    quant_df <- cbind(target_sp[i,], quant_df)

    return(quant_df)

  }))

  fwrite(speed_quant_df, file.path(data_dir, file_speed_quantiles))

}
  

# 2 - Plot speed quantiles ----------------------------------------------------

pal <- c("#088F8F", "#6e1354")

if(!file.exists(file.path(graphs_dir, graph_speed_quantiles))){
  
  speed_quant_df <- fread(file.path(data_dir, file_speed_quantiles))
    
  df_plot <- melt(
    speed_quant_df, 
    id.vars = c("birdlife_name", "speed_limit"),  
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
  
  ggsave(
    file.path(graphs_dir, graph_speed_quantiles), 
    height = 12
  )

}


# 3 - Filter speeds ------------------------------------------------------------

cleaned_tracks[, fout := gsub("2_cleaned", "3_filtered_speed", file)]

# get the theoretical speed limit for all the species in the track list
speed_limits <- add_birdlife_phylogeny(
  df = unique(cleaned_tracks[, .(birdlife_name, sex)]), 
  species_name = "birdlife_name"
)[
  , speed_limit := get_speed_limit(
    species = birdlife_name, family = family, order = order, sex = sex), 
  by = c("birdlife_name", "sex")
]

# add the spedd limit to the track info
cleaned_tracks <- merge(
  cleaned_tracks, speed_limits, by = c("birdlife_name", "sex"), all.x = T
)

rm(speed_limits)

# check which speed limits were already filtered, and continue the process
# for the ones that were not
to_filter <- cleaned_tracks[file.exists(fout) == F][
  , .(file, fout, speed_limit)]

n_tracks <- nrow(to_filter)

invisible(lapply(seq(1, n_tracks), function(i){
  
  # get speed limit input and output file
  sp_limit <- to_filter[i, speed_limit]
  fin <- to_filter[i, file]
  fout <- to_filter[i, fout]
  
  message(sprintf("Filtering speeds for track %d | %d!", i, n_tracks))
  
  track <- readRDS(fin)
  
  track <- filter_speed_limit(
    track = track, 
    speed_limit = sp_limit, 
    lonlat = T, units = "secs"
  )
  
  if(nrow(track) >= 3){ saveRDS(track, file = fout, compress = F) }
  
  return(NULL)
  
  }))


# 4 - Filter report -------------------------------------------------------

cleaned_tracks[, fout_exists := file.exists(fout)]

n_tracks <- nrow(cleaned_tracks)

filtered_report <- rbindlist(lapply(seq(1, n_tracks), function(i){
  
  df_out <- cleaned_tracks[i, ]
  fin <- df_out[,file]
  fout <- df_out[, fout]
  fout_exists <- df_out[, fout_exists]
  
  message(sprintf("Reading files %d | %d!", i, n_tracks))
  
  if(fout_exists){ n_filtered <- nrow(readRDS(fout)) }
  
  n_unfiltered <- nrow(readRDS(fin))
  
  df_out <- df_out[
    , n_unfiltered := n_unfiltered][
    , n_filtered := if(fout_exists) n_filtered else NA][
    , data_percentage_saved := round(n_filtered/n_unfiltered*100, 1)
  ][, file := NULL][, file := fout][, fout := NULL]
  
  return(df_out)
  
}))

fwrite(filtered_report, file.path(data_dir, file_filter_report))

