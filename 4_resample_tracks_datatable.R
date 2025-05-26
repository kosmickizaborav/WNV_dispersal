#' ---
#' title: "Resample tracks"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#'  **SECTION 1 - Calculate sampling rate per track**
#'  for each track and species summarize the sampling rate
#'  
#'  **SECTION 2 - Plot sampling rates per species**
#'  plot the distribution of sampling rates per species using t
#'  the median sampling rate
#'  
#'  **SECTION 3 - Re-sample track to defined rate**
#'  re-sample the track to a defined rate, in this case 60 minutes
#'  add day_cycle and day_period columns 
#'  output saved in "5_resampled" in the species folder

# 0 - Defining parameters and packages ---------------------------------------

library(data.table)
library(amt)
library(ggplot2)
library(suncalc)
source("0_helper_functions.R")


data_dir <- here::here("Data")
graphs_dir <- file.path(data_dir, "Graphs")

# INPUT
file_filter_report <- "3_filtered_speed_report.csv"

# OUTPUT
file_sampling_rate <- "4_filtered_deployments_sampling_rate.csv"
file_resampled_report <- "4_resampled_report.csv"

# get cleaned deployment list
filtered_tracks <- fread(file.path(data_dir, file_filter_report))[
  fout_exists == T]

n_tracks <- nrow(filtered_tracks)

# create output folders
create_dir(unique(filtered_tracks$birdlife_name), new_dir = "4_resampled")

# 1 - Calculate sampling rate per track ------------------------------------

# calculate the sampling rate per track for all species
sampling_rate <- rbindlist(lapply(seq(1, n_tracks), function(i){
  
  fin <- filtered_tracks[i, file]
  
  message(sprintf("Getting sampling rate %d | %d!", i, n_tracks))
  
  track <- readRDS(fin)
  
  df_out <- as.data.table(summarize_sampling_rate(track, time_unit = "min"))
  df_out[, file := fin]
  
  return(df_out)
  
}))

sampling_rate <- merge(
  sampling_rate, filtered_tracks[, .(birdlife_name, file)], by = "file")

fwrite(sampling_rate, file.path(data_dir, file_sampling_rate))


# 2 - Plot sampling rates per species --------------------------------------

sampling_rate <- sampling_rate[, median_hours := median/60][
  median_hours <= 24]

ggplot(sampling_rate) +
  geom_boxplot(
    aes(x = median_hours, y = birdlife_name), color = "gray22", fill = "gray80"
  ) +
  labs(
    x = "median sampling rate per track [h]", 
    y = "species",
    title = "Distrubution of median time lags per track"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

ggsave(
  file.path(graphs_dir, "4_median_sampling_rate_hist.png"), 
  height = 20,
  width = 15,
  units = "cm"
)



# 2 - Resample tracks -----------------------------------------------------


# for the data that has high resolution, we first re-sampled the track to, 
# arbitrary decidison of 60 minutes 
resample_rate <- minutes(60)
resample_tolerance <- minutes(15)

tracks <- sampling_rate[, file]
n_tracks <- length(tracks)

# calculate the sampling rate per track for all species
resampled_tracks <- rbindlist(lapply(seq_along(tracks), function(i){
  
  fin <- tracks[i]
  fout <-  gsub("3_filtered_speed", "4_resampled", fin)
  
  track <- readRDS(fin)
  
  n_org <- nrow(track)
  
  track <- track |> 
    track_resample(
      rate = resample_rate,
      tolerance = resample_tolerance
    ) |> 
    select(-burst_)
  
  message(sprintf("Processing track %d | %d!", i, n_tracks))
 
  n_resampled <- nrow(track)
  
  df_out <- data.table(
    fin = fin, 
    fout = fout,
    n_org_track = n_org,
    n_resampled = n_resampled,
    saved = n_resampled >= 3
  )
  
  if(df_out[, saved]){ saveRDS(track, fout) }
  
  return(df_out)
  
}))


resampled_tracks <- merge(
  resampled_tracks, filtered_tracks[, .(birdlife_name, file)], 
  by.x = "fin", by.y = "file")[
  , fin := NULL][
  , `:=`(
    resample_rate_min = as.numeric(resample_rate, units = "mins"),
    resample_tolerance_min = as.numeric(resample_tolerance, units = "mins")
  )]
names(resampled_tracks)[1] <- "file"


fwrite(resampled_tracks, file.path(data_dir, file_resampled_report))
