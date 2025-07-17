#' ---
#' title: "Resample tracks"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#'  **SECTION 0 - Load packages and files**
#'  
#'  **SECTION 1 - Calculate sampling rate per track**
#'  summarize sampling rate for the cleaned tracks 
#'  
#'  **SECTION 2 - Plot sampling rates per species**
#'  plot the distribution of sampling rates across species using median 
#'  sampling rate per track in hours
#'  
#'  **SECTION 3 - Re-sample tracks**
#'  re-sample the track to a defined rate, in this case 60 minutes
#'  add day_cycle and day_period columns 
#'  output saved in "5_resampled" in the species folder

# 0 - Load packages and files --------------------------------------------------

library(data.table)
library(amt)
library(ggplot2)

data_dir <- here::here("Data")
study_dir <- file.path(data_dir, "Studies")
graphs_dir <- file.path(data_dir, "Graphs")

# create output folders
invisible(lapply(
  file.path(list.files(study_dir, full.names = T), "4_resampled"), 
  dir.create, showWarnings = FALSE))

# OUTPUT
file_sampl_rate <- "4_filtered_deployments_sampling_rate.csv"
file_resampled_report <- "4_resampled_report.csv"


# INPUT tracks
files <- list.files(
  file.path(list.files(study_dir, full.names = T), "3_filtered_speed"), 
  full.names = T, pattern = "sen.rds")
ntrk <- length(files)

# 1 - Calculate sampling rate per track ------------------------------------

# calculate the sampling rate per track for all species
if(!file.exists(file.path(data_dir, file_sampl_rate))){
  
  sampl_rate <- rbindlist(lapply(seq_along(files), function(i){
    
    fin <- files[i]
    
    cat(sprintf("\n Getting sampling rate: %d | %d!", i, ntrk))
    
    track_time <- fread(fin)[, timestamp]
    n_locs <- length(track_time)
    
    dt <- data.table(
      timelag = difftime(track_time[-1], track_time[-n_locs], units = "min"))
    
    dt <- dt[, c(
      as.list(unclass(summary(timelag))),
      sd = sd(timelag))]
    dt[, names(dt) := lapply(.SD, round, 2)]
    dt[, ':=' (
      n_locs = n_locs, 
      file = fin, 
      unit = "min")]
    
    return(dt)
    
  }))
  
  sampl_rate <- sampl_rate[, birdlife_name := gsub(
    "_", " ", gsub(".*Studies/(.*?)/3_filtered_speed/.*", "\\1", file))]
  
  setorder(sampl_rate, birdlife_name)
  
  id_cols <- c("birdlife_name", "file")
  setcolorder(sampl_rate, c(id_cols, setdiff(names(sampl_rate), id_cols)))
  
  setnames(
    sampl_rate, old = names(sampl_rate), new = tolower(names(sampl_rate)))
  
  fwrite(sampl_rate, file.path(data_dir, file_sampl_rate))
  
} 


# 2 - Plot sampling rates across species --------------------------------------

# OUTPUT FILE
pout <- "4_median_sampling_rate_hist.png"

if(!file.exists(file.path(graphs_dir, pout))){
  
  sampl_rate <- fread(file.path(data_dir, file_sampl_rate))
  
  # because we are only interested in daily sampling frequencies
  sampl_rate <- sampl_rate[, median_hours := median/60][median_hours <= 25]
  sampl_rate[, birdlife_name := factor(
    birdlife_name, levels = sort(unique(birdlife_name), decreasing = T))]
  
  trkcount <- sampl_rate[, .(n_tracks = uniqueN(file)), by = birdlife_name]
  trkcount[, n_tracks := sprintf("[N=%d]", n_tracks)]
  
  ggplot(sampl_rate) +
    geom_boxplot(
      aes(x = median_hours, y = birdlife_name), color = "gray22", fill = "gray80", 
      alpha = 0.8
    ) +
    geom_text(data = trkcount, aes(y = birdlife_name, x = 26, label = n_tracks)) +
    labs(
      x = "median sampling rate per track [h]", 
      y = "species",
      title = "Distrubution of median time lags per track"
    ) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) 
  
  ggsave(file.path(graphs_dir, pout), height = 35, width = 15)
  
}


# 3 - Resample tracks -----------------------------------------------------

# for the data that has high resolution, we first re-sampled the track to, 
# arbitrary decidison of 60 minutes 
resample_rate <- minutes(60)
resample_tolerance <- minutes(15)

if(!file.exists(file.path(data_dir, file_resampled_report))){
  
  # calculate the sampling rate per track for all species
  resampled_tracks <- rbindlist(lapply(seq_along(files), function(i){
    
    fin <- files[i]
    fout <-  gsub("3_filtered_speed", "4_resampled", fin)
    
    track <- fread(fin)
    n_org <- nrow(track)
    
    track <- track |>
      make_track(
        x, y, timestamp,
        crs = sf::st_crs(4326),
        all_cols = TRUE
      )
    
    track <- track |> 
      track_resample(
        rate = resample_rate,
        tolerance = resample_tolerance
      ) |> 
      select(-burst_)
    
    setDT(track)
    setorder(track, t_)
    n_resampled <- nrow(track)
    
    message(sprintf("Processed track: %d | %d!", i, ntrk))
    
    dt_out <- data.table(
      file = fout,
      n_before_resample = n_org,
      n_after_resample = n_resampled)
    
    
    if(n_resampled >=3){ 
      
      fwrite(track, fout)
      
    } else{
      
      fout <- gsub(".rds", "_nodata.rds", fout)
      
      fwrite(dt_out, fout)
    }
    
    return(dt_out)
    
  }), fill = T)
  
  
  resampled_tracks <- resampled_tracks[, `:=` (
    species = gsub("_", " ", gsub(".*/Studies/(.*?)/4_resampled/.*", "\\1", file)), 
    resample_rate_min = as.numeric(resample_rate, units = "mins"),
    resample_tolerance_min = as.numeric(resample_tolerance, units = "mins"))]
  
  
  fwrite(resampled_tracks, file.path(data_dir, file_resampled_report))
  
}

