library(data.table)
library(dispfit)
library(fitdistrplus)
library(patchwork)


# MAIN DATA DIRECTORY
data_dir <- here::here("Data")
study_dir <- file.path(data_dir, "Studies")
graphs_dir <- file.path(data_dir, "Graphs")
sleep_dirs <- file.path(
  list.files(study_dir, full.names = T), "7_sleep_clusters")

dir.create(file.path(graphs_dir, "8_fit_distributions"), showWarnings = FALSE)


# -------------------------------------------------------------------------

plots_dir <- file.path(
  graphs_dir, "8_fit_distributions", "1_revisit_times_fit_check")

dir.create(plots_dir)

files <- list.files(sleep_dirs, full.names = T, 
    pattern = "1_all_tracks_dcp_sleep_spots.*_continent.rds")


lapply(seq_along(files), function(i){
  
  print(paste0("Processing file ", i, " of ", length(files)))
  
  fin <- files[i]
  
  spots <- fread(fin)
  nlocs <- uniqueN(spots$day_cycle)
  nfiles <- uniqueN(spots$file)
  
  if(length(unique(spots$revisit_day_cycle)) == 1) { return(NULL) }
  
  
  sp <- sub(".*/Studies/([^/]+)/7_sleep_clusters/.*", "\\1", fin)
  dl <- sub(
    ".*/1_all_tracks_dcp_sleep_spots_(.*)_continent.rds", "\\1", fin)
  
  pname <- file.path(
    plots_dir, paste(sp, dl, "revisit_time_fits.png", sep = "_"))
  
  sp <- gsub("_", " ", sp) 
  
  ptit <- paste0(
    sp, " [", nlocs, " nights, ", nfiles, " tracks] L: dbscan R: hclust")
  
  
  png(pname, width = 1400, height = 800, res = 150)
  par(mfrow = c(1,2))
  
  tryCatch(
    descdist(spots[method == "dbscan"]$revisit_day_cycle, boot = 1000)
    , error = function(e) {
      hist(spots[method == "dbscan"]$revisit_day_cycle, 
           main = "Dbscan - error", xlab = "Revisit time (days)", 
           breaks = 100)
    })
  
  tryCatch(
    descdist(spots[method == "hclust"]$revisit_day_cycle, boot = 1000)
    , error = function(e) {
      hist(spots[method == "hclust"]$revisit_day_cycle, 
           main = "Hclust - error", xlab = "Revisit time (days)", 
           breaks = 100)
    })

  # Add a main title across both
  mtext(ptit, outer = TRUE, line = -1, font = 2, cex = 1)
  
  dev.off()
  
  
})




descdist(spots_m$revisit_day_cycle, boot = 1000)

plotdist(spots_m$revisit_day_cycle, histo = T, demp = T, breaks = 100)


# Add a main title across both
species <- "SpeciesName" # Replace with your species name
n_points <- length(spots_m$revisit_day_cycle)
mtext(paste0(species, " (n = ", n_points, ")"), outer = TRUE, line = -2, cex = 2)

dev.off()

dp + pp
