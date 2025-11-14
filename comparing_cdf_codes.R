library(fitdistrplus)
library(data.table)
library(ggplot2)
library(patchwork)
library(dispfit)
library(purrr)
source("6_distance_fit_FUNCTIONS.R")

data_dir <- here::here("Data")
study_dir <- file.path(data_dir, "Studies")
graphs_dir <- file.path(data_dir, "Graphs")

plot_dir <- file.path(graphs_dir, "6_distance_fit")
dir.create(plot_dir, showWarnings = F)


fit_dir <- file.path(data_dir, "6_distance_fit")

file_fits <- list.files(file.path(fit_dir, "Sl_median"), full.names = T)

lf <- length(file_fits)

plots_path <- "/home/nina/R_projects/WNV_dispersal/Data/6_distance_fit/Sl_median_PLOTS"

eps <- 1e-12


files <- list.files(plots_path, pattern = ".rds")

species <- unique(sub("_sl_median_fit.*$", "", files))


check_dt <- rbindlist(lapply(species, function(sp){
  
  cat(sp, "\n")
  rds_files <- list.files(
    plots_path, pattern = sprintf("%s.*.rds", sp), full.names = T)
  
  if(sum(grepl("FEDE|NINA", rds_files)) == 2){
    
    fede <- readRDS(grep("FEDE", rds_files, value = T)) 
    
    fede <- melt(
      setDT(fede)[, r := NULL], 
      id.vars = c("data"), 
      variable.name = "fitted_function", value.name = "cdf_fede"
    )
    
    fede[, fitted_function := gsub("_cdf", "", fitted_function)]
    fede <- unique(fede)
    
    nina <- fread(grep("NINA", rds_files, value = T))
    nina <- unique(nina[, .(data, fitted_function, cdf_nina = cdf)])
    
    full_dt <- merge(nina, fede, by = c("data", "fitted_function"), all.x = T)
    
    out_dt <- data.table(
      species = sp,
      n_rows_fede = nrow(fede),
      n_rows_nina = nrow(nina),
      check_10 = sum(round(full_dt$cdf_fede, 10) == round(full_dt$cdf_nina, 10)), 
      check_15 = sum(round(full_dt$cdf_fede, 15) == round(full_dt$cdf_nina, 15))
    )
    return(out_dt)
    
  } else {return(NULL)}
  
  
 
    
}))
