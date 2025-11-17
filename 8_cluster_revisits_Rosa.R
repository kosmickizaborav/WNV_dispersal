library(data.table)
library(ggplot2)
library(patchwork)
library(MittagLeffleR)
library(stabledist)
library(reticulate)
source("color_palettes.R")

# to set up the virustal environment in Python, ran these commands in terminal: 
# python3 -m venv /home/nina/.virtualenvs/mlenv
# /home/nina/.virtualenvs/mlenv/bin/python -m pip install --upgrade pip setuptools wheel
# /home/nina/.virtualenvs/mlenv/bin/python -m pip install numpy scipy

# tell reticulate to use that env
use_virtualenv("mlenv", required = T)
source_python("ml_fit.py")



crs <- sf::st_crs(4326)

data_dir <- here::here("Data")
study_dir <- file.path(data_dir, "Studies")
graphs_dir <- file.path(data_dir, "Graphs")


cluster_dirs <- file.path(
  list.files(study_dir, full.names = T), "7_sleep_clusters")
# MAKE OUTPUT DIRECTORIES
revisit_dirs <- gsub("7_sleep_clusters", "8_cluster_revisits", cluster_dirs)
lapply(revisit_dirs, dir.create, showWarnings = F)

plots_dir <- file.path(graphs_dir, "8_cluster_revisits")
dir.create(plots_dir, showWarnings = F)

traits_dt <- fread(here::here("Published_data", "00_bird_traits.csv"))


# parameters --------------------------------------------------------------

# window size
wdw <- 7 # days
min_wdw <- wdw
max_gap <- 0


# 1 - Prepare data -------------------------------------------------------------

fin_name <- "3_dbscan_clusters_npts_5_distthr_350_nauticalDawn_nauticalDusk.rds"
fout_name <- sprintf(
  "1_sleep_clusters_bursts_%d_window_%d_gap.rds", min_wdw, max_gap)

files <- list.files(cluster_dirs, pattern = fin_name, full.names = T)
files_out <- file.path(
  gsub("7_sleep_clusters", "8_cluster_revisits", dirname(files)), fout_name)

files <- files[
  !(file.exists(files_out) | file.exists(gsub(".rds", "_nodata.rds", files_out)))]
nf <- length(files)

if(nf > 0){
  
  lapply(seq_along(files), function(n){
    
    fin <- files[n]
    fout <- file.path(
      gsub("7_sleep_clusters", "8_cluster_revisits", dirname(fin)), fout_name)
    
    dcp_dt <- fread(fin)[, .(file, day_cycle, sleep_cluster)]
    
    # Remove the deployments that are duplicated because of the two sensors, 
    # keep GPS when possible
    to_keep <- data.table(file = unique(basename(dcp_dt$file)))
    to_keep <- to_keep[, ':=' (
      file_id = sub("^(.*dep)_\\d+.*$", "\\1", file), 
      sensor = as.numeric(sub("^.*dep_(\\d+)_sen.*", "\\1", file)))]
    setorder(to_keep, sensor)
    to_keep <- to_keep[, duplicate := duplicated(file_id)]
    to_keep <- to_keep[duplicate == FALSE][, file]
    dcp_dt <- dcp_dt[basename(file) %in% to_keep]
    
    # Order by individual and date
    setorder(dcp_dt, file, day_cycle)
    
    dcp_dt[
      , track_length := max(day_cycle) - min(day_cycle) + 1, by = file]
    
    # check continuity of the tracking data
    dcp_dt[
      , burst_gap := c(0, as.numeric(diff(day_cycle, units = "days"))-1), 
      by = file]
    # group track by bursts (tracking gap lower than max_gap
    dcp_dt[, burst := cumsum(burst_gap > max_gap)+1, by = file]
    dcp_dt[, ':=' (
      burst_start = min(day_cycle), 
      burst_end = max(day_cycle),
      burst_length = max(day_cycle) - min(day_cycle) + 1
    ), by = .(burst, file)]
    
    # select only the bursts that would have enough data for the revisiting analysis
    dcp_dt <- dcp_dt[burst_length >= min_wdw]
    
    if(nrow(dcp_dt) == 0){
      
      fout <- gsub(".rds", "_nodata.rds", fout)
      fwrite(data.table(burst_length = min_wdw), fout)
      
    } 
    
    setorder(dcp_dt, file, day_cycle)
    dcp_dt[, file_id := .GRP, by = file]
    dcp_dt[, burst_id := paste(file_id, burst, sep = "-")]
    dcp_dt[, burst_day := day_cycle - burst_start + 1, by = burst_id]
    dcp_dt[, sleep_cluster_id := paste(burst_id, sleep_cluster, sep = "-")]
      
    fwrite(dcp_dt, fout)
    
    cat("DONE:", n, "|", nf, "\n")
    
  })
  
}

rm(fout_name, fin_name, files, files_out)


# 2 - Revisits -----------------------------------------------------------------

fin_name <- sprintf(
  "1_sleep_clusters_bursts_%d_window_%d_gap.rds", min_wdw, max_gap)
files <- list.files(revisit_dirs, pattern = fin_name, full.names = T)

files_out <- gsub(".rds", "_all_cluster_revisits.rds", files)


files <- files[!file.exists(files_out)]
nf <- length(files)


if(nf > 0){
  
  lapply(seq_along(files), function(n){
    
    fin <- files[n]
    sp <- gsub(".*/Studies/(.*)_(.*)/8_cluster_revisits/.*$", "\\1 \\2", fin)
    
    dcp_dt <- fread(fin)[
      , .(file, file_id, day_cycle, sleep_cluster, sleep_cluster_id, 
          burst_id, burst_start, burst_end)]
    
    dcp_dt[
      , first_in_burst := .I == min(.I), by = .(file, sleep_cluster_id)]
    
    # checking if the coluster was revisited in that burst
    dcp_dt[, total_visits := .N, by = sleep_cluster_id]
    
    
    # for calculating revisits, 
    # select only the first occurrence of each cluster in each burst
    cluster_dt <- dcp_dt[
      first_in_burst == TRUE & sleep_cluster != 0 & total_visits > 1][
      , .(sleep_cluster_id, burst_id, burst_start, burst_end, day_cycle)]
    
    if(nrow(cluster_dt) == 0){ return(NULL) }
    
    cluster_dt[, cluster_start_day := day_cycle][, day_cycle := NULL]
    
    dcp_dt[, first_in_burst := NULL]
    
    # get cluster ids to loop over
    cls_ids <- unique(cluster_dt$sleep_cluster_id)
    
    cls_full <- rbindlist(lapply(cls_ids, function(cls){
      
      cls_dt <- cluster_dt[sleep_cluster_id == cls]
      s <- cls_dt$cluster_start_day
      e <- cls_dt$burst_end # in theory it should be fine only with burst_id
      brst <- cls_dt$burst_id
      
      cls_revisit <- dcp_dt[day_cycle >= s & day_cycle <= e & burst_id == brst][
        , .(file, burst_id, sleep_cluster_id, day_cycle)]
      
      # days since the first visit
      cls_revisit[, visit_n := 0:(.N-1)]
      
      # total number of visits since the first visit
      cls_revisit[, cum_revisits := cumsum(sleep_cluster_id == cls)-1]
      
      cls_revisit[, selected_cluster := cls]
      
    }))
    
    fwrite(cls_full, gsub(".rds", "_all_cluster_revisits.rds", fin))
    
    cat("DONE:", n, "|", nf, "\n")
    
  })
}


# P: Revisit rate time window --------------------------------------------------

fout <- "8_sleep_cluster_revisit_rate_varying_tau_step.csv"

tau_step <- 7

burst_dt <- fread(
  file.path(data_dir, "8_summary_burst_length_min7_per_species.csv"))

burst_dt <- burst_dt[floor(mean_burst_length/wdw*wdw) >= 35]
burst_dt <- burst_dt[n_files >= 10 | n_bursts >= 10]

selected_sp <- burst_dt[, species]

selected_dirs <- grep(
  paste(gsub(" ", "_", selected_sp), collapse = "|"), revisit_dirs, value = T)

fin_name <- sprintf(
  "1_sleep_clusters_bursts_%d_window_%d_gap_all_cluster_revisits.rds", 
  min_wdw, max_gap)
files <- list.files(selected_dirs, pattern = fin_name, full.names = T)
nf <- length(files)

burst_dir <- file.path(plots_dir, "1_revisit_rate_windows")
dir.create(burst_dir, showWarnings = F)


if(!file.exists(file.path(data_dir, fout))){
  
  
  tau_full <- rbindlist(lapply(seq_along(files), function(n){
    
    fin <- files[n]
    sp <- gsub(".*/Studies/(.*)_(.*)/8_cluster_revisits/.*$", "\\1 \\2", fin)
    
    dt <- fread(fin)
    
    n_cls_all <- uniqueN(dt$selected_cluster)
    
    max_revisits <- dt[, .SD[which.max(visit_n)], by = selected_cluster]
    
    max_n_wdw <- floor(mean(max_revisits$visit_n)/tau_step)
    
    
    tau_dt <- rbindlist(lapply(1:max_n_wdw, function(i){
      
      # should not include 0 for the fits
      dt_plot <- dt[visit_n == i*tau_step & cum_revisits > 0]
      
      dt_plot[, med_revisit := sum(cum_revisits)/uniqueN(selected_cluster)]
      dt_plot[, revisit_rate := cum_revisits/med_revisit]
      
      n_ind <- uniqueN(dt_plot$file)
      n_brst <- uniqueN(dt_plot$burst_id)
      n_cls <- uniqueN(dt_plot$selected_cluster)
      mrr <- mean(dt_plot$revisit_rate)
      vrr <- var(dt_plot$revisit_rate)
      
      dt_plot |> 
        ggplot() + 
        geom_histogram(
          aes(x = revisit_rate), bins = 100,
          fill = "#682860", color = "gray33", linewidth = 0.2, alpha = 0.5) +
        theme_bw() +
        labs(
          x = "revisit rate", 
          title =  bquote(.(sp) ~ "|" ~ tau == .(i * tau_step)), 
          subtitle = sprintf(
            "revisit rate - mean: %.2f | variance: %.3f", mrr, vrr),
          caption = sprintf(
            "N individuals = %d | N bursts = %d | N patches total = %d | N patches = %d]", 
            n_ind, n_brst, n_cls_all, n_cls)
        )
      
      pname <- sprintf(
        "%s_revisit_rate_tau_%d_days.png", gsub(" ", "_", sp), i*tau_step)
      
      ggsave(file = file.path(burst_dir, pname), width = 10, height = 8)
      
      out_dt <- data.table(
        tau = i*tau_step, 
        n_individuals = n_ind,
        n_bursts = n_brst,
        n_clusters_total = n_cls_all,
        n_clusters = n_cls,
        prop_clusters = n_cls/n_cls_all,
        mean_revisit_rate = mrr,
        var_revisit_rate = vrr, 
        species = sp, 
        file = fin
      )
      
      return(out_dt)
      
    }))
    
    cat("DONE:", n, "|", length(files), "\n")
    
    return(tau_dt)
    
  }))
  
  # tolerance for percentage of clusters revisited
  tol <- 0.05  
  
  tau_selected <- tau_full[, {
    d <- abs(prop_clusters - 0.5)
    md <- min(d, na.rm = TRUE)
    cand <- .SD[d <= md + tol]
    cand[order(-tau)][1]
  }, by = species][, .(species, tau)]
  tau_selected[, selected := T]
  
  tau_full <- merge(
    tau_full, tau_selected, by = c("species", "tau"), all.x = T)
  
  fwrite(tau_full, file.path(data_dir, fout))
  
  
}

rm(burst_dt)

# 3 - Prepare data for fits tau-------------------------------------------------


tau_selected <- fread(
  file.path(data_dir, "8_sleep_cluster_revisit_rate_varying_tau_step.csv"))
tau_selected <- tau_selected[selected == TRUE]

files <- unique(tau_selected$file)

files <- files[!file.exists(
  gsub("_all_cluster_revisits.rds", "_revisits_for_fit.rds", files))]

if(nf > 0){
 
  lapply(seq_along(files), function(i){
    
    fin <- files[i]
    sp <- gsub(".*/Studies/(.*)_(.*)/8_cluster_revisits/.*$", "\\1 \\2", fin)
    
    dt <- fread(fin)
    
    taut <- tau_selected[species == sp]$tau
    
    dt_fit <- dt[visit_n == taut & cum_revisits > 0]
    dt_fit[, med_revisit := sum(cum_revisits)/uniqueN(selected_cluster)]
    dt_fit[, revisit_rate := cum_revisits/med_revisit]
    
    fout <- gsub("_all_cluster_revisits.rds", "_revisits_for_fit.rds", fin)
    
    fwrite(dt_fit, fout)
    
    cat("DONE:", i, "|", nf, "\n")
    
  })
   
}

rm(files, nf, tau_selected, tau_dt)


# fits --------------------------------------------------------------------

files <- list.files(
  revisit_dirs, pattern = "_revisits_for_fit.rds", full.names = T)

files <- files[!file.exists(file.path(dirname(files), "ml_fit.rds"))]
nf <- length(files)

fit_plots <- file.path(plots_dir, "3_ml_fits")
dir.create(fit_plots, showWarnings = F)

if(nf > 0){
  
  lapply(seq_along(files), function(i){
    
    fin <- files[i]
    sp <- gsub(".*/Studies/(.*)_(.*)/8_cluster_revisits/.*$", "\\1 \\2", fin)
    
    # info about the fitted data
    dt <- fread(fin)
    taut <- unique(dt$visit_n)
    n_ind <- uniqueN(dt$file)
    n_brst <- uniqueN(dt$burst_id)
    n_cls <- uniqueN(dt$selected_cluster)
    mrr <- mean(dt$revisit_rate)
    vrr <- var(dt$revisit_rate)
    
    xi_pos <- dt$revisit_rate
    # Call fitter (no bootstrap)
    res <- fit_ml_from_list(xi_pos, do_bootstrap = F)
    saveRDS(res, file = file.path(dirname(fin),  "ml_fit.rds"))
    
    # Get predicted pdf on a grid (call ml_distr from Python)
    beta_hat <- res$beta_hat
    xgrid <- seq(min(xi_pos), max(xi_pos), length.out = 400)
    # this calls the Python ml_distr; reticulate converts return
    pdf_vals <- ml_distr(xgrid, beta_hat)   
    
    df_pdf <- data.frame(x = xgrid, y = as.numeric(pdf_vals))
    
    dt |>
      ggplot() +
      geom_histogram(
        aes(x = revisit_rate, y = after_stat(density)),
        bins = 60,
        fill = "skyblue",
        alpha = 0.5,
        color = "gray30",
        linewidth = 0.2
      ) +
      # overlay pdf from df_pdf (use inherit.aes = FALSE)
      geom_line(
        data = df_pdf, 
        aes(x = x, y = y), inherit.aes = FALSE, color = "#682860", linewidth = 1
      ) +
      theme_bw() +
      labs(
        x = "revisit rate",
        y = "density",
        # build title using bquote (values inserted with .())
        title = bquote(.(sp) ~ "|" ~ tau == .(taut)),
        subtitle = sprintf(
          "revisit rate ~ mean = %0.1f, variance = %0.3f | ML fit ~ b = %0.3f", 
          mrr, vrr, beta_hat) ,
        caption = sprintf("N individuals = %d | N bursts = %d | N patches = %d",
                          n_ind, n_brst, n_cls)
      )
    
    ggsave(
      file.path(fit_plots, sprintf("%s_ml_fit.png", gsub(" ", "_", sp))), 
      width = 10, height = 8
    )
    
    
    # If you want bootstrap CIs:
    # res_bs <- fit_ml_from_list(xi_pos, do_bootstrap = TRUE, n_boot = 200)
    # print(res_bs$beta_hat)
    # print(res_bs$ci_95)
    
    cat("DONE:", i, "|", length(files), "\n")
    
  })
  
}


# P: Mean revisits vs t---------------------------------------------------------

fin_name <- sprintf(
  "1_sleep_clusters_bursts_%d_window_%d_gap_all_cluster_revisits.rds", 
  min_wdw, max_gap)
files <- list.files(selected_dirs, pattern = fin_name, full.names = T)
nf <- length(files)

mean_dir <- file.path(plots_dir, "2_mean_revisits")
dir.create(mean_dir, showWarnings = F)

lapply(seq_along(files), function(n){
  
  fin <- files[n]
  sp <- gsub(".*/Studies/(.*)_(.*)/8_cluster_revisits/.*$", "\\1 \\2", fin)
  migrat <- traits_dt[birdlife_name == sp, migration_txt]
  
  dt <- fread(fin)

  mean_dt <- dt[visit_n > 0][, .(
    mean_revisits = sum(cum_revisits)/uniqueN(selected_cluster), 
    n_revisited = uniqueN(selected_cluster)
    ), by = visit_n]
  
  p1 <- mean_dt |>
    ggplot() +
    geom_point(
      aes(x = visit_n, y = mean_revisits), color = "#2574a9", size = 1
    ) + 
    scale_x_continuous(breaks = seq(1, max(mean_dt$visit_n), 100)) +
    labs(
      x = "day of revisit [tau]", 
      y = "mean number of revisits", 
      title = "Mean number of revisits vs. tau"
    ) +
    theme_bw()
  
  p2 <- mean_dt |> 
    ggplot() + 
    geom_point(
      aes(x = visit_n, y = n_revisited), color = "#5f0f40", size = 1
    ) + 
    scale_x_continuous(breaks = seq(1, max(mean_dt$visit_n), 100)) +
    labs(
      x = "day of revisit [tau]", 
      y = "number of revisited patches", 
      title = "Number of revisited patches vs. tau"
    ) +
    theme_bw()
  
  p1 + p2 +
    plot_annotation(
      title = sprintf("Evolution of revisits through time - %s", sp), 
      subtitle = paste("migration status:", migrat)
    )
  
  ggsave(
    file.path(
      mean_dir, sprintf("%s_revisits_vs_tau.png", gsub(" ", "_", sp))
    ), width = 10, height = 6
  )
  
  cat("DONE:", n, "|", length(files), "\n")
  
  
})


# Data Rosa share ---------------------------------------------------------

tau_selected <- fread(
  file.path(data_dir, "8_sleep_cluster_revisit_rate_varying_tau_step.csv"))
tau_selected <- tau_selected[selected == TRUE]

files <- unique(tau_selected$file)
  
rosa_dir <- file.path(data_dir, "For_Rosa")
dir.create(rosa_dir, showWarnings = F)
dir.create(file.path(rosa_dir, "Data_revisits"), showWarnings = F)
dir.create(file.path(rosa_dir, "Data_fits"), showWarnings = F)


allcls_file <- sprintf(
  "1_sleep_clusters_bursts_%d_window_%d_gap_all_cluster_revisits.rds", 
  min_wdw, max_gap)

lapply(seq_along(files), function(n){
  
  fin <- files[n]
  sp <- gsub(".*/Studies/(.*)/8_cluster_revisits/.*$", "\\1", fin)
  
  dt <- fread(fin)[, file := basename(file)]
  setnames(
    dt, 
    c("sleep_cluster_id", "visit_n", "selected_cluster"), 
    c("patch_id", "tau", "selected_patch")
  )
  
  fout <- paste0(sp, "_revisits_for_fit.csv")
  
  fwrite(dt, file.path(rosa_dir, "Data_fits", fout))
  
  dt <- fread(file.path(dirname(fin), allcls_file))[, file := basename(file)]
  
  setnames(
    dt, 
    c("sleep_cluster_id", "visit_n", "selected_cluster"), 
    c("patch_id", "tau", "selected_patch")
  )
  
  fout <- paste0(sp, "_all_revisits_min_7_dies.csv")
  
  fwrite(dt, file.path(rosa_dir, "Data_revisits", fout))
  
})


# Data summary ------------------------------------------------------------


fin_name <- sprintf(
  "1_sleep_clusters_bursts_%d_window_%d_gap_all_time_windows.rds", min_wdw, max_gap)
files <- list.files(selected_dirs, pattern = fin_name, full.names = T)

rosa_dir <- file.path(data_dir, "For_Rosa")
dir.create(rosa_dir, showWarnings = F)

lapply(seq_along(files), function(n){
  
  fin <- files[n]
  sp <- gsub(".*/Studies/(.*)/8_cluster_revisits/.*$", "\\1", fin)
  
  dt <- fread(fin)[, sleep_cluster_id := NULL]
  dt[, sample_start := as.Date(sample_start)]
  dt[, sample_end := as.Date(sample_end)]
  dt[, file := basename(file)]
  
  setnames(dt, "sleep_cluster", "patch_id")
  
  fout <- paste0(sp, "_totes_mostres_min_7_dies.csv")
  
  fwrite(dt, file.path(rosa_dir, fout))
  
})





# PLOT: burst length ------------------------------------------------------

fout <- file.path(data_dir, "8_summary_burst_length_min7_per_species.csv")

fin_name <- sprintf(
  "1_sleep_clusters_bursts_%d_window_%d_gap.rds", min_wdw, max_gap)
files <- list.files(revisit_dirs, pattern = fin_name, full.names = T)

burst_dir <- file.path(plots_dir, "3_burst_length")
dir.create(burst_dir, showWarnings = F)

if(!file.exists(fout)){
  
  burst_dt <- rbindlist(lapply(seq_along(files), function(n){
    
    fin <- files[n]
    sp <- gsub(".*/Studies/(.*)_(.*)/8_cluster_revisits/.*$", "\\1 \\2", fin)
    
    dcp_dt <- unique(fread(fin)[, .(file, burst, burst_length)])
    n_files <- uniqueN(dcp_dt$file)
    
    dcp_dt |> 
      ggplot() + 
      geom_histogram(
        aes(x = burst_length), 
        binwidth = 1, fill = "#caefd7", color = "gray33", linewidth = 0.2
      ) +
      scale_x_continuous(limits = c(1, max(dcp_dt$burst_length)+1)) +
      theme_bw() +
      labs(
        x = "burst length", 
        title = sprintf(
          "%s | N files = %d | N bursts = %d", sp, n_files, nrow(dcp_dt))
      )
    
    ggsave(
      file.path(burst_dir, sprintf("%s_burst_length.png", gsub(" ", "_", sp))), 
      width = 6, height = 4
    )
    
    dcp_dt[, species := sp]
    
  }))
  
  burst_dt <- burst_dt[, .(
    median_burst_length = as.numeric(median(burst_length)),
    mean_burst_length = as.numeric(mean(burst_length)),
    n_bursts = .N, 
    n_files = uniqueN(file)), 
    by = species]
  
  fwrite(burst_dt, fout)
  
}


