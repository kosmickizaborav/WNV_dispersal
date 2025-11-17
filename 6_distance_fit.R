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

dist_dirs <- file.path(list.files(study_dir, full.names = T), "6_distances")


# 1 - Fit dispersal -----------------------------------------------------------

files <- list.files(
  dist_dirs, pattern = "_fltr_20_dist_50m.rds", full.names = T)

files_out <- gsub(".rds", "_sl_median_fit.rds", files)
files <- files[!file.exists(files_out)]

lfl <- length(files)

# functions available 
dist_funs <- c(
  "rayleigh", 
  "exponential", 
  "general normal", 
  "2Dt", 
  "geometric", 
  "lognormal", 
  "wald", 
  "weibull",
  "gamma"
)

# # quantiles for cuttiong off of step lengths vector
# quantiles <- c(0.95, 0.99, 1)

# a safe version of the function
# if the error occurs the process is not stooped,
# but skipped and the error is saved
safe_dispersal_kernel <- safely(dispersal.kernel)

if(lfl > 0){
  
  lapply(seq_along(files), function(i){
    
    fin <- files[i]
    sp <- gsub(".*Studies/(.*)_(.*)/6_distances/.*", "\\1_\\2", fin)
    
    dt <- fread(fin)
    
    # run safely dispersal kernel function for each distribution separately
    # the output is a list with dist_funs as names
    kernels <- dist_funs |> 
      map(~safe_dispersal_kernel(dt$sl_median, distribution = .x)) |> 
      set_names(dist_funs) 
    
    
    fout <- gsub(".rds", "_sl_median_fit.rds", fin)
    
    saveRDS(kernels, fout)
    
    cat("\n", i, " | ", lfl, " - ", sp, " DONE!")
    
  })
  
}


# 3 - All fits overview---------------------------------------------------------

fout <- "6_distance_fit_overview_sl_median.csv"

if(!file.exists(file.path(data_dir, fout))){
  
  file_fits <- list.files(dist_dirs,  "_sl_median_fit.rds", full.names = T)
  
  # extract values from each fit
  fits_overview <- rbindlist(lapply(seq_along(file_fits), function(i){
    
    fin <- file_fits[i]
    
    kernels <- readRDS(fin)
    
    rbindlist(lapply(names(kernels), function(fun){
      
      result <- kernels[[fun]]$result
      
      # if there is no results, return a column that says error_occured
      if(is.null(result)){
        
        data.table(
          fitted_function = fun,
          error_occurred = T
        )
        
      } else {
        
        # if there is data extract values
        as.data.table(result$values)[
          , fitted_function := fun][
            , error_occurred := F]
      }
      
    }), fill = T)[
      , species := gsub("\\.rds$", "", basename(fin))]
    
  }), fill = T)
  
  
  # just renaming things
  fits_overview <- janitor::clean_names(fits_overview)
  setnames(
    fits_overview, 
    old = c("aic", "delta_aic", "ai_cc", "delta_ai_cc", "bic", "delta_bic"),
    new = c("AIC", "delta_AIC", "AICc", "delta_AICc", "BIC", "delta_BIC")
  )
  
  # looked in the source code of dist.kernel to see how this is obtained, 
  # ao the logic comes from there
  fits_overview[, ':=' (
    delta_AIC = AIC - min(AIC, na.rm = T),
    delta_BIC = BIC - min(BIC, na.rm = T), 
    delta_AICc = AICc - min(AICc, na.rm = T),
    wi = exp(-.5*delta_AICc)/sum(exp(-.5*delta_AICc), na.rm = T)
  ), by = species]
  
  setorder(fits_overview, species, delta_AIC)
  
  fwrite(fits_overview, file.path(data_dir, fout))
  
}





# PLOT: fits ---------------------------------------------------------------

fin_name <- "4_all_tracks_max_active_steps_nauticalDawn_nauticalDusk_continent.rds"

disp_plot <- file.path(fit_dir, "Plots_sl_median")
dir.create(disp_plot, showWarnings = F)


file_fits <- list.files(median_dir, pattern = ".rds", full.names = T)
files_out <- gsub(
  "_sl_median_fit.rds", ".png", 
  gsub("6_distance_fit/Sl_median", "6_distance_fit/Plots_sl_median", file_fits))

file_fits <- file_fits[!file.exists(files_out)]

lf <- length(file_fits)

if(lf > 0){
  
  
  # extract values from each fit
  lapply(seq_along(file_fits), function(i){
    
    fin <- file_fits[i]
    sp <- gsub("_sl_median_fit.rds", "", basename(fin))
    
    print(sp)
    
    dt <- fread(file.path(grep(sp, dist_dirs, value = T), fin_name))[
      , .(sl_median, file)]
    
    kernels <- readRDS(fin)
    
    kernels <- lapply(kernels, function(krn){krn$result})
    
    pred_dt <- rbindlist(lapply(names(kernels), function(nkrn){
      
      krn <- kernels[[nkrn]]
      
      if(!is.null(krn)){
        pred_out <- as.data.table(predict(krn)[[1]])
        names(pred_out)[2] <- "density"
        pred_out[, ':=' (
          fitted_function = nkrn,
          AIC = krn$values$AIC
        )]
      } else{
        return(NULL)
      }
      
    }))
    
    setorder(pred_dt, AIC)
    
    pred_dt[, fitted_function := factor(
      fitted_function, 
      levels = unique(pred_dt$fitted_function)
    )]
    
    pred_dt[, fit_names := paste(fitted_function, "\nAIC:", round(AIC))][
      , fit_names := factor(fit_names, levels = unique(pred_dt$fit_names))  ]
    
    ggplot() +
      # Histogram of observed data (density)
      geom_histogram(
        data = dt, 
        aes(x = sl_median, y = after_stat(density)), 
        bins = 100, fill = "grey80", color = "grey40", alpha = 0.6
      ) +
      geom_ribbon(
        data = pred_dt, 
        aes(x = distance, ymin = lwr, ymax = upr, fill = fitted_function), 
        alpha = 0.6, inherit.aes = FALSE
      ) +
      # Overlay all fitted curves
      geom_line(
        data = pred_dt, 
        aes(x = distance, y = density, color = fitted_function), 
        linewidth = 1
      ) +
      labs(
        x = "Daily distance [m]", 
        y = "Density", 
        title = sprintf("%s - fitted runctions vs. data", gsub("_", " ", sp)), 
        subtitle = "ordered by AIC value"
      ) +
      theme_minimal() +
      facet_wrap(~fit_names, scales = "free") +
      scale_color_brewer(palette = "Dark2") +
      scale_fill_brewer(palette = "Dark2") +
      theme(legend.position = "none")
    
    ggsave(
      filename = file.path(disp_plot, paste0(sp, ".png")),
      width = 10, height = 8, units = "in"
    )
    
    cat("\n", i, " | ", lf)
    

    
  })
  
}




# CDF Fede plots ---------------------------------------------------------------

file_fits <- list.files(file.path(fit_dir, "Sl_median"), full.names = T)

lf <- length(file_fits)

plots_cdf <- file.path(fit_dir, "Sl_median_CDF_plots")
dir.create(plots_cdf, showWarnings = F)

dir_cdf <- file.path(fit_dir, "Sl_median_CDF")
dir.create(dir_cdf, showWarnings = F)


lapply(seq_along(file_fits), function(i){
  
  
  fin <- file_fits[i]
  
  pname <- file.path(plots_cdf, gsub(".rds", ".png", basename(fin)))
  
  sp <- gsub("_sl_median_fit.rds", "", basename(fin))
  
  f <- lapply(readRDS(fin), function(x) x$result)
  
  # get only fits without errors
  selected_mods <- unlist(lapply(names(f), function(mname){
    if(!is.null(f[[mname]])) {return(mname)} else {return(NULL)}
  }))
  
  #SELECTING MODEL: criteria (WE SHOULD CHECK dAIC below 2)??
  # Find which models have been fitted and take out exp, rayleigh, 
  # and wald (very unfrequent with dmax)
  # subset_fitmodels <- fits_dt[
  #   species == gsub(".rds", "", basename(fin)), fitted_function]
  # subset_fitmodels <- setdiff(subset_fitmodels,c("exponential","rayleigh","wald"))
  # models_to_use <- intersect(names(f), subset_fitmodels)
  
  
  cdf_dt <- rbindlist(lapply(names(f), function(mname){
    
    krnl <- f[[mname]]
    
    data <- sort(krnl$data)
    
    a <- krnl[["distribution.parameters"]][["Parameter 1"]]
    a_low <- krnl[["distribution.parameters"]][["Parameter 1 lower CI"]]
    a_up <- krnl[["distribution.parameters"]][["Parameter 1 upper CI"]]
    
    b <- krnl[["distribution.parameters"]][["Parameter 2"]]
    b_low <- krnl[["distribution.parameters"]][["Parameter 2 lower CI"]]
    b_up <- krnl[["distribution.parameters"]][["Parameter 2 upper CI"]]
    
    data.table(
      cdf = cdf_function(mname)(
        pdf_fun = pdf_function(mname), r = data, a = a, b = b),
      cdf_lower = cdf_function(mname)(
        pdf_fun = pdf_function(mname), r = data, a = a_low, b = b_low),
      cdf_upper = cdf_function(mname)(
        pdf_fun = pdf_function(mname), r = data, a = a_up, b = b_up),
      data = data, 
      fitted_function = mname
    )
    
    
  }), fill = T)
  
  
  cdf_vars <- c("cdf", "cdf_lower", "cdf_upper")
  
  cdf_dt[, (cdf_vars) := lapply(.SD, function(x) {
    bad <- !is.na(x) & (x < 0 | x > 1 | (1 - x) <= 0)
    # replace bad entries with NA_real_, keep others
    replace(x, bad, NA_real_)
  }), .SDcols = cdf_vars]
  
  setorder(cdf_dt, fitted_function, data)
  # Compute exceedance probability and its log10
  cdf_dt[, Pexc := .N:1 / .N, by = fitted_function]     
  cdf_dt[, xx := log10(data)] 
  cdf_dt[, yy := log10(Pexc)]
  
  # compute model log-exceedance and then RRMSE
  cdf_dt[, y_model := log10(1 - cdf)] 
  
  cdf_dt[, resid := yy - y_model]
  cdf_dt[, yy_diff := diff(range(yy, na.rm = T))]
  
  # compared with Fede's code and gives the same output
  cdf_dt[, RRMSE := sqrt(mean((yy - y_model)^2, na.rm = TRUE)) / yy_diff, 
         by = fitted_function]
  
  cdf_dt[, leg_txt := paste0(
    fitted_function, " (RRMSE = ", round(RRMSE, 3), ")")]
  
  cdf_dt |> 
    ggplot() +
    geom_point(
      aes(x = xx, y = yy), color = "black", size = 1, alpha = 0.5
    ) + 
    geom_line(
      aes(x = xx, y = log10(1 - cdf), color = leg_txt), linewidth = 1, alpha = 0.5
    ) +
    theme_bw() + 
    labs(
      x = "log10(r)", 
      y = "log10(P(R>=r))", 
      title = sprintf("%s: CDF (exceedance)", gsub("_", " ", sp)),
      color = "Fitted model (RRMSE)"
    ) + 
    theme(
      plot.title = element_text(size = 14, face = "bold")
    )
  
  ggsave(
    filename = pname, width = 10, height = 8, units = "in"
  )
  
  fwrite(cdf_dt, file = file.path(dir_cdf, basename(fin)))
  
  cat(i, " / ", lf, "\n")
  
})








































