library(fitdistrplus)
library(data.table)
library(ggplot2)
library(patchwork)
library(dispfit)
library(purrr)

data_dir <- here::here("Data")
study_dir <- file.path(data_dir, "Studies")
graphs_dir <- file.path(data_dir, "Graphs")

plot_dir <- file.path(graphs_dir, "6_distance_fit")
dir.create(plot_dir, showWarnings = F)


files <- list.files(data_dir, pattern = "6_overview.*\\.csv", full.names = T)

dt_overview <- fread(
  list.files(
    data_dir, 
    pattern = ".*filter_30_max_active_steps.*continent.csv", 
    full.names = T)
  )
n_fltr <- 30

target_sp <- dt_overview[, species]


disp_fit_dir <- file.path(data_dir, "Disp_fits")
dir.create(disp_fit_dir, showWarnings = F)

# Fit dispersal -----------------------------------------------------------

fin_name <- "4_all_tracks_max_active_steps_nauticalDawn_nauticalDusk_continent.rds"

dist_dirs <- file.path(study_dir, gsub(" ", "_", target_sp), "6_distances")
files <- file.path(dist_dirs, fin_name)

files <- files[!file.exists(
  file.path(disp_fit_dir, paste0(gsub(" ", "_", target_sp), ".rds")))]

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
# 
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
    
    # filter tracks that have less than 10 steps
    dt <- dt[, n_steps := .N, by = file][n_steps >= n_fltr]
    
    if(nrow(dt) == 0){ return(NULL) }
    
    # extract sensor
    dt[, sensor := sub(".*dep_(.*?)_sen.*", "\\1", file)][
      , sensor := fcase(sensor == 653, "gps", sensor == 2299894820, "sigfox")]
    
    # get track lengths and sensor
    dt_per_file <- dt[, .(sensor = unique(sensor)), by = file]
    
    # remove duplicated trakcs - deployments with both gps and sigfox
    dt_per_file[, track_id := sub("_dep_\\d+.*$", "", basename(file))]
    
    setorder(dt_per_file, sensor)
    
    dt_per_file[, duplicated_track := duplicated(track_id)]
    dpltrks <- dt_per_file[duplicated_track == T, file]
    rm(dt_per_file)
    
    dt <- dt[!file %in% dpltrks][sl_ > 0]
    
    # run safely dispersal kernel function for each distribution separately
    # the output is a list with dist_funs as names
    kernels <- dist_funs |> 
      map(~safe_dispersal_kernel(dt$sl_, distribution = .x)) |> 
      set_names(dist_funs) 
    
    saveRDS(kernels, file.path(disp_fit_dir, paste0(sp, ".rds")))
    
    
  })
  
}




# Warning messages:
#   1: In ks.test.default(data, simul.rayleigh) :
#   p-value will be approximate in the presence of ties
# 2: In ks.test.default(data, simul.exponential) :
#   p-value will be approximate in the presence of ties
# 3: In ks.test.default(data, simul.generalnormal) :
#   p-value will be approximate in the presence of ties
# 4: In ks.test.default(data, simul.2dt) :
#   p-value will be approximate in the presence of ties
# 5: In ks.test.default(data, simul.geometric) :
#   p-value will be approximate in the presence of ties
# 6: In ks.test.default(data, simul.lognorm) :
#   p-value will be approximate in the presence of ties
# 7: In ks.test.default(data, simul.wald) :
#   p-value will be approximate in the presence of ties
# 8: In ks.test.default(data, simul.weibull) :
#   p-value will be approximate in the presence of ties
# 9: In ks.test.default(data, simul.gamma) :
#   p-value will be approximate in the presence of ties
# 10: In sqrt(diag(solve(numDeriv::hessian(logdistfun, x = init.pars$par,  ... :
# NaNs produced
# 11: In sqrt(diag(solve(numDeriv::hessian(logdistfun, x = init.pars$par,  ... :
# NaNs produced
# 12: In confint.dispfit(dist.opt, log.dist.2dt, data = data,  ... :
# twodt: Upper CI for 'a' is not accurate, I've given up after 10000 trials.
# 13: In confint.dispfit(dist.opt, log.dist.2dt, data = data,  ... :
#   twodt: Upper CI for 'b' is not accurate, I've given up after 10000 trials.
# 14: In confint.dispfit(dist.opt, log.dist.geometric, data = data,  ... :
# geometric: Upper CI for 'a' is not accurate, I've given up after 10000 trials.
# 15: In ks.test.default(data, simul.rayleigh) :
#   p-value will be approximate in the presence of ties
# 16: In ks.test.default(data, simul.exponential) :
#   p-value will be approximate in the presence of ties
# 17: In ks.test.default(data, simul.generalnormal) :
#   p-value will be approximate in the presence of ties
# 18: In ks.test.default(data, simul.2dt) :
#   p-value will be approximate in the presence of ties
# 19: In ks.test.default(data, simul.geometric) :
#   p-value will be approximate in the presence of ties
# 20: In ks.test.default(data, simul.lognorm) :
#   p-value will be approximate in the presence of ties
# 21: In ks.test.default(data, simul.wald) :
#   p-value will be approximate in the presence of ties
# 22: In ks.test.default(data, simul.weibull) :
#   p-value will be approximate in the presence of ties
# 23: In ks.test.default(data, simul.gamma) :
#   p-value will be approximate in the presence of ties
# 24: In sqrt(diag(solve(numDeriv::hessian(logdistfun, x = init.pars$par,  ... :
#   NaNs produced
# 25: In sqrt(diag(solve(numDeriv::hessian(logdistfun, x = init.pars$par,  ... :
#   NaNs produced
# 26: In optimize(function(par) fn(par, ...)/con$fnscale, lower = lower,  ... :
#   Inf replaced by maximum positive value
# 27: In ks.test.default(data, simul.rayleigh) :
#   p-value will be approximate in the presence of ties
# 28: In ks.test.default(data, simul.exponential) :
#   p-value will be approximate in the presence of ties
# 29: In ks.test.default(data, simul.generalnormal) :
#   p-value will be approximate in the presence of ties
# 30: In ks.test.default(data, simul.2dt) :
#   p-value will be approximate in the presence of ties
# 31: In ks.test.default(data, simul.geometric) :
#   p-value will be approximate in the presence of ties
# 32: In ks.test.default(data, simul.lognorm) :
#   p-value will be approximate in the presence of ties
# 33: In ks.test.default(data, simul.wald) :
#   p-value will be approximate in the presence of ties
# 34: In ks.test.default(data, simul.weibull) :
#   p-value will be approximate in the presence of ties
# 35: In ks.test.default(data, simul.gamma) :
#   p-value will be approximate in the presence of ties
# 36: In optimize(function(par) fn(par, ...)/con$fnscale, lower = lower,  ... :
#   Inf replaced by maximum positive value
# 37: In ks.test.default(data, simul.rayleigh) :
#   p-value will be approximate in the presence of ties
# 38: In ks.test.default(data, simul.exponential) :
#   p-value will be approximate in the presence of ties
# 39: In ks.test.default(data, simul.generalnormal) :
#   p-value will be approximate in the presence of ties
# 40: In ks.test.default(data, simul.2dt) :
#   p-value will be approximate in the presence of ties
# 41: In ks.test.default(data, simul.geometric) :
#   p-value will be approximate in the presence of ties
# 42: In ks.test.default(data, simul.lognorm) :
#   p-value will be approximate in the presence of ties
# 43: In ks.test.default(data, simul.wald) :
#   p-value will be approximate in the presence of ties
# 44: In ks.test.default(data, simul.weibull) :
#   p-value will be approximate in the presence of ties
# 45: In ks.test.default(data, simul.gamma) :
#   p-value will be approximate in the presence of ties
# 46: In sqrt(diag(solve(numDeriv::hessian(logdistfun, x = init.pars$par,  ... :
#   NaNs produced
# 47: In sqrt(diag(solve(numDeriv::hessian(logdistfun, x = init.pars$par,  ... :
#   NaNs produced
# 48: In confint.dispfit(dist.opt, log.dist.gamma, data = data,  ... :
#   gamma: Parameter 'a' likely diverged, skipping CI calculation
# 49: In sqrt(diag(new.covar)) : NaNs produced
# 50: In sqrt(diag(new.covar)) : NaNs produced

# 3 - All fits overview---------------------------------------------------------

fout <- file.path(data_dir, "6_distance_fit_overview.csv")

file_fits <- list.files(disp_fit_dir, full.names = T)

if(!file.exists(fout)){
  
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
  
  fwrite(fits_overview, file.path(data_dir, "6_distance_fit_overview.csv"))
  
}





# PLOT: fits ---------------------------------------------------------------


disp_plot <- file.path(disp_fit_dir, "Plots")
dir.create(disp_plot, showWarnings = F)


file_fits <- list.files(disp_fit_dir, pattern = ".rds", full.names = T)
files_out <- gsub(
  ".rds", ".png", gsub("Disp_fits", "Disp_fits/Plots", file_fits))

file_fits <- file_fits[!file.exists(files_out)]

lf <- length(file_fits)

# extract values from each fit
lapply(seq_along(file_fits), function(i){
  
  fin <- file_fits[i]
  sp <- gsub(".rds", "", basename(fin))
  
  dt <- fread(
    file.path(grep(sp, dist_dirs, value = T), fin_name))[, .(sl_, file)]
  
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
      aes(x = sl_, y = ..density..), 
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
  
  return(invisible(NULL))
  
  cat(i, " | ", lf)

  })


# PLOTS: potential distributions ------------------------------------------

# A non-zero skewness reveals a lack of symmetry of the empirical distribution, 
# while the kurtosis value quantifies the weight of tails in comparison to 
# the normal distribution for which the kurtosis equals 3. 

fin_name <- "4_all_tracks_max_active_steps_nauticalDawn_nauticalDusk_continent.rds"
files <- list.files(dist_dirs, pattern = fin_name, full.names = T)

filter30 <- file.path(plot_dir, "1_filter_30")
dir.create(filter30, showWarnings = F)

lapply(seq_along(files), function(n){
  
  fin <- files[n]
  sp <- gsub(".*Studies/(.*)_(.*)/6_distances/.*", "\\1 \\2", fin)
  
  dt <- fread(fin)
  
  dt <- dt[, n_steps := .N, by = file][n_steps >= 30]
  # to km
  dt[, sl_ := sl_/1000] 

  
  n_steps <- dt_overview[species == sp, N_steps]
  n_depl <- dt_overview[species == sp, N_deployments]
  shennon <- dt_overview[species == sp, Shennon_n_steps]
  
  cf <- descdist(dt$sl_, boot = 1000)
  
  pname <- paste0(gsub(" ", "_", sp), "_max_step_distribution_check.png")
  
  png(
    filename = file.path(filter30, pname),
    width = 15, height = 5, units = "in", res = 300
  )
  
  par(mfrow = c(1, 3)) # 1 row, 2 columns
  
  #plotdist(dt$sl_, histo = TRUE, demp = TRUE)
  
  plot(
    ecdf(dt$sl_), 
    xlab = "max daily distance [km]", 
    main = "Empirical CDF", 
    ylab = "ecdf(x)")

  
  # # Plot ggplot
  hist(
    dt$sl_, breaks = 100, xlab = "max daily distance [km]", main = "")
  
  # Plot descdist
  # 
  # By default, unbiased estimations of the three last statistics are provided.
  # Nevertheless, the argument method can be changed from "unbiased" (default) 
  # to "sample" to obtain them without correction for bias. 
  # 
  # For some distributions (normal, uniform, logistic, exponential),
  # there is only one possible value for the skewness and the kurtosis. 
  # Thus, the distribution is represented by a single point on the plot. 
  # 
  # For other distributions, areas of possible values are represented, 
  # consisting in lines (as for gamma and lognormal distributions), 
  # or larger areas (as for beta distribution).
  descdist(dt$sl_, boot = 1000)
  
  tit <- sprintf(
    "%s | N tracks = %d | N steps = %d | shennon: %.3f",
    sp, n_depl, n_steps, shennon)
  
  # Add a common main title
  mtext(tit, side = 3, line = -2, outer = TRUE, cex = 1.5)
  
  dev.off()
  
  
  
})










































