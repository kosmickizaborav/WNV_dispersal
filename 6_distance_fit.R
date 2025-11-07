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


files <- list.files(data_dir, pattern = "6_overview.*\\.csv", full.names = T)

dt_overview <- fread(
  list.files(
    data_dir, 
    pattern = ".*filter_30_max_active_steps.*continent.csv", 
    full.names = T)
  )
n_fltr <- 30

target_sp <- dt_overview[, species]


fit_dir <- file.path(data_dir, "6_distance_fit")
dir.create(fit_dir, showWarnings = F)

# 1 - Fit dispersal -----------------------------------------------------------

median_dir <- file.path(fit_dir, "Sl_median")
dir.create(median_dir, showWarnings = F)

fin_name <- "4_all_tracks_max_active_steps_nauticalDawn_nauticalDusk_continent.rds"

dist_dirs <- file.path(study_dir, gsub(" ", "_", target_sp), "6_distances")
files <- file.path(dist_dirs, fin_name)
files_out <- file.path(
  median_dir, paste0(gsub(" ", "_", target_sp), "_sl_median_fit.rds"))
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
    
    # filter tracks that have less than 10 steps
    dt <- dt[, n_steps := .N, by = file][n_steps >= n_fltr]
    
    if(nrow(dt) == 0){ return(NULL) }
    
    setnames(dt, old = "sl_median", new = "sl_median_old")
    # when there was only one distance available no stats was calculated
    dt[, sl_median := fifelse(
      sl_n_steps == 1, as.numeric(sl_), as.numeric(sl_median_old))] 
    
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
    
    dt <- dt[!file %in% dpltrks][sl_median > 0]
    
    # run safely dispersal kernel function for each distribution separately
    # the output is a list with dist_funs as names
    kernels <- dist_funs |> 
      map(~safe_dispersal_kernel(dt$sl_median, distribution = .x)) |> 
      set_names(dist_funs) 
    
    saveRDS(kernels, file.path(median_dir, paste0(sp, "_sl_median_fit.rds")))
    
    
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

fout <- "6_distance_fit_overview_sl_median.csv"

file_fits <- list.files(median_dir, full.names = T)

if(!file.exists(file.path(data_dir, fout))){
  
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

fin_name <- "4_all_tracks_max_active_steps_nauticalDawn_nauticalDusk_continent.rds"

fits_dt <- fread(file.path(data_dir, "6_distance_fit_overview_sl_median.csv"))
#fits_dt <- fits_dt[error_occurred == F][delta_AIC < 2]

file_fits <- list.files(file.path(fit_dir, "Sl_median"), full.names = T)

lf <- length(file_fits)

fin <- file_fits[1]
sp <- gsub("_sl_median_fit.rds", "", basename(fin))


# get only fits without errors
f <- lapply(readRDS(fin), function(x){
  
  if(is.null(x$result)) { return(NULL)}
  x$result
  
  })

#SELECTING MODEL: criteria (WE SHOULD CHECK dAIC below 2)??
# Find which models have been fitted and take out exp, rayleigh, and wald (very unfrequent with dmax)
subset_fitmodels <- fits_dt[
  species == gsub(".rds", "", basename(fin)), fitted_function]
subset_fitmodels <- setdiff(subset_fitmodels,c("exponential","rayleigh","wald"))
models_to_use <- intersect(names(f), subset_fitmodels)


r_dt <- rbindlist(lapply(names(f), function(mname){
  
  krnl <- f[[mname]]
  
  data <- krnl$data
  
  a <- krnl[["distribution.parameters"]][["Parameter 1"]]
  a_low <- krnl[["distribution.parameters"]][["Parameter 1 lower CI"]]
  a_up <- krnl[["distribution.parameters"]][["Parameter 1 upper CI"]]
  
  b <- krnl[["distribution.parameters"]][["Parameter 2"]]
  b_low <- krnl[["distribution.parameters"]][["Parameter 2 lower CI"]]
  b_up <- krnl[["distribution.parameters"]][["Parameter 2 upper CI"]]
  
  
  data.table(
    cdf = cdf_function_fede(mname)(
      pdf_fun = pdf_function_fede(mname), r_vals = data, a = a, b = b),
    cdf_lower = cdf_function_fede(mname)(
      pdf_fun = pdf_function_fede(mname), r_vals = data, a = a_low, b = b_low),
    cdf_upper = cdf_function_fede(mname)(
      pdf_fun = pdf_function_fede(mname), r_vals = data, a = a_up, b = b_up), 
    data = data
  )[, fitted_function := mname]
  
  
}))

# --- 2 Prepare colors dynamically ---
n_models <- length(models_to_use)
colors <- brewer.pal(min(8, n_models), "Set1")  # distinct colors
names(colors) <- models_to_use

# --- 3 Compute RRMSE for each model ---
gofRRMSE <- function(x, y_model) {
  y_empirical <- log10(sapply(x, function(xk) length(which(10^x >= 10^xk)) / length(x)))
  sqrt(mean((y_empirical - y_model)^2)) / (max(y_empirical) - min(y_empirical))
}

rrmse_results <- sapply(models_to_use, function(mod) {
  cdf_vals <- cdf_table[[paste0(mod, "_cdf")]]
  y_model <- log10(1 - cdf_vals)
  gofRRMSE(xx, y_model)
})

cdf_table <- r_dt[fitted_function == fitted_function[1]]

# --- 1️ Prepare exceedance probability ---
x <- sort(cdf_table$data)
N <- length(x)
Pexc <- sapply(x, function(xk) length(which(x >= xk)) / N)
xx <- log10(x)
yy <- log10(Pexc)


setorder(r_dt, fitted_function, data)
# Compute exceedance probability and its log10
fits_dt[, Pexc := rev(seq_len(.N)) / .N, by = fitted_function]     
fit_dt[, xx := log10(sl_median), by = fitted_function]
fits_dt[, yy := log10(Pexc), by = fitted_function]

# --- 3 Compute RRMSE for each model ---
gofRRMSE <- function(x, y_model) {
  y_empirical <- log10(sapply(x, function(xk) length(which(10^x >= 10^xk)) / length(x)))
  sqrt(mean((y_empirical - y_model)^2)) / (max(y_empirical) - min(y_empirical))
}



rrmse_results <- sapply(models_to_use, function(mod) {
  cdf_vals <- cdf_table[[paste0(mod, "_cdf")]]
  y_model <- log10(1 - cdf_vals)
  gofRRMSE(xx, y_model)
})



ggplot(r_dt) +
  geom_line(
    aes(x = data, y = cdf, color = fitted_function), size = 1
  ) +
  geom_ribbon(
    aes(x = data, ymin = cdf_lower, ymax = cdf_upper, fill = fitted_function), 
    alpha = 0.3
  ) 

data <- sort(f$lognormal$result$data)
cdf_table <- data.frame(r = data, data = data)
# --- Step 4: Loop over models_to_use and compute CDFs dynamically ---
for(mod in models_to_use) {
  pdf_fun <- pdf_list[[mod]]
  cdf_fun <- cdf_func_list[[mod]]
  a <- params[[mod]]$a
  b <- params[[mod]]$b
  cdf_table[[paste0(mod, "_cdf")]] <- cdf_fun(pdf_fun, data, a = a, b = b)
}



# PLOTING DYNAMICALLY subset models

library(RColorBrewer)

# --- 1️ Prepare exceedance probability ---
# 
x <- sort(dt$sl_median)
N <- length(x)
Pexc <- sapply(x, function(xk) length(which(x >= xk)) / N)
xx <- log10(x)
yy <- log10(Pexc)

# --- 2 Prepare colors dynamically ---
# n_models <- length(models_to_use)
# colors <- brewer.pal(min(8, n_models), "Set1")  # distinct colors
# names(colors) <- models_to_use

# --- 3 Compute RRMSE for each model ---
gofRRMSE <- function(x, y_model) {
  y_empirical <- log10(sapply(x, function(xk) length(which(10^x >= 10^xk)) / length(x)))
  sqrt(mean((y_empirical - y_model)^2)) / (max(y_empirical) - min(y_empirical))
}

rrmse_results <- sapply(models_to_use, function(mod) {
  cdf_vals <- cdf_table[[paste0(mod, "_cdf")]]
  y_model <- log10(1 - cdf_vals)
  gofRRMSE(xx, y_model)
})

# --- 4 Prepare legend labels with RRMSE ---
legend_labels <- paste0(models_to_use, " (RRMSE=", round(rrmse_results, 3), ")")

# --- 5 Plot log-log exceedance probability dynamically ---
plot(xx, yy, type = "p", lwd = 2, col = "black",
     xlab = "log10(r)", ylab = "log10(P(R>=r))",
     main = paste0(species_name, ": CDF(exceedance)"))

for (i in seq_along(models_to_use)) {
  mod <- models_to_use[i]
  cdf_vals <- cdf_table[[paste0(mod, "_cdf")]]
  lines(xx, log10(1 - cdf_vals), col = colors[mod], lwd = 2)
}

legend("bottomleft", legend = legend_labels, col = colors[models_to_use], lwd = 2)


readline(prompt = "Press [Enter] to histogram plot...")



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










































