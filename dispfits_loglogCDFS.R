#F.Bartumeus 3-11-2025

# --- Load required libraries ---
library(stats)
library(pracma)  # for numeric integration
# using library ehaGoF for goodness of fit
library(ehaGoF)

library(dplyr)
library(tidyr)
library(readr)

# --------------------------
# PDF definitions---taken from dispfit package
# --------------------------
# --------------------------
# PDF definitions (b=NULL by default)
# --------------------------

pdf_rayleigh <- function(r, a, b = NULL) {
  ifelse(r >= 0, 1 / (pi * a^2) * exp(-(r / a)^2), 0)
}

pdf_exponential <- function(r, a, b = NULL) {
  ifelse(r >= 0, 1 / (2 * pi * a^2) * exp(-r / a), 0)
}

pdf_generalized_normal <- function(r, a, b) {
  ifelse(r >= 0, b / (2 * pi * a^2 * gamma(2 / b)) * exp(-(r / a)^b), 0)
}

pdf_2dt <- function(r, a, b) {
  ifelse(r >= 0, (b - 1) / (pi * a^2) * (1 + (r / a)^2)^(-b), 0)
}

pdf_geometric <- function(r, a, b) {
  ifelse(r >= 0, ((b - 2) * (b - 1)) / (2 * pi * a^2) * (1 + r / a)^(-b), 0)
}

pdf_lognormal <- function(r, a, b) {
  mu <- log(a)
  s <- b
  out <- rep(0, length(r))
  pos <- r > 0
  out[pos] <- (1 / (r[pos] * s * sqrt(2 * pi))) * exp(- (log(r[pos]) - mu)^2 / (2 * s^2))
  out
}

pdf_wald <- function(r, a, b) {
  pos <- r > 0
  out <- rep(0, length(r))
  out[pos] <- sqrt(b) / sqrt(8 * pi * r[pos]^5) * exp(- (b * (r[pos] - a)^2) / (2 * a^2 * r[pos]))
  out
}

#shape=b; scale=a
pdf_weibull <- function(r, a, b) {
  ifelse(r >= 0, b / (2 * pi * a^b) * r^(b - 2) * exp(-(r / a)^b), 0)
}

pdf_weibull_radial <- function(r, a, b) {
  ifelse(r >= 0, 2*pi*r*(b / (2 * pi * a^b) * r^(b - 2) * exp(-(r / a)^b)), 0)
}


pdf_gamma <- function(r, a, b) {
  ifelse(r >= 0, 1 / (2 * pi * a^2 * gamma(b)) * (r / a)^(b - 2) * exp(-r / a), 0)
}

#######################################################################################
#CDF DEFINITIONS
########################################################################################

# --------------------------
# Numeric CDF (general)
# --------------------------
cdf_numeric <- function(pdf_func, r_vals, a, b = NULL) {
  sapply(r_vals, function(r) {
    if (r <= 0) return(0)
    integrate(function(t) pdf_func(t, a, b), 0, r)$value
  })
}

# -----------------------------------
# Numeric CDF (2D expressed radially) *****
# -----------------------------------
cdf_numeric_radial <- function(pdf_func, r_vals, a, b = NULL) {
  sapply(r_vals, function(r) {
    if (r <= 0) return(0)
    integrate(function(t) 2 * pi * t * pdf_func(t, a, b), 0, r)$value
  })
}

# --------------------------
# Analytical CDFs (when available... but not used in this code)
# --------------------------
cdf_rayleigh_analytic <- function(r, a) { 1 - exp(-(r / a)^2) } # standard version
cdf_exponential_analytic <- function(r, a) { 1 - exp(-r / a) }
cdf_lognormal_analytic <- function(r, a, b) { plnorm(r, meanlog = log(a), sdlog = b) }
cdf_gamma_analytic <- function(r, a, b) { pgamma(r, shape = b-1, scale = a) }
cdf_weibull_analytic <- function(r, a, b) { pweibull(r, shape = b-1, scale = a) }
cdf_2dt_analytic <- function(r, a, b) {1 - (1 + (r / a)^2)^(1 - b)}


###########################################################################
##DATA PROCESSING SPECIES AND MODELS SELECTION
###########################################################################

df<-read.csv("/home/fbartu/Research/Nina_Bogdanovic/Dispersal_Kernels/2026/6_distance_fit_overview.csv",header=TRUE)
error_table <- df %>% select(species,fitted_function,error_occurred) 
# View the resulting table
#print(error_table)

folder_path <- "~/Research/Nina_Bogdanovic/Dispersal_Kernels/2026/Disp_fits"
rds_files <- list.files(folder_path, pattern = "\\.rds$", full.names = TRUE)

# --- Loop over each file ---
for (file_path in rds_files) {
  
  # Extract species name from file name
  species_name <- tools::file_path_sans_ext(basename(file_path))
  cat("\nProcessing species:", species_name, "\n")
 
  ################################################################# 
  #WITHOUT THE LOOP: TO ANALIZE ONE SPECIES ALONE, PICKED FROM rds_files  
  #file_path<-rds_files[143]
  #species_name <- tools::file_path_sans_ext(basename(file_path))
  #cat("\nProcessing species:", species_name, "\n")
  ##################################################################
  
  # ---Read the fitted models ---
  f <- readRDS(file_path)
 
#SELECTING SPECIES  
single_species <- error_table %>% filter(species == species_name)

#SELECTING MODEL: criteria (WE SHOULD CHECK dAIC below 2)??
# Find which models have been fitted and take out exp, rayleigh, and wald (very unfrequent with dmax)
subset_fitmodels <- single_species$fitted_function[which(!single_species$error_occurred)]
subset_fitmodels<-setdiff(subset_fitmodels,c("exponential","rayleigh","wald"))
models_to_use <- intersect(names(f), subset_fitmodels)

#################################################################################3

# Build the params list dynamically, keeping model names
params <- setNames(
  lapply(models_to_use, function(mod) {
    list(
      a = f[[mod]]$result$distribution.parameters[1, 1],
      b = f[[mod]]$result$distribution.parameters[1, 4]
    )
  }),
  models_to_use
)

pdf_list <- list(
  `general normal` = pdf_generalized_normal,
  `2Dt`= pdf_2dt,
  geometric = pdf_geometric,
  lognormal = pdf_lognormal,
  weibull = pdf_weibull,
  gamma = pdf_gamma
)

radial_cdfs <- c("general normal", "2Dt", "geometric", "weibull", "gamma") # adjust if needed
cdf_func_list <- lapply(models_to_use, function(mod) {
  if (mod %in% radial_cdfs) return(cdf_numeric_radial) 
  else return(cdf_numeric)  # e.g., lognormal
})
names(cdf_func_list) <- models_to_use


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


####################################################################################################
#PLOTING DYNAMICALLY subset models

library(RColorBrewer)

# --- 1️ Prepare exceedance probability ---
x <- sort(cdf_table$data)
N <- length(x)
Pexc <- sapply(x, function(xk) length(which(x >= xk)) / N)
xx <- log10(x)
yy <- log10(Pexc)

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

###### PLOTTING BASED ON HISTOGRAM AND PDF #####################

# --- 1 Prepare histogram ---
hist(data, probability = TRUE, breaks = 100, border = NA,
     xlab = "r", main = paste0(species_name, ": Histogram of sampled r with theoretical PDFs"))

# --- 2 Prepare grid for PDFs ---
r_grid <- seq(0, max(data) * 0.99, length.out = 1000)

# --- 3️ Colors ---
n_models <- length(models_to_use)
colors <- brewer.pal(min(8, n_models), "Set1")
names(colors) <- models_to_use

# --- 4 Define which PDFs need radial correction ---
radial_corrected <- c("general normal", "2Dt", "geometric", "weibull", "gamma")

# --- 5 Overlay PDFs dynamically ---
for (i in seq_along(models_to_use)) {
  mod <- models_to_use[i]
  
  # Get PDF function
  
    pdf_fun <- switch(mod,
              `general normal` = pdf_generalized_normal,
              `2Dt`= pdf_2dt,
               geometric = pdf_geometric,
               lognormal = pdf_lognormal,
               weibull = pdf_weibull,
               gamma = pdf_gamma)
  
    
  # Get parameters
  a <- params[[mod]]$a
  b <- params[[mod]]$b
  
  # Apply radial correction if needed
  y_vals <- if (mod %in% radial_corrected) {
    2 * pi * r_grid * pdf_fun(r_grid, a = a, b = b)
  } else {
    pdf_fun(r_grid, a = a, b = b)
  }
  
  lines(r_grid, y_vals, col = colors[mod], lwd = 2, lty = 2)
}

# --- 6 Optional: Add RRMSE to legend ---
legend_labels <- paste0(models_to_use, 
                        if(exists("rrmse_results")) paste0(" (RRMSE=", round(rrmse_results[models_to_use], 3), ")") else "")
legend("topright", legend = legend_labels, col = colors[models_to_use], lwd = 2, lty = 2)


readline(prompt = "Press [Enter] to continue to next species...")

} #if loop is used!


################## OLD, NON-DYNAMIC CODE #######################333

# Extracting the parameters for each distribution
#params <- list(
#  generalized_normal = list(a= f$`general normal`$result$distribution.parameters[1,1], b = f$`general normal`$result$distribution.parameters[1,4]),3  tStudbin = list(a = f$`2Dt`$result$distribution.parameters[1,1], b = f$`2Dt`$result$distribution.parameters[1,4]),
#  geometric = list(a = f$geometric$result$distribution.parameters[1,1], b = f$geometric$result$distribution.parameters[1,4]),
#  lognormal = list(a = f$lognormal$result$distribution.parameters[1,1], b = f$lognormal$result$distribution.parameters[1,4]),
#  weibull = list(a = f$weibull$result$distribution.parameters[1,1], b = f$weibull$result$distribution.parameters[1,4]),
#  gamma = list(a = f$gamma$result$distribution.parameters[1,1], b = f$gamma$result$distribution.parameters[1,4])
#)

#data=sort(f$lognormal$result$data)
# Compute all CDFs numerically
#cdf_table <- data.frame(r = data)
#cdf_table$data <-data
#cdf_table$generalized_normal_cdf <- cdf_numeric_radial(pdf_generalized_normal,data,a=params$`general normal`$a, b =params$`general normal`$b)
#cdf_table$tStudbin_cdf <- cdf_numeric_radial(pdf_2dt, data, a = params$`2Dt`$a, b =params$`2Dt`$b)
#cdf_table$geometric_cdf <- cdf_numeric_radial(pdf_geometric, data, a = params$geometric$a, b =params$geometric$b)
#for lognormal we dont need the radial correction
#cdf_table$lognormal_cdf <- cdf_numeric(pdf_lognormal, data, a =params$lognormal$a, b =params$lognormal$b)
#cdf_table$weibull_cdf <- cdf_numeric_radial(pdf_weibull,data,a=params$weibull$a, b =params$weibull$b)
#cdf_table$gamma_cdf <- cdf_numeric_radial(pdf_gamma, data, a=params$gamma$a, b =params$gamma$b)



#PLOTING CDFs in log-log scales
#######################################################
# x=sort(cdf_table$data,decreasing=FALSE)
# N=length(x)
# Pexc=vector(mode="numeric",length(x))   
# for (k in 1:length(x))
# {
#   Pexc[k]= length(which(x>=x[k]))/N #excedence probability
# }
# xx=log10(x)
# yy=log10(Pexc)
# plot(xx,yy)
# 
# pgenorm_exc=log10(1-cdf_table$generalized_normal_cdf)
# ptStudbin_exc=log10(1-cdf_table$tStudbin_cdf)
# pgeom_exc=log10(1-cdf_table$geometric_cdf)
# plogn_exc=log10(1-cdf_table$lognormal_cdf)
# pweib_exc=log10(1-cdf_table$weibull_cdf)
# pgamma_exc=log10(1-cdf_table$gamma_cdf)
# 
# lines(xx,pgenorm_exc, type = "l", col = "blue", lwd = 2)
# lines(xx,ptStudbin_exc, type = "l", col = "red", lwd = 2)
# lines(xx,pgeom_exc, type = "l", col = "green", lwd = 2)
# lines(xx,plogn_exc, type = "l", col = "orange", lwd = 2)
# lines(xx,pweib_exc, type = "l", col = "cyan", lwd = 2)
# lines(xx,pgamma_exc, type = "l", col = "black", lwd = 2)
# 
# legend("topright", legend=c("genorm", "tStudbin", "geom", "logN","weibull","gamma"), col=c("blue","red","green","orange","cyan","black"), lwd=2)
# 
# # Goodness of fit - relative root mean square error (RRMSE) on log-log cdfs
# #GoF(xx,pgenorm_exc)
# 
# gofRRMSE(xx,pgenorm_exc)
# gofRRMSE(xx,ptSudbin_exc)
# gofRRMSE(xx,pgeom_exc)
# gofRRMSE(xx,pweib_exc)
# gofRRMSE(xx,pgamma_exc)


#########################################################
# 
# #PLOTTING PDF WITH HISTOGRAM
# # Histogram of R (distance samples)
# hist(data, probability = TRUE, breaks = 100, border = NA,
#      xlab = "r", main = "Histogram of sampled r with theoretical PDFs")
# # Grid for plotting
# r_grid <- seq(0, max(samples)*0.99, length.out = 1000)
# # If you want to show the area PDF transformed to radial scale:
# # radial_from_area = 2*pi*r * pdf_area(r)
# lines(r_grid, 2 * pi * r_grid * pdf_generalized_normal(r_grid,a=params$generalized_normal$a, b =params$generalized_normal$b), col = "blue", lwd = 2, lty = 2)
# lines(r_grid, 2 * pi * r_grid * pdf_2dt(r_grid,a = params$tStudbin$a, b =params$tStudbin$b), col = "red", lwd = 2, lty = 2)
# lines(r_grid, 2 * pi * r_grid * pdf_geometric(r_grid, a = params$geometric$a, b =params$geometric$b), col = "green", lwd = 2, lty = 2)
# #for lognormal we dont need the radial correction
# lines(r_grid, pdf_lognormal (r_grid, a =params$lognormal$a, b =params$lognormal$b), col = "orange", lwd = 2, lty = 2)
# lines(r_grid, 2 * pi * r_grid * pdf_weibull (r_grid,a=params$weibull$a, b =params$weibull$b), col = "cyan", lwd = 2, lty = 2)
# lines(r_grid, 2 * pi * r_grid * pdf_gamma (r_grid, a=params$gamma$a, b =params$gamma$b), col = "black", lwd = 2, lty = 2)
# 
# legend("topright", legend=c("genorm", "tStudbin", "geom", "logN","weibull","gamma"), col=c("blue","red","green","orange","cyan","black"), lwd=2)
# #########################################################




