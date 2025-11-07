# CDF plots Fede ---------------------------------------------------------------




# PDF definition - Fede ---------------------------------------------------


pdf_function_fede <- function(mname){
  
  
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
  
  
  # Use switch to return the appropriate PDF function
  switch(
    mname,
    rayleigh = pdf_rayleigh,
    exponential = pdf_exponential,
    "general normal" = pdf_generalized_normal,
    "2Dt" = pdf_2dt,
    geometric = pdf_geometric,
    lognormal = pdf_lognormal,
    wald = pdf_wald,
    weibull = pdf_weibull,
    weibull_radial = pdf_weibull_radial,
    gamma = pdf_gamma,
    stop("Unknown model name!")
  )
  
}

# CDF DEFINITIONS---------------------------------------------------------------


cdf_function_fede <- function(mname, ...){
  
  # Numeric CDF (general)
  cdf_numeric <- function(pdf_func, r_vals, a, b = NULL) {
    sapply(r_vals, function(r) {
      if (r <= 0) return(0)
      integrate(function(t) pdf_func(t, a, b), 0, r)$value
    })
  }
  
  # Numeric CDF (2D expressed radially)
  cdf_numeric_radial <- function(pdf_func, r_vals, a, b = NULL) {
    sapply(r_vals, function(r) {
      if (r <= 0) return(0)
      integrate(function(t) 2 * pi * t * pdf_func(t, a, b), 0, r)$value
    })
  }
  
  # adjust if needed
  radial_cdfs <- c("general normal", "2Dt", "geometric", "weibull", "gamma") 
  
  if(mname %in% radial_cdfs){
    fn <- cdf_numeric_radial
  } else {
    fn <- cdf_numeric
  }
  
  function(...) fn(...)
  # cdf_func_list <- lapply(models_to_use, function(mod) {
  #   if (mod %in% radial_cdfs) return(cdf_numeric_radial) 
  #   else return(cdf_numeric)  # e.g., lognormal
  # })
  # names(cdf_func_list) <- models_to_use
  
  
  # # Analytical CDFs (when available... but not used in this code)
  # cdf_rayleigh_analytic <- function(r, a, b = NULL) { 1 - exp(-(r / a)^2) } # standard version
  # cdf_exponential_analytic <- function(r, a, b = NULL) { 1 - exp(-r / a) }
  # cdf_lognormal_analytic <- function(r, a, b) { plnorm(r, meanlog = log(a), sdlog = b) }
  # cdf_gamma_analytic <- function(r, a, b) { pgamma(r, shape = b-1, scale = a) }
  # cdf_weibull_analytic <- function(r, a, b) { pweibull(r, shape = b-1, scale = a) }
  # cdf_2dt_analytic <- function(r, a, b) {1 - (1 + (r / a)^2)^(1 - b)}
  
}



# # PDF definitions---taken from dispfit package----------------------------------
# 
# pdf_function <- function(mname){
#   
#   dist.rayleigh <- function (r, a) {
#     fg <- 2*pi*r*(1/(pi*a^2)) * exp(-r^2/a^2)
#   }
#   
#   dist.exponential <- function (r, a) {
#     2*pi*r*(1 / (2 * pi * a ^ 2 )) * exp(-r/a) 
#     # corrected function, adapted from Nathan 2012
#   }
#   
#   dist.generalnormal <- function (r, a, b) {
#     fgeneralnormal <- 2*pi*r*(b / (2 * pi * (a^2) * gamma(2 / b))) * exp(-(r / a) ^ b)
#   }
#   
#   dist.2dt <- function (r, a, b) {
#     f2dt <- 2*pi*r*((b-1) / (pi*(a^2))) * ((1 + (r^2)/(a^2))^(-b))
#   }
#   
#   dist.geometric <- function (r, a, b) {
#     2*pi*r*(((b - 2) * (b - 1)) / (2 * pi * (a^2))) * ((1 + (r / a)) ^ -b)
#   }
#   
#   dist.lognorm <- function (r, a, b) {
#     2*pi*r * (1 / (((2 * pi) ^ (3/2)) * (b * (r ^ 2)))) * exp(-(log(r / a)^2) / (2 * (b ^ 2)))
#   }
#   
#   dist.wald <- function (r, a, b) {
#     fwald <- 2*pi*r * (sqrt(b)/sqrt(8 * (pi^3) * (r^5))) * exp(-(b * ((r - a)^2))/(2 * (a^2) * r))
#   }
#   
#   dist.weibull <- function (r, a, b) {
#     fw <- 2*pi*r * (b/(2*pi*a^b)) * (r^(b-2)) * exp(-(r^b/a^b)) 
#     ## function from Austerlitz 2004
#   }
#   
#   dist.gamma <- function (r, a, b) {
#     fgamma <- 2*pi*r * (1 / (2 * pi * (a^2) * gamma(b))) * ((r/a)^(b-2)) * exp(-r/a)
#   }
#   
#   # Use switch to return the appropriate PDF function
#   switch(
#     mname,
#     rayleigh = dist.rayleigh,
#     exponential = dist.exponential,
#     "general normal" = dist.generalnormal,
#     "2Dt" = dist.2dt,
#     geometric = dist.geometric,
#     lognormal = dist.lognorm,
#     wald = dist.wald,
#     weibull = dist.weibull,
#     gamma = dist.gamma,
#     stop("Unknown model name!")
#   )
#   
# }
# 
# 

# # CDF function (calls pdf_function() for numeric integration)
# cdf_function <- function(mname) {
#   
#   # --- Analytic CDFs --- #
#   dist.rayleigh.cdf <- function(r, a) 1 - exp(-r^2 / a^2)
#   dist.exponential.cdf <- function(r, a) 1 - exp(-r / a) * (1 + r / a)
#   dist.2dt.cdf <- function(r, a, b) 1 - (1 + (r^2)/(a^2))^(1 - b)
#   dist.geometric.cdf <- function(r, a, b) 1 - (1 + (r / a))^(2 - b)
#   dist.weibull.cdf <- function(r, a, b) 1 - exp(-(r^b / a^b))
#   
#   
#   # # Generic numeric CDF helper (radial form)
# cdf_numeric <- function(pdf_func, r, a, b = NULL) {
#   sapply(r, function(ri) {
#     if (ri <= 0) return(0)
#     integrate(function(x) pdf_func(x, a, b), 0, ri)$value
#   })
# }
# 
#   
#   # --- Numeric fallback (calls the existing pdf_function) --- #
#   pdf_func <- pdf_function(mname)
#   dist.numeric.cdf <- function(r, a, b = NULL) cdf_numeric(pdf_func, r, a, b)
#   
#   # --- Return correct one --- #
#   switch(
#     mname,
#     rayleigh = dist.rayleigh.cdf,
#     exponential = dist.exponential.cdf,
#     "2Dt" = dist.2dt.cdf,
#     geometric = dist.geometric.cdf,
#     weibull = dist.weibull.cdf,
#     "general normal" = dist.numeric.cdf,
#     lognormal = dist.numeric.cdf,
#     wald = dist.numeric.cdf,
#     gamma = dist.numeric.cdf,
#     stop("Unknown model name!")
#   )
# }

