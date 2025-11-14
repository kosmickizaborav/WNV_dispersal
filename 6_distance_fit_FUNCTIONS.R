# PDF definitions---taken from dispfit package----------------------------------

pdf_function <- function(mname){

  dist.rayleigh <- function (r, a, b = NULL) {
    fg <- 2*pi*r*(1/(pi*a^2)) * exp(-r^2/a^2)
  }
  
  dist.exponential <- function (r, a, b = NULL) {
    2*pi*r*(1 / (2 * pi * a ^ 2 )) * exp(-r/a)
    # corrected function, adapted from Nathan 2012
  }
  
  dist.generalnormal <- function(r, a, b) {
    2 * pi * r * (b / (2 * pi * (a^2) * gamma(2 / b))) * exp(- (r / a)^b)
  }

  dist.2dt <- function(r, a, b) {
    2 * pi * r * ((b - 1) / (pi * a^2)) * ((1 + (r^2) / (a^2))^(-b))
  }

  dist.geometric <- function (r, a, b) {
    2*pi*r*(((b - 2) * (b - 1)) / (2 * pi * (a^2))) * ((1 + (r / a)) ^ -b)
  }

  dist.lognorm <- function (r, a, b) {
    2*pi*r * (1 / (((2 * pi) ^ (3/2)) * (b * (r ^ 2)))) * exp(-(log(r / a)^2) / (2 * (b ^ 2)))
  }

  dist.wald <- function (r, a, b) {
    2*pi*r * (sqrt(b)/sqrt(8 * (pi^3) * (r^5))) * exp(-(b * ((r - a)^2))/(2 * (a^2) * r))
  }

  dist.weibull <- function (r, a, b) {
    2*pi*r * (b/(2*pi*a^b)) * (r^(b-2)) * exp(-(r^b/a^b))
    ## function from Austerlitz 2004
  }

  dist.gamma <- function (r, a, b) {
    2*pi*r * (1 / (2 * pi * (a^2) * gamma(b))) * ((r/a)^(b-2)) * exp(-r/a)
  }

  # Use switch to return the appropriate PDF function
  switch(
    mname,
    rayleigh = dist.rayleigh,
    exponential = dist.exponential,
    "general normal" = dist.generalnormal,
    "2Dt" = dist.2dt,
    geometric = dist.geometric,
    lognormal = dist.lognorm,
    wald = dist.wald,
    weibull = dist.weibull,
    gamma = dist.gamma,
    stop("Unknown model name!")
  )

}


cdf_function <- function(mname) {
  
  # safe single integration -> numeric(1)
  safe_integrate_one <- function(pdf_func, r_val, a, b = NULL) {
    
    if (is.na(r_val) || r_val <= 0) return(0)
    
    integrand <- function(t) pdf_func(t, a, b)  # assumes pdf_func already radial
    
    res <- tryCatch(
      integrate(integrand, lower = 0, upper = r_val),
      error   = function(e) list(value = NA_real_),
      warning = function(w) {
        tryCatch(integrate(integrand, lower = 0, upper = r_val, rel.tol = 1e-6),
                 error = function(e) list(value = NA_real_))
      }
    )
    val <- if(is.list(res) && !is.null(res$value)) res$value else as.numeric(res)
    if (length(val) == 0) NA_real_ else as.numeric(val)[1]
  }
  
  # numeric CDF that ALWAYS returns numeric vector of same length as r_vals
  cdf_numeric <- function(pdf_func, r_vals, a, b = NULL) {
    vapply(
      r_vals,
      FUN = function(r) safe_integrate_one(pdf_func, r, a = a, b = b),
      FUN.VALUE = numeric(1),
      USE.NAMES = FALSE
    )
  }
  
  # return a function with explicit signature (avoid ambiguous ...)
  function(pdf_fun, r_vals, a, b = NULL) {
    cdf_numeric(pdf_fun, r_vals, a, b)
  }
  
}





# CDF function (calls pdf_function() for numeric integration)
# cdf_function <- function(mname) {
#   
#   # Analytic CDFs -- signature: function(pdf_fun, r_vals, a, b = NULL)
#   cdf_rayleigh_analytic <- function(pdf_fun, r_vals, a, b = NULL) {
#     1 - exp(-(r_vals / a)^2)
#   }
#   cdf_exponential_analytic <- function(pdf_fun, r_vals, a, b = NULL) {
#     # CDF for radial-exponential (with the pdf already radial as in pdf_function)
#     1 - (1 + r_vals / a) * exp(-r_vals / a)
#   }
#   cdf_lognormal_analytic <- function(pdf_fun, r_vals, a, b = NULL) {
#     # here 'a' and 'b' follow your prior convention: a = exp(meanlog), b = sdlog
#     plnorm(r_vals, meanlog = log(a), sdlog = b)
#   }
#   cdf_gamma_analytic <- function(pdf_fun, r_vals, a, b = NULL) {
#     pgamma(r_vals, shape = b - 1, scale = a)
#   }
#   cdf_weibull_analytic <- function(pdf_fun, r_vals, a, b = NULL) {
#     pweibull(r_vals, shape = b - 1, scale = a)
#   }
#   cdf_2dt_analytic <- function(pdf_fun, r_vals, a, b = NULL) {
#     1 - (1 + (r_vals / a)^2)^(1 - b)
#   }
#   cdf_geometric_analytic <- function(pdf_fun, r_vals, a, b = NULL) {
#     1 - (1 + (r_vals / a))^(2 - b)
#   }
#   
#   # Safe single integration returning numeric(1)
#   safe_integrate_one <- function(pdf_func, r_val, a, b = NULL) {
#     if (is.na(r_val) || r_val <= 0) return(0)
#     integrand <- function(t) pdf_func(t, a, b) # pdf_func returns radial pdf already
#     res <- tryCatch(
#       integrate(integrand, lower = 0, upper = r_val, rel.tol = 1e-8),
#       error = function(e) list(value = NA_real_),
#       warning = function(w) {
#         # try again with relaxed tolerance, otherwise NA
#         tryCatch(integrate(integrand, lower = 0, upper = r_val, rel.tol = 1e-6),
#                  error = function(e) list(value = NA_real_))
#       }
#     )
#     val <- if (is.list(res) && !is.null(res$value)) res$value else as.numeric(res)
#     if (length(val) == 0) NA_real_ else as.numeric(val)[1]
#   }
#   
#   # Numeric CDF fallback: integrate the radial pdf (pdf_func) directly
#   cdf_numeric_safe <- function(pdf_func, r_vals, a, b = NULL) {
#     vapply(r_vals, FUN.VALUE = numeric(1), USE.NAMES = FALSE, FUN = function(rr) {
#       safe_integrate_one(pdf_func, rr, a = a, b = b)
#     })
#   }
#   
#   # get pdf (used by numeric fallback)
#   pdf_func <- pdf_function(mname)
#   
#   # choose and return a function that always has the same signature:
#   # function(pdf_fun, r_vals, a, b = NULL) -> numeric vector length(r_vals)
#   switch(
#     mname,
#     rayleigh = cdf_rayleigh_analytic,
#     exponential = cdf_exponential_analytic,
#     lognormal = cdf_lognormal_analytic,
#     gamma = cdf_gamma_analytic,
#     weibull = cdf_weibull_analytic,
#     "2Dt" = cdf_2dt_analytic,
#     geometric = cdf_geometric_analytic,
#     # fallbacks for models without a closed-form (use numerical integration)
#     "general normal" = cdf_numeric_safe,
#     wald = cdf_numeric_safe,
#     stop("Unknown model name: ", mname)
#   )
# }