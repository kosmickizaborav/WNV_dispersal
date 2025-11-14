# Fit ml_distr (Python -> R) by MLE without silencing warnings.
# Strategy: validate params and inputs, avoid calling dstable() when arguments are
# numerically extreme, and penalize invalid regions so optimizer avoids them.
#
# Requires:
# install.packages("stabledist")
# install.packages("ggplot2")
library(stabledist)
library(ggplot2)

# ---------- stable-based ml density, but "safe" (checks before calling dstable) ----------
ml_distr_R_safe <- function(xi, beta, pm = 1,
                            xx_upper = 1e200,   # cutoff to avoid extremely large xx
                            xx_lower = 1e-300   # cutoff to avoid denormals/underflow
) {
  # xi: positive numeric vector
  # beta: scalar (we require 0 < beta < 1)
  if (!is.numeric(xi) || any(is.na(xi))) stop("xi must be numeric with no NAs")
  if (any(xi <= 0)) stop("xi must be strictly positive")
  if (!is.numeric(beta) || length(beta) != 1) stop("beta must be scalar numeric")
  if (!(beta > 0 && beta < 1)) {
    stop("beta must satisfy 0 < beta < 1 for the model used here")
  }
  
  # keep beta away from boundary to ensure cos(pi*beta/2) > 0
  eps <- 1e-12
  beta <- min(max(beta, eps), 1 - eps)
  
  b_m <- 1 / beta
  # compute log(xx) instead of xx directly to detect overflow
  log_gg <- lgamma(1 + beta)            # log(gamma(1+beta))
  log_xi <- log(xi)
  log_xx <- b_m * log_gg - b_m * log_xi # log(xx) = b_m*log(gamma(1+beta)) - b_m*log(xi)
  
  # quick checks: if any xx would be outside representable numeric range -> return NA
  if (any(log_xx > log(xx_upper)) || any(log_xx < log(xx_lower))) {
    # returning NA here: caller should treat NA/invalid pdfs as penalized
    return(rep(NA_real_, length(xi)))
  }
  
  # safe to compute xx and scale now
  xx <- exp(log_xx)
  
  cosval <- cos(pi * beta / 2)
  if (cosval <= 0) {
    # Should not happen because we forced beta < 1-eps, but guard anyway
    return(rep(NA_real_, length(xi)))
  }
  scale <- cosval^(1 / beta)
  
  # call dstable only now; we do not wrap it with suppressWarnings here,
  # because we avoided the main problematic inputs by checks above.
  # But we still catch errors to avoid fatal crash.
  ll <- tryCatch(
    stabledist::dstable(xx, alpha = beta, beta = 1, gamma = scale, delta = 0, pm = pm),
    error = function(e) {
      # return NA to indicate failure to the caller
      rep(NA_real_, length(xx))
    }
  )
  
  # If dstable returned NA or non-finite values, propagate NA
  if (any(!is.finite(ll))) {
    return(rep(NA_real_, length(xi)))
  }
  
  # compute the ml pdf (use logs to build, then exponentiate)
  # log(pdf) = b_m*log(gamma(1+beta)) - log(beta) - (1 + b_m)*log(xi) + log(ll)
  log_pdf <- b_m * log_gg - log(beta) - (1 + b_m) * log_xi + log(ll)
  
  pdf <- exp(log_pdf)
  
  # guard against underflow to exactly 0 by returning small values if needed:
  pdf[pdf <= 0 | !is.finite(pdf)] <- NA_real_
  
  return(pdf)
}

# ---------- Negative log-likelihood with validation (returns large penalty for bad params) ----------
negloglik_beta_safe <- function(beta, data_xi, pm = 1, penalty = 1e10,
                                xx_upper = 1e200, xx_lower = 1e-300) {
  # quick domain check
  if (!is.finite(beta) || length(beta) != 1) return(penalty)
  if (!(beta > 0 && beta < 1)) return(penalty)
  
  pdfs <- ml_distr_R_safe(data_xi, beta = beta, pm = pm,
                          xx_upper = xx_upper, xx_lower = xx_lower)
  
  # if ml_distr_R_safe returned NA values -> penalize (avoid warnings)
  if (any(is.na(pdfs)) || any(pdfs <= 0)) return(penalty)
  
  # compute neg log-likelihood
  -sum(log(pdfs))
}

# ---------- Fit wrapper: multiple starts, try pm=1 and pm=0, produce profile ----------
fit_ml_beta_safe <- function(data_xi,
                             pm_options = c(1, 0),
                             starts = c(0.2, 0.4, 0.6, 0.8),
                             lower = 1e-6, upper = 1 - 1e-6,
                             penalty = 1e10,
                             xx_upper = 1e200, xx_lower = 1e-300,
                             trace = FALSE) {
  if (!is.numeric(data_xi) || any(is.na(data_xi)) || any(data_xi <= 0)) {
    stop("data_xi must be a numeric vector of positive values without NAs")
  }
  
  best_val <- Inf
  best_res <- NULL
  
  for (pm in pm_options) {
    for (init in starts) {
      init <- min(max(init, lower + 1e-8), upper - 1e-8)
      res <- tryCatch(
        optim(par = init,
              fn = function(p) negloglik_beta_safe(p, data_xi, pm = pm, penalty = penalty,
                                                   xx_upper = xx_upper, xx_lower = xx_lower),
              method = "L-BFGS-B",
              lower = lower,
              upper = upper,
              control = list(fnscale = 1)),
        error = function(e) list(convergence = 1, value = Inf, par = NA_real_)
      )
      if (is.list(res) && is.finite(res$value) && res$value < best_val) {
        best_val <- res$value
        best_res <- list(par = res$par, value = res$value, convergence = res$convergence,
                         pm = pm, optim_res = res)
      }
      if (trace) {
        message("pm=", pm, " init=", init, " val=", res$value, " conv=", res$convergence)
      }
    }
  }
  
  if (is.null(best_res)) {
    stop("All fits failed or were penalized. Try different starts or inspect data.")
  }
  
  # Build log-likelihood profile for diagnostics
  grid_beta <- seq(max(lower, 1e-6), min(upper, 1 - 1e-6), length.out = 200)
  grid_loglik <- vapply(grid_beta, function(b) {
    val <- negloglik_beta_safe(b, data_xi, pm = best_res$pm, penalty = penalty,
                               xx_upper = xx_upper, xx_lower = xx_lower)
    if (!is.finite(val) || val >= penalty) return(NA_real_)
    -val
  }, numeric(1))
  
  list(beta_hat = as.numeric(best_res$par),
       negloglik = best_res$value,
       pm = best_res$pm,
       optim_res = best_res$optim_res,
       grid_beta = grid_beta,
       grid_loglik = grid_loglik)
}

# ---------- Plot function: histogram + fitted pdf ----------
plot_fit_safe <- function(data_xi, fit_res, bins = 60) {
  beta_hat <- fit_res$beta_hat
  pm_best <- fit_res$pm
  xgrid <- seq(min(data_xi), max(data_xi), length.out = 400)
  pdf_hat <- ml_distr_R_safe(xgrid, beta = beta_hat, pm = pm_best)
  # if ml_distr_R_safe returned NA for some grid points, set them to 0 for plotting
  pdf_hat[is.na(pdf_hat)] <- 0
  df_pdf <- data.frame(x = xgrid, y = pdf_hat)
  
  ggplot(data.frame(x = data_xi), aes(x = x)) +
    geom_histogram(aes(y = after_stat(density)), bins = bins,
                   fill = "#2574a9", color = "gray33", linewidth = 0.2, alpha = 0.5) +
    geom_line(data = df_pdf, aes(x = x, y = y), color = "red", size = 1.1) +
    theme_bw() +
    labs(x = "xi (revisit_rate)", y = "density",
         title = sprintf("Fitted ml_distr: beta = %.4f (pm = %s)", beta_hat, pm_best),
         subtitle = sprintf("negloglik = %.4g", fit_res$negloglik))
}

# ---------- Usage example (replace with your data) ----------
# 
# 
fits_dir <- "/home/nina/R_projects/WNV_dispersal/Data/Graphs/8_cluster_revisits/Data_for_fits"
files <- list.files(fits_dir, full.names = T)
fi

data_fits <- file.path(plots_dir, "Data_for_fits")
files <- list.files(data_fits, full.names = T)
dt_fit <- fread(files[5])

dt_fit <- dt_fit[cum_revisits > 0]
dt_fit <- dt_fit[, mean_revisit := sum(cum_revisits)/uniqueN(selected_cluster)]
dt_fit[, revisit_rate := cum_revisits/mean_revisit]
# data_xi <- dt_fit$revisit_rate  # must be positive numeric vector
fit <- fit_ml_beta_safe(data_xi, pm_options = c(1,0), starts = c(0.1,0.2,0.4,0.6,0.8), trace = TRUE)
print(fit$beta_hat)
plot_fit_safe(data_xi, fit)
# Optional: plot profile
dfprof <- data.frame(beta = fit$grid_beta, loglik = fit$grid_loglik)
ggplot(dfprof, aes(beta, loglik)) + geom_line() + theme_minimal() + labs(y = "log-likelihood")